# ============================================================
# PREPROCESSING SCRIPT
# Restaurant Spending Growth Analysis (Q1 2024 vs Q1 2025)
# ============================================================

library(dplyr)
library(sf)
library(readr)
library(purrr)
library(stringr)

# -----------------------------
# Data Paths
# -----------------------------
path_2024 <- "data/raw/Q1_2024"
path_2025 <- "data/raw/Q1_2025"

# -----------------------------
# Read In Dewey Data
# -----------------------------
read_dewey_folder <- function(folder, year) {
  files <- list.files(folder, pattern = "\\.csv$", full.names = TRUE)
  cat("Reading", length(files), "files from", folder, "\n")
  
  map_dfr(files, ~{
    read_csv(.x, show_col_types = FALSE) |>
      mutate(year = year, quarter = "Q1")
  })
}

cat("Loading 2024 data...\n")
data_2024 <- read_dewey_folder(path_2024, 2024)

cat("Loading 2025 data...\n")
data_2025 <- read_dewey_folder(path_2025, 2025)

cat("Combining data...\n")
raw_data <- bind_rows(data_2024, data_2025)

# -----------------------------
# Data Cleaning
# -----------------------------
cat("Processing data...\n")

# Check column names and standardize
if ("CITY" %in% names(raw_data)) {
  city_col <- "CITY"
  region_col <- "REGION"
  spend_col <- "RAW_TOTAL_SPEND"
  trans_col <- "RAW_NUM_TRANSACTIONS"
  place_col <- "PLACEKEY"
} else if ("city" %in% names(raw_data)) {
  city_col <- "city"
  region_col <- "region"
  spend_col <- "raw_total_spend"
  trans_col <- "raw_num_transactions"
  place_col <- "placekey"
}

spend_trimmed <- raw_data |>
  select(
    city = all_of(city_col),
    state = all_of(region_col),
    raw_total_spend = all_of(spend_col),
    raw_num_transactions = all_of(trans_col),
    placekey = all_of(place_col),
    year, 
    quarter
  )

city_qtr_spend <- spend_trimmed |>
  group_by(city, state, year, quarter) |>
  summarise(
    total_spend = sum(raw_total_spend, na.rm = TRUE),
    transaction_count = sum(raw_num_transactions, na.rm = TRUE),
    num_restaurants = n_distinct(placekey),
    .groups = "drop"
  )

city_yoy <- city_qtr_spend |>
  arrange(city, year) |>
  group_by(city, state) |>
  mutate(
    yoy_pct_change = round((total_spend - lag(total_spend)) / lag(total_spend) * 100, 2),
    abs_change = total_spend - lag(total_spend),
    valid_yoy = lag(total_spend) >= 5000
  ) |>
  ungroup()

# -----------------------------
# Spatial Join (PA only)
# -----------------------------
cat("Processing spatial data...\n")
map_data <- city_yoy |>
  filter(year == 2025, state == "PA")

pa_places <- st_read("data/spatial/tl_2025_42_place.shp") |>
  st_transform(4326) |>
  mutate(
    city_clean = str_trim(str_to_upper(NAME)),
    state = "PA"
  )

# Simplify to reduce file size
pa_places <- st_simplify(pa_places, dTolerance = 100)

map_sf <- pa_places |>
  left_join(
    map_data |>
      mutate(city_clean = str_trim(str_to_upper(city))),
    by = c("city_clean", "state")
  ) |>
  mutate(abs_magnitude = abs(abs_change))

# -----------------------------
# Save Processed Data
# -----------------------------
cat("Saving processed data...\n")

# Create processed data directory
dir.create("data/processed", recursive = TRUE, showWarnings = FALSE)

# Save as compressed RDS files
saveRDS(city_yoy, "data/processed/city_yoy.rds", compress = "xz")
saveRDS(map_sf, "data/processed/map_sf.rds", compress = "xz")

cat("\n=== PREPROCESSING COMPLETE ===\n")
cat("Files created:\n")
cat("  - data/processed/city_yoy.rds\n")
cat("  - data/processed/map_sf.rds\n")
cat("\nOriginal data size:\n")
cat("  raw_data:", format(object.size(raw_data), units = "MB"), "\n")
cat("\nProcessed data size:\n")
cat("  city_yoy:", format(object.size(city_yoy), units = "MB"), "\n")
cat("  map_sf:", format(object.size(map_sf), units = "MB"), "\n")
cat("\nFile sizes on disk:\n")
print(file.info(c("data/processed/city_yoy.rds", "data/processed/map_sf.rds"))[c("size")])

