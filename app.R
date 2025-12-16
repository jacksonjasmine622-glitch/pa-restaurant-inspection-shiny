# ============================================================
# APP
# Data 4100 – Final Project
# Restaurant Spending Growth Analysis (Q1 2024 vs Q1 2025)
# Jasmine Jackson
# ============================================================

# -----------------------------
# Load Libraries
# -----------------------------
library(shiny)
library(dplyr)
library(tidyr)  
library(sf)
library(leaflet)
library(ggplot2)
library(stringr)
library(scales)

# -----------------------------
# Load Pre-processed Data
# -----------------------------
cat("Loading pre-processed data...\n")
city_yoy <- readRDS("processed_data/city_yoy.rds")
map_sf <- readRDS("processed_data/map_sf.rds")
cat("Data loaded successfully!\n")
cat("City YoY rows:", nrow(city_yoy), "\n")
cat("Map SF rows:", nrow(map_sf), "\n")

# ============================================================
# UI
# ============================================================
ui <- fluidPage(
  titlePanel("Restaurant Spending Growth in Pennsylvania (Q1 2024 → Q1 2025)"),
  
  mainPanel(
    width = 12,
    tabsetPanel(
      
      # -----------------------------
      # INTRODUCTION TAB
      # -----------------------------
      tabPanel(
        "Introduction",
        tags$h3("Overview"),
        tags$p(
          "This application analyzes year-over-year changes in restaurant spending across
           Pennsylvania cities by comparing Q1 2024 to Q1 2025 transaction data."
        ),
        tags$p(
          "Rather than focusing solely on percentage growth, this analysis emphasizes absolute
           spending changes to better capture economically meaningful shifts."
        ),
        tags$img(
          src = "restaurant_analysis.jpg",
          style = "width:100%; max-width:800px; display:block; margin:auto; border-radius:8px;"
        ),
        tags$h4("Interpretation"),
        tags$p(
          "Overall, restaurant spending growth across Pennsylvania is uneven. Larger metro areas
           tend to experience substantial absolute increases driven by transaction volume,
           while smaller cities show more volatile growth patterns. Absolute dollar change
           provides a more stable indicator of economic activity than percentage change alone."
        )
      ),
      
      # -----------------------------
      # MAP TAB
      # -----------------------------
      tabPanel(
        "Map",
        fluidRow(
          column(
            width = 3,
            sliderInput(
              "top_n_cities",
              "Show Top N Cities by Absolute Change",
              min = 5,
              max = 50,
              value = 20,
              step = 5
            ),
            helpText(
              "N = the number of cities to display on the map and chart."
            ),
            checkboxInput(
              "show_all_boundaries",
              "Show all city boundaries",
              value = FALSE
            ),
            checkboxInput(
              "show_decline",
              "Include cities with spending decline",
              value = FALSE
            ),
            helpText(
              "Focus on cities with the largest absolute spending changes. 
               Toggle to see all boundaries or include declining cities."
            )
          ),
          column(
            width = 9,
            tags$h4("Map & Bar Chart Interpretation"),
            tags$p(
              "This visualization examines absolute changes in restaurant spending across Pennsylvania cities between Q1 2024 and Q1 2025. The map displays the geographic distribution of spending growth and decline, where larger circles indicate greater dollar changes, and color distinguishes growth from decline."
            ),
            tags$p(
              "The accompanying bar chart ranks the top cities by absolute spending increase, providing a clear comparison of which locations experienced the most substantial growth. Together, these views show that the largest gains are concentrated in major metropolitan areas such as Pittsburgh and Philadelphia, reflecting their higher transaction volumes and larger consumer bases."
            ),
            tags$p(
              "At the same time, several mid-sized cities appear among the top growth locations, suggesting that meaningful spending increases are not limited to the largest markets. This combined view highlights both where growth is happening geographically and which cities are driving overall spending increases in absolute terms. Red = Growth, Blue = Decline."
            )
          )
        ),
        leafletOutput("spend_map", height = 550),
        plotOutput("top_cities_bar", height = 300)
      ),
      
      # -----------------------------
      # STATISTICAL ANALYSIS TAB
      # -----------------------------
      tabPanel(
        "Statistical Analysis",
        verbatimTextOutput("regression_summary"),
        tags$h4("Interpretation"),
        tags$p(
          "The regression results indicate that transaction volume is the strongest and most consistent predictor of absolute restaurant spending growth across Pennsylvania cities. Holding the number of restaurants and year-over-year percentage change constant, cities with higher transaction counts experienced significantly larger increases in total spending."
        ),
        tags$p(
          "Interestingly, the number of restaurants is negatively associated with absolute spending growth once transaction volume is accounted for, suggesting that growth is driven more by consumer activity intensity than by the sheer number of establishments."
        ),
        tags$p(
          "With an R² of approximately 0.74, the model explains a substantial share of the variation in spending growth, highlighting that economic scale and customer behavior play a larger role in driving restaurant spending increases than restaurant count alone."
        ),
        plotOutput("regression_plot", height = 350)
      )
    )
  )
)

# ============================================================
# SERVER
# ============================================================
server <- function(input, output, session) {
  
  # Get top cities by absolute change
  top_cities_data <- reactive({
    data <- map_sf |>
      filter(!is.na(abs_change))
    
    # Filter based on whether to include declines
    if (!input$show_decline) {
      data <- data |> filter(abs_change > 0)
    }
    
    # Get top N cities
    data |>
      arrange(desc(abs_magnitude)) |>
      slice_head(n = input$top_n_cities)
  })
  
  output$spend_map <- renderLeaflet({
    top_data <- top_cities_data()
    
    # Get centroids for circle markers
    centroids <- st_centroid(top_data)
    
    # Create color palette (red for growth, blue for decline)
    pal <- colorBin(
      palette = c("#2166ac", "#fee090", "#f46d43", "#a50026"),
      domain = top_data$abs_change,
      bins = 5
    )
    
    # Determine circle size based on magnitude
    max_mag <- max(top_data$abs_magnitude, na.rm = TRUE)
    radius_scale <- function(x) {
      scales::rescale(x, to = c(5000, 30000), from = c(0, max_mag))
    }
    
    map <- leaflet() |>
      addProviderTiles(providers$CartoDB.Positron)
    
    # Add background boundaries if requested
    if (input$show_all_boundaries) {
      map <- map |>
        addPolygons(
          data = map_sf,
          fillColor = "#f0f0f0",
          fillOpacity = 0.3,
          color = "#cccccc",
          weight = 0.5
        )
    }
    
    # Add highlighted city polygons
    map <- map |>
      addPolygons(
        data = top_data,
        fillColor = ~pal(abs_change),
        fillOpacity = 0.6,
        color = "#333333",
        weight = 1.5,
        highlightOptions = highlightOptions(
          weight = 3,
          color = "#000000",
          fillOpacity = 0.8,
          bringToFront = TRUE
        ),
        label = ~paste0(city, ": $", comma(abs_change))
      ) |>
      # Add circle markers at centroids for emphasis
      addCircleMarkers(
        data = centroids,
        radius = ~radius_scale(abs_magnitude) / 1000,
        fillColor = ~pal(abs_change),
        fillOpacity = 0.8,
        color = "#000000",
        weight = 2,
        popup = ~paste0(
          "<b>", city, ", PA</b><br/>",
          "<b>Spend Change: $", comma(abs_change), "</b><br/>",
          "YoY % Change: ", yoy_pct_change, "%<br/>",
          "Total Spend (2025): $", comma(total_spend), "<br/>",
          "Transactions: ", comma(transaction_count), "<br/>",
          "Restaurants: ", num_restaurants
        )
      ) |>
      addLegend(
        pal = pal,
        values = top_data$abs_change,
        title = "Spending Change ($)",
        position = "bottomright",
        labFormat = labelFormat(prefix = "$")
      )
    
    map
  })
  
  output$top_cities_bar <- renderPlot({
    top_cities_data() |>
      st_drop_geometry() |>
      arrange(desc(abs_change)) |>
      mutate(
        city = reorder(city, abs_change),
        change_type = ifelse(abs_change > 0, "Growth", "Decline")
      ) |>
      ggplot(aes(city, abs_change, fill = change_type)) +
      geom_col() +
      coord_flip() +
      scale_fill_manual(values = c("Growth" = "#a50026", "Decline" = "#2166ac")) +
      scale_y_continuous(labels = dollar_format()) +
      labs(
        title = paste("Top", input$top_n_cities, "Cities by Absolute Spending Change"),
        x = NULL,
        y = "Absolute Spend Change ($)",
        fill = NULL
      ) +
      theme_minimal() +
      theme(legend.position = "bottom")
  })
  
  analysis_data <- reactive({
    city_yoy |>
      filter(year == 2025, state == "PA", valid_yoy) |>
      select(abs_change, transaction_count, num_restaurants, yoy_pct_change) |>
      drop_na()
  })
  
  output$regression_summary <- renderPrint({
    summary(lm(
      abs_change ~ transaction_count + num_restaurants + yoy_pct_change,
      data = analysis_data()
    ))
  })
  
  output$regression_plot <- renderPlot({
    # Cap at 1 million transactions for visualization only
    plot_data <- analysis_data() |>
      filter(transaction_count <= 1000000)
    
    ggplot(plot_data, aes(transaction_count, abs_change)) +
      geom_point(alpha = 0.6, color = "#b30000") +
      geom_smooth(method = "lm", se = FALSE, color = "black") +
      scale_x_continuous(labels = comma_format()) +
      scale_y_continuous(labels = dollar_format()) +
      labs(
        title = "Transaction Volume vs Absolute Spend Change",
        x = "Transactions",
        y = "Absolute Spend Change ($)"
      ) +
      theme_minimal() +
      theme(plot.title = element_text(hjust = 0.5))
  })
}

shinyApp(ui = ui, server = server)