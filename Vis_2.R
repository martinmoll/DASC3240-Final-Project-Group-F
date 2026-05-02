# =============================================================================
# VISUALIZATION 2: BASKETBALL PLAYER PERFORMANCE EXPLORER
# =============================================================================

# Load required libraries
library(shiny)       
library(tidyverse)   
library(plotly)      
library(bslib)       
library(bayesrules)  

# --- Data preparation (runs once when app starts) ---

# Load the basketball dataset from bayesrules package
data(basketball, package = "bayesrules")

# Remove rows where total_minutes is zero (players who never played)
basketball <- basketball[basketball$total_minutes > 0, ]

# Remove "TOT" rows – these are season totals for players who switched teams
# We only want per-team stats, not the aggregated seasonal totals
basketball <- basketball[basketball$team != "TOT", ]

# Some players appear multiple times (different teams). Keep only the last row
# for each player (their final team of the season) to avoid duplicates.
basketball <- basketball[!duplicated(basketball$player_name, fromLast = TRUE), ]

# Clean and transform data for our analysis:
# - Remove rows with missing key stats
# - Keep only players playing at least 8 minutes per game (filter low-sample noise)
# - Create per-minute efficiency metrics
# - Build a tooltip label for hover information
vis2_data <- basketball %>%
  filter(!is.na(height), !is.na(avg_minutes_played), 
         !is.na(avg_points), !is.na(avg_assists), 
         !is.na(avg_rb), !is.na(field_goal_pct),
         avg_minutes_played >= 8) %>%
  mutate(
    points_per_minute = avg_points / avg_minutes_played,
    assists_per_minute = avg_assists / avg_minutes_played,
    rebounds_per_minute = avg_rb / avg_minutes_played,
    player_label = paste0(
      player_name, "<br>",
      "Team: ", team, "<br>",
      "PPG: ", round(avg_points, 2), "<br>",
      "APG: ", round(avg_assists, 2), "<br>",
      "RPG: ", round(avg_rb, 2), "<br>",
      "FG%: ", round(field_goal_pct, 3)
    )
  )

# =============================================================================
# UI — defines what the user sees
# =============================================================================
vis2_ui <- function() {
  nav_panel(
    title = "Player Performance",
    icon  = icon("chart-scatter"),
    
    layout_sidebar(
      sidebar = sidebar(
        title = "Individual Player Performance",
        width = 300,
        
        # Context text 
        p("Let's take a deeper dive at a more granular level at each player."),
        
        hr(),
        
        # Filters
        selectInput(
          inputId  = "vis2_metric",
          label    = "Choose Metric:",
          choices  = c(
            "Points per Minute" = "points",
            "Assists per Minute" = "assists",
            "Rebounds per Minute" = "rebounds",
            "Field Goal %" = "fg"
          ),
          selected = "points"
        ),
        
        radioButtons(
          inputId   = "vis2_player_type",
          label     = "Player Type:",
          choices   = c("Bench" = "bench", "Starter" = "starter"),
          selected  = "bench"
        ),
        
        hr(),
        
        # Explanation of how to read the chart 
        p(strong("How to read this chart:")),
        p("Dynamically switch metrics/player types to quickly spot efficient bench players."),
        p("Players ", strong("above the red dashed line"), " are performing better than the starter average."),
        p("Players ", strong("above the green dotted line"), " are performing better than the best starter performance."),
        
        hr(),
        
        # Hover tip
        p(em("Hover over any point to see player details."))
      ),
      
      # Main content area
      card(
        card_header("Minutes vs. Per‑Minute Efficiency"),
        card_body(
          plotlyOutput("vis2_plot", height = "500px"),
          
          # box for the legend text 
          div(
            style = "margin-top: 15px; padding: 8px 12px; background-color: #f8f9fa; border: 1px solid #dee2e6; border-radius: 5px; text-align: center; font-size: 12px;",
            "🔴 Red dashed = Starter average | 🟢 Green dotted = Starter best"
          )
        )
      ),
      
      card(
        card_header("📊 Analysis"),
        card_body(
          tags$div(
            tags$ul(
              tags$li("Bench efficiency – Many bench players exceed starter average in points, rebounds, and FG%, showing deep benches add value."),
              tags$li("Bench can beat the best – For the same three metrics, some bench players surpass even the best starter’s value (except assists)."),
              tags$li("Assists separate roles – No bench player reaches the top starter’s assist rate; playmaking remains starter‑dominant.")
            )
          )
        )
      )
    )
  )
}

# =============================================================================
# SERVER — defines the logic behind the UI
# =============================================================================
vis2_server <- function(input, output, session) {
  
  # Reactive: subset data based on bench vs starter selection
  filtered_data <- reactive({
    if (input$vis2_player_type == "bench") {
      vis2_data %>% filter(starter == FALSE)   # bench players
    } else {
      vis2_data %>% filter(starter == TRUE)    # starters
    }
  })
  
  # Reactive: compute the starter average for the currently selected metric
  starter_avg_metric <- reactive({
    starter_data <- vis2_data %>% filter(starter == TRUE)
    switch(input$vis2_metric,
           "points"   = mean(starter_data$points_per_minute, na.rm = TRUE),
           "assists"  = mean(starter_data$assists_per_minute, na.rm = TRUE),
           "rebounds" = mean(starter_data$rebounds_per_minute, na.rm = TRUE),
           "fg"       = mean(starter_data$field_goal_pct, na.rm = TRUE))
  })
  
  # Reactive: compute the starter best (maximum) for the currently selected metric
  starter_best_metric <- reactive({
    starter_data <- vis2_data %>% filter(starter == TRUE)
    switch(input$vis2_metric,
           "points"   = max(starter_data$points_per_minute, na.rm = TRUE),
           "assists"  = max(starter_data$assists_per_minute, na.rm = TRUE),
           "rebounds" = max(starter_data$rebounds_per_minute, na.rm = TRUE),
           "fg"       = max(starter_data$field_goal_pct, na.rm = TRUE))
  })
  
  # Render the interactive plot using plotly
  output$vis2_plot <- renderPlotly({
    data <- filtered_data()                    # current player type subset
    avg_line <- starter_avg_metric()           # red dashed line value
    best_line <- starter_best_metric()         # green dotted line value
    
    # Extract y-axis values based on selected metric
    y_var <- switch(input$vis2_metric,
                    "points"   = data$points_per_minute,
                    "assists"  = data$assists_per_minute,
                    "rebounds" = data$rebounds_per_minute,
                    "fg"       = data$field_goal_pct)
    
    # y-axis label
    y_label <- switch(input$vis2_metric,
                      "points"   = "Points Per Minute",
                      "assists"  = "Assists Per Minute",
                      "rebounds" = "Rebounds Per Minute",
                      "fg"       = "Field Goal %")
    
    # Dynamic plot title
    title_text <- paste(
      ifelse(input$vis2_player_type == "bench", "Bench Players:", "Starters:"),
      y_label, "vs Playing Time (≥8 MPG)"
    )
    
    # Build the ggplot
    p <- ggplot(data, aes(x = avg_minutes_played,
                          y = y_var,
                          text = player_label)) +   # text = tooltip content
      geom_point(alpha = 0.65, size = 1.8, color = "steelblue") +   # all points same color
      geom_hline(yintercept = avg_line, 
                 linetype = "dashed", 
                 color = "red", 
                 size = 0.8) +                     # starter average line
      geom_hline(yintercept = best_line, 
                 linetype = "dotted", 
                 color = "green", 
                 size = 0.8) +                    # starter best line
      labs(title = title_text,
           x = "Minutes Per Game",
           y = y_label) +
      theme_minimal() +
      theme(legend.position = "none",
            axis.title.x = element_text(margin = margin(t = 15))) +
      scale_y_continuous(labels = scales::number_format(accuracy = 0.01))
    
    # Convert to interactive plotly (no annotation inside the plot)
    ggplotly(p, tooltip = "text") %>%
      layout(
        margin = list(b = 30, t = 50)  #
      )
  })
}
