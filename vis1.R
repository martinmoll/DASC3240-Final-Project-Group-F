# =============================================================================
# VIS1: Dumbbell Chart — Starters vs. Bench Comparison
# =============================================================================
# Modular Shiny tab. To integrate:
#   1. source("vis1.R") in app.R
#   2. Add vis1_ui() inside page_navbar()
#   3. Call vis1_server(input, output, session) inside server function
# =============================================================================

library(shiny)
library(bslib)
library(tidyverse)
library(plotly)
library(bayesrules)

# --- Data preparation (runs once when app loads) ---
data(basketball, package = "bayesrules")

vis1_data <- basketball %>%
  mutate(
    role = ifelse(starter == 1, "Starter", "Bench"),
    role = factor(role, levels = c("Starter", "Bench"))
  ) %>%
  filter(avg_minutes_played >= 5) %>%
  mutate(
    pts_per_min = avg_points / avg_minutes_played,
    reb_per_min = avg_rb / avg_minutes_played,
    ast_per_min = avg_assists / avg_minutes_played,
    stl_per_min = avg_steals / avg_minutes_played,
    blk_per_min = avg_blocks / avg_minutes_played,
    tov_per_min = avg_turnovers / avg_minutes_played
  )

# Pre-compute both dumbbell datasets
vis1_permin <- vis1_data %>%
  group_by(role) %>%
  summarise(
    Points    = mean(pts_per_min, na.rm = TRUE),
    Rebounds  = mean(reb_per_min, na.rm = TRUE),
    Assists   = mean(ast_per_min, na.rm = TRUE),
    Steals    = mean(stl_per_min, na.rm = TRUE),
    Blocks    = mean(blk_per_min, na.rm = TRUE),
    Turnovers = mean(tov_per_min, na.rm = TRUE),
    `FG Pct`  = mean(field_goal_pct, na.rm = TRUE),
    .groups   = "drop"
  ) %>%
  pivot_longer(-role, names_to = "stat", values_to = "value") %>%
  pivot_wider(names_from = role, values_from = value) %>%
  mutate(
    diff = Starter - Bench,
    pct_diff = round((Starter - Bench) / Bench * 100, 1),
    stat = factor(stat, levels = rev(c("Points", "Rebounds", "Assists",
                                       "Steals", "Blocks", "FG Pct",
                                       "Turnovers")))
  )

vis1_pergame <- vis1_data %>%
  group_by(role) %>%
  summarise(
    Points    = mean(avg_points, na.rm = TRUE),
    Rebounds  = mean(avg_rb, na.rm = TRUE),
    Assists   = mean(avg_assists, na.rm = TRUE),
    Steals    = mean(avg_steals, na.rm = TRUE),
    Blocks    = mean(avg_blocks, na.rm = TRUE),
    Turnovers = mean(avg_turnovers, na.rm = TRUE),
    `FG Pct`  = mean(field_goal_pct, na.rm = TRUE),
    .groups   = "drop"
  ) %>%
  pivot_longer(-role, names_to = "stat", values_to = "value") %>%
  pivot_wider(names_from = role, values_from = value) %>%
  mutate(
    diff = Starter - Bench,
    pct_diff = round((Starter - Bench) / Bench * 100, 1),
    stat = factor(stat, levels = rev(c("Points", "Rebounds", "Assists",
                                       "Steals", "Blocks", "FG Pct",
                                       "Turnovers")))
  )


# =============================================================================
# UI
# =============================================================================
vis1_ui <- function() {
  nav_panel(
    title = "Starters vs. Bench",
    icon  = icon("arrows-left-right"),
    
    layout_sidebar(
      sidebar = sidebar(
        title = "The Case for Hidden Value",
        width = 300,
        
        p("How do starters and bench players really compare?",
          "Toggle between per-minute and per-game stats to see",
          "the difference that playing time makes."),
        
        hr(),
        
        radioButtons(
          inputId  = "vis1_mode",
          label    = "View Mode:",
          choices  = c("Per-Minute (rate)" = "permin",
                       "Per-Game (volume)" = "pergame"),
          selected = "permin"
        ),
        
        hr(),
        
        p(strong("How to read this chart:")),
        p(tags$span("Blue", style = "color:#4575b4; font-weight:bold;"),
          "dots = Starters"),
        p(tags$span("Gold", style = "color:#f0ad4e; font-weight:bold;"),
          "dots = Bench players"),
        p("When the dots are close together, bench players are",
          "producing at nearly the same rate as starters."),
        
        hr(),
        
        p(em("Hover over any dot for exact values."))
      ),
      
      # Main panel
      card(
        card_header(
          textOutput("vis1_title"),
          class = "bg-primary text-white"
        ),
        card_body(
          plotlyOutput("vis1_dumbbell", height = "500px")
        )
      ),
      
      card(
        card_header("Key Insight"),
        card_body(
          textOutput("vis1_insight")
        )
      )
    )
  )
}


# =============================================================================
# SERVER
# =============================================================================
vis1_server <- function(input, output, session) {
  
  # Reactive data based on toggle
  dumbbell_data <- reactive({
    if (input$vis1_mode == "permin") {
      vis1_permin
    } else {
      vis1_pergame
    }
  })
  
  # Dynamic title
  output$vis1_title <- renderText({
    if (input$vis1_mode == "permin") {
      "Per-Minute Production: Starters vs. Bench"
    } else {
      "Per-Game Production: Starters vs. Bench"
    }
  })
  
  # Dynamic insight text
  output$vis1_insight <- renderText({
    if (input$vis1_mode == "permin") {
      paste("On a per-minute basis, bench players produce at surprisingly",
            "similar rates to starters across most categories.",
            "FG% in particular shows bench players matching or exceeding",
            "starters. The gap between starters and bench is primarily",
            "about opportunity (minutes), not ability (rate of production).",
            "This is the foundation of the Moneyball argument.")
    } else {
      paste("Per-game stats make starters look dominant across the board.",
            "But this is largely because they play more minutes,",
            "giving them more time to accumulate stats.",
            "Switch to Per-Minute view to see the real comparison",
            "that controls for playing time.")
    }
  })
  
  # Dumbbell chart
  output$vis1_dumbbell <- renderPlotly({
    df <- dumbbell_data()
    mode_label <- ifelse(input$vis1_mode == "permin", "Per-Minute", "Per-Game")
    digits <- ifelse(input$vis1_mode == "permin", 4, 2)
    
    p <- ggplot(df) +
      # Connecting segment
      geom_segment(aes(
        x = Bench, xend = Starter,
        y = stat, yend = stat,
        text = paste0(stat,
                      "\nStarter: ", round(Starter, digits),
                      "\nBench: ", round(Bench, digits),
                      "\nDifference: ", round(diff, digits),
                      "\nStarter advantage: ", pct_diff, "%")
      ), colour = "#cccccc", linewidth = 2.5) +
      # Bench dot (gold)
      geom_point(aes(
        x = Bench, y = stat,
        text = paste0(stat, " - Bench",
                      "\n", mode_label, " avg: ", round(Bench, digits))
      ), colour = "#f0ad4e", size = 6) +
      # Starter dot (blue)
      geom_point(aes(
        x = Starter, y = stat,
        text = paste0(stat, " - Starter",
                      "\n", mode_label, " avg: ", round(Starter, digits))
      ), colour = "#4575b4", size = 6) +
      labs(
        x = paste(mode_label, "Average"),
        y = NULL
      ) +
      theme_minimal(base_size = 14) +
      theme(
        panel.grid.major.y = element_blank(),
        legend.position = "none"
      )
    
    ggplotly(p, tooltip = "text") %>%
      layout(margin = list(l = 90, r = 30, t = 20, b = 60))
  })
}