# -----------------------------------------------------------------------------
# VIS1: Interactive Dumbbell Chart - Starters vs. Bench Comparison
# -----------------------------------------------------------------------------
# Purpose: Opening visualization for the Moneyball narrative. Shows that
#          bench players produce at similar per-minute rates to starters,
#          establishing the premise that the gap is about opportunity
#          (playing time), not ability (production rate).
#
# Integration:
#   1. source("vis1.R") in app.R
#   2. Add vis1_ui() inside page_navbar()
#   3. Call vis1_server(input, output, session) inside the server function
#
# Interactivity justification:
# A static dumbbell could only show one view. The per-minute vs per-game
# toggle lets the user discover the insight themselves: starters dominate
# on volume, but the gap shrinks dramatically on a rate basis, as is expected.
# -----------------------------------------------------------------------------


# Load packages ---------------------------------------------------------------
library(shiny)
library(bslib)
library(tidyverse)
library(plotly)
library(bayesrules)


# Data preparation ------------------------------------------------------------
# Runs once when the app loads (not reactively), for performance.
# Uses bayesrules::basketball so no external files are needed and
# runGitHub() works without downloading data.

data(basketball, package = "bayesrules")

vis1_data <- basketball %>%
  # Remove rows with missing player names (2 NaN rows in the dataset)
  filter(!is.na(player_name)) %>%
  # Remove "TOT" (total) rows for players who switched teams mid-season.
  # These players have a TOT row aggregating all teams plus individual
  # rows per team. We keep only the individual team rows.
  filter(team != "TOT") %>%
  # For players who appear on multiple teams, keep only the LAST team
  # they played for (last row per player in the dataset, which
  # basketball-reference orders chronologically).
  group_by(player_name) %>%
  slice_tail(n = 1) %>%
  ungroup() %>%
  mutate(
    # The starter column is numeric (1/0), not logical or character.
    role = ifelse(starter == 1, "Starter", "Bench"),
    role = factor(role, levels = c("Starter", "Bench"))
  ) %>%
  # Filter to players averaging 8+ minutes per game. Players with fewer
  # minutes have volatile per-minute stats (e.g., 2 points in 1 minute
  # = 2.0 pts/min, which is misleadingly high).
  
  filter(avg_minutes_played >= 8) %>%
  mutate(
    # Per-minute stats: divide each per-game average by minutes played.
    # This controls for playing time so bench and starters can be
    # compared on equal footing.
    pts_per_min = avg_points / avg_minutes_played,
    reb_per_min = avg_rb / avg_minutes_played,
    ast_per_min = avg_assists / avg_minutes_played,
    stl_per_min = avg_steals / avg_minutes_played,
    blk_per_min = avg_blocks / avg_minutes_played,
    tov_per_min = avg_turnovers / avg_minutes_played
  )


# Pre-compute dumbbell data ---------------------------------------------------
# Group averages are computed for both views (per-minute and per-game)
# at load time rather than reactively, since the data doesn't change.
# This avoids recalculating on every toggle.

# Per-minute view: shows production RATE (per minute on court)
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
  # Reshape: one row per stat, with Starter and Bench as columns.
  # This structure seems to be needed for the dumbbell (two points per row).
  pivot_longer(-role, names_to = "stat", values_to = "value") %>%
  pivot_wider(names_from = role, values_from = value) %>%
  mutate(
    diff = Starter - Bench,
    pct_diff = round((Starter - Bench) / Bench * 100, 1),
    # Factor levels control the top-to-bottom display order
    stat = factor(stat, levels = rev(c("Points", "Rebounds", "Assists",
                                       "Steals", "Blocks", "FG Pct",
                                       "Turnovers")))
  )

# Per-game view: shows production VOLUME (total per game).
# This naturally favours starters who play more minutes.
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


# -----------------------------------------------------------------------------
# UI COMPONENT
# -----------------------------------------------------------------------------
# The dumbbell chart is the first of the tabs in the app with the visualizations
# It's intentionally simple and visual to ease the audience in before
# the more complex Game Score analysis.

vis1_ui <- function() {
  nav_panel(
    title = "Starters vs. Bench",
    icon  = icon("arrows-left-right"),
    
    layout_sidebar(
      sidebar = sidebar(
        title = "The Case for Hidden Value",
        width = 400,
        
        # Context text
        p("How do starters and bench players really compare?",
          "Toggle between per-minute and per-game stats to see",
          "the difference that playing time makes."),
        
        hr(),
        
        # Toggle: the core interactive element for this visualization.
        # Switching between views is the "discovery moment" where the
        # user sees insight for themselves.
        radioButtons(
          inputId  = "vis1_mode",
          label    = "View Mode:",
          choices  = c("Per-Minute (rate)" = "permin",
                       "Per-Game (volume)" = "pergame"),
          selected = "permin"
        ),
        
        hr(),
        
        # Legend explanation (since the dumbbell doesn't use a standard legend)
        p(strong("How to read this chart:")),
        p(tags$span("Blue", style = "color:#4575b4; font-weight:bold;"),
          "dots = Starters"),
        p(tags$span("Gold", style = "color:#f0ad4e; font-weight:bold;"),
          "dots = Bench players"),
        p("When the dots are close together, bench players are",
          "producing at nearly the same rate as starters."),
        
        hr(),
        
        p(em("Hover over any dot or segment for exact values."))
      ),
      
      # --- Main panel: dumbbell chart ---
      card(
        card_header(
          textOutput("vis1_title"),
          class = "bg-primary text-white"
        ),
        card_body(
          plotlyOutput("vis1_dumbbell", height = "500px")
        )
      ),
      
      # Insight card: dynamically updates based on toggle
      card(
        card_header("Key Insight"),
        card_body(
          textOutput("vis1_insight")
        )
      )
    )
  )
}


# -----------------------------------------------------------------------------
# SERVER COMPONENT
# -----------------------------------------------------------------------------
vis1_server <- function(input, output, session) {
  
  # Reactive data: switches between pre-computed per-minute and per-game
  # datasets based on the radio button toggle
  dumbbell_data <- reactive({
    if (input$vis1_mode == "permin") {
      vis1_permin
    } else {
      vis1_pergame
    }
  })
  
  # Dynamic title: updates to reflect current view mode
  output$vis1_title <- renderText({
    if (input$vis1_mode == "permin") {
      "Per-Minute Production: Starters vs. Bench"
    } else {
      "Per-Game Production: Starters vs. Bench"
    }
  })
  
  # Dynamic insight text: provides the "so what?" interpretation
  # that the rubric requires. Different text for each view because
  # the insight IS the contrast between the two views.
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
  
  # Dumbbell chart: the main visualization
  # Built with three ggplot layers:
  #   1. geom_segment (grey bar connecting bench and starter)
  #   2. geom_point for bench (gold)
  #   3. geom_point for starter (blue)
  # Converted to plotly for hover interactivity.
  output$vis1_dumbbell <- renderPlotly({
    df <- dumbbell_data()
    mode_label <- ifelse(input$vis1_mode == "permin", "Per-Minute", "Per-Game")
    # Per-minute values are small decimals (0.35), per-game are larger (8.7)
    # so we adjust rounding accordingly
    digits <- ifelse(input$vis1_mode == "permin", 4, 2)
    
    p <- ggplot(df) +
      # Grey connecting segment: shows the gap between groups
      geom_segment(aes(
        x = Bench, xend = Starter,
        y = stat, yend = stat,
        text = paste0(stat,
                      "\nStarter: ", round(Starter, digits),
                      "\nBench: ", round(Bench, digits),
                      "\nDifference: ", round(diff, digits),
                      "\nStarter advantage: ", pct_diff, "%")
      ), colour = "#cccccc", linewidth = 2.5) +
      # Gold dot: bench player average
      geom_point(aes(
        x = Bench, y = stat,
        text = paste0(stat, " - Bench",
                      "\n", mode_label, " avg: ", round(Bench, digits))
      ), colour = "#f0ad4e", size = 6) +
      # Blue dot: starter average
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
        # Remove horizontal grid lines since the dumbbell segments
        # already provide the visual structure
        panel.grid.major.y = element_blank(),
        legend.position = "none"
      )
    
    # Convert to plotly for hover tooltips
    ggplotly(p, tooltip = "text") %>%
      layout(margin = list(l = 90, r = 30, t = 20, b = 60))
  })
}
