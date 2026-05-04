# -----------------------------------------------------------------------------
# VIS1_ANIMATION: The Game Score Shift
# -----------------------------------------------------------------------------
# Purpose:
# Interactive animated scatter plot showing the Moneyball insight.
# Uses plotly's frame-based animation to morph between per-game and
# per-minute Hollinger Game Score, with full hover tooltips on every
# dot during and after animation.
#
# Integration:
#   1. source("vis1_animation.R") in app.R
#   2. Add vis1_anim_ui() inside page_navbar()
#   3. Call vis1_anim_server(input, output, session) inside server function
#
# -----------------------------------------------------------------------------

library(shiny)
library(bslib)
library(tidyverse)
library(plotly)
library(bayesrules)


# Data preparation ------------------------------------------------------------
data(basketball, package = "bayesrules")

anim_data <- basketball %>%
  filter(!is.na(player_name)) %>%
  filter(team != "TOT") %>%
  group_by(player_name) %>%
  slice_tail(n = 1) %>%
  ungroup() %>%
  mutate(
    role = ifelse(starter == 1, "Starter", "Bench"),
    role = factor(role, levels = c("Starter", "Bench"))
  ) %>%
  filter(avg_minutes_played >= 5)


# Hollinger Game Score --------------------------------------------------------
anim_data <- anim_data %>%
  mutate(
    avg_fgm = field_goal_pct * avg_field_goal_attempts,
    
    game_score_pg = avg_points
    + 0.4 * avg_fgm
    - 0.7 * avg_field_goal_attempts
    - 0.4 * (avg_free_throw_attempts - avg_free_throws)
    + 0.7 * avg_offensive_rb
    + 0.3 * avg_defensive_rb
    + avg_steals
    + 0.7 * avg_assists
    + 0.7 * avg_blocks
    - 0.4 * avg_personal_fouls
    - avg_turnovers,
    
    game_score_pm = game_score_pg / avg_minutes_played
  )


# Normalise to 0-100 ----------------------------------------------------------
normalise_to_100 <- function(x) {
  rng <- range(x, na.rm = TRUE)
  (x - rng[1]) / (rng[2] - rng[1]) * 100
}

anim_data <- anim_data %>%
  mutate(
    gs_pg_scaled = normalise_to_100(game_score_pg),
    gs_pm_scaled = normalise_to_100(game_score_pm)
  )


# Hidden Gem classification (75th percentile, 8+ min) -------------------------
gs_threshold <- quantile(anim_data$game_score_pm, 0.75)

anim_data <- anim_data %>%
  mutate(
    moneyball_flag = case_when(
      role == "Bench" &
        avg_minutes_played >= 8 &
        game_score_pm >= gs_threshold       ~ "Hidden Gem",
      role == "Starter"                     ~ "Starter",
      TRUE                                  ~ "Bench"
    ),
    moneyball_flag = factor(moneyball_flag,
                            levels = c("Starter", "Hidden Gem", "Bench"))
  )

role_colors <- c(
  "Starter"    = "#0072B2",
  "Hidden Gem" = "#009E73",
  "Bench"      = "#F0E442"
)


# Build plotly animation data -------------------------------------------------

plotly_data <- bind_rows(
  anim_data %>%
    mutate(
      scaled_score = gs_pg_scaled,
      frame = "Per-Game"
    ),
  anim_data %>%
    mutate(
      scaled_score = gs_pm_scaled,
      frame = "Per-Minute"
    )
) %>%
  mutate(
    player_id = paste(player_name, team),
    # Hover text with full player details
    hover_text = paste0(
      "<b>", player_name, "</b><br>",
      "Team: ", team, "<br>",
      "Role: ", role, "<br>",
      "Moneyball: ", moneyball_flag, "<br>",
      "<br>",
      "Minutes/game: ", round(avg_minutes_played, 1), "<br>",
      "Points/game: ", round(avg_points, 1), "<br>",
      "FG%: ", round(field_goal_pct * 100, 1), "%<br>",
      "Rebounds/game: ", round(avg_rb, 1), "<br>",
      "Assists/game: ", round(avg_assists, 1), "<br>",
      "<br>",
      "Game Score/game: ", round(game_score_pg, 1), "<br>",
      "Game Score/min: ", round(game_score_pm, 3), "<br>",
      "Scaled score: ", round(scaled_score, 1)
    )
  )


# ----------------------------------------------------------------------------
# UI
# ----------------------------------------------------------------------------
vis1_anim_ui <- function() {
  nav_panel(
    title = "HGS Shift",
    icon  = icon("play"),
    
    layout_sidebar(
      sidebar = sidebar(
        title = "HGS Shift",
        width = 400,
        
        p("This animated chart morphs between two ways of measuring",
          "player value using Hollinger's Game Score (HGS):"),
        
        tags$ul(
          tags$li(strong("Per-Game"), " (raw) favours starters who",
                  "accumulate stats through more minutes"),
          tags$li(strong("Per-Minute"), " (time-adjusted) reveals bench",
                  "players who produce at a high rate")
        ),
        
        hr(),
        
        p("Both scores are scaled to 0-100 so you can see the",
          "relative positions shift."),
        
        p("Watch the", tags$span("teal dots", style = "color:#009E73; font-weight:bold;"),
          "(Hidden Gems) rise when the view shifts to per-minute."),
        
        hr(),
        
        p(
          tags$span(style = "display:inline-block;width:14px;height:14px;background:#0072B2;border-radius:3px;margin-right:8px;vertical-align:middle;"),
          "Starters"),
        p(
          tags$span(style = "display:inline-block;width:14px;height:14px;background:#009E73;border-radius:3px;margin-right:8px;vertical-align:middle;"),
          "Hidden Gems (bench, 8+ min, top 25% per-minute)"),
        p(
          tags$span(style = "display:inline-block;width:14px;height:14px;background:#F0E442;border-radius:3px;margin-right:8px;vertical-align:middle;border:1px solid #ccc;"),
          "Bench"),
        
        hr(),
        
        p(em("Hover over any dot to see player details.")),
        p(em("Use the play button or drag the slider to control the animation."))
      ),
      
      card(
        card_header(
          "The Game Score Shift: Per-Game vs. Per-Minute Game Score",
          class = "bg-primary text-white"
        ),
        card_body(
          plotlyOutput("vis1_anim_plot", height = "600px")
        )
      )
    )
  )
}


# ----------------------------------------------------------------------------
# SERVER
# ----------------------------------------------------------------------------
vis1_anim_server <- function(input, output, session) {
  
  output$vis1_anim_plot <- renderPlotly({
    
    # Build the animated plotly chart directly (not via ggplotly)
    # for better control over animation settings
    plot_ly(
      data = plotly_data,
      x = ~avg_minutes_played,
      y = ~scaled_score,
      color = ~moneyball_flag,
      colors = role_colors,
      size = ~avg_points,
      sizes = c(40, 400),
      # ids links dots across frames so plotly animates each player's movement
      ids = ~player_id,
      # frame controls which state each row belongs to
      frame = ~frame,
      type = "scatter",
      mode = "markers",
      marker = list(opacity = 0.7, line = list(width = 0.5, color = "white")),
      text = ~hover_text,
      hoverinfo = "text"
    ) %>%
      layout(
        xaxis = list(title = "Average Minutes per Game"),
        yaxis = list(title = "Hollinger Game Score (scaled 0-100)"),
        legend = list(orientation = "h", x = 0.2, y = 1.02,
                      xanchor = "left", yanchor = "bottom"),
        margin = list(l = 60, r = 20, t = 50, b = 80)
      ) %>%
      animation_opts(
        # Transition time, ms
        frame = 1500,
        # Pause duration
        transition = 800,
        # Easing function for smooth movement
        easing = "cubic-in-out",
        # Redraw the plot on each frame
        redraw = FALSE
      ) %>%
      animation_slider(
        currentvalue = list(
          prefix = "View: ",
          font = list(size = 12)
        )
      ) %>%
      animation_button(
        x = 0, y = -0.18,
        label = "Play"
      )
  })
}