# -----------------------------------------------------------------------------
# VIS1_ANIMATION: The Game Score Shift - gganimate Animation
# -----------------------------------------------------------------------------
# Author: Martin Moll
# Purpose: Animated visualization showing the key Moneyball insight.
#          Morphs between per-game and per-minute Hollinger Game Score,
#          letting the viewer watch Hidden Gems (green) rise as the
#          scoring shifts from volume-based to rate-based.
#
# Integration:
#   1. source("vis1_animation.R") in app.R
#   2. Add vis1_anim_ui() inside page_navbar()
#   3. Call vis1_anim_server(input, output, session) inside server function
#
# Animation justification:
#   Two static side-by-side charts would force the viewer to mentally track
#   each player across panels. The animated morph lets them watch individual
#   players move, making the shift from volume to rate viscerally clear.
#   The smooth easing (cubic-in-out) highlights which dots move the most,
#   naturally drawing attention to the Hidden Gems.
#
# Technical note:
#   The GIF is pre-rendered once on app startup and cached in tempdir().
#   This avoids re-rendering on every page load, which would take ~30 seconds.
#   Requires: gganimate, gifski
# -----------------------------------------------------------------------------


# Load packages ---------------------------------------------------------------
library(shiny)
library(bslib)
library(tidyverse)
library(gganimate)
library(bayesrules)
library(gifski)


# Data preparation ------------------------------------------------------------
# Uses bayesrules::basketball (no external files needed).

data(basketball, package = "bayesrules")

anim_data <- basketball %>%
  # Remove rows with missing player names (2 NaN rows in the dataset)
  filter(!is.na(player_name)) %>%
  # Remove "TOT" (total) rows for players who switched teams mid-season.
  # The dataset includes a combined TOT row plus individual rows per team.
  # We drop TOT and keep only the actual team-specific entries.
  filter(team != "TOT") %>%
  # For players on multiple teams, keep only the LAST team they played for.
  # basketball-reference lists teams in chronological order after TOT,
  # so slice_tail picks the final (most recent) team.
  group_by(player_name) %>%
  slice_tail(n = 1) %>%
  ungroup() %>%
  mutate(
    # starter column is numeric 1/0
    role = ifelse(starter == 1, "Starter", "Bench"),
    role = factor(role, levels = c("Starter", "Bench"))
  ) %>%
  # Same 5-minute filter as vis1.R for consistency across tabs
  filter(avg_minutes_played >= 5)


# Hollinger Game Score --------------------------------------------------------
# Formula by John Hollinger (used by NBA/WNBA analysts):
#   GmSc = PTS + 0.4*FGM - 0.7*FGA - 0.4*(FTA - FTM)
#          + 0.7*ORB + 0.3*DRB + STL + 0.7*AST + 0.7*BLK
#          - 0.4*PF - TOV
#
# We derive FGM from FG% * FGA because the dataset doesn't include
# field goals made as a separate column.
# field_goal_pct is stored as a decimal (e.g., 0.488, not 48.8%).
#
# Two versions:
#   game_score_pg = per-game (raw, favours high-minute starters)
#   game_score_pm = per-minute (time-adjusted, reveals efficient bench players)

anim_data <- anim_data %>%
  mutate(
    # Derive field goals made from percentage and attempts
    avg_fgm = field_goal_pct * avg_field_goal_attempts,
    
    # Per-game Game Score (sum of weighted box score stats)
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
    
    # Per-minute Game Score (controls for playing time)
    game_score_pm = game_score_pg / avg_minutes_played
  )


# Normalise to 0-100 scale ----------------------------------------------------
# The per-game score ranges ~(-1 to 12) and per-minute ranges ~(0 to 0.5).
# If we animate between them on the same y-axis, the per-minute values
# collapse to a flat line at zero. Normalising both to 0-100 independently
# puts them on the same visual scale so the relative positions shift
# and the viewer can see which players move up or down.

normalise_to_100 <- function(x) {
  rng <- range(x, na.rm = TRUE)
  (x - rng[1]) / (rng[2] - rng[1]) * 100
}

anim_data <- anim_data %>%
  mutate(
    gs_pg_scaled = normalise_to_100(game_score_pg),
    gs_pm_scaled = normalise_to_100(game_score_pm)
  )


# Moneyball classification -----------------------------------------------------
# A "Hidden Gem" must meet ALL three conditions:
#   1. Bench player (starter == 0)
#   2. Averages at least 8 minutes per game (enough playing time to
#      produce a reliable per-minute rate; players with 5-7 min have
#      very small sample sizes per game)
#   3. Per-minute Game Score is at or above the 75th percentile of
#      ALL players (starters + bench combined)
#
# Players with 5-7 minutes remain in the dataset as "Bench" even if
# their game score is high. They stay visible on the charts but are
# not flagged as gems because their per-minute stats are based on
# too few minutes to be trustworthy.

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

# Colour palette: consistent across all tabs in the app
# Blue = established starters, Green = undervalued gems, Gold = regular bench
role_colors <- c(
  "Starter"    = "#4575b4",
  "Hidden Gem" = "#2ca02c",
  "Bench"      = "#f0ad4e"
)


# Pre-render the GIF -----------------------------------------------------------
# The animation is rendered once on app startup and saved as a GIF in
# R's temporary directory. Subsequent page loads serve the cached file.
# 120 frames at 15fps = 8 seconds total loop time.
#
# gganimate techniques used:
#   transition_states() - cycles between per-game and per-minute states
#   ease_aes("cubic-in-out") - smooth acceleration/deceleration
#   group = player_id - ensures each dot tracks the same player across states

gif_path <- file.path(tempdir(), "moneyball_shift.gif")

if (!file.exists(gif_path)) {
  # Stack both score versions: each player appears twice (once per state)
  shift_frames <- bind_rows(
    anim_data %>%
      mutate(scaled_score = gs_pg_scaled,
             metric = "Per-Game Game Score (Raw Stats)"),
    anim_data %>%
      mutate(scaled_score = gs_pm_scaled,
             metric = "Per-Minute Game Score (Time-Adjusted)")
  ) %>%
    # Unique ID so gganimate tracks each player's dot across states
    mutate(player_id = paste(player_name, team))
  
  anim_plot <- ggplot(shift_frames, aes(
    x      = avg_minutes_played,
    y      = scaled_score,
    colour = moneyball_flag,
    size   = avg_points,
    group  = player_id
  )) +
    geom_point(alpha = 0.7) +
    scale_colour_manual(values = role_colors, drop = FALSE) +
    scale_size_continuous(range = c(2, 8), guide = "none") +
    labs(
      # {closest_state} is a gganimate variable that inserts the current
      # state name into the title during animation
      title    = "{closest_state}",
      subtitle = "Watch how Hidden Gems (green) rise when adjusting for playing time",
      x        = "Average Minutes per Game",
      y        = "Hollinger Game Score (scaled 0-100)",
      colour   = NULL
    ) +
    theme_minimal(base_size = 13) +
    theme(
      legend.position = "bottom",
      plot.title = element_text(size = 12, face = "bold"),
      plot.margin = margin(t = 10, r = 10, b = 20, l = 10)
    ) +
    transition_states(metric,
                      transition_length = 3,
                      state_length = 2) +
    ease_aes("cubic-in-out")
  
  # Render and save to disk
  anim_save(gif_path,
            animate(anim_plot, nframes = 120, fps = 15,
                    width = 1000, height = 620, res = 100,
                    renderer = gifski_renderer()))
}


# -----------------------------------------------------------------------------
# UI COMPONENT
# -----------------------------------------------------------------------------
vis1_anim_ui <- function() {
  nav_panel(
    title = "HGS Shift",
    icon  = icon("play"),
    
    layout_sidebar(
      sidebar = sidebar(
        title = "HGS Shift",
        width = 300,
        
        # Explanation of what the animation shows
        p("This animation morphs between two ways of measuring",
          "player value using Hollinger's Game Score(HGS):"),
        
        tags$ul(
          tags$li(strong("Per-Game"), " (raw) favours starters who",
                  "accumulate stats through more minutes"),
          tags$li(strong("Per-Minute"), " (time-adjusted) reveals bench",
                  "players who produce at a high rate")
        ),
        
        hr(),
        
        p("Both scores are scaled to 0-100 so you can see the",
          "relative positions shift."),
        
        p("Watch the", tags$span("green dots", style = "color:#2ca02c; font-weight:bold;"),
          "(Hidden Gems) rise when the view shifts to per-minute."),
        
        hr(),
        
        # Colour legend
        p(tags$span("Blue", style = "color:#4575b4; font-weight:bold;"),
          "= Starters"),
        p(tags$span("Green", style = "color:#2ca02c; font-weight:bold;"),
          "= Hidden Gems (bench, 8+ min, top 25% per-minute)"),
        p(tags$span("Gold", style = "color:#f0ad4e; font-weight:bold;"),
          "= Bench"),
        
        hr(),
        
        # Replay button
        actionButton("vis1_anim_replay", "Replay Animation",
                     icon = icon("rotate-right"),
                     class = "btn-primary btn-sm")
      ),
      
      # Main panel: the GIF
      card(
        card_header(
          "The Moneyball Shift: Per-Game vs. Per-Minute Game Score",
          class = "bg-primary text-white"
        ),
        card_body(
          # CSS constrains the GIF to 75% of viewport height so title,
          # axis labels, and legend are all visible on fullscreen displays
          tags$div(
            style = "text-align: center; overflow: hidden;",
            imageOutput("vis1_anim_gif", height = "auto",
                        inline = TRUE)
          )
        )
      )
    )
  )
}


# -----------------------------------------------------------------------------
# SERVER COMPONENT
# -----------------------------------------------------------------------------
vis1_anim_server <- function(input, output, session) {
  
  # Serve the pre-rendered GIF
  output$vis1_anim_gif <- renderImage({
    input$vis1_anim_replay
    
    list(
      src         = gif_path,
      contentType = "image/gif",
      style       = "max-height: 100vh; max-width: 100%; width: auto; height: auto; display: block; margin: 0 auto;",
      alt         = "HGS Shift Animation: per-game vs per-minute Hollinger Game Score"
    )
  }, deleteFile = FALSE)
}