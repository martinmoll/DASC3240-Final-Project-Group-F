# =============================================================================
# VIS_ANIMATION: The Moneyball Shift — gganimate Animation
# =============================================================================
# Modular Shiny tab. To integrate:
#   1. source("vis_animation.R") in app.R
#   2. Add vis_anim_ui() inside page_navbar()
#   3. Call vis_anim_server(input, output, session) inside server function
#
# The animation is pre-rendered once on app startup and served as a GIF.
# Requires: gganimate, gifski
# =============================================================================

library(shiny)
library(bslib)
library(tidyverse)
library(gganimate)
library(bayesrules)
library(gifski)

# --- Data preparation (runs once when app loads) ---
data(basketball, package = "bayesrules")

anim_data <- basketball %>%
  mutate(
    role = ifelse(starter == 1, "Starter", "Bench"),
    role = factor(role, levels = c("Starter", "Bench"))
  ) %>%
  filter(avg_minutes_played >= 5)

# Hollinger Game Score
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

# Normalise both to 0-100
normalise_to_100 <- function(x) {
  rng <- range(x, na.rm = TRUE)
  (x - rng[1]) / (rng[2] - rng[1]) * 100
}

anim_data <- anim_data %>%
  mutate(
    gs_pg_scaled = normalise_to_100(game_score_pg),
    gs_pm_scaled = normalise_to_100(game_score_pm)
  )

# Moneyball flags (75th percentile)
gs_threshold <- quantile(anim_data$game_score_pm, 0.75)

anim_data <- anim_data %>%
  mutate(
    moneyball_flag = case_when(
      role == "Bench" & game_score_pm >= gs_threshold ~ "Hidden Gem",
      role == "Starter" ~ "Starter",
      TRUE ~ "Bench"
    ),
    moneyball_flag = factor(moneyball_flag,
                            levels = c("Starter", "Hidden Gem", "Bench"))
  )

role_colors <- c(
  "Starter"    = "#4575b4",
  "Hidden Gem" = "#2ca02c",
  "Bench"      = "#f0ad4e"
)

# --- Pre-render the GIF (runs once on app startup) ---
gif_path <- file.path(tempdir(), "moneyball_shift.gif")

if (!file.exists(gif_path)) {
  shift_frames <- bind_rows(
    anim_data %>%
      mutate(scaled_score = gs_pg_scaled,
             metric = "Per-Game Game Score (Raw Stats)"),
    anim_data %>%
      mutate(scaled_score = gs_pm_scaled,
             metric = "Per-Minute Game Score (Time-Adjusted)")
  ) %>%
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
      title    = "{closest_state}",
      subtitle = "Watch how Hidden Gems (green) rise when adjusting for playing time",
      x        = "Average Minutes per Game",
      y        = "Hollinger Game Score (scaled 0-100)",
      colour   = NULL
    ) +
    theme_minimal(base_size = 14) +
    theme(
      legend.position = "bottom",
      plot.title = element_text(size = 13, face = "bold")
    ) +
    transition_states(metric,
                      transition_length = 3,
                      state_length = 2) +
    ease_aes("cubic-in-out")
  
  anim_save(gif_path,
            animate(anim_plot, nframes = 120, fps = 15,
                    width = 800, height = 550, res = 100,
                    renderer = gifski_renderer()))
}


# =============================================================================
# UI
# =============================================================================
vis_anim_ui <- function() {
  nav_panel(
    title = "Moneyball Shift",
    icon  = icon("play"),
    
    layout_sidebar(
      sidebar = sidebar(
        title = "The Moneyball Shift",
        width = 300,
        
        p("This animation morphs between two ways of measuring",
          "player value using Hollinger's Game Score:"),
        
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
        
        p(tags$span("Blue", style = "color:#4575b4; font-weight:bold;"),
          "= Starters"),
        p(tags$span("Green", style = "color:#2ca02c; font-weight:bold;"),
          "= Hidden Gems"),
        p(tags$span("Gold", style = "color:#f0ad4e; font-weight:bold;"),
          "= Bench"),
        
        hr(),
        
        actionButton("vis_anim_replay", "Replay Animation",
                     icon = icon("rotate-right"),
                     class = "btn-primary btn-sm")
      ),
      
      card(
        card_header(
          "The Moneyball Shift: Per-Game vs. Per-Minute Game Score",
          class = "bg-primary text-white"
        ),
        card_body(
          imageOutput("vis_anim_gif", height = "550px")
        )
      )
    )
  )
}


# =============================================================================
# SERVER
# =============================================================================
vis_anim_server <- function(input, output, session) {
  
  # Render the pre-built GIF
  # The replay button forces a re-render by invalidating the output
  output$vis_anim_gif <- renderImage({
    # Depend on replay button to force refresh
    input$vis_anim_replay
    
    list(
      src    = gif_path,
      contentType = "image/gif",
      width  = "100%",
      alt    = "Moneyball Shift Animation"
    )
  }, deleteFile = FALSE)
}