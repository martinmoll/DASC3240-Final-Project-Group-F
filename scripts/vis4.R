# =============================================================================
# WNBA Hidden Gems - Hollinger Game Score Animation
# =============================================================================

library(shiny)
library(bslib)
library(tidyverse)
library(gganimate)
library(ggimage)
library(gifski)
library(ggplot2)

# --- Data preparation (runs once when app loads) ---
data(basketball, package = "bayesrules")
vis4_df <- basketball
vis4_labels_df <- read.csv('player_photos_resized/labels.csv', stringsAsFactors = FALSE) %>%
  select(player_name, image_path)

vis4_df <- vis4_df %>%
  mutate(
    game_score = avg_points +
      (0.4 * avg_field_goals) -
      (0.7 * avg_field_goal_attempts) -
      (0.4 * (avg_free_throw_attempts - avg_free_throws)) +
      (0.7 * avg_offensive_rb) +
      (0.3 * avg_defensive_rb) +
      avg_steals +
      (0.7 * avg_assists) +
      (0.7 * avg_blocks) -
      (0.4 * avg_personal_fouls) -
      avg_turnovers
  )

# per-minute game score for later percentile-based classification
vis4_df <- vis4_df %>% mutate(per_min_game_score = game_score / avg_minutes_played)

## include all players (starters + bench) but keep a minutes cutoff
vis4_players_data <- vis4_df %>%
  filter(avg_minutes_played > 5) %>%
  drop_na(
    avg_points, avg_field_goals, avg_field_goal_attempts,
    avg_free_throw_attempts, avg_free_throws,
    avg_offensive_rb, avg_defensive_rb,
    avg_steals, avg_assists, avg_blocks,
    avg_personal_fouls, avg_turnovers
  )

vis4_players_scored <- vis4_players_data %>%
  mutate(
    score_s1  = avg_points,
    score_s2  = score_s1  + (0.4 * avg_field_goals),
    score_s3  = score_s2  - (0.7 * avg_field_goal_attempts),
    score_s4  = score_s3  - (0.4 * (avg_free_throw_attempts - avg_free_throws)),
    score_s5  = score_s4  + (0.7 * avg_offensive_rb),
    score_s6  = score_s5  + (0.3 * avg_defensive_rb),
    score_s7  = score_s6  + avg_steals,
    score_s8  = score_s7  + (0.7 * avg_assists),
    score_s9  = score_s8  + (0.7 * avg_blocks),
    score_s10 = score_s9  - (0.4 * avg_personal_fouls),
    score_s11 = score_s10 - avg_turnovers
  )

# 75th percentile per-minute game score for bench players (hidden-gem threshold)
bench_threshold <- quantile(
  vis4_players_data %>% filter(!starter) %>% pull(per_min_game_score),
  probs = 0.75, na.rm = TRUE
)

vis4_stage_levels <- c(
  "1. Points (PTS)",
  "2. + Field Goals (FG)",
  "3. - FG Attempts (FGA)",
  "4. - FT Misses (FTA-FT)",
  "5. + Offensive Rebounds (ORB)",
  "6. + Defensive Rebounds (DRB)",
  "7. + Steals (STL)",
  "8. + Assists (AST)",
  "9. + Blocks (BLK)",
  "10. - Personal Fouls (PF)",
  "11. - Turnovers (TOV)"
)

vis4_make_stage <- function(data, score_col, label) {
  data %>% mutate(score_raw = .data[[score_col]], stage = label)
}

vis4_animation_data <- bind_rows(
  vis4_make_stage(vis4_players_scored, "score_s1",  vis4_stage_levels[1]),
  vis4_make_stage(vis4_players_scored, "score_s2",  vis4_stage_levels[2]),
  vis4_make_stage(vis4_players_scored, "score_s3",  vis4_stage_levels[3]),
  vis4_make_stage(vis4_players_scored, "score_s4",  vis4_stage_levels[4]),
  vis4_make_stage(vis4_players_scored, "score_s5",  vis4_stage_levels[5]),
  vis4_make_stage(vis4_players_scored, "score_s6",  vis4_stage_levels[6]),
  vis4_make_stage(vis4_players_scored, "score_s7",  vis4_stage_levels[7]),
  vis4_make_stage(vis4_players_scored, "score_s8",  vis4_stage_levels[8]),
  vis4_make_stage(vis4_players_scored, "score_s9",  vis4_stage_levels[9]),
  vis4_make_stage(vis4_players_scored, "score_s10", vis4_stage_levels[10]),
  vis4_make_stage(vis4_players_scored, "score_s11", vis4_stage_levels[11])
) %>%
  mutate(stage = factor(stage, levels = vis4_stage_levels)) %>%
  mutate(score = score_raw) %>%
  filter(!starter) %>%
  group_by(stage) %>%
  arrange(desc(score_raw), player_name) %>%
  mutate(rank = row_number() * 1.0) %>%
  ungroup() %>%
  arrange(player_name, stage) %>%
  group_by(player_name) %>%
  mutate(
    prev_rank  = lag(rank),
    rank_delta = prev_rank - rank,
    rank_arrow = case_when(
      is.na(rank_delta) ~ "→",
      rank_delta > 0    ~ paste0("▲", rank_delta),
      rank_delta < 0    ~ paste0("▼", abs(rank_delta)),
      TRUE              ~ "→"
    )
  ) %>%
  ungroup() %>%
  left_join(vis4_labels_df, by = "player_name") %>%
  # compute player color using starter / hidden-gem rules
  mutate(
    image_path_resized = file.path("player_photos_resized", basename(image_path)),
    image_path_final   = if_else(file.exists(image_path_resized), image_path_resized, image_path),
    player_color = case_when(
      starter == TRUE ~ "#4575b4",                        # Blue: Starters
      starter == FALSE & per_min_game_score >= bench_threshold ~ "#2ca02c", # Green: Hidden Gems
      TRUE            ~ "#f0ad4e"                         # Gold: other bench
    )
  )


# =============================================================================
# UI
# =============================================================================
vis4_ui <- function() {
  nav_panel(
    title = "Hollinger Game Score",
    icon  = icon("trophy"),

    layout_sidebar(
      sidebar = sidebar(
        title = "Players Leaderboard animation",
        width = 300,

        h4("Animation Parameters"),
        # sliderInput("vis4_nframes", "Frames (nframes):",
        #             min = 11, max = 66, value = 11, step = 11),
        # vis4_fps <- 1, 
        # sliderInput("vis4_fps", "Frame Rate (FPS):",
        #             min = 1, max = 6, value = 1),
        sliderInput("vis4_top_n", "Show Top N:",
                    min = 5, max = 15, value = 10),
        actionButton("vis4_render_btn", "▶ Generate Animation / Replay",
                     class = "btn-primary w-100"),

        # Playback controls
        div(
          style = "display: flex; gap: 10px; margin: 15px 0;",
          actionButton("vis4_play_btn", "▶ Play", class = "btn-sm btn-success"),
          actionButton("vis4_pause_btn", "⏸ Pause", class = "btn-sm btn-warning")
        ),

        hr(),

        p(tags$span("Green", style = "color:#2ca02c; font-weight:bold;"),
          "= Hidden Gems (bench, 8+ min, top 25% per-minute)"),
        p(tags$span("Gold", style = "color:#f0ad4e; font-weight:bold;"),
          "= Bench"),

        hr(), 
        p(strong("How to read this leaderboard:")),
        p("The leaderboard ranks bench players by their cumulative impact score (raw) at each stage of the Hollinger Game Score formula."),
        p("The arrows on the right indicate how the player's rank changes from the previous stage (▲ = up, ▼ = down, → = no change)."),
        
        hr(), 
        p(em("Adjust the slider to show more or fewer players, and click 'Generate Animation' to replay the animation from the start.")),
        p(em("Click 'Play' or 'Pause' to control the animation.")),        

      ),

      card(
        card_header("WNBA Hidden Gems: Hollinger Game Score"),
        card_body(
        #   p("The ", strong("Hollinger Game Score"), " is a single-game impact metric. Formula:"),
        #   withMathJax(
        #     helpText("$$\\text{Game Score} = \\text{PTS} + 0.4\\cdot\\text{FG}
        #               - 0.7\\cdot\\text{FGA} - 0.4\\cdot(\\text{FTA}-\\text{FT})
        #               + 0.7\\cdot\\text{ORB} + 0.3\\cdot\\text{DRB}
        #               + \\text{STL} + 0.7\\cdot\\text{AST} + 0.7\\cdot\\text{BLK}
        #               - 0.4\\cdot\\text{PF} - \\text{TOV}$$")
        #   ),
          
          # Stage display
          div(
            style = "font-weight: bold; font-size: 16px; margin-bottom: 3px; color: #333;",
            textOutput("vis4_stage_display")
          ),
          
          plotOutput("vis4_plot", height = "600px"), 
        )
      ), 
    card(
        card_header("Key Insight"),
        card_body(
        tags$div(
            tags$ul(
            tags$li("Stage sensitivity – The leaderboard rankings shift dramatically at certain stages, particularly when adding assists and blocks, showing how different skills can elevate a player's impact."),
            tags$li("Volatility vs. Stability – Some players show consistent rank improvements across stages, while fluctuation indicates the player's rank depends on specific components.")
            )
        )
        )
    )
    )
  )
}


# =============================================================================
# SERVER
# =============================================================================
vis4_server <- function(input, output, session) {

  # Reactive values for animation control
  vis4_anim_data <- reactiveVal(NULL)
  vis4_current_stage_idx <- reactiveVal(1)
  vis4_is_playing <- reactiveVal(FALSE)
  vis4_animation_speed <- reactiveVal(1)  # Can be adjusted based on fps

  # Generate animation data when "Generate Animation" is clicked
  observeEvent(input$vis4_render_btn, {
    withProgress(message = "Preparing animation…", value = 0.5, {
      top_n <- input$vis4_top_n

      anim_df <- vis4_animation_data %>%
        group_by(stage) %>%
        arrange(desc(score_raw), player_name) %>%
        mutate(rank = row_number() * 1.0) %>%
        filter(rank <= top_n) %>%
        ungroup() %>%
        arrange(player_name, stage) %>%
        group_by(player_name) %>%
        mutate(
          prev_rank  = lag(rank),
          rank_delta = prev_rank - rank,
          rank_arrow = case_when(
            is.na(rank_delta) ~ "→",
            rank_delta > 0    ~ paste0("▲", rank_delta),
            rank_delta < 0    ~ paste0("▼", abs(rank_delta)),
            TRUE              ~ "→"
          ),
          arrow_color = case_when(
            is.na(rank_delta) ~ "white",
            rank_delta > 0    ~ "#00c853",
            rank_delta < 0    ~ "#d50000",
            TRUE              ~ "white"
          )
        ) %>%
        ungroup()

      vis4_anim_data(anim_df)
      vis4_current_stage_idx(1)
      vis4_is_playing(FALSE)
      vis4_animation_speed(1)

      incProgress(0.5, detail = "Ready!")
    })
  })

  # Auto-advance animation when playing
  observe({
    if (vis4_is_playing()) {
      req(vis4_anim_data())
      num_stages <- length(vis4_stage_levels)
      current_idx <- isolate(vis4_current_stage_idx())
      
      if (current_idx < num_stages) {
        vis4_current_stage_idx(current_idx + 1)
        invalidateLater(1000 / isolate(vis4_animation_speed()))
      } else {
        vis4_is_playing(FALSE)  # Stop when reaching end
      }
    }
  })

  # Play button
  observeEvent(input$vis4_play_btn, {
    vis4_is_playing(TRUE)
  })

  # Pause button
  observeEvent(input$vis4_pause_btn, {
    vis4_is_playing(FALSE)
  })

  # Stage display text
  output$vis4_stage_display <- renderText({
    idx <- vis4_current_stage_idx()
    num_stages <- length(vis4_stage_levels)
    stage_name <- vis4_stage_levels[idx]
    sprintf("Stage %d of %d: %s", idx, num_stages, stage_name)
  })

  # Reactive plot
  output$vis4_plot <- renderPlot({
    req(vis4_anim_data())
    
    anim_df <- vis4_anim_data()
    current_idx <- vis4_current_stage_idx()
    current_stage <- vis4_stage_levels[current_idx]
    
    # Filter data for current stage
    plot_df <- anim_df %>%
      filter(stage == current_stage) %>%
      arrange(desc(rank))
    NAME_Y           <- 0
    IMAGE_TIP_OFFSET <- 0.2
    IMAGE_SIZE       <- 0.06

    ggplot(plot_df, aes(group = player_name)) +
      geom_tile(
        aes(x = rank, y = score / 2, height = score, width = 0.9, fill = player_color),
        alpha = 0.9
      ) +
      geom_image(
        aes(x = rank, y = pmax(score - IMAGE_TIP_OFFSET, IMAGE_TIP_OFFSET),
            image = image_path_final),
        size = IMAGE_SIZE
      ) +
      geom_text(
        aes(x = rank, y = NAME_Y, label = player_name, color = "black"),
        hjust = -0.1, size = 5, fontface = "bold"
      ) +
      geom_text(
        aes(x = rank,
            # y = pmax(score - IMAGE_TIP_OFFSET, IMAGE_TIP_OFFSET) + 2,
            y =  max(score) - IMAGE_TIP_OFFSET + 2, 
            label = rank_arrow),
        color = with(plot_df, ifelse(rank_delta > 0, "#00c853",
                                  ifelse(rank_delta < 0, "#d50000", "#0066cc"))),
        size = 6, fontface = "bold"
      ) +
      geom_text(
        aes(x = rank, y = score, label = round(score, 2)),
        hjust = -0.2, size = 4
      ) +
      coord_flip(clip = "off", expand = FALSE) +
      scale_x_reverse() +
      scale_fill_identity() +
      scale_color_identity() +
      theme_minimal() +
      theme(
        panel.grid    = element_blank(),
        axis.text.y   = element_blank(),
        plot.title    = element_text(size = 22, face = "bold"),
        plot.margin   = margin(1, 6, 1, 1, "cm"),
        legend.position = "none"
      ) +
      labs(
        title    = "WNBA Hidden Gems: Hollinger Game Score",
        subtitle = "Cumulative Impact Score (Raw)",
        x = "", y = "Score"
      )
  }, width = 1000, height = 600)
}

