# =============================================================================
# PROJECT INTRODUCTION & OVERVIEW
# =============================================================================
# Prefix: vis_intro
# =============================================================================

library(shiny)
library(bslib)

vis_intro_ui <- function() {
  nav_panel(
    title = "Project Overview",
    icon  = icon("star"),
    
    fluidPage(
      theme = bs_theme(bootswatch = "flatly"),
      
      # 1. Narrative Section (Storytelling & "So What?")
      card(
        card_header("Are Bench Stars Real or Flukes?"),
        p("Every year in the WNBA, bench players unexpectedly explode with star-like performance. 
           Are these just flukes, or are there hidden stars who simply lack minutes?"),
        p("This app explores the 2019 WNBA season to identify 'Hidden Gems'—undervalued players 
           whose per-minute production rivals that of starters."),
        card_footer(
          strong("The Goal:"), 
          "Using Moneyball analytics to identify high-efficiency, under-utilized talent 
           who deserve more playtime based on statistical impact."
        )
      ),
      
      # 2. Data Context and Cleaning (Addressing Win's Feedback & Rubric)
      layout_column_wrap(
        width = 1/2,
        card(
          card_header("Dataset Summary"),
          p("Sourced from the ", code("bayesrules::basketball"), " dataset (originally via basketball-reference.com)."),
          tags$ul(
            tags$li(strong("Scope:"), " 2019 WNBA Season"),
            tags$li(strong("Observations:"), " 146 Players"),
            tags$li(strong("Variables:"), " 33 metrics including height, weight, and per-game stats")
          )
        ),
        card(
          card_header("Data Cleaning Steps"),
          p("To ensure statistical integrity and meaningful comparisons, we applied the following logic:"),
          tags$ul(
            tags$li(strong("Trade Handling:"), " Retained only the latest team's data for players who switched teams mid-season."),
            tags$li(strong("Stability Filter:"), " Removed players averaging < 8 minutes per game to eliminate small-sample outliers."),
            tags$li(strong("Efficiency Metric:"), " Calculated Hollinger's Game Score to measure total impact per minute.")
          )
        )
      ),
      
      # 3. Licensing (Rubric Requirement)
      hr(),
      tags$small(
        style = "color: #777;",
        "License: This project uses open-source data under the GPL (>= 3) License."
      )
    )
  )
}

# Server logic remains empty as this is a static informational tab
vis_intro_server <- function(input, output, session) {
  # No reactive logic required for the intro tab
}

