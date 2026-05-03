# -----------------------------------------------------------------------------
# PROJECT INTRODUCTION & OVERVIEW
# -----------------------------------------------------------------------------
# Prefix: vis_intro
# Purpose: Opening tab that frames the Hidden Gems question, provides dataset
#          context, and presents key findings and conclusions.
# -----------------------------------------------------------------------------
library(shiny)
library(bslib)

vis_intro_ui <- function() {
  nav_panel(
    title = "Project Overview",
    icon  = icon("star"),
    
    fluidPage(
      
      # 1. Narrative Section 
      card(
        card_header("Are Bench Stars Real or Flukes?"),
        p("Every year in the WNBA, bench players unexpectedly explode with star-like performance.
           Are these just flukes, or are there hidden stars who simply lack minutes?"),
        p("This app explores the 2019 WNBA season to identify 'Hidden Gems' — undervalued players
           whose per-minute production rivals that of starters."),
        card_footer(
          strong("The Goal:"),
          "Using Moneyball analytics to identify high-efficiency, under-utilized talent
           who deserve more playtime based on statistical impact."
        )
      ),
      
      # 2. Data Context and Cleaning
      layout_column_wrap(
        width = 1/2,
        card(
          card_header("Dataset Summary"),
          p("Sourced from the ", code("bayesrules::basketball"), " dataset (originally via ",
            tags$a("basketball-reference.com", href = "https://www.basketball-reference.com/wnba/players/"),
            ")."),
          tags$ul(
            tags$li(strong("Scope:"), " 2019 WNBA Season"),
            tags$li(strong("Observations:"), " 146 Players"),
            tags$li(strong("Variables:"), " 30 metrics including height, weight, and per-game stats")
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
      
      # 3. Key Findings & Conclusion
      card(
        card_header("Key Findings"),
        card_body(
          
          tags$h5(strong("Rate-Based Efficiency")),
          p("While starters typically produce higher raw totals, per-minute analysis reveals that
             bench players perform at a comparable — and sometimes superior — level. The gap between
             starters and bench is primarily about opportunity, not ability."),
          
          tags$h5(strong("Bench Value & Depth")),
          p("Bench players frequently exceed the 75th percentile of starters in points, rebounds,
             and FG% per minute. In several metrics, top-tier reserves even surpass the maximum
             values recorded by starters, underscoring the strategic importance of roster depth."),
          
          tags$h5(strong("Predictive Success of Hollinger Game Score")),
          p("The top ten players identified by our analysis using Hollinger Game Score have turned
             out to be promising players. Among these candidates, 30% reached All-Star status,
             50% secured championships, and 20% earned Finals MVP honors."),
          
          tags$h5(strong("Top Talent Case Studies")),
          tags$ul(
            tags$li(
              strong("Emma Meesseman:"),
              " Validated as a premier 'hidden gem,' she made history in 2019 as the first reserve
                to be named WNBA Finals MVP, leading her team to a championship. She subsequently
                earned All-Star honors in 2022."
            ),
            tags$li(
              strong("Dearica Hamby:"),
              " A consistent high-performer who translated bench efficiency into a decorated career,
                including a 2022 championship, three All-Star selections (2021, 2022, 2024), and a
                career-best statistical campaign in 2025."
            )
          ),
          
          tags$h5(strong("Scoring Distribution")),
          p("Analysis of scoring distribution — including 2-pointers, 3-pointers, and free throws —
             reveals that bench players maintain scoring efficiency levels nearly identical to those
             of starters, showing no significant drop-off in execution across different shot types.")
        ),
        card_footer(
          strong("Conclusion:"),
          " While starters are elite, the data highlights a wealth of 'hidden gems' within the
            league. When provided with increased opportunity, these players demonstrate the potential
            to match the production and efficiency of established superstars."
        )
      ),
      
      # 4. Licensing
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