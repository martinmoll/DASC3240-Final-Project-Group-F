# -----------------------------------------------------------------------------
# WNBA 2019 Hidden Gem Analysis - Main Application
# -----------------------------------------------------------------------------
# This is the main entry point for the Shiny app. It sources individual
# visualization tabs from separate .R files and assembles them into a
# multi-tab navigation layout using bslib::page_navbar().
#
# To add a new tab:
#   1. Create a file (e.g., visNUMBER_YOURNAME.R) with _ui() and _server() functions
#   2. Add source("visNUMBER_YOURNAME.R") below
#   3. Add the _ui() call inside page_navbar()
#   4. Add the _server() call inside the server function
#
# To run: click "Run App" in RStudio, or use
#   shiny::runGitHub("test1-dasc3240-finalproject", "martinmoll")
# -----------------------------------------------------------------------------


# Source visualization modules ------------------------------------------------
# Each file defines a _ui() and _server() function pair
source("scripts/vis_intro.R")
source("scripts/vis1.R")              # Dumbbell chart: Starters vs. Bench
source("scripts/vis1_animation.R")    # Moneyball Shift animation
source("scripts/Vis_2.R")             # Visualization 2. More info needed(?)
source("scripts/vis3.R")              # Visualization 3. Scoring methods
source("scripts/vis4.R")              # Visualization 4. Leaderboard animation 

# Additional packages needed by app.R itself
library(bslib)
library(markdown)  # Required for includeMarkdown() in the About tab


# -----------------------------------------------------------------------------
# UI: Assemble all tabs into a navbar layout
# -----------------------------------------------------------------------------
ui <- page_navbar(
  title = "WNBA 2019 - Finding Hidden Gems",
  theme = bs_theme(bootswatch = "flatly"),
  
  # --- Visualization tabs ---
  vis_intro_ui(),
  vis1_ui(),               
  vis1_anim_ui(),           
  vis2_ui(),
  vis3_ui(),
  vis4_ui(),
  # source("vis_OTHER.R")   # Tab 3: Add other members' tabs here
  
  
  # --- About tab: dataset background, license, methodology ---
  
  nav_panel("About", icon = icon("info-circle"),
            includeMarkdown("about.md"))
)


# -----------------------------------------------------------------------------
# Server: Wire up all tab server functions
# -----------------------------------------------------------------------------
server <- function(input, output, session) {
  vis_intro_server(input, output, session)
  vis1_server(input, output, session)
  vis1_anim_server(input, output, session)
  vis2_server(input, output, session)
  vis3_server(input, output, session)
  vis4_server(input, output, session)
  # vis_OTHER_server(input, output, session)  # Add other members here
}


# Run the app -----------------------------------------------------------------
shinyApp(ui, server)
