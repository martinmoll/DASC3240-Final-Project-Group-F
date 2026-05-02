# Setup Instructions for Group Members

## Testing the App

### Option 1: Run directly from GitHub(no need to clone)

Open R or RStudio and paste in console:

```r
shiny::runGitHub("test1-dasc3240-finalproject", "martinmoll")
```

If the app launches, the repo is working correctly.

### Option 2: Clone and run locally

```bash
git clone https://github.com/martinmoll/test1-dasc3240-finalproject.git
```

Open the `.Rproj` file in RStudio, then click "Run App".

### Required packages

Make sure these are installed before running:

```r
install.packages(c("shiny", "bslib", "tidyverse", "plotly",
                    "bayesrules", "gganimate", "gifski"))
```

---

## How to Add Your Visualization

Each group member creates **one R file** containing their visualization
as a modular tab. You do NOT need to touch `app.R` yourself — just
follow the template below and tell the person managing `app.R` to
source your file.

### Step 1: Copy this template into a new file

Save it as something like `visNUMBER_YOURNAME.R` in the project root folder
(same directory as `app.R`).

```r
# =============================================================================
# YOUR VISUALIZATION NAME
# =============================================================================
# Replace "vis2" everywhere below with your own prefix (vis3, vis4, etc.)
# to avoid name collisions with other group members.
# =============================================================================

library(shiny)
library(bslib)
library(tidyverse)
library(plotly)
library(bayesrules)

# --- Data preparation (runs once when app loads) ---
data(basketball, package = "bayesrules")

# Do your data cleaning and transformations here
vis2_data <- basketball %>%
  filter(!is.na(avg_minutes_played), avg_minutes_played > 0)
  # Add your own mutate(), filter(), etc.


# =============================================================================
# UI — defines what the user sees
# =============================================================================
vis2_ui <- function() {
  nav_panel(
    title = "Your Tab Title",        # <-- Change this
    icon  = icon("chart-bar"),       # <-- Pick an icon from fontawesome

    layout_sidebar(
      sidebar = sidebar(
        title = "Controls",
        width = 300,

        # Add your inputs here, for example:
        selectInput(
          inputId  = "vis2_variable",
          label    = "Choose a stat:",
          choices  = c("Points" = "avg_points",
                       "Rebounds" = "avg_rb",
                       "Assists" = "avg_assists"),
          selected = "avg_points"
        )
      ),

      # Main panel — your plot goes here
      card(
        card_header("Your Chart Title"),
        card_body(
          plotlyOutput("vis2_plot", height = "500px")
        )
      )
    )
  )
}


# =============================================================================
# SERVER — defines the logic behind the UI
# =============================================================================
vis2_server <- function(input, output, session) {

  output$vis2_plot <- renderPlotly({
    # Use input$vis2_variable to access the dropdown selection
    y_var <- input$vis2_variable

    p <- ggplot(vis2_data, aes(
        x    = avg_minutes_played,
        y    = .data[[y_var]],
        text = paste0("Player: ", player_name,
                      "\nTeam: ", team)
      )) +
      geom_point(alpha = 0.6, size = 2) +
      theme_minimal(base_size = 13) +
      labs(x = "Minutes per Game", y = y_var)

    ggplotly(p, tooltip = "text")
  })
}
```

### Step 2: Important naming rules

- **All input IDs must be unique across the entire app.** Prefix
  everything with your vis number. For example: `vis2_variable`,
  `vis2_plot`, `vis3_team_filter`, etc. If two people both use
  `inputId = "metric"`, the app will break.

- **All output IDs must be unique too.** Use `vis2_plot`, `vis3_chart`,
  etc. Never use generic names like `"mainPlot"` or `"plot1"`.

- **Function names must be unique.** Name your functions `vis2_ui` /
  `vis2_server`, `vis3_ui` / `vis3_server`, etc.

### Step 3: Test your file standalone

Before pushing, test that your tab works on its own. Create a temporary
test file (do not push this):

```r
source("vis_yourname.R")

ui <- page_navbar(
  title = "Test",
  theme = bs_theme(bootswatch = "flatly"),
  vis2_ui()
)

server <- function(input, output, session) {
  vis2_server(input, output, session)
}

shinyApp(ui, server)
```

If this runs without errors, your tab is ready to integrate.

### Step 4: Push your file to the repo

```bash
git pull origin main
git add vis_yourname.R
git commit -m "Add [your name]'s visualization tab"
git push origin main
```

### Step 5: Tell the app.R manager to add your tab

The person managing `app.R` adds three lines:

At the top:
```r
source("vis_yourname.R")
```

Inside `page_navbar()`:
```r
vis2_ui(),
```

Inside the server function:
```r
vis2_server(input, output, session)
```

---

## Converting Existing Code to This Format

If you already have a working visualization in a `.qmd` file or a
standalone `app.R`, here is how to convert it:

1. **Move your data loading and cleaning** into the top of your new
   file (outside any function). This runs once when the app starts.

2. **Move your UI elements** (inputs like `selectInput`, `sliderInput`,
   and outputs like `plotlyOutput`) into your `_ui()` function, wrapped
   inside `nav_panel()` and `layout_sidebar()`.

3. **Move your server logic** (`renderPlotly`, `reactive`, etc.) into
   your `_server()` function.

4. **Rename all IDs** to have your unique prefix.

5. **Remove** any `shinyApp(ui, server)` call from your file — that
   only goes in `app.R`.

6. **Remove** any `fluidPage()` or `titlePanel()` — the overall page
   layout is handled by `app.R`.

7. **Do not use `read.csv()` with absolute paths** like
   `/Users/yourname/Desktop/...`. Use `bayesrules::basketball` or
   relative paths to files in the repo.

---

## Current File Structure

```
test1-dasc3240-finalproject/
├── app.R                    # Main app - sources all tabs
├── vis1.R                   # Martin's dumbbell chart
├── vis_animation.R          # Martin's Moneyball Shift animation
├── vis_yourname.R           # YOUR file goes here
├── test1-dasc3240-finalproject.Rproj
└── .gitignore
```

---

## Quick Reference: Useful Shiny Input Widgets

| Widget | Code | Use for |
|--------|------|---------|
| Dropdown | `selectInput("id", "Label", choices)` | Picking a stat or team |
| Radio buttons | `radioButtons("id", "Label", choices)` | Toggling between views |
| Checkboxes | `checkboxGroupInput("id", "Label", choices)` | Filtering teams |
| Slider | `sliderInput("id", "Label", min, max, value)` | Filtering ranges |
| Button | `actionButton("id", "Label")` | Triggering actions |

## Quick Reference: Useful Shiny Output Widgets

| Widget | UI function | Server function |
|--------|-------------|-----------------|
| Plotly chart | `plotlyOutput("id")` | `renderPlotly({})` |
| Static plot | `plotOutput("id")` | `renderPlot({})` |
| Table | `tableOutput("id")` | `renderTable({})` |
| Text | `textOutput("id")` | `renderText({})` |
| Image/GIF | `imageOutput("id")` | `renderImage({})` |
