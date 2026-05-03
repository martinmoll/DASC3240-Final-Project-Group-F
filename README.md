# WNBA 2019 Moneyball Analysis

A Shiny application exploring the 2019 WNBA season through Moneyball
analytics: using data to identify undervalued bench players whose
per-minute production rivals that of starters.

## How to Run

### Option 1: From GitHub (no cloning needed)

```r
shiny::runGitHub("test1-dasc3240-finalproject", "martinmoll")
```
Paste in Rstudio's console.
### Option 2: Clone and run locally

```bash
git clone https://github.com/martinmoll/test1-dasc3240-finalproject.git
```

Open the `.Rproj` file in RStudio and click "Run App".

## Required Packages

```r
install.packages(c(
  "shiny", "bslib", "tidyverse", "plotly",
  "bayesrules", "markdown", "gganimate", "gifski"
))
```

## Dataset

- **Source:** `bayesrules::basketball` (CRAN package), originally from
  [basketball-reference.com](https://www.basketball-reference.com/wnba/players/)
- **License:** `bayesrules` package is GPL (>= 3)
- **Coverage:** 146 WNBA players, 2019 season, 30 variables
- **Key variables:** per-game averages for points, rebounds, assists,
  steals, blocks, turnovers, field goal/3-point/free throw percentages,
  plus player demographics (age, height, weight) and team info

## Methodology

Player value is measured using **Hollinger's Game Score**, a published
composite metric used by NBA/WNBA analysts. We compute both per-game
(raw) and per-minute (time-adjusted) versions. Bench players whose
per-minute Game Score reaches the 75th percentile are classified as
"Hidden Gems."

## App Structure

| File | Description |
|------|-------------|
| `app.R` | Main app: sources tabs, assembles navbar layout |
| `vis1.R` | Dumbbell chart comparing starters vs. bench |
| `vis1_animation.R` | Animated Moneyball Shift (gganimate) |
| `about.md` | Dataset background, license, methodology |
| `setup_instructions.md` | Guide for group members adding tabs |
| `TEMPLATE_visX.R` | Template for new visualization tabs |

## Group Contributions

| Member | Contribution |
|--------|-------------|
| Martin Møllenhus | Visualization 1: dumbbell chart, Game Score animation |
| [Member 2] | [Description] |
| CHAN, Yin Hang Nick | Visualization 3: Scoring method bar charts |
| YIP, Chi Ho | Visualization 4: players leaderboard, Game score research, players photos dataset |
| [Member 5] | [Description] |

## License

GPL (>= 3)
