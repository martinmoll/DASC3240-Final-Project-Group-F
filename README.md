
# WNBA 2019 - Finding Hidden Gems

A Shiny application exploring the 2019 WNBA season through analytics:
using data to identify undervalued bench players whose
per-minute production rivals that of starters - what we call "Hidden Gems".

## How to Run

### Option 1: From GitHub (no cloning needed)

``` r
shiny::runGitHub("DASC3240-Final-Project-Group-F", "martinmoll")
```

Paste in Rstudio's console. 

### Option 2: Clone and run locally

``` bash
git clone https://github.com/martinmoll/DASC3240-Final-Project-Group-F.git
```

Open the `.Rproj` file in RStudio and click "Run App".

## Required Packages

``` r
install.packages(c(
  "shiny", "bslib", "tidyverse", "plotly",
  "bayesrules", "markdown", "gganimate", "gifski"
))
```

## Dataset

-   **Source:** `bayesrules::basketball` (CRAN package), originally from
    [basketball-reference.com](https://www.basketball-reference.com/wnba/players/)
-   **License:** `bayesrules` package is GPL (\>= 3)
-   **Coverage:** 146 WNBA players, 2019 season, 30 variables
-   **Key variables:** per-game averages for points, rebounds, assists,
    steals, blocks, turnovers, field goal/3-point/free throw
    percentages, plus player demographics (age, height, weight) and team
    info

## Methodology

Player value is measured using **Hollinger's Game Score**, a published
composite metric used by NBA/WNBA analysts. We compute both per-game
(raw) and per-minute (time-adjusted) versions. Bench players whose
per-minute Game Score reaches the 75th percentile are classified as
"Hidden Gems."

## App Structure

| File | Description                                                    
|-----------------------------------------------------------------------------|
| `app.R`       | Main app: sources tabs, assembles navbar layout             |
| `scripts/...` | R scripts for our visualizations                            | 
| `about.md`    | Dataset background, license, methodology                    |


## Group Contributions

| Member              | Contribution (files. Not representative of tab order) |
|---------------------|-------------------------------------------------------|
| Lau, Shing Chung    | Introduction file                                     |
| Møllenhus, Martin   | Visualization 1: Dumbbell chart, HGS animation,       |
| Liao, Win           | Visualization 2: Individual Performance, Conclusion   |
| CHAN, Yin Hang Nick | Visualization 3: Scoring method bar charts            |
| Yip, Chi Ho         | Visualization 4: Leaderboard, photos, game score      |

## License

GPL (\>= 3)
