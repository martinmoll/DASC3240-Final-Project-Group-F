### About This App

This application explores the 2019 WNBA season through the lens of
**Moneyball (the movie) analytics**: using data to identify undervalued players
whose on-court production exceeds what their playing time suggests.

---

### The Question

Which bench players are producing at a starter level **per minute**
but not getting the minutes to prove it? And could they be the
league's next breakout stars?

---

### Dataset

**Source:** The `basketball` dataset from the `bayesrules` R package
(v0.0.3+), originally scraped from
[basketball-reference.com](https://www.basketball-reference.com/wnba/players/).

**License:** The `bayesrules` package is distributed under GNU General Public Licence 
version 3.0 (GPL >= 3) on CRAN. The underlying data is sourced from
basketball-reference.com, which provides sports statistics for
personal, non-commercial use by using official sportradar.com data.

**Coverage:** All 146 rostered WNBA players from the 2019 season.

**Structure:** 146 rows x 30 variables, including:

- **Player info:** name, team, age, height, weight
- **Playing time:** games played, games started, total minutes,
  average minutes per game
- **Scoring:** average points, field goal attempts/percentage,
  two-pointer and three-pointer attempts/percentages,
  free throw attempts/percentage
- **Other stats:** rebounds (offensive, defensive, total), assists,
  steals, blocks, turnovers, personal fouls

---

### Data Preparation

The following preprocessing steps are applied:

1. **Role classification:** Players are classified as "Starter"
   (starter == 1) or "Bench" (starter == 0) based on whether they
   started in more than half their games.

2. **Minimum minutes filter:** Players averaging fewer than 5 minutes
   per game are excluded. This removes players with extremely small
   sample sizes whose per-minute stats would be unreliable (e.g.,
   2 points in 1 minute = 2.0 pts/min, which is misleadingly high).

3. **Per-minute stat derivation:** Each per-game average is divided
   by average minutes played to produce per-minute rates. This
   controls for playing time differences between starters and bench.

4. **Hollinger Game Score calculation:** A composite metric using
   the published formula by John Hollinger (see Methodology below).
   Computed in both per-game and per-minute versions.

5. **Moneyball classification:** Bench players whose per-minute
   Game Score is at or above the 75th percentile of all players
   are flagged as "Hidden Gems".

---

### Methodology: Hollinger Game Score

The Game Score was developed by [John Hollinger](https://www.nbastuffer.com/analytics101/game-score/) to provide a single
number that captures a player's total box score contribution.
The formula is:

**Game score = Points Scored + (0.4 x Field Goals) - (0.7 x Field Goal Attempts)- (0.4 x (Free Throw Attempts - Free Throws)) + (0.7 x Offensive Rebounds) + (0.3 x Defensive Rebounds) + Steals + (0.7 x Assists) + (0.7 x Blocks) - (0.4 x Personal Fouls) - Turnovers**

The scale is similar to points scored: approximately 10 represents
an average game, 20 is very good, and 40+ is an elite single-game
performance. Since this dataset contains season averages, most
players fall between 0 and 12 on the per-game version.

We use Game Score rather than a custom composite because it is an
established, peer-recognised metric used by NBA and WNBA analysts,
with published, defensible weights based on basketball domain
expertise.

---

### Why Interactivity?

With 146 players across 12 teams and 30 statistical variables,
static charts cannot capture the full picture. Our interactive
elements provide clear advantages:

- **Dumbbell toggle** (per-minute vs per-game) lets users discover
  the Moneyball insight themselves rather than being told
- **Plotly hover** reveals individual player identities within
  aggregate views
- **Animation** makes the per-game to per-minute shift
  viscerally clear by showing dots physically move
- **Dropdown selectors** let users explore 20+ stat combinations
  without needing 200+ static charts

---

### Colour Scheme

Consistent across all tabs:
- **Blue (#0072B2):** Starters
- **Green (#009E73):** Hidden Gems (bench players above the 75th
  percentile in per-minute Game Score)
- **Yellow (#F0E442):** Bench players (below the threshold)

These colours are adapted from [Bang Wong's colorblind-safe palette](https://www.nature.com/articles/nmeth.1618) to improve accessibility for users with colour vision deficiencies. For additional details, see this [palette reference](https://davidmathlogic.com/colorblind/#%23000000-%23E69F00-%2356B4E9-%23009E73-%23F0E442-%230072B2-%23D55E00-%23CC79A7).

--- 

### Players photos

**License:** All player photos are sourced from Wikimedia Commons and
are distributed under open licenses:
- Most images: **Creative Commons Attribution-ShareAlike 4.0** (CC BY-SA 4.0)
  or similar CC versions (2.0, 3.0, 4.0)
- Some images: **Creative Commons Public Domain** (CC0)
- One image: **Public Domain** (via NASA)

**Attribution:** Detailed license information, photographer credits, and
source URLs for each player photo are provided in
`player_photos_resized/labels.csv`. This file includes:
- License type (CC BY-SA 4.0, CC BY 4.0, CC0, etc.)
- License URL
- Photographer/artist name
- Original source page


