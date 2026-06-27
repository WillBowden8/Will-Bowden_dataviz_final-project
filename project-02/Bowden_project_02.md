---
title: "Data Visualization Mini-Project 2: Births, Beats & Basketball"
author: "Will Bowden — wbowden@floridapoly.edu"
date: "2026-06-26"
output:
  html_document:
    keep_md: true
    theme: flatly
    highlight: tango
    toc: true
    toc_float:
      collapsed: false
    self_contained: true
    code_folding: hide
---




``` r
library(tidyverse)
library(plotly)
library(sf)
library(broom)
library(scales)

# Shared project theme applied to all ggplot2 figures
project_theme <- theme_minimal(base_size = 13) +
  theme(
    plot.title       = element_text(face = "bold", size = 15, margin = margin(b = 5)),
    plot.subtitle    = element_text(color = "gray45", margin = margin(b = 10)),
    plot.caption     = element_text(color = "gray55", size = 9, hjust = 0),
    panel.grid.minor = element_blank(),
    legend.position  = "bottom"
  )

# Okabe-Ito colorblind-safe 8-color palette
pal <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442",
         "#0072B2", "#D55E00", "#CC79A7", "#000000")
```

---

## Introduction

This report explores three distinct datasets to surface patterns across American demography, pop culture, and sports.

- **U.S. Births (2000–2014):** How do birth counts vary by season, year, and day of the week?
- **Billboard Summer Hits (1958–2017):** How have the sonic qualities of summer pop hits changed over decades?
- **NBA Champions (1980–2018):** What statistical profile characterizes championship-winning teams, and which cities have dominated?

---

## Data

### Loading


``` r
births    <- read_csv("../data/us_births_00_14.csv")
billboard <- read_csv("../data/all_billboard_summer_hits.csv")
nba       <- read_csv("../data/NBAchampionsdata.csv")
```

### U.S. Births (2000–2014)


``` r
glimpse(births)
```

```
## Rows: 5,479
## Columns: 6
## $ year          <dbl> 2000, 2000, 2000, 2000, 2000, 2000, 2000, 2…
## $ month         <dbl> 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1…
## $ date_of_month <dbl> 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, …
## $ date          <date> 2000-01-01, 2000-01-02, 2000-01-03, 2000-0…
## $ day_of_week   <chr> "Sat", "Sun", "Mon", "Tues", "Wed", "Thurs"…
## $ births        <dbl> 9083, 8006, 11363, 13032, 12558, 12466, 125…
```

The dataset contains **5479 rows**, each representing a single calendar day. Columns include `year`, `month`, `date`, `day_of_week`, and `births`.

**Missing values:**


``` r
births |>
  summarise(across(everything(), ~sum(is.na(.)))) |>
  pivot_longer(everything(), names_to = "Column", values_to = "Missing") |>
  knitr::kable()
```



|Column        | Missing|
|:-------------|-------:|
|year          |       0|
|month         |       0|
|date_of_month |       0|
|date          |       0|
|day_of_week   |       0|
|births        |       0|

No missing values. The `day_of_week` column requires factor reordering before plotting.

---

### Billboard Summer Hits (1958–2017)


``` r
glimpse(billboard)
```

```
## Rows: 600
## Columns: 22
## $ danceability     <dbl> 0.518, 0.543, 0.541, 0.408, 0.554, 0.679…
## $ energy           <dbl> 0.060, 0.332, 0.676, 0.397, 0.189, 0.279…
## $ key              <chr> "A#", "C", "C", "A", "E", "G", "F#", "B"…
## $ loudness         <dbl> -14.887, -11.573, -7.988, -12.536, -14.2…
## $ mode             <chr> "major", "major", "major", "major", "maj…
## $ speechiness      <dbl> 0.0441, 0.0317, 0.1350, 0.0300, 0.0279, …
## $ acousticness     <dbl> 0.9870, 0.6690, 0.1880, 0.8730, 0.9150, …
## $ instrumentalness <dbl> 7.87e-06, 0.00e+00, 8.03e-01, 0.00e+00, …
## $ liveness         <dbl> 0.1610, 0.1340, 0.1230, 0.2800, 0.1320, …
## $ valence          <dbl> 0.336, 0.795, 0.911, 0.697, 0.214, 0.854…
## $ tempo            <dbl> 127.870, 154.999, 76.231, 72.615, 136.71…
## $ track_uri        <chr> "006Ndmw2hHxvnLbJsBFnPx", "5ayybTSXNwcar…
## $ duration_ms      <dbl> 216373, 153933, 128360, 162773, 165293, …
## $ time_signature   <dbl> 4, 4, 4, 4, 3, 3, 4, 4, 4, 4, 3, 4, 4, 4…
## $ key_mode         <chr> "A# major", "C major", "C major", "A maj…
## $ playlist_name    <chr> "summer_hits_1958", "summer_hits_1958", …
## $ playlist_img     <chr> "https://mosaic.scdn.co/640/5e8c49f7a8d1…
## $ track_name       <chr> "Nel blu dipinto di blu", "Poor Little F…
## $ artist_name      <chr> "Domenico Modugno", "Ricky Nelson", "Pér…
## $ album_name       <chr> "Tutto Modugno (Mister Volare)", "Ricky …
## $ album_img        <chr> "https://i.scdn.co/image/5e8c49f7a8d161c…
## $ year             <dbl> 1958, 1958, 1958, 1958, 1958, 1958, 1958…
```

The dataset contains **600 songs** with Spotify audio features. Relevant features include `danceability`, `energy`, `valence`, `acousticness`, `tempo`, `loudness`, and `year`.

**Missing values:**


``` r
billboard |>
  summarise(across(everything(), ~sum(is.na(.)))) |>
  pivot_longer(everything(), names_to = "Column", values_to = "Missing") |>
  filter(Missing > 0) |>
  knitr::kable()
```



|Column | Missing|
|:------|-------:|

No missing values in the columns used for analysis.

---

### NBA Champions (1980–2018)


``` r
glimpse(nba)
```

```
## Rows: 220
## Columns: 24
## $ Year <dbl> 1980, 1980, 1980, 1980, 1980, 1980, 1981, 1981, 1981…
## $ Team <chr> "Lakers", "Lakers", "Lakers", "Lakers", "Lakers", "L…
## $ Game <dbl> 1, 2, 3, 4, 5, 6, 1, 2, 3, 4, 5, 6, 1, 2, 3, 4, 5, 6…
## $ Win  <dbl> 1, 0, 1, 0, 1, 1, 1, 0, 1, 0, 1, 1, 1, 0, 1, 1, 0, 1…
## $ Home <dbl> 1, 1, 0, 0, 1, 0, 1, 1, 0, 0, 1, 0, 0, 0, 1, 1, 0, 1…
## $ MP   <dbl> 240, 240, 240, 240, 240, 240, 240, 240, 240, 240, 24…
## $ FG   <dbl> 48, 48, 44, 44, 41, 45, 41, 41, 40, 35, 41, 43, 49, …
## $ FGA  <dbl> 89, 95, 92, 93, 91, 92, 95, 82, 89, 74, 94, 78, 93, …
## $ FGP  <dbl> 0.539, 0.505, 0.478, 0.473, 0.451, 0.489, 0.432, 0.5…
## $ TP   <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 2, 0, 0, 1, 0, 0, 1, 0, 0, 0…
## $ TPA  <dbl> 0, 1, 1, 0, 0, 2, 1, 3, 3, 3, 3, 4, 0, 5, 1, 1, 2, 0…
## $ TPP  <dbl> NA, 0.000, 0.000, NA, NA, 0.000, 0.000, 0.000, 0.667…
## $ FT   <dbl> 13, 8, 23, 14, 26, 33, 16, 8, 12, 16, 27, 15, 26, 24…
## $ FTA  <dbl> 15, 12, 30, 19, 33, 35, 20, 13, 19, 24, 35, 18, 35, …
## $ FTP  <dbl> 0.867, 0.667, 0.767, 0.737, 0.788, 0.943, 0.800, 0.6…
## $ ORB  <dbl> 12, 15, 22, 18, 19, 17, 25, 14, 16, 17, 19, 9, 19, 1…
## $ DRB  <dbl> 31, 37, 34, 31, 37, 35, 29, 34, 28, 30, 35, 28, 31, …
## $ TRB  <dbl> 43, 52, 56, 49, 56, 52, 54, 48, 44, 47, 54, 37, 50, …
## $ AST  <dbl> 30, 32, 20, 23, 28, 27, 23, 17, 24, 22, 25, 26, 34, …
## $ STL  <dbl> 5, 12, 5, 12, 7, 14, 6, 6, 12, 5, 5, 6, 11, 11, 15, …
## $ BLK  <dbl> 9, 7, 5, 6, 6, 4, 5, 7, 6, 6, 8, 0, 7, 6, 5, 4, 9, 1…
## $ TOV  <dbl> 17, 26, 20, 19, 21, 17, 19, 22, 11, 22, 14, 13, 22, …
## $ PF   <dbl> 24, 27, 25, 22, 27, 22, 21, 27, 25, 22, 23, 21, 26, …
## $ PTS  <dbl> 109, 104, 111, 102, 108, 123, 98, 90, 94, 86, 109, 1…
```

Each row is a single Finals game for that year's champion. With **220 games** across 39 championship years.

**Missing values:**


``` r
nba |>
  summarise(across(everything(), ~sum(is.na(.)))) |>
  pivot_longer(everything(), names_to = "Column", values_to = "Missing") |>
  filter(Missing > 0) |>
  knitr::kable()
```



|Column | Missing|
|:------|-------:|
|TPP    |       6|

`TPP` (three-point field goal percentage) is missing when a team attempted zero three-pointers — a structural NA, not a data quality issue.

---

## Data Cleaning & Preparation


``` r
dow_levels <- c("Mon", "Tues", "Wed", "Thurs", "Fri", "Sat", "Sun")

births <- births |>
  mutate(
    day_of_week = factor(day_of_week, levels = dow_levels),
    month_name  = factor(month.abb[month], levels = month.abb)
  )
```


``` r
billboard <- billboard |>
  mutate(decade = paste0(floor(year / 10) * 10, "s"))
```


``` r
nba <- nba |>
  mutate(
    Team = str_trim(str_remove_all(Team, "'")),
    Team = case_when(
      Team == "Warriorrs" ~ "Warriors",
      TRUE                ~ Team
    )
  )

champs_count <- nba |>
  distinct(Year, Team) |>
  count(Team, name = "championships", sort = TRUE)

champs_count |> knitr::kable(caption = "NBA Championships by Team (1980–2018)")
```



Table: NBA Championships by Team (1980–2018)

|Team      | championships|
|:---------|-------------:|
|Lakers    |            10|
|Bulls     |             6|
|Spurs     |             5|
|Celtics   |             4|
|Heat      |             3|
|Pistons   |             3|
|Warriors  |             3|
|Rockets   |             2|
|Cavaliers |             1|
|Mavericks |             1|
|Sixers    |             1|


``` r
# City coordinates for each franchise
team_cities <- tribble(
  ~Team,       ~City,           ~lon,      ~lat,
  "Lakers",    "Los Angeles",   -118.243,   34.052,
  "Celtics",   "Boston",         -71.057,   42.361,
  "Sixers",    "Philadelphia",   -75.165,   39.952,
  "Pistons",   "Detroit",        -83.045,   42.331,
  "Bulls",     "Chicago",        -87.629,   41.878,
  "Rockets",   "Houston",        -95.370,   29.760,
  "Spurs",     "San Antonio",    -98.494,   29.424,
  "Heat",      "Miami",          -80.191,   25.774,
  "Mavericks", "Dallas",         -96.797,   32.776,
  "Warriors",  "San Francisco", -122.419,   37.774,
  "Cavaliers", "Cleveland",      -81.694,   41.499,
  "Raptors",   "Toronto",        -79.383,   43.653,
  "Thunder",   "Oklahoma City",  -97.519,   35.467,
  "Bucks",     "Milwaukee",      -87.907,   43.038
)
```

---

## Visualizations

### Plot 1 — U.S. Births: Monthly Heatmap (2000–2014)

This tile heatmap encodes average daily births using a blue color gradient, allowing simultaneous inspection of seasonal patterns (rows) and year-over-year changes (columns).


``` r
births_monthly <- births |>
  group_by(year, month, month_name) |>
  summarise(avg_births = mean(births), .groups = "drop")

min_cell <- births_monthly |> slice_min(avg_births, n = 1)

ggplot(births_monthly, aes(x = factor(year), y = fct_rev(month_name), fill = avg_births)) +
  geom_tile(color = "white", linewidth = 0.4) +
  geom_text(
    data = min_cell,
    aes(x = factor(year), y = fct_rev(month_name), label = "Lowest\naverage"),
    color = "white", size = 2.6, fontface = "bold"
  ) +
  scale_fill_gradient(
    low  = "#d6eaf8",
    high = "#1a5276",
    labels = comma,
    name = "Avg. Daily Births"
  ) +
  labs(
    title    = "Average Daily U.S. Births by Month and Year (2000–2014)",
    subtitle = "September peaks each year (~9 months after the holiday season); the post-2007 recession\nis visible as a gradual darkening across all months.",
    x = "Year", y = NULL,
    caption  = "Source: FiveThirtyEight · us_births_00_14.csv"
  ) +
  project_theme +
  theme(
    axis.text.x      = element_text(angle = 45, hjust = 1),
    legend.key.width = unit(2, "cm")
  )
```

<img src="C:\Users\Will Bowden\Documents\Will-Bowden_dataviz_final-project\project-02\Bowden_project_02_files/figure-html/births-heatmap-1.png" alt="Tile heatmap showing average daily U.S. births by month (rows) and year (columns) from 2000 to 2014. September is consistently the darkest row, indicating the most births. Color lightens across columns from 2007 onward, reflecting a post-recession decline in birth rates. The lowest cell (February 2014) is annotated." style="display: block; margin: auto;" />

**Takeaway:** September is consistently the busiest birth month — linked to peak conception around the holiday period. Birth levels decline measurably from 2007 onward, corresponding to the Great Recession.

---

### Plot 2 — U.S. Births by Day of Week


``` r
births_dow <- births |>
  group_by(day_of_week) |>
  summarise(avg_births = mean(births), .groups = "drop")

weekday_avg <- births_dow |> filter(!day_of_week %in% c("Sat", "Sun")) |>
  summarise(m = mean(avg_births)) |> pull()
weekend_avg <- births_dow |> filter(day_of_week %in% c("Sat", "Sun")) |>
  summarise(m = mean(avg_births)) |> pull()
pct_diff <- round((weekday_avg - weekend_avg) / weekday_avg * 100)

ggplot(births_dow, aes(x = day_of_week, y = avg_births)) +
  geom_col(fill = "#0072B2", width = 0.7) +
  geom_text(
    aes(label = comma(round(avg_births))),
    vjust = -0.5, size = 3.4, color = "gray30"
  ) +
  annotate(
    "rect",
    xmin = 5.5, xmax = 7.5,
    ymin = 0,   ymax = max(births_dow$avg_births) * 1.14,
    fill = "#D55E00", alpha = 0.10
  ) +
  annotate(
    "text", x = 6.5, y = max(births_dow$avg_births) * 1.10,
    label = paste0("Weekend:\n~", pct_diff, "% fewer\nbirths than weekdays"),
    color = "#D55E00", size = 3.3, fontface = "italic"
  ) +
  scale_y_continuous(labels = comma, expand = expansion(mult = c(0, 0.18))) +
  labs(
    title    = "Average U.S. Daily Births by Day of Week (2000–2014)",
    subtitle = "The weekday/weekend gap reflects the scheduling of elective inductions and C-sections",
    x = NULL, y = "Average Daily Births",
    caption  = "Source: FiveThirtyEight · us_births_00_14.csv"
  ) +
  project_theme
```

<img src="C:\Users\Will Bowden\Documents\Will-Bowden_dataviz_final-project\project-02\Bowden_project_02_files/figure-html/births-dow-1.png" alt="Bar chart of average daily U.S. births by day of week from 2000 to 2014. Weekdays (Monday through Friday) average around 12,000 to 13,000 births. Saturday and Sunday drop to roughly 8,000, approximately 23 percent fewer than weekdays. The weekend bars are highlighted in red." style="display: block; margin: auto;" />

**Takeaway:** Saturday and Sunday record roughly 37% fewer births than the midweek peak — a direct reflection of hospital scheduling practices for planned deliveries.

---

### Plot 3 — Billboard Summer Hits: Danceability vs. Positivity *(Interactive)*

Hover over any point to see the track name, artist, year, and audio feature values. Points are colored by decade using a colorblind-safe palette.

> **Interactivity:** Hovering reveals individual song details (title, artist, year, audio feature values) that are impossible to show in a static scatter of 600 points. Clicking decade labels in the legend toggles those years on/off for focused comparison.


``` r
p_bb <- billboard |>
  ggplot(aes(
    x     = danceability,
    y     = valence,
    color = decade,
    text  = paste0(
      "<b>", track_name, "</b><br>",
      artist_name, "  (", year, ")<br>",
      "Danceability: ", round(danceability, 2), "<br>",
      "Valence: ",      round(valence, 2), "<br>",
      "Energy: ",       round(energy, 2)
    )
  )) +
  geom_point(alpha = 0.75, size = 2) +
  scale_color_manual(values = pal, name = "Decade") +
  labs(
    title = "Billboard Summer Hits: Danceability vs. Emotional Positivity (1958–2017)",
    x = "Danceability",
    y = "Valence  (0 = negative / sad,  1 = positive / happy)",
    caption = "Source: Spotify · all_billboard_summer_hits.csv"
  ) +
  project_theme

ggplotly(p_bb, tooltip = "text") |>
  layout(
    title = list(
      text = paste0(
        "Billboard Summer Hits: Danceability vs. Emotional Positivity",
        "<br><sup>Hover over a point to see track details · colored by decade · click legend to toggle</sup>"
      )
    ),
    legend = list(orientation = "h", y = -0.15)
  )
```

```{=html}
<div class="plotly html-widget html-fill-item" id="htmlwidget-063097dba70432243285" style="width:960px;height:576px;"></div>
<script type="application/json" data-for="htmlwidget-063097dba70432243285">{"x":{"data":[{"x":[0.51800000000000002,0.54300000000000004,0.54100000000000004,0.40799999999999997,0.55400000000000005,0.67900000000000005,0.66300000000000003,0.68400000000000005,0.64500000000000002,0.38800000000000001,0.55600000000000005,0.70299999999999996,0.84299999999999997,0.55100000000000005,0.45500000000000002,0.58799999999999997,0.71499999999999997,0.46800000000000003,0.46300000000000002,0.56799999999999995],"y":[0.33600000000000002,0.79500000000000004,0.91100000000000003,0.69699999999999995,0.214,0.85399999999999998,0.97899999999999998,0.86699999999999999,0.96499999999999997,0.873,0.84799999999999998,0.92100000000000004,0.622,0.95999999999999996,0.32400000000000001,0.88900000000000001,0.85899999999999999,0.47899999999999998,0.96399999999999997,0.94699999999999995],"text":["<b>Nel blu dipinto di blu<\/b><br>Domenico Modugno  (1958)<br>Danceability: 0.52<br>Valence: 0.34<br>Energy: 0.06","<b>Poor Little Fool<\/b><br>Ricky Nelson  (1958)<br>Danceability: 0.54<br>Valence: 0.8<br>Energy: 0.33","<b>Patricia<\/b><br>Pérez Prado  (1958)<br>Danceability: 0.54<br>Valence: 0.91<br>Energy: 0.68","<b>Little Star<\/b><br>The Elegants  (1958)<br>Danceability: 0.41<br>Valence: 0.7<br>Energy: 0.4","<b>My True Love<\/b><br>Jack Scott  (1958)<br>Danceability: 0.55<br>Valence: 0.21<br>Energy: 0.19","<b>Just A Dream<\/b><br>Jimmy Clanton  (1958)<br>Danceability: 0.68<br>Valence: 0.85<br>Energy: 0.28","<b>When (Originally Performed By The Kalin Twins) [Full Vocal Version]<\/b><br>Paris Music  (1958)<br>Danceability: 0.66<br>Valence: 0.98<br>Energy: 0.62","<b>Bird Dog<\/b><br>The Everly Brothers  (1958)<br>Danceability: 0.68<br>Valence: 0.87<br>Energy: 0.56","<b>Splish Splash<\/b><br>Bobby Darin  (1958)<br>Danceability: 0.64<br>Valence: 0.96<br>Energy: 0.94","<b>Rebel Rouser<\/b><br>Duane Eddy  (1958)<br>Danceability: 0.39<br>Valence: 0.87<br>Energy: 0.43","<b>Lonely Boy<\/b><br>Paul Anka  (1959)<br>Danceability: 0.56<br>Valence: 0.85<br>Energy: 0.46","<b>The Battle Of New Orleans<\/b><br>Johnny Horton  (1959)<br>Danceability: 0.7<br>Valence: 0.92<br>Energy: 0.75","<b>My Heart Is An Open Book<\/b><br>Carl Dobkins  (1959)<br>Danceability: 0.84<br>Valence: 0.62<br>Energy: 0.12","<b>A Big Hunk O' Love<\/b><br>Elvis Presley  (1959)<br>Danceability: 0.55<br>Valence: 0.96<br>Energy: 0.93","<b>The Three Bells (Les Trois Cloches)<\/b><br>The Browns  (1959)<br>Danceability: 0.46<br>Valence: 0.32<br>Energy: 0.33","<b>Personality<\/b><br>Lloyd Price  (1959)<br>Danceability: 0.59<br>Valence: 0.89<br>Energy: 0.45","<b>There Goes My Baby<\/b><br>The Drifters  (1959)<br>Danceability: 0.72<br>Valence: 0.86<br>Energy: 0.57","<b>Lavender Blue<\/b><br>Sammy Turner  (1959)<br>Danceability: 0.47<br>Valence: 0.48<br>Energy: 0.28","<b>Waterloo<\/b><br>Stonewall Jackson  (1959)<br>Danceability: 0.46<br>Valence: 0.96<br>Energy: 0.61","<b>Tiger<\/b><br>Fabian  (1959)<br>Danceability: 0.57<br>Valence: 0.95<br>Energy: 0.75"],"type":"scatter","mode":"markers","marker":{"autocolorscale":false,"color":"rgba(230,159,0,1)","opacity":0.75,"size":7.559055118110237,"symbol":"circle","line":{"width":1.8897637795275593,"color":"rgba(230,159,0,1)"}},"hoveron":"points","name":"1950s","legendgroup":"1950s","showlegend":true,"xaxis":"x","yaxis":"y","hoverinfo":"text","frame":null},{"x":[0.55800000000000005,0.64300000000000002,0.58899999999999997,0.65600000000000003,0.79500000000000004,0.56999999999999995,0.46600000000000003,0.498,0.51000000000000001,0.70099999999999996,0.56899999999999995,0.46300000000000002,0.78600000000000003,0.57699999999999996,0.52000000000000002,0.57499999999999996,0.84499999999999997,0.61599999999999999,0.70199999999999996,0.72299999999999998,0.51500000000000001,0.38900000000000001,0.48299999999999998,0.73399999999999999,0.63600000000000001,0.76400000000000001,0.47399999999999998,0.28299999999999997,0.64300000000000002,0.47299999999999998,0.32100000000000001,0.46899999999999997,0.79800000000000004,0.53800000000000003,0.41499999999999998,0.46000000000000002,0.38100000000000001,0.67900000000000005,0.64400000000000002,0.48099999999999998,0.55200000000000005,0.39000000000000001,0.26000000000000001,0.58999999999999997,0.28899999999999998,0.77700000000000002,0.747,0.47799999999999998,0.67900000000000005,0.48099999999999998,0.72499999999999998,0.68600000000000005,0.61699999999999999,0.55200000000000005,0.64400000000000002,0.45600000000000002,0.378,0.23699999999999999,0.36599999999999999,0.63,0.45800000000000002,0.42299999999999999,0.752,0.64600000000000002,0.502,0.71099999999999997,0.22900000000000001,0.64400000000000002,0.629,0.51900000000000002,0.41299999999999998,0.67700000000000005,0.58099999999999996,0.51500000000000001,0.371,0.40000000000000002,0.249,0.56599999999999995,0.68100000000000005,0.68100000000000005,0.63500000000000001,0.59699999999999998,0.67000000000000004,0.67200000000000004,0.55700000000000005,0.64700000000000002,0.54800000000000004,0.59399999999999997,0.47399999999999998,0.439,0.437,0.56200000000000006,0.60299999999999998,0.35499999999999998,0.69199999999999995,0.57499999999999996,0.69399999999999995,0.81200000000000006,0.53800000000000003,0.68500000000000005],"y":[0.30299999999999999,0.753,0.78100000000000003,0.88600000000000001,0.96799999999999997,0.93400000000000005,0.95499999999999996,0.86599999999999999,0.54500000000000004,0.84799999999999998,0.96499999999999997,0.89800000000000002,0.90200000000000002,0.81299999999999994,0.64800000000000002,0.73999999999999999,0.91800000000000004,0.68500000000000005,0.61799999999999999,0.85899999999999999,0.53400000000000003,0.55700000000000005,0.56399999999999995,0.96599999999999997,0.88700000000000001,0.875,0.58199999999999996,0.874,0.89600000000000002,0.53300000000000003,0.33500000000000002,0.94199999999999995,0.84599999999999997,0.41899999999999998,0.40400000000000003,0.72599999999999998,0.55700000000000005,0.96399999999999997,0.71499999999999997,0.874,0.83299999999999996,0.51400000000000001,0.39900000000000002,0.79700000000000004,0.28000000000000003,0.63900000000000001,0.77800000000000002,0.94299999999999995,0.72799999999999998,0.68000000000000005,0.90400000000000003,0.96399999999999997,0.61899999999999999,0.84599999999999997,0.96299999999999997,0.63900000000000001,0.61899999999999999,0.47899999999999998,0.52500000000000002,0.59099999999999997,0.67100000000000004,0.55900000000000005,0.56599999999999995,0.89500000000000002,0.80500000000000005,0.66100000000000003,0.52600000000000002,0.85199999999999998,0.627,0.80300000000000005,0.44,0.89800000000000002,0.56100000000000005,0.80200000000000005,0.89000000000000001,0.65300000000000002,0.435,0.64900000000000002,0.316,0.79500000000000004,0.246,0.96299999999999997,0.95999999999999996,0.92100000000000004,0.73299999999999998,0.92200000000000004,0.61699999999999999,0.71699999999999997,0.71299999999999997,0.53300000000000003,0.58099999999999996,0.96599999999999997,0.81699999999999995,0.161,0.58199999999999996,0.39600000000000002,0.86199999999999999,0.874,0.52700000000000002,0.89200000000000002],"text":["<b>I'm Sorry<\/b><br>Brenda Lee  (1960)<br>Danceability: 0.56<br>Valence: 0.3<br>Energy: 0.22","<b>It's Now or Never<\/b><br>Elvis Presley  (1960)<br>Danceability: 0.64<br>Valence: 0.75<br>Energy: 0.49","<b>Everybody's Somebody's Fool<\/b><br>Connie Francis  (1960)<br>Danceability: 0.59<br>Valence: 0.78<br>Energy: 0.64","<b>Alley Oop<\/b><br>Hollywood Argyles  (1960)<br>Danceability: 0.66<br>Valence: 0.89<br>Energy: 0.56","<b>Itsy Bitsy Teenie Weenie Yellow Polka-Dot Bikini<\/b><br>Brian Hyland  (1960)<br>Danceability: 0.8<br>Valence: 0.97<br>Energy: 0.4","<b>Only the Lonely<\/b><br>Roy Orbison  (1960)<br>Danceability: 0.57<br>Valence: 0.93<br>Energy: 0.53","<b>Walk - Don't Run<\/b><br>The Ventures  (1960)<br>Danceability: 0.47<br>Valence: 0.96<br>Energy: 0.86","<b>Cathy's Clown<\/b><br>The Everly Brothers  (1960)<br>Danceability: 0.5<br>Valence: 0.87<br>Energy: 0.58","<b>Mule Skinner Blues<\/b><br>The Fendermen  (1960)<br>Danceability: 0.51<br>Valence: 0.54<br>Energy: 0.59","<b>Because They're Young<\/b><br>Duane Eddy  (1960)<br>Danceability: 0.7<br>Valence: 0.85<br>Energy: 0.62","<b>Tossin' And Turnin'<\/b><br>Bobby Lewis  (1961)<br>Danceability: 0.57<br>Valence: 0.96<br>Energy: 0.72","<b>Quarter To Three<\/b><br>Gary U.S. Bonds  (1961)<br>Danceability: 0.46<br>Valence: 0.9<br>Energy: 0.83","<b>The Boll Weevil Song<\/b><br>Brook Benton  (1961)<br>Danceability: 0.79<br>Valence: 0.9<br>Energy: 0.26","<b>I Like It Like That - Part 1<\/b><br>Chris Kenner  (1961)<br>Danceability: 0.58<br>Valence: 0.81<br>Energy: 0.54","<b>Michael<\/b><br>Michael  (1961)<br>Danceability: 0.52<br>Valence: 0.65<br>Energy: 0.1","<b>Raindrops<\/b><br>Dee Clark  (1961)<br>Danceability: 0.58<br>Valence: 0.74<br>Energy: 0.4","<b>Wooden Heart<\/b><br>Joe Dowell  (1961)<br>Danceability: 0.84<br>Valence: 0.92<br>Energy: 0.45","<b>Moody River<\/b><br>Pat Boone  (1961)<br>Danceability: 0.62<br>Valence: 0.69<br>Energy: 0.73","<b>Dum Dum - Remastered<\/b><br>Brenda Lee  (1961)<br>Danceability: 0.7<br>Valence: 0.62<br>Energy: 0.52","<b>Last Night<\/b><br>The Mar-Keys  (1961)<br>Danceability: 0.72<br>Valence: 0.86<br>Energy: 0.56","<b>Roses Are Red (My Love) - Single Version<\/b><br>Bobby Vinton  (1962)<br>Danceability: 0.52<br>Valence: 0.53<br>Energy: 0.22","<b>I Can't Stop Loving You<\/b><br>Ray Anthony  (1962)<br>Danceability: 0.39<br>Valence: 0.56<br>Energy: 0.28","<b>The Stripper<\/b><br>David Rose  (1962)<br>Danceability: 0.48<br>Valence: 0.56<br>Energy: 0.46","<b>Breaking Up Is Hard to Do<\/b><br>Neil Sedaka  (1962)<br>Danceability: 0.73<br>Valence: 0.97<br>Energy: 0.55","<b>The Loco-Motion - Single Version<\/b><br>Little Eva  (1962)<br>Danceability: 0.64<br>Valence: 0.89<br>Energy: 0.89","<b>The Wah-Watusi<\/b><br>The Orlons  (1962)<br>Danceability: 0.76<br>Valence: 0.88<br>Energy: 0.68","<b>Sealed With A Kiss<\/b><br>Brian Hyland  (1962)<br>Danceability: 0.47<br>Valence: 0.58<br>Energy: 0.28","<b>Palisades Park<\/b><br>Freddy Cannon  (1962)<br>Danceability: 0.28<br>Valence: 0.87<br>Energy: 0.83","<b>Sheila<\/b><br>Tommy Roe  (1962)<br>Danceability: 0.64<br>Valence: 0.9<br>Energy: 0.54","<b>It Keeps Right on A-Hurtin'<\/b><br>Johnny Tillotson  (1962)<br>Danceability: 0.47<br>Valence: 0.53<br>Energy: 0.28","<b>Fingertips - Pt. 2<\/b><br>Stevie Wonder  (1963)<br>Danceability: 0.32<br>Valence: 0.34<br>Energy: 0.91","<b>Surf City<\/b><br>Jan & Dean  (1963)<br>Danceability: 0.47<br>Valence: 0.94<br>Energy: 0.65","<b>Easier Said Than Done<\/b><br>The Essex  (1963)<br>Danceability: 0.8<br>Valence: 0.85<br>Energy: 0.39","<b>So Much In Love<\/b><br>The Tymes  (1963)<br>Danceability: 0.54<br>Valence: 0.42<br>Energy: 0.48","<b>Sukiyaki - Original Hit Version<\/b><br>Kyu Sakamoto  (1963)<br>Danceability: 0.42<br>Valence: 0.4<br>Energy: 0.36","<b>Wipe Out<\/b><br>The Surfaris  (1963)<br>Danceability: 0.46<br>Valence: 0.73<br>Energy: 0.96","<b>Blowin' In The Wind<\/b><br>Peter, Paul and Mary  (1963)<br>Danceability: 0.38<br>Valence: 0.56<br>Energy: 0.08","<b>My Boyfriend's Back<\/b><br>The Angels  (1963)<br>Danceability: 0.68<br>Valence: 0.96<br>Energy: 0.87","<b>Candy Girl<\/b><br>Frankie Valli & The Four Seasons  (1963)<br>Danceability: 0.64<br>Valence: 0.72<br>Energy: 0.52","<b>(You're The) Devil in Disguise<\/b><br>Elvis Presley  (1963)<br>Danceability: 0.48<br>Valence: 0.87<br>Energy: 0.73","<b>Where Did Our Love Go<\/b><br>The Supremes  (1964)<br>Danceability: 0.55<br>Valence: 0.83<br>Energy: 0.48","<b>I Get Around (Mono)<\/b><br>The Beach Boys  (1964)<br>Danceability: 0.39<br>Valence: 0.51<br>Energy: 0.62","<b>Everybody Loves Somebody<\/b><br>Dean Martin  (1964)<br>Danceability: 0.26<br>Valence: 0.4<br>Energy: 0.49","<b>A Hard Day's Night - Remastered<\/b><br>The Beatles  (1964)<br>Danceability: 0.59<br>Valence: 0.8<br>Energy: 0.8","<b>House Of The Rising Sun<\/b><br>The Animals  (1964)<br>Danceability: 0.29<br>Valence: 0.28<br>Energy: 0.58","<b>Memphis<\/b><br>Johnny Rivers  (1964)<br>Danceability: 0.78<br>Valence: 0.64<br>Energy: 0.26","<b>Under The Boardwalk<\/b><br>The Drifters  (1964)<br>Danceability: 0.75<br>Valence: 0.78<br>Energy: 0.23","<b>The Little Old Lady (From Pasadena)<\/b><br>Jan & Dean  (1964)<br>Danceability: 0.48<br>Valence: 0.94<br>Energy: 0.82","<b>Wishin' And Hopin'<\/b><br>Dusty Springfield  (1964)<br>Danceability: 0.68<br>Valence: 0.73<br>Energy: 0.38","<b>Rag Doll<\/b><br>Frankie Valli & The Four Seasons  (1964)<br>Danceability: 0.48<br>Valence: 0.68<br>Energy: 0.48","<b>(I Can't Get No) Satisfaction - Mono Version / Remastered 2002<\/b><br>The Rolling Stones  (1965)<br>Danceability: 0.72<br>Valence: 0.9<br>Energy: 0.85","<b>I Can't Help Myself (Sugar Pie, Honey Bunch)<\/b><br>Four Tops  (1965)<br>Danceability: 0.69<br>Valence: 0.96<br>Energy: 0.71","<b>I Got You Babe<\/b><br>Sonny & Cher  (1965)<br>Danceability: 0.62<br>Valence: 0.62<br>Energy: 0.7","<b>Help!<\/b><br>The Beatles  (1965)<br>Danceability: 0.55<br>Valence: 0.85<br>Energy: 0.73","<b>I'm Henry VIII, I Am<\/b><br>Herman's Hermits  (1965)<br>Danceability: 0.64<br>Valence: 0.96<br>Energy: 0.78","<b>Mr. Tambourine Man<\/b><br>The Byrds  (1965)<br>Danceability: 0.46<br>Valence: 0.64<br>Energy: 0.47","<b>What's New Pussycat?<\/b><br>Tom Jones  (1965)<br>Danceability: 0.38<br>Valence: 0.62<br>Energy: 0.46","<b>Yes. I'm Ready<\/b><br>Barbara Mason  (1965)<br>Danceability: 0.24<br>Valence: 0.48<br>Energy: 0.38","<b>Cara, Mia<\/b><br>Jay & The Americans  (1965)<br>Danceability: 0.37<br>Valence: 0.52<br>Energy: 0.71","<b>Save Your Heart For Me<\/b><br>Gary Lewis & The Playboys  (1965)<br>Danceability: 0.63<br>Valence: 0.59<br>Energy: 0.27","<b>Wild Thing<\/b><br>The Troggs  (1966)<br>Danceability: 0.46<br>Valence: 0.67<br>Energy: 0.85","<b>Summer in the City - Remastered<\/b><br>The Lovin' Spoonful  (1966)<br>Danceability: 0.42<br>Valence: 0.56<br>Energy: 0.68","<b>Lil' Red Riding Hood<\/b><br>Sam The Sham & The Pharaohs  (1966)<br>Danceability: 0.75<br>Valence: 0.57<br>Energy: 0.4","<b>Hanky Panky - Single Version<\/b><br>Tommy James & The Shondells  (1966)<br>Danceability: 0.65<br>Valence: 0.9<br>Energy: 0.91","<b>Paperback Writer - Remastered<\/b><br>The Beatles  (1966)<br>Danceability: 0.5<br>Valence: 0.8<br>Energy: 0.69","<b>Sunny<\/b><br>Bobby Hebb  (1966)<br>Danceability: 0.71<br>Valence: 0.66<br>Energy: 0.35","<b>Strangers In The Night<\/b><br>Frank Sinatra  (1966)<br>Danceability: 0.23<br>Valence: 0.53<br>Energy: 0.49","<b>Red Rubber Ball<\/b><br>The Cyrkle  (1966)<br>Danceability: 0.64<br>Valence: 0.85<br>Energy: 0.46","<b>Sunshine Superman<\/b><br>Donovan  (1966)<br>Danceability: 0.63<br>Valence: 0.63<br>Energy: 0.68","<b>See You In September<\/b><br>The Happenings  (1966)<br>Danceability: 0.52<br>Valence: 0.8<br>Energy: 0.42","<b>Light My Fire<\/b><br>The Doors  (1967)<br>Danceability: 0.41<br>Valence: 0.44<br>Energy: 0.72","<b>Windy<\/b><br>The Association  (1967)<br>Danceability: 0.68<br>Valence: 0.9<br>Energy: 0.72","<b>Can't Take My Eyes Off You<\/b><br>Frankie Valli  (1967)<br>Danceability: 0.58<br>Valence: 0.56<br>Energy: 0.78","<b>Little Bit O' Soul - Original Hit Version<\/b><br>Music Explosion  (1967)<br>Danceability: 0.52<br>Valence: 0.8<br>Energy: 0.79","<b>I Was Made To Love Her<\/b><br>Stevie Wonder  (1967)<br>Danceability: 0.37<br>Valence: 0.89<br>Energy: 0.61","<b>All You Need Is Love - Remastered<\/b><br>The Beatles  (1967)<br>Danceability: 0.4<br>Valence: 0.65<br>Energy: 0.48","<b>A Whiter Shade Of Pale<\/b><br>Procol Harum  (1967)<br>Danceability: 0.25<br>Valence: 0.44<br>Energy: 0.66","<b>San Francisco (Be Sure to Wear Flowers In Your Hair)<\/b><br>Scott McKenzie  (1967)<br>Danceability: 0.57<br>Valence: 0.65<br>Energy: 0.5","<b>Ode to Billie Joe<\/b><br>Jimmy Smith  (1967)<br>Danceability: 0.68<br>Valence: 0.32<br>Energy: 0.17","<b>Baby, I Love You<\/b><br>Aretha Franklin  (1967)<br>Danceability: 0.68<br>Valence: 0.8<br>Energy: 0.26","<b>This Guy's In Love With You<\/b><br>Herb Alpert & The Tijuana Brass  (1968)<br>Danceability: 0.64<br>Valence: 0.25<br>Energy: 0.17","<b>Hello, I Love You<\/b><br>The Doors  (1968)<br>Danceability: 0.6<br>Valence: 0.96<br>Energy: 0.55","<b>People Got To Be Free - Single Version<\/b><br>The Rascals  (1968)<br>Danceability: 0.67<br>Valence: 0.96<br>Energy: 0.62","<b>Grazing In the Grass<\/b><br>Hugh Masekela  (1968)<br>Danceability: 0.67<br>Valence: 0.92<br>Energy: 0.65","<b>Stoned Soul Picnic<\/b><br>The 5th Dimension  (1968)<br>Danceability: 0.56<br>Valence: 0.73<br>Energy: 0.45","<b>The Horse<\/b><br>Cliff Nobles  (1968)<br>Danceability: 0.65<br>Valence: 0.92<br>Energy: 0.57","<b>Lady Willpower<\/b><br>Gary Puckett & The Union Gap  (1968)<br>Danceability: 0.55<br>Valence: 0.62<br>Energy: 0.52","<b>Jumpin' Jack Flash - Mono / Remastered<\/b><br>The Rolling Stones  (1968)<br>Danceability: 0.59<br>Valence: 0.72<br>Energy: 0.89","<b>Classical Gas<\/b><br>Mason Williams  (1968)<br>Danceability: 0.47<br>Valence: 0.71<br>Energy: 0.72","<b>Born To Be Wild<\/b><br>Steppenwolf  (1968)<br>Danceability: 0.44<br>Valence: 0.53<br>Energy: 0.74","<b>In the Year 2525<\/b><br>Zager & Evans  (1969)<br>Danceability: 0.44<br>Valence: 0.58<br>Energy: 0.6","<b>Honky Tonk Women<\/b><br>The Rolling Stones  (1969)<br>Danceability: 0.56<br>Valence: 0.97<br>Energy: 0.77","<b>Crystal Blue Persuasion<\/b><br>Tommy James & The Shondells  (1969)<br>Danceability: 0.6<br>Valence: 0.82<br>Energy: 0.31","<b>Love Theme from \"Romeo and Juliet\" - Remastered<\/b><br>Henry Mancini  (1969)<br>Danceability: 0.36<br>Valence: 0.16<br>Energy: 0.19","<b>Spinning Wheel<\/b><br>Blood, Sweat & Tears  (1969)<br>Danceability: 0.69<br>Valence: 0.58<br>Energy: 0.37","<b>One - Single Version<\/b><br>Three Dog Night  (1969)<br>Danceability: 0.58<br>Valence: 0.4<br>Energy: 0.41","<b>Good Morning Starshine<\/b><br>Oliver  (1969)<br>Danceability: 0.69<br>Valence: 0.86<br>Energy: 0.54","<b>What Does It Take (To Win Your Love)<\/b><br>Alton Ellis  (1969)<br>Danceability: 0.81<br>Valence: 0.87<br>Energy: 0.45","<b>Sweet Caroline<\/b><br>Neil Diamond  (1969)<br>Danceability: 0.54<br>Valence: 0.53<br>Energy: 0.13","<b>A Boy Named Sue - Live at San Quentin State Prison, San Quentin, CA - February 1969<\/b><br>Johnny Cash  (1969)<br>Danceability: 0.69<br>Valence: 0.89<br>Energy: 0.49"],"type":"scatter","mode":"markers","marker":{"autocolorscale":false,"color":"rgba(86,180,233,1)","opacity":0.75,"size":7.559055118110237,"symbol":"circle","line":{"width":1.8897637795275593,"color":"rgba(86,180,233,1)"}},"hoveron":"points","name":"1960s","legendgroup":"1960s","showlegend":true,"xaxis":"x","yaxis":"y","hoverinfo":"text","frame":null},{"x":[0.54300000000000004,0.67800000000000005,0.629,0.755,0.56899999999999995,0.56899999999999995,0.73799999999999999,0.84699999999999998,0.67200000000000004,0.754,0.437,0.69499999999999995,0.88400000000000001,0.255,0.54600000000000004,0.66900000000000004,0.83999999999999997,0.435,0.63900000000000001,0.747,0.55800000000000005,0.71899999999999997,0.61899999999999999,0.502,0.51600000000000001,0.75700000000000001,0.47199999999999998,0.71199999999999997,0.55000000000000004,0.55800000000000005,0.67400000000000004,0.68899999999999995,0.69699999999999995,0.42199999999999999,0.40500000000000003,0.42699999999999999,0.38800000000000001,0.54300000000000004,0.59299999999999997,0.253,0.32500000000000001,0.59599999999999997,0.68899999999999995,0.76700000000000002,0.60999999999999999,0.70999999999999996,0.65700000000000003,0.59699999999999998,0.79400000000000004,0.43099999999999999,0.65500000000000003,0.67600000000000005,0.81499999999999995,0.64200000000000002,0.627,0.46300000000000002,0.45800000000000002,0.40100000000000002,0.45700000000000002,0.57799999999999996,0.72099999999999997,0.58699999999999997,0.49099999999999999,0.68300000000000005,0.77000000000000002,0.69099999999999995,0.754,0.73699999999999999,0.64900000000000002,0.62,0.64800000000000002,0.78400000000000003,0.628,0.56699999999999995,0.32900000000000001,0.67200000000000004,0.29699999999999999,0.59799999999999998,0.27400000000000002,0.55200000000000005,0.74199999999999999,0.48199999999999998,0.81999999999999995,0.871,0.497,0.83499999999999996,0.52900000000000003,0.59599999999999997,0.64100000000000001,0.70699999999999996,0.89300000000000002,0.78100000000000003,0.86399999999999999,0.83199999999999996,0.58599999999999997,0.64700000000000002,0.88200000000000001,0.497,0.69199999999999995,0.78700000000000003],"y":[0.20499999999999999,0.80400000000000005,0.47199999999999998,0.94899999999999995,0.76800000000000002,0.82399999999999995,0.95999999999999996,0.95899999999999996,0.871,0.97299999999999998,0.251,0.44500000000000001,0.97399999999999998,0.54600000000000004,0.93100000000000005,0.64700000000000002,0.82799999999999996,0.74299999999999999,0.20899999999999999,0.60099999999999998,0.55900000000000005,0.77200000000000002,0.42299999999999999,0.53100000000000003,0.90700000000000003,0.81499999999999995,0.443,0.79900000000000004,0.373,0.60499999999999998,0.876,0.89800000000000002,0.84999999999999998,0.50600000000000001,0.42099999999999999,0.20100000000000001,0.59999999999999998,0.60699999999999998,0.91700000000000004,0.36199999999999999,0.44700000000000001,0.58699999999999997,0.64300000000000002,0.80900000000000005,0.94299999999999995,0.96299999999999997,0.75700000000000001,0.88500000000000001,0.751,0.38,0.76500000000000001,0.91800000000000004,0.70399999999999996,0.69399999999999995,0.85399999999999998,0.56100000000000005,0.67900000000000005,0.20799999999999999,0.16600000000000001,0.70699999999999996,0.81499999999999995,0.65500000000000003,0.80800000000000005,0.96099999999999997,0.90400000000000003,0.93400000000000005,0.64400000000000002,0.82699999999999996,0.96099999999999997,0.59199999999999997,0.83099999999999996,0.96999999999999997,0.64300000000000002,0.876,0.16400000000000001,0.79700000000000004,0.45600000000000002,0.34200000000000003,0.29699999999999999,0.30299999999999999,0.95399999999999996,0.13600000000000001,0.871,0.96399999999999997,0.42099999999999999,0.91000000000000003,0.252,0.77400000000000002,0.89200000000000002,0.52200000000000002,0.96799999999999997,0.96799999999999997,0.80200000000000005,0.96499999999999997,0.89700000000000002,0.68400000000000005,0.92600000000000005,0.34499999999999997,0.51900000000000002,0.91200000000000003],"text":["<b>(They Long To Be) Close To You<\/b><br>Carpenters  (1970)<br>Danceability: 0.54<br>Valence: 0.2<br>Energy: 0.24","<b>Mama Told Me (Not To Come)<\/b><br>Three Dog Night  (1970)<br>Danceability: 0.68<br>Valence: 0.8<br>Energy: 0.62","<b>Make It With You<\/b><br>Bread  (1970)<br>Danceability: 0.63<br>Valence: 0.47<br>Energy: 0.36","<b>The Love You Save<\/b><br>The Jackson 5  (1970)<br>Danceability: 0.76<br>Valence: 0.95<br>Energy: 0.7","<b>War<\/b><br>Edwin Starr  (1970)<br>Danceability: 0.57<br>Valence: 0.77<br>Energy: 0.75","<b>Ball Of Confusion (That's What The World Is Today)<\/b><br>The Temptations  (1970)<br>Danceability: 0.57<br>Valence: 0.82<br>Energy: 0.83","<b>Band of Gold<\/b><br>Freeda Payne  (1970)<br>Danceability: 0.74<br>Valence: 0.96<br>Energy: 0.62","<b>Spill The Wine<\/b><br>Eric Burdon  (1970)<br>Danceability: 0.85<br>Valence: 0.96<br>Energy: 0.46","<b>Signed, Sealed, Delivered (I'm Yours)<\/b><br>Stevie Wonder  (1970)<br>Danceability: 0.67<br>Valence: 0.87<br>Energy: 0.6","<b>In the Summertime<\/b><br>Mungo Jerry  (1970)<br>Danceability: 0.75<br>Valence: 0.97<br>Energy: 0.45","<b>How Can You Mend A Broken Heart<\/b><br>Bee Gees  (1971)<br>Danceability: 0.44<br>Valence: 0.25<br>Energy: 0.26","<b>You've Got A Friend<\/b><br>James Taylor  (1971)<br>Danceability: 0.7<br>Valence: 0.44<br>Energy: 0.28","<b>Mr. Big Stuff<\/b><br>Jean Knight  (1971)<br>Danceability: 0.88<br>Valence: 0.97<br>Energy: 0.49","<b>Take Me Home, Country Roads<\/b><br>John Denver  (1971)<br>Danceability: 0.26<br>Valence: 0.55<br>Energy: 0.43","<b>Don't Pull Your Love<\/b><br>Hamilton  (1971)<br>Danceability: 0.55<br>Valence: 0.93<br>Energy: 0.51","<b>Draggin' The Line<\/b><br>Tommy James & The Shondells  (1971)<br>Danceability: 0.67<br>Valence: 0.65<br>Energy: 0.58","<b>Treat Her Like A Lady<\/b><br>Cornelius Brothers & Sister Rose  (1971)<br>Danceability: 0.84<br>Valence: 0.83<br>Energy: 0.38","<b>Signs<\/b><br>Five Man Electrical Band  (1971)<br>Danceability: 0.44<br>Valence: 0.74<br>Energy: 0.72","<b>Indian Reservation (The Lament of the Cherokee Reservation Indian)<\/b><br>Paul Revere & The Raiders  (1971)<br>Danceability: 0.64<br>Valence: 0.21<br>Energy: 0.38","<b>It's Too Late<\/b><br>The Hit Co., The Tribute Co.  (1971)<br>Danceability: 0.75<br>Valence: 0.6<br>Energy: 0.25","<b>Alone Again (Naturally)<\/b><br>Gilbert O'Sullivan  (1972)<br>Danceability: 0.56<br>Valence: 0.56<br>Energy: 0.46","<b>Brandy (You're a Fine Girl)<\/b><br>Looking Glass  (1972)<br>Danceability: 0.72<br>Valence: 0.77<br>Energy: 0.77","<b>Lean on Me<\/b><br>Bill Withers  (1972)<br>Danceability: 0.62<br>Valence: 0.42<br>Energy: 0.22","<b>(If Loving You Is Wrong) I Don't Want to Be Right<\/b><br>Luther Ingram  (1972)<br>Danceability: 0.5<br>Valence: 0.53<br>Energy: 0.45","<b>Too Late To Turn Back Now<\/b><br>Cornelius Brothers & Sister Rose  (1972)<br>Danceability: 0.52<br>Valence: 0.91<br>Energy: 0.73","<b>Long Cool Woman (In A Black Dress) - 1999 Remastered Version<\/b><br>The Hollies  (1972)<br>Danceability: 0.76<br>Valence: 0.81<br>Energy: 0.87","<b>Daddy Don't You Walk So Fast - Re-Recorded In Stereo<\/b><br>Wayne Newton  (1972)<br>Danceability: 0.47<br>Valence: 0.44<br>Energy: 0.5","<b>I'm Still in Love with You<\/b><br>Al Green  (1972)<br>Danceability: 0.71<br>Valence: 0.8<br>Energy: 0.4","<b>Song Sung Blue - Single Version<\/b><br>Neil Diamond  (1972)<br>Danceability: 0.55<br>Valence: 0.37<br>Energy: 0.33","<b>Where Is The Love<\/b><br>Roberta Flack  (1972)<br>Danceability: 0.56<br>Valence: 0.6<br>Energy: 0.44","<b>Bad, Bad Leroy Brown<\/b><br>Jim Croce  (1973)<br>Danceability: 0.67<br>Valence: 0.88<br>Energy: 0.8","<b>Will It Go Round In Circles<\/b><br>Billy Preston  (1973)<br>Danceability: 0.69<br>Valence: 0.9<br>Energy: 0.87","<b>Brother Louie<\/b><br>Stories  (1973)<br>Danceability: 0.7<br>Valence: 0.85<br>Energy: 0.5","<b>Touch Me In The Morning<\/b><br>Diana Ross  (1973)<br>Danceability: 0.42<br>Valence: 0.51<br>Energy: 0.49","<b>The Morning After - Single Version<\/b><br>Maureen McGovern  (1973)<br>Danceability: 0.41<br>Valence: 0.42<br>Energy: 0.52","<b>Live And Let Die<\/b><br>Wings  (1973)<br>Danceability: 0.43<br>Valence: 0.2<br>Energy: 0.4","<b>Give Me Love (Give Me Peace On Earth)<\/b><br>George Harrison  (1973)<br>Danceability: 0.39<br>Valence: 0.6<br>Energy: 0.56","<b>Let's Get It On<\/b><br>Marvin Gaye  (1973)<br>Danceability: 0.54<br>Valence: 0.61<br>Energy: 0.62","<b>Kodachrome<\/b><br>Paul Simon  (1973)<br>Danceability: 0.59<br>Valence: 0.92<br>Energy: 0.69","<b>Yesterday Once More<\/b><br>Carpenters  (1973)<br>Danceability: 0.25<br>Valence: 0.36<br>Energy: 0.39","<b>Annie's Song<\/b><br>John Denver  (1974)<br>Danceability: 0.32<br>Valence: 0.45<br>Energy: 0.31","<b>(You're) Having My Baby - feat. Odia Coates<\/b><br>Paul Anka  (1974)<br>Danceability: 0.6<br>Valence: 0.59<br>Energy: 0.59","<b>Feel Like Makin' Love<\/b><br>Roberta Flack  (1974)<br>Danceability: 0.69<br>Valence: 0.64<br>Energy: 0.24","<b>The Night Chicago Died<\/b><br>Paper Lace  (1974)<br>Danceability: 0.77<br>Valence: 0.81<br>Energy: 0.45","<b>Rock Your Baby<\/b><br>George McCrae  (1974)<br>Danceability: 0.61<br>Valence: 0.94<br>Energy: 0.66","<b>Billy, Don't Be A Hero<\/b><br>Bo Donaldson & The Heywoods  (1974)<br>Danceability: 0.71<br>Valence: 0.96<br>Energy: 0.69","<b>Tell Me Something Good<\/b><br>Rufus Featuring Chaka Khan  (1974)<br>Danceability: 0.66<br>Valence: 0.76<br>Energy: 0.55","<b>Rock the Boat<\/b><br>Hues Corporation  (1974)<br>Danceability: 0.6<br>Valence: 0.88<br>Energy: 0.58","<b>Sundown<\/b><br>Gordon Lightfoot  (1974)<br>Danceability: 0.79<br>Valence: 0.75<br>Energy: 0.43","<b>Don't Let The Sun Go Down On Me<\/b><br>Elton John  (1974)<br>Danceability: 0.43<br>Valence: 0.38<br>Energy: 0.46","<b>One Of These Nights<\/b><br>Eagles  (1975)<br>Danceability: 0.66<br>Valence: 0.76<br>Energy: 0.61","<b>Love Will Keep Us Together<\/b><br>Captain & Tennille  (1975)<br>Danceability: 0.68<br>Valence: 0.92<br>Energy: 0.49","<b>Jive Talkin'<\/b><br>Bee Gees  (1975)<br>Danceability: 0.81<br>Valence: 0.7<br>Energy: 0.57","<b>Rhinestone Cowboy<\/b><br>Glen Campbell  (1975)<br>Danceability: 0.64<br>Valence: 0.69<br>Energy: 0.62","<b>The Hustle - Original Mix<\/b><br>Van McCoy  (1975)<br>Danceability: 0.63<br>Valence: 0.85<br>Energy: 0.88","<b>Please Mr. Please<\/b><br>Olivia Newton-John  (1975)<br>Danceability: 0.46<br>Valence: 0.56<br>Energy: 0.45","<b>Listen To What The Man Said<\/b><br>Wings  (1975)<br>Danceability: 0.46<br>Valence: 0.68<br>Energy: 0.44","<b>I'm Not In Love<\/b><br>10cc  (1975)<br>Danceability: 0.4<br>Valence: 0.21<br>Energy: 0.53","<b>Someone Saved My Life Tonight<\/b><br>Elton John  (1975)<br>Danceability: 0.46<br>Valence: 0.17<br>Energy: 0.45","<b>Fallin' in Love<\/b><br>Hamilton  (1975)<br>Danceability: 0.58<br>Valence: 0.71<br>Energy: 0.41","<b>Don't Go Breaking My Heart - Remastered<\/b><br>Elton John  (1976)<br>Danceability: 0.72<br>Valence: 0.81<br>Energy: 0.93","<b>Kiss and Say Goodbye<\/b><br>The Manhattans  (1976)<br>Danceability: 0.59<br>Valence: 0.66<br>Energy: 0.38","<b>Afternoon Delight<\/b><br>Starland Vocal Band  (1976)<br>Danceability: 0.49<br>Valence: 0.81<br>Energy: 0.44","<b>You Should Be Dancing - Edit<\/b><br>Bee Gees  (1976)<br>Danceability: 0.68<br>Valence: 0.96<br>Energy: 0.72","<b>Love Is Alive<\/b><br>Gary Wright  (1976)<br>Danceability: 0.77<br>Valence: 0.9<br>Energy: 0.55","<b>You'll Never Find Another Love Like Mine<\/b><br>Lou Rawls  (1976)<br>Danceability: 0.69<br>Valence: 0.93<br>Energy: 0.72","<b>Let 'Em In<\/b><br>Wings  (1976)<br>Danceability: 0.75<br>Valence: 0.64<br>Energy: 0.37","<b>Silly Love Songs<\/b><br>Wings  (1976)<br>Danceability: 0.74<br>Valence: 0.83<br>Energy: 0.33","<b>(Shake, Shake, Shake) Shake Your Booty<\/b><br>KC & The Sunshine Band  (1976)<br>Danceability: 0.65<br>Valence: 0.96<br>Energy: 0.82","<b>I'd Really Love To See You Tonight<\/b><br>England Dan  (1976)<br>Danceability: 0.62<br>Valence: 0.59<br>Energy: 0.56","<b>I Just Want To Be Your Everything<\/b><br>Andy Gibb  (1977)<br>Danceability: 0.65<br>Valence: 0.83<br>Energy: 0.66","<b>Best of My Love<\/b><br>The Emotions  (1977)<br>Danceability: 0.78<br>Valence: 0.97<br>Energy: 0.71","<b>(Your Love Has Lifted Me) Higher And Higher<\/b><br>Rita Coolidge  (1977)<br>Danceability: 0.63<br>Valence: 0.64<br>Energy: 0.68","<b>Da Doo Ron Ron<\/b><br>Shaun Cassidy  (1977)<br>Danceability: 0.57<br>Valence: 0.88<br>Energy: 0.58","<b>I'm In You<\/b><br>Peter Frampton  (1977)<br>Danceability: 0.33<br>Valence: 0.16<br>Energy: 0.5","<b>Undercover Angel<\/b><br>Alan O'Day  (1977)<br>Danceability: 0.67<br>Valence: 0.8<br>Energy: 0.53","<b>Looks Like We Made It<\/b><br>Barry Manilow  (1977)<br>Danceability: 0.3<br>Valence: 0.46<br>Energy: 0.48","<b>Easy<\/b><br>Commodores  (1977)<br>Danceability: 0.6<br>Valence: 0.34<br>Energy: 0.35","<b>My Heart Belongs to Me<\/b><br>Barbra Streisand  (1977)<br>Danceability: 0.27<br>Valence: 0.3<br>Energy: 0.21","<b>Do You Wanna Make Love - Re-Recording<\/b><br>Peter McCann  (1977)<br>Danceability: 0.55<br>Valence: 0.3<br>Energy: 0.45","<b>Shadow Dancing<\/b><br>Andy Gibb  (1978)<br>Danceability: 0.74<br>Valence: 0.95<br>Energy: 0.6","<b>Three Times A Lady<\/b><br>Commodores  (1978)<br>Danceability: 0.48<br>Valence: 0.14<br>Energy: 0.08","<b>Grease<\/b><br>Frankie Valli  (1978)<br>Danceability: 0.82<br>Valence: 0.87<br>Energy: 0.37","<b>Miss You - Remastered<\/b><br>The Rolling Stones  (1978)<br>Danceability: 0.87<br>Valence: 0.96<br>Energy: 0.52","<b>Baker Street<\/b><br>Gerry Rafferty  (1978)<br>Danceability: 0.5<br>Valence: 0.42<br>Energy: 0.35","<b>Boogie Oogie Oogie - Digitally Remastered 99<\/b><br>A Taste Of Honey  (1978)<br>Danceability: 0.84<br>Valence: 0.91<br>Energy: 0.52","<b>Last Dance - Single Version<\/b><br>Donna Summer  (1978)<br>Danceability: 0.53<br>Valence: 0.25<br>Energy: 0.72","<b>Hot Blooded<\/b><br>Foreigner  (1978)<br>Danceability: 0.6<br>Valence: 0.77<br>Energy: 0.82","<b>Use ta Be My Girl<\/b><br>The O'Jays  (1978)<br>Danceability: 0.64<br>Valence: 0.89<br>Energy: 0.62","<b>Still The Same<\/b><br>Bob Seger  (1978)<br>Danceability: 0.71<br>Valence: 0.52<br>Energy: 0.76","<b>Bad Girls<\/b><br>Donna Summer  (1979)<br>Danceability: 0.89<br>Valence: 0.97<br>Energy: 0.61","<b>Ring My Bell<\/b><br>Anita Ward  (1979)<br>Danceability: 0.78<br>Valence: 0.97<br>Energy: 0.56","<b>Good Times<\/b><br>CHIC  (1979)<br>Danceability: 0.86<br>Valence: 0.8<br>Energy: 0.88","<b>Hot Stuff<\/b><br>Donna Summer  (1979)<br>Danceability: 0.83<br>Valence: 0.96<br>Energy: 0.74","<b>My Sharona<\/b><br>The Knack  (1979)<br>Danceability: 0.59<br>Valence: 0.9<br>Energy: 0.7","<b>The Main Event/Fight<\/b><br>Barbra Streisand  (1979)<br>Danceability: 0.65<br>Valence: 0.68<br>Energy: 0.88","<b>Makin' It (Re-Recorded)<\/b><br>David Naughton  (1979)<br>Danceability: 0.88<br>Valence: 0.93<br>Energy: 0.56","<b>After The Love Has Gone - (Live)<\/b><br>Earth Wind & Fire Experience  (1979)<br>Danceability: 0.5<br>Valence: 0.34<br>Energy: 0.51","<b>Gold<\/b><br>John Stewart  (1979)<br>Danceability: 0.69<br>Valence: 0.52<br>Energy: 0.6","<b>Boogie Wonderland (In the Style of Earth, Wind & Fire With the Emotions) [Karaoke Version]<\/b><br>Ameritz Karaoke Entertainment  (1979)<br>Danceability: 0.79<br>Valence: 0.91<br>Energy: 0.64"],"type":"scatter","mode":"markers","marker":{"autocolorscale":false,"color":"rgba(0,158,115,1)","opacity":0.75,"size":7.559055118110237,"symbol":"circle","line":{"width":1.8897637795275593,"color":"rgba(0,158,115,1)"}},"hoveron":"points","name":"1970s","legendgroup":"1970s","showlegend":true,"xaxis":"x","yaxis":"y","hoverinfo":"text","frame":null},{"x":[0.752,0.69999999999999996,0.56999999999999995,0.59999999999999998,0.46899999999999997,0.86799999999999999,0.68000000000000005,0.75900000000000001,0.34699999999999998,0.877,0.72699999999999998,0.67500000000000004,0.48199999999999998,0.52100000000000002,0.76300000000000001,0.42499999999999999,0.501,0.82499999999999996,0.61399999999999999,0.55500000000000005,0.81499999999999995,0.81200000000000006,0.79100000000000004,0.70899999999999996,0.73099999999999998,0.53700000000000003,0.38400000000000001,0.434,0.82899999999999996,0.44500000000000001,0.81299999999999994,0.64400000000000002,0.70499999999999996,0.61099999999999999,0.94899999999999995,0.76500000000000001,0.63300000000000001,0.56999999999999995,0.63800000000000001,0.84199999999999997,0.72899999999999998,0.77900000000000003,0.85099999999999998,0.52700000000000002,0.52100000000000002,0.85999999999999999,0.55400000000000005,0.97999999999999998,0.54100000000000004,0.55200000000000005,0.51100000000000001,0.60699999999999998,0.76600000000000001,0.70599999999999996,0.47399999999999998,0.61199999999999999,0.65000000000000002,0.76100000000000001,0.55000000000000004,0.69299999999999995,0.80700000000000005,0.51800000000000002,0.64100000000000001,0.68500000000000005,0.71199999999999997,0.51500000000000001,0.72199999999999998,0.67300000000000004,0.57599999999999996,0.54400000000000004,0.41799999999999998,0.70899999999999996,0.56599999999999995,0.57399999999999995,0.34799999999999998,0.81200000000000006,0.625,0.61599999999999999,0.45800000000000002,0.63900000000000001,0.75900000000000001,0.63200000000000001,0.68300000000000005,0.35299999999999998,0.52900000000000003,0.54300000000000004,0.44900000000000001,0.72399999999999998,0.66000000000000003,0.432,0.39500000000000002,0.52200000000000002,0.879,0.504,0.66500000000000004,0.54500000000000004,0.85099999999999998,0.747,0.69999999999999996,0.67600000000000005],"y":[0.53900000000000003,0.61399999999999999,0.76100000000000001,0.80500000000000005,0.17799999999999999,0.90300000000000002,0.80000000000000004,0.877,0.184,0.86599999999999999,0.82199999999999995,0.59599999999999997,0.17299999999999999,0.45100000000000001,0.61299999999999999,0.20899999999999999,0.309,0.77500000000000002,0.92400000000000004,0.51800000000000002,0.55200000000000005,0.96899999999999997,0.96299999999999997,0.77900000000000003,0.95799999999999996,0.25600000000000001,0.73899999999999999,0.23599999999999999,0.92800000000000005,0.77700000000000002,0.72899999999999998,0.57799999999999996,0.89400000000000002,0.73099999999999998,0.84099999999999997,0.98599999999999999,0.34200000000000003,0.77600000000000002,0.63700000000000001,0.90600000000000003,0.83999999999999997,0.78900000000000003,0.79200000000000004,0.495,0.26700000000000002,0.85399999999999998,0.81999999999999995,0.89100000000000001,0.63600000000000001,0.59299999999999997,0.47299999999999998,0.71499999999999997,0.94799999999999995,0.77800000000000002,0.45800000000000002,0.47099999999999997,0.79100000000000004,0.91000000000000003,0.47899999999999998,0.84899999999999998,0.97099999999999997,0.312,0.44600000000000001,0.83399999999999996,0.94199999999999995,0.253,0.91000000000000003,0.57399999999999995,0.35999999999999999,0.64700000000000002,0.16800000000000001,0.86699999999999999,0.58699999999999997,0.47499999999999998,0.88600000000000001,0.80000000000000004,0.82599999999999996,0.90700000000000003,0.56999999999999995,0.80000000000000004,0.95399999999999996,0.34200000000000003,0.70199999999999996,0.184,0.59099999999999997,0.40699999999999997,0.69599999999999995,0.746,0.96399999999999997,0.152,0.126,0.19900000000000001,0.72399999999999998,0.21299999999999999,0.57299999999999995,0.46500000000000002,0.84599999999999997,0.89300000000000002,0.92400000000000004,0.46500000000000002],"text":["<b>It's Still Rock and Roll to Me<\/b><br>Billy Joel  (1980)<br>Danceability: 0.75<br>Valence: 0.54<br>Energy: 0.68","<b>Magic<\/b><br>Olivia Newton-John  (1980)<br>Danceability: 0.7<br>Valence: 0.61<br>Energy: 0.62","<b>Coming Up<\/b><br>Paul McCartney  (1980)<br>Danceability: 0.57<br>Valence: 0.76<br>Energy: 0.53","<b>Little Jeannie<\/b><br>Elton John  (1980)<br>Danceability: 0.6<br>Valence: 0.8<br>Energy: 0.37","<b>Sailing<\/b><br>Christopher Cross  (1980)<br>Danceability: 0.47<br>Valence: 0.18<br>Energy: 0.38","<b>Take Your Time (Do It Right)<\/b><br>The S.O.S Band  (1980)<br>Danceability: 0.87<br>Valence: 0.9<br>Energy: 0.72","<b>Emotional Rescue<\/b><br>The Rolling Stones  (1980)<br>Danceability: 0.68<br>Valence: 0.8<br>Energy: 0.62","<b>Cupid/I've Loved You For A Long Time - Remastered Single Version<\/b><br>The Spinners  (1980)<br>Danceability: 0.76<br>Valence: 0.88<br>Energy: 0.8","<b>The Rose<\/b><br>Bette Midler  (1980)<br>Danceability: 0.35<br>Valence: 0.18<br>Energy: 0.23","<b>Upside Down<\/b><br>Diana Ross  (1980)<br>Danceability: 0.88<br>Valence: 0.87<br>Energy: 0.84","<b>Jessie's Girl<\/b><br>Rick Springfield  (1981)<br>Danceability: 0.73<br>Valence: 0.82<br>Energy: 0.83","<b>Bette Davis Eyes<\/b><br>Kim Carnes  (1981)<br>Danceability: 0.68<br>Valence: 0.6<br>Energy: 0.65","<b>Endless Love<\/b><br>Lionel Richie  (1981)<br>Danceability: 0.48<br>Valence: 0.17<br>Energy: 0.3","<b>Believe It or Not (Theme From \"Greatest American Hero\")<\/b><br>Joey Scarbury  (1981)<br>Danceability: 0.52<br>Valence: 0.45<br>Energy: 0.74","<b>Slow Hand<\/b><br>The Pointer Sisters  (1981)<br>Danceability: 0.76<br>Valence: 0.61<br>Energy: 0.28","<b>The One That You Love<\/b><br>Air Supply  (1981)<br>Danceability: 0.42<br>Valence: 0.21<br>Energy: 0.46","<b>I Don't Need You<\/b><br>Kenny Rogers  (1981)<br>Danceability: 0.5<br>Valence: 0.31<br>Energy: 0.3","<b>Elvira<\/b><br>The Oak Ridge Boys  (1981)<br>Danceability: 0.82<br>Valence: 0.78<br>Energy: 0.27","<b>Queen of Hearts - Rerecorded<\/b><br>Juice Newton  (1981)<br>Danceability: 0.61<br>Valence: 0.92<br>Energy: 0.84","<b>All Those Years Ago - 2004 Digital Remaster<\/b><br>George Harrison  (1981)<br>Danceability: 0.56<br>Valence: 0.52<br>Energy: 0.64","<b>Eye of the Tiger<\/b><br>Survivor  (1982)<br>Danceability: 0.81<br>Valence: 0.55<br>Energy: 0.44","<b>Hurts So Good<\/b><br>John Mellencamp  (1982)<br>Danceability: 0.81<br>Valence: 0.97<br>Energy: 0.72","<b>Abracadabra<\/b><br>Steve Miller Band  (1982)<br>Danceability: 0.79<br>Valence: 0.96<br>Energy: 0.54","<b>Hold Me<\/b><br>Fleetwood Mac  (1982)<br>Danceability: 0.71<br>Valence: 0.78<br>Energy: 0.57","<b>Don't You Want Me - 2002 - Remaster<\/b><br>The Human League  (1982)<br>Danceability: 0.73<br>Valence: 0.96<br>Energy: 0.74","<b>Hard To Say I'm Sorry - Remastered Version<\/b><br>Chicago  (1982)<br>Danceability: 0.54<br>Valence: 0.26<br>Energy: 0.42","<b>Rosanna<\/b><br>Toto  (1982)<br>Danceability: 0.38<br>Valence: 0.74<br>Energy: 0.51","<b>Even the Nights Are Better<\/b><br>Air Supply  (1982)<br>Danceability: 0.43<br>Valence: 0.24<br>Energy: 0.51","<b>Let It Whip<\/b><br>Dazz Band  (1982)<br>Danceability: 0.83<br>Valence: 0.93<br>Energy: 0.89","<b>Keep the Fire Burnin'<\/b><br>REO Speedwagon  (1982)<br>Danceability: 0.44<br>Valence: 0.78<br>Energy: 0.82","<b>Every Breath You Take - Remastered 2003<\/b><br>The Police  (1983)<br>Danceability: 0.81<br>Valence: 0.73<br>Energy: 0.46","<b>Flashdance... What A Feeling<\/b><br>Irene Cara  (1983)<br>Danceability: 0.64<br>Valence: 0.58<br>Energy: 0.62","<b>Sweet Dreams (Are Made of This)<\/b><br>Eurythmics  (1983)<br>Danceability: 0.7<br>Valence: 0.89<br>Energy: 0.51","<b>Maniac<\/b><br>Michael Sembello  (1983)<br>Danceability: 0.61<br>Valence: 0.73<br>Energy: 0.7","<b>Electric Avennue<\/b><br>Eddie Grant  (1983)<br>Danceability: 0.95<br>Valence: 0.84<br>Energy: 0.41","<b>She Works Hard For The Money<\/b><br>Donna Summer  (1983)<br>Danceability: 0.76<br>Valence: 0.99<br>Energy: 0.64","<b>Never Gonna Let You Go<\/b><br>Sérgio Mendes  (1983)<br>Danceability: 0.63<br>Valence: 0.34<br>Energy: 0.4","<b>Is There Something I Should Know<\/b><br>Duran Duran  (1983)<br>Danceability: 0.57<br>Valence: 0.78<br>Energy: 0.97","<b>Stand Back<\/b><br>Stevie Nicks  (1983)<br>Danceability: 0.64<br>Valence: 0.64<br>Energy: 0.78","<b>Wanna Be Startin' Somethin'<\/b><br>Michael Jackson  (1983)<br>Danceability: 0.84<br>Valence: 0.91<br>Energy: 0.87","<b>When Doves Cry<\/b><br>Prince  (1984)<br>Danceability: 0.73<br>Valence: 0.84<br>Energy: 0.99","<b>Ghostbusters<\/b><br>Ray Parker, Jr.  (1984)<br>Danceability: 0.78<br>Valence: 0.79<br>Energy: 0.72","<b>What's Love Got to Do with It<\/b><br>Tina Turner  (1984)<br>Danceability: 0.85<br>Valence: 0.79<br>Energy: 0.41","<b>Dancing In the Dark<\/b><br>Bruce Springsteen  (1984)<br>Danceability: 0.53<br>Valence: 0.5<br>Energy: 0.94","<b>Stuck On You<\/b><br>Lionel Richie  (1984)<br>Danceability: 0.52<br>Valence: 0.27<br>Energy: 0.35","<b>Jump (For My Love)<\/b><br>The Pointer Sisters  (1984)<br>Danceability: 0.86<br>Valence: 0.85<br>Energy: 0.7","<b>The Reflex - Single Version;2010 Remastered Version<\/b><br>Duran Duran  (1984)<br>Danceability: 0.55<br>Valence: 0.82<br>Energy: 0.89","<b>State of Shock<\/b><br>The Jacksons  (1984)<br>Danceability: 0.98<br>Valence: 0.89<br>Energy: 0.85","<b>Eyes Without A Face<\/b><br>Billy Idol  (1984)<br>Danceability: 0.54<br>Valence: 0.64<br>Energy: 0.8","<b>Missing You<\/b><br>John Waite  (1984)<br>Danceability: 0.55<br>Valence: 0.59<br>Energy: 0.55","<b>Shout<\/b><br>Tears For Fears  (1985)<br>Danceability: 0.51<br>Valence: 0.47<br>Energy: 0.94","<b>Everytime You Go Away<\/b><br>Paul Young  (1985)<br>Danceability: 0.61<br>Valence: 0.72<br>Energy: 0.64","<b>The Power Of Love<\/b><br>Huey Lewis & The News  (1985)<br>Danceability: 0.77<br>Valence: 0.95<br>Energy: 0.83","<b>A View To A Kill<\/b><br>Duran Duran  (1985)<br>Danceability: 0.71<br>Valence: 0.78<br>Energy: 0.81","<b>Sussudio<\/b><br>Phil Collins  (1985)<br>Danceability: 0.47<br>Valence: 0.46<br>Energy: 0.82","<b>St. Elmos Fire (Man In Motion)<\/b><br>John Parr  (1985)<br>Danceability: 0.61<br>Valence: 0.47<br>Energy: 0.6","<b>If You Love Somebody Set Them Free<\/b><br>Sting  (1985)<br>Danceability: 0.65<br>Valence: 0.79<br>Energy: 0.83","<b>Raspberry Beret<\/b><br>Prince  (1985)<br>Danceability: 0.76<br>Valence: 0.91<br>Energy: 0.67","<b>Never Surrender<\/b><br>Corey Hart  (1985)<br>Danceability: 0.55<br>Valence: 0.48<br>Energy: 0.55","<b>Freeway Of Love<\/b><br>Aretha Franklin  (1985)<br>Danceability: 0.69<br>Valence: 0.85<br>Energy: 0.84","<b>Papa Don't Preach<\/b><br>Madonna  (1986)<br>Danceability: 0.81<br>Valence: 0.97<br>Energy: 0.91","<b>Glory Of Love<\/b><br>Peter Cetera  (1986)<br>Danceability: 0.52<br>Valence: 0.31<br>Energy: 0.58","<b>Sledgehammer<\/b><br>Peter Gabriel  (1986)<br>Danceability: 0.64<br>Valence: 0.45<br>Energy: 0.67","<b>Invisible Touch<\/b><br>Genesis  (1986)<br>Danceability: 0.69<br>Valence: 0.83<br>Energy: 0.93","<b>Higher Love - Single Version<\/b><br>Steve Winwood  (1986)<br>Danceability: 0.71<br>Valence: 0.94<br>Energy: 0.9","<b>There'll Be Sad Songs (To Make You Cry)<\/b><br>Billy Ocean  (1986)<br>Danceability: 0.52<br>Valence: 0.25<br>Energy: 0.41","<b>Venus<\/b><br>Bananarama  (1986)<br>Danceability: 0.72<br>Valence: 0.91<br>Energy: 0.96","<b>Holding Back The Years<\/b><br>Simply Red  (1986)<br>Danceability: 0.67<br>Valence: 0.57<br>Energy: 0.51","<b>Mad About You<\/b><br>Belinda Carlisle  (1986)<br>Danceability: 0.58<br>Valence: 0.36<br>Energy: 0.74","<b>Danger Zone<\/b><br>Kenny Loggins  (1986)<br>Danceability: 0.54<br>Valence: 0.65<br>Energy: 0.9","<b>Alone<\/b><br>Heart  (1987)<br>Danceability: 0.42<br>Valence: 0.17<br>Energy: 0.45","<b>I Wanna Dance with Somebody (Who Loves Me)<\/b><br>Whitney Houston  (1987)<br>Danceability: 0.71<br>Valence: 0.87<br>Energy: 0.82","<b>I Still Haven't Found What I'm Looking For<\/b><br>U2  (1987)<br>Danceability: 0.57<br>Valence: 0.59<br>Energy: 0.78","<b>Shakedown<\/b><br>Bob Seger  (1987)<br>Danceability: 0.57<br>Valence: 0.48<br>Energy: 0.94","<b>La Bamba<\/b><br>Los Lobos  (1987)<br>Danceability: 0.35<br>Valence: 0.89<br>Energy: 0.75","<b>I Want Your Sex - Pts. 1 & 2 Remastered<\/b><br>George Michael  (1987)<br>Danceability: 0.81<br>Valence: 0.8<br>Energy: 0.6","<b>Who's That Girl<\/b><br>Madonna  (1987)<br>Danceability: 0.62<br>Valence: 0.83<br>Energy: 0.65","<b>Only In My Dreams<\/b><br>Debbie Gibson  (1987)<br>Danceability: 0.62<br>Valence: 0.91<br>Energy: 0.79","<b>Heart And Soul<\/b><br>T'Pau  (1987)<br>Danceability: 0.46<br>Valence: 0.57<br>Energy: 0.66","<b>Luka<\/b><br>Suzanne Vega  (1987)<br>Danceability: 0.64<br>Valence: 0.8<br>Energy: 0.67","<b>Roll With It<\/b><br>Steve Winwood  (1988)<br>Danceability: 0.76<br>Valence: 0.95<br>Energy: 0.64","<b>The Flame<\/b><br>Cheap Trick  (1988)<br>Danceability: 0.63<br>Valence: 0.34<br>Energy: 0.67","<b>Monkey - Remastered<\/b><br>George Michael  (1988)<br>Danceability: 0.68<br>Valence: 0.7<br>Energy: 0.88","<b>Hold On To The Nights<\/b><br>Richard Marx  (1988)<br>Danceability: 0.35<br>Valence: 0.18<br>Energy: 0.21","<b>Pour Some Sugar On Me - Remastered 2017<\/b><br>Def Leppard  (1988)<br>Danceability: 0.53<br>Valence: 0.59<br>Energy: 0.96","<b>Hands To Heaven<\/b><br>Breathe  (1988)<br>Danceability: 0.54<br>Valence: 0.41<br>Energy: 0.38","<b>Sweet Child O' Mine<\/b><br>Guns N' Roses  (1988)<br>Danceability: 0.45<br>Valence: 0.7<br>Energy: 0.9","<b>Make Me Lose Control<\/b><br>Eric Carmen  (1988)<br>Danceability: 0.72<br>Valence: 0.75<br>Energy: 0.62","<b>I Don't Wanna Go On With You Like That<\/b><br>Elton John  (1988)<br>Danceability: 0.66<br>Valence: 0.96<br>Energy: 0.89","<b>I Don't Wanna Live Without Your Love<\/b><br>Chicago  (1988)<br>Danceability: 0.43<br>Valence: 0.15<br>Energy: 0.67","<b>Right Here Waiting<\/b><br>Richard Marx  (1989)<br>Danceability: 0.4<br>Valence: 0.13<br>Energy: 0.25","<b>Toy Soldiers<\/b><br>Martika  (1989)<br>Danceability: 0.52<br>Valence: 0.2<br>Energy: 0.44","<b>Cold Hearted<\/b><br>Paula Abdul  (1989)<br>Danceability: 0.88<br>Valence: 0.72<br>Energy: 0.68","<b>If You Don't Know Me By Now<\/b><br>Simply Red  (1989)<br>Danceability: 0.5<br>Valence: 0.21<br>Energy: 0.37","<b>On Our Own<\/b><br>Bobby Brown  (1989)<br>Danceability: 0.66<br>Valence: 0.57<br>Energy: 0.9","<b>Don't Wanna Lose You<\/b><br>Gloria Estefan  (1989)<br>Danceability: 0.54<br>Valence: 0.47<br>Energy: 0.36","<b>Baby Don't Forget My Number<\/b><br>Milli Vanilli  (1989)<br>Danceability: 0.85<br>Valence: 0.85<br>Energy: 0.65","<b>Good Thing<\/b><br>Fine Young Cannibals  (1989)<br>Danceability: 0.75<br>Valence: 0.89<br>Energy: 0.59","<b>So Alive<\/b><br>Love and Rockets  (1989)<br>Danceability: 0.7<br>Valence: 0.92<br>Energy: 0.57","<b>Batdance<\/b><br>Prince  (1989)<br>Danceability: 0.68<br>Valence: 0.47<br>Energy: 0.68"],"type":"scatter","mode":"markers","marker":{"autocolorscale":false,"color":"rgba(240,228,66,1)","opacity":0.75,"size":7.559055118110237,"symbol":"circle","line":{"width":1.8897637795275593,"color":"rgba(240,228,66,1)"}},"hoveron":"points","name":"1980s","legendgroup":"1980s","showlegend":true,"xaxis":"x","yaxis":"y","hoverinfo":"text","frame":null},{"x":[0.25600000000000001,0.78200000000000003,0.61699999999999999,0.78700000000000003,0.49299999999999999,0.72799999999999998,0.52000000000000002,0.73999999999999999,0.67600000000000005,0.50800000000000001,0.51900000000000002,0.70599999999999996,0.63700000000000001,0.65700000000000003,0.64800000000000002,0.67900000000000005,0.61399999999999999,0.55100000000000005,0.60899999999999999,0.82299999999999995,0.93600000000000005,0.64200000000000002,0.67900000000000005,0.40400000000000003,0.79500000000000004,0.499,0.55900000000000005,0.217,0.68300000000000005,0.71699999999999997,0.64200000000000002,0.871,0.69299999999999995,0.70299999999999996,0.38500000000000001,0.85099999999999998,0.879,0.72599999999999998,0.81100000000000005,0.88200000000000001,0.53200000000000003,0.44400000000000001,0.745,0.88800000000000001,0.30299999999999999,0.84199999999999997,0.57099999999999995,0.67400000000000004,0.55700000000000005,0.70599999999999996,0.77000000000000002,0.73099999999999998,0.72499999999999998,0.58299999999999996,0.63500000000000001,0.73299999999999998,0.57299999999999995,0.58299999999999996,0.56699999999999995,0.57999999999999996,0.92700000000000005,0.84799999999999998,0.60799999999999998,0.67900000000000005,0.77000000000000002,0.61499999999999999,0.84299999999999997,0.74399999999999999,0.64900000000000002,0.621,0.70899999999999996,0.61699999999999999,0.69299999999999995,0.79800000000000004,0.71499999999999997,0.64000000000000001,0.72599999999999998,0.85699999999999998,0.63500000000000001,0.77100000000000002,0.70399999999999996,0.55100000000000005,0.84699999999999998,0.81499999999999995,0.52600000000000002,0.40000000000000002,0.54900000000000004,0.76000000000000001,0.73499999999999999,0.63400000000000001,0.627,0.68000000000000005,0.84499999999999997,0.622,0.68899999999999995,0.71899999999999997,0.73099999999999998,0.86099999999999999,0.80600000000000005,0.68700000000000006],"y":[0.35399999999999998,0.90900000000000003,0.59899999999999998,0.88200000000000001,0.188,0.38300000000000001,0.72199999999999998,0.70699999999999996,0.84399999999999997,0.54000000000000004,0.29799999999999999,0.39500000000000002,0.93600000000000005,0.72299999999999998,0.80800000000000005,0.93000000000000005,0.44400000000000001,0.47399999999999998,0.31900000000000001,0.39600000000000002,0.65700000000000003,0.505,0.90000000000000002,0.27900000000000003,0.96499999999999997,0.070800000000000002,0.45800000000000002,0.27400000000000002,0.84099999999999997,0.73199999999999998,0.83599999999999997,0.627,0.56799999999999995,0.58199999999999996,0.312,0.80700000000000005,0.80400000000000005,0.58199999999999996,0.56699999999999995,0.754,0.23000000000000001,0.56999999999999995,0.752,0.76200000000000001,0.16,0.77800000000000002,0.20699999999999999,0.94499999999999995,0.24399999999999999,0.90300000000000002,0.75700000000000001,0.52900000000000003,0.69499999999999995,0.223,0.42299999999999999,0.96199999999999997,0.56499999999999995,0.46999999999999997,0.38,0.84699999999999998,0.96499999999999997,0.89300000000000002,0.60199999999999998,0.53400000000000003,0.77100000000000002,0.82199999999999995,0.92300000000000004,0.122,0.48799999999999999,0.161,0.37,0.629,0.66200000000000003,0.89600000000000002,0.61199999999999999,0.69999999999999996,0.751,0.91800000000000004,0.85699999999999998,0.61099999999999999,0.76100000000000001,0.64200000000000002,0.752,0.88700000000000001,0.251,0.108,0.76500000000000001,0.58099999999999996,0.626,0.17999999999999999,0.91200000000000003,0.81399999999999995,0.46100000000000002,0.77400000000000002,0.48399999999999999,0.75700000000000001,0.78000000000000003,0.69399999999999995,0.52900000000000003,0.58199999999999996],"text":["<b>Vision of Love<\/b><br>Mariah Carey  (1990)<br>Danceability: 0.26<br>Valence: 0.35<br>Energy: 0.46","<b>She Ain't Worth It<\/b><br>Glenn Medeiros  (1990)<br>Danceability: 0.78<br>Valence: 0.91<br>Energy: 0.88","<b>Cradle Of Love<\/b><br>Billy Idol  (1990)<br>Danceability: 0.62<br>Valence: 0.6<br>Energy: 0.94","<b>Step by Step<\/b><br>New Kids On The Block  (1990)<br>Danceability: 0.79<br>Valence: 0.88<br>Energy: 0.63","<b>If Wishes Came True<\/b><br>Sweet Sensation  (1990)<br>Danceability: 0.49<br>Valence: 0.19<br>Energy: 0.45","<b>Hold On<\/b><br>En Vogue  (1990)<br>Danceability: 0.73<br>Valence: 0.38<br>Energy: 0.46","<b>It Must Have Been Love<\/b><br>Roxette  (1990)<br>Danceability: 0.52<br>Valence: 0.72<br>Energy: 0.65","<b>The Power<\/b><br>SNAP!  (1990)<br>Danceability: 0.74<br>Valence: 0.71<br>Energy: 0.88","<b>Rub You The Right Way<\/b><br>Johnny Gill  (1990)<br>Danceability: 0.68<br>Valence: 0.84<br>Energy: 0.87","<b>Unskinny Bop<\/b><br>Poison  (1990)<br>Danceability: 0.51<br>Valence: 0.54<br>Energy: 0.78","<b>(Everything I Do) I Do It For You<\/b><br>Bryan Adams  (1991)<br>Danceability: 0.52<br>Valence: 0.3<br>Energy: 0.37","<b>Rush, Rush<\/b><br>Paula Abdul  (1991)<br>Danceability: 0.71<br>Valence: 0.4<br>Energy: 0.44","<b>Unbelievable<\/b><br>EMF  (1991)<br>Danceability: 0.64<br>Valence: 0.94<br>Energy: 0.85","<b>Right Here Right Now<\/b><br>Jesus Jones  (1991)<br>Danceability: 0.66<br>Valence: 0.72<br>Energy: 0.61","<b>Every Heartbeat<\/b><br>Amy Grant  (1991)<br>Danceability: 0.65<br>Valence: 0.81<br>Energy: 0.89","<b>It Ain't Over 'Til It's Over<\/b><br>Lenny Kravitz  (1991)<br>Danceability: 0.68<br>Valence: 0.93<br>Energy: 0.57","<b>Summertime - Single Edit<\/b><br>DJ Jazzy Jeff & The Fresh Prince  (1991)<br>Danceability: 0.61<br>Valence: 0.44<br>Energy: 0.75","<b>I Wanna Sex You Up - Single Mix<\/b><br>Color Me Badd  (1991)<br>Danceability: 0.55<br>Valence: 0.47<br>Energy: 0.59","<b>Fading Like A Flower<\/b><br>Roxette  (1991)<br>Danceability: 0.61<br>Valence: 0.32<br>Energy: 0.67","<b>P. A. S. S. I. O. N (Passion) [Originally Performed By Rhythm Syndicate] [Karaoke Version]<\/b><br>Party City  (1991)<br>Danceability: 0.82<br>Valence: 0.4<br>Energy: 0.53","<b>Baby Got Back<\/b><br>Sir Mix-A-Lot  (1992)<br>Danceability: 0.94<br>Valence: 0.66<br>Energy: 0.76","<b>End Of The Road<\/b><br>Boyz II Men  (1992)<br>Danceability: 0.64<br>Valence: 0.5<br>Energy: 0.42","<b>Baby-Baby-Baby<\/b><br>TLC  (1992)<br>Danceability: 0.68<br>Valence: 0.9<br>Energy: 0.6","<b>I'll Be There<\/b><br>Mariah Carey  (1992)<br>Danceability: 0.4<br>Valence: 0.28<br>Energy: 0.51","<b>Achy Breaky Heart<\/b><br>Billy Ray Cyrus  (1992)<br>Danceability: 0.8<br>Valence: 0.96<br>Energy: 0.58","<b>This Used To Be My Playground<\/b><br>Madonna  (1992)<br>Danceability: 0.5<br>Valence: 0.07<br>Energy: 0.31","<b>Under The Bridge<\/b><br>Red Hot Chili Peppers  (1992)<br>Danceability: 0.56<br>Valence: 0.46<br>Energy: 0.34","<b>November Rain<\/b><br>Guns N' Roses  (1992)<br>Danceability: 0.22<br>Valence: 0.27<br>Energy: 0.63","<b>Life Is A Highway<\/b><br>Tom Cochrane  (1992)<br>Danceability: 0.68<br>Valence: 0.84<br>Energy: 0.75","<b>Just Another Day<\/b><br>Jon Secada  (1992)<br>Danceability: 0.72<br>Valence: 0.73<br>Energy: 0.64","<b>(I Can't Help) Falling in Love With You<\/b><br>UB40  (1993)<br>Danceability: 0.64<br>Valence: 0.84<br>Energy: 0.72","<b>Whoomp! (There It Is) - Original Radio Edit<\/b><br>Viper  (1993)<br>Danceability: 0.87<br>Valence: 0.63<br>Energy: 0.67","<b>Weak<\/b><br>SWV  (1993)<br>Danceability: 0.69<br>Valence: 0.57<br>Energy: 0.53","<b>That's The Way Love Goes<\/b><br>Janet Jackson  (1993)<br>Danceability: 0.7<br>Valence: 0.58<br>Energy: 0.7","<b>Lately<\/b><br>Jodeci  (1993)<br>Danceability: 0.38<br>Valence: 0.31<br>Energy: 0.32","<b>I'm Gonna Be (500 Miles)<\/b><br>The Proclaimers  (1993)<br>Danceability: 0.85<br>Valence: 0.81<br>Energy: 0.55","<b>Slam<\/b><br>Onyx  (1993)<br>Danceability: 0.88<br>Valence: 0.8<br>Energy: 0.69","<b>Knockin' da Boots<\/b><br>H-Town  (1993)<br>Danceability: 0.73<br>Valence: 0.58<br>Energy: 0.5","<b>Show Me Love<\/b><br>Robin S  (1993)<br>Danceability: 0.81<br>Valence: 0.57<br>Energy: 0.83","<b>If I Had No Loot<\/b><br>Tony! Toni! Toné!  (1993)<br>Danceability: 0.88<br>Valence: 0.75<br>Energy: 0.71","<b>I Swear<\/b><br>All-4-One  (1994)<br>Danceability: 0.53<br>Valence: 0.23<br>Energy: 0.41","<b>Stay (I Missed You) - Storytellers<\/b><br>Lisa Loeb  (1994)<br>Danceability: 0.44<br>Valence: 0.57<br>Energy: 0.63","<b>Don't Turn Around<\/b><br>Ace of Base  (1994)<br>Danceability: 0.74<br>Valence: 0.75<br>Energy: 0.77","<b>Fantastic Voyage<\/b><br>Coolio  (1994)<br>Danceability: 0.89<br>Valence: 0.76<br>Energy: 0.58","<b>Can You Feel The Love Tonight<\/b><br>Elton John  (1994)<br>Danceability: 0.3<br>Valence: 0.16<br>Energy: 0.35","<b>Regulate<\/b><br>Warren G  (1994)<br>Danceability: 0.84<br>Valence: 0.78<br>Energy: 0.57","<b>Any Time, Any Place<\/b><br>Janet Jackson  (1994)<br>Danceability: 0.57<br>Valence: 0.21<br>Energy: 0.23","<b>Wild Night<\/b><br>John Mellencamp  (1994)<br>Danceability: 0.67<br>Valence: 0.94<br>Energy: 0.88","<b>I'll Make Love To You<\/b><br>Boyz II Men  (1994)<br>Danceability: 0.56<br>Valence: 0.24<br>Energy: 0.51","<b>Back & Forth<\/b><br>Aaliyah  (1994)<br>Danceability: 0.71<br>Valence: 0.9<br>Energy: 0.73","<b>Waterfalls<\/b><br>TLC  (1995)<br>Danceability: 0.77<br>Valence: 0.76<br>Energy: 0.5","<b>Don't Take It Personal (Just One of Dem Days)<\/b><br>Monica  (1995)<br>Danceability: 0.73<br>Valence: 0.53<br>Energy: 0.36","<b>One More Chance/Stay With Me - Remix<\/b><br>The Notorious B.I.G.  (1995)<br>Danceability: 0.72<br>Valence: 0.7<br>Energy: 0.68","<b>Kiss From A Rose<\/b><br>Seal  (1995)<br>Danceability: 0.58<br>Valence: 0.22<br>Energy: 0.53","<b>I Can Love You Like That<\/b><br>All-4-One  (1995)<br>Danceability: 0.64<br>Valence: 0.42<br>Energy: 0.71","<b>In the Summertime - feat. Rayvon<\/b><br>Shaggy  (1995)<br>Danceability: 0.73<br>Valence: 0.96<br>Energy: 0.68","<b>Water Runs Dry<\/b><br>Boyz II Men  (1995)<br>Danceability: 0.57<br>Valence: 0.56<br>Energy: 0.38","<b>Total Eclipse of the Heart (Total Bromance Mix Edit)<\/b><br>Nicki French  (1995)<br>Danceability: 0.58<br>Valence: 0.47<br>Energy: 0.75","<b>Have You Ever Really Loved A Woman?<\/b><br>Bryan Adams  (1995)<br>Danceability: 0.57<br>Valence: 0.38<br>Energy: 0.48","<b>Run-Around<\/b><br>Blues Traveler  (1995)<br>Danceability: 0.58<br>Valence: 0.85<br>Energy: 0.9","<b>Macarena<\/b><br>Los Del Rio  (1996)<br>Danceability: 0.93<br>Valence: 0.96<br>Energy: 0.72","<b>You're Makin' Me High<\/b><br>Toni Braxton  (1996)<br>Danceability: 0.85<br>Valence: 0.89<br>Energy: 0.56","<b>Give Me One Reason<\/b><br>Tracy Chapman  (1996)<br>Danceability: 0.61<br>Valence: 0.6<br>Energy: 0.42","<b>Tha Crossroads<\/b><br>Bone Thugs-N-Harmony  (1996)<br>Danceability: 0.68<br>Valence: 0.53<br>Energy: 0.51","<b>California Love - Original Mix (Explicit)<\/b><br>2Pac  (1996)<br>Danceability: 0.77<br>Valence: 0.77<br>Energy: 0.83","<b>Twisted<\/b><br>Keith Sweat  (1996)<br>Danceability: 0.62<br>Valence: 0.82<br>Energy: 0.51","<b>C'mon N' Ride It (The Train)<\/b><br>Quad City DJ's  (1996)<br>Danceability: 0.84<br>Valence: 0.92<br>Energy: 0.96","<b>I Love You Always Forever<\/b><br>Donna Lewis  (1996)<br>Danceability: 0.74<br>Valence: 0.12<br>Energy: 0.45","<b>Always Be My Baby<\/b><br>Mariah Carey  (1996)<br>Danceability: 0.65<br>Valence: 0.49<br>Energy: 0.53","<b>Because You Loved Me (Theme from \"Up Close and Personal\")<\/b><br>Céline Dion  (1996)<br>Danceability: 0.62<br>Valence: 0.16<br>Energy: 0.5","<b>I'll Be Missing You (feat. Faith Evans & 112)<\/b><br>Diddy  (1997)<br>Danceability: 0.71<br>Valence: 0.37<br>Energy: 0.35","<b>Bitch<\/b><br>Meredith Brooks  (1997)<br>Danceability: 0.62<br>Valence: 0.63<br>Energy: 0.89","<b>MMMBop<\/b><br>Hanson  (1997)<br>Danceability: 0.69<br>Valence: 0.66<br>Energy: 0.94","<b>Quit Playing Games (With My Heart)<\/b><br>Backstreet Boys  (1997)<br>Danceability: 0.8<br>Valence: 0.9<br>Energy: 0.86","<b>Return Of The Mack<\/b><br>Mark Morrison  (1997)<br>Danceability: 0.72<br>Valence: 0.61<br>Energy: 0.83","<b>Semi-Charmed Life<\/b><br>Third Eye Blind  (1997)<br>Danceability: 0.64<br>Valence: 0.7<br>Energy: 0.86","<b>Say You'll Be There - Single Mix<\/b><br>Spice Girls  (1997)<br>Danceability: 0.73<br>Valence: 0.75<br>Energy: 0.68","<b>Mo Money Mo Problems (feat. Mase & Puff Daddy)<\/b><br>The Notorious B.I.G.  (1997)<br>Danceability: 0.86<br>Valence: 0.92<br>Energy: 0.87","<b>Do You Know (What It Takes)<\/b><br>Robyn  (1997)<br>Danceability: 0.64<br>Valence: 0.86<br>Energy: 0.74","<b>Look into My Eyes<\/b><br>Bone Thugs-N-Harmony  (1997)<br>Danceability: 0.77<br>Valence: 0.61<br>Energy: 0.56","<b>The Boy Is Mine<\/b><br>Brandy  (1998)<br>Danceability: 0.7<br>Valence: 0.76<br>Energy: 0.71","<b>You're Still The One<\/b><br>Shania Twain  (1998)<br>Danceability: 0.55<br>Valence: 0.64<br>Energy: 0.53","<b>Too Close<\/b><br>Next  (1998)<br>Danceability: 0.85<br>Valence: 0.75<br>Energy: 0.4","<b>My Way<\/b><br>Usher  (1998)<br>Danceability: 0.81<br>Valence: 0.89<br>Energy: 0.52","<b>Adia<\/b><br>Sarah McLachlan  (1998)<br>Danceability: 0.53<br>Valence: 0.25<br>Energy: 0.37","<b>My All<\/b><br>Mariah Carey  (1998)<br>Danceability: 0.4<br>Valence: 0.11<br>Energy: 0.32","<b>Come With Me originally by Puff Daddy featuring Jimmy Page<\/b><br>Studio Group  (1998)<br>Danceability: 0.55<br>Valence: 0.76<br>Energy: 0.85","<b>Make It Hot (feat. Missy Elliott & Mocha)<\/b><br>Nicole  (1998)<br>Danceability: 0.76<br>Valence: 0.58<br>Energy: 0.46","<b>Crush<\/b><br>Jennifer Paige  (1998)<br>Danceability: 0.74<br>Valence: 0.63<br>Energy: 0.69","<b>All My Life<\/b><br>K-Ci & JoJo  (1998)<br>Danceability: 0.63<br>Valence: 0.18<br>Energy: 0.53","<b>Genie in a Bottle<\/b><br>Christina Aguilera  (1999)<br>Danceability: 0.63<br>Valence: 0.91<br>Energy: 0.8","<b>If You Had My Love<\/b><br>Jennifer Lopez  (1999)<br>Danceability: 0.68<br>Valence: 0.81<br>Energy: 0.62","<b>Bills, Bills, Bills<\/b><br>Destiny's Child  (1999)<br>Danceability: 0.84<br>Valence: 0.46<br>Energy: 0.58","<b>Last Kiss<\/b><br>Pearl Jam  (1999)<br>Danceability: 0.62<br>Valence: 0.77<br>Energy: 0.69","<b>I Want It That Way<\/b><br>Backstreet Boys  (1999)<br>Danceability: 0.69<br>Valence: 0.48<br>Energy: 0.7","<b>Where My Girls At<\/b><br>702  (1999)<br>Danceability: 0.72<br>Valence: 0.76<br>Energy: 0.74","<b>All Star<\/b><br>Smash Mouth  (1999)<br>Danceability: 0.73<br>Valence: 0.78<br>Energy: 0.86","<b>Wild Wild West - Album Version With Intro<\/b><br>Will Smith  (1999)<br>Danceability: 0.86<br>Valence: 0.69<br>Energy: 0.6","<b>It's Not Right But It's Okay<\/b><br>Whitney Houston  (1999)<br>Danceability: 0.81<br>Valence: 0.53<br>Energy: 0.8","<b>Tell Me It's Real<\/b><br>K-Ci & JoJo  (1999)<br>Danceability: 0.69<br>Valence: 0.58<br>Energy: 0.53"],"type":"scatter","mode":"markers","marker":{"autocolorscale":false,"color":"rgba(0,114,178,1)","opacity":0.75,"size":7.559055118110237,"symbol":"circle","line":{"width":1.8897637795275593,"color":"rgba(0,114,178,1)"}},"hoveron":"points","name":"1990s","legendgroup":"1990s","showlegend":true,"xaxis":"x","yaxis":"y","hoverinfo":"text","frame":null},{"x":[0.51800000000000002,0.63600000000000001,0.77300000000000002,0.63700000000000001,0.72499999999999998,0.77100000000000002,0.45400000000000001,0.75,0.48099999999999998,0.63500000000000001,0.65700000000000003,0.90900000000000003,0.53100000000000003,0.69799999999999995,0.46800000000000003,0.70599999999999996,0.83999999999999997,0.59599999999999997,0.65300000000000002,0.64100000000000001,0.96599999999999997,0.58499999999999996,0.73099999999999998,0.92900000000000005,0.71299999999999997,0.42699999999999999,0.63400000000000001,0.47699999999999998,0.56000000000000005,0.53800000000000003,0.44800000000000001,0.80900000000000005,0.86599999999999999,0.25600000000000001,0.71199999999999997,0.73499999999999999,0.53300000000000003,0.52800000000000002,0.67300000000000004,0.80100000000000005,0.84499999999999997,0.71099999999999997,0.79000000000000004,0.69799999999999995,0.44700000000000001,0.60899999999999999,0.876,0.67700000000000005,0.79100000000000004,0.84499999999999997,0.83799999999999997,0.92000000000000004,0.879,0.76800000000000002,0.54800000000000004,0.69099999999999995,0.90400000000000003,0.81499999999999995,0.69599999999999995,0.45600000000000002,0.81100000000000005,0.77800000000000002,0.83499999999999996,0.80300000000000005,0.88800000000000001,0.54400000000000004,0.78800000000000003,0.59099999999999997,0.86199999999999999,0.65600000000000003,0.57899999999999996,0.71199999999999997,0.61699999999999999,0.65100000000000002,0.73399999999999999,0.45100000000000001,0.76200000000000001,0.80600000000000005,0.67500000000000004,0.60499999999999998,0.69899999999999995,0.70499999999999996,0.82799999999999996,0.48499999999999999,0.63800000000000001,0.67200000000000004,0.72099999999999997,0.70599999999999996,0.69299999999999995,0.95099999999999996,0.74099999999999999,0.86899999999999999,0.59299999999999997,0.42699999999999999,0.69099999999999995,0.83899999999999997,0.89200000000000002,0.81699999999999995,0.27600000000000002,0.52400000000000002],"y":[0.52700000000000002,0.90800000000000003,0.79100000000000004,0.74099999999999999,0.59899999999999998,0.68300000000000005,0.45100000000000001,0.29099999999999998,0.66000000000000003,0.91500000000000004,0.72599999999999998,0.86899999999999999,0.42399999999999999,0.67000000000000004,0.499,0.77500000000000002,0.63700000000000001,0.85999999999999999,0.48199999999999998,0.216,0.91100000000000003,0.42699999999999999,0.58099999999999996,0.65500000000000003,0.78100000000000003,0.30399999999999999,0.91300000000000003,0.68999999999999995,0.26800000000000002,0.91300000000000003,0.83699999999999997,0.32100000000000001,0.89100000000000001,0.42899999999999999,0.85099999999999998,0.72599999999999998,0.432,0.372,0.84999999999999998,0.81200000000000006,0.70199999999999996,0.87,0.53600000000000003,0.69099999999999995,0.069500000000000006,0.16600000000000001,0.82799999999999996,0.875,0.55400000000000005,0.81899999999999995,0.77800000000000002,0.91500000000000004,0.58199999999999996,0.46200000000000002,0.42499999999999999,0.57399999999999995,0.81000000000000005,0.61099999999999999,0.52400000000000002,0.39100000000000001,0.878,0.75600000000000001,0.61199999999999999,0.73899999999999999,0.60899999999999999,0.434,0.84399999999999997,0.33400000000000002,0.51100000000000001,0.90400000000000003,0.55100000000000005,0.29199999999999998,0.64200000000000002,0.313,0.80500000000000005,0.59399999999999997,0.76900000000000002,0.89500000000000002,0.40500000000000003,0.57299999999999995,0.69599999999999995,0.55200000000000005,0.44,0.41599999999999998,0.22500000000000001,0.44600000000000001,0.65500000000000003,0.72899999999999998,0.88900000000000001,0.78000000000000003,0.59999999999999998,0.39500000000000002,0.67100000000000004,0.58099999999999996,0.45400000000000001,0.88800000000000001,0.84999999999999998,0.83299999999999996,0.17299999999999999,0.58999999999999997],"text":["<b>Bent<\/b><br>Matchbox Twenty  (2000)<br>Danceability: 0.52<br>Valence: 0.53<br>Energy: 0.83","<b>It's Gonna Be Me<\/b><br>*NSYNC  (2000)<br>Danceability: 0.64<br>Valence: 0.91<br>Energy: 0.87","<b>Doesn't Really Matter<\/b><br>Janet Jackson  (2000)<br>Danceability: 0.77<br>Valence: 0.79<br>Energy: 0.8","<b>Everything You Want<\/b><br>Vertical Horizon  (2000)<br>Danceability: 0.64<br>Valence: 0.74<br>Energy: 0.76","<b>I Wanna Know<\/b><br>Joe  (2000)<br>Danceability: 0.72<br>Valence: 0.6<br>Energy: 0.49","<b>Jumpin', Jumpin'<\/b><br>Destiny's Child  (2000)<br>Danceability: 0.77<br>Valence: 0.68<br>Energy: 0.69","<b>Higher<\/b><br>Creed  (2000)<br>Danceability: 0.45<br>Valence: 0.45<br>Energy: 0.83","<b>Incomplete<\/b><br>Sisqo  (2000)<br>Danceability: 0.75<br>Valence: 0.29<br>Energy: 0.45","<b>Absolutely (Story of a Girl) - Radio Mix<\/b><br>Nine Days  (2000)<br>Danceability: 0.48<br>Valence: 0.66<br>Energy: 0.94","<b>Try Again<\/b><br>Aaliyah Tribute  (2000)<br>Danceability: 0.64<br>Valence: 0.92<br>Energy: 0.84","<b>U Remind Me<\/b><br>Usher  (2001)<br>Danceability: 0.66<br>Valence: 0.73<br>Energy: 0.63","<b>Let Me Blow Ya Mind<\/b><br>Eve  (2001)<br>Danceability: 0.91<br>Valence: 0.87<br>Energy: 0.54","<b>Hanging By A Moment<\/b><br>Lifehouse  (2001)<br>Danceability: 0.53<br>Valence: 0.42<br>Energy: 0.86","<b>Hit 'Em up Style (Oops!)<\/b><br>Blu Cantrell  (2001)<br>Danceability: 0.7<br>Valence: 0.67<br>Energy: 0.72","<b>Drops of Jupiter<\/b><br>Train  (2001)<br>Danceability: 0.47<br>Valence: 0.5<br>Energy: 0.64","<b>Peaches & Cream (feat. P. Diddy)<\/b><br>112  (2001)<br>Danceability: 0.71<br>Valence: 0.78<br>Energy: 0.54","<b>Bootylicious<\/b><br>Destiny's Child  (2001)<br>Danceability: 0.84<br>Valence: 0.64<br>Energy: 0.84","<b>Where the Party At<\/b><br>Jagged Edge  (2001)<br>Danceability: 0.6<br>Valence: 0.86<br>Energy: 0.66","<b>Fallin'<\/b><br>Alicia Keys  (2001)<br>Danceability: 0.65<br>Valence: 0.48<br>Energy: 0.61","<b>All or Nothing<\/b><br>O-Town  (2001)<br>Danceability: 0.64<br>Valence: 0.22<br>Energy: 0.45","<b>Hot In Herre<\/b><br>Nelly  (2002)<br>Danceability: 0.97<br>Valence: 0.91<br>Energy: 0.77","<b>Complicated<\/b><br>Avril Lavigne  (2002)<br>Danceability: 0.58<br>Valence: 0.43<br>Energy: 0.78","<b>Dilemma<\/b><br>Nelly  (2002)<br>Danceability: 0.73<br>Valence: 0.58<br>Energy: 0.54","<b>Without Me<\/b><br>Eminem  (2002)<br>Danceability: 0.93<br>Valence: 0.66<br>Energy: 0.65","<b>I Need a Girl Part 2 (feat. Loon, Ginuwine & Mario Winans)<\/b><br>Diddy  (2002)<br>Danceability: 0.71<br>Valence: 0.78<br>Energy: 0.47","<b>Hero (feat. Josey Scott)<\/b><br>Chad Kroeger  (2002)<br>Danceability: 0.43<br>Valence: 0.3<br>Energy: 0.84","<b>The Middle<\/b><br>Jimmy Eat World  (2002)<br>Danceability: 0.63<br>Valence: 0.91<br>Energy: 0.85","<b>Foolish<\/b><br>Ashanti  (2002)<br>Danceability: 0.48<br>Valence: 0.69<br>Energy: 0.73","<b>A Thousand Miles<\/b><br>Vanessa Carlton  (2002)<br>Danceability: 0.56<br>Valence: 0.27<br>Energy: 0.82","<b>Just a Friend 2002 - Radio Edit<\/b><br>Mario  (2002)<br>Danceability: 0.54<br>Valence: 0.91<br>Energy: 0.68","<b>Crazy in Love - Beyonce Feat. Jay Z<\/b><br>Rhythm Spectacle  (2003)<br>Danceability: 0.45<br>Valence: 0.84<br>Energy: 0.59","<b>Magic Stick (as made famous by Lil Kim feat. 50 Cent)<\/b><br>Urban All Stars  (2003)<br>Danceability: 0.81<br>Valence: 0.32<br>Energy: 0.46","<b>Right Thurr<\/b><br>Chingy  (2003)<br>Danceability: 0.87<br>Valence: 0.89<br>Energy: 0.75","<b>Unwell<\/b><br>Matchbox Twenty  (2003)<br>Danceability: 0.26<br>Valence: 0.43<br>Energy: 0.79","<b>Rock Wit U (Awww Baby)<\/b><br>Ashanti  (2003)<br>Danceability: 0.71<br>Valence: 0.85<br>Energy: 0.8","<b>Get Busy<\/b><br>Sean Paul  (2003)<br>Danceability: 0.74<br>Valence: 0.73<br>Energy: 0.82","<b>Bring Me To Life (In The Style Of Evanescence feat Paul Mccoy)<\/b><br>Music Factory Karaoke  (2003)<br>Danceability: 0.53<br>Valence: 0.43<br>Energy: 0.78","<b>This Is The Night<\/b><br>Clay Aiken  (2003)<br>Danceability: 0.53<br>Valence: 0.37<br>Energy: 0.56","<b>P.I.M.P.<\/b><br>50 Cent  (2003)<br>Danceability: 0.67<br>Valence: 0.85<br>Energy: 0.78","<b>Never Leave You Uh Ooh, Uh Ooh! (Lumidee)<\/b><br>Starlite Karaoke  (2003)<br>Danceability: 0.8<br>Valence: 0.81<br>Energy: 0.59","<b>Confessions Part II - Confessions Special Edition Version<\/b><br>Usher  (2004)<br>Danceability: 0.84<br>Valence: 0.7<br>Energy: 0.47","<b>Slow Motion<\/b><br>Juvenile  (2004)<br>Danceability: 0.71<br>Valence: 0.87<br>Energy: 0.73","<b>Burn - Radio Mix<\/b><br>Usher  (2004)<br>Danceability: 0.79<br>Valence: 0.54<br>Energy: 0.54","<b>Lean Back<\/b><br>Terror Squad  (2004)<br>Danceability: 0.7<br>Valence: 0.69<br>Energy: 0.92","<b>The Reason<\/b><br>Hoobastank  (2004)<br>Danceability: 0.45<br>Valence: 0.07<br>Energy: 0.67","<b>If I Ain't Got You<\/b><br>Alicia Keys  (2004)<br>Danceability: 0.61<br>Valence: 0.17<br>Energy: 0.44","<b>Move Ya Body<\/b><br>Nina Sky  (2004)<br>Danceability: 0.88<br>Valence: 0.83<br>Energy: 0.71","<b>Turn Me On<\/b><br>Kevin Lyttle  (2004)<br>Danceability: 0.68<br>Valence: 0.88<br>Energy: 0.68","<b>Dip It Low<\/b><br>Christina Milian  (2004)<br>Danceability: 0.79<br>Valence: 0.55<br>Energy: 0.72","<b>Sunshine<\/b><br>Lil' Flip  (2004)<br>Danceability: 0.84<br>Valence: 0.82<br>Energy: 0.35","<b>We Belong Together<\/b><br>Mariah Carey  (2005)<br>Danceability: 0.84<br>Valence: 0.78<br>Energy: 0.47","<b>Hollaback Girl<\/b><br>Gwen Stefani  (2005)<br>Danceability: 0.92<br>Valence: 0.92<br>Energy: 0.92","<b>Don't Cha<\/b><br>The Pussycat Dolls  (2005)<br>Danceability: 0.88<br>Valence: 0.58<br>Energy: 0.64","<b>Pon de Replay<\/b><br>Rihanna  (2005)<br>Danceability: 0.77<br>Valence: 0.46<br>Energy: 0.64","<b>Behind These Hazel Eyes<\/b><br>Kelly Clarkson  (2005)<br>Danceability: 0.55<br>Valence: 0.42<br>Energy: 0.89","<b>Don't Phunk With My Heart<\/b><br>The Black Eyed Peas  (2005)<br>Danceability: 0.69<br>Valence: 0.57<br>Energy: 0.93","<b>Lose Control (feat. Ciara & Fat Man Scoop)<\/b><br>Missy Elliott  (2005)<br>Danceability: 0.9<br>Valence: 0.81<br>Energy: 0.81","<b>Let Me Hold You<\/b><br>Bow Wow  (2005)<br>Danceability: 0.81<br>Valence: 0.61<br>Energy: 0.66","<b>Just A Lil Bit<\/b><br>50 Cent  (2005)<br>Danceability: 0.7<br>Valence: 0.52<br>Energy: 0.71","<b>You And Me<\/b><br>Lifehouse  (2005)<br>Danceability: 0.46<br>Valence: 0.39<br>Energy: 0.43","<b>Promiscuous<\/b><br>Nelly Furtado  (2006)<br>Danceability: 0.81<br>Valence: 0.88<br>Energy: 0.97","<b>Hips Don't Lie<\/b><br>Shakira  (2006)<br>Danceability: 0.78<br>Valence: 0.76<br>Energy: 0.82","<b>Crazy<\/b><br>Gnarls Barkley  (2006)<br>Danceability: 0.84<br>Valence: 0.61<br>Energy: 0.74","<b>Me & U<\/b><br>Cassie  (2006)<br>Danceability: 0.8<br>Valence: 0.74<br>Energy: 0.45","<b>It's Goin' Down (feat. Nitti)<\/b><br>Yung Joc  (2006)<br>Danceability: 0.89<br>Valence: 0.61<br>Energy: 0.58","<b>Buttons<\/b><br>The Pussycat Dolls  (2006)<br>Danceability: 0.54<br>Valence: 0.43<br>Energy: 0.82","<b>Ridin'<\/b><br>Chamillionaire  (2006)<br>Danceability: 0.79<br>Valence: 0.84<br>Energy: 0.81","<b>Unfaithful<\/b><br>Rihanna  (2006)<br>Danceability: 0.59<br>Valence: 0.33<br>Energy: 0.41","<b>Ain't No Other Man<\/b><br>Christina Aguilera  (2006)<br>Danceability: 0.86<br>Valence: 0.51<br>Energy: 0.74","<b>Over My Head<\/b><br>Echosmith  (2006)<br>Danceability: 0.66<br>Valence: 0.9<br>Energy: 0.72","<b>Umbrella<\/b><br>Rihanna  (2007)<br>Danceability: 0.58<br>Valence: 0.55<br>Energy: 0.82","<b>Big Girls Don't Cry (Personal)<\/b><br>Fergie  (2007)<br>Danceability: 0.71<br>Valence: 0.29<br>Energy: 0.65","<b>Party Like A Rock Star<\/b><br>Shop Boyz  (2007)<br>Danceability: 0.62<br>Valence: 0.64<br>Energy: 0.69","<b>Hey There Delilah<\/b><br>Plain White T's  (2007)<br>Danceability: 0.65<br>Valence: 0.31<br>Energy: 0.29","<b>The Way I Are<\/b><br>Timbaland  (2007)<br>Danceability: 0.73<br>Valence: 0.8<br>Energy: 0.81","<b>Buy U a Drank (Shawty Snappin')<\/b><br>T-Pain  (2007)<br>Danceability: 0.45<br>Valence: 0.59<br>Energy: 0.55","<b>Beautiful Girls<\/b><br>Sean Kingston  (2007)<br>Danceability: 0.76<br>Valence: 0.77<br>Energy: 0.66","<b>Makes Me Wonder<\/b><br>Maroon 5  (2007)<br>Danceability: 0.81<br>Valence: 0.9<br>Energy: 0.87","<b>Bartender<\/b><br>T-Pain  (2007)<br>Danceability: 0.68<br>Valence: 0.41<br>Energy: 0.39","<b>Make Me Better<\/b><br>Fabolous  (2007)<br>Danceability: 0.6<br>Valence: 0.57<br>Energy: 0.61","<b>I Kissed a Girl<\/b><br>Katy Perry  (2008)<br>Danceability: 0.7<br>Valence: 0.7<br>Energy: 0.76","<b>Take A Bow<\/b><br>Rihanna  (2008)<br>Danceability: 0.7<br>Valence: 0.55<br>Energy: 0.47","<b>Lollipop<\/b><br>Lil Wayne  (2008)<br>Danceability: 0.83<br>Valence: 0.44<br>Energy: 0.43","<b>Viva La Vida<\/b><br>Coldplay  (2008)<br>Danceability: 0.48<br>Valence: 0.42<br>Energy: 0.62","<b>Bleeding Love<\/b><br>Leona Lewis  (2008)<br>Danceability: 0.64<br>Valence: 0.22<br>Energy: 0.66","<b>Forever<\/b><br>Chris Brown  (2008)<br>Danceability: 0.67<br>Valence: 0.45<br>Energy: 0.82","<b>Pocketful of Sunshine<\/b><br>Natasha Bedingfield  (2008)<br>Danceability: 0.72<br>Valence: 0.66<br>Energy: 0.88","<b>Disturbia<\/b><br>Rihanna  (2008)<br>Danceability: 0.71<br>Valence: 0.73<br>Energy: 0.79","<b>Leavin'<\/b><br>Jesse McCartney  (2008)<br>Danceability: 0.69<br>Valence: 0.89<br>Energy: 0.71","<b>Dangerous<\/b><br>Kardinal Offishall  (2008)<br>Danceability: 0.95<br>Valence: 0.78<br>Energy: 0.79","<b>I Gotta Feeling<\/b><br>The Black Eyed Peas  (2009)<br>Danceability: 0.74<br>Valence: 0.6<br>Energy: 0.75","<b>Boom Boom Pow<\/b><br>The Black Eyed Peas  (2009)<br>Danceability: 0.87<br>Valence: 0.4<br>Energy: 0.85","<b>Knock You Down<\/b><br>Keri Hilson  (2009)<br>Danceability: 0.59<br>Valence: 0.67<br>Energy: 0.88","<b>Best I Ever Had<\/b><br>Drake  (2009)<br>Danceability: 0.43<br>Valence: 0.58<br>Energy: 0.86","<b>You Belong With Me<\/b><br>Taylor Swift  (2009)<br>Danceability: 0.69<br>Valence: 0.45<br>Energy: 0.77","<b>Fire Burning<\/b><br>Sean Kingston  (2009)<br>Danceability: 0.84<br>Valence: 0.89<br>Energy: 0.8","<b>LoveGame<\/b><br>Lady Gaga  (2009)<br>Danceability: 0.89<br>Valence: 0.85<br>Energy: 0.65","<b>I Know You Want Me (Calle Ocho)<\/b><br>Pitbull  (2009)<br>Danceability: 0.82<br>Valence: 0.83<br>Energy: 0.73","<b>Use Somebody<\/b><br>Kings of Leon  (2009)<br>Danceability: 0.28<br>Valence: 0.17<br>Energy: 0.72","<b>Waking Up In Vegas<\/b><br>Katy Perry  (2009)<br>Danceability: 0.52<br>Valence: 0.59<br>Energy: 0.88"],"type":"scatter","mode":"markers","marker":{"autocolorscale":false,"color":"rgba(213,94,0,1)","opacity":0.75,"size":7.559055118110237,"symbol":"circle","line":{"width":1.8897637795275593,"color":"rgba(213,94,0,1)"}},"hoveron":"points","name":"2000s","legendgroup":"2000s","showlegend":true,"xaxis":"x","yaxis":"y","hoverinfo":"text","frame":null},{"x":[0.79100000000000004,0.64700000000000002,0.66000000000000003,0.78100000000000003,0.75,0.63300000000000001,0.76800000000000002,0.65900000000000003,0.623,0.85099999999999998,0.82099999999999995,0.73299999999999998,0.72899999999999998,0.64900000000000002,0.72699999999999998,0.64100000000000001,0.58299999999999996,0.63600000000000001,0.68600000000000005,0.62,0.77800000000000002,0.73999999999999999,0.85699999999999998,0.51400000000000001,0.68400000000000005,0.71199999999999997,0.754,0.59899999999999998,0.378,0.748,0.86199999999999999,0.47799999999999998,0.81000000000000005,0.61299999999999999,0.64100000000000001,0.46400000000000002,0.57399999999999995,0.874,0.871,0.51700000000000002,0.91100000000000003,0.74299999999999999,0.66000000000000003,0.42999999999999999,0.72499999999999998,0.42199999999999999,0.69699999999999995,0.65200000000000002,0.59599999999999997,0.81799999999999995,0.78000000000000003,0.68899999999999995,0.65000000000000002,0.71099999999999997,0.82499999999999996,0.746,0.57799999999999996,0.72299999999999998,0.54000000000000004,0.56399999999999995,0.79600000000000004,0.66900000000000004,0.53200000000000003,0.628,0.63,0.71999999999999997,0.68799999999999994,0.64500000000000002,0.80300000000000005,0.68999999999999995,0.63,0.85299999999999998,0.59899999999999998,0.61299999999999999,0.82499999999999996,0.78400000000000003,0.90400000000000003,0.72599999999999998,0.73099999999999998,0.627],"y":[0.42499999999999999,0.61399999999999999,0.377,0.32600000000000001,0.82399999999999995,0.65900000000000003,0.625,0.72999999999999998,0.82199999999999995,0.67900000000000005,0.17599999999999999,0.29899999999999999,0.52200000000000002,0.76500000000000001,0.65300000000000002,0.28999999999999998,0.40100000000000002,0.63700000000000001,0.81399999999999995,0.76000000000000001,0.626,0.505,0.754,0.57499999999999996,0.77300000000000002,0.39100000000000001,0.75,0.23300000000000001,0.73499999999999999,0.68999999999999995,0.83599999999999997,0.17599999999999999,0.86299999999999999,0.48399999999999999,0.84699999999999998,0.877,0.51200000000000001,0.93700000000000006,0.76900000000000002,0.57799999999999996,0.375,0.89900000000000002,0.625,0.13700000000000001,0.76000000000000001,0.33100000000000002,0.72099999999999997,0.96199999999999997,0.74299999999999999,0.081500000000000003,0.59399999999999997,0.28299999999999997,0.28999999999999998,0.58699999999999997,0.96499999999999997,0.81699999999999995,0.61899999999999999,0.27400000000000002,0.107,0.33000000000000002,0.39100000000000001,0.69999999999999996,0.42199999999999999,0.73199999999999998,0.46500000000000002,0.246,0.27500000000000002,0.56599999999999995,0.59299999999999997,0.56200000000000006,0.81299999999999994,0.85999999999999999,0.81699999999999995,0.61899999999999999,0.93100000000000005,0.72199999999999998,0.40000000000000002,0.73299999999999998,0.63100000000000001,0.504],"text":["<b>California Gurls - feat. Snoop Dogg<\/b><br>Katy Perry  (2010)<br>Danceability: 0.79<br>Valence: 0.42<br>Energy: 0.75","<b>Love The Way You Lie<\/b><br>Eminem  (2010)<br>Danceability: 0.65<br>Valence: 0.61<br>Energy: 0.92","<b>Airplanes (feat. Hayley Williams of Paramore)<\/b><br>B.o.B  (2010)<br>Danceability: 0.66<br>Valence: 0.38<br>Energy: 0.87","<b>OMG<\/b><br>Usher  (2010)<br>Danceability: 0.78<br>Valence: 0.33<br>Energy: 0.74","<b>Dynamite<\/b><br>Taio Cruz  (2010)<br>Danceability: 0.75<br>Valence: 0.82<br>Energy: 0.78","<b>Billionaire (feat. Bruno Mars)<\/b><br>Travie McCoy  (2010)<br>Danceability: 0.63<br>Valence: 0.66<br>Energy: 0.67","<b>Cooler Than Me - Single Mix<\/b><br>Mike Posner  (2010)<br>Danceability: 0.77<br>Valence: 0.62<br>Energy: 0.82","<b>I Like It<\/b><br>Enrique Iglesias  (2010)<br>Danceability: 0.66<br>Valence: 0.73<br>Energy: 0.94","<b>Find Your Love<\/b><br>Drake  (2010)<br>Danceability: 0.62<br>Valence: 0.82<br>Energy: 0.6","<b>Not Afraid<\/b><br>Eminem  (2010)<br>Danceability: 0.85<br>Valence: 0.68<br>Energy: 0.95","<b>Party Rock Anthem (LMFAO Feat. Lauren Bennett And Goonrock)<\/b><br>Ameritz Audio Karaoke  (2011)<br>Danceability: 0.82<br>Valence: 0.18<br>Energy: 0.43","<b>Give Me Everything (Tonight) (Pitbull feat. Ne-Yo, Afrojack & Nayer) inst<\/b><br>Idolmakers United  (2011)<br>Danceability: 0.73<br>Valence: 0.3<br>Energy: 0.39","<b>Rolling in the Deep<\/b><br>Adele  (2011)<br>Danceability: 0.73<br>Valence: 0.52<br>Energy: 0.76","<b>Last Friday Night (T.G.I.F.)<\/b><br>Katy Perry  (2011)<br>Danceability: 0.65<br>Valence: 0.76<br>Energy: 0.81","<b>Super Bass<\/b><br>Nicki Minaj  (2011)<br>Danceability: 0.73<br>Valence: 0.65<br>Energy: 0.87","<b>How To Love<\/b><br>Lil Wayne  (2011)<br>Danceability: 0.64<br>Valence: 0.29<br>Energy: 0.67","<b>The Edge Of Glory<\/b><br>Lady Gaga  (2011)<br>Danceability: 0.58<br>Valence: 0.4<br>Energy: 0.77","<b>Good Life<\/b><br>OneRepublic  (2011)<br>Danceability: 0.64<br>Valence: 0.64<br>Energy: 0.7","<b>Tonight Tonight<\/b><br>Hot Chelle Rae  (2011)<br>Danceability: 0.69<br>Valence: 0.81<br>Energy: 0.78","<b>E.T. - feat. Kanye West<\/b><br>Katy Perry  (2011)<br>Danceability: 0.62<br>Valence: 0.76<br>Energy: 0.87","<b>Call Me Maybe<\/b><br>Carly Rae Jepsen  (2012)<br>Danceability: 0.78<br>Valence: 0.63<br>Energy: 0.58","<b>Payphone<\/b><br>Maroon 5  (2012)<br>Danceability: 0.74<br>Valence: 0.5<br>Energy: 0.75","<b>Somebody That I Used To Know<\/b><br>Gotye  (2012)<br>Danceability: 0.86<br>Valence: 0.75<br>Energy: 0.52","<b>Wide Awake<\/b><br>Katy Perry  (2012)<br>Danceability: 0.51<br>Valence: 0.58<br>Energy: 0.68","<b>Lights - Single Version<\/b><br>Ellie Goulding  (2012)<br>Danceability: 0.68<br>Valence: 0.77<br>Energy: 0.79","<b>Where Have You Been<\/b><br>Rihanna  (2012)<br>Danceability: 0.71<br>Valence: 0.39<br>Energy: 0.85","<b>Whistle<\/b><br>Flo Rida  (2012)<br>Danceability: 0.75<br>Valence: 0.75<br>Energy: 0.93","<b>Titanium (feat. Sia)<\/b><br>David Guetta  (2012)<br>Danceability: 0.6<br>Valence: 0.23<br>Energy: 0.8","<b>We Are Young (feat. Janelle Monáe)<\/b><br>fun.  (2012)<br>Danceability: 0.38<br>Valence: 0.74<br>Energy: 0.64","<b>Starships<\/b><br>Nicki Minaj  (2012)<br>Danceability: 0.75<br>Valence: 0.69<br>Energy: 0.73","<b>Blurred Lines<\/b><br>Robin Thicke  (2013)<br>Danceability: 0.86<br>Valence: 0.84<br>Energy: 0.61","<b>Radioactive<\/b><br>Imagine Dragons  (2013)<br>Danceability: 0.48<br>Valence: 0.18<br>Energy: 0.8","<b>Get Lucky<\/b><br>Daft Punk  (2013)<br>Danceability: 0.81<br>Valence: 0.86<br>Energy: 0.79","<b>We Can't Stop<\/b><br>Miley Cyrus  (2013)<br>Danceability: 0.61<br>Valence: 0.48<br>Energy: 0.62","<b>Can't Hold Us - feat. Ray Dalton<\/b><br>Macklemore & Ryan Lewis  (2013)<br>Danceability: 0.64<br>Valence: 0.85<br>Energy: 0.92","<b>Cruise<\/b><br>Florida Georgia Line  (2013)<br>Danceability: 0.46<br>Valence: 0.88<br>Energy: 0.94","<b>Mirrors<\/b><br>Justin Timberlake  (2013)<br>Danceability: 0.57<br>Valence: 0.51<br>Energy: 0.51","<b>Treasure<\/b><br>Bruno Mars  (2013)<br>Danceability: 0.87<br>Valence: 0.94<br>Energy: 0.69","<b>Cups (Pitch Perfect’s “When I’m Gone”) - Pop Version<\/b><br>Anna Kendrick  (2013)<br>Danceability: 0.87<br>Valence: 0.77<br>Energy: 0.44","<b>Come & Get It<\/b><br>Selena Gomez  (2013)<br>Danceability: 0.52<br>Valence: 0.58<br>Energy: 0.8","<b>Fancy<\/b><br>Iggy Azalea  (2014)<br>Danceability: 0.91<br>Valence: 0.38<br>Energy: 0.71","<b>Rude<\/b><br>MAGIC!  (2014)<br>Danceability: 0.74<br>Valence: 0.9<br>Energy: 0.81","<b>Problem<\/b><br>Ariana Grande  (2014)<br>Danceability: 0.66<br>Valence: 0.62<br>Energy: 0.8","<b>Stay With Me<\/b><br>Sam Smith  (2014)<br>Danceability: 0.43<br>Valence: 0.14<br>Energy: 0.43","<b>Am I Wrong<\/b><br>Nico & Vinz  (2014)<br>Danceability: 0.72<br>Valence: 0.76<br>Energy: 0.68","<b>All of Me<\/b><br>John Legend  (2014)<br>Danceability: 0.42<br>Valence: 0.33<br>Energy: 0.26","<b>Wiggle (feat. Snoop Dogg)<\/b><br>Jason Derulo  (2014)<br>Danceability: 0.7<br>Valence: 0.72<br>Energy: 0.62","<b>Happy<\/b><br>Pharrell Williams  (2014)<br>Danceability: 0.65<br>Valence: 0.96<br>Energy: 0.76","<b>Summer<\/b><br>Calvin Harris  (2014)<br>Danceability: 0.6<br>Valence: 0.74<br>Energy: 0.86","<b>Turn Down for What<\/b><br>DJ Snake  (2014)<br>Danceability: 0.82<br>Valence: 0.08<br>Energy: 0.8","<b>Cheerleader - Felix Jaehn Remix Radio Edit<\/b><br>OMI  (2015)<br>Danceability: 0.78<br>Valence: 0.59<br>Energy: 0.68","<b>See You Again (feat. Charlie Puth)<\/b><br>Wiz Khalifa  (2015)<br>Danceability: 0.69<br>Valence: 0.28<br>Energy: 0.48","<b>Bad Blood<\/b><br>Taylor Swift  (2015)<br>Danceability: 0.65<br>Valence: 0.29<br>Energy: 0.79","<b>Can't Feel My Face<\/b><br>The Weeknd  (2015)<br>Danceability: 0.71<br>Valence: 0.59<br>Energy: 0.78","<b>Watch Me (Whip / Nae Nae)<\/b><br>Silentó  (2015)<br>Danceability: 0.82<br>Valence: 0.96<br>Energy: 0.77","<b>Trap Queen<\/b><br>Fetty Wap  (2015)<br>Danceability: 0.75<br>Valence: 0.82<br>Energy: 0.87","<b>Shut Up and Dance<\/b><br>WALK THE MOON  (2015)<br>Danceability: 0.58<br>Valence: 0.62<br>Energy: 0.87","<b>Lean On<\/b><br>Major Lazer  (2015)<br>Danceability: 0.72<br>Valence: 0.27<br>Energy: 0.81","<b>The Hills<\/b><br>The Weeknd  (2015)<br>Danceability: 0.54<br>Valence: 0.11<br>Energy: 0.56","<b>Fight Song<\/b><br>Rachel Platten  (2015)<br>Danceability: 0.56<br>Valence: 0.33<br>Energy: 0.71","<b>One Dance<\/b><br>Drake  (2016)<br>Danceability: 0.8<br>Valence: 0.39<br>Energy: 0.61","<b>CAN'T STOP THE FEELING! (Original Song from DreamWorks Animation's \"TROLLS\")<\/b><br>Justin Timberlake  (2016)<br>Danceability: 0.67<br>Valence: 0.7<br>Energy: 0.83","<b>Don't Let Me Down<\/b><br>The Chainsmokers  (2016)<br>Danceability: 0.53<br>Valence: 0.42<br>Energy: 0.87","<b>Cheap Thrills<\/b><br>Sia  (2016)<br>Danceability: 0.63<br>Valence: 0.73<br>Energy: 0.7","<b>This Is What You Came For<\/b><br>Calvin Harris  (2016)<br>Danceability: 0.63<br>Valence: 0.47<br>Energy: 0.93","<b>Panda<\/b><br>Desiigner  (2016)<br>Danceability: 0.72<br>Valence: 0.25<br>Energy: 0.75","<b>Needed Me<\/b><br>Rihanna  (2016)<br>Danceability: 0.69<br>Valence: 0.28<br>Energy: 0.32","<b>Ride<\/b><br>Twenty One Pilots  (2016)<br>Danceability: 0.64<br>Valence: 0.57<br>Energy: 0.71","<b>Work from Home<\/b><br>Fifth Harmony  (2016)<br>Danceability: 0.8<br>Valence: 0.59<br>Energy: 0.58","<b>Send My Love (To Your New Lover)<\/b><br>Adele  (2016)<br>Danceability: 0.69<br>Valence: 0.56<br>Energy: 0.52","<b>Despacito - Remix<\/b><br>Luis Fonsi  (2017)<br>Danceability: 0.63<br>Valence: 0.81<br>Energy: 0.81","<b>That's What I Like<\/b><br>Bruno Mars  (2017)<br>Danceability: 0.85<br>Valence: 0.86<br>Energy: 0.56","<b>I'm the One<\/b><br>DJ Khaled  (2017)<br>Danceability: 0.6<br>Valence: 0.82<br>Energy: 0.67","<b>Wild Thoughts<\/b><br>DJ Khaled  (2017)<br>Danceability: 0.61<br>Valence: 0.62<br>Energy: 0.68","<b>Shape of You<\/b><br>Ed Sheeran  (2017)<br>Danceability: 0.82<br>Valence: 0.93<br>Energy: 0.65","<b>Believer<\/b><br>Imagine Dragons  (2017)<br>Danceability: 0.78<br>Valence: 0.72<br>Energy: 0.78","<b>HUMBLE.<\/b><br>Kendrick Lamar  (2017)<br>Danceability: 0.9<br>Valence: 0.4<br>Energy: 0.61","<b>Unforgettable<\/b><br>French Montana  (2017)<br>Danceability: 0.73<br>Valence: 0.73<br>Energy: 0.77","<b>Body Like A Back Road<\/b><br>Sam Hunt  (2017)<br>Danceability: 0.73<br>Valence: 0.63<br>Energy: 0.47","<b>Congratulations<\/b><br>Post Malone  (2017)<br>Danceability: 0.63<br>Valence: 0.5<br>Energy: 0.81"],"type":"scatter","mode":"markers","marker":{"autocolorscale":false,"color":"rgba(204,121,167,1)","opacity":0.75,"size":7.559055118110237,"symbol":"circle","line":{"width":1.8897637795275593,"color":"rgba(204,121,167,1)"}},"hoveron":"points","name":"2010s","legendgroup":"2010s","showlegend":true,"xaxis":"x","yaxis":"y","hoverinfo":"text","frame":null}],"layout":{"margin":{"t":44.559568285595688,"r":8.634288086342881,"b":44.034869240348698,"l":57.849730178497303},"paper_bgcolor":"rgba(255,255,255,1)","font":{"color":"rgba(0,0,0,1)","family":"","size":17.268576172685766},"title":{"text":"Billboard Summer Hits: Danceability vs. Emotional Positivity<br><sup>Hover over a point to see track details · colored by decade · click legend to toggle<\/sup>","font":{"color":"rgba(0,0,0,1)","family":"","size":19.925280199252807},"x":0,"xref":"paper"},"xaxis":{"domain":[0,1],"automargin":true,"type":"linear","autorange":false,"range":[0.17885000000000001,1.0181499999999999],"tickmode":"array","ticktext":["0.2","0.4","0.6","0.8","1.0"],"tickvals":[0.20000000000000001,0.40000000000000002,0.60000000000000009,0.80000000000000004,1],"categoryorder":"array","categoryarray":["0.2","0.4","0.6","0.8","1.0"],"nticks":null,"ticks":"","tickcolor":null,"ticklen":4.3171440431714414,"tickwidth":0,"showticklabels":true,"tickfont":{"color":"rgba(77,77,77,1)","family":"","size":13.814860938148611},"tickangle":-0,"showline":false,"linecolor":null,"linewidth":0,"showgrid":true,"gridcolor":"rgba(235,235,235,1)","gridwidth":0.78493528057662576,"zeroline":false,"anchor":"y","title":{"text":"Danceability","font":{"color":"rgba(0,0,0,1)","family":"","size":17.268576172685766}},"hoverformat":".2f"},"yaxis":{"domain":[0,1],"automargin":true,"type":"linear","autorange":false,"range":[0.023675000000000002,1.031825],"tickmode":"array","ticktext":["0.25","0.50","0.75","1.00"],"tickvals":[0.25,0.5,0.75,1],"categoryorder":"array","categoryarray":["0.25","0.50","0.75","1.00"],"nticks":null,"ticks":"","tickcolor":null,"ticklen":4.3171440431714405,"tickwidth":0,"showticklabels":true,"tickfont":{"color":"rgba(77,77,77,1)","family":"","size":13.814860938148611},"tickangle":-0,"showline":false,"linecolor":null,"linewidth":0,"showgrid":true,"gridcolor":"rgba(235,235,235,1)","gridwidth":0.78493528057662576,"zeroline":false,"anchor":"x","title":{"text":"Valence  (0 = negative / sad,  1 = positive / happy)","font":{"color":"rgba(0,0,0,1)","family":"","size":17.268576172685766}},"hoverformat":".2f"},"shapes":[],"showlegend":true,"legend":{"bgcolor":null,"bordercolor":null,"borderwidth":0,"font":{"color":"rgba(0,0,0,1)","family":"","size":13.814860938148611},"orientation":"h","x":0.5,"y":-0.14999999999999999,"xanchor":"center","yanchor":"top","title":{"text":"Decade","font":{"color":"rgba(0,0,0,1)","family":"","size":17.268576172685766}}},"hovermode":"closest","barmode":"relative"},"config":{"doubleClick":"reset","modeBarButtonsToAdd":["hoverclosest","hovercompare"],"showSendToCloud":false},"source":"A","attrs":{"36fcfff70d1":{"x":{},"y":{},"colour":{},"text":{},"type":"scatter"}},"cur_data":"36fcfff70d1","visdat":{"36fcfff70d1":["function (y) ","x"]},"highlight":{"on":"plotly_click","persistent":false,"dynamic":false,"selectize":false,"opacityDim":0.20000000000000001,"selected":{"opacity":1},"debounce":0},"shinyEvents":["plotly_hover","plotly_click","plotly_selected","plotly_relayout","plotly_brushed","plotly_brushing","plotly_clickannotation","plotly_doubleclick","plotly_deselect","plotly_afterplot","plotly_sunburstclick"],"base_url":"https://plot.ly"},"evals":[],"jsHooks":[]}</script>
```

**Takeaway:** Earlier decades (1958–1979) cluster toward the upper-right — highly danceable and emotionally upbeat. From the 1990s onward, valence spreads widely, reflecting that popular summer music increasingly embraces melancholy and introspective tones alongside feel-good fare.

---

### Plot 4 — NBA Championship Cities (1980–2018) *(Spatial)*


``` r
us_map <- st_read("../data/us_states.shp", quiet = TRUE)

champs_geo <- team_cities |>
  inner_join(champs_count, by = "Team") |>
  filter(City != "Toronto") |>
  st_as_sf(coords = c("lon", "lat"), crs = 4326)

ggplot() +
  geom_sf(data = us_map, fill = "#ecf0f1", color = "white", linewidth = 0.4) +
  geom_sf(
    data  = champs_geo,
    aes(size = championships, color = championships),
    alpha = 0.85
  ) +
  geom_sf_text(
    data    = champs_geo |> filter(championships >= 3),
    aes(label = paste0(Team, "\n(", championships, ")")),
    size    = 2.7,
    nudge_y = 1.4,
    fontface = "bold",
    color   = "#2C3E50"
  ) +
  scale_size_continuous(range = c(3, 14), name = "Championships", breaks = c(1, 3, 5, 10)) +
  # viridis for colorblind safety on the continuous championship scale
  scale_color_viridis_c(name = "Championships", option = "plasma",
                        breaks = c(1, 3, 5, 10)) +
  coord_sf(xlim = c(-125, -66), ylim = c(24, 50), expand = FALSE) +
  labs(
    title    = "NBA Championships by City (1980–2018)",
    subtitle = "Point size and color both encode number of championships won.",
    caption  = "Source: NBAchampionsdata.csv · US state boundaries: R maps package"
  ) +
  guides(size = guide_legend(override.aes = list(alpha = 0.85))) +
  project_theme +
  theme(
    panel.background = element_rect(fill = "#d6eaf8", color = NA),
    axis.text        = element_blank(),
    axis.ticks       = element_blank(),
    panel.grid       = element_blank()
  )
```

<img src="C:\Users\Will Bowden\Documents\Will-Bowden_dataviz_final-project\project-02\Bowden_project_02_files/figure-html/nba-map-1.png" alt="U.S. map showing NBA championship cities from 1980 to 2018. Points are sized and colored by number of championships won, using a blue-to-red gradient. Los Angeles (Lakers) has the largest point at 10 championships. Chicago Bulls and San Antonio Spurs are also prominently labeled." style="display: block; margin: auto;" />

**Takeaway:** Los Angeles (Lakers, 10 championships in this span) is the dominant franchise. San Antonio (Spurs, 5) and Chicago (Bulls, 6) are the other major dynasties. Championships are geographically concentrated along the coasts and Sun Belt.

---

### Plot 5 — Predicting NBA Championship Points: Linear Model

We model points scored per game as a function of key box score statistics using ordinary least squares regression.

$$\widehat{\text{PTS}} = \beta_0 + \beta_1\,\text{FGP} + \beta_2\,\text{FTP} + \beta_3\,\text{TRB} + \beta_4\,\text{AST} + \beta_5\,\text{TOV}$$


``` r
pts_model <- lm(PTS ~ FGP + FTP + TRB + AST + TOV, data = nba)

model_summary <- glance(pts_model) |>
  select(r.squared, adj.r.squared, sigma, statistic, p.value) |>
  mutate(across(where(is.numeric), ~round(., 3)))

knitr::kable(model_summary, caption = "Model Summary Statistics")
```



Table: Model Summary Statistics

| r.squared| adj.r.squared| sigma| statistic| p.value|
|---------:|-------------:|-----:|---------:|-------:|
|     0.689|         0.682| 7.509|    94.907|       0|


``` r
model_tidy <- tidy(pts_model, conf.int = TRUE) |>
  filter(term != "(Intercept)") |>
  mutate(
    term = recode(term,
      FGP = "Field Goal %",
      FTP = "Free Throw %",
      TRB = "Total Rebounds",
      AST = "Assists",
      TOV = "Turnovers"
    ),
    significant = p.value < 0.05,
    term = fct_reorder(term, estimate)
  )

ggplot(model_tidy, aes(x = estimate, y = term, color = significant)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray60", linewidth = 0.9) +
  geom_errorbar(
    aes(xmin = conf.low, xmax = conf.high),
    width = 0.22, linewidth = 0.9
  ) +
  geom_point(size = 4) +
  scale_color_manual(
    values = c("TRUE" = "#E69F00", "FALSE" = "#56B4E9"),
    labels = c("TRUE" = "p < 0.05  (significant)", "FALSE" = "p \u2265 0.05"),
    name   = NULL
  ) +
  labs(
    title    = "Predictors of Points Scored in NBA Finals Games",
    subtitle = "OLS regression coefficients with 95% confidence intervals",
    x = "Coefficient Estimate  (change in points per unit increase)",
    y = NULL,
    caption  = "Source: NBAchampionsdata.csv"
  ) +
  project_theme +
  theme(legend.position = "top")
```

<img src="C:\Users\Will Bowden\Documents\Will-Bowden_dataviz_final-project\project-02\Bowden_project_02_files/figure-html/nba-model-plot-1.png" alt="Coefficient plot showing the estimated effect of five predictors on NBA championship team points scored per game. Field goal percentage has the largest positive coefficient. Turnovers have a negative coefficient. Color distinguishes statistically significant predictors (orange) from non-significant ones (gray). Error bars show 95 percent confidence intervals." style="display: block; margin: auto;" />

**Takeaway:** Field goal percentage is by far the strongest predictor — a 10-percentage-point improvement in FG% corresponds to about 5–6 more points per game. Turnovers carry a clear negative coefficient. The model explains approximately 69% of game-to-game scoring variance (R² = 0.689).

---

## Discussion

### a. Original Plans and Data Preparation

My initial concept was a simple time-series line for births, a faceted bar chart of Billboard audio features per decade, and a win/loss bar chart for NBA teams. After exploring the data, I shifted to more informative forms:

- A **tile heatmap** for births captures both year-over-year decline and seasonal rhythm in a single view.
- An **interactive scatter** for Billboard lets viewers explore 600+ individual songs across 60 years without overplotting.
- A **spatial map** for NBA makes geographic concentration of championships immediately legible.

Key preparation steps included ordering `day_of_week` as a proper factor, deriving `decade` from `year`, and fixing NBA team name inconsistencies (`'Heat'` stray quotes, `Warriorrs` typo).

### b. Stories, Difficulties, and Further Exploration

The three datasets together tell a story about American culture: demographic impact of economic shocks (birth decline post-2008), emotional diversification of pop music since the 1990s, and the persistent geographic concentration of sporting success.

Further approaches worth pursuing:

- **Animated map** of NBA championships year by year, showing the shifting geographic center of gravity.
- **Audio feature radar/spider charts** per decade for a richer Billboard comparison.
- **Interaction analysis** for births: is the weekday/weekend gap stable over 15 years, or has it widened as C-section rates changed?

### c. Data Visualization Principles Applied

- **Accessibility:** All figures use the Okabe-Ito colorblind-safe palette. Continuous scales use `viridis` (plasma option) for the map. Alt text is provided for every figure via `fig.alt`.
- **Pre-attentive encoding:** Color saturation in the heatmap guides the eye to extremes immediately.
- **Annotation over legend lookup:** The heatmap, day-of-week bar chart, and NBA map carry inline annotations so the viewer doesn't cross-reference a legend for the key insight.
- **Interactivity with purpose:** The plotly chart is interactive because individual song identity is meaningful — hover tooltips make 60 years of track data explorable.
- **Honest uncertainty:** The coefficient plot distinguishes statistically significant predictors from non-significant ones and displays full confidence intervals.

---

## References

- FiveThirtyEight. "US Births Data (2000–2014)." `us_births_00_14.csv`.
- Spotify / Billboard. "All Billboard Summer Hits." `all_billboard_summer_hits.csv`.
- "NBA Champions Data (1980–2018)." `NBAchampionsdata.csv`.
- U.S. state boundaries: R `maps` package, converted via `sf`.
