# Data Visualization and Reproducible Research

> **Will Bowden** — wbowden@floridapoly.edu  
> Data Visualization and Reproducible Research · Florida Polytechnic University

The following is a sample of work produced during the *"Data Visualization and Reproducible Research"* course. Each project applies principles of exploratory data analysis, storytelling through visualization, and reproducible research using R and R Markdown.

---

## Project 01 — 2017 Boston Marathon Finisher Analysis

In `project-01/` I explore finish-time patterns across gender, age groups, and countries of origin for all 26,410 finishers of the 2017 Boston Marathon. The project uses density plots, box plots, a horizontal stacked bar chart, and an interactive density visualization built with `plotly`.

**Key findings:** Male and female finish-time distributions differ by ~30 minutes at the median; peak performance occurs in the 25–44 age bracket; the vast majority of runners slow in the second half, with female runners showing a tighter pacing pattern than males.

**Sample visualization:**

![Finish time density plot](figures/project01_density.png)

*(See `project-01/Bowden_project_01.html` for the interactive version)*

---

## Project 02 — Births, Beats & Basketball

In `project-02/` I explore three unrelated datasets to surface patterns across American demography, pop culture, and sports:

- **U.S. Births (2000–2014):** Monthly heatmap and day-of-week bar chart reveal the holiday conception peak and hospital scheduling effects.
- **Billboard Summer Hits (1958–2017):** An interactive `plotly` scatter plot shows how summer pop hits shifted from uniformly upbeat to emotionally diverse over 60 years.
- **NBA Champions (1980–2018):** A geographic map and coefficient plot show championship geographic concentration and the statistical predictors of scoring.

**Sample visualization:**

![Billboard summer hits scatter](figures/project02_billboard.png)

*(See `project-02/Bowden_project_02.html` for the full interactive version)*

---

## Project 03 — Distribution Visualization & Concrete Strength EDA

In `project-03/` I work through two tasks:

**Part 1 (TPA Weather):** Five density plots using 2022 Tampa International Airport weather data — a faceted histogram, a rectangular-kernel density curve, faceted density plots, a ggridges ridge plot (viridis plasma palette with quantile lines), and a log-scaled precipitation histogram.

**Part 2 (Concrete Strength):** Exploratory analysis of the UCI concrete compressive strength dataset — distribution plots for strength and water content, a violin + box plot of strength by curing age, and an interactive `plotly` scatter plot linking cement content to strength.

**Sample visualization:**

![Concrete strength by age](figures/project03_concrete_age.png)

*(See `project-03/Bowden_project_03.html` for the interactive concrete scatter plot)*

---

## Where the Three Global Requirements Are Addressed

| Requirement | Location |
|---|---|
| **Interactive chart** | Project 01: interactive plotly density (finish times by gender); Project 02: interactive plotly scatter (Billboard danceability vs. valence); Project 03: interactive plotly scatter (cement vs. compressive strength) |
| **Accessibility** | All figures in all three projects use the Okabe-Ito colorblind-safe palette (`#E69F00`, `#0072B2`) or viridis/plasma. Every `{r}` chunk includes a `fig.alt` description. Color is never the sole encoding channel. |
| **Chart redesign (before/after)** | Project 03, *Redesign* section: a rainbow 3D-style pie chart of NBA championships is redesigned as an ordered, labeled horizontal bar chart with a viridis palette. The explanation covers truncated perception, unsafe palettes, and lack of ordering. |

---

## Moving Forward

This course significantly expanded my toolkit for data storytelling. The shift I found most valuable was learning to match geometry to data type — density plots for large continuous distributions, ridge plots for many-group comparisons, spatial maps for geographic concentration. I plan to continue exploring:

- **Animation** (`gganimate`) for showing temporal evolution of patterns.
- **Quarto** for a more modern, flexible reproducible document workflow.
- **Network visualization** for text co-occurrence and topic modeling.
- **Dashboard design** with `Shiny` and `bslib` for interactive, audience-facing reporting.
