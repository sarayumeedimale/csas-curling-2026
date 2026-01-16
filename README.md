# The Anxious Sniper: Quantifying Strategic Regression in High-Stakes Mixed Doubles Curling
### 2026 CSAS Data Challenge

## Project Overview
This repository contains the code and methodology for our analysis of Power Play execution under pressure in Mixed Doubles Curling. 

Our research introduces the **Power Play Clutch Score (PPCS)** and the **Shot 2 Protection Strategy**. We analyze over 26,000 stone observations to demonstrate the "Anxious Sniper" effect: the phenomenon where elite teams maintain high physical execution metrics under pressure but regress strategically, thus opting for conservative shot selections that lower their win probability.


## Data Setup Instructions

Before running any scripts, you must configure the data directory locally:

1.  Create a folder named `data` in the root directory of this project.
2.  Inside that folder, create a subfolder named `raw`.
3.  Place the official CSAS Data Challenge CSV files into `data/raw/`.

**Required File Structure:**
```text
project_root/
├── data/
│   └── raw/
│       ├── Stones.csv
│       ├── Games.csv
│       ├── Competitions.csv
│       ├── Ends.csv
│       ├── Teams.csv
│       └── Competitors.csv
├── scripts/
│   └── ... (01 through 07)
└── README.md
```


## Dependencies
All analysis was performed using **R**. Please ensure you have the following packages installed before executing the pipeline:

```r
install.packages(c("tidyverse", "broom", "pROC", "ggplot2", "dplyr"))
```

*   **tidyverse** (Data manipulation and visualization)
*   **broom** (Tidy model output)
*   **pROC** (ROC/AUC calculation for the Win Probability model)


## Execution Guide
The analysis pipeline is modular. Please run the scripts in the numerical order listed below to ensure dependencies (cleaned data, model objects) are generated correctly.

### 1. Data Preparation
*   **`01_data_cleaning.R`**
    *   **Function:** Imports raw Curlit data, handles sentinel values (e.g., converting coordinate `4095` to `NA`), and merges Game/End/Stone tables.
    *   **Output:** Creates the master dataset and filters specifically for **Power Play ends**.

### 2. Metric Definition
*   **`02_win_probability.R`**
    *   **Function:** Trains a logistic regression model on historical game states to calculate Win Probability ($P_{win}$).
    *   **Output:** Derives the **Leverage Index** (the potential swing in $P_{win}$ based on the current end's outcome).
*   **`03_pressure_metrics.R`**
    *   **Function:** Calculates the **Combined Pressure Index**.
    *   **Methodology:** Integrates Score Differential, Ends Remaining, and the Leverage Index to classify ends into "Low," "Medium," "High," and "Very High" pressure categories.

### 3. Core Analysis
*   **`04_power_play_analysis.R`**
    *   **Function:** Performs the primary statistical analysis.
    *   **Output:** Calculates the **Power Play Clutch Score (PPCS)** for the global dataset and identifies the divergence between Execution Quality (Points) and Scoring Output.

### 4. Team Profiling
*   **`05_team_profiles.R`**
    *   **Function:** Aggregates data by National Olympic Committee (NOC).
    *   **Output:** Generates team-specific efficiency rankings, identifying "Antifragile" teams (e.g., ITA) vs. "Regressive" teams (e.g., USA).

### 5. Predictive Modeling
*   **`06_predictive_model.R`**
    *   **Function:** Trains a multivariate logistic regression model to predict Power Play success ($\ge 2$ points).
    *   **Output:** Generates Odds Ratios for shot selection, identifying the **"Shot 2 Protection Strategy"** as a statistically significant predictor of success.

### 6. Visualization
*   **`07_visualizations.R`**
    *   **Function:** Generates the publication-ready figures used in the final report.
    *   **Output:** Saves `.png` files to the `output/figures/` directory (e.g., The Clutch Paradox Chart, Team Rankings Lollipop Chart).


## Output
All intermediate data files and final figures will be saved to the `output/` directory created by the scripts.

*   **`output/figures/`**: Contains the visual evidence for the "Anxious Sniper" theory.
*   **`output/models/`**: Contains the saved `.rds` model objects.
```