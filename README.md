# CSAS Curling Data Challenge 2026

**Team:** Sarayu & Armaan

**Power Play Count:** 598

---
## Update: Day 3 (Exploration & Modeling)

### 1. Power Play Exploration (Day 3S)
**Analyst:** Sarayu | **Dataset:** 2,985 stones (597 Power Play ends)

**Data Integrity & Cleaning**
- **Anomaly Detected:** Match `0_10_1` (End 7) was flagged as a Power Play but the team did not have the hammer.
- **Action:** Removed this end (5 stones) as a data entry error.
- **Validation:** Remaining dataset has 100% compliance with hammer rules.

**Strategic Insights**
- **Timing:** Usage is heavily back-loaded, peaking in **Ends 6 and 7**. Teams treat the Power Play as a "setup" mechanic for the final end.
- **Score Context:** Usage is skewed towards **losing teams (down 1-2)** or **tied games**.
- **Defensive Behavior:** Teams leading by 2+ points rarely use the Power Play, preferring open defense.

### 2. Win Probability Model v1 (Day 3A)
**Analyst:** Armaan | **Algorithm:** Logistic Regression

**Objective**
To establish a baseline for "expected performance," we trained a model to predict a team's probability of winning ($P(Win)$) based on the game state.

**Model Specification**
- **Outcome:** Win (1) or Loss (0)
- **Predictors:** Score Differential, Hammer Possession, Ends Remaining.

**Validation Results ("The Smell Test")**
- **Hammer Advantage:** Coefficient is **+1.929** ($p < 2e^{-16}$), confirming that having the last shot is a massive advantage.
- **Score Impact:** Coefficient is **+1.283**, quantifying the value of every point on the board.
- **Reality Check:** In a **Tied Game, End 8 (Last End) with Hammer**, the model predicts a **70.7% Win Probability**. This aligns perfectly with professional curling analytics (typically 65–75%).

**Goodness of Fit**
- Residual Deviance (5019) is significantly lower than Null Deviance (7219), indicating the model explains a large portion of the variance.

## 📊 Key Findings (Preliminary)

### 1. The "Do-or-Die" Strategy Shift (Day 8 Analysis)
We analyzed the first 3 shots of an end to see if teams play more conservatively (throwing guards) when under pressure.
* **The Trend:** It follows a "U-shape."
    * **Low Pressure:** Teams experiment more (~5.4% Guards).
    * **High Pressure:** Teams play "clean," avoiding clutter to minimize risks (~4.4% Guards).
    * **Very High Pressure:** Teams suddenly switch to aggressive "junk" play, throwing **significantly more guards (~6.9%)**.
* **Insight:** In desperate situations, teams try to clutter the house to force opponent errors, rather than playing it safe.

### 2. The "Clutch" Factor: Execution Improves Under Pressure (Day 9 Analysis)
We tested if shot execution (scored 0-4) drops due to nerves in high-stakes moments.
* **Result:** Performance actually **improves** as pressure rises.
    * **Low Pressure Avg Score:** 2.96 / 4.0
    * **High Pressure Avg Score:** 3.13 / 4.0
    * **Perfect Shots:** In "Very High" pressure scenarios, **58.5%** of shots are executed perfectly (4/4).
* **Statistical Significance:** An ANOVA test confirmed this difference is significant ($p < 0.001$). Professional curlers lock in rather than choke when the game is on the line.