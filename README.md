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