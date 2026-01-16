---
title: "draft"
output: html_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = FALSE, warning = FALSE, message = FALSE, fig.align = "center")
library(tidyverse)
library(knitr)
```

# Introduction

Power plays are mixed doubles curling's nuclear option—a single-use strategic advantage that teams save for critical moments. But here's the paradox: teams often deploy power plays precisely when pressure is highest. When the game is close and the end is late, do teams rise to the occasion or crumble under pressure?

Despite the strategic importance of power plays, no systematic study has examined how pressure affects their execution. We address this gap by analyzing 313 power play ends across four major international competitions. We introduce the **Power Play Clutch Score (PPCS)**, a novel metric quantifying whether teams overperform or underperform when stakes are highest.

Our analysis reveals a surprising finding: contrary to conventional wisdom, teams actually perform *better* on power plays under pressure—but not equally. Some nations thrive dramatically while others collapse, insights directly actionable for Olympic preparation.

# Data and Methods

## Data

We analyzed shot-by-shot data from mixed doubles curling competitions provided by the CSAS Data Challenge. The dataset includes 5,274 ends across four international competitions, of which 313 (5.9%) were power play ends. After filtering for teams with sufficient sample sizes ($\geq 5$ power plays), 21 national teams remained for analysis.

## Pressure Definitions

We operationalized pressure using two complementary approaches:

**Definition 1: Game Situation.** High pressure required two or fewer ends remaining AND score differential within two points:

$$
\text{Pressure}_{\text{situation}} = 
\begin{cases} 
\text{high} & \text{if } \text{ends\_remaining} \leq 2 \text{ AND } |\text{score\_diff}| \leq 2 \\
\text{medium} & \text{if } \text{ends\_remaining} \leq 3 \text{ OR } |\text{score\_diff}| \leq 3 \\
\text{low} & \text{otherwise}
\end{cases}
$$

**Definition 2: Win Probability Leverage.** We built a logistic regression model predicting game outcomes from score differential, hammer possession, and ends remaining. Ends in the top quartile of potential win probability swing were classified as high leverage.

We combined these into a final four-level variable: ends flagged high by both definitions became "very high" pressure ($n=85$); high by either became "high" ($n=50$); medium by either became "medium" ($n=127$); else "low" ($n=51$).

## Power Play Clutch Score (PPCS)

We introduce the Power Play Clutch Score to quantify team performance under pressure:

$$
\text{PPCS} = \bar{X}_{\text{high pressure}} - \bar{X}_{\text{low pressure}}
$$

where $\bar{X}$ represents mean points scored on power plays.

**Interpretation:**

- $\text{PPCS} > 0$: Team performs better under pressure ("clutch")
- $\text{PPCS} < 0$: Team performs worse under pressure ("chokes")
- $\text{PPCS} \approx 0$: Consistent performance regardless of pressure

## Win Probability Added (WPA)

To measure actual game impact beyond raw scoring, we calculated Win Probability Added for each power play:

$$
\text{WPA} = P(\text{Win})_{\text{after}} - P(\text{Win})_{\text{before}}
$$

This allows us to distinguish between teams that score under pressure versus teams that actually change game outcomes.

# Results

## Finding 1: Teams Rise to the Occasion Under Pressure

Contrary to the "choking under pressure" hypothesis, teams actually scored *more* points as pressure increased (Figure 1). Under low pressure, teams averaged 1.43 points per power play with a 41.2% success rate ($\geq 2$ points). This improved to 1.78 points (58.0% success) under high pressure and 1.93 points (52.9% success) under very high pressure—a **35% increase** in scoring from low to very high pressure situations.
```{r fig1, fig.cap="Average points scored on power plays increases with pressure level. Error bars show standard error.", out.width="85%"}
knitr::include_graphics("output/figures/fig1_scoring_by_pressure.png")
```

## Finding 2: Teams Vary Dramatically in Clutch Performance

While the overall trend shows improvement under pressure, PPCS revealed substantial variation across the 21 teams analyzed (Figure 2):

**Most Clutch Teams:**

1. New Zealand (PPCS = +2.67)
2. Italy (PPCS = +1.75)
3. Canada (PPCS = +1.33)

**Teams That Struggle Under Pressure:**

1. Estonia (PPCS = -1.50)
2. France (PPCS = -1.00)
3. Scotland (PPCS = -0.62)

Notably, 12 of 21 teams (57%) performed better under pressure, while 8 teams (38%) performed worse.
```{r fig2, fig.cap="PPCS rankings reveal wide variation in clutch performance across national teams.", out.width="90%"}
knitr::include_graphics("output/figures/fig3_ppcs_rankings.png")
```

## Finding 3: The False Clutch Problem

The correlation between PPCS and average WPA was surprisingly weak ($r = 0.17$), revealing that high scorers are not always game-changers (Figure 3).

**True Clutch Teams** (High PPCS AND High WPA): Canada, New Zealand, Korea, Denmark

**False Clutch Teams** (High PPCS but Negative WPA): Italy, Turkey, China, Spain

This distinction is crucial: Italy ranks 2nd in PPCS but has *negative* WPA, meaning their high-pressure scoring doesn't translate to winning more games.
```{r fig3, fig.cap="PPCS vs WPA reveals 'false clutch' teams who score more but don't win more under pressure.", out.width="90%"}
knitr::include_graphics("output/figures/fig6_ppcs_vs_wpa.png")
```

## Finding 4: Pressure Response Curves

Rather than binary "clutch vs choke" labels, we modeled how each team's performance changes across the full pressure spectrum (Figure 4). This reveals distinct patterns:

- **Risers** (NZL, ITA): Performance increases steadily with pressure
- **Folders** (EST, GBR, USA): Performance declines as pressure mounts
- **Flat-liners**: Consistent regardless of pressure
```{r fig4, fig.cap="Pressure response curves show distinct team personalities under increasing pressure.", out.width="90%"}
knitr::include_graphics("output/figures/fig5_pressure_curves.png")
```

## Finding 5: Pressure Predicts Success

Our logistic regression model confirmed that pressure level significantly predicts power play success (Figure 5). High and very high pressure situations were associated with increased odds of scoring 2+ points compared to low pressure situations.
```{r fig5, fig.cap="Model coefficients show pressure significantly increases power play success odds.", out.width="80%"}
knitr::include_graphics("output/figures/fig4_model_coefficients.png")
```

# Discussion

## The Surprising Truth: Clutch Performance Is Real

Our most striking finding contradicts the common assumption that pressure hurts performance. In mixed doubles curling power plays, teams score 35% more when stakes are highest. Several explanations merit consideration:

1. **Strategic deployment:** Teams may save power plays for situations where they have positional advantages
2. **Heightened focus:** Elite athletes may perform better when pressure triggers optimal arousal states
3. **Opponent effects:** Opposing teams may also feel pressure, leading to defensive mistakes

## The False Clutch Insight

Perhaps our most actionable finding is the weak correlation between PPCS and WPA ($r = 0.17$). This reveals that:

- High scoring under pressure $\neq$ High impact under pressure
- Context matters: *when* you score affects game outcomes
- Coaches should look beyond raw scoring numbers

## Implications for Olympic Coaches

1. **Don't fear pressure power plays.** The data suggests power plays are *more* effective in high-stakes moments.

2. **Scout for true clutch.** Canada and New Zealand are genuinely dangerous under pressure. Italy's high PPCS is misleading—they score but don't convert to wins.

3. **Exploit the folders.** Estonia, Great Britain, and USA show declining performance curves. Apply pressure and they may crack.

## Limitations

- Sample size: 313 power play ends limits team-level precision
- Cannot account for opponent quality or ice conditions
- Predictive model has modest accuracy (59.1%)

# Conclusion

Power plays are meant to be game-changers, and our analysis confirms they are—especially under pressure. Contrary to choking narratives, teams score 35% more on power plays in high-pressure situations. Yet this rising tide does not lift all boats equally.

The Power Play Clutch Score identifies which teams thrive (New Zealand, Canada) versus struggle (Estonia, France) under pressure. More importantly, the distinction between PPCS and WPA reveals "false clutch" teams like Italy who score more but don't win more.

For coaches preparing for the 2026 Olympics, the message is clear: knowing which opponents are truly clutch—and which are not—could make the difference between gold and going home empty-handed.
Step 4: Save the File
