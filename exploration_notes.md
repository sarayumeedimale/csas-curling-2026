# Exploration Notes: Power Play Patterns (Day 3S)
**Dataset:** 2,985 stones (597 Power Play ends)

## 1. Data Integrity & Cleaning
- **Anomaly Detected:** Match `0_10_1` (End 7) was flagged as a Power Play but the team did not have the hammer.
- **Action:** Removed this end (5 stones) as a data entry error.
- **Validation:** Remaining dataset has 100% compliance with hammer rules (Teams only call Power Play when `has_hammer == TRUE`).

## 2. Timing (When is the Power Play used?)
*Referencing Plot: Power Play Usage by End*
- **Pattern:** Usage is heavily back-loaded. Teams almost never use the Power Play in the first half of the game (Ends 1-4).
- **Peak Usage:** The most common time to call a Power Play is **Ends 6 and 7** (over 200 occurrences each).
- **Strategic Insight:** Teams treat the Power Play as a "setup" mechanic for the final end. Usage drops in End 8, likely because teams prefer to use it earlier (Ends 6/7) to secure a lead or tie the game *before* the final end pressure.

## 3. Score Context (At what score is it used?)
*Referencing Plot: Score Differential During Power Plays*
- **Distribution:** The histogram is skewed slightly negative (left-leaning).
- **Most Common Scenarios:**
  1. **Down by 1 or 2 points** (trying to catch up).
  2. **Tied Game (0)** (trying to break the deadlock).
- **Defensive Behavior:** There is a sharp drop-off in usage when teams are **leading by 2+ points**.
- **Insight:** The Power Play is primarily a **"Comeback Mechanism"**. Teams who are comfortably winning prefer to play an open game (standard defense) rather than risking the cluttered house of a Power Play.

## 4. Summary for Modeling
- When building our win probability model, we must account for the fact that Power Plays are correlated with **losing positions** and **late-game situations**.
- A "successful" Power Play for a losing team might just be scoring 2 points to tie, whereas a winning team might be happy just blanking the end.