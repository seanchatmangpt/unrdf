# Full-Factorial DOE

**Duration:** 2 hours
**Objective:** Implement and analyze a full-factorial experiment to capture all main and interaction effects.

## Learning Objectives
- Design full-factorial experiments for multiple factors.  
- Analyze results using ANOVA and regression.  
- Interpret interaction plots and effect estimates.

## Overview
A full-factorial DOE tests all possible combinations of factor levels. It provides complete information on main effects and interactions but may require many runs when factors are numerous.

## Key Steps
1. **List Factors & Levels:** Define each factor’s levels (e.g., Factor A: Low/High, Factor B: L/H).  
2. **Generate Runs:** Create all combinations (2^k runs for k factors).  
3. **Randomize & Block:** Randomize run order; block if needed.  
4. **Perform Experiments:** Execute runs and record responses.  
5. **Analyze:** Use ANOVA to identify significant effects and interactions.

## Example
2³ factorial for three factors yields 8 runs. ANOVA identifies Factor A and A×B interaction as significant.

## Exercise
1. Conduct a 2^3 DOE on sample data.  
2. Generate effect estimates and Pareto chart.  
3. Discuss which interactions warrant further study.

## Summary
Full-factorial DOE comprehensively explores factor effects but scales exponentially; use when factor count is small or resources allow.