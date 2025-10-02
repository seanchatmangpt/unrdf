# Monte Carlo Simulation

**Duration:** 2 hours
**Objective:** Use Monte Carlo simulation to model process variability and assess risk.

## Learning Objectives
- Build a simulation model using random sampling.  
- Analyze output distributions and confidence levels.  
- Apply simulation to design risk assessment.

## Overview
Monte Carlo simulation uses repeated random sampling to model the impact of variability on system performance. It estimates probabilities of outcomes and helps identify key sources of risk.

## Key Steps
1. **Define Variables:** Identify input distributions (normal, uniform, triangular).  
2. **Setup Simulation:** Create model combining variables (e.g., sum, product).  
3. **Run Trials:** Execute thousands of iterations, record outputs.  
4. **Analyze Results:** Plot histograms, compute percentiles and probabilities of failure.  
5. **Decision Making:** Adjust inputs or design to meet target performance.

## Example
Model total project duration with three phases (each normal distribution). Run 10,000 simulations to find 90th percentile completion time.

## Exercise
1. Simulate assembly yield based on component tolerance distributions.  
2. Determine probability that yield < 95%.  
3. Propose design changes to improve yield.

## Summary
Monte Carlo simulation provides quantitative insight into risk and variability, supporting robust design and informed decision-making.