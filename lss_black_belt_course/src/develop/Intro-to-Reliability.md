# Introduction to Reliability

**Duration:** 1.5 hours
**Objective:** Learn basic reliability engineering concepts to design robust products.

## Learning Objectives
- Define reliability, failure rate, and MTBF.  
- Use life data analysis to estimate reliability.  
- Apply reliability metrics in design decisions.

## Overview
Reliability engineering ensures products perform as intended over their lifecycle. Key metrics include failure rate (λ), Mean Time Between Failures (MTBF), and survival probability.

## Key Steps
1. **Collect Life Data:** Gather time-to-failure data from tests or field usage.  
2. **Fit Distribution:** Model data using exponential, Weibull, or log-normal distributions.  
3. **Estimate Metrics:** Compute MTBF and reliability at given time intervals (R(t)).  
4. **Design Adjustments:** Improve reliability via redundancy, derating, or enhanced materials.

## Example
Field data shows exponential failure rate λ=0.0001 per hour. MTBF=1/λ=10,000 hours. Reliability at 1000 hours: R(1000)=e^(−0.0001×1000)=0.90.

## Exercise
1. Given sample failure data, fit an exponential distribution and calculate MTBF.  
2. Determine reliability at 500 and 2000 hours.

## Summary
Reliability metrics quantify product lifetime performance, guiding design choices to meet customer expectations and warranty targets.