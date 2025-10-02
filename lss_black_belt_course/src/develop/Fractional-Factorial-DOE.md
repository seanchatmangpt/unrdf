# Fractional Factorial DOE

**Duration:** 2 hours
**Objective:** Use fractional factorial designs to study many factors with fewer runs.

## Learning Objectives
- Understand aliasing and resolution in fractional designs.  
- Plan a 2^(k-p) fractional factorial experiment.  
- Analyze main effects while recognizing confounded interactions.

## Overview
Fractional factorial DOE tests a subset of all combinations, trading off run count for confounding of higher-order interactions. Resolution indicates which effects are aliased.

## Key Steps
1. **Define Full Factorial:** List k factors, each at two levels.  
2. **Choose Fraction:** Determine p to reduce runs (e.g., half-fraction: p=1, 2^(k-1) runs).  
3. **Assign Generators:** Define which interaction defines the fractal pattern.  
4. **Create Design:** Generate run matrix with alias structure.  
5. **Analyze:** Focus on main effects; interpret with respect to resolution.

## Example
4 factors in a half-fraction design (2^(4-1)=8 runs) with generator D=ABC. Main effects clear; two-way interactions partially confounded.

## Exercise
1. Plan a 2^(5-2) design (quarter fraction) and list runs.  
2. Identify alias sets for main effects.  
3. Discuss how to resolve key interactions in follow-up studies.

## Summary
Fractional factorial DOE reduces experimental burden while providing insight into main effects, suitable for high-dimension factor spaces.