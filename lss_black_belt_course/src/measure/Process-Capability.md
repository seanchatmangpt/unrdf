# Process Capability

**Duration:** 2 hours
**Objective:** Compute capability indices (Cp, Cpk, Pp, Ppk) and interpret process performance relative to specifications.

## Learning Objectives
- Calculate capability indices from process data and spec limits.  
- Differentiate between short-term (Cp, Cpk) and long-term (Pp, Ppk) metrics.  
- Use capability analysis to guide process improvements.

## Overview
Process capability measures how well a process produces output within specification limits (USL, LSL). Key indices:
- **Cp = (USL – LSL) / (6σ)**: potential capability (ignores centering).  
- **Cpk = min[(USL – μ) / (3σ), (μ – LSL) / (3σ)]**: actual capability (accounts for centering).  
- **Pp/Ppk:** Similar to Cp/Cpk but use overall (long-term) σ.

## Key Steps
1. **Collect Data:** Obtain stable process data, at least 25–30 samples.  
2. **Verify Stability:** Ensure process is in control before capability analysis.  
3. **Calculate σ and μ:** Use standard deviation and mean from data.  
4. **Compute Indices:** Apply Cp, Cpk formulas.  
5. **Interpret Results:** Cp/Cpk ≥ 1.33 is desirable; <1 indicates improvement needed.

## Example
Given USL=10, LSL=2, μ=6, σ=1:  
- Cp = (10–2)/(6×1) = 1.33  
- Cpk = min[(10–6)/(3×1), (6–2)/(3×1)] = min[1.33, 1.33] = 1.33

## Exercise
1. Perform capability analysis on a dataset of 50 measurements.  
2. Determine Cp and Cpk, and recommend process adjustments if Cpk < 1.33.

## Summary
Capability analysis quantifies how well a process can meet specifications, informing whether process improvements or spec changes are required.