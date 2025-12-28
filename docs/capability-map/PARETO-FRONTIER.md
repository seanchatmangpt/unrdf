# UNRDF Pareto Frontier Analysis

**Generated**: 2025-12-28T04:09:54.775Z
**Total Compositions Analyzed**: 5
**Pareto Frontier**: Top 20 compositions by value/complexity ratio

---

## Overview

The Pareto frontier identifies compositions that provide maximum value with minimum complexity. This analysis helps:

1. Prioritize integration efforts (80/20 rule)
2. Identify high-ROI capabilities
3. Understand trade-offs between value and complexity
4. Guide architectural decisions

### Scoring Methodology

- **Value Score** (1-10): Higher is better
  - 10: Unique/fastest capabilities
  - 8: High value/deterministic
  - 7: Complete/auditable
  - 6: Type-safe/governed
  - 5: Default

- **Complexity Score** (1-6): Lower is better
  - 1: O(1) constant time
  - 2: O(log n) logarithmic
  - 3: O(n) linear
  - 4: O(n log n) linearithmic
  - 5: Default
  - 6: O(n²) quadratic

- **Pareto Score**: value / complexity (higher is better)

---

## Top 20 Compositions

### 1. C1: Oxigraph + Core

**Package**: @unrdf/oxigraph

**Description**: Foundation

**Value**: High-performance RDF foundation (Score: 8/10)

**Complexity**: O(1) store overhead (Score: 1/6)

**Pareto Score**: 8.00

**Dependencies**: @unrdf/oxigraph, @unrdf/core

**Evidence**: test/basic.test.mjs:45-67

---

### 2. C2: Store + Logging

**Package**: @unrdf/core

**Description**: Auditable Mutations

**Value**: Complete audit trail of mutations (Score: 7/10)

**Complexity**: O(1) per operation (Score: 1/6)

**Pareto Score**: 7.00

**Dependencies**: @unrdf/core

**Evidence**: test/logging.test.mjs (67 lines)

---

### 3. C1: Query + Validation

**Package**: @unrdf/core

**Description**: Validated SPARQL Queries

**Value**: Type-safe SPARQL results (Score: 6/10)

**Complexity**: O(n) validation overhead (Score: 3/6)

**Pareto Score**: 2.00

**Dependencies**: @unrdf/core

**Evidence**: test/query-validation.test.mjs (45 lines)

---

### 4. C3: Canonicalize + Isomorphism

**Package**: @unrdf/core

**Description**: Graph Comparison

**Value**: Deterministic graph equality (Score: 8/10)

**Complexity**: O(n log n) canonicalization (Score: 4/6)

**Pareto Score**: 2.00

**Dependencies**: @unrdf/core

**Evidence**: test/canonicalize.test.mjs (89 lines)

---

### 5. C2: Oxigraph + Hooks

**Package**: @unrdf/oxigraph

**Description**: Policy-Gated Store

**Value**: Governed mutations with policies (Score: 6/10)

**Complexity**: O(c) where c = hook conditions (Score: 5/6)

**Pareto Score**: 1.20

**Dependencies**: @unrdf/oxigraph, @unrdf/hooks

**Evidence**: test/policy-gated-store.test.mjs:23-45

---

## Pareto Frontier Visualization

```
Value/Complexity Scatter Plot (Pareto Score)
━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

10 │ C1
 9 │ C2
 8 │ C1
 7 │ C3
 6 │ C2
 5 │
 4 │
 3 │
 2 │
 1 │
 0 └─────────────────────────────────────
   0  1  2  3  4  5  6  7  8  9  10
              Complexity →
```

---

## Complete Ranking

1. **C1** (@unrdf/oxigraph) - Pareto: 8.00 (V:8, C:1)
2. **C2** (@unrdf/core) - Pareto: 7.00 (V:7, C:1)
3. **C1** (@unrdf/core) - Pareto: 2.00 (V:6, C:3)
4. **C3** (@unrdf/core) - Pareto: 2.00 (V:8, C:4)
5. **C2** (@unrdf/oxigraph) - Pareto: 1.20 (V:6, C:5)

---

**Last Updated**: 2025-12-28T04:09:54.775Z
**Source**: Automated Pareto frontier analysis
**Methodology**: Value/Complexity ratio optimization
