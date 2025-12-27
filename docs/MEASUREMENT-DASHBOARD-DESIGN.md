# Measurement Dashboard Design

## Real-Time Observability for UNRDF Monorepo

**Version**: 1.0.0
**Status**: Active
**Last Updated**: 2024-12-26

---

## 1. Overview

This document specifies the dashboard design for visualizing the dimension certificate measurements across the 43-package UNRDF monorepo. The dashboard provides real-time visibility into entropy, coupling, and capacity metrics.

### 1.1 Design Principles

1. **Glanceability**: Critical metrics visible in <3 seconds
2. **Drill-down**: From system overview to package details in 2 clicks
3. **Alert-first**: Problems surface immediately, health is ambient
4. **Historical context**: Current state shown relative to trends

---

## 2. Dashboard Views

### 2.1 System Overview (Landing Page)

```
+==============================================================================+
|                        UNRDF MEASUREMENT DASHBOARD                           |
|==============================================================================|
|                                                                              |
|  +-------------------------+  +-------------------------+  +---------------+ |
|  |     SYSTEM ENTROPY      |  |    CHANGE CAPACITY     |  |    HEALTH     | |
|  |        D_t = 18.79      |  |      C_t = 5.21        |  |   43/43 OK    | |
|  |       +2.3% 7d          |  |     -0.8% 7d           |  |   [========]  | |
|  +-------------------------+  +-------------------------+  +---------------+ |
|                                                                              |
|  +-----------------------------------+  +----------------------------------+ |
|  |        TIER DISTRIBUTION          |  |        ENTROPY BY TIER          | |
|  |                                   |  |                                  | |
|  |  T1 [=====] 5 pkg (11.6%)        |  |  T1  [==============] 8.2 bits  | |
|  |  T2 [========] 8 pkg (18.6%)     |  |  T2  [==========] 5.1 bits      | |
|  |  T3 [==============] 12 pkg (28%)|  |  T3  [=======] 3.5 bits         | |
|  |  T4 [==============] 12 pkg (28%)|  |  T4  [====] 1.5 bits            | |
|  |  T5 [======] 6 pkg (14%)         |  |  T5  [=] 0.5 bits               | |
|  +-----------------------------------+  +----------------------------------+ |
|                                                                              |
|  +--------------------------------------------------------------------------+|
|  |                    ENTROPY TREND (30 DAYS)                               ||
|  |   20 |                                                      *            ||
|  |      |                                             *   *  *              ||
|  |   15 |                                    *   *  *                       ||
|  |      |                         *    *  *                                 ||
|  |   10 |              *    *  *                                            ||
|  |      |    *    *  *                                                      ||
|  |    5 +----+----+----+----+----+----+----+----+----+----+----+----+----+  ||
|  |       D-30 D-25 D-20 D-15 D-10  D-5  NOW                                 ||
|  +--------------------------------------------------------------------------+|
|                                                                              |
+==============================================================================+
```

#### Metrics Displayed:

| Metric | Description | Update Frequency |
|--------|-------------|------------------|
| System D_t | Total weighted entropy | Real-time |
| System C_t | Average change capacity | Real-time |
| Health | Packages in healthy state | Real-time |
| Tier Distribution | Package count per tier | On change |
| Entropy by Tier | Sum of D_t per tier | Real-time |
| Entropy Trend | 30-day historical chart | Hourly |

---

### 2.2 D_t Entropy Tracker

```
+==============================================================================+
|                         ENTROPY TRACKER (D_t)                                |
|==============================================================================|
|                                                                              |
|  [System] [By Tier] [By Package] [Top Changes]                   [Export]   |
|                                                                              |
|  +--------------------------------------------------------------------------+|
|  |                     PACKAGE ENTROPY HEATMAP                              ||
|  |                                                                          ||
|  |    core    [=========] 4.82 bits                                         ||
|  |    oxigraph [========] 4.12 bits                                         ||
|  |    hooks   [=======] 3.45 bits                                           ||
|  |    kgc-4d  [======] 2.98 bits                                            ||
|  |    yawl    [=====] 2.67 bits                                             ||
|  |    ...                                                                   ||
|  |    atomvm  [=] 0.34 bits                                                 ||
|  |                                                                          ||
|  |  Legend: [Low: 0-2] [Medium: 2-4] [High: 4-6] [Critical: >6]            ||
|  +--------------------------------------------------------------------------+|
|                                                                              |
|  +----------------------------------+  +-----------------------------------+ |
|  |      TOP 5 ENTROPY PACKAGES     |  |    LARGEST ENTROPY CHANGES (7d)   | |
|  |                                  |  |                                   | |
|  |  1. core         4.82 bits      |  |  federation   +0.82 bits (+18%)   | |
|  |  2. oxigraph     4.12 bits      |  |  yawl-api     +0.45 bits (+12%)   | |
|  |  3. hooks        3.45 bits      |  |  streaming    -0.33 bits (-8%)    | |
|  |  4. kgc-4d       2.98 bits      |  |  caching      +0.21 bits (+5%)    | |
|  |  5. yawl         2.67 bits      |  |  react        -0.12 bits (-3%)    | |
|  +----------------------------------+  +-----------------------------------+ |
|                                                                              |
|  +--------------------------------------------------------------------------+|
|  |                    ENTROPY BREAKDOWN BY PARTITION                        ||
|  |                                                                          ||
|  |           src     tests    docs    config                                ||
|  |  core   [====]   [===]    [==]     [=]                                   ||
|  |  hooks  [===]    [===]    [=]      [=]                                   ||
|  |  yawl   [===]    [==]     [==]     [=]                                   ||
|  |                                                                          ||
|  +--------------------------------------------------------------------------+|
|                                                                              |
+==============================================================================+
```

#### Drill-down Paths:

1. **System -> Tier**: Click tier bar to filter packages
2. **Tier -> Package**: Click package row for detail view
3. **Package -> Partition**: See entropy contribution by src/tests/docs/config

---

### 2.3 TC Coupling Heatmap

```
+==============================================================================+
|                       TEMPORAL COUPLING (TC) MATRIX                          |
|==============================================================================|
|                                                                              |
|  [Full Matrix] [Clusters Only] [High Coupling] [Time Range: 90d v]          |
|                                                                              |
|  +--------------------------------------------------------------------------+|
|  |                         43x43 COUPLING MATRIX                            ||
|  |                                                                          ||
|  |          core oxgph valid hooks strm obsrv cache yawl  kgn  ...          ||
|  |  core    [##] [##]  [##]  [##]  [#]   [#]   [#]  [.]  [.]   ...          ||
|  |  oxgph   [##] [##]  [#]   [.]   [#]   [.]   [.]  [.]  [#]   ...          ||
|  |  valid   [##] [#]   [##]  [##]  [.]   [.]   [.]  [.]  [.]   ...          ||
|  |  hooks   [##] [.]   [##]  [##]  [.]   [#]   [.]  [#]  [.]   ...          ||
|  |  strm    [#]  [#]   [.]   [.]   [##]  [.]   [.]  [.]  [#]   ...          ||
|  |  obsrv   [#]  [.]   [.]   [#]   [.]   [##]  [.]  [.]  [.]   ...          ||
|  |  ...     ...  ...   ...   ...   ...   ...   ...  ...  ...   ...          ||
|  |                                                                          ||
|  |  Legend: [##]=TC>0.7  [#]=TC 0.3-0.7  [.]=TC<0.3  [ ]=No coupling        ||
|  +--------------------------------------------------------------------------+|
|                                                                              |
|  +----------------------------------+  +-----------------------------------+ |
|  |      HIGH COUPLING PAIRS        |  |        COUPLING CLUSTERS          | |
|  |                                  |  |                                   | |
|  |  core <-> oxigraph    TC=0.87   |  |  Cluster 1 (5 packages):          | |
|  |  core <-> validation  TC=0.84   |  |    core, oxigraph, validation,    | |
|  |  hooks <-> validation TC=0.72   |  |    hooks, kgc-4d                  | |
|  |  yawl <-> yawl-api    TC=0.68   |  |                                   | |
|  |  ml-* packages        TC=0.65   |  |  Cluster 2 (8 packages):          | |
|  |                                  |  |    yawl, yawl-api, yawl-kafka,    | |
|  |                                  |  |    yawl-queue, ...               | |
|  +----------------------------------+  +-----------------------------------+ |
|                                                                              |
|  +--------------------------------------------------------------------------+|
|  |                    COUPLING TREND (DENSITY)                              ||
|  |                                                                          ||
|  |  0.30 |                                                                  ||
|  |       |      *                                                           ||
|  |  0.25 |  *     *   *                                                     ||
|  |       |          *   *   *   *                                           ||
|  |  0.20 |                      *   *   *   *   *                           ||
|  |       |                                      *   *   *                   ||
|  |  0.15 +----+----+----+----+----+----+----+----+----+----+----+          ||
|  |                                                                          ||
|  +--------------------------------------------------------------------------+|
|                                                                              |
+==============================================================================+
```

#### Interaction:

- **Hover**: Show exact TC value and common commits
- **Click cell**: Navigate to commit history showing co-changes
- **Select cluster**: Highlight related packages, show refactoring suggestions

---

### 2.4 TE Causality Graph

```
+==============================================================================+
|                     TRANSITIVE ENTROPY (TE) GRAPH                            |
|==============================================================================|
|                                                                              |
|  [DAG View] [Table View] [Policy Packages Only]                 [Refresh]   |
|                                                                              |
|  +--------------------------------------------------------------------------+|
|  |                      DEPENDENCY-WEIGHTED ENTROPY FLOW                    ||
|  |                                                                          ||
|  |                            +----------+                                  ||
|  |                            | oxigraph |                                  ||
|  |                            | TE=42.3  |                                  ||
|  |                            +----+-----+                                  ||
|  |                                 |                                        ||
|  |                                 v                                        ||
|  |                            +----------+                                  ||
|  |            +-------------->|   core   |<--------------+                  ||
|  |            |               | TE=38.7  |               |                  ||
|  |            |               +----+-----+               |                  ||
|  |            |                    |                     |                  ||
|  |            v                    v                     v                  ||
|  |     +----------+          +----------+          +----------+             ||
|  |     |validation|          |  hooks   |          | streaming|             ||
|  |     | TE=28.4  |          | TE=24.1  |          | TE=12.8  |             ||
|  |     +----------+          +----+-----+          +----------+             ||
|  |            |                   |                                         ||
|  |            v                   v                                         ||
|  |     +----------+          +----------+                                   ||
|  |     |  kgc-4d  |          |   yawl   |                                   ||
|  |     | TE=15.2  |          | TE=18.9  |                                   ||
|  |     +----------+          +----------+                                   ||
|  |                                                                          ||
|  +--------------------------------------------------------------------------+|
|                                                                              |
|  +--------------------------------------------------------------------------+|
|  |                    POLICY INFLUENCE TABLE                                ||
|  |                                                                          ||
|  |  Package      | Direct D_t | Dep Count | TE Total | Influence Score     ||
|  |  -------------|------------|-----------|----------|--------------------  ||
|  |  oxigraph     | 4.12       | 0         | 42.3     | CRITICAL (100%)     ||
|  |  core         | 4.82       | 1         | 38.7     | CRITICAL (91%)      ||
|  |  validation   | 2.54       | 1         | 28.4     | HIGH (67%)          ||
|  |  hooks        | 3.45       | 2         | 24.1     | HIGH (57%)          ||
|  |  observability| 1.87       | 0         | 18.9     | MEDIUM (45%)        ||
|  |  ...          | ...        | ...       | ...      | ...                 ||
|  +--------------------------------------------------------------------------+|
|                                                                              |
+==============================================================================+
```

#### Features:

- **Node size**: Proportional to TE value
- **Edge thickness**: Proportional to entropy propagation
- **Colors**: Red=Critical, Orange=High, Yellow=Medium, Green=Low
- **Animation**: Shows entropy "flowing" through graph on hover

---

### 2.5 C_t Capacity Chart

```
+==============================================================================+
|                        CHANGE CAPACITY (C_t) GAUGE                           |
|==============================================================================|
|                                                                              |
|  [By Package] [By Tier] [Alerts Only] [Historical]              [Export]    |
|                                                                              |
|  +--------------------------------------------------------------------------+|
|  |                      PACKAGE CAPACITY STATUS                             ||
|  |                                                                          ||
|  |  Package       | C_max | D_t  | C_t  | Usage  | Status                   ||
|  |  --------------|-------|------|------|--------|------------------------  ||
|  |  core          | 24    | 4.82 | 19.2 | 20%    | [====.............] OK   ||
|  |  oxigraph      | 24    | 4.12 | 19.9 | 17%    | [===..............] OK   ||
|  |  hooks         | 20    | 3.45 | 16.6 | 17%    | [===..............] OK   ||
|  |  kgc-4d        | 18    | 2.98 | 15.0 | 17%    | [===..............] OK   ||
|  |  yawl          | 18    | 2.67 | 15.3 | 15%    | [===..............] OK   ||
|  |  ...           | ...   | ...  | ...  | ...    | ...                      ||
|  |  yawl-viz      | 16    | 0.34 | 15.7 | 2%     | [................] OK    ||
|  |                                                                          ||
|  |  Legend: [.....] OK  [!!!!!] Warning (<25%)  [XXXXX] Critical (<10%)     ||
|  +--------------------------------------------------------------------------+|
|                                                                              |
|  +----------------------------------+  +-----------------------------------+ |
|  |      LOW CAPACITY ALERTS        |  |       CAPACITY DISTRIBUTION       | |
|  |                                  |  |                                   | |
|  |  No packages in critical state  |  |   100% |                          | |
|  |                                  |  |        |     *                    | |
|  |  Packages approaching warning:  |  |    75% |   *   *                  | |
|  |  (none)                          |  |        | *       *                | |
|  |                                  |  |    50% +           *   *          | |
|  |                                  |  |        |               *   *      | |
|  |                                  |  |    25% |                   *  *   | |
|  |                                  |  |        +--+--+--+--+--+--+--+--+  | |
|  |                                  |  |         0  5  10 15 20 25  C_t    | |
|  +----------------------------------+  +-----------------------------------+ |
|                                                                              |
|  +--------------------------------------------------------------------------+|
|  |                  EPOCH-OVER-EPOCH CAPACITY BURN                          ||
|  |                                                                          ||
|  |  This Week:   +0.23 bits consumed (net)                                  ||
|  |  This Month:  +1.87 bits consumed (net)                                  ||
|  |  Projection:  At current rate, will reach 50% system capacity in 8 mo   ||
|  |                                                                          ||
|  +--------------------------------------------------------------------------+|
|                                                                              |
+==============================================================================+
```

---

## 3. Alert Configuration

### 3.1 Alert Thresholds

| Alert Level | Condition | Action |
|-------------|-----------|--------|
| **INFO** | D_t increased >1 bit in 24h | Log, no notification |
| **WARNING** | Package C_t < 25% of C_max | Slack notification |
| **CRITICAL** | Package C_t < 10% of C_max | Page on-call, block PR |
| **OVERFLOW** | Package D_t > C_max | Block all changes, escalate |

### 3.2 Alert Panel

```
+==============================================================================+
|                            ACTIVE ALERTS                                     |
|==============================================================================+
|                                                                              |
|  [!] CRITICAL (0)  [*] WARNING (2)  [i] INFO (5)          [Acknowledge All] |
|                                                                              |
|  +--------------------------------------------------------------------------+|
|  |                                                                          ||
|  |  [*] WARNING  federation  TC spike detected (0.45 -> 0.72)    2h ago    ||
|  |      Action: Review recent commits for coupling patterns                 ||
|  |      [View Details] [Acknowledge] [Suppress 24h]                         ||
|  |                                                                          ||
|  |  [*] WARNING  yawl-api    Approaching capacity (C_t = 4.2)    5h ago    ||
|  |      Action: Consider refactoring or splitting package                   ||
|  |      [View Details] [Acknowledge] [Suppress 24h]                         ||
|  |                                                                          ||
|  |  [i] INFO     kgc-4d      Entropy increased +0.34 bits        12h ago   ||
|  |  [i] INFO     streaming   New dependency added: observability 1d ago    ||
|  |  [i] INFO     ...                                                        ||
|  |                                                                          ||
|  +--------------------------------------------------------------------------+|
|                                                                              |
+==============================================================================+
```

---

## 4. Drill-down Paths

### 4.1 Navigation Hierarchy

```
System Overview
    |
    +-- D_t Tracker
    |       |-- By Tier
    |       |-- By Package --> Package Detail
    |       |       |-- By Partition (src/tests/docs/config)
    |       |       |-- Historical (30/60/90 days)
    |       |       +-- Related Changes (git commits)
    |       +-- Top Changes
    |
    +-- TC Heatmap
    |       |-- Cluster View
    |       |-- Package Pair Detail --> Commit History
    |       +-- Temporal Analysis
    |
    +-- TE Graph
    |       |-- DAG View
    |       |-- Table View
    |       +-- Policy Impact Analysis
    |
    +-- C_t Gauge
            |-- Package Capacity
            |-- Tier Capacity
            +-- Burn Rate Projection
```

### 4.2 Package Detail View

```
+==============================================================================+
|                         PACKAGE: core                                        |
|==============================================================================|
|                                                                              |
|  Tier: 1 (Core)      Path: packages/core      Last Measured: 2 min ago      |
|                                                                              |
|  +----------------------------------+  +-----------------------------------+ |
|  |          ENTROPY (D_t)          |  |          CAPACITY (C_t)           | |
|  |                                  |  |                                   | |
|  |        4.82 bits                |  |    19.18 bits remaining           | |
|  |     +0.12 bits (7d)             |  |    (79.9% of C_max)               | |
|  |                                  |  |                                   | |
|  |  [====......] 20% of capacity   |  |  Burn rate: +0.02 bits/day        | |
|  +----------------------------------+  +-----------------------------------+ |
|                                                                              |
|  +--------------------------------------------------------------------------+|
|  |                        ENTROPY BY PARTITION                              ||
|  |                                                                          ||
|  |  src      [========] 1.93 bits  (40%)   21 files, 4500 lines            ||
|  |  tests    [======] 1.45 bits    (30%)   18 files, 3200 lines            ||
|  |  docs     [===] 0.72 bits       (15%)   12 files                        ||
|  |  config   [==] 0.72 bits        (15%)   5 files                         ||
|  +--------------------------------------------------------------------------+|
|                                                                              |
|  +--------------------------------------------------------------------------+|
|  |                       FEATURE METRICS                                    ||
|  |                                                                          ||
|  |  Source Files:      21          Function Count:      156                 ||
|  |  Test Coverage:     94.2%       Cyclomatic Complexity: 8.3 (avg)        ||
|  |  Internal Deps:     1           External Deps:        12                 ||
|  |  Test Pass Rate:    100%        Max File Length:      342 lines         ||
|  +--------------------------------------------------------------------------+|
|                                                                              |
|  +--------------------------------------------------------------------------+|
|  |                    TEMPORAL COUPLING                                     ||
|  |                                                                          ||
|  |  Top coupled packages:                                                   ||
|  |    oxigraph    TC=0.87  (High - frequently change together)             ||
|  |    validation  TC=0.84  (High - shared dependencies)                    ||
|  |    hooks       TC=0.72  (High - API contract)                           ||
|  +--------------------------------------------------------------------------+|
|                                                                              |
|  [View History] [Compare Epochs] [Export Data] [View Commits]               |
|                                                                              |
+==============================================================================+
```

---

## 5. Real-Time Metric Definitions

| Metric | Computation | Update Trigger |
|--------|-------------|----------------|
| `D_t` (package) | Shannon entropy of features | On file change |
| `D_t` (system) | Weighted sum of package D_t | On any package change |
| `C_t` (package) | `C_max - D_t` | On D_t change |
| `TC` (pair) | Jaccard similarity of commit sets | On commit (debounced) |
| `TE` (package) | Transitive closure of D_t | On dependency change |
| `Health` | `C_t > 0 ? 'OK' : 'OVERFLOW'` | On C_t change |

---

## 6. Technical Implementation Notes

### 6.1 Data Sources

- **Feature extraction**: File system scan + AST analysis
- **TC computation**: Git log analysis (90-day window default)
- **TE computation**: package.json dependency graph
- **Historical data**: Time-series database (InfluxDB/Prometheus)

### 6.2 Update Strategy

1. **Real-time**: File watcher triggers feature re-extraction
2. **Batched**: TC matrix recomputed every 15 minutes
3. **Scheduled**: Full measurement + certificate at midnight
4. **On-demand**: PR trigger for affected packages only

### 6.3 Performance Targets

| Operation | Target Latency | Acceptable |
|-----------|---------------|------------|
| Dashboard load | <500ms | <1s |
| Package drill-down | <200ms | <500ms |
| Full TC matrix render | <2s | <5s |
| Historical query (30d) | <1s | <3s |

---

## 7. Appendix: Color Scheme

| Status | Hex Code | Usage |
|--------|----------|-------|
| Healthy | `#22C55E` | C_t > 50% |
| Warning | `#F59E0B` | C_t 25-50% |
| Critical | `#EF4444` | C_t < 25% |
| Overflow | `#7C3AED` | C_t < 0 |
| Neutral | `#6B7280` | Inactive |

---

*Dashboard design maintained by the UNRDF Platform Team.*
