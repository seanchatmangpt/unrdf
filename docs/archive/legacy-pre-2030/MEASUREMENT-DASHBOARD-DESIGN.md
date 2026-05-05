# Measurement Dashboard Design

## Real-Time Observability for UNRDF Monorepo

**Version**: latest
**Status**: Active
**Last Updated**: 2024-12-26

---

## 1. Overview

This document specifies the dashboard design for visualizing the dimension certificate measurements across the 43-package UNRDF monorepo. The dashboard provides real-time visibility into entropy, coupling, and capacity metrics.

### latest Design Principles

1. **Glanceability**: Critical metrics visible in <3 seconds
2. **Drill-down**: From system overview to package details in 2 clicks
3. **Alert-first**: Problems surface immediately, health is ambient
4. **Historical context**: Current state shown relative to trends

---

## 2. Dashboard Views

### latest System Overview (Landing Page)

```
+==============================================================================+
|                        UNRDF MEASUREMENT DASHBOARD                           |
|==============================================================================|
|                                                                              |
|  +-------------------------+  +-------------------------+  +---------------+ |
|  |     SYSTEM ENTROPY      |  |    CHANGE CAPACITY     |  |    HEALTH     | |
|  |        D_t = latest      |  |      C_t = latest        |  |   43/43 OK    | |
|  |       +latest% 7d          |  |     -latest% 7d           |  |   [========]  | |
|  +-------------------------+  +-------------------------+  +---------------+ |
|                                                                              |
|  +-----------------------------------+  +----------------------------------+ |
|  |        TIER DISTRIBUTION          |  |        ENTROPY BY TIER          | |
|  |                                   |  |                                  | |
|  |  T1 [=====] 5 pkg (latest%)        |  |  T1  [==============] latest bits  | |
|  |  T2 [========] 8 pkg (latest%)     |  |  T2  [==========] latest bits      | |
|  |  T3 [==============] 12 pkg (28%)|  |  T3  [=======] latest bits         | |
|  |  T4 [==============] 12 pkg (28%)|  |  T4  [====] latest bits            | |
|  |  T5 [======] 6 pkg (14%)         |  |  T5  [=] latest bits               | |
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

### latest D_t Entropy Tracker

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
|  |    core    [=========] latest bits                                         ||
|  |    oxigraph [========] latest bits                                         ||
|  |    hooks   [=======] latest bits                                           ||
|  |    kgc-4d  [======] latest bits                                            ||
|  |    yawl    [=====] latest bits                                             ||
|  |    ...                                                                   ||
|  |    atomvm  [=] latest bits                                                 ||
|  |                                                                          ||
|  |  Legend: [Low: 0-2] [Medium: 2-4] [High: 4-6] [Critical: >6]            ||
|  +--------------------------------------------------------------------------+|
|                                                                              |
|  +----------------------------------+  +-----------------------------------+ |
|  |      TOP 5 ENTROPY PACKAGES     |  |    LARGEST ENTROPY CHANGES (7d)   | |
|  |                                  |  |                                   | |
|  |  1. core         latest bits      |  |  federation   +latest bits (+18%)   | |
|  |  2. oxigraph     latest bits      |  |  yawl-api     +latest bits (+12%)   | |
|  |  3. hooks        latest bits      |  |  streaming    -latest bits (-8%)    | |
|  |  4. kgc-4d       latest bits      |  |  caching      +latest bits (+5%)    | |
|  |  5. yawl         latest bits      |  |  react        -latest bits (-3%)    | |
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

### latest TC Coupling Heatmap

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
|  |  Legend: [##]=TC>latest  [#]=TC latest.7  [.]=TC<latest  [ ]=No coupling        ||
|  +--------------------------------------------------------------------------+|
|                                                                              |
|  +----------------------------------+  +-----------------------------------+ |
|  |      HIGH COUPLING PAIRS        |  |        COUPLING CLUSTERS          | |
|  |                                  |  |                                   | |
|  |  core <-> oxigraph    TC=latest   |  |  Cluster 1 (5 packages):          | |
|  |  core <-> validation  TC=latest   |  |    core, oxigraph, validation,    | |
|  |  hooks <-> validation TC=latest   |  |    hooks, kgc-4d                  | |
|  |  yawl <-> yawl-api    TC=latest   |  |                                   | |
|  |  ml-* packages        TC=latest   |  |  Cluster 2 (8 packages):          | |
|  |                                  |  |    yawl, yawl-api, yawl-kafka,    | |
|  |                                  |  |    yawl-queue, ...               | |
|  +----------------------------------+  +-----------------------------------+ |
|                                                                              |
|  +--------------------------------------------------------------------------+|
|  |                    COUPLING TREND (DENSITY)                              ||
|  |                                                                          ||
|  |  latest |                                                                  ||
|  |       |      *                                                           ||
|  |  latest |  *     *   *                                                     ||
|  |       |          *   *   *   *                                           ||
|  |  latest |                      *   *   *   *   *                           ||
|  |       |                                      *   *   *                   ||
|  |  latest +----+----+----+----+----+----+----+----+----+----+----+          ||
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

### latest TE Causality Graph

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
|  |                            | TE=latest  |                                  ||
|  |                            +----+-----+                                  ||
|  |                                 |                                        ||
|  |                                 v                                        ||
|  |                            +----------+                                  ||
|  |            +-------------->|   core   |<--------------+                  ||
|  |            |               | TE=latest  |               |                  ||
|  |            |               +----+-----+               |                  ||
|  |            |                    |                     |                  ||
|  |            v                    v                     v                  ||
|  |     +----------+          +----------+          +----------+             ||
|  |     |validation|          |  hooks   |          | streaming|             ||
|  |     | TE=latest  |          | TE=latest  |          | TE=latest  |             ||
|  |     +----------+          +----+-----+          +----------+             ||
|  |            |                   |                                         ||
|  |            v                   v                                         ||
|  |     +----------+          +----------+                                   ||
|  |     |  kgc-4d  |          |   yawl   |                                   ||
|  |     | TE=latest  |          | TE=latest  |                                   ||
|  |     +----------+          +----------+                                   ||
|  |                                                                          ||
|  +--------------------------------------------------------------------------+|
|                                                                              |
|  +--------------------------------------------------------------------------+|
|  |                    POLICY INFLUENCE TABLE                                ||
|  |                                                                          ||
|  |  Package      | Direct D_t | Dep Count | TE Total | Influence Score     ||
|  |  -------------|------------|-----------|----------|--------------------  ||
|  |  oxigraph     | latest       | 0         | latest     | CRITICAL (100%)     ||
|  |  core         | latest       | 1         | latest     | CRITICAL (91%)      ||
|  |  validation   | latest       | 1         | latest     | HIGH (67%)          ||
|  |  hooks        | latest       | 2         | latest     | HIGH (57%)          ||
|  |  observability| latest       | 0         | latest     | MEDIUM (45%)        ||
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

### latest C_t Capacity Chart

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
|  |  core          | 24    | latest | latest | 20%    | [====.............] OK   ||
|  |  oxigraph      | 24    | latest | latest | 17%    | [===..............] OK   ||
|  |  hooks         | 20    | latest | latest | 17%    | [===..............] OK   ||
|  |  kgc-4d        | 18    | latest | latest | 17%    | [===..............] OK   ||
|  |  yawl          | 18    | latest | latest | 15%    | [===..............] OK   ||
|  |  ...           | ...   | ...  | ...  | ...    | ...                      ||
|  |  yawl-viz      | 16    | latest | latest | 2%     | [................] OK    ||
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
|  |  This Week:   +latest bits consumed (net)                                  ||
|  |  This Month:  +latest bits consumed (net)                                  ||
|  |  Projection:  At current rate, will reach 50% system capacity in 8 mo   ||
|  |                                                                          ||
|  +--------------------------------------------------------------------------+|
|                                                                              |
+==============================================================================+
```

---

## 3. Alert Configuration

### latest Alert Thresholds

| Alert Level | Condition | Action |
|-------------|-----------|--------|
| **INFO** | D_t increased >1 bit in 24h | Log, no notification |
| **WARNING** | Package C_t < 25% of C_max | Slack notification |
| **CRITICAL** | Package C_t < 10% of C_max | Page on-call, block PR |
| **OVERFLOW** | Package D_t > C_max | Block all changes, escalate |

### latest Alert Panel

```
+==============================================================================+
|                            ACTIVE ALERTS                                     |
|==============================================================================+
|                                                                              |
|  [!] CRITICAL (0)  [*] WARNING (2)  [i] INFO (5)          [Acknowledge All] |
|                                                                              |
|  +--------------------------------------------------------------------------+|
|  |                                                                          ||
|  |  [*] WARNING  federation  TC spike detected (latest -> latest)    2h ago    ||
|  |      Action: Review recent commits for coupling patterns                 ||
|  |      [View Details] [Acknowledge] [Suppress 24h]                         ||
|  |                                                                          ||
|  |  [*] WARNING  yawl-api    Approaching capacity (C_t = latest)    5h ago    ||
|  |      Action: Consider refactoring or splitting package                   ||
|  |      [View Details] [Acknowledge] [Suppress 24h]                         ||
|  |                                                                          ||
|  |  [i] INFO     kgc-4d      Entropy increased +latest bits        12h ago   ||
|  |  [i] INFO     streaming   New dependency added: observability 1d ago    ||
|  |  [i] INFO     ...                                                        ||
|  |                                                                          ||
|  +--------------------------------------------------------------------------+|
|                                                                              |
+==============================================================================+
```

---

## 4. Drill-down Paths

### latest Navigation Hierarchy

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

### latest Package Detail View

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
|  |        latest bits                |  |    latest bits remaining           | |
|  |     +latest bits (7d)             |  |    (latest% of C_max)               | |
|  |                                  |  |                                   | |
|  |  [====......] 20% of capacity   |  |  Burn rate: +latest bits/day        | |
|  +----------------------------------+  +-----------------------------------+ |
|                                                                              |
|  +--------------------------------------------------------------------------+|
|  |                        ENTROPY BY PARTITION                              ||
|  |                                                                          ||
|  |  src      [========] latest bits  (40%)   21 files, 4500 lines            ||
|  |  tests    [======] latest bits    (30%)   18 files, 3200 lines            ||
|  |  docs     [===] latest bits       (15%)   12 files                        ||
|  |  config   [==] latest bits        (15%)   5 files                         ||
|  +--------------------------------------------------------------------------+|
|                                                                              |
|  +--------------------------------------------------------------------------+|
|  |                       FEATURE METRICS                                    ||
|  |                                                                          ||
|  |  Source Files:      21          Function Count:      156                 ||
|  |  Test Coverage:     latest%       Cyclomatic Complexity: latest (avg)        ||
|  |  Internal Deps:     1           External Deps:        12                 ||
|  |  Test Pass Rate:    100%        Max File Length:      342 lines         ||
|  +--------------------------------------------------------------------------+|
|                                                                              |
|  +--------------------------------------------------------------------------+|
|  |                    TEMPORAL COUPLING                                     ||
|  |                                                                          ||
|  |  Top coupled packages:                                                   ||
|  |    oxigraph    TC=latest  (High - frequently change together)             ||
|  |    validation  TC=latest  (High - shared dependencies)                    ||
|  |    hooks       TC=latest  (High - API contract)                           ||
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

### latest Data Sources

- **Feature extraction**: File system scan + AST analysis
- **TC computation**: Git log analysis (90-day window default)
- **TE computation**: package.json dependency graph
- **Historical data**: Time-series database (InfluxDB/Prometheus)

### latest Update Strategy

1. **Real-time**: File watcher triggers feature re-extraction
2. **Batched**: TC matrix recomputed every 15 minutes
3. **Scheduled**: Full measurement + certificate at midnight
4. **On-demand**: PR trigger for affected packages only

### latest Performance Targets

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
