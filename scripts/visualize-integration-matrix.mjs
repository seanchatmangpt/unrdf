#!/usr/bin/env node
/**
 * @file Integration Matrix Visualization
 * @description ASCII visualization of package integration complexity
 */

console.log(`
┌────────────────────────────────────────────────────────────────────────────┐
│                  UNRDF INTEGRATION MATRIX VISUALIZATION                    │
│                           58 Packages, 133 Dependencies                     │
└────────────────────────────────────────────────────────────────────────────┘

═══════════════════════════════════════════════════════════════════════════════
  1. HUB-AND-SPOKE TOPOLOGY (Top 4 Hubs)
═══════════════════════════════════════════════════════════════════════════════

                                    ┌──────────────┐
                       ┌───────────▶│  oxigraph    │◀───────────┐
                       │            │  (30 deps)   │            │
                       │            └──────────────┘            │
                       │                                        │
        ┌──────────────┴──────┐              ┌─────────────────┴──────┐
        │  cli                │              │  yawl                   │
        │  kgc-claude         │              │  kgc-4d                 │
        │  fusion             │              │  v6-core                │
        │  (15 packages)      │              │  (15 packages)          │
        └─────────────────────┘              └─────────────────────────┘

                       │            ┌──────────────┐            │
                       │            │    core      │            │
                       └───────────▶│  (28 deps)   │◀───────────┘
                                    └──────────────┘
                                           │
                                           │
                                    ┌──────┴───────┐
                                    │  kgc-4d      │
                                    │  (20 deps)   │
                                    └──────────────┘
                                           │
                                           │
                                    ┌──────┴───────┐
                                    │    yawl      │
                                    │  (13 deps)   │
                                    └──────────────┘

═══════════════════════════════════════════════════════════════════════════════
  2. LAYER DEPENDENCY FLOW (Simplified)
═══════════════════════════════════════════════════════════════════════════════

    L5 (Application)         L4 (Knowledge)       L3 (KGC)           L2 (RDF)
    ┌─────────────┐         ┌─────────────┐     ┌──────────┐      ┌──────────┐
    │    cli      │────────▶│   hooks     │────▶│ kgc-4d   │─────▶│ oxigraph │
    │   react     │         │ federation  │     │ receipts │      │   core   │
    │   nextra    │         │ streaming   │     │ v6-core  │      │          │
    └─────────────┘         └─────────────┘     └──────────┘      └──────────┘
          │  3                    │  4                │ 18              │ 55
          │                       │                   │                 │
          └───────────────────────┴───────────────────┴─────────────────┘
                         (Dependencies flow downward)

                                  L1 (Infrastructure)
                                  ┌──────────────┐
                             ┌───▶│  consensus   │
                             │    │  blockchain  │
                             │    │    daemon    │
                             │    └──────────────┘
                             │           │ 2
                             └───────────┘
                          (⚠ 14 violations detected)

═══════════════════════════════════════════════════════════════════════════════
  3. INTEGRATION PATTERN HEAT MAP (Top 10)
═══════════════════════════════════════════════════════════════════════════════

  kgc-claude → kgc-4d          ████████████████████████████  59
  yawl → kgc-4d                ███████████████████████████   56
  yawl → oxigraph              ████████████                  25
  core → oxigraph              ████████                      17
  cli → core                   █████                         11
  yawl → yawl (internal)       ████                          10
  kgc-claude → oxigraph        ████                           9
  cli → knowledge-engine       ███                            7
  fusion → kgc-4d              ███                            7
  cli → decision-fabric        ██                             6

═══════════════════════════════════════════════════════════════════════════════
  4. CRITICAL API DEPENDENCIES (Top 10 Functions)
═══════════════════════════════════════════════════════════════════════════════

  now()                        ████████████████████████████████  64
  createStore()                ████████████████████████████      51
  dataFactory                  ████████████████████████████      51
  toISO()                      █████████████████                 34
  VectorClock                  ████████                          16
  createWorkflowEngine()       ███████                           14
  freezeUniverse()             ████                               9
  createWorkflow()             ████                               8
  KGCStore                     ███                                7
  defaultObservabilityManager  ███                                6

═══════════════════════════════════════════════════════════════════════════════
  5. CIRCULAR DEPENDENCY CHAIN
═══════════════════════════════════════════════════════════════════════════════

                    ┌─────────────────┐
                    │ kgc-multiverse  │
                    └────────┬────────┘
                             │
                             ▼
                    ┌─────────────────┐
                    │    receipts     │
                    └────────┬────────┘
                             │
                             ▼
                    ┌─────────────────┐
                    │ kgc-multiverse  │  ⚠ CYCLE DETECTED
                    └─────────────────┘

═══════════════════════════════════════════════════════════════════════════════
  6. PACKAGE TIER DISTRIBUTION
═══════════════════════════════════════════════════════════════════════════════

  L5_APPLICATION               ███                          3 packages
  L4_KNOWLEDGE_SUBSTRATE       ████                         4 packages
  L3_KGC                       █████████████               13 packages
  L2_RDF_CORE                  ████                         4 packages
  L1_INFRASTRUCTURE            ████                         4 packages
  YAWL_ECOSYSTEM               ██████████                  10 packages
  SPECIALIZED                  ████████████████████        20 packages

═══════════════════════════════════════════════════════════════════════════════
  7. COMPLEXITY METRICS SUMMARY
═══════════════════════════════════════════════════════════════════════════════

  Metric                            Value          Grade     Status
  ────────────────────────────────────────────────────────────────────────────
  Package Count                     58             🟡        MEDIUM
  Dependency Density                3.95%          🟢        LOW (Good)
  Hub Concentration                 43.6%          🟡        MEDIUM
  Layer Violations                  14 (1 crit)    🟢        LOW
  Circular Dependencies             1 cycle        🟢        LOW
  API Surface                       296 exports    🟢        LOW
  Integration Diversity             3.41 avg       🟢        LOW
  Version Fragmentation             8 versions     🟡        MEDIUM

  ────────────────────────────────────────────────────────────────────────────
  OVERALL ARCHITECTURE GRADE: B+ (Good)
  ────────────────────────────────────────────────────────────────────────────

═══════════════════════════════════════════════════════════════════════════════
  8. KEY FINDINGS
═══════════════════════════════════════════════════════════════════════════════

  ✅ STRENGTHS:
     • Low dependency density (96% of potential connections unused)
     • Clear layer separation (mostly clean architecture)
     • Focused API surfaces (avg 5.1 exports per package)
     • Minimal circular dependencies (only 1 cycle)

  ⚠️  AREAS FOR IMPROVEMENT:
     • High hub concentration (oxigraph + core = 43.6% of dependencies)
     • Version fragmentation (8 versions across 58 packages)
     • 1 critical layer violation (consensus → federation)
     • YAWL ↔ KGC tight coupling (5 cross-domain dependencies)

═══════════════════════════════════════════════════════════════════════════════
  9. RECOMMENDED ACTIONS
═══════════════════════════════════════════════════════════════════════════════

  PRIORITY 1 (Critical):
    1. Resolve consensus → federation violation (L1 → L4 upward dependency)
    2. Stabilize core package (latest-alpha.1 → latest)
    3. Freeze critical APIs (now, createStore, dataFactory)

  PRIORITY 2 (High):
    4. Break kgc-multiverse ↔ receipts cycle
    5. Document YAWL → KGC-4D integration pattern
    6. Version alignment (consolidate 8 versions → 3-4 major versions)

  PRIORITY 3 (Medium):
    7. Create adapter layers for hub packages
    8. Audit 14 layer violations
    9. Performance benchmark critical paths

═══════════════════════════════════════════════════════════════════════════════

Analysis Date: 2026-01-11
Generated by: analyze-integration-matrix.mjs, analyze-import-patterns.mjs
Source Files Analyzed: 906 .mjs files across 58 packages
`);
