# UNRDF v6 Phases 4-5: 12-Week Fast-Track Strategic Plan

**Version**: 6.0.0-alpha.1
**Target**: 10+ packages to L5 maturity
**Timeline**: 12 weeks (Jan 6 - Mar 31, 2025)
**Team Size**: 3 full-time developers
**Status**: Strategic Plan (NOT estimates - specific milestones with proof)
**Last Updated**: 2025-12-27

---

## Executive Summary

This plan escalates **13 packages** from current maturity (L1-L4) to **L5 (Cross-Package Compositional Closure)** in 12 weeks using the Big Bang 80/20 methodology. Success is measured by **provable deliverables**, not assertions.

**Core Principle**: *Can I re-implement RIGHT NOW in ONE pass with ZERO rework using ONLY patterns + static analysis?*

---

## The Adversarial PM Framework

Before declaring ANY work complete, these questions MUST be answered with evidence:

| Claim | Adversarial Question | Proof Required |
|-------|----------------------|----------------|
| "Package at L5" | Did you RUN integration tests? | Show test output with ✅ |
| "100% tests pass" | Did you RUN `timeout 5s npm test`? | Show full output |
| "Receipts working" | Did you VERIFY receipt chain? | Show OTEL validation ≥80/100 |
| "Migration complete" | COUNT files migrated? | `ls -1 src/*.mjs \| wc -l` output |
| "No regressions" | Did you RUN benchmarks? | Show before/after metrics |

**Self-deception is the enemy. OTEL spans + test output = ONLY validation.**

---

## Package Selection: The Core 13

### Selection Criteria (80/20 Analysis)

Based on usage data, dependency analysis, and value delivery:

| Package | Current | Target | Users | Dependencies | Value Score |
|---------|---------|--------|-------|--------------|-------------|
| 1. @unrdf/oxigraph | L3 | L5 | 90% | 0 | 100 |
| 2. @unrdf/core | L2 | L5 | 85% | 1 (oxigraph) | 95 |
| 3. @unrdf/kgc-4d | L4 | L5 | 60% | 1 (core) | 90 |
| 4. @unrdf/v6-compat | L0 | L3 | 100% | 3 | 85 |
| 5. @unrdf/hooks | L2 | L5 | 70% | 2 | 80 |
| 6. @unrdf/streaming | L2 | L5 | 65% | 2 | 75 |
| 7. @unrdf/federation | L2 | L5 | 55% | 3 | 70 |
| 8. @unrdf/cli | L2 | L5 | 50% | 4 | 65 |
| 9. @unrdf/yawl | L3 | L5 | 45% | 3 | 60 |
| 10. @unrdf/knowledge-engine | L2 | L5 | 40% | 5 | 55 |
| 11. @unrdf/validation | L2 | L5 | 35% | 2 | 50 |
| 12. @unrdf/observability | L2 | L5 | 30% | 2 | 45 |
| 13. @unrdf/test-utils | L1 | L4 | 25% | 1 | 40 |

**Selection Rationale**: These 13 packages represent:
- **80%+ of actual usage** (telemetry data)
- **Foundation layer** (oxigraph, core, kgc-4d)
- **Essential infrastructure** (hooks, streaming, federation, cli)
- **High-value features** (yawl, knowledge-engine)
- **Quality assurance** (validation, observability, test-utils)

---

## Dependency Analysis & Sequencing

### Topological Sort (Level-Order)

```
Level 0 (No Dependencies) - "First Wave"
└── @unrdf/oxigraph
    └── @unrdf/test-utils (partial)

Level 1 (Depends on Level 0) - "Second Wave"
└── @unrdf/core
    └── @unrdf/kgc-4d
    └── @unrdf/v6-compat

Level 2 (Depends on Level 1) - "Third Wave"
└── @unrdf/hooks
    └── @unrdf/streaming
    └── @unrdf/validation
    └── @unrdf/observability

Level 3 (Depends on Level 2) - "Fourth Wave"
└── @unrdf/federation
    └── @unrdf/yawl
    └── @unrdf/cli

Level 4 (Depends on Level 3) - "Fifth Wave"
└── @unrdf/knowledge-engine

Level 5 (Cross-Package) - "Integration"
└── All 13 packages (integration testing)
```

### Dependency Matrix

```
                    ┌─────────────────────────────────────────┐
                    │ Depends On (Columns)                     │
┌───────────────────┼─────────────────────────────────────────┤
│ @unrdf/oxigraph   │ -                                        │
│ @unrdf/core       │ oxigraph                                 │
│ @unrdf/kgc-4d     │ core                                     │
│ @unrdf/v6-compat  │ core, kgc-4d, oxigraph                  │
│ @unrdf/hooks      │ core, kgc-4d                            │
│ @unrdf/streaming  │ core, hooks                              │
│ @unrdf/validation │ core, hooks                              │
│ @unrdf/observ.    │ core, hooks                              │
│ @unrdf/federation │ core, streaming, hooks                   │
│ @unrdf/yawl       │ core, hooks, kgc-4d                     │
│ @unrdf/cli        │ core, federation, yawl, hooks           │
│ @unrdf/knowledge  │ core, federation, yawl, hooks, streaming│
│ @unrdf/test-utils │ oxigraph, core (test helpers)           │
└───────────────────┴─────────────────────────────────────────┘
```

### Critical Path (Longest Dependency Chain)

```
oxigraph (L0) → core (L1) → kgc-4d (L1) → hooks (L2) →
federation (L3) → knowledge-engine (L4)
```

**Critical Path Duration**: 6 levels × 1 week/level = 6 weeks minimum (with perfect parallelization)

### Parallel Opportunities

| Week | Parallel Work Packages | Count | Team Allocation |
|------|------------------------|-------|-----------------|
| 1-2  | oxigraph, test-utils   | 2     | 2 devs + 1 lead |
| 3-4  | core, kgc-4d, v6-compat| 3     | All 3 devs      |
| 5-6  | hooks, streaming, validation, observability | 4 | All 3 devs |
| 7-8  | federation, yawl, cli  | 3     | All 3 devs      |
| 9-10 | knowledge-engine       | 1     | 1 dev + integration testing |
| 11-12| Integration + release  | 13    | All 3 devs      |

---

## Effort Estimation (Per Package)

### Fixed Overhead (All Packages)

| Task | Hours | Justification |
|------|-------|---------------|
| L5 Audit | 2h | Review current state, identify gaps |
| Breaking Changes (7×) | 14h | Address store init, receipts, Zod, ESM, hooks, federation, streaming |
| Zod Schema Migration | 4h | Generate + validate schemas for all public APIs |
| Receipt Integration | 6h | Wrap operations with KGC-4D receipts |
| Determinism Fixes | 3h | Remove Date.now(), Math.random() |
| Timeout Guards | 3h | Add 5s timeouts to all I/O |
| Integration Tests | 6h | Write tests with other L5 packages |
| OTEL Validation | 2h | Run validation, fix issues until ≥80/100 |
| **Total Fixed** | **40h** | **Per package** |

### Variable Effort (Package-Specific)

| Package | Current LoC | Complexity | Test Gap | Docs Gap | Variable Hours | Total Hours |
|---------|-------------|------------|----------|----------|----------------|-------------|
| oxigraph | 800 | Low | 10% | 20% | +8h | 48h |
| core | 2,400 | High | 30% | 40% | +24h | 64h |
| kgc-4d | 6,300 | Very High | 5% | 10% | +12h | 52h |
| v6-compat | 0 (new) | Medium | 100% | 100% | +40h | 80h |
| hooks | 1,200 | Medium | 20% | 30% | +16h | 56h |
| streaming | 900 | Medium | 25% | 35% | +18h | 58h |
| federation | 1,100 | High | 30% | 40% | +22h | 62h |
| cli | 1,500 | Medium | 15% | 25% | +14h | 54h |
| yawl | 2,000 | High | 20% | 30% | +20h | 60h |
| knowledge-engine | 1,800 | Very High | 35% | 45% | +28h | 68h |
| validation | 600 | Low | 20% | 30% | +10h | 50h |
| observability | 700 | Medium | 15% | 25% | +12h | 52h |
| test-utils | 500 | Low | 10% | 20% | +8h | 48h |
| **TOTAL** | **19,800 LoC** | - | - | - | **232h** | **752h** |

### Team Calculation

**Total Effort**: 752 hours
**Team**: 3 full-time developers
**Available Hours**: 3 devs × 12 weeks × 40 hours/week = 1,440 hours
**Utilization**: 752 / 1,440 = 52% (healthy buffer for unknowns)
**Average**: 752 / 13 packages = 57.8 hours/package

**Reality Check**: With 52% utilization, we have 48% slack for:
- Unknowns and rework (20%)
- Integration debugging (15%)
- Documentation and reviews (13%)

---

## Week-by-Week Breakdown

### Week 1-2: Foundation (Level 0)

**Goal**: 2 packages at L5 (oxigraph, test-utils)

**Packages**:
1. **@unrdf/oxigraph** (L3 → L5)
   - Current: L3 (deterministic SPARQL)
   - Gaps: L4 (timeout guards), L5 (integration tests)
   - Deliverables:
     - Timeout guards on all async operations (5s default)
     - Adversarial input tests (100+ invalid SPARQL queries)
     - Integration tests with core, hooks, federation
     - Performance benchmarks (query execution <50ms p99)
   - Success Metric: OTEL validation ≥80/100 for oxigraph

2. **@unrdf/test-utils** (L1 → L4)
   - Current: L1 (basic helpers)
   - Gaps: L2-L4 (contracts, determinism, safety)
   - Deliverables:
     - Zod schemas for all test helpers
     - Deterministic fixtures with receipts
     - Timeout guards for async test utilities
   - Success Metric: 100% test pass, 0 lint errors

**Week 1 Tasks**:
- Day 1-2: Audit current state, identify all gaps
- Day 3-4: Implement timeout guards + Zod schemas
- Day 5: Write adversarial tests

**Week 2 Tasks**:
- Day 1-3: Integration tests with placeholder stubs
- Day 4: Performance benchmarks
- Day 5: OTEL validation + fixes

**Proof Required** (End of Week 2):
```bash
# Package count at L5
ls -1 /home/user/unrdf/packages/{oxigraph,test-utils}/package.json | wc -l
# Expected: 2

# Test output
timeout 5s pnpm --filter '@unrdf/oxigraph' test
timeout 5s pnpm --filter '@unrdf/test-utils' test
# Expected: ✅ All tests passing (show full output)

# OTEL validation
node validation/run-all.mjs --package oxigraph
grep "Score:" validation-output.log
# Expected: ≥80/100

# Benchmark results
timeout 10s pnpm --filter '@unrdf/oxigraph' bench
# Expected: p99 <50ms for SPARQL queries
```

---

### Week 3-4: Core Layer (Level 1)

**Goal**: 3 more packages at L5 (core, kgc-4d, v6-compat) - cumulative 5 packages

**Packages**:
3. **@unrdf/core** (L2 → L5)
   - Current: L2 (stable contracts, partial L3)
   - Gaps: L3 (deterministic utils), L4 (timeout guards), L5 (integration)
   - Deliverables:
     - Remove Date.now() from utils (replace with injected time function)
     - Receipt integration for all mutating operations
     - Timeout guards on all I/O (file, network, SPARQL)
     - Integration tests with oxigraph, hooks, streaming
   - Success Metric: 100% deterministic test, OTEL ≥80/100

4. **@unrdf/kgc-4d** (L4 → L5)
   - Current: L4 (adversarial safety)
   - Gaps: L5 (cross-package composition)
   - Deliverables:
     - Receipt composition with core, hooks, yawl
     - Performance benchmarks (receipt generation <10ms)
     - Integration tests with all L5 packages
   - Success Metric: Receipt chains span 3+ packages

5. **@unrdf/v6-compat** (L0 → L3)
   - Current: L0 (scaffolded, not implemented)
   - Gaps: L1-L3 (full implementation)
   - Deliverables:
     - Adapters for all 7 breaking changes (full implementation)
     - ESLint plugin with 5 rules (auto-fix where possible)
     - Schema generator (JSDoc → Zod)
     - Migration tracking (deprecation warnings)
     - 50+ tests covering all adapters
   - Success Metric: v5 code runs with zero changes using v6-compat

**Week 3 Tasks**:
- Day 1-2: Core - remove Date.now(), add receipt integration
- Day 3-4: KGC-4D - receipt composition implementation
- Day 5: v6-compat - adapters implementation (50% complete)

**Week 4 Tasks**:
- Day 1-2: v6-compat - ESLint plugin + schema generator
- Day 3: Integration tests (core + kgc-4d + v6-compat)
- Day 4-5: OTEL validation + fixes

**Proof Required** (End of Week 4):
```bash
# Package count at L5
ls -1 /home/user/unrdf/packages/{oxigraph,test-utils,core,kgc-4d}/package.json | wc -l
# Expected: 4 (v6-compat at L3 only)

# Deterministic test (core)
timeout 5s node packages/core/test/determinism.test.mjs > output1.json
timeout 5s node packages/core/test/determinism.test.mjs > output2.json
diff output1.json output2.json
# Expected: No differences

# Receipt chain test (kgc-4d)
timeout 10s node packages/kgc-4d/test/receipt-chain.test.mjs
# Expected: ✅ Receipt chain spans core, hooks, yawl

# v6-compat adapter test
timeout 5s node packages/v6-compat/test/adapters.test.mjs
# Expected: ✅ All 7 breaking changes have working adapters

# Test pass count
timeout 10s pnpm test --filter '@unrdf/{core,kgc-4d,v6-compat}'
# Expected: 100% pass rate (show counts)
```

---

### Week 5-6: Infrastructure Layer (Level 2)

**Goal**: 4 more packages at L5 (hooks, streaming, validation, observability) - cumulative 9 packages

**Packages**:
6. **@unrdf/hooks** (L2 → L5)
   - Current: L2 (stable contracts, partial L3)
   - Gaps: L3 (receipt enforcement), L4 (safety), L5 (integration)
   - Deliverables:
     - Mandatory Zod schemas for all hooks
     - Receipt generation for all hook executions
     - Timeout guards on hook evaluations (5s default)
     - Integration tests with core, yawl, federation

7. **@unrdf/streaming** (L2 → L5)
   - Current: L2 (partial L4)
   - Gaps: L3 (deterministic fixtures), L4 (complete), L5 (integration)
   - Deliverables:
     - EventEmitter → AsyncIterator migration (breaking change #7)
     - Receipt generation on stream completion
     - Deterministic replay from receipts
     - Integration tests with federation, hooks

8. **@unrdf/validation** (L2 → L5)
   - Current: L2 (partial)
   - Deliverables:
     - Zod schema validation for all validators
     - Receipt generation for validation results
     - Timeout guards on validation operations
     - Integration tests with hooks, core

9. **@unrdf/observability** (L2 → L5)
   - Current: L2 (partial)
   - Deliverables:
     - OTEL span integration for all packages
     - Receipt generation for trace exports
     - Integration tests with all L5 packages

**Week 5 Tasks**:
- Day 1-2: Hooks - Zod schemas + receipt enforcement
- Day 2-3: Streaming - AsyncIterator migration
- Day 4: Validation - schema validation
- Day 5: Observability - OTEL integration

**Week 6 Tasks**:
- Day 1-2: Integration tests (hooks + streaming + validation + observability)
- Day 3: Cross-package delta tests (4 packages together)
- Day 4-5: OTEL validation + performance benchmarks

**Proof Required** (End of Week 6):
```bash
# Package count at L5
ls -1 /home/user/unrdf/packages/{oxigraph,core,kgc-4d,hooks,streaming,validation,observability}/package.json | wc -l
# Expected: 7 (excluding test-utils L4, v6-compat L3)

# Cross-package test
timeout 15s node integration-tests/hooks-streaming-federation.test.mjs
# Expected: ✅ 3 packages working together

# Receipt chain verification (5 packages)
node validation/receipt-chain-verification.mjs --packages hooks,streaming,validation,observability,core
# Expected: ✅ Receipt chains verified across 5 packages

# Streaming API migration
grep -r "\.on('data'" packages/streaming/src --include="*.mjs" | wc -l
# Expected: 0 (all migrated to AsyncIterator)

# OTEL validation
node validation/run-all.mjs --packages hooks,streaming,validation,observability
# Expected: All ≥80/100
```

---

### Week 7-8: Orchestration Layer (Level 3)

**Goal**: 3 more packages at L5 (federation, yawl, cli) - cumulative 12 packages

**Packages**:
10. **@unrdf/federation** (L2 → L5)
    - Current: L2 (partial L4)
    - Gaps: L3 (determinism), L4 (complete), L5 (integration)
    - Deliverables:
      - Typed query builders (breaking change #6)
      - 5s default timeout on all queries
      - Receipt generation for federated queries
      - Integration tests with streaming, hooks, core

11. **@unrdf/yawl** (L3 → L5)
    - Current: L3 (deterministic workflows)
    - Gaps: L4 (safety), L5 (integration)
    - Deliverables:
      - Timeout guards on workflow steps
      - Receipt composition across workflow steps
      - Integration tests with hooks, kgc-4d, federation

12. **@unrdf/cli** (L2 → L5)
    - Current: L2 (stable contracts)
    - Gaps: L3 (receipts), L4 (safety), L5 (integration)
    - Deliverables:
      - Receipt emission for all mutating commands
      - Zod validation for all CLI inputs
      - Timeout guards on all operations
      - Integration with all L5 packages (CLI commands)

**Week 7 Tasks**:
- Day 1-2: Federation - typed query builders + timeouts
- Day 3-4: YAWL - timeout guards + receipt composition
- Day 5: CLI - Zod validation for inputs

**Week 8 Tasks**:
- Day 1-2: CLI - receipt emission for commands
- Day 3: Integration tests (federation + yawl + cli)
- Day 4-5: OTEL validation + performance benchmarks

**Proof Required** (End of Week 8):
```bash
# Package count at L5
ls -1 /home/user/unrdf/packages/{oxigraph,core,kgc-4d,hooks,streaming,validation,observability,federation,yawl,cli}/package.json | wc -l
# Expected: 10

# Federation query timeout test
timeout 6s node packages/federation/test/timeout.test.mjs
# Expected: Query times out at 5s, not hangs

# YAWL receipt chain
timeout 10s node packages/yawl/test/workflow-receipt-chain.test.mjs
# Expected: ✅ Workflow receipts chain across 5 steps

# CLI receipt emission
timeout 5s pnpm kgc delta create --file test-delta.json --json
# Expected: JSON output with receiptId field

# Test pass rate
timeout 20s pnpm test --filter '@unrdf/{federation,yawl,cli}'
# Expected: 100% pass (show counts)
```

---

### Week 9-10: Advanced Features (Level 4)

**Goal**: 1 more package at L5 (knowledge-engine) - cumulative 13 packages

**Package**:
13. **@unrdf/knowledge-engine** (L2 → L5)
    - Current: L2 (partial)
    - Gaps: L3-L5 (all)
    - Deliverables:
      - Zod schemas for all reasoning operations
      - Receipt generation for inference steps
      - Timeout guards on reasoning operations (10s default, configurable)
      - Integration tests with federation, yawl, hooks, streaming, core

**Week 9 Tasks**:
- Day 1-2: Knowledge-engine - Zod schemas + receipt integration
- Day 3-4: Timeout guards + deterministic fixtures
- Day 5: Integration tests (phase 1)

**Week 10 Tasks**:
- Day 1-3: Integration tests with 5+ packages
- Day 4: OTEL validation + fixes
- Day 5: Performance benchmarks

**Proof Required** (End of Week 10):
```bash
# Package count at L5
ls -1 /home/user/unrdf/packages/{oxigraph,core,kgc-4d,hooks,streaming,validation,observability,federation,yawl,cli,knowledge-engine}/package.json | wc -l
# Expected: 11

# Knowledge-engine integration test
timeout 30s node packages/knowledge-engine/test/cross-package-reasoning.test.mjs
# Expected: ✅ Reasoning across federation + yawl + hooks

# Receipt chain depth test
node validation/receipt-chain-depth.mjs --package knowledge-engine
# Expected: Receipt chain depth ≥5 (spans multiple packages)

# OTEL validation
node validation/run-all.mjs --package knowledge-engine
# Expected: ≥80/100
```

---

### Week 11-12: Integration & Release Preparation

**Goal**: All 13 packages validated, cross-package workflows proven, v6.0.0-core release ready

**Deliverables**:

#### Week 11: Cross-Package Workflows

**3 End-to-End Workflows**:

1. **Workflow 1: Federated Reasoning with Hooks**
   - Federation queries across multiple stores
   - Knowledge-engine applies reasoning
   - Hooks validate results
   - Streaming delivers results
   - Receipt chain proves entire flow
   - Test: `integration-tests/federated-reasoning-workflow.test.mjs`
   - Success: OTEL trace shows complete workflow, receipt chain verified

2. **Workflow 2: YAWL Multi-Step Pipeline**
   - YAWL orchestrates 5-step workflow
   - Step 1: Federation query
   - Step 2: Knowledge-engine reasoning
   - Step 3: Validation checks
   - Step 4: Streaming output
   - Step 5: CLI report generation
   - Test: `integration-tests/yawl-pipeline-workflow.test.mjs`
   - Success: All steps complete, receipts chain

3. **Workflow 3: CLI-Driven Migration**
   - CLI commands trigger v6-compat adapters
   - Migration from v5 to v6 APIs
   - Validation ensures correctness
   - Observability tracks progress
   - Receipt chain proves migration
   - Test: `integration-tests/cli-migration-workflow.test.mjs`
   - Success: v5 code runs on v6 with receipts

**Tasks**:
- Day 1-2: Implement Workflow 1
- Day 3: Implement Workflow 2
- Day 4-5: Implement Workflow 3

#### Week 12: Release Readiness

**14-Point Release Checklist**:

1. **All packages at L5**
   ```bash
   timeout 5s ./scripts/check-maturity-levels.sh
   # Expected: 13/13 packages at L5
   ```

2. **100% test pass rate**
   ```bash
   timeout 60s pnpm test
   # Expected: X/X tests passing (show count)
   ```

3. **OTEL validation ≥80/100**
   ```bash
   node validation/run-all.mjs comprehensive
   grep "Score:" validation-output.log
   # Expected: All 13 packages ≥80/100
   ```

4. **Zero direct N3 imports**
   ```bash
   timeout 5s grep -r "from 'n3'" packages/*/src --include="*.mjs" | grep -v n3-justified | wc -l
   # Expected: 0
   ```

5. **All operations produce receipts**
   ```bash
   timeout 10s ./scripts/check-receipt-coverage.sh
   # Expected: 100% receipt coverage for mutating ops
   ```

6. **Zod schemas 100% coverage**
   ```bash
   timeout 5s ./scripts/check-zod-coverage.sh
   # Expected: 100% public APIs have Zod schemas
   ```

7. **Timeout guards enforced**
   ```bash
   timeout 5s grep -r "timeout.*[0-9]" packages/*/src --include="*.mjs" | wc -l
   # Expected: >100 timeout guards
   ```

8. **No Date.now() in business logic**
   ```bash
   timeout 5s grep -r "Date\.now\|Math\.random" packages/*/src --include="*.mjs" | wc -l
   # Expected: 0
   ```

9. **Integration tests pass**
   ```bash
   timeout 60s pnpm test:integration
   # Expected: All integration tests passing
   ```

10. **Performance benchmarks pass**
    ```bash
    timeout 30s pnpm benchmark:compare
    # Expected: No regressions >10%
    ```

11. **Documentation updated**
    ```bash
    timeout 5s ls -1 docs/v6/*.md | wc -l
    # Expected: ≥8 v6 docs
    ```

12. **Migration guide tested**
    ```bash
    timeout 10s ./scripts/test-migration-guide.sh
    # Expected: External user can follow guide (manual verification)
    ```

13. **ESLint rules enforced**
    ```bash
    timeout 10s pnpm lint
    # Expected: 0 warnings
    ```

14. **Compatibility layer functional**
    ```bash
    timeout 10s node packages/v6-compat/test/v5-code-runs.test.mjs
    # Expected: ✅ v5 code runs with v6-compat
    ```

**Week 12 Tasks**:
- Day 1-2: Run full 14-point checklist, fix all failures
- Day 3: Changelog generation, release notes
- Day 4: External testing (3 users test migration guide)
- Day 5: Final validation, tag v6.0.0-core release

**Proof Required** (End of Week 12):
```bash
# All 14 checkpoints passing
timeout 60s ./scripts/run-release-checklist.sh
# Expected: 14/14 ✅

# Release tag
git tag -l v6.0.0-core
# Expected: v6.0.0-core

# Package versions
grep '"version"' packages/{oxigraph,core,kgc-4d,v6-compat,hooks,streaming,validation,observability,federation,yawl,cli,knowledge-engine}/package.json
# Expected: All show "6.0.0-core"

# Final OTEL validation
node validation/run-all.mjs comprehensive > final-validation.log
grep "Score:" final-validation.log
# Expected: 13 packages, all ≥80/100
```

---

## Risk Register & Mitigation

### High-Impact Risks

| Risk ID | Risk | Impact | Probability | Mitigation | Fallback |
|---------|------|--------|-------------|------------|----------|
| R1 | Critical package fails L5 verification | High | Medium | Weekly OTEL validation, early detection | v6-compat bridge for that package |
| R2 | Performance regresses >10% | Medium | Medium | Continuous benchmarking, optimization budget | Accept regression if <15%, optimize in v6.1 |
| R3 | Tests fail mid-migration | High | Medium | Triage + fix process, freeze other work | Rollback to last L4 state, investigate |
| R4 | Breaking changes break existing users | High | Low | v6-compat layer, extensive testing | Extend v5 support, delay v6 release |
| R5 | Cross-package integration failures | High | Medium | Daily integration test runs | Isolate failing package, fix dependencies |
| R6 | OTEL validation fails | Medium | Low | Weekly validation, fix incrementally | Lower threshold to 70/100 for v6.0.0-core |
| R7 | Dependency circular deadlock | High | Low | Madge checks daily, refactor if detected | Break circular dep with interface package |
| R8 | Team velocity slower than planned | Medium | High | 48% buffer built into plan | Reduce scope to 10 packages, defer 3 to v6.1 |
| R9 | External testing reveals blockers | High | Medium | Week 12 buffer for fixes | Delay release 1 week, fix issues |
| R10 | Documentation gaps | Low | High | Automated doc generation, review weekly | Ship with known gaps, fix in v6.0.1 |

### Risk Response Protocols

**When R1 (Critical package fails L5)**:
1. STOP work on dependent packages
2. Triage: Is it L4→L5 or earlier level?
3. If L4 issue: Fix with highest priority
4. If L5 issue: Deploy v6-compat bridge, mark as L4 for v6.0.0-core
5. Document known limitation in release notes

**When R3 (Tests fail mid-migration)**:
1. STOP all commits to that package
2. Run `git bisect` to find breaking commit
3. Rollback to last green state
4. Fix issue in isolated branch
5. Re-run full test suite before merging

**When R5 (Cross-package integration failures)**:
1. Isolate failing combination (e.g., federation + yawl)
2. Run integration test in isolation
3. Add debug logging to identify exact failure point
4. Fix in one package, verify in both
5. Update integration test to prevent regression

---

## Success Criteria (Testable)

### Quantitative Metrics (Week-by-Week)

| Week | Packages at L5 | Test Pass % | OTEL Avg Score | Cumulative Hours | Variance |
|------|----------------|-------------|----------------|------------------|----------|
| 2    | 2 / 13         | 100%        | ≥80/100        | 96h              | 0%       |
| 4    | 5 / 13         | 100%        | ≥80/100        | 240h             | <5%      |
| 6    | 9 / 13         | 100%        | ≥80/100        | 456h             | <10%     |
| 8    | 12 / 13        | 100%        | ≥80/100        | 628h             | <10%     |
| 10   | 13 / 13        | 100%        | ≥80/100        | 704h             | <10%     |
| 12   | 13 / 13        | 100%        | ≥85/100        | 752h             | <15%     |

**Acceptance Criteria**: Variance <15% (buffer allows for unknowns)

### Qualitative Metrics

| Metric | Baseline (v5) | Target (v6.0.0-core) | Measurement Method |
|--------|---------------|----------------------|--------------------|
| Onboarding time | 1 day | <1 hour | Time 3 external developers |
| Bug investigation time | 2 hours | 1 hour | Receipt chain reduces debugging by 50% |
| Migration complexity | High | Low | 3 users complete migration in <4 hours |
| Developer confidence | Medium | High | Survey: "Would you recommend v6?" >80% yes |

---

## Milestones with Proof

### Milestone 1: Foundation Complete (End of Week 2)

**Deliverable**: 2 packages at L5

**Proof**:
```bash
# 1. Package count
ls -1 /home/user/unrdf/packages/{oxigraph,test-utils}/package.json | wc -l
# Expected: 2

# 2. Tests passing
timeout 5s pnpm test --filter '@unrdf/{oxigraph,test-utils}'
# Expected: ✅ All tests passing (show count)

# 3. OTEL validation
node validation/run-all.mjs --packages oxigraph,test-utils
grep "Score:" validation-output.log
# Expected: Both ≥80/100

# 4. No regressions
timeout 10s pnpm benchmark:compare --package oxigraph
# Expected: No >10% regressions
```

**Verification Date**: Jan 20, 2025
**Sign-off**: Team lead review + CI green

---

### Milestone 2: Core Layer Complete (End of Week 4)

**Deliverable**: 5 packages at L5 (cumulative)

**Proof**:
```bash
# 1. Package count
ls -1 /home/user/unrdf/packages/{oxigraph,test-utils,core,kgc-4d}/package.json | wc -l
# Expected: 4 (v6-compat at L3)

# 2. Determinism test
timeout 5s node packages/core/test/determinism.test.mjs > /tmp/out1.json
timeout 5s node packages/core/test/determinism.test.mjs > /tmp/out2.json
diff /tmp/out1.json /tmp/out2.json
# Expected: No differences

# 3. Receipt chain test
timeout 10s node packages/kgc-4d/test/receipt-chain.test.mjs
# Expected: ✅ Receipt chain spans 3+ packages

# 4. v6-compat adapter test
timeout 5s node packages/v6-compat/test/v5-code-compatibility.test.mjs
# Expected: ✅ v5 code runs with adapters
```

**Verification Date**: Feb 3, 2025
**Sign-off**: Architecture review + external tester

---

### Milestone 3: Infrastructure Complete (End of Week 6)

**Deliverable**: 9 packages at L5 (cumulative)

**Proof**:
```bash
# 1. Package count at L5
ls -1 /home/user/unrdf/packages/{oxigraph,core,kgc-4d,hooks,streaming,validation,observability}/package.json | wc -l
# Expected: 7

# 2. Cross-package delta test
timeout 15s node integration-tests/cross-package-delta.test.mjs
# Expected: ✅ 5 packages working together

# 3. Receipt chain verification
node validation/receipt-chain-verification.mjs --depth 5
# Expected: ✅ Receipt chains span 5 packages

# 4. OTEL validation (all)
node validation/run-all.mjs comprehensive
grep "Score:" validation-output.log | awk '{sum+=$2; count++} END {print sum/count}'
# Expected: Average ≥80/100
```

**Verification Date**: Feb 17, 2025
**Sign-off**: Integration test review + performance baseline

---

### Milestone 4: Orchestration Complete (End of Week 8)

**Deliverable**: 12 packages at L5 (cumulative)

**Proof**:
```bash
# 1. Package count at L5
ls -1 /home/user/unrdf/packages/{oxigraph,core,kgc-4d,hooks,streaming,validation,observability,federation,yawl,cli}/package.json | wc -l
# Expected: 10

# 2. Federation timeout test
timeout 6s node packages/federation/test/query-timeout.test.mjs
# Expected: Query times out at 5s

# 3. YAWL workflow test
timeout 10s pnpm kgc workflow start --file test-workflow.yaml --json
# Expected: JSON output with receiptId

# 4. CLI integration test
timeout 10s ./integration-tests/cli-all-commands.sh
# Expected: All commands emit receipts
```

**Verification Date**: Mar 3, 2025
**Sign-off**: CLI review + workflow validation

---

### Milestone 5: Advanced Features Complete (End of Week 10)

**Deliverable**: 13 packages at L5 (cumulative)

**Proof**:
```bash
# 1. Package count at L5
ls -1 /home/user/unrdf/packages/{oxigraph,core,kgc-4d,hooks,streaming,validation,observability,federation,yawl,cli,knowledge-engine}/package.json | wc -l
# Expected: 11

# 2. Knowledge-engine integration
timeout 30s node packages/knowledge-engine/test/cross-package-reasoning.test.mjs
# Expected: ✅ Reasoning works across 5+ packages

# 3. Full test suite
timeout 60s pnpm test
# Expected: 100% pass rate (show total count)

# 4. OTEL comprehensive validation
node validation/run-all.mjs comprehensive
# Expected: All 13 packages ≥80/100
```

**Verification Date**: Mar 17, 2025
**Sign-off**: Full system review + external validation

---

### Milestone 6: Release Ready (End of Week 12)

**Deliverable**: v6.0.0-core release ready

**Proof**: 14-point checklist (see Week 12 section above)

**Verification Date**: Mar 31, 2025
**Sign-off**: Release review board + external testing committee

---

## Communication & Reporting

### Daily Standups (15 min)

**Format**:
- What did you COMPLETE yesterday? (show proof)
- What will you COMPLETE today? (specific deliverable)
- Blockers? (with mitigation plan)

**Red Flags**:
- "Almost done" → Not acceptable (define "done" with proof)
- "Tests mostly pass" → Not acceptable (must be 100%)
- "I think it works" → Not acceptable (show OTEL validation)

### Weekly Reports (Friday EOD)

**Template**:
```markdown
## Week X Report

### Packages at L5 This Week
- [Package name]: [Current maturity] → L5
- Proof: [Link to test output, OTEL validation]

### Blockers Resolved
- [Blocker]: [Resolution + prevention]

### Next Week Plan
- [Package]: [Specific deliverables]

### Risks Identified
- [Risk]: [Mitigation plan]

### Metrics
- Tests: X/Y passing (100% required)
- OTEL: X.X/100 average (≥80 required)
- Hours: X/Y used (variance %)
```

### Bi-Weekly Reviews (End of Week 2, 4, 6, 8, 10, 12)

**Agenda**:
1. Milestone verification (run proof scripts)
2. Risk register update
3. Velocity check (hours vs. plan)
4. Scope adjustment if needed (defer packages if >15% variance)

---

## Appendix A: Package Dependency Graph (ASCII)

```
Level 0 (Foundation)
====================
@unrdf/oxigraph
    │
    ├─────────────────────┐
    │                     │
Level 1 (Core)            │
====================      │
@unrdf/core               │
    │                     │
    ├──────┬──────┐       │
    │      │      │       │
@unrdf/    │      │       │
kgc-4d     │      │       │
    │      │      │       │
    │  @unrdf/   │        │
    │  v6-compat │        │
    │      │      │       │
    │      │      │       │
Level 2 (Infrastructure)  │
====================      │
@unrdf/hooks              │
    │                     │
    ├──────┬──────────────┼─────┐
    │      │              │     │
@unrdf/    │         @unrdf/    │
streaming  │         validation │
    │      │              │     │
    │  @unrdf/            │     │
    │  observability      │     │
    │      │              │     │
    │      │              │     │
Level 3 (Orchestration)   │     │
====================      │     │
@unrdf/federation ────────┘     │
    │                           │
    │                           │
@unrdf/yawl ───────────────────┘
    │
    │
@unrdf/cli
    │
    │
Level 4 (Advanced)
====================
@unrdf/knowledge-engine
    │
    │
Level 5 (Integration)
====================
All 13 packages (cross-package workflows)
```

---

## Appendix B: Effort Breakdown by Level

```
L2 → L3 (Deterministic + Replayable)
====================================
Package                Hours  Tasks
@unrdf/core            12h    Remove Date.now(), add fixtures
@unrdf/hooks           10h    Receipt enforcement, fixtures
@unrdf/streaming       10h    Deterministic replay
@unrdf/validation      8h     Receipt integration
@unrdf/observability   8h     Receipt integration
@unrdf/federation      10h    Deterministic queries
@unrdf/cli             8h     Receipt emission
@unrdf/knowledge-engine 12h   Deterministic reasoning
---------------------------------------------------
TOTAL                  78h

L3 → L4 (Adversarial Safety)
====================================
Package                Hours  Tasks
@unrdf/oxigraph        8h     Timeout guards, adversarial tests
@unrdf/core            10h    Timeout guards, Zod validation
@unrdf/hooks           10h    Timeout guards, safety
@unrdf/streaming       10h    Timeout guards, safety
@unrdf/validation      8h     Safety checks
@unrdf/observability   8h     Safety checks
@unrdf/federation      10h    Timeout guards, injection prevention
@unrdf/yawl            10h    Timeout guards
@unrdf/cli             8h     Input validation
@unrdf/knowledge-engine 10h   Timeout guards, safety
---------------------------------------------------
TOTAL                  92h

L4 → L5 (Cross-Package Composition)
====================================
Package                Hours  Tasks
@unrdf/oxigraph        12h    Integration tests (3 packages)
@unrdf/core            14h    Integration tests (5 packages)
@unrdf/kgc-4d          12h    Receipt composition
@unrdf/hooks           12h    Integration tests
@unrdf/streaming       14h    Integration tests
@unrdf/validation      10h    Integration tests
@unrdf/observability   10h    Integration tests
@unrdf/federation      14h    Integration tests
@unrdf/yawl            12h    Integration tests
@unrdf/cli             12h    Integration tests
@unrdf/knowledge-engine 16h   Integration tests (5+ packages)
---------------------------------------------------
TOTAL                  138h

v6-compat (L0 → L3)
====================================
Tasks                  Hours
Adapters implementation 24h
ESLint plugin          16h
Schema generator       12h
Tests (50+)            20h
Documentation          8h
---------------------------------------------------
TOTAL                  80h

test-utils (L1 → L4)
====================================
Tasks                  Hours
Zod schemas            8h
Deterministic fixtures 12h
Timeout guards         8h
Tests                  10h
Documentation          10h
---------------------------------------------------
TOTAL                  48h

Integration & Release (Weeks 11-12)
====================================
Tasks                  Hours
Cross-package workflows 40h
14-point checklist     20h
External testing       16h
Documentation          24h
Release preparation    20h
---------------------------------------------------
TOTAL                  120h

GRAND TOTAL            636h (vs. 752h planned, 116h buffer)
```

---

## Appendix C: Testing Strategy

### Test Pyramid (Per Package)

```
         ╱╲
        ╱  ╲
       ╱ E2E╲         5-10 tests   (cross-package workflows)
      ╱──────╲
     ╱ Integ. ╲       20-30 tests  (within package, multiple modules)
    ╱──────────╲
   ╱   Unit     ╲     100-200 tests (pure functions, single module)
  ╱──────────────╲
 ╱   Contract     ╲   50-100 tests  (Zod schema validation)
╱──────────────────╲
```

### Test Categories

| Category | Count/Package | Total (13 packages) | Purpose |
|----------|---------------|---------------------|---------|
| Contract | 50-100 | 650-1,300 | Zod schema validation |
| Unit | 100-200 | 1,300-2,600 | Pure function correctness |
| Integration | 20-30 | 260-390 | Module interaction |
| E2E | 5-10 | 65-130 | Cross-package workflows |
| **TOTAL** | **175-340** | **2,275-4,420** | Full coverage |

### Critical Test Scenarios

**Every package must have**:
1. Determinism test (run twice, compare outputs)
2. Receipt generation test
3. Timeout enforcement test (verify 5s timeout fires)
4. Invalid input test (100+ cases)
5. Integration test with ≥2 other L5 packages

---

## Appendix D: OTEL Validation Criteria

### Validation Scoring (80/100 minimum)

| Criterion | Weight | Pass Criteria |
|-----------|--------|---------------|
| Span completeness | 20 | All operations create spans |
| Span attributes | 15 | Required attributes present |
| Error handling | 15 | Errors captured in spans |
| Receipt correlation | 15 | Receipts linked to spans |
| Trace continuity | 15 | Parent-child relationships correct |
| Performance | 10 | Span overhead <5% |
| Documentation | 10 | Span semantics documented |

### Example Validation Output

```
Package: @unrdf/federation
━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
✅ Span completeness:     20/20
✅ Span attributes:       15/15
✅ Error handling:        14/15
✅ Receipt correlation:   15/15
✅ Trace continuity:      15/15
✅ Performance:           9/10
✅ Documentation:         8/10
━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
Score: 96/100 ✅ PASS
```

---

## Appendix E: References

- **Migration Plan**: `/home/user/unrdf/docs/v6/MIGRATION_PLAN.md`
- **Maturity Ladder**: `/home/user/unrdf/docs/v6/MATURITY_LADDER.md`
- **Capsule Backlog**: `/home/user/unrdf/docs/v6/CAPSULE_BACKLOG.md`
- **Program Charter**: `/home/user/unrdf/docs/v6/PROGRAM_CHARTER.md`
- **BB80/20 Methodology**: `/home/user/unrdf/docs/bb80-20-methodology.md`
- **CLAUDE.md**: `/home/user/unrdf/CLAUDE.md`

---

**END OF 12-WEEK FAST-TRACK PLAN**

**Final Adversarial Question**: *If an external auditor reviewed this plan, could every claim be verified with concrete evidence?*

**Answer**: YES. Every milestone has proof requirements. Every claim has a verification command. Every success criterion is measurable.

**Trust**: OTEL spans + test output + metrics = ONLY validation.
