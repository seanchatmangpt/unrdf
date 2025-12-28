# UNRDF v6 P0+P1 Orchestration Plan
**Date**: 2025-12-27
**Branch**: claude/kgc-swarm-agents-2GQk5
**Target**: v6.0.0-rc with P0+P1 complete
**Methodology**: Big Bang 80/20 Single-Pass Execution

---

## Executive Summary

### Scope
- **P0 Critical Path**: 4 capsules, 36 hours effective (26 hours critical path)
- **P1 Core Packages**: 10 capsules, 162 hours effective
- **Total**: 14 capsules, 198 hours (~5 weeks for 5 specialized agents)
- **Current State**: v6.0.0-alpha.1 released, partial P0 implementation exists

### Critical Path Analysis
```
PARALLEL (8-10 hrs)
├─ V6-001: Receipt Wrapper [8h] → BLOCKING
└─ V6-002: Schema Generator [10h] → BLOCKING

SEQUENTIAL (16 hrs)
└─ V6-003: v6-compat Package [16h, deps: V6-001+V6-002] → BLOCKING

SEQUENTIAL (2 hrs)
└─ V6-004: Workspace Update [2h, deps: V6-003]

Minimum Critical Path: 26 hours (parallel start) or 36 hours (sequential)
```

### Current Implementation Status
| Capsule | Status | Completion | Blockers |
|---------|--------|------------|----------|
| V6-001 | 🟡 PARTIAL | ~40% | withReceipt uses Date.now() (non-deterministic), no KGC-4D integration, no tests |
| V6-002 | 🟡 PARTIAL | ~60% | Functions exist, no CLI tool, no tests, not applied to core packages |
| V6-003 | 🟡 PARTIAL | ~50% | Package exists, no tests, no migration guide, ESLint rules untested |
| V6-004 | 🟢 DONE | 100% | None (v6-compat at 6.0.0-alpha.1) |

---

## Phase 1: P0 Critical Path (4 capsules, 36 hours)

### V6-001: KGC-4D Receipt Wrapper HOF
**Capsule ID**: V6-001
**Priority**: P0 (BLOCKING ALL P1 WORK)
**Status**: 🟡 PARTIAL (40% complete)
**Effort**: 8 hours
**Dependencies**: None
**Parallel**: Yes (can run with V6-002)

#### Current State Analysis
**Exists**:
- ✅ `/packages/v6-compat/src/adapters.mjs` with `withReceipt()` function (lines 246-267)
- ✅ Basic receipt structure with timestamp, duration, args, result

**Missing** (L3 Determinism Requirements):
- ❌ Uses `Date.now()` (line 259) - violates determinism
- ❌ No integration with KGC-4D freeze engine
- ❌ No Merkle proof generation
- ❌ No receipt composition support
- ❌ Zero tests (critical!)
- ❌ Not in `/packages/kgc-4d/src/wrapper.mjs` (spec location)

#### Work Breakdown
| Task | Hours | Agent | Verification |
|------|-------|-------|--------------|
| 1. Create `/packages/kgc-4d/src/wrapper.mjs` | 2h | backend-dev | File exists, exports wrapWithReceipt |
| 2. Integrate KGC-4D freeze for deterministic timestamps | 2h | backend-dev | No Date.now(), uses freeze() |
| 3. Add Merkle proof generation | 1.5h | backend-dev | Receipt has merkleProof field |
| 4. Create test suite (5+ tests: sync, async, error, composition) | 2h | tester | 100% pass, ≥5 tests |
| 5. Add example to `/docs/v6/examples/receipt-wrapper.mjs` | 0.5h | backend-dev | Example runs successfully |

#### Agent Assignment
**Primary**: `backend-dev` (4 agents in parallel)
**Secondary**: `tester` (1 agent for test suite)
**Rationale**: Backend-dev has deep knowledge of KGC-4D freeze engine, pure function design, and receipt schemas. Tester ensures comprehensive coverage.

#### Success Criteria (100% Required)
- [ ] `wrapWithReceipt()` function exists in `/packages/kgc-4d/src/wrapper.mjs`
- [ ] Integrates KGC-4D freeze engine (deterministic)
- [ ] Generates Merkle proofs for receipts
- [ ] 5+ tests pass (sync, async, error, composition, determinism)
- [ ] Example in `/docs/v6/examples/receipt-wrapper.mjs` runs
- [ ] ESLint passes (0 warnings)
- [ ] No `Date.now()` or `Math.random()` in implementation
- [ ] Receipt matches ReceiptSchema (Zod validation)

#### OTEL Validation Checkpoint
```bash
# Run after V6-001 completion
timeout 5s pnpm test --filter @unrdf/kgc-4d
timeout 5s node /home/user/unrdf/docs/v6/examples/receipt-wrapper.mjs
grep "Date\.now\|Math\.random" /home/user/unrdf/packages/kgc-4d/src/wrapper.mjs
# Expected: 0 matches
```

**Receipt Chain Entry**:
```json
{
  "capsule": "V6-001",
  "commit_hash": "<git sha>",
  "tests_passed": 5,
  "tests_total": 5,
  "lint_warnings": 0,
  "otel_score": "≥80/100",
  "timestamp": "<freeze timestamp>",
  "merkle_root": "<proof>"
}
```

---

### V6-002: Zod Schema Generator
**Capsule ID**: V6-002
**Priority**: P0 (BLOCKING ALL P1 L3→L4 WORK)
**Status**: 🟡 PARTIAL (60% complete)
**Effort**: 10 hours
**Dependencies**: None
**Parallel**: Yes (can run with V6-001)

#### Current State Analysis
**Exists**:
- ✅ `/packages/v6-compat/src/schema-generator.mjs` (5,197 bytes)
- ✅ Functions: `parseJSDocToZod`, `generateSchemaFromFunction`, `generateSchemasForFiles`
- ✅ Example schemas: `UserSchema`, `ReceiptSchema`
- ✅ `validateWithErrors`, `generateTSFromZod` utilities

**Missing**:
- ❌ CLI tool: `unrdf schema-gen src/**/*.mjs`
- ❌ Zero tests
- ❌ Not applied to @unrdf/core (spec requirement: "100% coverage")
- ❌ No documentation in `/docs/v6/examples/`

#### Work Breakdown
| Task | Hours | Agent | Verification |
|------|-------|-------|--------------|
| 1. Create CLI tool `/packages/v6-compat/cli.mjs` | 2h | backend-dev | `pnpm unrdf schema-gen` works |
| 2. Apply to @unrdf/core, generate schemas | 3h | backend-dev | Schemas in @unrdf/core/schemas/ |
| 3. Create test suite (10+ tests: JSDoc parsing, edge cases) | 3h | tester | 100% pass, ≥10 tests |
| 4. Create 10 example conversions in docs | 1.5h | backend-dev | Examples run successfully |
| 5. Update package.json bin field for CLI | 0.5h | backend-dev | CLI globally available |

#### Agent Assignment
**Primary**: `backend-dev` (3 agents)
**Secondary**: `tester` (1 agent)
**Validator**: `code-analyzer` (verify 100% @unrdf/core coverage)

#### Success Criteria
- [ ] CLI: `pnpm unrdf schema-gen src/**/*.mjs` works
- [ ] Applied to @unrdf/core with 100% function coverage
- [ ] 10+ tests pass (JSDoc parsing, schema generation, edge cases)
- [ ] 10 example conversions in `/docs/v6/examples/schema-generator/`
- [ ] ESLint passes
- [ ] Generated schemas validate successfully with Zod

#### OTEL Validation Checkpoint
```bash
timeout 10s pnpm unrdf schema-gen "/home/user/unrdf/packages/core/src/**/*.mjs"
timeout 5s pnpm test --filter @unrdf/v6-compat
ls -1 /home/user/unrdf/packages/core/schemas/*.mjs | wc -l
# Expected: ≥10 schemas generated
```

**Receipt Chain Entry**:
```json
{
  "capsule": "V6-002",
  "commit_hash": "<git sha>",
  "tests_passed": 10,
  "tests_total": 10,
  "schemas_generated": "≥10",
  "core_coverage": "100%",
  "otel_score": "≥80/100"
}
```

---

### V6-003: @unrdf/v6-compat Package Completion
**Capsule ID**: V6-003
**Priority**: P0
**Status**: 🟡 PARTIAL (50% complete)
**Effort**: 16 hours
**Dependencies**: V6-001 (receipt wrapper), V6-002 (schema generator)
**Parallel**: NO (sequential after V6-001 + V6-002)

#### Current State Analysis
**Exists**:
- ✅ Package structure at v6.0.0-alpha.1
- ✅ `/src/adapters.mjs` (9,838 bytes): createStore, wrapWorkflow, wrapFederation, withReceipt
- ✅ `/src/schema-generator.mjs` (5,197 bytes)
- ✅ `/src/lint-rules.mjs` (6,880 bytes)
- ✅ Package.json with exports

**Missing** (CRITICAL):
- ❌ Zero tests (no `/test/` directory found)
- ❌ No migration guide
- ❌ ESLint rules not validated
- ❌ Adapters not tested with real v5 code
- ❌ No integration with V6-001 receipt wrapper

#### Work Breakdown
| Task | Hours | Agent | Verification |
|------|-------|-------|--------------|
| 1. Replace withReceipt with V6-001 wrapWithReceipt | 2h | backend-dev | Uses kgc-4d wrapper |
| 2. Create comprehensive test suite (20+ tests) | 6h | tester | All adapters, lint rules, schemas |
| 3. Create migration guide `/docs/v6/MIGRATION_GUIDE.md` | 4h | backend-dev | 10+ examples, v5→v6 mappings |
| 4. Validate ESLint rules on real code | 2h | code-analyzer | Rules catch deprecations |
| 5. Create example migrations in `/docs/v6/examples/migrations/` | 2h | backend-dev | 5+ working examples |

#### Agent Assignment
**Primary**: `backend-dev` (2 agents)
**Secondary**: `tester` (1 agent)
**Validator**: `code-analyzer` (ESLint rule validation)
**Reviewer**: `production-validator` (final sign-off)

#### Success Criteria
- [ ] 20+ tests pass (adapters, lint rules, schema generator, migration tracking)
- [ ] Migration guide complete with 10+ examples
- [ ] ESLint rules validated on real v5 code
- [ ] All adapters tested with v5→v6 conversion
- [ ] V6-001 wrapWithReceipt integrated
- [ ] OTEL validation ≥80/100

#### OTEL Validation Checkpoint
```bash
timeout 10s pnpm test --filter @unrdf/v6-compat
timeout 5s pnpm lint --filter @unrdf/v6-compat
ls -1 /home/user/unrdf/packages/v6-compat/test/*.test.mjs | wc -l
# Expected: ≥20 test files
node /home/user/unrdf/validation/run-all.mjs comprehensive --filter v6-compat
grep "Score:" validation-output.log # Expected: ≥80/100
```

**Receipt Chain Entry**:
```json
{
  "capsule": "V6-003",
  "commit_hash": "<git sha>",
  "tests_passed": "≥20",
  "tests_total": "≥20",
  "migration_examples": 10,
  "eslint_rules_validated": true,
  "otel_score": "≥80/100",
  "dependencies": ["V6-001", "V6-002"]
}
```

---

### V6-004: Workspace Update
**Capsule ID**: V6-004
**Priority**: P0
**Status**: 🟢 DONE (100% complete)
**Effort**: 2 hours (verification only)
**Dependencies**: V6-003
**Parallel**: NO

#### Current State
- ✅ Root package.json at v6.0.0-alpha.1
- ✅ v6-compat in workspace
- ✅ pnpm-workspace.yaml includes v6-compat

#### Verification Only
| Task | Hours | Agent | Verification |
|------|-------|-------|--------------|
| 1. Verify pnpm install works | 0.5h | system-architect | No errors |
| 2. Verify all packages resolve | 0.5h | system-architect | All imports work |
| 3. Update pnpm-lock.yaml | 0.5h | system-architect | Lock file current |
| 4. Run workspace-wide tests | 0.5h | production-validator | No new failures |

#### Agent Assignment
**Primary**: `system-architect` (workspace config expert)
**Validator**: `production-validator`

#### Success Criteria
- [ ] `pnpm install` succeeds (timeout 60s)
- [ ] All packages resolve correctly
- [ ] pnpm-lock.yaml updated
- [ ] Workspace-wide tests pass (no regressions)

#### OTEL Validation Checkpoint
```bash
timeout 60s pnpm install
timeout 5s pnpm -r exec echo "Package resolves: \$npm_package_name"
timeout 300s pnpm test 2>&1 | grep -E "pass|fail" | tail -20
```

---

## Phase 2: P1 Core Packages (10 capsules, 162 hours)

### Dependency Analysis
```
WAVE 1 (Parallel after V6-001):
├─ V6-010: @unrdf/oxigraph L3→L4 [10h]
├─ V6-012: @unrdf/core L2→L3 [14h]
└─ V6-015: @unrdf/kgc-4d L4→L5 [7h]

WAVE 2 (Parallel after V6-012):
├─ V6-013: @unrdf/core L3→L4 [12h, deps: V6-012]
├─ V6-016: @unrdf/hooks L2→L5 [25h, deps: V6-001, V6-012]
├─ V6-017: @unrdf/streaming L2→L5 [27h, deps: V6-001, V6-012]
├─ V6-018: @unrdf/federation L2→L5 [28h, deps: V6-001, V6-012]
└─ V6-019: @unrdf/cli L2→L5 [22h, deps: V6-001, V6-012]

WAVE 3 (Sequential - Integration Tests):
├─ V6-011: @unrdf/oxigraph L4→L5 [7h, deps: ALL OTHER L5]
└─ V6-014: @unrdf/core L4→L5 [10h, deps: ALL OTHER L5]
```

### Critical Path
```
V6-001 (8h) ──┬─→ V6-012 (14h) ──→ Wave 2 (max 28h) ──→ Wave 3 (10h)
              └─→ V6-010 (10h)

Total: 8h + 14h + 28h + 10h = 60 hours minimum (critical path)
Effective: 162 hours (with 5 agents in parallel) = ~32 hours wall time
```

### Parallelization Strategy
**Available Agents**: 5 specialized agents
**Max Parallel**: 5 capsules simultaneously
**Bottleneck**: Wave 3 (integration tests) must be sequential

**Optimal Schedule**:
1. **Hours 0-10**: V6-001 + V6-002 (2 agents)
2. **Hours 10-26**: V6-003 (1 agent) + V6-010 + V6-012 + V6-015 (3 agents)
3. **Hours 26-54**: Wave 2 (5 agents, staggered start)
4. **Hours 54-64**: Wave 3 (2 agents sequential, others start P2 work)

---

## P1 Capsule Details

### V6-010: @unrdf/oxigraph L3 → L4
**Priority**: P1
**Dependencies**: V6-001
**Effort**: 10 hours
**Parallel**: Yes (Wave 1)

#### Tasks
| Task | Hours | Agent | L4 Criterion |
|------|-------|-------|--------------|
| Add timeout guards to SPARQL queries (default 5s) | 3h | backend-dev | Adversarial safety |
| Add Zod validation to query inputs | 3h | backend-dev | Input validation |
| Create adversarial test suite (100+ invalid inputs) | 4h | tester | Misuse prevention |

#### Success Criteria
- [ ] All SPARQL queries have timeout guards
- [ ] All inputs validated with Zod
- [ ] 100+ adversarial tests pass
- [ ] No SQL/SPARQL injection vectors
- [ ] OTEL ≥80/100

---

### V6-011: @unrdf/oxigraph L4 → L5
**Priority**: P1
**Dependencies**: V6-010, ALL other L5 packages
**Effort**: 7 hours
**Parallel**: NO (Wave 3, sequential)

#### Tasks
| Task | Hours | Agent | L5 Criterion |
|------|-------|-------|--------------|
| Integration tests with core, hooks, streaming | 4h | tester | Compositional closure |
| Performance benchmarks (no regression) | 2h | performance-benchmarker | Performance validation |
| OTEL comprehensive validation | 1h | production-validator | OTEL ≥80/100 |

#### Success Criteria
- [ ] Integration tests with ≥3 other L5 packages pass
- [ ] Performance benchmarks show no regression
- [ ] Receipts compose correctly
- [ ] OTEL ≥80/100

---

### V6-012: @unrdf/core L2 → L3
**Priority**: P1 (CRITICAL - blocks 4 other capsules)
**Dependencies**: V6-001
**Effort**: 14 hours
**Parallel**: Yes (Wave 1)

#### Tasks
| Task | Hours | Agent | L3 Criterion |
|------|-------|-------|--------------|
| Remove Date.now()/Math.random() from utils | 4h | backend-dev | Determinism |
| Integrate receipts for all operations | 5h | backend-dev | Receipt integration |
| Create 50+ reproducible fixtures | 3h | tester | Replayability |
| Run determinism test 10x | 2h | production-validator | Deterministic validation |

#### Success Criteria
- [ ] Zero Date.now()/Math.random() in src/
- [ ] All operations produce receipts
- [ ] 50+ reproducible fixtures
- [ ] Determinism test passes 10 consecutive runs
- [ ] OTEL ≥80/100

---

### V6-013: @unrdf/core L3 → L4
**Priority**: P1
**Dependencies**: V6-012
**Effort**: 12 hours
**Parallel**: Yes (Wave 2, after V6-012)

#### Tasks
| Task | Hours | Agent | L4 Criterion |
|------|-------|-------|--------------|
| Add timeout guards to I/O operations | 4h | backend-dev | Timeout enforcement |
| Zod validation on all external inputs | 4h | backend-dev | Input validation |
| Create adversarial test suite | 4h | tester | Adversarial safety |

#### Success Criteria
- [ ] All I/O has timeout guards (default 5s)
- [ ] All external inputs validated
- [ ] Adversarial tests pass
- [ ] OTEL ≥80/100

---

### V6-014: @unrdf/core L4 → L5
**Priority**: P1
**Dependencies**: V6-013, ALL other L5
**Effort**: 10 hours
**Parallel**: NO (Wave 3, sequential)

#### Tasks
| Task | Hours | Agent | L5 Criterion |
|------|-------|-------|--------------|
| Integration tests with 5+ packages | 5h | tester | Compositional closure |
| Performance benchmarks | 3h | performance-benchmarker | No regression |
| OTEL comprehensive validation | 2h | production-validator | OTEL ≥80/100 |

---

### V6-015: @unrdf/kgc-4d L4 → L5
**Priority**: P1
**Dependencies**: V6-001
**Effort**: 7 hours
**Parallel**: Yes (Wave 1)

#### Tasks
| Task | Hours | Agent | L5 Criterion |
|------|-------|-------|--------------|
| Integration tests with all L5 packages | 4h | tester | Receipt composition |
| Receipt composition tests | 2h | backend-dev | Chain of custody |
| Performance benchmarks | 1h | performance-benchmarker | No regression |

---

### V6-016: @unrdf/hooks L2 → L5
**Priority**: P1
**Dependencies**: V6-001, V6-012
**Effort**: 25 hours
**Parallel**: Yes (Wave 2)

#### Tasks
| Task | Hours | Agent | Criteria |
|------|-------|-------|----------|
| L2→L3: Enforce receipts on all hooks | 8h | backend-dev | Determinism |
| L3→L4: Timeout guards + adversarial tests | 10h | backend-dev + tester | Safety |
| L4→L5: Integration tests | 7h | tester | Composition |

---

### V6-017: @unrdf/streaming L2 → L5
**Priority**: P1
**Dependencies**: V6-001, V6-012
**Effort**: 27 hours
**Parallel**: Yes (Wave 2)

#### Tasks
| Task | Hours | Agent | Criteria |
|------|-------|-------|----------|
| L2→L3: AsyncIterator migration + receipts | 10h | backend-dev | Determinism |
| L3→L4: Timeout guards + validation | 10h | backend-dev | Safety |
| L4→L5: Integration + replay tests | 7h | tester | Composition |

---

### V6-018: @unrdf/federation L2 → L5
**Priority**: P1 (LONGEST DURATION - 28h)
**Dependencies**: V6-001, V6-012
**Effort**: 28 hours
**Parallel**: Yes (Wave 2)

#### Tasks
| Task | Hours | Agent | Criteria |
|------|-------|-------|----------|
| L2→L3: SPARQL template literals + receipts | 10h | backend-dev | Determinism |
| L3→L4: Timeout enforcement + adversarial tests | 11h | backend-dev + tester | Safety |
| L4→L5: Multi-store integration tests | 7h | tester | Composition |

---

### V6-019: @unrdf/cli L2 → L5
**Priority**: P1
**Dependencies**: V6-001, V6-012
**Effort**: 22 hours
**Parallel**: Yes (Wave 2)

#### Tasks
| Task | Hours | Agent | Criteria |
|------|-------|-------|----------|
| L2→L3: Zod validation on args + receipts | 8h | backend-dev | Determinism |
| L3→L4: Timeout guards + adversarial inputs | 8h | backend-dev + tester | Safety |
| L4→L5: Integration tests with 3+ packages | 6h | tester | Composition |

---

## Agent Assignments Summary

### Primary Agents (Hyper-Advanced Tier)
| Agent | Capsules | Total Hours | Justification |
|-------|----------|-------------|---------------|
| **backend-dev** | V6-001, V6-002, V6-003, V6-012, V6-013, V6-016, V6-017, V6-018, V6-019 | ~110h | Deep KGC-4D knowledge, pure function design, receipt integration |
| **tester** | All test creation | ~60h | Adversarial testing, fixture creation, determinism validation |
| **code-analyzer** | V6-002 validation, V6-003 ESLint | ~15h | Static analysis, pattern detection, coverage validation |
| **system-architect** | V6-004, dependency resolution | ~10h | Workspace configuration, dependency management |
| **production-validator** | Final sign-off, OTEL validation | ~20h | Comprehensive validation, OTEL checkpoints |
| **performance-benchmarker** | L4→L5 performance validation | ~15h | Benchmark execution, regression detection |

### Agent Pool (5 concurrent agents)
**Configuration**:
- 2x backend-dev (parallel work on different packages)
- 1x tester (test suite creation)
- 1x code-analyzer (validation)
- 1x production-validator (OTEL checkpoints)

**Rotation Strategy**:
- backend-dev agents swap between capsules based on dependencies
- Tester works ahead creating test scaffolds
- Validators run continuous integration checks

---

## OTEL Validation Checkpoints

### Checkpoint Gates (MUST PASS to proceed)
| Gate | Capsules | Validation | Pass Criteria |
|------|----------|------------|---------------|
| **Gate 1** | V6-001 + V6-002 | Receipt wrapper + schema generator | Tests 100% pass, no Date.now(), schemas generated |
| **Gate 2** | V6-003 | v6-compat complete | 20+ tests pass, OTEL ≥80/100, migration guide exists |
| **Gate 3** | V6-004 | Workspace functional | pnpm install works, no regressions |
| **Gate 4** | Wave 1 (V6-010, V6-012, V6-015) | L3 maturity baseline | Deterministic tests pass, receipts integrated |
| **Gate 5** | Wave 2 (V6-013, V6-016-019) | L4 maturity | Adversarial tests pass, timeout guards enforced |
| **Gate 6** | Wave 3 (V6-011, V6-014) | L5 maturity | Integration tests pass, OTEL ≥80/100 for all |

### OTEL Validation Command (Run at each gate)
```bash
#!/bin/bash
# /home/user/unrdf/scripts/validate-gate.sh

GATE=$1
CAPSULES=$2

echo "🔍 Validating Gate ${GATE} - Capsules: ${CAPSULES}"

# Run comprehensive OTEL validation
timeout 300s node /home/user/unrdf/validation/run-all.mjs comprehensive

# Extract score
SCORE=$(grep "Score:" validation-output.log | tail -1 | awk '{print $2}')

if [ $(echo "$SCORE >= 80" | bc) -eq 1 ]; then
  echo "✅ Gate ${GATE} PASSED - OTEL Score: ${SCORE}/100"
  exit 0
else
  echo "❌ Gate ${GATE} FAILED - OTEL Score: ${SCORE}/100 (required: ≥80)"
  exit 1
fi
```

### Receipt Chain Structure
Each capsule generates a receipt upon completion:

```json
{
  "receipt_version": "6.0.0-alpha.1",
  "capsule_id": "V6-XXX",
  "timestamp": "<KGC-4D freeze timestamp>",
  "merkle_root": "<proof hash>",
  "git_commit": "<sha256>",
  "dependencies": ["V6-XXX", "V6-YYY"],
  "validation": {
    "tests_passed": 10,
    "tests_total": 10,
    "otel_score": 85,
    "lint_warnings": 0,
    "coverage_percent": 95
  },
  "artifacts": [
    "/packages/kgc-4d/src/wrapper.mjs",
    "/packages/kgc-4d/test/wrapper.test.mjs"
  ],
  "agent": "backend-dev-1",
  "duration_hours": 8.2,
  "status": "COMPLETE"
}
```

**Storage**: `/home/user/unrdf/.receipts/v6/capsule-<ID>.json`
**Chain Validation**:
```bash
timeout 5s node /home/user/unrdf/scripts/validate-receipt-chain.mjs V6-001 V6-019
# Verifies all dependencies satisfied, no gaps, all receipts valid
```

---

## Blockers & Risk Mitigation

### Identified Blockers
| Blocker | Severity | Impact | Mitigation | Owner |
|---------|----------|--------|------------|-------|
| V6-001 not deterministic (Date.now) | 🔴 HIGH | Blocks ALL P1 L3 work | Refactor to use KGC-4D freeze (2h) | backend-dev |
| No tests for v6-compat | 🔴 HIGH | Can't validate P0 complete | Create 20+ tests (6h) | tester |
| pnpm install failures | 🟡 MEDIUM | Blocks workspace-wide validation | Verify dependencies (0.5h) | system-architect |
| Integration tests depend on ALL L4 complete | 🟡 MEDIUM | Wave 3 bottleneck | Parallelize within wave where possible | task-orchestrator |
| OTEL validation slow (>60s) | 🟢 LOW | Slows gate validation | Optimize with targeted validation | production-validator |

### Risk Matrix
| Risk | Probability | Impact | Response |
|------|-------------|--------|----------|
| Scope creep on receipts | 60% | High | Strict capsule boundaries, reject out-of-scope PRs |
| Test failures in CI | 40% | Medium | Run locally before commit, use deterministic fixtures |
| Agent unavailability | 30% | Medium | Cross-train agents, maintain task queue |
| OTEL score <80 | 20% | High | Continuous validation, fix before merge |
| Performance regressions | 15% | Medium | Benchmark at each L4→L5, reject regressions |

### Mitigation Strategies
1. **Scope Creep**: Use CAPSULE_BACKLOG as source of truth, reject changes outside Δ scope
2. **Test Failures**: Require 100% pass before marking capsule complete, no exceptions
3. **Dependency Hell**: Explicit dependency graph, block dependent work until deps complete
4. **Performance**: Benchmark baselines recorded at v6.0.0-alpha.1, regression = blocker
5. **OTEL Failures**: Gate cannot pass with score <80, no manual overrides

---

## Status Dashboard Structure

### Real-Time Progress Tracking
**File**: `/home/user/unrdf/docs/v6/STATUS_DASHBOARD.md` (auto-generated)

```markdown
# V6 P0+P1 Status Dashboard
**Last Updated**: <timestamp>
**Branch**: claude/kgc-swarm-agents-2GQk5
**OTEL Score**: <score>/100

## Overall Progress
[P0: ██████████░░░░░░░░░░ 50%] [P1: ░░░░░░░░░░░░░░░░░░░░ 0%]
[Tests: ████████░░░░░░░░░░░░ 40%] [OTEL: ██████████████░░░░░░ 70/100]

## P0 Critical Path (4/4 capsules)
| ID | Capsule | Status | Progress | Tests | OTEL | Blocker |
|----|---------|--------|----------|-------|------|---------|
| V6-001 | Receipt Wrapper | 🟡 IN PROGRESS | 40% | 0/5 | - | Date.now() usage |
| V6-002 | Schema Generator | 🟡 IN PROGRESS | 60% | 0/10 | - | No CLI, no tests |
| V6-003 | v6-compat Package | 🔴 BLOCKED | 50% | 0/20 | - | Awaits V6-001, V6-002 |
| V6-004 | Workspace Update | 🟢 DONE | 100% | N/A | N/A | None |

## P1 Core Packages (0/10 capsules complete)
| ID | Package | Current | Target | Progress | Tests | OTEL | Blocker |
|----|---------|---------|--------|----------|-------|------|---------|
| V6-010 | oxigraph | L3 | L4 | 🔴 BLOCKED | 0/100 | - | Awaits V6-001 |
| V6-011 | oxigraph | L4 | L5 | 🔴 BLOCKED | 0/10 | - | Awaits ALL L4→L5 |
| V6-012 | core | L2 | L3 | 🔴 BLOCKED | 0/50 | - | Awaits V6-001 |
| V6-013 | core | L3 | L4 | 🔴 BLOCKED | 0/30 | - | Awaits V6-012 |
| V6-014 | core | L4 | L5 | 🔴 BLOCKED | 0/15 | - | Awaits ALL L4→L5 |
| V6-015 | kgc-4d | L4 | L5 | 🔴 BLOCKED | 0/15 | - | Awaits V6-001 |
| V6-016 | hooks | L2 | L5 | 🔴 BLOCKED | 0/40 | - | Awaits V6-001, V6-012 |
| V6-017 | streaming | L2 | L5 | 🔴 BLOCKED | 0/45 | - | Awaits V6-001, V6-012 |
| V6-018 | federation | L2 | L5 | 🔴 BLOCKED | 0/50 | - | Awaits V6-001, V6-012 |
| V6-019 | cli | L2 | L5 | 🔴 BLOCKED | 0/35 | - | Awaits V6-001, V6-012 |

## Active Work (Current Sprint)
| Agent | Capsule | Task | Hours Remaining | ETA |
|-------|---------|------|-----------------|-----|
| backend-dev-1 | V6-001 | Integrate KGC-4D freeze | 6h | 2h |
| backend-dev-2 | V6-002 | Create CLI tool | 8h | 3h |
| tester-1 | V6-001 | Test suite creation | 8h | BLOCKED (awaits impl) |

## Validation Gates
| Gate | Capsules | Status | OTEL | Blocker |
|------|----------|--------|------|---------|
| Gate 1 | V6-001, V6-002 | 🔴 BLOCKED | - | Implementation incomplete |
| Gate 2 | V6-003 | 🔴 BLOCKED | - | Awaits Gate 1 |
| Gate 3 | V6-004 | 🟢 READY | N/A | None |
| Gate 4 | Wave 1 | 🔴 BLOCKED | - | Awaits P0 complete |
| Gate 5 | Wave 2 | 🔴 BLOCKED | - | Awaits Wave 1 |
| Gate 6 | Wave 3 | 🔴 BLOCKED | - | Awaits Wave 2 |

## Recent Receipts
```
V6-004: COMPLETE (commit: 8a4ceae9, tests: N/A, otel: N/A)
<No other capsules complete yet>
```

## Critical Path Visualization
```
DAY 1-2: [V6-001 ████░░] [V6-002 ██████░░] ← IN PROGRESS
DAY 3-4: [V6-003 ░░░░░░░░░░░░░░░░] ← BLOCKED
DAY 5:   [V6-004 ✓] ← DONE

DAY 6+:  [Wave 1 ░░░░░░░░] ← BLOCKED (awaits P0)
         [Wave 2 ░░░░░░░░░░░░░░░░] ← BLOCKED
         [Wave 3 ░░░░░░] ← BLOCKED
```

## Alerts & Blockers
🔴 **CRITICAL**: V6-001 uses Date.now() - non-deterministic, violates L3
🟡 **WARNING**: No tests for v6-compat - cannot validate completion
🔴 **CRITICAL**: V6-003 blocked on dependencies - critical path delayed
```

### Dashboard Update Command
```bash
#!/bin/bash
# /home/user/unrdf/scripts/update-dashboard.sh

# Auto-generate dashboard from receipts + test results
timeout 10s node /home/user/unrdf/scripts/generate-dashboard.mjs \
  --receipts /home/user/unrdf/.receipts/v6/ \
  --tests /home/user/unrdf/packages/*/test/ \
  --output /home/user/unrdf/docs/v6/STATUS_DASHBOARD.md

echo "✅ Dashboard updated: /home/user/unrdf/docs/v6/STATUS_DASHBOARD.md"
```

---

## Execution Timeline (Wall Clock)

### Assumptions
- 5 specialized agents working concurrently
- 8-hour work days
- No rework (Big Bang 80/20 methodology)
- All dependencies managed via blocking

### Optimistic Timeline (No Blockers)
| Day | Hours | Work | Capsules | Status |
|-----|-------|------|----------|--------|
| 1 | 0-8 | P0 parallel start | V6-001 (8h), V6-002 (8h of 10h) | V6-001 ✓ |
| 2 | 8-16 | P0 completion | V6-002 finish (2h), V6-003 start (6h of 16h) | V6-002 ✓ |
| 3 | 16-24 | V6-003 continues | V6-003 (8h) | - |
| 4 | 24-32 | V6-003 finish + V6-004 | V6-003 finish (2h), V6-004 (2h), Gate 3 validation | P0 COMPLETE ✓ |
| 5-6 | 32-48 | Wave 1 parallel | V6-010 (10h), V6-012 (14h), V6-015 (7h) | Wave 1 ✓ |
| 7-10 | 48-80 | Wave 2 parallel | V6-013, V6-016, V6-017, V6-018, V6-019 (max 28h per agent) | Wave 2 ✓ |
| 11-12 | 80-96 | Wave 3 sequential | V6-011 (7h), V6-014 (10h) | P1 COMPLETE ✓ |

**Total Wall Time**: 12 days (96 hours, ~2.5 weeks)
**Effective Hours**: 198 hours (5 agents × average utilization)

### Realistic Timeline (With Typical Blockers)
Add 20% buffer for:
- Test failures requiring fixes
- OTEL validation iterations
- Integration issues in Wave 3
- Dependency resolution delays

**Realistic Wall Time**: 15 days (~3 weeks)

---

## Success Metrics

### Completion Criteria (100% Required)
- [ ] All 4 P0 capsules COMPLETE with receipts
- [ ] All 10 P1 capsules COMPLETE with receipts
- [ ] OTEL score ≥80/100 for ALL packages
- [ ] 100% test pass rate (no failing tests)
- [ ] Zero ESLint warnings
- [ ] All 6 validation gates PASSED
- [ ] Receipt chain validated end-to-end
- [ ] Migration guide published with 10+ examples
- [ ] Performance benchmarks show no regressions

### Key Performance Indicators (KPIs)
| Metric | Target | Tracking |
|--------|--------|----------|
| Capsule completion rate | 100% | STATUS_DASHBOARD.md |
| OTEL validation score | ≥80/100 | validation-output.log |
| Test pass rate | 100% | pnpm test output |
| Critical path adherence | <5% variance | Timeline vs actual |
| Scope creep incidents | 0 | PR rejection log |
| Rework cycles | <2 per capsule | Git commit history |

### Daily Standup Questions (Adversarial PM Style)
1. **Did you RUN the tests or just write code?** (Show `pnpm test` output)
2. **What's the OTEL score?** (Show `validation-output.log`)
3. **Can you PROVE determinism?** (Show 10x run output, no Date.now())
4. **What BREAKS if this merges?** (Show integration test results)
5. **Is the receipt chain valid?** (Show receipt validation output)

---

## Appendix: Agent Profiles

### backend-dev
**Expertise**: KGC-4D freeze engine, pure functions, receipt schemas, Zod validation
**Strengths**: Deterministic design, refactoring, pattern reuse
**Use For**: Implementation tasks, receipt integration, schema generation
**Avoid For**: Test creation (use tester), performance benchmarking

### tester
**Expertise**: Adversarial testing, fixture creation, determinism validation
**Strengths**: Edge case discovery, 100% coverage, test suite design
**Use For**: Test suite creation, adversarial inputs, determinism validation
**Avoid For**: Implementation, architecture decisions

### code-analyzer
**Expertise**: Static analysis, pattern detection, ESLint rules, coverage
**Strengths**: Automated validation, anti-pattern detection
**Use For**: Code validation, ESLint rule creation, coverage analysis
**Avoid For**: Implementation, dynamic testing

### system-architect
**Expertise**: Workspace configuration, dependency management, build systems
**Strengths**: Monorepo management, dependency resolution, CI/CD
**Use For**: Workspace updates, dependency conflicts, build optimization
**Avoid For**: Feature implementation, testing

### production-validator
**Expertise**: End-to-end validation, OTEL analysis, integration testing
**Strengths**: Comprehensive validation, failure detection, quality gates
**Use For**: Final sign-off, OTEL validation, gate approval
**Avoid For**: Implementation, early-stage validation

### performance-benchmarker
**Expertise**: Benchmark execution, regression detection, profiling
**Strengths**: Performance analysis, bottleneck identification
**Use For**: L4→L5 performance validation, regression testing
**Avoid For**: Implementation, functional testing

---

## References
- **Capsule Backlog**: `/home/user/unrdf/docs/v6/CAPSULE_BACKLOG.md`
- **Maturity Ladder**: `/home/user/unrdf/docs/v6/MATURITY_LADDER.md`
- **Migration Plan**: `/home/user/unrdf/docs/v6/MIGRATION_PLAN.md`
- **BB80/20 Methodology**: `/home/user/unrdf/docs/bb80-20-methodology.md`
- **CLAUDE.md**: `/home/user/unrdf/CLAUDE.md` (Adversarial PM principles)

---

**END OF ORCHESTRATION PLAN**

**Next Actions**:
1. Review and approve this plan
2. Assign agents to Wave 1 capsules
3. Execute V6-001 and V6-002 in parallel
4. Validate Gate 1 before proceeding to V6-003
5. Update STATUS_DASHBOARD.md daily
