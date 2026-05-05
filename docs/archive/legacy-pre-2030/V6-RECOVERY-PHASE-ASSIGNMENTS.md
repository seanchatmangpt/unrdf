# 📋 V6 Recovery - Phase Assignments
**Date**: 2025-12-27
**Total Agents**: 10
**Total Phases**: 5 (PHASE 0-4)
**Execution Mode**: Mixed (Sequential critical path + Parallel optimization)

---

## Quick Reference Table

| Phase | Agent # | Role | Task | Duration | Dependencies | Parallel |
|-------|---------|------|------|----------|--------------|----------|
| **0** | 1 | Backend Dev | Fix vitest version in package.json | 15 min | None | ❌ Sequential |
| **1** | 1 | Backend Dev | Execute pnpm install (timeout 300s) | 2-4h | PHASE 0 | ❌ Sequential |
| **latest** | 2 | Backend Dev | Fix CLI determinism (18 violations) | 5-8h | PHASE 1 | ✅ Parallel |
| **latest** | 3 | Backend Dev | Fix delta determinism (23 violations) | 7-10h | PHASE 1 | ✅ Parallel |
| **latest** | 4 | Backend Dev | Fix compat determinism (22 violations) | 6-9h | PHASE 1 | ✅ Parallel |
| **latest** | 5 | Tester | Verify 100/100 determinism proof | 2-3h | PHASE latest.3 | ❌ Sequential |
| **latest** | 6 | Coder | Generate v6-core schemas (30 files) | 16-20h | PHASE 1 | ✅ Parallel |
| **latest** | 7 | Coder | Generate v6-compat schemas (15 files) | 8-12h | PHASE 1 | ✅ Parallel |
| **latest** | 8 | Backend Dev | Add .parse() calls at boundaries | 6-8h | PHASE latest.2 | ✅ Parallel |
| **latest** | 9 | Reviewer | Verify 100% Zod coverage | 2-4h | PHASE latest | ❌ Sequential |
| **latest** | 10 | Prod Validator | Run npm run build | 1h | PHASE latest + latest | ❌ Sequential |
| **latest** | 10 | Prod Validator | Run npm test | 1-2h | PHASE latest | ❌ Sequential |
| **latest** | 10 | Prod Validator | Run npm run lint | latesth | PHASE latest | ❌ Sequential |
| **latest** | 10 | Prod Validator | Run OTEL validation | 1-2h | PHASE latest | ❌ Sequential |
| **latest** | 10 | Prod Validator | Generate evidence report | latest | PHASE latest | ❌ Sequential |

---

## Critical Path Analysis

### Critical Path (Sequential Only)
```
PHASE 0 (15m) → PHASE 1 (2-4h) → PHASE 4 (4-6h)
= latest.25 hours MINIMUM
```

**Why this is the critical path:**
- PHASE 0 must complete before PHASE 1 (vitest version blocks install)
- PHASE 1 must complete before any testing (dependencies block execution)
- PHASE 4 must wait for all previous phases (validation requires complete code)

---

### Non-Critical Paths (Parallelizable)
```
PHASE 2 (Determinism): Agents 2-5 work simultaneously
  - 7-10h maximum (Agent 3 longest) + 2-3h validation
  = 9-13h total

PHASE 3 (Zod Schemas): Agents 6-9 work simultaneously
  - 16-20h maximum (Agent 6 longest) + 6-8h .parse() + 2-4h audit
  = 24-32h total
```

**Parallelization speedup:**
- Without: PHASE 2 (26-30h sequential) + PHASE 3 (32-44h sequential) = 58-74h
- With: PHASE 2 (9-13h parallel) + PHASE 3 (24-32h parallel) = 33-45h
- **Savings**: ~25-29 hours (38-39% faster)

---

## Agent Allocation Strategy

### Sequential Agents (Critical Path)
**Agent 1** (Backend Dev):
- Hours: latest.5h
- Tasks: PHASE 0 (vitest) + PHASE 1 (pnpm install)
- **Why sequential**: Blocks all downstream work

**Agent 10** (Production Validator):
- Hours: 4-6h
- Tasks: PHASE 4 (build/test/lint/OTEL/report)
- **Why sequential**: Requires all previous phases complete

**Total sequential hours**: latest.5h (critical path)

---

### Parallel Agents (PHASE 2 - Determinism)
**Agents 2-4** (Backend Dev):
- Hours: 18-27h combined (5-10h individual)
- Tasks: Fix determinism violations in different modules
- **Why parallel**: No dependencies between CLI/delta/compat modules

**Agent 5** (Tester):
- Hours: 2-3h
- Tasks: Verify determinism proof after agents 2-4 complete
- **Why after**: Needs fixes applied before validation

**Total PHASE 2 hours**: 20-30h combined, 9-13h wall clock (parallel)

---

### Parallel Agents (PHASE 3 - Zod Schemas)
**Agents 6-7** (Coder):
- Hours: 24-32h combined (8-20h individual)
- Tasks: Generate Zod schemas for different packages
- **Why parallel**: No dependencies between v6-core/v6-compat

**Agent 8** (Backend Dev):
- Hours: 6-8h
- Tasks: Add .parse() calls after schemas generated
- **Why after agents 6-7**: Needs schemas to exist first

**Agent 9** (Reviewer):
- Hours: 2-4h
- Tasks: Audit coverage after .parse() calls added
- **Why after agent 8**: Needs complete implementation

**Total PHASE 3 hours**: 32-44h combined, 24-32h wall clock (partial parallel)

---

## Validation Gate Assignments

### Gate 1: PHASE 0 → PHASE 1
**Responsible**: Agent 1 (Backend Dev)
**Check**: Vitest version updated in package.json
**Command**: `grep '"vitest": "\^latest"' package.json`
**On Failure**: Manual edit, re-verify

---

### Gate 2: PHASE 1 → PHASE 2/3
**Responsible**: Agent 1 (Backend Dev)
**Check**: pnpm test runs without ERR_MODULE_NOT_FOUND
**Command**: `timeout 30s pnpm test 2>&1 | grep -v ERR_MODULE_NOT_FOUND`
**On Failure**: Clean install, retry

---

### Gate 3: PHASE 2 → PHASE 4
**Responsible**: Agent 5 (Tester)
**Check**: 0 violations + 100/100 determinism proof
**Command**: `grep -r "Date\.now()" packages/v6-*/src | wc -l` = 0
**On Failure**: Rollback agent 2-4 commits, re-execute

---

### Gate 4: PHASE 3 → PHASE 4
**Responsible**: Agent 9 (Reviewer)
**Check**: 100% Zod coverage + 0 lint errors
**Command**: `npm run lint | grep -i "missing.*schema" | wc -l` = 0
**On Failure**: Manual schema generation, re-audit

---

### Gate 5: PHASE 4 → Production
**Responsible**: Agent 10 (Production Validator)
**Check**: OTEL ≥80/100 AND 100% test pass rate
**Command**: `node validation/run-all.mjs comprehensive | grep "Score:"`
**On Failure**: Root cause analysis, phase-specific rollback

---

## Work Breakdown by Agent

### Agent 1 (Backend Dev) - CRITICAL PATH
**Total Hours**: latest.5h
**Phases**: PHASE 0 + PHASE 1

**PHASE 0 Tasks** (15 min):
1. Read `/home/user/unrdf/package.json` line 107
2. Edit vitest version: "^latest" → "^latest"
3. Verify with grep: `grep '"vitest"' package.json`
4. Pass Gate 1

**PHASE 1 Tasks** (2-4h):
1. Execute: `timeout 300s pnpm install --loglevel debug 2>&1 | tee pnpm-install.log`
2. Verify: `ls node_modules/ | wc -l` > 100 (rough check)
3. Test: `pnpm --filter @unrdf/v6-core test 2>&1 | head -50`
4. Confirm no ERR_MODULE_NOT_FOUND errors
5. Pass Gate 2

**Deliverables**:
- ✅ Updated package.json (vitest version)
- ✅ pnpm-install.log (proof of successful install)
- ✅ Test execution output (proof no module errors)

---

### Agent 2 (Backend Dev) - PARALLEL
**Total Hours**: 5-8h
**Phases**: PHASE latest (CLI determinism)

**Tasks**:
1. Read all CLI module files (~8 files)
2. Identify 18 Date.now/Math.random violations
3. Refactor to accept `context = {}` parameter
4. Replace Date.now() with `context.t_ns || Date.now()`
5. Replace Math.random() with `context.random || Math.random`
6. Verify 0 violations in CLI modules: `grep -r "Date\.now()" packages/v6-core/src/cli | wc -l` = 0
7. Run CLI tests: `pnpm --filter @unrdf/v6-core test -- cli`

**Deliverables**:
- ✅ 18 violations fixed in CLI modules
- ✅ CLI test output (all pass)
- ✅ Grep verification (0 violations)

---

### Agent 3 (Backend Dev) - PARALLEL
**Total Hours**: 7-10h
**Phases**: PHASE latest (Delta determinism)

**Tasks**:
1. Read all delta adapter files (~5 files)
2. Identify 23 randomUUID/Date.now violations
3. Replace randomUUID() with deterministic UUIDv5
4. Add `import { v5 as uuidv5 } from 'uuid'`
5. Refactor to use seed/namespace for UUID generation
6. Verify 0 violations in delta adapters
7. Run delta tests: `pnpm --filter @unrdf/v6-core test -- delta`

**Deliverables**:
- ✅ 23 violations fixed in delta adapters
- ✅ Delta test output (all pass)
- ✅ UUIDv5 implementation verified

---

### Agent 4 (Backend Dev) - PARALLEL
**Total Hours**: 6-9h
**Phases**: PHASE latest (Compat determinism)

**Tasks**:
1. Read `/packages/v6-compat/src/adapters.mjs`
2. Identify 22 Math.random violations
3. Add seedrandom dependency
4. Refactor to use `context.random` with seeded RNG
5. Verify 0 violations in compat adapters
6. Run compat tests: `pnpm --filter @unrdf/v6-compat test`

**Deliverables**:
- ✅ 22 violations fixed in compat adapters
- ✅ Compat test output (all pass)
- ✅ Seeded RNG implementation verified

---

### Agent 5 (Tester) - SEQUENTIAL AFTER 2-4
**Total Hours**: 2-3h
**Phases**: PHASE latest (Determinism proof)

**Tasks**:
1. Wait for agents 2-4 to complete
2. Verify 0 total violations: `grep -r "Date\.now()\|Math\.random()\|randomUUID()" packages/v6-*/src | wc -l` = 0
3. Run determinism proof: `timeout 30s node test/l5-maturity/l3-determinism-direct.test.mjs`
4. Verify 100/100 identical hashes
5. Extract hash: `grep "Hash:" test-output.log`
6. Pass Gate 3

**Deliverables**:
- ✅ Determinism proof output (100/100 pass)
- ✅ Identical hash verification
- ✅ 0 violations grep output

---

### Agent 6 (Coder) - PARALLEL
**Total Hours**: 16-20h
**Phases**: PHASE latest (v6-core schemas)

**Tasks**:
1. Run schema generator on v6-core:
   ```bash
   node packages/v6-compat/scripts/generate-schemas.mjs \
     --input packages/v6-core/src \
     --output packages/v6-core/src
   ```
2. Verify ~30 schema files created
3. Review generated schemas for correctness
4. Test schemas validate expected inputs
5. Fix any generation errors

**Deliverables**:
- ✅ ~30 .schema.mjs files in v6-core/src
- ✅ Schema validation tests pass
- ✅ File count verification

---

### Agent 7 (Coder) - PARALLEL
**Total Hours**: 8-12h
**Phases**: PHASE latest (v6-compat schemas)

**Tasks**:
1. Run schema generator on v6-compat:
   ```bash
   node packages/v6-compat/scripts/generate-schemas.mjs \
     --input packages/v6-compat/src \
     --output packages/v6-compat/src
   ```
2. Verify ~15 schema files created
3. Review adapters.schema.mjs for correctness
4. Test schemas validate adapter inputs

**Deliverables**:
- ✅ ~15 .schema.mjs files in v6-compat/src
- ✅ Adapter schema tests pass
- ✅ File count verification

---

### Agent 8 (Backend Dev) - SEQUENTIAL AFTER 6-7
**Total Hours**: 6-8h
**Phases**: PHASE latest (.parse() calls)

**Tasks**:
1. Wait for agents 6-7 to complete
2. Identify all public API functions in v6-core/v6-compat
3. Add schema imports: `import { xSchema } from './x.schema.mjs'`
4. Add .parse() calls at function entry: `const validated = xSchema.parse(input)`
5. Test invalid inputs throw ZodError
6. Verify 100% API coverage

**Deliverables**:
- ✅ .parse() calls added to all public APIs
- ✅ Validation tests pass (invalid inputs rejected)
- ✅ Code review shows complete coverage

---

### Agent 9 (Reviewer) - SEQUENTIAL AFTER 8
**Total Hours**: 2-4h
**Phases**: PHASE latest (Coverage audit)

**Tasks**:
1. Wait for agent 8 to complete
2. Count total exports: `find packages/v6-*/src -name "*.mjs" -exec grep -c "^export" {} \; | awk '{s+=$1} END {print s}'`
3. Count schema files: `find packages/v6-*/src -name "*.schema.mjs" | wc -l`
4. Verify 100% coverage (counts match)
5. Run lint: `npm run lint 2>&1 | grep -i "missing.*schema" | wc -l` = 0
6. Code review: Spot-check 10 random files for .parse() calls
7. Pass Gate 4

**Deliverables**:
- ✅ Coverage audit report (100% verified)
- ✅ Lint output (0 missing schemas)
- ✅ Code review notes (10 files checked)

---

### Agent 10 (Production Validator) - SEQUENTIAL AFTER ALL
**Total Hours**: 4-6h
**Phases**: PHASE latest.5 (Full validation)

**PHASE latest Tasks** (1h):
1. Run build: `timeout 60s npm run build 2>&1 | tee build-output.log`
2. Verify 0 errors: `grep -i "error" build-output.log | wc -l` = 0
3. Pass build gate

**PHASE latest Tasks** (1-2h):
1. Run tests: `timeout 120s npm test 2>&1 | tee test-output.log`
2. Count passes: `grep "✅.*pass" test-output.log | wc -l`
3. Count fails: `grep "❌.*fail" test-output.log | wc -l` = 0
4. Verify 100% pass rate
5. Pass test gate

**PHASE latest Tasks** (latesth):
1. Run lint: `timeout 60s npm run lint 2>&1 | tee lint-output.log`
2. Verify 0 violations: `grep -i "error\|warning" lint-output.log | wc -l` = 0
3. Pass lint gate

**PHASE latest Tasks** (1-2h):
1. Run OTEL: `timeout 300s node validation/run-all.mjs comprehensive 2>&1 | tee validation-output.log`
2. Extract score: `grep "Score:" validation-output.log`
3. Verify ≥80/100
4. Count failures: `grep "FAILED\|Error" validation-output.log | wc -l` = 0
5. Pass Gate 5

**PHASE latest Tasks** (latest):
1. Generate report: `/home/user/unrdf/V6-PRODUCTION-READINESS-REPORT.md`
2. Include all output logs (build/test/lint/OTEL)
3. Document before/after metrics
4. Adversarial PM checklist
5. Sign off with evidence

**Deliverables**:
- ✅ build-output.log (0 errors)
- ✅ test-output.log (100% pass)
- ✅ lint-output.log (0 violations)
- ✅ validation-output.log (≥80/100)
- ✅ V6-PRODUCTION-READINESS-REPORT.md

---

## Resource Allocation Summary

### Peak Parallelization (After PHASE 1)
**Simultaneous Agents**: 7 agents (Agents 2-8)
- Agents 2-4: PHASE latest.3 (determinism fixes)
- Agents 6-7: PHASE latest.2 (schema generation)
- Agent 8: PHASE latest (.parse() calls, after 6-7 complete)

**Peak Hours**: Hours latest.5 (24 hours of parallel work)

---

### Total Agent Hours
| Agent | Hours | Utilization |
|-------|-------|-------------|
| Agent 1 | latest.5h | 3-6% |
| Agent 2 | 5-8h | 7-11% |
| Agent 3 | 7-10h | 9-14% |
| Agent 4 | 6-9h | 8-12% |
| Agent 5 | 2-3h | 3-4% |
| Agent 6 | 16-20h | 21-27% |
| Agent 7 | 8-12h | 11-16% |
| Agent 8 | 6-8h | 8-11% |
| Agent 9 | 2-4h | 3-5% |
| Agent 10 | 4-6h | 5-8% |
| **TOTAL** | **58-84h** | **78-114% avg** |

**Total Wall Clock Time**: 54-74 hours (parallelization saves ~4-10 hours)

---

## Next Steps

**Ready to Execute**: PHASE 0 (Agent 1, 15 min)
**Awaiting Approval**: User confirmation to proceed

**Recommended First Action**:
```bash
# Agent 1 executes PHASE 0
Edit /home/user/unrdf/package.json line 107:
BEFORE: "vitest": "^latest"
AFTER:  "vitest": "^latest"

Verify: grep '"vitest"' package.json
Pass Gate 1, proceed to PHASE 1
```

---

**Phase Assignments Complete**
**Status**: ✅ Ready for Execution
**Agents**: 10 assigned with clear responsibilities
**Gates**: 5 validation gates with failure remediation
