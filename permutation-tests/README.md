# Package Permutation Use Case Tests

**Purpose:** Empirically test ALL combinations of @unrdf packages to determine:
- What works vs what breaks
- Which combinations are production-ready
- Dependency requirements
- 80/20 consolidation opportunities

## Test Matrix

### Single Package Tests (4 tests)
1. `01-core-only.mjs` - Core package in isolation
2. `02-hooks-only.mjs` - Hooks package in isolation
3. `03-kgc4d-only.mjs` - KGC 4D package in isolation
4. `04-knowledge-engine-only.mjs` - Knowledge engine in isolation

### Two Package Combinations (6 tests)
5. `05-core-hooks.mjs` - Core + Hooks
6. `06-core-kgc4d.mjs` - Core + KGC 4D
7. `07-core-knowledge.mjs` - Core + Knowledge Engine
8. `08-hooks-kgc4d.mjs` - Hooks + KGC 4D
9. `09-hooks-knowledge.mjs` - Hooks + Knowledge Engine
10. `10-kgc4d-knowledge.mjs` - KGC 4D + Knowledge Engine

### Three Package Combinations (4 tests)
11. `11-core-hooks-kgc4d.mjs` - Core + Hooks + KGC 4D
12. `12-core-hooks-knowledge.mjs` - Core + Hooks + Knowledge Engine
13. `13-core-kgc4d-knowledge.mjs` - Core + KGC 4D + Knowledge Engine
14. `14-hooks-kgc4d-knowledge.mjs` - Hooks + KGC 4D + Knowledge Engine

### Full Integration (1 test)
15. `15-all-packages.mjs` - All 4 packages together

## Running Tests

```bash
# Run all permutation tests
node permutation-tests/run-all.mjs

# Run individual test
node permutation-tests/01-core-only.mjs

# Run with timeout (recommended)
timeout 10s node permutation-tests/run-all.mjs
```

## Success Criteria

Each test script:
- ✅ Imports packages successfully
- ✅ Creates basic instances
- ✅ Executes core functionality
- ✅ Reports clear PASS/FAIL status
- ✅ Completes in <5 seconds

## Expected Results

**Hypothesis based on audit:**
- Core only: ✅ Should work (foundation)
- Hooks only: ❌ Should fail (needs core)
- KGC 4D only: ❌ Should fail (needs core + oxigraph)
- Knowledge Engine only: ❌ Should fail (needs core)
- Core + Hooks: ✅ Should work
- Core + KGC 4D: ✅ Should work
- Core + Knowledge Engine: ⚠️ Unknown
- All combinations with core: ✅ Should work

## Adversarial PM Validation

Before claiming consolidation decisions:
- ❓ Did I RUN all 15 permutations?
- ❓ Did I verify success/failure with OUTPUT?
- ❓ What SPECIFIC functionality breaks in each combo?
- ❓ Can I PROVE which packages are truly independent?

## Results Format

```
PERMUTATION TEST RESULTS
========================

01 - Core Only:           ✅ PASS (12ms)
02 - Hooks Only:          ❌ FAIL (Cannot find '@unrdf/core')
03 - KGC 4D Only:         ❌ FAIL (Cannot find '@unrdf/core')
04 - Knowledge Engine:    ❌ FAIL (Cannot find '@unrdf/core')
05 - Core + Hooks:        ✅ PASS (24ms)
06 - Core + KGC 4D:       ✅ PASS (31ms)
...

SUMMARY
=======
Passed: 7/15 (46.7%)
Failed: 8/15 (53.3%)

CONSOLIDATION INSIGHT
=====================
Core is required for: hooks, kgc-4d, knowledge-engine (100%)
Independent packages: 0
Recommendation: Merge all into core OR keep dependency explicit
```
