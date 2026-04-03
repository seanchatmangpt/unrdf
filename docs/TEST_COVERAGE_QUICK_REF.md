# TEST COVERAGE QUICK REFERENCE - UNRDF

**Status:** 🚨 CRITICAL GAPS - Immediate Action Required
**Overall Coverage:** ~35-40% (Target: 80%)
**Generated:** 2025-12-21

---

## 🚨 TOP 10 CRITICAL ISSUES (Fix First)

| # | Issue | Impact | Package | Action |
|---|-------|--------|---------|--------|
| 1 | **24 failing tests** | Blocks CI/CD | hooks, kgc-4d, core | Debug & fix |
| 2 | **Validation has 0 tests** | Core validation untested | validation | Add 75 tests |
| 3 | **Knowledge engine 1.75% test ratio** | Reasoning engine untested | knowledge-engine | Add 90 tests |
| 4 | **CLI 3% test ratio** | User-facing bugs | cli | Add 40 tests |
| 5 | **Project engine 2.7% test ratio** | Core features untested | project-engine | Add 50 tests |
| 6 | **Security modules 0% coverage** | Sandbox escape risk | knowledge-engine/security | Add 55 tests |
| 7 | **Zero concurrency tests** | Race conditions undetected | core, hooks | Add 30 tests |
| 8 | **edge-case-handler.mjs untested** | Edge cases unhandled (ironic!) | core | Add 25 tests |
| 9 | **MaxListenersExceededWarning** | Memory leak risk | streaming | Fix cleanup |
| 10 | **Weak assertions** | Low mutation score | all packages | Strengthen assertions |

---

## 📊 PACKAGE COVERAGE SNAPSHOT

| Package | Source Files | Test Files | Coverage | Status |
|---------|--------------|------------|----------|--------|
| **kgc-4d** | 23 | 24 | ~90% | ⭐ Excellent |
| **hooks** | 30 | 18 | ~80% | ⭐ Good (22 failures!) |
| **oxigraph** | 3 | 4 | ~85% | ⭐ Excellent |
| **federation** | 11 | 4 | ~70% | ✅ Moderate |
| **streaming** | 9 | 4 | ~65% | ✅ Moderate |
| **atomvm** | 10 | 7 | ~70% | ✅ Moderate |
| **core** | 48 | 6 | ~55% | ⚠️ Needs work |
| **engine-gateway** | 4 | 1 | ~40% | ⚠️ Needs work |
| **dark-matter** | 9 | 1 | ~15% | 🚨 Critical |
| **kgn** | 9 | 2 | ~30% | 🚨 Critical |
| **composables** | 16 | 1 | ~20% | 🚨 Critical |
| **cli** | **32** | **1** | **~10%** | 🚨 **Critical** |
| **project-engine** | **37** | **1** | **~10%** | 🚨 **Critical** |
| **knowledge-engine** | **57** | **1** | **~5%** | 🚨 **Critical** |
| **validation** | **8** | **0** | **0%** | 🚨 **Critical** |
| **domain** | **11** | **0** | **0%** | 🚨 **Critical** |
| **react** | 4 | 0 | 0% | 🚨 Critical |
| **nextra** | 2 | 0 | 0% | 🚨 Critical |

**Total:** 376 source files, 77 test files (20.5% ratio)

---

## ⚡ IMMEDIATE ACTIONS (This Week)

### Day 1: Fix Failing Tests
```bash
# Run tests and identify failures
pnpm test 2>&1 | tee test-output.log
grep "×\|failed" test-output.log

# Fix priority order:
# 1. hooks/error-handling.test.mjs (13 failures)
# 2. hooks/effect-sandbox.test.mjs (4 failures)
# 3. hooks/telemetry.test.mjs (4 failures)
# 4. kgc-4d/doctest-integration.test.mjs (19 failures)
# 5. core/integration/store-integration.test.mjs (1 failure)
```

### Day 2: Add Validation Tests
```bash
# Create test file
touch packages/validation/test/validation.test.mjs

# Add 75 tests covering:
# - RDF validation (30 tests)
# - SHACL validation (20 tests)
# - Zod schema validation (15 tests)
# - Security validation (10 tests)
```

### Day 3: Add Knowledge Engine Tests
```bash
# Create critical test files
touch packages/knowledge-engine/test/reason.test.mjs
touch packages/knowledge-engine/test/validate-shacl.test.mjs
touch packages/knowledge-engine/test/security/sandbox-escape.test.mjs

# Add 90+ tests
```

### Day 4: Fix MaxListenersExceededWarning
```bash
# Add cleanup to streaming tests
# packages/streaming/test/*.test.mjs
afterEach(async () => {
  await cleanupAllProcessors();
});
```

### Day 5: Add CLI Tests
```bash
touch packages/cli/test/cli-commands.test.mjs
# Add 40 tests for all CLI commands
```

---

## 🔍 UNCOVERED CRITICAL FILES (High Priority)

### Core Package
- ❌ `src/utils/edge-case-handler.mjs` - Edge case detection (0 tests!)
- ❌ `src/utils/transaction.mjs` - ACID guarantees (0 tests!)
- ❌ `src/utils/lockchain-writer.mjs` - Concurrent safety (0 tests!)
- ❌ `src/utils/memory-manager.mjs` - Memory management (0 tests!)
- ❌ `src/utils/performance-optimizer.mjs` - Performance tuning (0 tests!)

### Knowledge Engine
- ❌ `src/reason.mjs` - Reasoning engine (0 tests!)
- ❌ `src/validate.mjs` - SHACL validation (0 tests!)
- ❌ `src/security/*` - All security modules (0 tests!)
- ❌ `src/ken-parliment.mjs` - Multi-engine coordination (0 tests!)

### CLI
- ❌ All 32 files except basic graph commands (0 tests!)

---

## 📋 TEST QUALITY ISSUES

### Weak Assertions (Need Strengthening)
```javascript
// ❌ Found in many tests
expect(result).toBeDefined();
expect(array).toHaveLength(1);

// ✅ Should be
expect(result).toEqual({ subject: 'alice', predicate: 'knows', object: 'bob' });
expect(array[0].value).toBe('expected-value');
```

### Missing Error Path Tests (~50% coverage)
```javascript
// ✅ Add these patterns
expect(() => fn(null)).toThrow(ValidationError);
expect(() => fn(invalid)).toThrow(/specific error message/);
await expect(asyncFn()).rejects.toThrow(/error/);
```

### Missing Edge Case Tests
```javascript
// ✅ Add these patterns
it('should handle null input', () => {});
it('should handle undefined input', () => {});
it('should handle empty string', () => {});
it('should handle extremely large input (1MB)', () => {});
it('should handle Unicode/emoji', () => {});
```

### Missing Concurrency Tests
```javascript
// ✅ Add these patterns
it('should handle 100 concurrent operations', async () => {
  const promises = Array(100).fill(null).map(() => operation());
  await Promise.all(promises);
  expect(noDataLoss()).toBe(true);
});
```

---

## 📈 COVERAGE TARGETS

| Phase | Timeline | Target | Status |
|-------|----------|--------|--------|
| **Current** | 2025-12-21 | ~35% | 🚨 Critical |
| **Phase 1** | Week 3 | 60% | ⏳ Planning |
| **Phase 2** | Week 6 | 80% | ⏳ Planning |
| **Phase 3** | Week 12 | 85%+ | ⏳ Planning |

---

## 🛠️ TOOLS & COMMANDS

### Run Tests
```bash
# All tests
pnpm test

# Specific package
pnpm test:core
pnpm test:hooks
pnpm test:knowledge-engine

# With coverage
pnpm test:coverage

# Watch mode
pnpm test:watch
```

### Analyze Coverage
```bash
# Generate coverage report
pnpm coverage

# View HTML report
open coverage/index.html

# Check coverage per file
grep -A 5 "File" coverage/coverage-summary.json
```

### Find Uncovered Code
```bash
# Find source files without tests
./scripts/find-uncovered.sh

# Count test ratio per package
./scripts/test-ratio.sh
```

### Run Mutation Testing (Phase 3)
```bash
pnpm stryker run
```

---

## 📚 REFERENCES

- **Full Analysis:** `/Users/sac/unrdf/docs/TEST_COVERAGE_ANALYSIS.md`
- **Implementation Plan:** `/Users/sac/unrdf/docs/TEST_IMPLEMENTATION_PLAN.md`
- **Vitest Docs:** https://vitest.dev/
- **Fast-check (Property Testing):** https://github.com/dubzzz/fast-check
- **Stryker (Mutation Testing):** https://stryker-mutator.io/

---

## ✅ WEEKLY CHECKLIST

### Week 1
- [ ] Fix all 24 failing tests
- [ ] Add validation tests (75 tests, 70% coverage)
- [ ] Add knowledge-engine tests (90 tests, 60% coverage)
- [ ] Fix MaxListenersExceededWarning
- [ ] Add CLI tests (40 tests)

### Week 2
- [ ] Add security tests (55 tests)
- [ ] Add concurrency tests (30 tests)
- [ ] Add core utils tests (75 tests)
- [ ] Core package reaches 70% coverage

### Week 3
- [ ] Add project-engine tests (50 tests)
- [ ] Add domain tests (40 tests)
- [ ] Add integration tests (30 tests)
- [ ] Overall coverage reaches 60%

---

**Quick Ref Version:** 1.0
**Last Updated:** 2025-12-21
**Next Review:** Weekly
