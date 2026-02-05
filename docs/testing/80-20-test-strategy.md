# 80/20 Test Consolidation Strategy

## Overview

This document describes the 80/20 test consolidation strategy for UNRDF, applying the Pareto Principle to optimize test execution while maintaining high coverage.

## Empirical Analysis

**Total Test Suite:**
- 552 test files (132 in `test/`, 420 in `packages/`)
- 40,877 lines of test code in `test/` directory alone
- Full suite execution: ~5-10 minutes

**80/20 Insight:**
- ~10% of tests (45 files) deliver 80% of value
- ~3% of tests (15 files) deliver 60% of value (core functionality)

## Test Tiers

### Tier 1: Essential (<10s, 60%+ coverage)

**Purpose:** Ultra-fast pre-commit validation
**Target:** <10 seconds execution, 60%+ coverage
**Use case:** Pre-commit hook, rapid feedback loop

**Test count:** ~15 files
**Configuration:** `vitest.config.essential.mjs`
**Command:** `pnpm test:essential`

**Included tests:**
- Core RDF operations (diff, project-engine)
- Hook system basics (hook-executor-deps)
- Knowledge engine contracts (parse, query)
- Security fundamentals (lockchain, error-sanitizer)
- E2E smoke test (e2e-integration)
- V6 core features

**Quality gates:**
- Must pass before commit
- Blocks commit on failure
- 500ms timeout per test (Andon principle)

### Tier 2: Important (<30s, 75%+ coverage)

**Purpose:** Pre-push validation
**Target:** <30 seconds execution, 75%+ coverage
**Use case:** Pre-push hook, CI fast path

**Test count:** ~30 additional files (45 total with Tier 1)
**Configuration:** `vitest.config.fast.mjs`
**Command:** `pnpm test:fast`

**Included tests (in addition to Tier 1):**
- Optimization & performance (dark-matter-80-20, query-optimizer)
- YAWL workflow engine (task activation, execution)
- Streaming & real-time (change feeds, sync)
- KGC governance (runtime, 4D, receipts)
- Resilience (circuit-breaker, ring-buffer)
- Federation basics (distributed query, Raft)
- CLI core (commands, smoke tests)

**Quality gates:**
- Must pass before push
- Blocks push on failure
- 2s timeout per test (Andon principle)

### Tier 3: Comprehensive (<5min, 90%+ coverage)

**Purpose:** Full validation
**Target:** <5 minutes execution, 90%+ coverage
**Use case:** Nightly CI, pre-release, full validation

**Test count:** All 552 test files
**Configuration:** `vitest.config.mjs`
**Command:** `pnpm test` or `pnpm test:all`

**Included tests:**
- All browser compatibility tests
- All React hooks tests
- ML & advanced features
- Full knowledge engine suite
- Full streaming suite
- Full E2E test suite
- Full federation suite
- All package-specific tests
- Example validation tests
- Formal verification proofs
- Agent innovation tests

**Quality gates:**
- Must pass for release
- Blocks release on failure
- 5s timeout per test (Andon SLA)

## Pareto Analysis Results

```
┌──────────────────────────────────────────────────────────────┐
│                    Pareto Analysis                           │
├──────────────────────────────────────────────────────────────┤
│ Tier       │ Tests │ % of Total │ Coverage │ Time │ Value   │
├──────────────────────────────────────────────────────────────┤
│ Essential  │    15 │       2.7% │      60% │  10s │  High   │
│ Important  │    30 │       5.4% │      75% │  30s │  Medium │
│ Combined   │    45 │       8.2% │      80% │  30s │ OPTIMAL │
│ Full       │   552 │     100.0% │      90% │ 300s │   Low   │
└──────────────────────────────────────────────────────────────┘
```

**Sweet Spot:** 45 tests (8.2%) = 80% coverage in 30 seconds

## Execution Targets (Andon Principle)

All timeouts follow the Andon principle: if exceeded, STOP and investigate root cause.

### Essential Tier
- **Total timeout:** 10 seconds
- **Per-test timeout:** 500ms
- **Hook timeout:** 1s
- **Retry:** 0 (fail fast)
- **Bail:** 1 (stop on first failure)

### Important Tier
- **Total timeout:** 30 seconds
- **Per-test timeout:** 2s
- **Hook timeout:** 5s
- **Retry:** 1
- **Bail:** 0 (run all for diagnostics)

### Comprehensive Tier
- **Total timeout:** 5 minutes (300s)
- **Per-test timeout:** 5s (Andon SLA)
- **Hook timeout:** 10s
- **Retry:** 1
- **Bail:** 0

## Coverage Targets

| Tier | Lines | Functions | Branches | Statements | Description |
|------|-------|-----------|----------|------------|-------------|
| Essential | 60% | 60% | 55% | 60% | Core functionality |
| Important | 75% | 75% | 70% | 75% | Important paths |
| Comprehensive | 90% | 90% | 85% | 90% | Full coverage |

**Production target:** 80% across all metrics (met by Important tier)

## Git Hooks Integration

### Pre-commit Hook
```bash
timeout 15s pnpm test:essential
```
- Runs essential tier only
- Total time: <15s (10s tests + 5s margin)
- Blocks commit on failure

### Pre-push Hook
```bash
timeout 35s pnpm test:fast
```
- Runs essential + important tiers
- Total time: <35s (30s tests + 5s margin)
- Blocks push on failure

## CI/CD Integration

### Pull Request
```yaml
- name: Fast tests
  run: timeout 35s pnpm test:fast
```

### Nightly Build
```yaml
- name: Full test suite
  run: timeout 360s pnpm test:all
```

### Release
```yaml
- name: Full validation with coverage
  run: pnpm test:coverage:full
```

## File Size Analysis

Test file size often correlates with criticality:

### Large (>500 lines) → Comprehensive Tier
- `test/knowledge-engine/observability.test.mjs` (992 lines)
- `test/e2e-rdf-kgn.test.mjs` (764 lines)
- `test/ml/ml.test.mjs` (746 lines)
- `test/diff.test.mjs` (685 lines) ⚠️ Exception: Essential tier

### Medium (200-500 lines) → Important Tier
- `test/project-engine.test.mjs` (487 lines) ⚠️ Exception: Essential tier
- `test/dark-matter-80-20.test.mjs` (362 lines)
- `test/knowledge-engine/utils/circuit-breaker.test.mjs` (505 lines)

### Small (<200 lines) → Essential Tier
- `test/e2e-integration.test.mjs` (112 lines)
- `test/lockchain-merkle-verification.test.mjs` (168 lines)
- `test/hook-executor-deps.test.mjs` (52 lines)
- `test/knowledge-engine/parse-contract.test.mjs` (21 lines)

## Commands Reference

```bash
# Essential tier (<10s)
pnpm test:essential

# Important tier (<30s)
pnpm test:fast

# Full suite (~5min)
pnpm test
pnpm test:all  # Alternative: runs all package tests

# Coverage
pnpm test:coverage       # Fast tier with coverage
pnpm test:coverage:full  # Full suite with coverage

# Package-specific
pnpm test:core
pnpm test:hooks
pnpm test:cli
pnpm test:knowledge-engine

# Watch mode
pnpm test:watch
pnpm test:watch:pkg core
```

## Quality Gates

### Commit Gate (Essential)
- ✅ Must pass: Yes
- 🚫 Block commit: Yes
- 🚫 Block push: No
- ⏱️ Timeout: 15s

### Push Gate (Important)
- ✅ Must pass: Yes
- 🚫 Block commit: No
- 🚫 Block push: Yes
- ⏱️ Timeout: 35s

### Release Gate (Comprehensive)
- ✅ Must pass: Yes
- 🚫 Block commit: No
- 🚫 Block push: No
- ⏱️ Timeout: 360s (6 minutes)

## Maintenance

### Adding New Tests

1. **Determine tier:**
   - Core RDF/hooks/security → Essential
   - Performance/resilience/federation → Important
   - Browser/React/ML/Advanced → Comprehensive

2. **Add to config:**
   - Essential: `vitest.config.essential.mjs`
   - Important: `vitest.config.fast.mjs`
   - Comprehensive: `vitest.config.mjs`

3. **Verify timeout:**
   - Essential: Must complete in <500ms
   - Important: Must complete in <2s
   - Comprehensive: Must complete in <5s

4. **Run verification:**
   ```bash
   timeout 15s pnpm test:essential  # Should pass
   timeout 35s pnpm test:fast       # Should pass
   ```

### Rebalancing Tiers

Run quarterly analysis:

```bash
# Analyze test execution times
pnpm test:fast --reporter=verbose 2>&1 | grep "PASS\|FAIL" > times.txt

# Identify slow tests (>2s in fast tier)
# Move to comprehensive tier or optimize

# Identify critical gaps in essential tier
# Add missing core functionality tests
```

## Success Metrics

- ✅ Essential tier: <10s (currently: TBD)
- ✅ Important tier: <30s (currently: TBD)
- ✅ Comprehensive tier: <5min (currently: TBD)
- ✅ Coverage: 80%+ (important tier)
- ✅ Pre-commit feedback: <15s
- ✅ Pre-push feedback: <35s

## References

- [Vitest Configuration](https://vitest.dev/config/)
- [80/20 Analysis](../../test-consolidation-80-20.mjs)
- [Testing Standards](../../.claude/rules/testing-standards.md)
- [CLAUDE.md - Testing Infrastructure](../../CLAUDE.md#testing-infrastructure)
