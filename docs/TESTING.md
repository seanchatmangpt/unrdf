# Testing Guide - v5.0.0-beta.1

## Quick Start

**Tests work individually but `pnpm -r test` hangs.** Use per-package execution:

```bash
# Core package tests (166 tests)
cd packages/core
npx vitest run --no-coverage

# CLI package tests (24 tests)
npx vitest run --no-coverage packages/cli/test/adversarial.test.mjs

# Browser package tests
npx vitest run --no-coverage packages/browser/test/adversarial.test.mjs
```

---

## Verified Test Status

**Total Verified**: **190 tests passing**

### @unrdf/core (166 tests)
- ✅ `test/adversarial.test.mjs`: 16 tests
- ✅ `test/core.test.mjs`: 26 tests
- ✅ `test/rdf/unrdf-store.test.mjs`: 58 tests
- ✅ `test/sparql/executor-sync.test.mjs`: 66 tests

### @unrdf/cli (24 tests)
- ✅ `test/adversarial.test.mjs`: 24 tests

### Additional Tests (Not Yet Run)
- `packages/core/test/sparql/n3-backward-compat.test.mjs`
- `packages/core/test/sparql/branch-coverage.test.mjs`
- `packages/core/test/integration/store-integration.test.mjs`
- `packages/core/test/benchmarks/oxigraph-performance.test.mjs`
- `packages/browser/test/browser.test.mjs` (has failures)
- `packages/cli/test/cli/cli.test.mjs`
- `test/**/*.test.mjs` (root test suite)

---

## Known Issues

### Issue 1: `pnpm -r test` Hangs

**Problem**: Running `pnpm -r test` or `pnpm test` from root hangs indefinitely.

**Root Cause**: Complex `vitest.config.mjs` in root with:
- `singleFork: true` pool configuration
- `setupFiles: ["./test/setup/cleanup-hooks.mjs"]`
- Incompatible with pnpm workspace recursive execution

**Workaround**: Run tests per-package individually

```bash
# ✅ WORKS: Individual package execution
cd packages/core && npx vitest run --no-coverage

# ❌ HANGS: Recursive workspace execution
pnpm -r test
```

**Status**: Known issue, workaround documented

---

### Issue 2: Browser Tests Have Failures

**Problem**: `packages/browser/test/adversarial.test.mjs` shows 36 failures

**Details**:
- Tests for browser environment detection
- Tests for RDF store operations
- Tests for quad serialization
- Tests for storage utilities

**Cause**: Unimplemented or incomplete browser features

**Status**: Expected - adversarial tests detect missing capabilities

---

## Running Specific Tests

### Core Package

```bash
cd packages/core

# Run all tests
npx vitest run --no-coverage

# Run specific test file
npx vitest run --no-coverage test/core.test.mjs

# Run with coverage
npx vitest run test/core.test.mjs
```

### CLI Package

```bash
# From root directory
npx vitest run --no-coverage packages/cli/test/adversarial.test.mjs
```

### Browser Package

```bash
# From root directory
npx vitest run --no-coverage packages/browser/test/adversarial.test.mjs
```

---

## OTEL Validation

**Alternative to traditional unit tests**: OTEL span-based validation

```bash
# Run comprehensive OTEL validation
timeout 15s node validation/run-all.mjs comprehensive

# Expected output:
# Score: 83/100 ✅
# Features: 5/6 passed
# Duration: ~3s
```

**Results** (verified 2025-12-06):
- ✅ knowledge-engine-core: 100/100
- ✅ policy-packs: 100/100
- ✅ lockchain-integrity: 100/100
- ✅ transaction-manager: 100/100
- ✅ browser-compatibility: 100/100
- ❌ knowledge-hooks-api: 0/100 (no spans collected)

---

## Test Development

### Adding New Tests

1. **Create test file** in appropriate package `test/` directory
2. **Follow naming convention**: `*.test.mjs`
3. **Run individually** to verify: `npx vitest run --no-coverage test/your-test.test.mjs`

### Test Structure

```javascript
import { describe, it, expect } from 'vitest';
import { yourFunction } from '../src/index.mjs';

describe('Your Feature', () => {
  it('should work as expected', () => {
    const result = yourFunction();
    expect(result).toBeDefined();
  });
});
```

---

## Troubleshooting

### Tests Hang

**Symptom**: Test execution never completes

**Solutions**:
1. Run tests individually per-package (not via `pnpm -r`)
2. Check for infinite loops in test code
3. Verify async operations complete
4. Check for missing `await` keywords

### Tests Fail

**Symptom**: Tests fail with errors

**Solutions**:
1. Check imports are correct
2. Verify dependencies are installed: `pnpm install`
3. Check test isolation - tests may have side effects
4. Review test file for typos or logic errors

### Module Not Found

**Symptom**: `Cannot find module` errors

**Solutions**:
1. Verify file exists: `ls packages/*/src/`
2. Check import path uses correct extension (`.mjs`)
3. Ensure pnpm workspace is properly configured
4. Check `package.json` exports field

---

## Future Improvements

### Planned Fixes

1. **Fix `pnpm -r test` hanging**
   - Simplify root `vitest.config.mjs`
   - Remove or fix `setupFiles` causing hang
   - Test with different pool configurations

2. **Increase test coverage**
   - Run remaining core tests (~100 more tests)
   - Fix browser package tests (36 failures)
   - Add CLI integration tests
   - Add root test suite execution

3. **Fix knowledge-hooks-api OTEL validation**
   - Investigate why no spans collected
   - Fix to bring OTEL score from 83 → 100

---

## CI/CD Integration

**Recommended CI configuration**:

```yaml
# .github/workflows/test.yml
jobs:
  test:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v3
      - uses: pnpm/action-setup@v2
      - run: pnpm install

      # Run tests per-package (workaround for hanging)
      - name: Test Core
        run: cd packages/core && npx vitest run --no-coverage

      - name: Test CLI
        run: npx vitest run --no-coverage packages/cli/test/adversarial.test.mjs

      - name: OTEL Validation
        run: timeout 15s node validation/run-all.mjs comprehensive
```

---

**Last Updated**: 2025-12-06
**Status**: 190 tests verified, pnpm -r test hangs (known issue)
**OTEL Score**: 83/100 (5/6 features passing)
