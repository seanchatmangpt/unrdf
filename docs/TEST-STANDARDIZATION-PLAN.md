# UNRDF Test Standardization Plan

**Objective**: Enforce ≥80% coverage across all packages with production-grade quality gates

**Timeline**: 3 phases (Immediate → Short-term → Long-term)
**Success Criteria**: 100% of packages enforce 80/80/80/80 thresholds, 0 test failures, <5s test execution per package

---

## Phase 1: IMMEDIATE (Week 1) - Critical Coverage Enforcement

**Goal**: Add missing coverage thresholds to all 18 packages

### 1.1 Update Package Configurations

**Action**: Add standardized thresholds to all `vitest.config.mjs` files

**Template** (apply to all packages):

```javascript
/**
 * @file Vitest Configuration for @unrdf/[PACKAGE]
 * @description Production-grade testing with 80%+ coverage enforcement
 */
import { defineConfig } from 'vitest/config';

export default defineConfig({
  test: {
    globals: true,
    environment: 'node', // or 'jsdom' for browser packages

    // Test execution
    include: ['test/**/*.test.mjs'],
    testTimeout: 15000, // 15s default (adjust per package needs)
    hookTimeout: 5000,

    // Coverage configuration - MANDATORY 80% THRESHOLDS
    coverage: {
      provider: 'v8',
      reporter: ['text', 'json', 'html', 'lcov'], // Add lcov for CI
      include: ['src/**/*.mjs'],
      exclude: [
        'node_modules/**',
        'test/**',
        'dist/**',
        '**/*.config.mjs',
        '**/types.mjs', // Type definitions exempt
      ],

      // ⚠️ CRITICAL: 80% minimum coverage thresholds
      thresholds: {
        lines: 80,
        functions: 80,
        branches: 80,
        statements: 80,
      },

      // Enforce coverage on all files
      all: true,

      // Fail tests if thresholds not met
      skipFull: false,
    },

    // Isolation and performance
    isolate: true,
    pool: 'forks',
    poolOptions: {
      forks: { singleFork: true }
    },

    // Reporter configuration
    reporter: process.env.CI ? ['json', 'github'] : ['verbose'],

    // Retry configuration
    retry: process.env.CI ? 2 : 0,
  },
});
```

### 1.2 Package-Specific Adjustments

| Package          | Environment | Timeout | Notes                           |
|------------------|-------------|---------|---------------------------------|
| atomvm           | jsdom       | 10s     | Increase threshold from 39→80   |
| core             | node        | 60s     | Keep 60s for RDF perf tests     |
| hooks            | node        | 15s     | Add browser test config         |
| federation       | node        | 20s     | Distributed tests need margin   |
| streaming        | node        | 20s     | WebSocket tests need margin     |
| others           | node        | 15s     | Standard configuration          |

### 1.3 Execution Plan

**Batch 1** (Critical packages - Day 1):
```bash
# Update configurations
packages/core/vitest.config.mjs
packages/hooks/vitest.config.mjs
packages/federation/vitest.config.mjs
packages/streaming/vitest.config.mjs
```

**Batch 2** (Supporting packages - Day 2):
```bash
packages/cli/vitest.config.mjs
packages/composables/vitest.config.mjs
packages/oxigraph/vitest.config.mjs
packages/validation/vitest.config.mjs
```

**Batch 3** (Remaining packages - Day 3):
```bash
# All other packages
packages/atomvm/vitest.config.mjs
packages/dark-matter/vitest.config.mjs
packages/domain/vitest.config.mjs
packages/engine-gateway/vitest.config.mjs
packages/kgc-4d/vitest.config.mjs
packages/kgn/vitest.config.js
packages/knowledge-engine/vitest.config.mjs
packages/project-engine/vitest.config.mjs
packages/test-utils/vitest.config.mjs
packages/docs/vitest.config.ts
```

### 1.4 Verification Commands

```bash
# Per-package verification
timeout 5s pnpm -C packages/[PACKAGE] test

# Check coverage output includes thresholds
grep -A5 "Coverage" packages/[PACKAGE]/coverage/coverage-summary.json

# Workspace-wide verification
timeout 30s pnpm test:coverage

# Expected output:
# ✅ All packages ≥80% on lines/functions/branches/statements
# ❌ Fail if ANY package below 80%
```

---

## Phase 2: SHORT-TERM (Week 2-3) - Quality Improvements

### 2.1 Standardize File Naming

**Action**: Rename `*.spec.mjs` → `*.test.mjs`

```bash
# Find outliers
find packages -name "*.spec.mjs"

# Rename (verify count = 2)
# Update imports if any test files cross-reference
```

### 2.2 Standardize Timeout Configuration

**Action**: Add explicit timeouts to all configs

**Rationale**: Prevent silent hangs, enforce Andon principle from CLAUDE.md

```javascript
// Add to all configs
testTimeout: 15000, // Default: 15s
hookTimeout: 5000,  // Default: 5s

// Override for specific packages:
// - core: 60000 (RDF performance tests)
// - federation: 20000 (distributed coordination)
// - streaming: 20000 (WebSocket tests)
```

### 2.3 Add CI-Optimized Reporters

**Action**: Update reporter configuration for CI/local split

```javascript
reporter: process.env.CI
  ? ['json', 'github', 'junit']
  : ['verbose', 'html'],

coverage: {
  reporter: process.env.CI
    ? ['json', 'lcov'] // CI: machine-readable only
    : ['text', 'json', 'html', 'lcov'], // Local: human + machine
}
```

### 2.4 Migrate Experiment Dependencies

**Action**: Replace Mocha/Chai in atomvm experiments with Vitest

**Affected directories**:
- `packages/atomvm/experiments/docker-swarm-messaging/`
- `packages/atomvm/experiments/chaos-cluster/`

**Migration**:
1. Replace `chai` expect syntax → Vitest `expect()`
2. Remove mocha `describe/it` → Use Vitest equivalents
3. Update package.json dependencies
4. Verify tests still pass

---

## Phase 3: LONG-TERM (Month 1-2) - Advanced Optimization

### 3.1 Workspace-Level Coverage Aggregation

**Action**: Create `vitest.workspace.mjs` for unified coverage

```javascript
import { defineWorkspace } from 'vitest/config';

export default defineWorkspace([
  'packages/*/vitest.config.mjs',
  {
    test: {
      name: 'integration',
      include: ['tests/**/*.test.mjs'], // Root-level integration tests
    }
  }
]);
```

**Benefits**:
- Single command for all tests: `vitest --workspace`
- Aggregated coverage report across packages
- Parallel test execution with proper isolation

### 3.2 Add Coverage Trend Tracking

**Action**: Integrate coverage reports into CI/CD

```yaml
# .github/workflows/test.yml (example)
- name: Test with Coverage
  run: pnpm test:coverage

- name: Upload Coverage to Codecov
  uses: codecov/codecov-action@v3
  with:
    files: ./coverage/lcov.info
    flags: unittests
    fail_ci_if_error: true
```

### 3.3 Add Pre-commit Coverage Hooks

**Action**: Enforce coverage locally before commits

```bash
# .husky/pre-commit (or similar)
#!/bin/sh
pnpm test:fast # Uses vitest.config.fast.mjs with 80/20 tests
```

**Expected runtime**: <30s for fast suite (per CLAUDE.md Andon SLA)

### 3.4 Package-Specific Exemptions (If Justified)

**Process**: Document any packages requiring <80% coverage

Example exemption criteria:
- **Pure type definitions** (e.g., `@unrdf/types`)
- **Experimental packages** (mark as `private: true`)
- **CLI wrappers** (minimal logic, mostly delegation)

**Format** (in vitest.config.mjs):
```javascript
// Exemption: CLI wrapper with minimal logic
// Approved: 2025-12-20
// Reviewer: [Name]
// Rationale: 90% of code is argparse delegation
thresholds: {
  lines: 60, // Reduced from 80
  // ... must document why
}
```

---

## Implementation Checklist

### Week 1: Coverage Enforcement
- [ ] Day 1: Update core, hooks, federation, streaming configs
- [ ] Day 2: Update cli, composables, oxigraph, validation configs
- [ ] Day 3: Update remaining 10 packages
- [ ] Day 4: Run `pnpm test:coverage` - fix failures
- [ ] Day 5: Verify all packages ≥80% or document exemptions

### Week 2-3: Quality Improvements
- [ ] Rename *.spec.mjs → *.test.mjs (2 files)
- [ ] Add explicit timeouts to all configs
- [ ] Split CI vs local reporters
- [ ] Migrate experiment tests from Mocha/Chai
- [ ] Add OTEL validation for test claims

### Month 1-2: Advanced Features
- [ ] Create vitest.workspace.mjs
- [ ] Integrate Codecov or similar
- [ ] Add pre-commit coverage hooks
- [ ] Document exemption process
- [ ] Create coverage dashboard

---

## Success Metrics

### Coverage Quality

| Metric                     | Current | Target | Deadline  |
|----------------------------|---------|--------|-----------|
| Packages with thresholds   | 5%      | 100%   | Week 1    |
| Average coverage           | ~60%?   | ≥80%   | Week 3    |
| Packages at ≥80%           | ?       | 95%+   | Month 1   |
| Test execution time        | ?       | <5s/pkg| Month 2   |

### Process Quality

| Metric                     | Current | Target | Deadline  |
|----------------------------|---------|--------|-----------|
| Test file naming standard  | 98%     | 100%   | Week 2    |
| Config standardization     | Mixed   | 100%   | Week 1    |
| Timeout enforcement        | Partial | 100%   | Week 2    |
| CI integration             | Partial | 100%   | Month 1   |

---

## Risk Mitigation

### Risk 1: Existing Code Below 80%

**Impact**: Configs updated but tests fail
**Mitigation**:
1. Run baseline coverage report per package
2. Identify gaps (uncovered functions/branches)
3. Add tests incrementally OR document exemptions
4. Use `skipFull: true` temporarily during migration

**Timeline**: Allow 2-week grace period for test additions

### Risk 2: Performance Regression

**Impact**: Tests take >5s per package
**Mitigation**:
1. Profile slow tests with `vitest --reporter=verbose`
2. Use `test.concurrent` for independent tests
3. Mock expensive operations (DB, network)
4. Split unit vs integration tests

**Timeline**: Optimize during Week 2-3

### Risk 3: False Positives in Coverage

**Impact**: High coverage % but low quality tests
**Mitigation**:
1. Require OTEL validation (per CLAUDE.md)
2. Code review for test quality
3. Add mutation testing (optional, Month 2+)
4. Enforce test naming conventions (AAA pattern)

**Timeline**: Ongoing code review

---

## Rollback Plan

If standardization causes >50% test failures:

1. **Immediate**: Revert all configs to pre-update state
2. **Analysis**: Identify root cause (config issue vs code quality)
3. **Incremental**: Apply changes per-package with verification
4. **Timeline**: Re-attempt after 1-week investigation

---

## Approval Required

**Stakeholders**:
- Development Lead: Approve timeline
- QA Lead: Approve coverage thresholds
- DevOps Lead: Approve CI integration

**Sign-off**:
- [ ] Phase 1 approved (Coverage enforcement)
- [ ] Phase 2 approved (Quality improvements)
- [ ] Phase 3 approved (Advanced optimization)

---

## Appendix: Configuration Templates

### Template A: Standard Node Package

```javascript
import { defineConfig } from 'vitest/config';

export default defineConfig({
  test: {
    globals: true,
    environment: 'node',
    include: ['test/**/*.test.mjs'],
    testTimeout: 15000,
    hookTimeout: 5000,
    coverage: {
      provider: 'v8',
      reporter: process.env.CI ? ['json', 'lcov'] : ['text', 'json', 'html', 'lcov'],
      include: ['src/**/*.mjs'],
      exclude: ['node_modules/**', 'test/**', 'dist/**', '**/*.config.mjs'],
      thresholds: { lines: 80, functions: 80, branches: 80, statements: 80 },
      all: true,
      skipFull: false,
    },
    isolate: true,
    pool: 'forks',
    poolOptions: { forks: { singleFork: true } },
    reporter: process.env.CI ? ['json', 'github'] : ['verbose'],
    retry: process.env.CI ? 2 : 0,
  },
});
```

### Template B: Browser Package (jsdom)

```javascript
import { defineConfig } from 'vitest/config';

export default defineConfig({
  test: {
    globals: true,
    environment: 'jsdom', // Browser simulation
    include: ['test/**/*.test.mjs'],
    testTimeout: 10000,
    hookTimeout: 5000,
    coverage: {
      provider: 'v8',
      reporter: process.env.CI ? ['json', 'lcov'] : ['text', 'json', 'html', 'lcov'],
      include: ['src/**/*.mjs'],
      exclude: ['node_modules/**', 'test/**', 'dist/**', 'public/**'],
      thresholds: { lines: 80, functions: 80, branches: 80, statements: 80 },
      all: true,
    },
    setupFiles: ['./test/setup/browser-polyfills.mjs'], // Optional
  },
});
```

### Template C: Performance-Critical Package (RDF)

```javascript
import { defineConfig } from 'vitest/config';

export default defineConfig({
  test: {
    globals: true,
    environment: 'node',
    include: ['test/**/*.test.mjs'],
    testTimeout: 60000, // Extended for RDF operations
    hookTimeout: 5000,
    coverage: {
      provider: 'v8',
      reporter: process.env.CI ? ['json', 'lcov'] : ['text', 'json', 'html', 'lcov'],
      include: ['src/**/*.mjs'],
      exclude: ['node_modules/**', 'test/**', 'dist/**'],
      thresholds: { lines: 80, functions: 80, branches: 80, statements: 80 },
      all: true,
    },
    // Performance tests may need sequential execution
    concurrent: false,
    pool: 'forks',
    poolOptions: { forks: { singleFork: true } },
  },
});
```

---

**End of Standardization Plan**
