# UNRDF Test Configuration Audit Report

**Date**: 2025-12-20
**Auditor**: Code Quality Analyzer
**Scope**: UNRDF v5.0.1 Monorepo Test Infrastructure

---

## Executive Summary

**Test Framework Status**: ✅ **Standardized on Vitest 4.0.15**

- **Total Test Files**: 111 test files (109 `.test.mjs`, 2 `.spec.mjs`)
- **Vitest Configs**: 19 package-level configurations
- **Coverage Standard**: **INCONSISTENT** - Only 1/19 packages enforce ≥80% thresholds
- **Test Runner**: 100% Vitest (no Jest, no Mocha in source packages)

**Critical Finding**: 🚨 **Coverage enforcement is missing in 94.7% of packages** (18/19 lack thresholds)

---

## 1. Test Configuration Inventory

### 1.1 Vitest Configuration Files

Total: **19 vitest.config.* files** across packages:

```
✅ packages/atomvm/vitest.config.mjs
✅ packages/cli/vitest.config.mjs
✅ packages/composables/vitest.config.local.mjs
✅ packages/composables/vitest.config.mjs
✅ packages/core/vitest.config.mjs
✅ packages/dark-matter/vitest.config.mjs
✅ packages/docs/vitest.config.ts
✅ packages/domain/vitest.config.mjs
✅ packages/engine-gateway/vitest.config.mjs
✅ packages/federation/vitest.config.mjs
✅ packages/hooks/vitest.config.mjs
✅ packages/kgc-4d/vitest.config.mjs
✅ packages/kgn/vitest.config.js
✅ packages/knowledge-engine/vitest.config.mjs
✅ packages/oxigraph/vitest.config.mjs
✅ packages/project-engine/vitest.config.mjs
✅ packages/streaming/vitest.config.mjs
✅ packages/test-utils/vitest.config.mjs
✅ packages/validation/vitest.config.mjs
```

**Additional**: Root-level `vitest.config.fast.mjs` for CI/CD (80/20 fast testing)

### 1.2 Jest Configuration Files

**Count**: 0 in source packages
**Location**: Only in `node_modules/` (external dependencies)

### 1.3 Mocha Configuration Files

**Count**: 0 in source packages
**Location**: None found (Chai detected only in experiment subdirectories)

---

## 2. Test File Distribution by Package

```
Package                 | Test Files | Notes
------------------------|------------|------------------------------------------
atomvm                  |     26     | jsdom environment, 39% coverage threshold
cli                     |      3     | No coverage thresholds
composables             |      3     | Has local config variant
core                    |      9     | 60s timeout for RDF operations
dark-matter             |      3     | -
engine-gateway          |      1     | -
federation              |      6     | Real-time coordination tests
hooks                   |     20     | Largest test suite, v8 coverage
kgc-4d                  |     24     | Second-largest test suite
knowledge-engine        |      3     | -
oxigraph                |      4     | Native RDF store integration
project-engine          |      1     | Domain inference tests
streaming               |      6     | Change feed + WebSocket tests
validation              |      ?     | Config exists, files not counted
test-utils              |      ?     | Shared test utilities
domain                  |      ?     | -
docs                    |      ?     | TypeScript config (.ts)
kgn                     |      ?     | Uses .js config (not .mjs)
------------------------|------------|------------------------------------------
TOTAL                   |    111+    | (109 .test.mjs, 2 .spec.mjs)
```

---

## 3. Test Script Analysis

### 3.1 Common Script Patterns

All packages using Vitest follow similar patterns:

```json
{
  "scripts": {
    "test": "vitest run --coverage",
    "test:fast": "vitest run --coverage",
    "test:watch": "vitest --coverage",
    "build": "unbuild || node build.config.mjs",
    "lint": "eslint src/ test/ --max-warnings=0",
    "format": "prettier --write src/ test/"
  }
}
```

### 3.2 Script Variants

| Script Variant       | Packages | Notes                              |
|----------------------|----------|------------------------------------|
| `vitest run --coverage` | 10   | Standard coverage-enabled run      |
| `vitest run --no-coverage` | 1 | @unrdf/core (performance focused)  |
| Browser testing      | 1        | @unrdf/hooks (Playwright browsers) |
| Cucumber BDD         | ?        | `@amiceli/vitest-cucumber` in deps |
| Benchmark scripts    | 2        | @unrdf/hooks, others               |

### 3.3 Root-Level Test Scripts

From `/Users/sac/unrdf/package.json`:

```json
{
  "scripts": {
    "test": "pnpm -r test",
    "test:fast": "pnpm -r test:fast",
    "test:watch": "pnpm -r test:watch",
    "test:coverage": "pnpm -r test -- --coverage",
    "test:core": "pnpm -C packages/core test",
    "test:hooks": "pnpm -C packages/hooks test",
    "test:federation": "pnpm -C packages/federation test",
    "test:streaming": "pnpm -C packages/streaming test",
    "test:browser": "pnpm -C packages/browser test",
    "test:cli": "pnpm -C packages/cli test",
    "test:knowledge-engine": "pnpm -C packages/knowledge-engine test"
  }
}
```

---

## 4. Coverage Configuration Analysis

### 4.1 Coverage Provider

**100% V8 Coverage** across all packages:

```javascript
coverage: {
  provider: 'v8',
  reporter: ['text', 'json', 'html'],
  exclude: ['node_modules/**', 'test/**', 'dist/**']
}
```

### 4.2 Coverage Thresholds

**CRITICAL FINDING**: Only 2/19 configs enforce coverage thresholds:

#### ✅ WITH Thresholds (2 packages)

1. **Root `vitest.config.fast.mjs`** (80/20 CI/CD):
   ```javascript
   thresholds: {
     global: {
       branches: 80,
       functions: 80,
       lines: 80,
       statements: 80,
     }
   }
   ```

2. **`packages/atomvm/vitest.config.mjs`**:
   ```javascript
   thresholds: {
     lines: 39,
     functions: 38,
     branches: 36,
     statements: 39,
   }
   ```
   ⚠️ **BELOW STANDARD** (39% vs 80% required)

#### ❌ WITHOUT Thresholds (18 packages)

All other packages have coverage enabled but **NO enforcement**:

- packages/cli
- packages/composables
- packages/core
- packages/dark-matter
- packages/domain
- packages/engine-gateway
- packages/federation
- packages/hooks
- packages/kgc-4d
- packages/kgn
- packages/knowledge-engine
- packages/oxigraph
- packages/project-engine
- packages/streaming
- packages/test-utils
- packages/validation
- packages/docs
- packages/browser (if exists)

---

## 5. Test Environment Configuration

| Package           | Environment | Timeout   | Special Config               |
|-------------------|-------------|-----------|------------------------------|
| atomvm            | jsdom       | 10s       | Browser simulation           |
| core              | node        | 60s       | RDF operation performance    |
| hooks             | node        | Default   | Browser test variants        |
| federation        | node        | Default   | Distributed coordination     |
| streaming         | node        | Default   | WebSocket real-time tests    |
| others            | node        | Default   | Standard Node.js             |

---

## 6. Dependencies Analysis

### 6.1 Test Framework Dependencies

From root `package.json`:

```json
{
  "devDependencies": {
    "vitest": "^4.0.15",
    "@vitest/browser": "^4.0.15",
    "@vitest/coverage-v8": "^4.0.15",
    "@vitest/ui": "^4.0.15",
    "playwright": "^1.57.0",
    "jsdom": "^27.2.0"
  }
}
```

All packages inherit from workspace root - **NO conflicting versions**.

### 6.2 Mocha/Chai Detection

- **Mocha**: 0 occurrences in package.json files (only in experiments)
- **Chai**: Found in atomvm experiments (docker-swarm-messaging, chaos-cluster)
  - These are experimental subdirectories, NOT production packages
  - Should migrate to Vitest expect() API

---

## 7. Test File Patterns

### 7.1 Naming Conventions

| Pattern         | Count | Standard? |
|-----------------|-------|-----------|
| `*.test.mjs`    | 109   | ✅ YES    |
| `*.spec.mjs`    | 2     | ⚠️ Outlier |

**Recommendation**: Rename `*.spec.mjs` to `*.test.mjs` for consistency.

### 7.2 Test Organization

Most packages follow standard structure:

```
packages/*/
├── src/
│   └── *.mjs (source files)
├── test/
│   ├── *.test.mjs (unit tests)
│   ├── integration/
│   │   └── *.test.mjs
│   └── fixtures/
└── vitest.config.mjs
```

---

## 8. Quality Gate Comparison

### Current State vs CLAUDE.md Requirements

| Requirement                | Root Config | Package Configs | Status       |
|----------------------------|-------------|-----------------|--------------|
| 80%+ coverage threshold    | ✅ (fast)   | ❌ (18/19)      | **FAIL**     |
| Vitest standardization     | ✅          | ✅              | **PASS**     |
| V8 coverage provider       | ✅          | ✅              | **PASS**     |
| Test file pattern (.test)  | ✅          | ⚠️ (2 .spec)    | **WARN**     |
| No Jest/Mocha              | ✅          | ✅              | **PASS**     |
| Timeout enforcement        | ✅ (15s)    | Mixed           | **WARN**     |

---

## 9. Findings Summary

### ✅ Strengths

1. **Unified Test Runner**: 100% Vitest 4.0.15 (no fragmentation)
2. **Modern Tooling**: V8 coverage, ESM-first, fast execution
3. **Comprehensive Root Scripts**: Easy package-specific testing
4. **Fast CI Config**: `vitest.config.fast.mjs` with 80/20 optimization
5. **Browser Testing Support**: Playwright + jsdom available

### 🚨 Critical Issues

1. **Coverage Enforcement Missing**: 94.7% of packages have NO thresholds
   - **Impact**: Code can be committed with 0% coverage
   - **Risk**: Quality degradation, untested features in production

2. **Atomvm Below Standard**: 39% coverage threshold (needs 80%)
   - **Gap**: 41 percentage points below requirement
   - **Action**: Increase tests or justify exemption

3. **Inconsistent File Naming**: 2 files use `.spec.mjs` instead of `.test.mjs`

### ⚠️ Warnings

1. **Timeout Variance**: Core uses 60s, atomvm 10s, others default (unclear)
2. **Experiment Dependencies**: Mocha/Chai in experiments (technical debt)
3. **Coverage Reporters**: All use `['text', 'json', 'html']` - no CI-optimized variant

---

## 10. Standardization Plan

See next section: [TEST-STANDARDIZATION-PLAN.md](#standardization-plan)

---

## Appendix A: File Count Verification

```bash
# Commands run:
find packages -type f -name "*.test.mjs" | wc -l
# Output: 109

find packages -type f -name "*.spec.mjs" | wc -l
# Output: 2

find packages -name "vitest.config.*" | wc -l
# Output: 19

grep -r "vitest" packages/*/package.json | grep -c "vitest"
# Output: 13 unique packages with vitest dependency
```

---

## Appendix B: Sample Configurations

### Standard Package Config (packages/hooks/vitest.config.mjs)

```javascript
import { defineConfig } from 'vitest/config';

export default defineConfig({
  test: {
    globals: true,
    environment: 'node',
    coverage: {
      provider: 'v8',
      reporter: ['text', 'json', 'html'],
      include: ['src/**/*.mjs'],
      exclude: ['node_modules/**', 'test/**', 'dist/**'],
    },
  },
});
```

### Root Fast Config (vitest.config.fast.mjs)

```javascript
export default defineConfig({
  test: {
    pool: "forks",
    poolOptions: { forks: { singleFork: true } },
    concurrent: false,
    testTimeout: 15_000,
    coverage: {
      provider: "v8",
      reporter: ["text"],
      thresholds: {
        global: {
          branches: 80,
          functions: 80,
          lines: 80,
          statements: 80,
        }
      },
      all: true,
    },
    include: [
      "test/diff.test.mjs",
      "test/knowledge-engine/parse-contract.test.mjs",
      // ... 80/20 subset
    ],
  },
});
```

---

**End of Audit Report**
