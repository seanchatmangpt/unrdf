# Production Blocker Action Items

**Status:** ❌ **4 CRITICAL BLOCKERS** preventing production deployment
**Generated:** 2025-12-20 21:30 PST
**Estimated Total Time:** 5-7 business days

---

## 🔴 CRITICAL BLOCKERS (Fix Immediately)

### 1. Build System Broken ⛔ [15 minutes]

**Issue:** esbuild configuration missing `--outdir` flag - cannot generate distributable packages

**Evidence:**
```bash
$ time pnpm run build
✘ [ERROR] Must use "outdir" when there are multiple input files
1 error
Duration: 0.524s
```

**Root Cause:**
```json
// package.json (INCORRECT)
"build": "esbuild $(ls -d packages/*/src/index.mjs 2>/dev/null | sed 's|/src/index.mjs||g' | tr '\\n' ' ')"
```

**Fix:**
```bash
# Option 1: Create build script
cat > scripts/build-all.mjs << 'EOF'
import { build } from 'esbuild';
import { glob } from 'glob';

const packages = glob.sync('packages/*/src/index.mjs');
await build({
  entryPoints: packages,
  outdir: 'dist',
  format: 'esm',
  bundle: true,
  splitting: true,
  outExtension: { '.js': '.mjs' }
});
EOF

# Update package.json
# "build": "node scripts/build-all.mjs"

# Option 2: Use unbuild per-package
# "build": "pnpm -r exec unbuild"
```

**Verification:**
```bash
time pnpm run build  # Must complete <30s
ls -la dist/          # Verify all packages built
```

---

### 2. Test Failures (98 tests failing) ⛔ [2-4 hours]

**Issue:** 16.4% test failure rate (need 100% pass)

**Evidence:**
```
hooks:     74 failures (error sanitizer tests)
streaming: 24 failures (N3→Oxigraph migration incomplete)
Total:     98/598 tests failing (83.6% pass rate)
```

#### 2a. Fix Vitest Config [10 minutes]

**Issue:**
```bash
$ pnpm run test
ReferenceError: src is not defined
at vitest.config.unified.mjs:4:1
```

**Fix:**
```bash
# Check vitest.config.unified.mjs line 4
# Remove any undefined 'src' variable reference
# Likely leftover from template or refactor
```

#### 2b. Fix Hooks Error Sanitizer [2-3 hours]

**Issue:**
```
Test Files  9 failed | 11 passed (20)
Tests       74 failed | 354 passed (428)

FAILURE: Expected "at connect" to be removed from sanitized errors
ACTUAL: "at connect" still present in output
```

**Fix:**
```bash
cd packages/hooks
npm test -- error-sanitizer.test.mjs --reporter=verbose

# Debug sanitization logic
# Check src/security/error-sanitizer.mjs
# Verify stack trace cleaning regex
```

#### 2c. Fix Streaming N3→Oxigraph Migration [2-3 hours]

**Issue:**
```
Test Files  4 failed | 2 passed (6)
Tests       24 failed | 66 passed | 8 skipped (98)

ERROR: store.removeQuad is not a function
CAUSE: Examples using N3.js API instead of Oxigraph
```

**Fix:**
```bash
cd packages/streaming

# Find N3.js API usage
grep -r "removeQuad" src/ examples/
grep -r "from 'n3'" src/ examples/

# Replace with Oxigraph API
# N3: store.removeQuad(quad)
# Oxigraph: store.delete(quad)

# Update examples/real-time-sync/src/index.mjs
sed -i 's/store.removeQuad/store.delete/g' examples/real-time-sync/src/index.mjs
```

**Verification:**
```bash
pnpm test  # Must show 100% pass rate
# Test Files  ALL passed
# Tests       ALL passed (0 failures)
```

---

### 3. Test Coverage Below 80% ⛔ [4-6 hours]

**Issue:** Federation at 60.48% (need ≥80%), consensus-manager at 18.61%

**Evidence:**
```
All files          |   59.96 |    57.26 |   58.06 |   60.48 |  ❌ BELOW 80%
consensus-manager  |   18.61 |     6.45 |   15.78 |   19.02 |  ❌ 81% untested
federation-coord   |   58.27 |    40.54 |   51.61 |   59.12 |  ❌ 41% untested
```

**Fix:**

#### 3a. Add consensus-manager.mjs tests [3-4 hours]
```bash
cd packages/federation/test

# Create comprehensive test file
cat > consensus-manager.test.mjs << 'EOF'
import { describe, it, expect } from 'vitest';
import { ConsensusManager } from '../src/federation/consensus-manager.mjs';

describe('ConsensusManager', () => {
  it('should initialize consensus protocol', async () => {
    const manager = new ConsensusManager({ protocol: 'raft' });
    expect(manager).toBeDefined();
  });

  // Add tests for all uncovered lines 143-588
  // - Election handling
  // - Vote counting
  // - Leader selection
  // - Network partitions
  // - Byzantine fault tolerance
});
EOF
```

#### 3b. Add federation-coordinator.mjs tests [2-3 hours]
```bash
# Create test file
cat > federation-coordinator.test.mjs << 'EOF'
import { describe, it, expect } from 'vitest';
import { FederationCoordinator } from '../src/federation/federation-coordinator.mjs';

describe('FederationCoordinator', () => {
  // Add tests for uncovered lines 378-381, 408-434
  // - Coordinator startup
  // - Peer discovery
  // - Message routing
  // - Failure handling
});
EOF
```

#### 3c. Add index.mjs tests [1 hour]
```bash
# Test export layer
cat > src/index.test.mjs << 'EOF'
import { describe, it, expect } from 'vitest';
import * as federation from './index.mjs';

describe('Federation exports', () => {
  it('should export all required modules', () => {
    expect(federation.FederationCoordinator).toBeDefined();
    expect(federation.ConsensusManager).toBeDefined();
    // ... test all exports
  });
});
EOF
```

**Verification:**
```bash
pnpm -C packages/federation test:coverage
# All files must show ≥80%
# consensus-manager: ≥80% (currently 18.61%)
# federation-coord:  ≥80% (currently 58.27%)
```

---

### 4. Circular Dependencies ⛔ [1-2 hours]

**Issue:** core ↔ oxigraph circular dependency breaks tree-shaking and bundling

**Evidence:**
```bash
$ pnpm run check:deps
❌ Found 2 circular dependency cycle(s):

1. @unrdf/core → (runtime) @unrdf/oxigraph ⇢ (dev) @unrdf/core
2. @unrdf/oxigraph ⇢ (dev) @unrdf/core → (runtime) @unrdf/oxigraph
```

**Root Cause:**
```json
// packages/core/package.json
{
  "dependencies": {
    "@unrdf/oxigraph": "workspace:*"  // Runtime dependency
  }
}

// packages/oxigraph/package.json
{
  "devDependencies": {
    "@unrdf/core": "workspace:*"  // Test dependency
  }
}
```

**Fix Option 1: Extract Test Utilities [RECOMMENDED]**
```bash
# Move shared test fixtures to test-utils
cd packages/test-utils
mkdir -p src/fixtures

# Move fixtures from oxigraph
mv ../oxigraph/test/fixtures/* src/fixtures/

# Update oxigraph package.json
cd ../oxigraph
# Remove: "@unrdf/core": "workspace:*" from devDependencies
# Add:    "@unrdf/test-utils": "workspace:*"

# Update test imports
sed -i 's|from "@unrdf/core/test|from "@unrdf/test-utils/fixtures|g' test/**/*.mjs
```

**Fix Option 2: Inline Test Fixtures**
```bash
# Copy minimal fixtures directly into oxigraph tests
cd packages/oxigraph/test

# Instead of importing from core, define inline:
cat > fixtures.mjs << 'EOF'
export const testQuads = [
  // Inline minimal test data
];
EOF
```

**Verification:**
```bash
pnpm run check:deps
# ✅ No circular dependencies found
# 0 cycles detected
```

---

## ⚠️ HIGH PRIORITY (Fix Before Release)

### 5. Linting Configuration Wrong [30 minutes]

**Issue:** Python linter configured for JavaScript project

**Evidence:**
```bash
$ pnpm run lint
> ruff check packages/*/src --config pyproject.toml

No preset version installed for command ruff
```

**Fix:**
```bash
# Install ESLint
pnpm add -D eslint @eslint/js eslint-config-prettier eslint-plugin-jsdoc

# Create config
cat > eslint.config.js << 'EOF'
import js from '@eslint/js';
import jsdoc from 'eslint-plugin-jsdoc';

export default [
  js.configs.recommended,
  {
    plugins: { jsdoc },
    rules: {
      'no-unused-vars': 'error',
      'no-console': 'warn',
      'jsdoc/require-jsdoc': 'error'
    }
  }
];
EOF

# Update package.json
# "lint": "eslint packages/*/src --config eslint.config.js"
# "lint:fix": "eslint packages/*/src --config eslint.config.js --fix"
```

**Verification:**
```bash
pnpm run lint  # Must show 0 violations
# ✓ No ESLint violations found
```

---

### 6. Missing Test Files [2-3 hours]

**Issue:** 6 packages have no test files

**Packages:**
- validation (0 tests) - MUST HAVE
- test-utils (0 tests) - Optional (infrastructure)
- nextra (tests exist but wrong location)
- kgn (tests exist but structure check failed)
- domain (0 tests)
- docs (Nuxt app - different structure)

**Fix:**
```bash
# Add validation package tests
cd packages/validation
mkdir -p test
cat > test/validation.test.mjs << 'EOF'
import { describe, it, expect } from 'vitest';
import * as validation from '../src/index.mjs';

describe('Validation exports', () => {
  it('should export validators', () => {
    expect(validation).toBeDefined();
  });
});
EOF

# Fix kgn test location
cd packages/kgn
# Tests exist at src/index.test.mjs
# Structure checker expects test/ or src/**/*.test.mjs
# Already compliant, check structure checker logic

# Add domain tests
cd packages/domain
mkdir -p test
cat > test/domain.test.mjs << 'EOF'
import { describe, it, expect } from 'vitest';
import * as domain from '../src/index.mjs';

describe('Domain exports', () => {
  it('should export domain models', () => {
    expect(domain).toBeDefined();
  });
});
EOF
```

---

## 📋 Verification Checklist

After completing all fixes, run full quality gate:

```bash
# 1. Structure
pnpm run check:structure
# Expected: 21/21 passed (or 19/21 if private packages excluded)

# 2. Build
time pnpm run build
# Expected: Completes <30s, all dist/ created

# 3. Linting
pnpm run lint
# Expected: 0 violations

# 4. Tests
pnpm run test
# Expected: 100% pass rate, 0 failures

# 5. Coverage
pnpm run test:coverage
# Expected: All packages ≥80%

# 6. Dependencies
pnpm run check:deps
# Expected: 0 circular dependencies

# 7. Exports (after build succeeds)
# Verify package.json "exports" fields
# Verify .d.ts files generated
```

---

## 📊 Progress Tracking

| Blocker | Status | Estimated | Actual | Owner |
|---------|--------|-----------|--------|-------|
| 1. Build Config | ❌ TODO | 15m | - | DevOps |
| 2a. Vitest Config | ❌ TODO | 10m | - | Testing |
| 2b. Hooks Tests | ❌ TODO | 2-3h | - | Testing |
| 2c. Streaming Tests | ❌ TODO | 2-3h | - | Testing |
| 3a. Consensus Coverage | ❌ TODO | 3-4h | - | Testing |
| 3b. Federation Coverage | ❌ TODO | 2-3h | - | Testing |
| 3c. Index Coverage | ❌ TODO | 1h | - | Testing |
| 4. Circular Deps | ❌ TODO | 1-2h | - | Architecture |
| 5. Linting Config | ❌ TODO | 30m | - | DevOps |
| 6. Missing Tests | ❌ TODO | 2-3h | - | Testing |

**Total Estimated:** 14-20 hours (2-3 working days)

---

## 🚀 Day 1 Action Plan (Critical Path)

**Goal:** Fix build, linting, and vitest config (blockers 1, 2a, 5)

### Morning (3 hours)
```
09:00-09:15  Fix esbuild configuration
09:15-09:25  Fix vitest.config.unified.mjs
09:25-09:55  Setup ESLint + remove ruff
09:55-10:00  Verify build + lint work
10:00-10:30  BREAK
10:30-12:00  Start hooks error sanitizer debugging
```

### Afternoon (4 hours)
```
13:00-15:00  Continue hooks error sanitizer fixes
15:00-15:30  BREAK
15:30-17:00  Start streaming N3→Oxigraph fixes
17:00-17:30  Daily checkpoint + status update
```

**Day 1 Exit Criteria:**
- [x] Build completes successfully
- [x] Linting shows 0 violations
- [x] Vitest config loads without errors
- [x] Hooks tests partially fixed (≥50% pass rate)

---

**Document Owner:** Production Validation Team
**Priority:** P0 (CRITICAL)
**Approval Required:** Cannot deploy until ALL blockers resolved
**Next Review:** After Day 1 EOD
