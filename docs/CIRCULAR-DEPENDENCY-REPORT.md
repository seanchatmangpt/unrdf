# Circular Dependency Report - UNRDF v5.0.1

**Report Date:** 2025-12-20
**Analysis Tool:** check-circular-deps.mjs + madge v8.0.0
**Total Packages Scanned:** 19

---

## Executive Summary

**Status:** ✗ CIRCULAR DEPENDENCIES DETECTED
**Total Cycles Found:** 2 (representing 1 unique bidirectional cycle)
**Affected Packages:** 2 (@unrdf/core, @unrdf/oxigraph)
**Severity:** HIGH - Core architecture violation blocking monorepo unification

### Quick Stats
- **Runtime Circular Dependencies:** 0 (madge scan clean)
- **Package-Level Circular Dependencies:** 1 unique cycle
- **Impact:** Build order ambiguity, test isolation issues, publish order problems

---

## Dependency Graph Overview

### Current Architecture (Text Visualization)

```
LAYER 0 (Leaf Packages - No Dependencies)
──────────────────────────────────────────────────
├─ validation
├─ test-utils
├─ nextra-docs
├─ domain
├─ docs
└─ atomvm

LAYER 2 (CIRCULAR - CRITICAL ISSUE)
──────────────────────────────────────────────────
├─ oxigraph
│  └─→ core (devDependency)
└─ core
   └─→ oxigraph (runtime dependency)

LAYER 3 (Depends on Core/Oxigraph)
──────────────────────────────────────────────────
├─ project-engine → core
├─ kgn → core, test-utils
├─ kgc-4d → core, oxigraph
├─ hooks → core, oxigraph
├─ engine-gateway → core, oxigraph
└─ dark-matter → core

LAYER 4 (Higher-Level Packages)
──────────────────────────────────────────────────
├─ streaming → core, hooks, oxigraph
├─ federation → core, hooks
├─ composables → core, streaming
└─ knowledge-engine → core, streaming

LAYER 5 (Hub Package)
──────────────────────────────────────────────────
└─ cli → core, federation, hooks, oxigraph, streaming
```

### Package Statistics

- **Total Packages:** 19
- **Leaf Packages (0 deps):** 6 (31.6%)
- **Mid-Level Packages (1-3 deps):** 12 (63.2%)
- **Hub Packages (4+ deps):** 1 (5.3%)

**Most Depended Upon:**
1. `@unrdf/core` - 12 dependents
2. `@unrdf/oxigraph` - 6 dependents
3. `@unrdf/hooks` - 3 dependents
4. `@unrdf/streaming` - 3 dependents
5. `@unrdf/test-utils` - 1 dependent

---

## Detailed Cycle Analysis

### Cycle 1: Core ↔ Oxigraph (Bidirectional Dependency)

**Cycle Path:**
```
@unrdf/core → (runtime) @unrdf/oxigraph ⇢ (dev) @unrdf/core
```

**Normalized Representation (Same Cycle):**
```
@unrdf/oxigraph ⇢ (dev) @unrdf/core → (runtime) @unrdf/oxigraph
```

#### Why This Cycle Exists

**Runtime Dependency:** `@unrdf/core` → `@unrdf/oxigraph`

Location: `packages/core/package.json`
```json
{
  "dependencies": {
    "@unrdf/oxigraph": "workspace:*"
  }
}
```

**Reason:** Core package uses Oxigraph as the backing store implementation:
- `src/sparql/executor-sync.mjs` imports `OxigraphStore`
- `src/rdf/store.mjs` imports `createStore`, `dataFactory`
- 15+ files import `dataFactory` from oxigraph
- Core is a **high-level facade** that depends on oxigraph as **storage engine**

**Dev Dependency:** `@unrdf/oxigraph` → `@unrdf/core`

Location: `packages/oxigraph/package.json`
```json
{
  "devDependencies": {
    "@unrdf/core": "workspace:*"
  }
}
```

**Reason:** Oxigraph tests **incorrectly** import from core for test fixtures:
- NONE found in current test scan (tests use local imports only)
- **Likely artifact:** devDependency may be unused/stale

#### Impact Assessment

**BUILD IMPACT: HIGH**
- Circular dependencies create ambiguous build order
- `pnpm` may fail to determine which package to build first
- Workspace resolution may fail in CI/CD pipelines
- Publication order cannot be determined

**RUNTIME IMPACT: ZERO**
- No runtime import cycles detected by madge
- Code executes correctly at runtime
- Only affects development/build time

**TEST IMPACT: MEDIUM**
- Test isolation compromised
- Cannot test oxigraph independently of core
- Harder to mock dependencies in tests

**ARCHITECTURE IMPACT: HIGH**
- Violates layered architecture principles
- Core should depend on oxigraph, NOT vice versa
- Makes code harder to understand and maintain

---

## Root Cause Analysis

### Architectural Misalignment

The cycle exists because of a **layering violation**:

**Intended Architecture (Correct):**
```
┌─────────────────────────┐
│   Application Layer     │  ← cli, federation, streaming
│   (@unrdf/cli, etc.)    │
└────────────┬────────────┘
             │
┌────────────▼────────────┐
│     Core Layer          │  ← High-level RDF operations
│    (@unrdf/core)        │
└────────────┬────────────┘
             │
┌────────────▼────────────┐
│   Storage Layer         │  ← Low-level SPARQL engine
│  (@unrdf/oxigraph)      │
└─────────────────────────┘
```

**Actual Architecture (BROKEN):**
```
┌─────────────────────────┐
│     Core Layer          │
│    (@unrdf/core)        │ ────────┐
└────────────┬────────────┘         │
             │                       │ (devDep)
             │ (runtime dep)         │
             ▼                       │
┌─────────────────────────┐         │
│   Storage Layer         │         │
│  (@unrdf/oxigraph)      │ ◄───────┘
└─────────────────────────┘
    CIRCULAR DEPENDENCY!
```

### Specific Issues

1. **Oxigraph devDependency on Core:** Unnecessary
   - Tests in `packages/oxigraph/test/` do NOT import from `@unrdf/core`
   - All tests use local imports: `from '../src/index.mjs'`
   - DevDependency appears to be **dead code** in package.json

2. **No Shared Test Utilities:**
   - No evidence of shared test fixtures requiring core
   - Tests are self-contained

---

## Proposed Fix: Remove Stale DevDependency

### Strategy: Simple Removal (Estimated Time: 5 minutes)

The fix is **trivial** because the devDependency is unused:

**Step 1: Remove devDependency**
```diff
--- a/packages/oxigraph/package.json
+++ b/packages/oxigraph/package.json
@@ -44,7 +44,6 @@
   },
   "devDependencies": {
     "@types/node": "^24.10.1",
-    "@unrdf/core": "workspace:*",
     "vitest": "^4.0.15"
   },
```

**Step 2: Verify no imports exist**
```bash
# Should return NO results
grep -r "from '@unrdf/core" packages/oxigraph/test/
grep -r "from '@unrdf/core" packages/oxigraph/src/
```

**Step 3: Re-run circular dependency check**
```bash
pnpm run check:deps
# Expected: ✅ No circular dependencies detected!
```

**Step 4: Verify tests still pass**
```bash
pnpm -C packages/oxigraph test
# Expected: All tests passing
```

### Alternative Fix (If Dependency Was Actually Used)

**Only if** tests genuinely needed core (NOT the case here):

#### Option A: Extract to @unrdf/test-utils
```javascript
// packages/test-utils/src/fixtures.mjs
export const createTestFixtures = () => ({
  // Shared test data
});

// packages/oxigraph/test/basic.test.mjs
import { createTestFixtures } from '@unrdf/test-utils';
```

#### Option B: Inline Test Fixtures
```javascript
// packages/oxigraph/test/fixtures.mjs
// Copy minimal data needed for tests (no external deps)
export const testData = {
  // Duplicated but isolated
};
```

#### Option C: Use Dependency Injection
```javascript
// packages/oxigraph/src/store.mjs
export const createStore = (config = {}) => {
  // Don't import core directly
  // Let consumer pass dependencies
};
```

---

## Verification Results

### Scan 1: Package-Level Dependencies
**Tool:** `scripts/check-circular-deps.mjs`
**Result:** ✗ 2 cycles found (same bidirectional dependency)

```
❌ Found 2 circular dependency cycle(s):

1. @unrdf/core → (runtime) @unrdf/oxigraph ⇢ (dev) @unrdf/core
2. @unrdf/oxigraph ⇢ (dev) @unrdf/core → (runtime) @unrdf/oxigraph
```

### Scan 2: Runtime Import Dependencies (Core)
**Tool:** `madge --circular packages/core/src`
**Result:** ✅ No circular dependencies found

```
Processed 3 files (248ms) (1 warning)
✔ No circular dependency found!
```

### Scan 3: Runtime Import Dependencies (Oxigraph)
**Tool:** `madge --circular packages/oxigraph/src`
**Result:** ✅ No circular dependencies found

```
Processed 3 files
✔ No circular dependency found!
```

### Scan 4: Test File Import Analysis
**Tool:** `grep -r "from '@unrdf/core" packages/oxigraph/test/`
**Result:** ✅ No imports found (0 results)

**Files Analyzed:**
- `test/application-jtbd.test.mjs` - Uses `from '../src/index.mjs'`
- `test/comparison.test.mjs` - Uses `from '../src/index.mjs'`
- `test/basic.test.mjs` - Uses local imports only
- `test/benchmark.test.mjs` - Uses local imports only

---

## Implementation Plan

### Phase 1: Immediate Fix (5 minutes)

**Task 1.1:** Remove stale devDependency
- File: `packages/oxigraph/package.json`
- Action: Delete `"@unrdf/core": "workspace:*"` from devDependencies
- Risk: NONE (dependency is unused)

**Task 1.2:** Verify with automated checks
```bash
pnpm install                    # Update lockfile
pnpm run check:deps            # Should show 0 cycles
pnpm -C packages/oxigraph test # Should pass
```

### Phase 2: Validation (10 minutes)

**Task 2.1:** Run full test suite
```bash
pnpm test                      # All packages
pnpm test:coverage            # Coverage should be unchanged
```

**Task 2.2:** Verify build order
```bash
pnpm -r --workspace-concurrency=1 build
# Should build in correct order: oxigraph → core → others
```

**Task 2.3:** Independent scans (confirmation)
```bash
# Run twice to confirm deterministic results
pnpm run check:deps
pnpm run check:deps
# Both should show: ✅ No circular dependencies detected!
```

### Phase 3: Documentation (5 minutes)

**Task 3.1:** Update this report with fix status
**Task 3.2:** Document architecture decisions
**Task 3.3:** Add to CONTRIBUTING.md:

```markdown
## Dependency Guidelines

- **@unrdf/oxigraph** is the storage layer (no upstream dependencies)
- **@unrdf/core** depends on oxigraph (high-level facade)
- **Application packages** depend on core (cli, federation, etc.)
- **NEVER** add devDependency from lower layer to higher layer
```

---

## Post-Fix Verification Checklist

**Before declaring complete, verify ALL items:**

- [ ] Removed `@unrdf/core` from `packages/oxigraph/package.json` devDependencies
- [ ] Ran `pnpm install` to update lockfile
- [ ] Ran `pnpm run check:deps` → Result: ✅ 0 cycles
- [ ] Ran `pnpm run check:deps` again → Result: ✅ 0 cycles (deterministic)
- [ ] Ran `madge --circular packages/core/src` → Result: ✅ No cycles
- [ ] Ran `madge --circular packages/oxigraph/src` → Result: ✅ No cycles
- [ ] Ran `pnpm -C packages/oxigraph test` → Result: ✅ All tests pass
- [ ] Ran `pnpm test` → Result: ✅ All packages tests pass
- [ ] Ran `pnpm -r build` → Result: ✅ Builds in correct order
- [ ] Verified `pnpm-lock.yaml` updated correctly
- [ ] No new warnings or errors introduced
- [ ] Documentation updated

**Final Confirmation:**
- [ ] Can publish packages independently: oxigraph → core → others
- [ ] Can test packages in isolation
- [ ] Build order is deterministic
- [ ] Zero circular dependencies detected

---

## Architecture Recommendations

### Maintain Layered Architecture

**Layer 1: Storage Engines** (No internal dependencies)
- `@unrdf/oxigraph` - SPARQL engine adapter

**Layer 2: Core Abstractions** (Depends on Layer 1 only)
- `@unrdf/core` - RDF operations, SPARQL facade
- `@unrdf/validation` - Schema validation

**Layer 3: Domain Services** (Depends on Layer 1-2)
- `@unrdf/hooks` - Event system
- `@unrdf/streaming` - Change feeds
- `@unrdf/federation` - Distributed coordination

**Layer 4: Applications** (Depends on Layer 1-3)
- `@unrdf/cli` - Command-line tools
- `@unrdf/knowledge-engine` - Higher-level abstractions

### Dependency Rules

1. **One-way dependencies only:** Lower layers NEVER depend on higher layers
2. **No peer layer dependencies:** Packages in same layer don't depend on each other
3. **Runtime vs devDependencies:** Be intentional about dependency type
4. **Test isolation:** Tests should NOT import from sibling packages
5. **Shared test utilities:** Extract to `@unrdf/test-utils` if needed

### Prevention

**Add to CI Pipeline:**
```yaml
# .github/workflows/ci.yml
- name: Check Circular Dependencies
  run: |
    pnpm run check:deps
    if [ $? -ne 0 ]; then
      echo "❌ Circular dependencies detected!"
      exit 1
    fi
```

**Add to Pre-commit Hook:**
```bash
#!/bin/bash
# .husky/pre-commit
pnpm run check:deps || {
  echo "⚠️  Circular dependencies detected. Commit blocked."
  exit 1
}
```

---

## Appendix: Tooling Details

### check-circular-deps.mjs
- **Purpose:** Detect package.json circular dependencies (dev + runtime)
- **Algorithm:** Depth-first search with cycle detection
- **Coverage:** All `@unrdf/*` packages in workspace
- **Limitations:** Does NOT detect runtime import cycles (use madge for that)

### madge
- **Version:** 8.0.0
- **Purpose:** Detect runtime circular imports in JavaScript/TypeScript
- **Usage:** `npx madge --circular --extensions mjs,js <directory>`
- **Coverage:** Analyzes actual `import`/`require` statements
- **Limitations:** Does NOT analyze package.json dependencies

### Combined Approach
Use BOTH tools for comprehensive detection:
1. `check-circular-deps.mjs` for package architecture
2. `madge` for runtime import cycles

---

## Summary

**Current State:**
- ✗ 1 circular dependency cycle detected (core ↔ oxigraph)
- ✅ 0 runtime import cycles (madge clean)
- ✗ BLOCKER for production deployment

**Required Action:**
- Remove `@unrdf/core` from `packages/oxigraph/package.json` devDependencies
- Verify with `pnpm run check:deps` (2 independent scans)
- Confirm all tests pass

**Estimated Time to Fix:**
- Implementation: 5 minutes
- Verification: 10 minutes
- Documentation: 5 minutes
- **Total: 20 minutes**

**Post-Fix State:**
- ✅ 0 circular dependencies
- ✅ Clean layered architecture
- ✅ Independent package publishing
- ✅ Production-ready dependency graph

---

**Report Generated:** 2025-12-20
**Next Review:** After fix implementation
**Owner:** UNRDF Core Team
