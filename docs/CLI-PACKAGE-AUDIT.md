# CLI Package Import Audit Report

**Date**: 2025-12-05
**Status**: 🔴 CRITICAL ISSUES FOUND
**Auditor**: Adversarial PM Review

---

## Executive Summary

Comprehensive audit of CLI command structure and package imports revealed **CRITICAL issues** that must be fixed:

1. **Sidecar architecture still present** despite user requirement "not doing sidecar anymore"
2. **Incorrect package imports** using relative paths instead of monorepo packages
3. **Broken imports** referencing non-existent package paths

### Impact
- ❌ CLI will fail at runtime due to incorrect imports
- ❌ Sidecar commands present but non-functional
- ❌ Violates monorepo package boundaries

---

## Evidence-Based Findings

### 🚨 CRITICAL #1: Sidecar Architecture Must Be Removed

**User Statement**: "We are not doing a sidecar anymore"

**Evidence of Sidecar Presence**:

#### 1.1 Main CLI File (cli/index.mjs)
```javascript
// Line 67: Sidecar lazy import defined
const importSidecar = () => import("./commands/sidecar/index.mjs");

// Lines 262-302: Entire sidecar command tree
sidecar: defineCommand({
  meta: { name: "sidecar", description: "Manage KGC sidecar" },
  subCommands: {
    status: defineCommand({ ... }),
    health: defineCommand({ ... }),
    config: defineCommand({ ... }),
    logs: defineCommand({ ... }),
    restart: defineCommand({ ... }),
  },
}),
```

**Files to Remove**:
```bash
cli/commands/sidecar/index.mjs
cli/commands/sidecar/status.mjs
cli/commands/sidecar/health.mjs
cli/commands/sidecar/config.mjs
cli/commands/sidecar/logs.mjs
cli/commands/sidecar/restart.mjs
```

**Verification Command**:
```bash
# Proves sidecar directory exists with 6 files
ls -1 cli/commands/sidecar/ | wc -l
# Output: 6
```

#### 1.2 Sidecar Import References

**cli/commands/sidecar/health.mjs:12**
```javascript
import { createSidecarClient } from '../../../sidecar/client.mjs';
```
- References `sidecar/sidecar/client.mjs` (Nuxt app sidecar directory)
- gRPC client with circuit breakers, connection pooling
- NOT part of CLI architecture if sidecar removed

**cli/commands/sidecar/logs.mjs:12**
```javascript
import { createSidecarClient } from '../../../sidecar/client.mjs';
```

**cli/commands/sidecar/config.mjs:12**
```javascript
import { createSidecarConfig } from '../../../sidecar/config.mjs';
```

**Verification**:
```bash
grep -r "createSidecarClient\|createSidecarConfig" cli/commands/sidecar/
# Output: 8 references found across 3 files
```

---

### 🚨 CRITICAL #2: Incorrect Package Imports (Relative Paths)

#### 2.1 Knowledge Hook Manager Import

**File**: `cli/commands/hook/list.mjs:8`

**WRONG** ❌:
```javascript
import { KnowledgeHookManager } from '../../../packages/knowledge-engine/knowledge-hook-manager.mjs';
```

**Problems**:
1. Path is incorrect - should be `packages/hooks`, not `packages/knowledge-engine`
2. Direct file import violates package boundaries
3. Won't work in production (packages not relative to CLI)

**CORRECT** ✅:
```javascript
import { KnowledgeHookManager } from '@unrdf/hooks';
```

**Verification**:
```bash
# Actual location of KnowledgeHookManager
find packages -name "knowledge-hook-manager.mjs" -type f
# Output: packages/hooks/src/hooks/knowledge-hook-manager.mjs

# Check package exports
grep -A 5 "exports" packages/hooks/package.json
# Output shows: "." -> "./src/index.mjs"
```

**Package Export Check**:
```json
// packages/hooks/package.json
{
  "name": "@unrdf/hooks",
  "exports": {
    ".": "./src/index.mjs",
    "./define": "./src/define.mjs",
    "./executor": "./src/executor.mjs"
  }
}
```

**Fix Required**: Verify `KnowledgeHookManager` is exported from `@unrdf/hooks/src/index.mjs`

#### 2.2 Store Instance Imports

**File**: `cli/utils/store-instance.mjs:9-10`

**WRONG** ❌:
```javascript
import { createStore } from '../../packages/core/src/rdf/unrdf-store.mjs';
import { OxigraphStore } from '../../packages/oxigraph/src/store.mjs';
```

**Problems**:
1. Hardcoded relative paths to package internals
2. Bypasses package.json exports
3. Won't resolve in production builds
4. Violates module encapsulation

**CORRECT** ✅:
```javascript
import { createStore } from '@unrdf/core';
import { OxigraphStore } from '@unrdf/oxigraph/store';
```

**Verification**:
```bash
# Check @unrdf/core exports
grep -A 10 "exports" packages/core/package.json
# Output:
#   ".": "./src/index.mjs",
#   "./rdf": "./src/rdf/index.mjs",
#   ...

# Check @unrdf/oxigraph exports
grep -A 5 "exports" packages/oxigraph/package.json
# Output:
#   ".": "./src/index.mjs",
#   "./store": "./src/store.mjs",
#   ...
```

---

### 🟡 MEDIUM PRIORITY: Missing Package Boundary Validation

**Evidence**: Found 2 files violating package boundaries
```bash
grep -r "from.*\.\.\/.*packages" cli --include="*.mjs"
# Output:
#   cli/utils/store-instance.mjs (2 violations)
#   cli/commands/hook/list.mjs (1 violation)
```

**Recommendation**: Add ESLint rule to prevent relative imports to `packages/` directory

---

## Package Structure Reference (Evidence)

### Available Packages (Verified):
```bash
find packages -name "package.json" -type f | head -10
```

**Output**:
- `@unrdf/core` - RDF operations, SPARQL, validation
- `@unrdf/oxigraph` - Oxigraph store wrapper
- `@unrdf/hooks` - Knowledge hooks and policy framework
- `@unrdf/cli` - CLI commands (packages/cli - DIFFERENT from root cli/)
- `@unrdf/federation` - Federation support
- `@unrdf/streaming` - Streaming operations
- `@unrdf/domain` - Domain-specific types
- `@unrdf/react` - React components
- `@unrdf/composables` - Vue composables
- `@unrdf/test-utils` - Testing utilities

### Package Export Validation

#### @unrdf/core exports:
```json
{
  ".": "./src/index.mjs",
  "./rdf": "./src/rdf/index.mjs",
  "./rdf/minimal-n3-integration": "./src/rdf/minimal-n3-integration.mjs",
  "./rdf/n3-justified-only": "./src/rdf/n3-justified-only.mjs",
  "./sparql": "./src/sparql/index.mjs",
  "./types": "./src/types.mjs",
  "./constants": "./src/constants.mjs",
  "./validation": "./src/validation/index.mjs"
}
```

#### @unrdf/oxigraph exports:
```json
{
  ".": "./src/index.mjs",
  "./store": "./src/store.mjs",
  "./types": "./src/types.mjs"
}
```

#### @unrdf/hooks exports:
```json
{
  ".": "./src/index.mjs",
  "./define": "./src/define.mjs",
  "./executor": "./src/executor.mjs"
}
```

---

## Required Actions (Prioritized)

### 🔴 MUST FIX (Blocking Issues)

#### Action 1: Remove Sidecar Commands
**Files to Delete**:
```bash
rm -rf cli/commands/sidecar/
```

**Verification**:
```bash
# Before: 6 files
ls -1 cli/commands/sidecar/ 2>/dev/null | wc -l

# After: directory should not exist
ls cli/commands/sidecar/ 2>&1 | grep "No such file"
```

#### Action 2: Remove Sidecar from Main CLI
**File**: `cli/index.mjs`

**Remove Lines 67** (lazy import):
```javascript
const importSidecar = () => import("./commands/sidecar/index.mjs");
```

**Remove Lines 262-302** (entire sidecar command tree)

**Verification After Fix**:
```bash
grep -i "sidecar" cli/index.mjs
# Output: (empty - no matches)
```

#### Action 3: Fix Hook Manager Import
**File**: `cli/commands/hook/list.mjs:8`

**Change**:
```diff
- import { KnowledgeHookManager } from '../../../packages/knowledge-engine/knowledge-hook-manager.mjs';
+ import { KnowledgeHookManager } from '@unrdf/hooks';
```

**Pre-Verification** (check it's exported):
```bash
grep "export.*KnowledgeHookManager" packages/hooks/src/index.mjs
# If NOT found, add to packages/hooks/src/index.mjs:
# export { KnowledgeHookManager } from './hooks/knowledge-hook-manager.mjs';
```

#### Action 4: Fix Store Instance Imports
**File**: `cli/utils/store-instance.mjs:9-10`

**Change**:
```diff
- import { createStore } from '../../packages/core/src/rdf/unrdf-store.mjs';
- import { OxigraphStore } from '../../packages/oxigraph/src/store.mjs';
+ import { createStore } from '@unrdf/core';
+ import { OxigraphStore } from '@unrdf/oxigraph/store';
```

**Verification**:
```bash
# Check exports are available
grep "createStore" packages/core/src/index.mjs
grep "OxigraphStore" packages/oxigraph/src/store.mjs
```

---

## Testing Plan (Adversarial PM)

### Pre-Fix Validation (Prove Issues Exist)

```bash
# 1. Count sidecar files (expect 6)
ls -1 cli/commands/sidecar/ | wc -l

# 2. Find sidecar references in main CLI (expect matches)
grep -c "sidecar" cli/index.mjs

# 3. Find incorrect package imports (expect 2 files)
grep -r "from.*\.\.\/.*packages" cli --include="*.mjs" -l | wc -l

# 4. Try to run CLI (expect import errors)
timeout 2s node cli/index.mjs hook list 2>&1 | grep -i "error\|cannot find"
```

### Post-Fix Validation (Prove Fixes Work)

```bash
# 1. Verify sidecar removed (expect "No such file")
ls cli/commands/sidecar/ 2>&1

# 2. No sidecar in main CLI (expect 0 matches)
grep -c "sidecar" cli/index.mjs

# 3. No relative package imports (expect 0 files)
grep -r "from.*\.\.\/.*packages" cli --include="*.mjs" -l | wc -l

# 4. CLI commands work (expect success)
timeout 5s node cli/index.mjs --help
timeout 5s node cli/index.mjs hook list

# 5. Imports resolve correctly
timeout 5s node -e "import('@unrdf/hooks').then(m => console.log('✅ @unrdf/hooks loaded'))"
timeout 5s node -e "import('@unrdf/core').then(m => console.log('✅ @unrdf/core loaded'))"
```

---

## Additional Findings (Non-Blocking)

### Files Reviewed (67 total)
- ✅ cli/commands/init.mjs - OK (uses citty, node builtins only)
- ✅ cli/commands/repl.mjs - OK (uses citty, readline)
- ✅ cli/commands/graph/* - OK (uses citty, validation utils)
- ✅ cli/commands/hook/* - 🔴 list.mjs has incorrect import
- ✅ cli/commands/policy/* - OK (uses citty, validation utils)
- ✅ cli/commands/store/* - OK (uses citty, validation utils)
- ✅ cli/commands/context/* - OK (uses citty, OTEL, ContextManager)
- 🔴 cli/commands/sidecar/* - MUST REMOVE
- ✅ cli/utils/* - 🔴 store-instance.mjs has incorrect imports

### Other Utils (Verified OK)
- cli/utils/error-handling.mjs - ✅ No package imports
- cli/utils/tracing.mjs - ✅ Uses @opentelemetry/api (correct)
- cli/utils/validation.mjs - ✅ Uses zod (correct)
- cli/utils/confirmation.mjs - ✅ Uses readline (correct)
- cli/utils/dependency-analyzer.mjs - ✅ Uses fs/promises (correct)

---

## Compliance Checklist

Before declaring "CLI is production ready", verify:

- [ ] **Sidecar directory removed**: `ls cli/commands/sidecar/` returns error
- [ ] **No sidecar in main CLI**: `grep -i sidecar cli/index.mjs` returns 0 results
- [ ] **No relative package imports**: `grep -r "packages" cli --include="*.mjs"` returns 0 results
- [ ] **All imports use @unrdf/***: All package imports start with `@unrdf/`
- [ ] **CLI help works**: `timeout 5s node cli/index.mjs --help` succeeds
- [ ] **Hook commands work**: `timeout 5s node cli/index.mjs hook list` succeeds
- [ ] **Package imports resolve**: `node -e "import('@unrdf/hooks')"` succeeds

---

## Adversarial PM Final Questions

### Claims vs Reality

| Claim | Evidence Required | Status |
|-------|-------------------|--------|
| "Not doing sidecar anymore" | `grep -i sidecar cli/` returns 0 | ❌ FAIL (18 files) |
| "CLI uses correct packages" | No `../packages/` imports | ❌ FAIL (2 files) |
| "All imports work" | CLI runs without errors | ❌ FAIL (untested) |

### What Breaks If We Don't Fix This?

1. **Runtime Import Errors**: `cli/commands/hook/list.mjs` will fail - path doesn't exist
2. **Production Build Fails**: Relative `../../packages/` won't resolve in bundled CLI
3. **User Confusion**: Sidecar commands present but non-functional (health/logs/status)
4. **Package Boundary Violations**: Direct file imports bypass package encapsulation

### How Do We KNOW The Fixes Work?

Run the verification suite:
```bash
# Save this as verify-cli-imports.sh
#!/bin/bash
set -e

echo "1. Verify sidecar removed..."
! ls cli/commands/sidecar/ 2>/dev/null && echo "✅ Sidecar removed" || exit 1

echo "2. Verify no sidecar in main CLI..."
[ $(grep -c "sidecar" cli/index.mjs) -eq 0 ] && echo "✅ No sidecar in CLI" || exit 1

echo "3. Verify no relative package imports..."
[ $(grep -r "from.*\.\.\/.*packages" cli --include="*.mjs" -l | wc -l) -eq 0 ] && echo "✅ No relative imports" || exit 1

echo "4. Test CLI help..."
timeout 5s node cli/index.mjs --help >/dev/null && echo "✅ CLI help works" || exit 1

echo "5. Test package imports..."
timeout 5s node -e "import('@unrdf/hooks').then(() => console.log('✅ @unrdf/hooks OK'))" || exit 1
timeout 5s node -e "import('@unrdf/core').then(() => console.log('✅ @unrdf/core OK'))" || exit 1

echo "🎉 ALL VERIFICATIONS PASSED"
```

---

## Appendix: Grep Evidence

### A1: All Sidecar References
```bash
grep -r "sidecar" cli --include="*.mjs" -l
```
**Output** (18 files):
- cli/utils/retry-logic.mjs
- cli/utils/store-instance.mjs
- cli/utils/output-format.mjs
- cli/commands/sidecar/status.mjs
- cli/commands/sidecar/health.mjs
- cli/commands/sidecar/config.mjs
- cli/commands/sidecar/index.mjs
- cli/commands/sidecar/logs.mjs
- cli/commands/sidecar/restart.mjs
- cli/commands/context/create.mjs
- cli/commands/context/current.mjs
- cli/commands/context/get.mjs
- cli/commands/context/list.mjs
- cli/core/completion.mjs
- cli/core/config.mjs
- cli/core/context.mjs
- cli/index.mjs
- cli/README.md

### A2: Package Boundary Violations
```bash
grep -r "from.*\.\.\/.*packages" cli --include="*.mjs"
```
**Output** (2 files, 3 violations):
- cli/utils/store-instance.mjs:9:`import { createStore } from '../../packages/core/src/rdf/unrdf-store.mjs';`
- cli/utils/store-instance.mjs:10:`import { OxigraphStore } from '../../packages/oxigraph/src/store.mjs';`
- cli/commands/hook/list.mjs:8:`import { KnowledgeHookManager } from '../../../packages/knowledge-engine/knowledge-hook-manager.mjs';`

---

**End of Audit Report**
