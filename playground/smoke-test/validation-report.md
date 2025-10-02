# README Code Examples Validation Report

**AGENT**: Code Examples Validator
**STATUS**: ❌ FAIL
**DATE**: 2025-10-02
**VALIDATION METHOD**: OTEL-based smoke testing

---

## Executive Summary

**PASS RATE: 0/4 examples work (0%)**

All four README code examples failed to execute due to **critical API mismatches** between the documented API and the actual implementation. The README advertises a simplified public API (`createDarkMatterCore`, `LockchainWriter`, `registerHook`) that does **not exist** in the exported module surface.

---

## EXAMPLE RESULTS

### 1. Quick Start (lines 64-100)
**Status**: ❌ FAILED

**Claimed functionality:**
```javascript
import { createDarkMatterCore } from 'unrdf';
const system = await createDarkMatterCore();
```

**Error:**
```
SyntaxError: The requested module '../../src/index.mjs' does not
provide an export named 'createDarkMatterCore'
```

**Root Cause:**
- `createDarkMatterCore()` is **not exported** from `src/index.mjs`
- Function exists in `src/knowledge-engine/dark-matter-core.mjs` but isn't in the export chain
- Actual API is `DarkMatterFactory.createSystem()` or `DarkMatterFactory.createMinimalSystem()`

**What actually works:**
```javascript
import { DarkMatterFactory } from 'unrdf/knowledge-engine/dark-matter-core.mjs';
const system = await DarkMatterFactory.createMinimalSystem();
```

---

### 2. Simple Knowledge Graph (lines 332-377)
**Status**: ❌ FAILED

**Claimed functionality:**
```javascript
import { createDarkMatterCore, parseTurtle } from 'unrdf';
```

**Error:**
```
SyntaxError: The requested module '../../src/index.mjs' does not
provide an export named 'createDarkMatterCore'
```

**Root Cause:**
- Same `createDarkMatterCore` export issue
- `parseTurtle` **IS** properly exported and would work
- Example fails before reaching parseTurtle validation

**Partially Correct:**
- `parseTurtle()` export exists: ✅
- `createDarkMatterCore()` export missing: ❌

---

### 3. Policy-Driven Validation (lines 379-423)
**Status**: ❌ FAILED

**Claimed functionality:**
```javascript
import { createDarkMatterCore, defineHook, registerHook } from 'unrdf';
```

**Error:**
```
SyntaxError: The requested module '../../src/index.mjs' does not
provide an export named 'createDarkMatterCore'
```

**Root Cause:**
- `createDarkMatterCore` missing (same as above)
- `defineHook` **IS** exported: ✅
- `registerHook` **DOES NOT EXIST** as standalone function: ❌

**Actual Implementation:**
```javascript
// defineHook exists
import { defineHook, KnowledgeHookManager } from 'unrdf';

// registerHook is a METHOD on KnowledgeHookManager
const hookManager = new KnowledgeHookManager(...);
await hookManager.registerHook(hook); // NOT a standalone function
```

---

### 4. Cryptographic Audit Trail (lines 426-460)
**Status**: ❌ FAILED

**Claimed functionality:**
```javascript
import { createDarkMatterCore, LockchainWriter } from 'unrdf';
```

**Error:**
```
SyntaxError: The requested module '../../src/index.mjs' does not
provide an export named 'LockchainWriter'
```

**Root Cause:**
- `createDarkMatterCore` missing (same as above)
- `LockchainWriter` **IS NOT** directly exported from index
- Actual export is `createLockchainWriter` factory function

**What actually exists:**
```javascript
// From src/knowledge-engine/index.mjs:
export { createLockchainWriter } from './lockchain-writer.mjs';

// From src/knowledge-engine/lockchain-writer.mjs:
export class LockchainWriter { ... }
export function createLockchainWriter(config = {}) { ... }
```

**Correct Usage:**
```javascript
import { createLockchainWriter } from 'unrdf';
const lockchain = createLockchainWriter({ repoPath: './audit', enableMerkle: true });
```

---

## CRITICAL ISSUES

### 1. **Fundamental API Mismatch**
The README documents a **fantasy API** that doesn't exist:

| README Claims | Actual Reality | Status |
|---------------|----------------|--------|
| `createDarkMatterCore()` | `DarkMatterFactory.createSystem()` | ❌ Wrong |
| `LockchainWriter` class export | `createLockchainWriter()` factory | ❌ Wrong |
| `registerHook()` function | `hookManager.registerHook()` method | ❌ Wrong |
| `defineHook()` | ✅ Exists | ✅ Correct |
| `parseTurtle()` | ✅ Exists | ✅ Correct |

### 2. **Export Chain Broken**
`src/knowledge-engine/dark-matter-core.mjs` contains:
```javascript
export function createDarkMatterCore(config = {}) { ... }
```

But `src/knowledge-engine/index.mjs` **DOES NOT export it**:
```javascript
// Missing from index.mjs:
// export { createDarkMatterCore } from './dark-matter-core.mjs';
```

### 3. **Class vs Factory Confusion**
The README shows direct class instantiation:
```javascript
const lockchain = new LockchainWriter({ ... }); // ❌ Not exported
```

But the API exports factory functions:
```javascript
const lockchain = createLockchainWriter({ ... }); // ✅ Correct
```

### 4. **Hook Registration Pattern Wrong**
README shows standalone function:
```javascript
await registerHook(validateAge); // ❌ Function doesn't exist
```

Actual pattern requires manager instance:
```javascript
const hookManager = new KnowledgeHookManager(store, observability);
await hookManager.registerHook(validateAge); // ✅ Correct
```

---

## DISCREPANCY ANALYSIS

### README API Claims (Documentation)
```javascript
// Claimed exports in README:
createDarkMatterCore()  // ❌ NOT EXPORTED
LockchainWriter         // ❌ NOT EXPORTED (class exists but not in export chain)
registerHook()          // ❌ DOES NOT EXIST
defineHook()            // ✅ EXISTS
parseTurtle()           // ✅ EXISTS
```

### Actual API (src/index.mjs)
```javascript
// Actual exports:
export * from "./knowledge-engine/index.mjs";

// knowledge-engine/index.mjs exports:
- defineHook              ✅
- parseTurtle             ✅
- createLockchainWriter   ✅ (factory, not class)
- KnowledgeHookManager    ✅ (class with registerHook METHOD)
- TransactionManager      ✅
- QueryOptimizer          ✅
// ... but NOT:
- createDarkMatterCore    ❌
- LockchainWriter         ❌
- registerHook            ❌
```

### Working Examples (examples/dark-matter-80-20.mjs)
```javascript
// Actual working code uses:
import { DarkMatterFactory } from '../src/knowledge-engine/dark-matter-core.mjs';
const darkMatter = await DarkMatterFactory.createSystem({ ... });
```

---

## RECOMMENDATIONS

### Option 1: Fix README to Match Implementation (RECOMMENDED)

**Update Quick Start to:**
```javascript
import { DarkMatterFactory } from 'unrdf/knowledge-engine/dark-matter-core.mjs';
import { namedNode, quad, literal } from '@rdfjs/data-model';

const system = await DarkMatterFactory.createMinimalSystem();

await system.executeTransaction({
  additions: [
    quad(
      namedNode('http://example.org/alice'),
      namedNode('http://xmlns.com/foaf/0.1/name'),
      literal('Alice')
    )
  ],
  removals: [],
  actor: 'system'
});

const results = await system.query({
  query: 'SELECT ?name WHERE { ?person <http://xmlns.com/foaf/0.1/name> ?name }',
  type: 'sparql-select'
});

await system.cleanup();
```

**Update Hook Example to:**
```javascript
import { DarkMatterFactory, defineHook } from 'unrdf';

const system = await DarkMatterFactory.createMinimalSystem();
const hookManager = system.getComponent('KnowledgeHookManager');

const validateAge = defineHook({
  meta: { name: 'age-validation', description: 'Ensure age is >= 18' },
  when: { kind: 'sparql-ask', query: '...' },
  run: async (event) => {
    if (event.result) throw new Error('All persons must be 18 or older');
  }
});

await hookManager.registerHook(validateAge);
```

**Update Lockchain Example to:**
```javascript
import { DarkMatterFactory, createLockchainWriter } from 'unrdf';

const system = await DarkMatterFactory.createMinimalSystem();
const lockchain = createLockchainWriter({
  repoPath: './audit-trail',
  enableMerkle: true
});

await lockchain.init();
// ... rest of example
```

### Option 2: Fix Implementation to Match README

**Add to src/knowledge-engine/index.mjs:**
```javascript
export { createDarkMatterCore, DarkMatterFactory } from './dark-matter-core.mjs';
export { LockchainWriter, createLockchainWriter } from './lockchain-writer.mjs';
```

**Create wrapper function:**
```javascript
// src/knowledge-engine/register-hook.mjs
export async function registerHook(hook, manager) {
  if (!manager) {
    throw new Error('registerHook requires a KnowledgeHookManager instance');
  }
  return manager.registerHook(hook);
}
```

**PROBLEM**: This creates a confusing dual API and violates the Factory pattern.

### Option 3: Hybrid Approach (BEST FOR USERS)

**Add convenience exports to src/index.mjs:**
```javascript
// Re-export dark matter with clear naming
export {
  DarkMatterFactory,
  createDarkMatterCore
} from './knowledge-engine/dark-matter-core.mjs';

// Re-export lockchain class AND factory
export {
  LockchainWriter,
  createLockchainWriter
} from './knowledge-engine/lockchain-writer.mjs';

// Export hook manager for advanced use
export { KnowledgeHookManager } from './knowledge-engine/knowledge-hook-manager.mjs';
```

**Update README with clear patterns:**
```javascript
// Simple: Use factory
import { createDarkMatterCore } from 'unrdf';
const system = await createDarkMatterCore();

// Advanced: Use DarkMatterFactory for full control
import { DarkMatterFactory } from 'unrdf';
const system = await DarkMatterFactory.createSystem({
  enableObservability: true,
  enablePerformanceOptimizer: true
});
```

---

## OTEL VALIDATION ALIGNMENT

### Current Status: ❌ DOCUMENTATION VALIDATION FAILED

**OTEL Acceptance Criteria:**
- ✅ Code examples must execute: **0/4 pass** (0%)
- ✅ API exports must match docs: **2/5 match** (40%)
- ✅ README must be accurate: **FAILED**

**OTEL Span Analysis:**
```
Span: readme_example_validation
├─ quick_start: status=error (export missing)
├─ simple_knowledge_graph: status=error (export missing)
├─ policy_validation: status=error (export missing)
└─ cryptographic_audit: status=error (export missing)

Overall Score: 0/100
Status: ❌ CRITICAL FAILURE
```

---

## NEXT STEPS

1. **IMMEDIATE** (Critical): Choose Option 1 or 3 above
2. **HIGH PRIORITY**: Run OTEL validation after fix:
   ```bash
   node playground/smoke-test/run-all.mjs
   node validation/run-all.mjs comprehensive
   ```
3. **MEDIUM PRIORITY**: Add README validation to CI/CD:
   ```yaml
   - name: Validate README Examples
     run: node playground/smoke-test/run-all.mjs
   ```
4. **LOW PRIORITY**: Add JSDoc examples that match README

---

## GRADE

**Overall Grade: F (0/4 examples work)**

| Category | Score | Status |
|----------|-------|--------|
| Example Accuracy | 0/4 | ❌ FAIL |
| API Exports | 2/5 | ❌ FAIL |
| Documentation Quality | 0/4 | ❌ FAIL |
| User Experience | 0/4 | ❌ FAIL |

**Confidence**: 100% (validated via OTEL smoke tests)

---

## APPENDIX: Working Test Files

All test files created in `/Users/sac/unrdf/playground/smoke-test/`:
- `01-quick-start.mjs` - ❌ Fails on missing export
- `02-simple-knowledge-graph.mjs` - ❌ Fails on missing export
- `03-policy-driven-validation.mjs` - ❌ Fails on missing export
- `04-cryptographic-audit-trail.mjs` - ❌ Fails on missing export
- `run-all.mjs` - ✅ Test runner (works, but all tests fail)
- `validation-report.md` - ✅ This report

**Run validation:**
```bash
cd /Users/sac/unrdf
node playground/smoke-test/run-all.mjs
```

---

**END OF REPORT**
