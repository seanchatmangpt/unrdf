---
description: Systematically diagnose and fix issues in UNRDF packages using structured debugging methodology
---

# Debugger

## User Input

```text
$ARGUMENTS
```

You **MUST** consider the user input before proceeding (if not empty).

## Purpose

Apply systematic debugging methodology to identify root causes and fix issues in UNRDF packages.

## Debugging Methodology

### Phase 1: Problem Definition

1. **Reproduce the issue**

   ```bash
   # Run the failing command/test
   timeout 30s pnpm test path/to/failing.test.mjs 2>&1
   ```

2. **Capture error context**

   ```bash
   # Get full stack trace
   NODE_OPTIONS="--enable-source-maps" pnpm test 2>&1 | tee error.log
   ```

3. **Define expected vs actual behavior**

### Phase 2: Hypothesis Generation

Based on error type, form hypotheses:

| Error Type        | Likely Causes                     |
| ----------------- | --------------------------------- |
| `TypeError`       | Null/undefined access, wrong type |
| `ReferenceError`  | Missing import, scope issue       |
| `SPARQL Error`    | Query syntax, missing prefix      |
| `RDF Parse Error` | Invalid Turtle syntax             |
| `Timeout`         | Infinite loop, blocking operation |

### Phase 3: Investigation

```bash
# Check recent changes
git log --oneline -10

# Find when issue was introduced
git bisect start
git bisect bad HEAD
git bisect good <known-good-commit>

# Trace execution
DEBUG=* pnpm test 2>&1 | head -100
```

### Phase 4: Root Cause Analysis

#### For RDF/Store Issues

```bash
# Verify store initialization
grep -r "createStore" packages/*/src/ --include="*.mjs"

# Check for N3 vs Oxigraph conflicts
grep -r "from 'n3'" packages/*/src/ --include="*.mjs"
```

#### For Import Issues

```bash
# Check export/import alignment
grep -r "export" packages/core/src/index.mjs
grep -r "from '@unrdf/core'" packages/*/src/ --include="*.mjs"
```

#### For Async Issues

```bash
# Find unhandled promises
grep -r "async" packages/*/src/ --include="*.mjs" | head -20
grep -r "\.then(" packages/*/src/ --include="*.mjs" | head -20
```

### Phase 5: Fix Implementation

1. **Minimal fix first** - Don't refactor while debugging
2. **Verify fix** - Run the exact reproduction steps
3. **Check regression** - Run full test suite

```bash
# Verify fix works
timeout 30s pnpm test path/to/failing.test.mjs

# Check for regressions
timeout 120s pnpm test
```

### Phase 6: Prevention

Document in test to prevent regression:

```javascript
// test/regression/issue-XXX.test.mjs
import { describe, it, expect } from 'vitest';

describe('Issue XXX: [description]', () => {
  it('should [expected behavior]', () => {
    // Reproduction of the fixed issue
  });
});
```

## Common UNRDF Issues

### 1. Store Creation

```javascript
// ❌ Wrong (N3)
import { Store } from 'n3';
const store = new Store();

// ✅ Correct (Oxigraph)
import { createStore } from '@unrdf/oxigraph';
const store = createStore();
```

### 2. Quad Creation

```javascript
// ❌ Wrong
import { DataFactory } from 'n3';

// ✅ Correct
import { dataFactory } from '@unrdf/oxigraph';
const quad = dataFactory.quad(s, p, o, g);
```

### 3. SPARQL Prefix

```javascript
// ❌ Missing prefix
const query = `SELECT * WHERE { ?s mat:hasLevel ?o }`;

// ✅ With prefix
const query = `
  PREFIX mat: <https://unrdf.org/ontology/maturity#>
  SELECT * WHERE { ?s mat:hasLevel ?o }
`;
```

## Output Format

````markdown
## Debug Report

**Issue**: [brief description]
**Status**: ✅ Fixed / 🔍 Investigating / ❌ Blocked

### Reproduction Steps

1. [step 1]
2. [step 2]
3. [observed error]

### Root Cause

[explanation of why the issue occurred]

### Fix Applied

```diff
- old code
+ new code
```
````

### Verification

- [ ] Original issue resolved
- [ ] No regressions introduced
- [ ] Test added for prevention

### Prevention Measures

- [what to avoid in future]

````

## Quick Diagnostic Commands

```bash
# Check package dependencies
pnpm why <package-name>

# Verify build output
pnpm build && ls -la packages/*/dist/

# Check for circular dependencies
pnpm run check:circular 2>&1 || echo "No circular dep check configured"

# Memory profiling
node --inspect packages/cli/dist/index.mjs
````

End Command ---
