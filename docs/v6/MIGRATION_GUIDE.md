# UNRDF V6 Migration Guide

**Complete step-by-step guide for migrating from v5 to v6**

**Last Updated**: 2025-12-27
**Target Version**: v6.0.0
**Estimated Migration Time**: 2-4 hours for typical project

---

## Table of Contents

1. [Pre-Migration Checklist](#pre-migration-checklist)
2. [Quick Start (Automated)](#quick-start-automated)
3. [Manual Migration Steps](#manual-migration-steps)
4. [Breaking Changes Deep Dive](#breaking-changes-deep-dive)
5. [Common Migration Scenarios](#common-migration-scenarios)
6. [Troubleshooting](#troubleshooting)
7. [Rollback Procedures](#rollback-procedures)
8. [Validation & Testing](#validation--testing)

---

## Pre-Migration Checklist

### Before You Begin

- [ ] **Backup your codebase**: `git commit -am "Pre-v6 migration checkpoint"`
- [ ] **Review breaking changes**: Read [`MIGRATION_PLAN.md`](./MIGRATION_PLAN.md)
- [ ] **Check dependencies**: Ensure Node.js ≥18.0.0, pnpm ≥7.0.0
- [ ] **Run existing tests**: `pnpm test` (100% pass required)
- [ ] **Document current behavior**: Save baseline metrics

### Environment Requirements

```bash
# Verify Node.js version
node --version  # Must be ≥18.0.0

# Verify pnpm
pnpm --version  # Must be ≥7.0.0

# Check current UNRDF version
pnpm list @unrdf/core  # Should show v5.x
```

### Compatibility Matrix

| Your Setup | Migration Path | Complexity |
|------------|----------------|------------|
| v5.x + N3 Store | Automated + Manual | Medium |
| v5.x + Custom Store | Manual Only | High |
| v4.x or earlier | Migrate to v5 first | N/A |
| v6.0.0-alpha.x | Review delta only | Low |

---

## Quick Start (Automated)

### Option 1: Full Automation (Recommended)

```bash
# 1. Run migration script (dry-run first)
node scripts/migrate-to-v6.mjs --all --dry-run --report dry-run-report.json

# 2. Review the report
cat dry-run-report.json | jq '.summary'

# 3. Apply migrations
node scripts/migrate-to-v6.mjs --all --report migration-report.json

# 4. Install v6 dependencies
pnpm install

# 5. Run tests
pnpm test

# 6. Validate with OTEL
node validation/run-all.mjs comprehensive
```

**Expected Output**:
```
✅ Migration complete!
Migrated: 47/47 packages
JS Files: 234/234 updated
Errors: 0
```

### Option 2: Per-Package Migration

```bash
# Migrate one package at a time
node scripts/migrate-to-v6.mjs --package packages/my-app --dry-run

# Apply if looks good
node scripts/migrate-to-v6.mjs --package packages/my-app

# Test immediately
cd packages/my-app && pnpm test
```

---

## Manual Migration Steps

### Step 1: Update Dependencies

**package.json changes**:

```diff
{
  "dependencies": {
-   "n3": "^1.26.0",
-   "@unrdf/engine": "^5.0.0"
+   "@unrdf/oxigraph": "workspace:*",
+   "@unrdf/core": "workspace:*",
+   "@unrdf/v6-compat": "workspace:*"
  },
  "engines": {
-   "node": ">=16.0.0"
+   "node": ">=18.0.0",
+   "pnpm": ">=7.0.0"
  },
+ "type": "module"
}
```

**Install**:
```bash
pnpm install
```

---

### Step 2: Migrate Store Initialization

#### Before (v5)

```javascript
import { Store } from 'n3';

function createMyStore() {
  const store = new Store();
  return store;
}
```

#### After (v6)

```javascript
import { createStore } from '@unrdf/oxigraph';

async function createMyStore() {
  const store = await createStore();
  return store;
}
```

**Key Changes**:
- Store creation is now **async**
- Use factory function instead of constructor
- Functions calling `createStore()` must be `async`

---

### Step 3: Update Imports

#### Before (v5)

```javascript
import { Store, DataFactory, Parser, Writer } from 'n3';
import { createEngine } from '@unrdf/engine';
```

#### After (v6)

```javascript
import { createStore, dataFactory } from '@unrdf/oxigraph';
import { Parser, Writer } from '@unrdf/core/rdf/n3-justified-only';
```

**Migration Rule**: Direct N3 imports **only** in designated modules:
- `packages/*/src/rdf/n3-justified-only.mjs`
- All other code imports from `@unrdf/oxigraph` or `@unrdf/core`

---

### Step 4: Add Receipt Generation

#### Before (v5)

```javascript
async function processWorkflow(task) {
  const result = await workflow.run(task);
  return result;
}
```

#### After (v6)

```javascript
import { withReceipt } from '@unrdf/v6-compat/adapters';

const processWorkflow = withReceipt(
  async (task) => {
    const result = await workflow.execute(task);
    return result;
  },
  { operation: 'processWorkflow' }
);

// Usage
const { result, receipt } = await processWorkflow(myTask);
console.log('Receipt:', receipt.hash);
```

**Why Receipts?**
- Deterministic execution proof
- Replay capability
- Adversarial validation

---

### Step 5: Add Zod Validation

#### Before (v5)

```javascript
function createUser(data) {
  // Manual validation or none
  if (!data.id || !data.name) {
    throw new Error('Invalid user');
  }
  return data;
}
```

#### After (v6)

```javascript
import { z } from 'zod';

const UserSchema = z.object({
  id: z.string().uuid(),
  name: z.string().min(1).max(100),
  email: z.string().email().optional()
});

function createUser(data) {
  const validated = UserSchema.parse(data);
  return validated;
}
```

**Benefits**:
- Runtime type safety
- Self-documenting
- Better error messages

---

### Step 6: Update SPARQL Queries

#### Before (v5)

```javascript
const results = await federation.query('SELECT * WHERE { ?s ?p ?o }');
```

#### After (v6)

```javascript
import { sparql } from '@unrdf/federation';

const results = await federation.query(
  sparql`SELECT * WHERE { ?s ?p ?o }`
    .timeout(5000)
    .receipt(true)
);
```

**Key Changes**:
- Template literals prevent injection
- Explicit timeouts (default 5s)
- Receipt generation option

---

### Step 7: Migrate Streams

#### Before (v5)

```javascript
stream.on('data', (quad) => {
  console.log(quad);
});

stream.on('error', (err) => {
  console.error(err);
});

stream.on('end', () => {
  console.log('Done');
});
```

#### After (v6)

```javascript
try {
  for await (const quad of stream) {
    console.log(quad);
  }
  console.log('Done');
} catch (err) {
  console.error(err);
}

// Get receipt after completion
const receipt = stream.receipt();
```

**Why AsyncIterators?**
- Better backpressure handling
- Simpler error handling
- Receipt support

---

## Breaking Changes Deep Dive

### 1. Store Initialization (CRITICAL)

**Impact**: High - Affects all RDF operations
**Effort**: Low - Automated script handles this
**Rationale**: Oxigraph provides 10x faster SPARQL + WASM support

**Migration**:
```bash
# Automated
node scripts/migrate-to-v6.mjs --all

# Manual
# 1. Replace imports
# 2. Convert constructors to factory calls
# 3. Add async/await
```

**Gotcha**: Functions become async, callers need updating.

---

### 2. Pure ESM (No CommonJS)

**Impact**: Medium - Build/bundler changes
**Effort**: Low - Update package.json
**Rationale**: Simplify build, reduce bundle size

**Migration**:
```json
{
  "type": "module",
  "exports": {
    ".": "./src/index.mjs"
  }
}
```

**Gotcha**: No more `require()`. Use `import` or dynamic `import()`.

---

### 3. Receipt-Driven Operations

**Impact**: Medium - New concept to learn
**Effort**: Medium - Wrap existing functions
**Rationale**: Deterministic execution + replay guarantees

**Migration**:
```javascript
import { withReceipt } from '@unrdf/v6-compat/adapters';

const myFunction = withReceipt(
  originalFunction,
  { operation: 'myFunction' }
);
```

**Gotcha**: Return value changes from `result` to `{ result, receipt }`.

---

### 4. Zod Schema Validation

**Impact**: Low - Incremental adoption
**Effort**: Medium - Write schemas for inputs
**Rationale**: Runtime safety + self-documenting

**Migration**:
```javascript
// Generate from TypeScript types (if available)
node scripts/schema-generator.mjs --input src/types.ts --output src/schemas.mjs

// Or write manually
const MySchema = z.object({ ... });
```

**Gotcha**: Zod errors are verbose. Use `validateSchema()` adapter for better messages.

---

## Common Migration Scenarios

### Scenario 1: Simple Node.js Script

**Before**:
```javascript
import { Store } from 'n3';

const store = new Store();
store.addQuad(/* ... */);
console.log(store.size);
```

**After**:
```javascript
import { createStore } from '@unrdf/oxigraph';

async function main() {
  const store = await createStore();
  await store.addQuad(/* ... */);
  console.log(await store.size());
}

main().catch(console.error);
```

**Changes**:
1. Wrap in async function
2. Use `createStore()`
3. Await store operations

---

### Scenario 2: Express API Server

**Before**:
```javascript
import express from 'express';
import { Store } from 'n3';

const app = express();
const store = new Store();

app.get('/query', async (req, res) => {
  const results = await runQuery(req.query.sparql);
  res.json(results);
});
```

**After**:
```javascript
import express from 'express';
import { createStore } from '@unrdf/oxigraph';
import { sparql } from '@unrdf/federation';
import { z } from 'zod';

const app = express();
let store;

// Initialize store at startup
app.listen(3000, async () => {
  store = await createStore();
  console.log('Server ready');
});

const QuerySchema = z.object({
  sparql: z.string().min(1)
});

app.get('/query', async (req, res) => {
  try {
    const { sparql: queryString } = QuerySchema.parse(req.query);
    const results = await store.query(sparql`${queryString}`.timeout(5000));
    res.json({ results, receipt: results.receipt() });
  } catch (error) {
    res.status(400).json({ error: error.message });
  }
});
```

**Changes**:
1. Initialize store in server startup
2. Add Zod validation
3. Return receipts with results
4. Proper error handling

---

### Scenario 3: React Application

**Before**:
```javascript
import { useEffect, useState } from 'react';
import { Store } from 'n3';

function MyComponent() {
  const [data, setData] = useState([]);

  useEffect(() => {
    const store = new Store();
    loadData(store).then(setData);
  }, []);

  return <div>{JSON.stringify(data)}</div>;
}
```

**After**:
```javascript
import { useEffect, useState } from 'react';
import { createStore } from '@unrdf/oxigraph';

function MyComponent() {
  const [data, setData] = useState([]);
  const [receipt, setReceipt] = useState(null);

  useEffect(() => {
    let mounted = true;

    async function load() {
      const store = await createStore();
      const { result, receipt } = await loadData(store);

      if (mounted) {
        setData(result);
        setReceipt(receipt);
      }
    }

    load().catch(console.error);

    return () => { mounted = false; };
  }, []);

  return (
    <div>
      <div>{JSON.stringify(data)}</div>
      {receipt && <div>Receipt: {receipt.hash}</div>}
    </div>
  );
}
```

**Changes**:
1. Use async function in useEffect
2. Handle cleanup properly
3. Display receipts
4. Error handling

---

### Scenario 4: Vitest Tests

**Before**:
```javascript
import { describe, it, expect } from 'vitest';
import { Store } from 'n3';

describe('My Tests', () => {
  it('should work', () => {
    const store = new Store();
    expect(store.size).toBe(0);
  });
});
```

**After**:
```javascript
import { describe, it, expect } from 'vitest';
import { createStore } from '@unrdf/oxigraph';

describe('My Tests', () => {
  it('should work', async () => {
    const store = await createStore();
    expect(await store.size()).toBe(0);
  });
});
```

**Changes**:
1. Make test function async
2. Await store creation
3. Await store operations

---

## Troubleshooting

### Problem: "Cannot use import statement outside a module"

**Cause**: package.json missing `"type": "module"`

**Fix**:
```json
{
  "type": "module"
}
```

---

### Problem: "createStore is not a function"

**Cause**: Wrong import path

**Fix**:
```javascript
// ❌ Wrong
import { createStore } from '@unrdf/core';

// ✅ Correct
import { createStore } from '@unrdf/oxigraph';
```

---

### Problem: "Top-level await not supported"

**Cause**: Using await outside async function in old Node version

**Fix**:
```javascript
// Wrap in async IIFE
(async () => {
  const store = await createStore();
  // ...
})();

// Or use .mjs files with Node 18+
```

---

### Problem: Tests failing after migration

**Checklist**:
1. All test functions are `async`?
2. All store operations are `await`ed?
3. Dependencies updated in package.json?
4. Using correct import paths?

**Debug**:
```bash
# Check imports
grep -r "from 'n3'" src/ test/

# Run with verbose logging
NODE_ENV=test pnpm test -- --reporter=verbose

# Check individual test
pnpm test -- --run test/my-test.test.mjs
```

---

### Problem: OTEL validation failing

**Cause**: Missing receipts or non-deterministic operations

**Fix**:
```javascript
// Add receipt wrappers
import { withReceipt } from '@unrdf/v6-compat/adapters';

const myOp = withReceipt(originalOp, { operation: 'myOp' });

// Remove non-deterministic calls
// ❌ Don't use in business logic
const timestamp = Date.now();
const random = Math.random();

// ✅ Use in receipt generation only
const receipt = {
  timestamp: Date.now(), // OK here
  operation: 'myOp'
};
```

**Validate**:
```bash
node validation/run-all.mjs comprehensive
grep "Score:" validation-output.log  # Should be ≥80/100
```

---

## Rollback Procedures

### If Migration Fails

#### Option 1: Git Rollback

```bash
# Restore to pre-migration state
git reset --hard HEAD~1

# Or restore specific commit
git reset --hard <commit-hash>

# Reinstall v5 dependencies
pnpm install
```

#### Option 2: Keep Both Versions

```json
{
  "dependencies": {
    "@unrdf/core-v5": "npm:@unrdf/core@5.0.1",
    "@unrdf/core": "^6.0.0"
  }
}
```

```javascript
// Use v5 selectively
import { Store as StoreV5 } from '@unrdf/core-v5';
import { createStore } from '@unrdf/core';
```

#### Option 3: Use Compatibility Layer

```javascript
// Keep v5 API, use v6 backend
import { createStore } from '@unrdf/v6-compat/adapters';

const store = await createStore(); // Warns but works
```

---

## Validation & Testing

### Post-Migration Checklist

- [ ] **All tests pass**: `pnpm test` (100% pass rate)
- [ ] **No N3 imports**: `grep -r "from 'n3'" packages/*/src` (0 results)
- [ ] **Linting passes**: `pnpm lint` (0 errors, 0 warnings)
- [ ] **Type checking**: `pnpm -r run typecheck` (0 errors)
- [ ] **OTEL validation**: Score ≥80/100
- [ ] **Benchmarks**: No regressions >10%
- [ ] **Documentation updated**: All examples use v6 API

### Validation Commands

```bash
# 1. Run full test suite
timeout 20s pnpm test
echo "✅ Tests: $?"

# 2. Check for v5 patterns
timeout 5s grep -r "from 'n3'" packages/*/src | wc -l
# Expected: 0

# 3. Check for non-async stores
timeout 5s grep -r "new Store()" packages/*/src | wc -l
# Expected: 0

# 4. Run OTEL validation
node validation/run-all.mjs comprehensive
grep "Score:" validation-output.log
# Expected: ≥80/100

# 5. Run benchmarks
timeout 30s pnpm benchmark:regression
# Expected: No regressions >10%

# 6. Check bundle size
timeout 10s pnpm -r exec du -sh dist/
# Expected: v6 ≤ v5 size
```

### Performance Comparison

```bash
# Before migration
git checkout v5-tag
pnpm benchmark:baseline

# After migration
git checkout main
pnpm benchmark:compare

# Expected improvements:
# - SPARQL queries: 10x faster
# - Store operations: 5x faster
# - Bundle size: 20% smaller
```

---

## Next Steps

After successful migration:

1. **Update documentation**: Replace all v5 examples
2. **Train team**: Share migration guide
3. **Monitor production**: Watch for deprecation warnings
4. **Remove compat layer**: After 30 days, remove `@unrdf/v6-compat`
5. **Optimize further**: Explore v6-specific features

---

## Support & Resources

### Documentation
- [Migration Plan](./MIGRATION_PLAN.md) - Strategic overview
- [Maturity Ladder](./MATURITY_LADDER.md) - Package readiness
- [BB80/20 Methodology](../bb80-20-methodology.md) - Development approach

### Getting Help
- **GitHub Issues**: https://github.com/unrdf/unrdf/issues
- **Discussions**: https://github.com/unrdf/unrdf/discussions
- **Community**: Join Discord (link in README)

### Reporting Issues
```bash
# Create issue with migration report
node scripts/migrate-to-v6.mjs --all --report issue-report.json

# Attach to GitHub issue with:
# - Node version: node --version
# - UNRDF version: pnpm list @unrdf/core
# - Error logs: full stack trace
# - Migration report: issue-report.json
```

---

## Appendix: API Mapping Reference

### Store Operations

| v5 API | v6 API | Notes |
|--------|--------|-------|
| `new Store()` | `createStore()` | Now async |
| `store.size` | `await store.size()` | Now async |
| `store.addQuad(q)` | `await store.addQuad(q)` | Now async |
| `store.match(s,p,o,g)` | `store.match(s,p,o,g)` | Same (sync iterator) |

### DataFactory

| v5 API | v6 API | Notes |
|--------|--------|-------|
| `DataFactory.namedNode()` | `dataFactory.namedNode()` | Lowercase |
| `DataFactory.literal()` | `dataFactory.literal()` | Lowercase |

### Workflows

| v5 API | v6 API | Notes |
|--------|--------|-------|
| `workflow.run(task)` | `workflow.execute(task)` | Returns `{ result, receipt }` |

### Federation

| v5 API | v6 API | Notes |
|--------|--------|-------|
| `federation.query(string)` | `federation.query(sparql\`...\`)` | Template literal |

---

**Version**: v6.0.0
**Last Updated**: 2025-12-27
**Maintained By**: UNRDF Core Team
