# UNRDF v6 Troubleshooting Guide

**Target Audience**: Developers encountering issues with UNRDF v6
**Prerequisites**: Basic familiarity with UNRDF
**Last Updated**: January 2025

This guide helps you diagnose and resolve common issues with UNRDF v6.

---

## Table of Contents

1. [Installation Issues](#installation-issues)
2. [Import and Module Errors](#import-and-module-errors)
3. [Store and Database Issues](#store-and-database-issues)
4. [SPARQL Query Problems](#sparql-query-problems)
5. [Receipt and Validation Errors](#receipt-and-validation-errors)
6. [Performance Issues](#performance-issues)
7. [Migration from v5](#migration-from-v5)
8. [Getting Help](#getting-help)

---

## Installation Issues

### Error: `Cannot find module '@unrdf/oxigraph'`

**Symptom**:
```bash
Error: Cannot find module '@unrdf/oxigraph'
```

**Cause**: Package not installed or incorrect version.

**Solution**:
```bash
# Verify installation
pnpm list @unrdf/oxigraph

# Install if missing
pnpm add @unrdf/oxigraph@latest

# Clear node_modules and reinstall
rm -rf node_modules pnpm-lock.yaml
pnpm install
```

---

### Error: `npm ERR! peer dep missing`

**Symptom**:
```bash
npm ERR! peer dep missing: zod@^3.0.0, required by @unrdf/core@6.0.0
```

**Cause**: Missing peer dependency.

**Solution**:
```bash
# Install peer dependencies
pnpm add zod@latest

# Or install all peer deps automatically (pnpm)
pnpm install --shamefully-hoist
```

---

### Error: `ERESOLVE unable to resolve dependency tree`

**Symptom**:
```bash
npm ERR! code ERESOLVE
npm ERR! ERESOLVE unable to resolve dependency tree
```

**Cause**: Conflicting dependency versions.

**Solution**:
```bash
# Use pnpm instead of npm
pnpm install

# Or use npm with --legacy-peer-deps
npm install --legacy-peer-deps

# Or use --force (not recommended)
npm install --force
```

---

## Import and Module Errors

### Error: `SyntaxError: Cannot use import statement outside a module`

**Symptom**:
```bash
import { createStore } from '@unrdf/oxigraph';
^^^^^^
SyntaxError: Cannot use import statement outside a module
```

**Cause**: Missing `"type": "module"` in package.json.

**Solution**:
```json
// package.json
{
  "type": "module"
}
```

**OR** use `.mjs` extension:
```bash
mv app.js app.mjs
node app.mjs
```

---

### Error: `require() of ES Module not supported`

**Symptom**:
```bash
Error [ERR_REQUIRE_ESM]: require() of ES Module @unrdf/core not supported
```

**Cause**: Using CommonJS `require()` with ESM module.

**Solution**:
```javascript
// ❌ Don't use require() in v6
const unrdf = require('@unrdf/core');

// ✅ Use import instead
import * as unrdf from '@unrdf/core';
```

**If you must use CommonJS**:
```javascript
// Use dynamic import() in async context
(async () => {
  const unrdf = await import('@unrdf/core');
  // Use unrdf...
})();
```

---

### Error: `ERR_MODULE_NOT_FOUND`

**Symptom**:
```bash
Error [ERR_MODULE_NOT_FOUND]: Cannot find module '/path/to/app'
```

**Cause**: Missing file extension in import.

**Solution**:
```javascript
// ❌ Wrong
import { helper } from './helper';

// ✅ Correct
import { helper } from './helper.mjs';
```

---

### Error: `Directory import is not supported`

**Symptom**:
```bash
Error [ERR_UNSUPPORTED_DIR_IMPORT]: Directory import '/path/to/dir' is not supported
```

**Cause**: Importing directory instead of specific file.

**Solution**:
```javascript
// ❌ Wrong
import * as utils from './utils';

// ✅ Correct
import * as utils from './utils/index.mjs';
```

---

## Store and Database Issues

### Error: `Store creation fails silently`

**Symptom**:
```javascript
const store = createStore(); // No error, but store is undefined
```

**Cause**: Missing `await` on async function.

**Solution**:
```javascript
// ❌ Wrong
const store = createStore();

// ✅ Correct
const store = await createStore();
```

---

### Error: `Cannot read properties of undefined (reading 'add')`

**Symptom**:
```bash
TypeError: Cannot read properties of undefined (reading 'add')
```

**Cause**: Store not properly initialized.

**Solution**:
```javascript
// Ensure store is awaited
const store = await createStore();

// Check store is defined
if (!store) {
  throw new Error('Store creation failed');
}

await store.add(quad);
```

---

### Error: `SQLite: database is locked`

**Symptom**:
```bash
Error: SQLite: database is locked
```

**Cause**: Multiple processes accessing same SQLite file.

**Solution**:
```javascript
// Option 1: Use separate databases per process
const store = await createStore({
  backend: 'sqlite',
  path: `/tmp/store-${process.pid}.db`
});

// Option 2: Use memory backend for tests
const store = await createStore({ backend: 'memory' });

// Option 3: Close store properly
await store.close();
```

---

### Error: `WASM instantiation failed`

**Symptom**:
```bash
RuntimeError: WASM instantiation failed
```

**Cause**: WASM binary corrupt or incompatible.

**Solution**:
```bash
# Reinstall Oxigraph
pnpm remove @unrdf/oxigraph
pnpm add @unrdf/oxigraph@latest

# Clear cache
pnpm store prune

# Verify Node.js version (>=18.0.0)
node --version
```

---

## SPARQL Query Problems

### Error: `Query returns no results`

**Symptom**:
```javascript
const results = await executeSparql(store, query);
console.log(results.length); // 0 (but expected results)
```

**Diagnose**:
```javascript
// 1. Check store has data
console.log('Store size:', store.size);

// 2. Verify prefixes match
const debugQuery = `
  SELECT ?s ?p ?o
  WHERE { ?s ?p ?o }
  LIMIT 10
`;
const all = await executeSparql(store, debugQuery);
console.log('Sample data:', all);

// 3. Check exact URIs
console.log('Expected subject:', 'http://example.org/Alice');
console.log('Actual subject:', all[0].get('s')?.value);
```

**Common Causes**:
```javascript
// ❌ Wrong prefix
PREFIX ex: <http://example.com/>  // Wrong domain

// ✅ Correct prefix
PREFIX ex: <http://example.org/>

// ❌ Wrong case
?person foaf:Name ?name .  // Capital N

// ✅ Correct case
?person foaf:name ?name .  // Lowercase n
```

---

### Error: `SPARQL query timeout`

**Symptom**:
```bash
TimeoutError: Query exceeded timeout (5000ms)
```

**Solution**:
```javascript
// Increase timeout
const results = await executeSparql(store, query, {
  timeout: 30000 // 30 seconds
});

// Optimize query (use LIMIT)
const query = `
  SELECT ?s ?p ?o
  WHERE { ?s ?p ?o }
  LIMIT 100  -- Add LIMIT
`;

// Use indexes (specific predicates)
const query = `
  PREFIX foaf: <http://xmlns.com/foaf/0.1/>

  SELECT ?name
  WHERE {
    ?person foaf:name ?name .  -- Specific predicate (fast)
    # FILTER regex(str(?person), "Alice")  -- Avoid regex (slow)
  }
`;
```

---

### Error: `SyntaxError in SPARQL query`

**Symptom**:
```bash
SyntaxError: Unexpected token in SPARQL query
```

**Solution**:
```javascript
// Use multiline template literals
const query = `
  PREFIX foaf: <http://xmlns.com/foaf/0.1/>

  SELECT ?name
  WHERE {
    ?person foaf:name ?name .
  }
`;

// Check for typos
// ❌ Missing dot
WHERE { ?s ?p ?o }

// ✅ Correct
WHERE { ?s ?p ?o . }
```

---

## Receipt and Validation Errors

### Error: `ZodError: Invalid input`

**Symptom**:
```bash
ZodError: [
  {
    code: 'invalid_type',
    expected: 'string',
    received: 'number',
    path: ['subject']
  }
]
```

**Solution**:
```javascript
import { z } from 'zod';

const TripleSchema = z.object({
  subject: z.string().url(),
  predicate: z.string().url(),
  object: z.string()
});

// ❌ Wrong type
const data = {
  subject: 123,  // Number, not string
  predicate: 'http://example.org/p',
  object: 'value'
};

// ✅ Correct type
const data = {
  subject: 'http://example.org/s',  // String
  predicate: 'http://example.org/p',
  object: 'value'
};

// Validate
try {
  const validated = TripleSchema.parse(data);
} catch (error) {
  if (error instanceof z.ZodError) {
    console.error('Validation errors:', error.errors);
  }
}
```

---

### Error: `Receipt verification failed`

**Symptom**:
```javascript
const isValid = verifyReceipt(receipt);
console.log(isValid); // false (expected true)
```

**Diagnose**:
```javascript
import { verifyReceipt } from '@unrdf/v6-core/receipts';

// Check receipt structure
console.log('Receipt ID:', receipt.id);
console.log('Merkle root:', receipt.merkleRoot);
console.log('Proof:', receipt.proof);

// Verify manually
const { id, operation, timestamp, merkleRoot, proof, metadata } = receipt;

if (!id || !merkleRoot || !proof) {
  console.error('Receipt missing required fields');
}

// Check for tampering
const originalMetadata = { ...metadata };
// ... later ...
if (JSON.stringify(metadata) !== JSON.stringify(originalMetadata)) {
  console.error('Receipt metadata was modified');
}
```

---

### Error: `Chain verification failed`

**Symptom**:
```javascript
const chainValid = verifyChain(receipts);
console.log(chainValid); // false
```

**Solution**:
```javascript
import { verifyChain } from '@unrdf/v6-core/receipts';

// Check each receipt individually
for (const receipt of receipts) {
  const valid = verifyReceipt(receipt);
  if (!valid) {
    console.error('Invalid receipt:', receipt.id);
  }
}

// Check links between receipts
for (let i = 1; i < receipts.length; i++) {
  const prev = receipts[i - 1];
  const curr = receipts[i];

  if (curr.metadata.previousReceipt !== prev.id) {
    console.error('Chain broken between', prev.id, 'and', curr.id);
  }
}

// Verify full chain
const chainValid = verifyChain(receipts);
```

---

## Performance Issues

### Issue: Slow SPARQL queries

**Symptom**:
```javascript
// Takes >10 seconds for simple query
const results = await executeSparql(store, query);
```

**Solutions**:

1. **Use persistent backend** (not memory):
```javascript
const store = await createStore({
  backend: 'sqlite',  // Faster for large datasets
  path: '/path/to/data.db'
});
```

2. **Add indexes** (use specific predicates):
```javascript
// ❌ Slow (full scan)
SELECT ?s ?p ?o WHERE { ?s ?p ?o }

// ✅ Fast (uses predicate index)
SELECT ?s ?name WHERE { ?s foaf:name ?name }
```

3. **Batch inserts**:
```javascript
// ❌ Slow (1000 individual inserts)
for (const quad of quads) {
  await store.add(quad);
}

// ✅ Fast (1 batch insert)
await store.addAll(quads);
```

4. **Limit results**:
```javascript
SELECT ?s ?p ?o
WHERE { ?s ?p ?o }
LIMIT 1000  -- Add LIMIT
```

---

### Issue: High memory usage

**Symptom**:
```bash
FATAL ERROR: Reached heap limit Allocation failed - JavaScript heap out of memory
```

**Solutions**:

1. **Use streaming** (not load all at once):
```javascript
import { createReadStream } from '@unrdf/streaming';

// ❌ Memory-intensive
const allQuads = await parseRdf(largeFile);
await store.addAll(allQuads);

// ✅ Memory-efficient
const stream = createReadStream(largeFile);
for await (const quad of stream) {
  await store.add(quad);
}
```

2. **Use persistent backend**:
```javascript
// ❌ All in memory
const store = await createStore({ backend: 'memory' });

// ✅ Data on disk
const store = await createStore({
  backend: 'sqlite',
  path: '/path/to/data.db'
});
```

3. **Process in batches**:
```javascript
const BATCH_SIZE = 1000;

for (let i = 0; i < quads.length; i += BATCH_SIZE) {
  const batch = quads.slice(i, i + BATCH_SIZE);
  await store.addAll(batch);
}
```

---

## Migration from v5

### Issue: `new Store()` not found

**Symptom**:
```bash
TypeError: Store is not a constructor
```

**Solution**:
```javascript
// v5 (deprecated)
import { Store } from 'n3';
const store = new Store();

// v6 (required)
import { createStore } from '@unrdf/oxigraph';
const store = await createStore();
```

---

### Issue: `workflow.run()` deprecated

**Symptom**:
```bash
DeprecationWarning: workflow.run() is deprecated. Use workflow.execute()
```

**Solution**:
```javascript
// v5 (deprecated)
await workflow.run(task);

// v6 (required)
const receipt = await workflow.execute(task);
```

---

### Issue: Direct N3 imports break

**Symptom**:
```bash
Error: Direct N3 imports are not allowed in v6
```

**Solution**:
```javascript
// v5 (deprecated)
import { Parser, Writer } from 'n3';

// v6 (required)
import { parseRdf, serializeRdf } from '@unrdf/core/rdf';
import { dataFactory } from '@unrdf/core/rdf/n3-justified-only';
```

---

## Getting Help

### Before Opening an Issue

1. **Search existing issues**: [GitHub Issues](https://github.com/unrdf/unrdf/issues)
2. **Check documentation**:
   - [Quick Start](/home/user/unrdf/docs/v6/QUICK-START.md)
   - [API Reference](/home/user/unrdf/docs/v6/API-REFERENCE.md)
   - [Migration Guide](/home/user/unrdf/docs/v6/MIGRATION_PLAN.md)
3. **Run diagnostics**:
   ```bash
   # Node.js version
   node --version  # Should be >=18.0.0

   # Package versions
   pnpm list @unrdf/core @unrdf/oxigraph @unrdf/v6-core

   # Test basic functionality
   node -e "import('@unrdf/oxigraph').then(m => m.createStore()).then(() => console.log('✅ Works'))"
   ```

---

### Creating a Good Issue Report

Include:

1. **Environment**:
   ```bash
   Node version: v18.19.0
   OS: macOS 14.1
   Package manager: pnpm 8.15.0
   UNRDF version: 6.0.0-alpha.1
   ```

2. **Minimal reproduction**:
   ```javascript
   import { createStore } from '@unrdf/oxigraph';

   const store = await createStore();
   // ... minimal code that reproduces issue ...
   ```

3. **Expected vs actual behavior**:
   ```
   Expected: Query returns 2 results
   Actual: Query returns 0 results
   ```

4. **Error messages** (full stack trace):
   ```bash
   Error: ...
   at ...
   at ...
   ```

---

### Community Support

- **GitHub Discussions**: [unrdf/unrdf/discussions](https://github.com/unrdf/unrdf/discussions)
- **GitHub Issues**: [unrdf/unrdf/issues](https://github.com/unrdf/unrdf/issues)
- **Stack Overflow**: Tag `unrdf`

---

## Common Commands Reference

```bash
# Installation
pnpm add @unrdf/core@6.0.0-alpha.1 @unrdf/oxigraph @unrdf/v6-core zod

# Run code
node app.mjs  # ESM
node --experimental-modules app.mjs  # Node <18

# Tests
pnpm test
pnpm test:coverage

# Linting
pnpm lint
pnpm lint:fix

# Diagnostics
node --version
pnpm list
pnpm audit

# Clean install
rm -rf node_modules pnpm-lock.yaml
pnpm install
```

---

**Still stuck?** Open an issue on [GitHub](https://github.com/unrdf/unrdf/issues) with details 🚀
