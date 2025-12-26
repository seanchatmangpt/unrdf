# Documentation Code Alignment Report

**Generated:** 2025-12-25T23:28:59.487Z

## Executive Summary

| Metric | Value |
|--------|-------|
| Total Code Examples | 78 |
| Executable Examples | 57 |
| Static Errors Found | 11 |
| Success Rate | 81% |

## Examples by Category

| Category | Count |
|----------|-------|
| executable | 57 |
| comment-only | 11 |
| negative-example | 5 |
| incomplete-snippet | 3 |
| migration-before | 2 |

## Examples by Documentation File

### ❌ QUICK-START.md

- **Total Examples:** 16
- **Executable:** 10
- **Errors:** 1 (10%)

### ❌ API-REFERENCE.md

- **Total Examples:** 34
- **Executable:** 30
- **Errors:** 1 (3%)

### ❌ MIGRATION.md

- **Total Examples:** 20
- **Executable:** 9
- **Errors:** 3 (33%)

### ❌ WALKTHROUGHS.md

- **Total Examples:** 8
- **Executable:** 8
- **Errors:** 6 (75%)

## ❌ Failures (11)

### 1. QUICK-START.md - Use Case 5: Federation (Query Multiple Stores) (line 306)

**Errors:**

- federatedQuery is not exported from @unrdf/federation
   → Check actual @unrdf/federation exports

**Code snippet:**

```javascript
import { createStore } from '@unrdf/core';
import { federatedQuery } from '@unrdf/federation';

const store1 = createStore(); // Local data
const store2 = createStore(); // Remote data

// Query across both stores
const results = await federatedQuery([store1, store2], `
  SELECT ?name ?skill WHERE {
    ?person foaf:name ?name ;
...
```

### 2. API-REFERENCE.md - 3. Use Oxigraph, Not N3 (line 703)

**Errors:**

- N3 import in non-migration example
   → Use @unrdf/oxigraph instead of N3

**Code snippet:**

```javascript
// ✅ Oxigraph (Rust, fast)
import { createStore } from '@unrdf/oxigraph';

// ❌ N3 (JS, slow)
import { Store } from 'n3';
...
```

### 3. MIGRATION.md - Example 1: Load RDF File (line 366)

**Errors:**

- N3 import in non-migration example
   → Use @unrdf/oxigraph instead of N3

**Code snippet:**

```javascript
import { Parser } from 'n3';
import { readFile } from 'fs/promises';

const turtle = await readFile('data.ttl', 'utf-8');
const parser = new Parser();

const quads = [];
parser.parse(turtle, (error, quad) => {
  if (quad) quads.push(quad);
  else if (error) console.error(error);
...
```

### 4. MIGRATION.md - Example 3: Export to Different Format (line 438)

**Errors:**

- N3 import in non-migration example
   → Use @unrdf/oxigraph instead of N3

**Code snippet:**

```javascript
import { Writer } from 'n3';

const writer = new Writer({ format: 'N-Triples' });
for (const quad of store) {
  writer.addQuad(quad);
}

writer.end((error, result) => {
  console.log(result);
});
...
```

### 5. MIGRATION.md - Step 1: Parallel Implementation (line 482)

**Errors:**

- N3 import in non-migration example
   → Use @unrdf/oxigraph instead of N3

**Code snippet:**

```javascript
// Old code (keep working)
import { Store as N3Store } from 'n3';
const n3Store = new N3Store();

// New code (build alongside)
import { createStore } from '@unrdf/oxigraph';
const oxiStore = createStore();

// Load same data in both
oxiStore.load(turtleData);
...
```

### 6. WALKTHROUGHS.md - Step 2: Create the Knowledge Base (line 41)

**Errors:**

- createKnowledgeSubstrateCore() does not exist in @unrdf/core exports
   → Use createStore() from @unrdf/oxigraph and executeSelectSync() from @unrdf/core
- core.parseRdf() is not a valid API - no "core" object exists
   → Use store.load() from @unrdf/oxigraph instead

**Code snippet:**

```javascript
import { createKnowledgeSubstrateCore } from '@unrdf/core';

// Initialize the core engine
const core = await createKnowledgeSubstrateCore();

// Define people and relationships in Turtle format
const data = `
  @prefix ex: <http://example.org/> .
  @prefix foaf: <http://xmlns.com/foaf/0.1/> .
  @prefix schema: <https://schema.org/> .
...
```

### 7. WALKTHROUGHS.md - Step 3: Create Query Functions (line 91)

**Errors:**

- core.query() is not a valid API
   → Use executeSelectSync(store, sparql) from @unrdf/core

**Code snippet:**

```javascript
import { core, store } from './knowledge-base.mjs';

/**
 * Find all people in the knowledge graph
 */
export async function findAllPeople() {
  const sparql = `
    PREFIX foaf: <http://xmlns.com/foaf/0.1/>

    SELECT ?name ?age ?job
...
```

### 8. WALKTHROUGHS.md - Step 1: Add Interactive CLI (line 299)

**Errors:**

- createKnowledgeSubstrateCore() does not exist in @unrdf/core exports
   → Use createStore() from @unrdf/oxigraph and executeSelectSync() from @unrdf/core
- core.parseRdf() is not a valid API - no "core" object exists
   → Use store.load() from @unrdf/oxigraph instead
- core.query() is not a valid API
   → Use executeSelectSync(store, sparql) from @unrdf/core

**Code snippet:**

```javascript
import readline from 'readline';
import { createKnowledgeSubstrateCore } from '@unrdf/core';

const core = await createKnowledgeSubstrateCore();

// Load sample data
const data = `
  @prefix ex: <http://example.org/> .
  @prefix foaf: <http://xmlns.com/foaf/0.1/> .

...
```

### 9. WALKTHROUGHS.md - Step 2: Create a Hook-Enabled Application (line 416)

**Errors:**

- createKnowledgeSubstrateCore() does not exist in @unrdf/core exports
   → Use createStore() from @unrdf/oxigraph and executeSelectSync() from @unrdf/core
- core.createStore() is not a valid API
   → Import createStore directly from @unrdf/oxigraph

**Code snippet:**

```javascript
import { createKnowledgeSubstrateCore } from '@unrdf/core';
import { KnowledgeHookManager } from '@unrdf/hooks';

const core = await createKnowledgeSubstrateCore();
const hookManager = new KnowledgeHookManager();

// Define a hook that logs when new people are added
const logNewPersonHook = {
  meta: {
    name: 'log-new-person',
...
```

### 10. WALKTHROUGHS.md - Step 2: Create Streaming Processor (line 536)

**Errors:**

- createKnowledgeSubstrateCore() does not exist in @unrdf/core exports
   → Use createStore() from @unrdf/oxigraph and executeSelectSync() from @unrdf/core
- core.parseRdf() is not a valid API - no "core" object exists
   → Use store.load() from @unrdf/oxigraph instead

**Code snippet:**

```javascript
import { createReadStream } from 'fs';
import { createKnowledgeSubstrateCore } from '@unrdf/core';

const core = await createKnowledgeSubstrateCore();

console.log('Processing large dataset...');
console.time('Processing time');

let tripleCount = 0;

...
```

### 11. WALKTHROUGHS.md - Step 2: Create Instrumented Application (line 603)

**Errors:**

- createKnowledgeSubstrateCore() does not exist in @unrdf/core exports
   → Use createStore() from @unrdf/oxigraph and executeSelectSync() from @unrdf/core
- core.parseRdf() is not a valid API - no "core" object exists
   → Use store.load() from @unrdf/oxigraph instead
- core.query() is not a valid API
   → Use executeSelectSync(store, sparql) from @unrdf/core

**Code snippet:**

```javascript
import { trace } from '@opentelemetry/api';
import { createKnowledgeSubstrateCore } from '@unrdf/core';

const tracer = trace.getTracer('unrdf-demo');

async function main() {
  const span = tracer.startSpan('main');

  try {
    const core = await createKnowledgeSubstrateCore();
...
```

## Recommendations

1. **Fix API mismatches:** Update documentation to use actual exported APIs
2. **Verify imports:** Ensure all examples import from correct packages
3. **Test examples:** Run examples to verify they execute correctly
4. **Update walkthroughs:** Align walkthrough code with current API
