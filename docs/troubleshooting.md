# Troubleshooting Guide

Common issues, solutions, and debugging techniques for UNRDF.

## Table of Contents

- [Installation Issues](#installation-issues)
- [Import and Module Errors](#import-and-module-errors)
- [RDF Parsing Errors](#rdf-parsing-errors)
- [SPARQL Query Issues](#sparql-query-issues)
- [Performance Problems](#performance-problems)
- [Memory Leaks](#memory-leaks)
- [Test Failures](#test-failures)
- [Build Errors](#build-errors)
- [Debugging Techniques](#debugging-techniques)

---

## Installation Issues

### Error: "Cannot find module '@unrdf/core'"

**Symptoms:**
```
Error: Cannot find module '@unrdf/core'
```

**Causes:**
1. Package not installed
2. Incorrect import path
3. Node modules cache corruption

**Solutions:**

```bash
# 1. Install the package
pnpm add @unrdf/core

# 2. Clear cache and reinstall
rm -rf node_modules pnpm-lock.yaml
pnpm install

# 3. Verify installation
pnpm list @unrdf/core
```

**Verification:**
```javascript
// ❌ WRONG
import { createKnowledgeSubstrateCore } from 'unrdf';

// ✅ CORRECT
import { createKnowledgeSubstrateCore } from '@unrdf/core';
```

---

### Error: "pnpm: command not found"

**Symptoms:**
```bash
pnpm install
-bash: pnpm: command not found
```

**Solution:**
```bash
# Install pnpm globally
npm install -g pnpm@8

# Verify installation
pnpm --version
# Expected: 8.x.x

# Alternative: Use npx
npx pnpm install
```

---

### Error: "Node version incompatible"

**Symptoms:**
```
error @unrdf/core@5.0.1: The engine "node" is incompatible with this module.
Expected version ">=18.0.0". Got "16.14.0"
```

**Solution:**
```bash
# Check Node version
node --version

# Upgrade Node.js (using nvm)
nvm install 18
nvm use 18

# Or download from nodejs.org
# https://nodejs.org/en/download/
```

---

## Import and Module Errors

### Error: "ERR_MODULE_NOT_FOUND"

**Symptoms:**
```
Error [ERR_MODULE_NOT_FOUND]: Cannot find module '.../index.mjs'
```

**Causes:**
1. File extension missing (`.mjs` required)
2. Incorrect relative path
3. Circular dependency

**Solutions:**

```javascript
// ❌ WRONG: Missing .mjs extension
import { query } from './sparql/executor';

// ✅ CORRECT
import { query } from './sparql/executor.mjs';

// ❌ WRONG: Relative path to N3 (forbidden)
import { Store } from 'n3';

// ✅ CORRECT: Use Oxigraph
import { createStore } from '@unrdf/oxigraph';
```

**Verification:**
```bash
# Check for forbidden N3 imports
grep -r "from 'n3'" packages/*/src --exclude-dir=justified

# Should return 0 results (except in justified modules)
```

---

### Error: "Unexpected token 'export'"

**Symptoms:**
```
SyntaxError: Unexpected token 'export'
```

**Cause:** Using CommonJS instead of ES Modules.

**Solution:**

```json
// package.json - MUST include this
{
  "type": "module"
}
```

```javascript
// ❌ WRONG: CommonJS syntax
const { createKnowledgeSubstrateCore } = require('@unrdf/core');

// ✅ CORRECT: ES Module syntax
import { createKnowledgeSubstrateCore } from '@unrdf/core';
```

---

## RDF Parsing Errors

### Error: "Unexpected ... in graph at line X"

**Symptoms:**
```
Error: Unexpected ">" in graph at line 3.
```

**Cause:** Invalid Turtle syntax.

**Solution:**

```turtle
# ❌ WRONG: Missing final period
@prefix ex: <http://example.org/> .
ex:Alice ex:knows ex:Bob

# ✅ CORRECT
@prefix ex: <http://example.org/> .
ex:Alice ex:knows ex:Bob .

# ❌ WRONG: Malformed URI
ex:Alice ex:knows <http://example.org/Bob> .

# ✅ CORRECT: Escaped angle brackets
ex:Alice ex:knows <http://example.org/Bob> .
```

**Debugging:**
```javascript
import { parseTurtle } from '@unrdf/core/rdf';

try {
  const store = await parseTurtle(turtleData);
} catch (error) {
  console.error('Parse error:', error.message);
  console.error('Line:', error.line, 'Column:', error.column);

  // Show context around error
  const lines = turtleData.split('\n');
  const contextStart = Math.max(0, error.line - 3);
  const contextEnd = Math.min(lines.length, error.line + 2);

  console.error('Context:');
  for (let i = contextStart; i < contextEnd; i++) {
    console.error(`${i + 1}: ${lines[i]}`);
  }
}
```

---

### Error: "Prefix not defined"

**Symptoms:**
```
Error: Prefix "foaf" not defined
```

**Cause:** Using prefix without declaring it.

**Solution:**

```turtle
# ❌ WRONG: Using undeclared prefix
ex:Alice foaf:name "Alice" .

# ✅ CORRECT: Declare prefix first
@prefix ex: <http://example.org/> .
@prefix foaf: <http://xmlns.com/foaf/0.1/> .

ex:Alice foaf:name "Alice" .
```

**Common RDF Prefixes:**
```turtle
@prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .
@prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> .
@prefix owl: <http://www.w3.org/2002/07/owl#> .
@prefix xsd: <http://www.w3.org/2001/XMLSchema#> .
@prefix foaf: <http://xmlns.com/foaf/0.1/> .
@prefix dct: <http://purl.org/dc/terms/> .
```

---

## SPARQL Query Issues

### Error: "SPARQL query returned no results"

**Symptoms:**
```javascript
const results = await query(store, sparql);
console.log(results); // []
```

**Debugging Checklist:**

1. **Verify data exists:**
   ```javascript
   console.log('Store size:', store.size);
   // Should be > 0
   ```

2. **Check prefixes match:**
   ```sparql
   # ❌ WRONG: Mismatched prefix
   PREFIX ex: <http://example.com/>  # Note: .com
   SELECT * WHERE {
     ?s ?p ?o .
   }

   # When data uses http://example.org/ (Note: .org)

   # ✅ CORRECT: Match data prefixes
   PREFIX ex: <http://example.org/>
   SELECT * WHERE {
     ?s ?p ?o .
   }
   ```

3. **Simplify query:**
   ```sparql
   # Start simple
   SELECT * WHERE { ?s ?p ?o }

   # Add constraints incrementally
   SELECT * WHERE {
     ?s a ex:Person .
   }

   SELECT * WHERE {
     ?s a ex:Person ;
        ex:name ?name .
   }
   ```

4. **Use OPTIONAL for debugging:**
   ```sparql
   SELECT * WHERE {
     ?person a ex:Person .
     OPTIONAL { ?person ex:name ?name }
     OPTIONAL { ?person ex:email ?email }
   }
   # Shows which properties are missing
   ```

---

### Error: "Query timeout exceeded"

**Symptoms:**
```
Error: Query timeout exceeded (30000ms)
```

**Causes:**
1. Query too complex
2. Large dataset
3. Missing indexes

**Solutions:**

```javascript
// Increase timeout
const results = await query(store, sparql, {
  timeout: 60000  // 60 seconds
});

// Add LIMIT to reduce results
const sparql = `
  SELECT * WHERE {
    ?s ?p ?o .
  }
  LIMIT 1000
`;

// Use streaming for large results
import { queryStream } from '@unrdf/streaming';

const stream = queryStream(store, sparql);
stream.on('data', (binding) => {
  console.log(binding);
});
```

**Optimize query:**
```sparql
# ❌ SLOW: Cartesian product
SELECT * WHERE {
  ?person1 ex:knows ?person2 .
  ?person3 ex:knows ?person4 .
}

# ✅ FAST: Specific pattern
SELECT * WHERE {
  ?person1 ex:knows ?person2 .
  FILTER (?person1 = <http://example.org/Alice>)
}
```

---

## Performance Problems

### Problem: Slow query execution

**Symptoms:**
- Queries taking >5 seconds
- High CPU usage
- Increased memory consumption

**Diagnosis:**

```bash
# Enable OTEL tracing
OTEL_ENABLED=true node your-app.mjs

# View traces in Jaeger
# http://localhost:16686
```

**Solutions:**

1. **Add indexes (Oxigraph backend):**
   ```javascript
   import { createStore } from '@unrdf/oxigraph';

   const store = createStore({
     indexes: ['spo', 'pos', 'osp']  // Common access patterns
   });
   ```

2. **Use query cache:**
   ```javascript
   import { createKnowledgeSubstrateCore } from '@unrdf/core';

   const core = await createKnowledgeSubstrateCore({
     enableQueryCache: true,
     cacheTTL: 300  // 5 minutes
   });
   ```

3. **Batch operations:**
   ```javascript
   // ❌ SLOW: Individual adds
   for (const quad of quads) {
     store.addQuad(quad);
   }

   // ✅ FAST: Batch add
   store.addQuads(quads);
   ```

---

### Problem: High memory usage

**Symptoms:**
```bash
FATAL ERROR: Reached heap limit Allocation failed - JavaScript heap out of memory
```

**Diagnosis:**

```bash
# Monitor memory
node --expose-gc --max-old-space-size=4096 your-app.mjs

# Heap snapshot
node --inspect your-app.mjs
# Chrome DevTools → Memory → Take snapshot
```

**Solutions:**

1. **Use streaming for large datasets:**
   ```javascript
   import { parseStream } from '@unrdf/streaming';
   import { createReadStream } from 'fs';

   const stream = createReadStream('large-file.ttl');
   const quadStream = parseStream(stream);

   quadStream.on('data', (quad) => {
     // Process one quad at a time (constant memory)
     processQuad(quad);
   });
   ```

2. **Call destroy() on stores:**
   ```javascript
   const store = createStore();
   // ... use store ...
   store.destroy();  // Free memory
   ```

3. **Limit result sets:**
   ```sparql
   SELECT * WHERE {
     ?s ?p ?o .
   }
   LIMIT 10000  -- Prevent unbounded results
   ```

---

## Memory Leaks

### Detection

```javascript
// Track store instances
const stores = new Set();

function createStoreWithTracking() {
  const store = createStore();
  stores.add(store);
  return store;
}

// Periodically check
setInterval(() => {
  console.log('Active stores:', stores.size);
  // Should not continuously grow
}, 5000);
```

### Common Causes

1. **Not calling destroy():**
   ```javascript
   // ❌ LEAK
   function processData() {
     const store = createStore();
     // ... use store ...
     // Missing: store.destroy()
   }

   // ✅ FIXED
   function processData() {
     const store = createStore();
     try {
       // ... use store ...
     } finally {
       store.destroy();
     }
   }
   ```

2. **Event listener leaks:**
   ```javascript
   // ❌ LEAK
   store.on('change', handler);
   // Missing: store.off('change', handler)

   // ✅ FIXED
   store.on('change', handler);
   try {
     // ... use store ...
   } finally {
     store.off('change', handler);
   }
   ```

---

## Test Failures

### Error: "Vitest version mismatch"

**Symptoms:**
```
Error: Vitest version mismatch
```

**Solution:**

```bash
# Remove all node_modules
find . -name "node_modules" -type d -prune -exec rm -rf {} \;

# Remove lock file
rm pnpm-lock.yaml

# Reinstall with exact versions
pnpm install --frozen-lockfile

# Verify versions match
pnpm list vitest
```

---

### Error: "Test timeout exceeded"

**Symptoms:**
```
Test timed out after 5000ms
```

**Solution:**

```javascript
// Increase timeout for specific test
it('long running test', async () => {
  // Test code
}, 10000);  // 10 second timeout

// Or in vitest.config.mjs
export default {
  test: {
    testTimeout: 10000
  }
};
```

---

## Build Errors

### Error: "Module parse failed"

**Symptoms:**
```
Module parse failed: Unexpected token
```

**Cause:** Build tool doesn't recognize `.mjs` extension.

**Solution:**

```javascript
// vitest.config.mjs
export default {
  test: {
    include: ['**/*.test.mjs'],
    globals: true
  }
};
```

---

## Debugging Techniques

### Enable Debug Logging

```javascript
// Set log level
process.env.LOG_LEVEL = 'debug';

import { createKnowledgeSubstrateCore } from '@unrdf/core';

const core = await createKnowledgeSubstrateCore({
  logLevel: 'debug'
});
```

### OTEL Tracing

```javascript
// Enable tracing
process.env.OTEL_ENABLED = 'true';
process.env.OTEL_EXPORTER_OTLP_ENDPOINT = 'http://localhost:4318';

import { trace } from '@opentelemetry/api';

const tracer = trace.getTracer('my-app');

const span = tracer.startSpan('custom-operation');
try {
  // Your code
} finally {
  span.end();
}
```

### Node.js Debugger

```bash
# Start with debugger
node --inspect-brk packages/cli/src/index.mjs

# Open Chrome DevTools
# chrome://inspect
```

### VSCode Debugging

Create `.vscode/launch.json`:

```json
{
  "version": "0.2.0",
  "configurations": [
    {
      "type": "node",
      "request": "launch",
      "name": "Debug UNRDF",
      "program": "${workspaceFolder}/packages/cli/src/index.mjs",
      "args": ["query", "data.ttl"],
      "console": "integratedTerminal"
    }
  ]
}
```

---

## Getting Help

If you're still stuck:

1. **Check existing issues:** https://github.com/unrdf/unrdf/issues
2. **Search discussions:** https://github.com/unrdf/unrdf/discussions
3. **Ask in Discord:** [Coming soon]
4. **Open an issue:**
   - Include UNRDF version (`pnpm list @unrdf/core`)
   - Include Node.js version (`node --version`)
   - Provide minimal reproducible example
   - Include full error message and stack trace

---

**Next:** [Production Deployment Guide](deployment/production.md)
