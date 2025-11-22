# How-To: Troubleshoot Common Issues

Task-oriented guide for diagnosing and fixing common UNRDF problems.

## Quick Diagnostics

```javascript
// Check store has data
console.log('Store size:', store.size);

// Log first few quads
console.log('Sample quads:', [...store].slice(0, 5));

// Test simple query
const test = await select(store, 'SELECT * WHERE { ?s ?p ?o } LIMIT 1');
console.log('Query works:', test.length > 0);
```

## Issue: "Cannot find module 'unrdf'"

### Symptoms

```
Error: Cannot find module 'unrdf'
```

### Causes & Solutions

**1. Package not installed**

```bash
pnpm add unrdf
```

**2. Wrong import path**

```javascript
// Wrong
import { parseTurtle } from 'unrdf';

// Correct
import { parseTurtle } from 'unrdf/knowledge-engine';
```

**3. Missing ES modules config**

Add to `package.json`:

```json
{
  "type": "module"
}
```

## Issue: Parse Errors

### Symptoms

```
Error: Unexpected token at line 5
```

### Causes & Solutions

**1. Invalid Turtle syntax**

```turtle
# Wrong - missing period
ex:alice foaf:name "Alice"

# Correct
ex:alice foaf:name "Alice" .
```

**2. Undeclared prefix**

```turtle
# Wrong - prefix not declared
ex:alice foaf:name "Alice" .

# Correct
@prefix ex: <http://example.org/> .
@prefix foaf: <http://xmlns.com/foaf/0.1/> .
ex:alice foaf:name "Alice" .
```

**3. Encoding issues**

```javascript
// Ensure UTF-8 encoding
const data = readFileSync('./data.ttl', 'utf-8');
```

### Debug Tool

```javascript
async function debugParse(turtle) {
  try {
    const store = await parseTurtle(turtle);
    console.log(`Parsed ${store.size} triples`);
    return { success: true, store };
  } catch (error) {
    console.error('Parse error:', error.message);

    // Find line number
    const match = error.message.match(/line (\d+)/i);
    if (match) {
      const lineNum = parseInt(match[1]);
      const lines = turtle.split('\n');
      console.log('\nContext:');
      for (let i = Math.max(0, lineNum - 3); i < Math.min(lines.length, lineNum + 2); i++) {
        const marker = i + 1 === lineNum ? '>>>' : '   ';
        console.log(`${marker} ${i + 1}: ${lines[i]}`);
      }
    }

    return { success: false, error };
  }
}
```

## Issue: Empty Query Results

### Symptoms

```javascript
const results = await select(store, query);
console.log(results); // []
```

### Causes & Solutions

**1. Store is empty**

```javascript
console.log('Store size:', store.size);
if (store.size === 0) {
  console.log('No data loaded!');
}
```

**2. Missing await**

```javascript
// Wrong - store is a Promise
const store = parseTurtle(data);

// Correct
const store = await parseTurtle(data);
```

**3. Prefix mismatch**

```javascript
// Data uses
// ex:alice foaf:name "Alice"

// Query wrong prefix
const wrong = `
  PREFIX ex: <http://wrong.example/>  # Wrong!
  SELECT ?name WHERE { ex:alice foaf:name ?name }
`;

// Query correct prefix
const correct = `
  PREFIX ex: <http://example.org/>  # Matches data
  SELECT ?name WHERE { ex:alice foaf:name ?name }
`;
```

**4. Case sensitivity**

```javascript
// RDF is case-sensitive
// ex:Alice != ex:alice
```

### Debug Tool

```javascript
async function debugQuery(store, sparql) {
  console.log('Store size:', store.size);
  console.log('\nSample triples:');
  for (const quad of [...store].slice(0, 5)) {
    console.log(`  ${quad.subject.value} ${quad.predicate.value} ${quad.object.value}`);
  }

  console.log('\nQuery:');
  console.log(sparql);

  const results = await select(store, sparql);
  console.log('\nResults:', results.length);

  return results;
}
```

## Issue: Validation Always Passes

### Symptoms

```javascript
const report = await validateShacl(store, shapes);
console.log(report.conforms); // true (but shouldn't be)
```

### Causes & Solutions

**1. Target class mismatch**

```turtle
# Shape targets foaf:Person
sh:targetClass foaf:Person .

# But data uses ex:Person
ex:alice a ex:Person .  # Won't be validated!
```

**2. Shapes not loaded**

```javascript
// Check shapes parsed correctly
const shapesStore = await parseTurtle(shapes);
console.log('Shapes store size:', shapesStore.size);
```

**3. Wrong property path**

```turtle
# Shape checks foaf:name
sh:path foaf:name .

# Data uses ex:name
ex:alice ex:name "Alice" .  # Won't match!
```

### Debug Tool

```javascript
async function debugValidation(dataStore, shapesString) {
  console.log('Data store size:', dataStore.size);

  // Parse shapes to verify
  const shapesStore = await parseTurtle(shapesString);
  console.log('Shapes store size:', shapesStore.size);

  // Find target classes in shapes
  const targets = await select(shapesStore, `
    PREFIX sh: <http://www.w3.org/ns/shacl#>
    SELECT ?class WHERE { ?shape sh:targetClass ?class }
  `);
  console.log('Target classes:', targets.map(t => t.class.value));

  // Find types in data
  const types = await select(dataStore, `
    SELECT DISTINCT ?type WHERE { ?s a ?type }
  `);
  console.log('Data types:', types.map(t => t.type.value));

  // Run validation
  const report = await validateShacl(dataStore, shapesString);
  console.log('Conforms:', report.conforms);
  console.log('Results:', report.results?.length || 0);

  return report;
}
```

## Issue: Memory Errors

### Symptoms

```
FATAL ERROR: CALL_AND_RETRY_LAST Allocation failed - JavaScript heap out of memory
```

### Solutions

**1. Increase Node.js memory**

```bash
node --max-old-space-size=4096 app.mjs
```

**2. Stream large files**

```javascript
import { createReadStream } from 'node:fs';
import { Parser, Store } from 'n3';

async function streamParse(filePath) {
  const store = new Store();
  const parser = new Parser();
  const stream = createReadStream(filePath, 'utf-8');

  return new Promise((resolve, reject) => {
    stream.pipe(parser);
    parser.on('data', quad => store.addQuad(quad));
    parser.on('error', reject);
    parser.on('end', () => resolve(store));
  });
}
```

**3. Process in batches**

```javascript
async function processLargeDataset(quads, batchSize = 10000) {
  for (let i = 0; i < quads.length; i += batchSize) {
    const batch = quads.slice(i, i + batchSize);
    await processBatch(batch);

    // Force garbage collection hint
    if (global.gc) global.gc();
  }
}
```

## Issue: Slow Queries

### Symptoms

Queries taking seconds instead of milliseconds.

### Solutions

**1. Check query complexity**

```javascript
// Simple timing
const start = Date.now();
const results = await select(store, query);
console.log(`Query took ${Date.now() - start}ms`);
```

**2. Add LIMIT during development**

```javascript
const devQuery = `${query} LIMIT 100`;
```

**3. Reorder patterns**

```javascript
// Put most selective patterns first
const optimized = `
  SELECT ?name WHERE {
    ?p a foaf:Person .           # Specific type first
    ?p foaf:name ?name .         # Then properties
  }
`;
```

## Issue: Hook Not Triggering

### Symptoms

Hook defined but never executes.

### Causes & Solutions

**1. Condition file missing**

```javascript
import { existsSync } from 'node:fs';

const conditionUri = hook.when.ref.uri.replace('file://', '');
if (!existsSync(conditionUri)) {
  console.error(`Condition file not found: ${conditionUri}`);
}
```

**2. SHA-256 mismatch**

```javascript
import { createHash } from 'node:crypto';
import { readFileSync } from 'node:fs';

const content = readFileSync(conditionUri, 'utf-8');
const actualHash = createHash('sha256').update(content).digest('hex');
const expectedHash = hook.when.ref.sha256;

if (actualHash !== expectedHash) {
  console.error(`Hash mismatch!`);
  console.error(`Expected: ${expectedHash}`);
  console.error(`Actual: ${actualHash}`);
}
```

**3. Wrong graph scope**

```javascript
// Hook watches specific graph
channel: { graphs: ['urn:graph:production'] }

// But data is in different graph
// Make sure data is in the watched graph
```

## General Debugging Tips

### Enable Verbose Logging

```javascript
process.env.DEBUG = 'unrdf:*';
```

### Create Minimal Reproduction

```javascript
// Isolate the problem
const minimalData = `
  @prefix ex: <http://example.org/> .
  ex:test ex:value "1" .
`;

const minimalQuery = `SELECT * WHERE { ?s ?p ?o }`;

const store = await parseTurtle(minimalData);
const results = await select(store, minimalQuery);
console.log(results);
```

### Check Dependencies

```bash
pnpm list unrdf n3 @comunica/query-sparql
```

## Related

- [Getting Started](../GETTING_STARTED.md) - Setup guide
- [FAQ](../FAQ.md) - Frequently asked questions
- [API Reference](../reference/api-reference.md) - Full API docs
