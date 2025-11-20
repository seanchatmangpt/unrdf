# The Dark Matter Problem

In physics, dark matter accounts for 85% of the universe's mass but is invisible and unproductive. In software, **dark matter code** accounts for 80% of your codebase but delivers only 20% of value.

## What is Dark Matter Code?

Dark matter code is the invisible mass of boilerplate, glue code, configuration, and infrastructure that:

- ❌ **Takes 80% of development time**
- ❌ **Delivers 20% of business value**
- ❌ **Creates 80% of bugs**
- ❌ **Requires 80% of maintenance effort**
- ❌ **Provides 0% competitive advantage**

### Example: Traditional RDF Application

```typescript
// ⚫ DARK MATTER: 400 lines of setup (80% of code, 20% of value)

import { Store } from 'n3';
import { Engine } from '@comunica/query-sparql';
import SHACLValidator from 'rdf-validate-shacl';

// Store configuration (50 lines)
const store = new Store();
const baseIRI = 'http://example.org/';
const graphName = namedNode(`${baseIRI}graph1`);

// Transaction manager (100 lines)
class TransactionManager {
  private pending: Quad[] = [];
  private locks: Map<string, boolean> = new Map();
  
  async begin() { /* ... */ }
  async commit() { /* ... */ }
  async rollback() { /* ... */ }
  // ... 80 more lines
}

// Query engine setup (80 lines)
const engine = new Engine();
const context = {
  sources: [store],
  unionDefaultGraph: true,
  // ... 60 more config lines
};

// SPARQL result parsing (100 lines)
function parseResults(bindings) {
  return bindings.map(binding => {
    const result = {};
    binding.forEach((term, variable) => {
      result[variable.value] = term.value;
    });
    return result;
  });
}

// Validation setup (70 lines)
const validator = new SHACLValidator(shapesGraph);
async function validate(data) {
  const report = await validator.validate(data);
  if (!report.conforms) {
    // ... 50 lines of error handling
  }
}

// ⭐ PRODUCTIVE CODE: 50 lines of business logic (20% of code, 80% of value)

async function getProducts() {
  const tx = await transactionManager.begin();
  try {
    const results = await engine.query(`
      SELECT ?name ?price WHERE {
        ?product schema:name ?name ;
                 schema:price ?price .
      }
    `, context);
    
    const products = parseResults(results);
    await validate(products);
    await tx.commit();
    return products;
  } catch (error) {
    await tx.rollback();
    throw error;
  }
}
```

**Analysis:**
- **450 total lines**
- **400 lines (89%) = dark matter** (setup, boilerplate, infrastructure)
- **50 lines (11%) = productive** (actual business logic)

**Result:** 9:1 ratio of dark matter to productive code!

## The Same with UNRDF

```typescript
// ⭐ ALL PRODUCTIVE CODE: 15 lines (100% business value)

import { createKnowledgeEngine } from 'unrdf';

const engine = await createKnowledgeEngine(); // Handles all setup

async function getProducts() {
  return engine.queryTyped({
    query: `
      SELECT ?name ?price WHERE {
        ?product schema:name ?name ;
                 schema:price ?price .
      }
    `,
    schema: ProductSchema // Type-safe + validates automatically
  });
}
```

**Analysis:**
- **15 total lines**
- **0 lines (0%) = dark matter**
- **15 lines (100%) = productive**

**Improvement:** 30x less code, 0% dark matter!

## Dark Matter Categories

### 1. Configuration Dark Matter
Endless setup, initialization, and wiring code.

**Traditional:**
```typescript
// 200 lines of configuration
const storeConfig = { /* ... */ };
const engineConfig = { /* ... */ };
const validatorConfig = { /* ... */ };
// Connect everything together (50 lines)
```

**UNRDF:**
```typescript
const engine = await createKnowledgeEngine(); // Done
```

### 2. Transaction Dark Matter
Manual ACID handling, locks, rollback logic.

**Traditional:**
```typescript
// 100+ lines per transaction
const tx = await begin();
try {
  // operation
  await commit();
} catch {
  await rollback();
}
```

**UNRDF:**
```typescript
await engine.insert(quads); // ACID built-in
```

### 3. Parsing Dark Matter
Converting between formats, extracting values, type coercion.

**Traditional:**
```typescript
// 80 lines of parsing logic
function parseBindings(bindings) {
  // Extract values, handle nulls, convert types...
}
```

**UNRDF:**
```typescript
const results = await engine.queryTyped({ schema }); // Parsed & typed
```

### 4. Validation Dark Matter
Scattered validation logic across the codebase.

**Traditional:**
```typescript
// 50 lines in file1.ts
if (!data.name) throw new Error('...');
// 40 lines in file2.ts
if (data.price < 0) throw new Error('...');
// ... 10 more files
```

**UNRDF:**
```typescript
engine.setPolicyPack(productPolicy); // Enforced everywhere
```

### 5. Caching Dark Matter
Manual cache management, invalidation, coherence.

**Traditional:**
```typescript
// 150 lines of cache logic
const cache = new Map();
function getCached(key) {
  if (cache.has(key) && !isStale(key)) {
    return cache.get(key);
  }
  // ... cache invalidation logic
}
```

**UNRDF:**
```typescript
// Automatic intelligent caching built-in
const results = await engine.query(sparql); // Cached automatically
```

### 6. Observability Dark Matter
Manual instrumentation, logging, tracing.

**Traditional:**
```typescript
// 100 lines per operation
const span = tracer.startSpan('query');
try {
  const result = await query();
  span.setStatus({ code: SpanStatusCode.OK });
  return result;
} catch (error) {
  span.recordException(error);
  span.setStatus({ code: SpanStatusCode.ERROR });
  throw error;
} finally {
  span.end();
}
```

**UNRDF:**
```typescript
// OpenTelemetry spans created automatically
const results = await engine.query(sparql); // Instrumented
```

## Measuring Your Dark Matter

Calculate your dark matter ratio:

```
Dark Matter Ratio = (LOC_boilerplate / LOC_total) × 100%

Healthy codebase: < 30% dark matter
Typical RDF app: 70-90% dark matter
UNRDF app: 0-10% dark matter
```

### Audit Exercise

Count lines in your codebase:

1. **Setup/Configuration:** Store, engine, validator setup
2. **Infrastructure:** Transactions, caching, error handling
3. **Parsing/Conversion:** Result parsing, type conversion
4. **Validation:** Manual validation logic
5. **Observability:** Logging, tracing, metrics
6. **Business Logic:** Actual domain code

**Typical results:**
- 1-5: **80-90% of code** (dark matter)
- 6: **10-20% of code** (productive)

## The Dark Energy Alternative

Instead of dark matter, UNRDF provides **dark energy** — productive abstractions that accelerate development:

```
Dark Matter (Bad)          →  Dark Energy (Good)
────────────────────────────────────────────────────
Boilerplate configuration  →  Smart defaults
Manual transaction code    →  Automatic ACID
Result parsing            →  Type-safe queries
Scattered validation      →  Policy packs
Manual caching           →  Intelligent caching
Manual instrumentation   →  Auto observability
```

**Next:**
- **[Identifying Your Dark Matter](./identifying.md)**
- **[Eliminating Boilerplate](./eliminating-boilerplate.md)**
- **[Dark Energy Abstractions](./dark-energy.md)**

---

> **⚡ Dark Matter Alert:** Any code that exists purely to wire other code together is dark matter. Eliminate it.
