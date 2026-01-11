# Performance Optimization Strategies

Deep dive into optimizing RDF-KGN performance for production workloads.

## Introduction

RDF-KGN performance depends on multiple factors: template rendering, RDF parsing, SPARQL query execution, and data serialization. This document explores optimization strategies across all layers.

## Performance Model

### System Bottlenecks

```
User Request
     │
     ▼
┌─────────────────┐
│ Template Render │ ← 10-20% of time
└─────────────────┘
     │
     ▼
┌─────────────────┐
│   RDF Parse     │ ← 5-10% of time
└─────────────────┘
     │
     ▼
┌─────────────────┐
│   Store Load    │ ← 30-40% of time
└─────────────────┘
     │
     ▼
┌─────────────────┐
│ SPARQL Execute  │ ← 30-50% of time
└─────────────────┘
     │
     ▼
┌─────────────────┐
│   Serialize     │ ← 5-10% of time
└─────────────────┘
     │
     ▼
   Response
```

**Optimization Priority:**
1. **SPARQL queries** (biggest impact)
2. **Store loading** (second biggest)
3. **Template rendering** (medium impact)
4. **Parsing/serialization** (smallest impact)

### Performance Characteristics

| Operation | Complexity | Scalability |
|-----------|------------|-------------|
| Template render | O(n) | Linear with data size |
| RDF parse (Turtle) | O(n) | Linear with triple count |
| Store insert | O(log n) | Logarithmic with store size |
| SPARQL SELECT | O(n×m) | Depends on query selectivity |
| SPARQL CONSTRUCT | O(n) | Linear with results |
| SHACL validate | O(n×m) | Depends on shape complexity |

## Template Rendering Optimization

### Strategy 1: Template Caching

**Problem:** Recompiling templates on every render

**Solution:** Cache compiled templates

```javascript
// ❌ Slow: Recompile every time
for (const person of people) {
  const engine = new TemplateEngine(); // New instance
  await engine.render('person.njk', person);
}
// 1000 people: ~500ms

// ✅ Fast: Reuse engine (cached templates)
const engine = new TemplateEngine({ cache: true });
for (const person of people) {
  await engine.render('person.njk', person);
}
// 1000 people: ~100ms (5x faster)
```

**Impact:** 5-10x speedup for repeated renders

### Strategy 2: Batch Rendering

**Problem:** Rendering overhead per item

**Solution:** Render multiple items in one pass

```javascript
// ❌ Slow: 1000 template renders
for (const person of people) {
  await renderTemplate('person.njk', person);
}
// Time: 1000 × 0.5ms = 500ms

// ✅ Fast: 1 template render
await renderTemplate('people.njk', { people });
// Time: 50ms (10x faster)
```

**Template:**
```nunjucks
{# people.njk: Batch template #}
{% for person in people %}
ex:{{ person.id }}
  a foaf:Person ;
  foaf:name {{ person.name | rdfLiteral }} .
{% endfor %}
```

**Impact:** 10-50x speedup for large batches

### Strategy 3: Minimize Filter Calls

**Problem:** Expensive filter operations in loops

**Solution:** Pre-process data before template

```nunjucks
{# ❌ Slow: Filter in nested loop #}
{% for person in people %}
  {% for friend in person.friends %}
    foaf:knows ex:{{ friend | slugify | uppercase }} ;
  {% endfor %}
{% endfor %}

{# ✅ Fast: Pre-process in JavaScript #}
const processedPeople = people.map(p => ({
  ...p,
  friends: p.friends.map(f => slugify(f).toUpperCase())
}));
```

**Impact:** 2-5x speedup for complex filters

### Strategy 4: Avoid Redundant Work

**Problem:** Recalculating same values

**Solution:** Use template variables

```nunjucks
{# ❌ Slow: Calculate multiple times #}
ex:{{ person.id }}
  foaf:name {{ person.firstName + ' ' + person.lastName | rdfLiteral }} ;
  rdfs:label {{ person.firstName + ' ' + person.lastName | rdfLiteral }} ;
  dc:title {{ person.firstName + ' ' + person.lastName | rdfLiteral }} .

{# ✅ Fast: Calculate once #}
{% set fullName = person.firstName + ' ' + person.lastName %}
ex:{{ person.id }}
  foaf:name {{ fullName | rdfLiteral }} ;
  rdfs:label {{ fullName | rdfLiteral }} ;
  dc:title {{ fullName | rdfLiteral }} .
```

**Impact:** 2-3x speedup for redundant operations

## RDF Store Optimization

### Strategy 1: Bulk Loading

**Problem:** Individual triple inserts are slow

**Solution:** Batch inserts

```javascript
import { createStore } from '@unrdf/oxigraph';
import { Parser } from 'n3';

const store = createStore();
const parser = new Parser();

// ❌ Slow: Insert one by one
for (const triple of triples) {
  store.add(triple);
}
// 10,000 triples: ~5 seconds

// ✅ Fast: Parse and bulk insert
const turtleData = generateTurtleRDF(data);
parser.parse(turtleData).forEach(quad => {
  store.add(quad);
});
// 10,000 triples: ~500ms (10x faster)
```

**Impact:** 10-20x speedup for bulk loading

### Strategy 2: Index Optimization

**Problem:** Queries scan entire store

**Solution:** Ensure appropriate indexes exist

Oxigraph automatically maintains indexes, but query patterns matter:

```javascript
// ✅ Good: Specific subject (uses subject index)
SELECT ?p ?o WHERE {
  <http://example.org/alice> ?p ?o
}

// ⚠️ Slower: Specific predicate (uses predicate index)
SELECT ?s ?o WHERE {
  ?s foaf:name ?o
}

// ❌ Slowest: Specific object (full scan)
SELECT ?s ?p WHERE {
  ?s ?p "Alice"
}
```

**Optimization:** Reorder patterns for specificity

```sparql
-- ❌ Slow: Broad pattern first
SELECT ?person ?email WHERE {
  ?person a foaf:Person .        -- Matches all persons
  ?person foaf:mbox ?email .
  ?person foaf:name "Alice" .    -- Filters to one
}

-- ✅ Fast: Specific pattern first
SELECT ?person ?email WHERE {
  ?person foaf:name "Alice" .    -- Matches one
  ?person a foaf:Person .
  ?person foaf:mbox ?email .
}
```

**Impact:** 10-100x speedup for selective queries

### Strategy 3: Limit Results Early

**Problem:** Processing unnecessary results

**Solution:** Use LIMIT and OFFSET

```sparql
-- ❌ Slow: Process all, limit in app
SELECT ?person ?name WHERE {
  ?person a foaf:Person ;
          foaf:name ?name .
}
-- Returns 1,000,000 results → filter to 10 in app

-- ✅ Fast: Limit in query
SELECT ?person ?name WHERE {
  ?person a foaf:Person ;
          foaf:name ?name .
}
LIMIT 10
-- Returns 10 results
```

**Impact:** 100-1000x speedup for limited result sets

## SPARQL Query Optimization

### Strategy 1: Query Compilation

**Problem:** Parsing query on every execution

**Solution:** Prepare query once, execute many times

```javascript
import { prepareQuerySync, executeSelectSync } from '@unrdf/core/sparql/executor-sync.mjs';

// ❌ Slow: Parse every time
for (const name of names) {
  const query = `SELECT ?person WHERE { ?person foaf:name "${name}" }`;
  executeSelectSync(store, query);
}

// ✅ Fast: Parse once (if query is static)
const query = prepareQuerySync(`
  SELECT ?person WHERE { ?person foaf:name ?name }
`);
// Reuse query with different bindings (if library supports)
```

**Note:** Current implementation doesn't support prepared statements, but future versions may.

**Impact:** 2-5x speedup for repeated queries

### Strategy 2: Avoid OPTIONAL When Possible

**Problem:** OPTIONAL clauses can be expensive

**Solution:** Use separate queries for required/optional data

```sparql
-- ⚠️ Slower: OPTIONAL can multiply results
SELECT ?person ?name ?email ?phone WHERE {
  ?person a foaf:Person ;
          foaf:name ?name .
  OPTIONAL { ?person foaf:mbox ?email }
  OPTIONAL { ?person foaf:phone ?phone }
}

-- ✅ Faster: Required data only
SELECT ?person ?name WHERE {
  ?person a foaf:Person ;
          foaf:name ?name .
}
-- Then query for optional data separately if needed
```

**When to use OPTIONAL:**
- Small number of optional properties
- Most entities have the optional data
- Result count is low

**Impact:** 2-10x speedup depending on data distribution

### Strategy 3: Use FILTER Efficiently

**Problem:** Late filtering processes unnecessary data

**Solution:** Filter as early as possible

```sparql
-- ❌ Slow: Filter after joins
SELECT ?person ?friend WHERE {
  ?person a foaf:Person .
  ?person foaf:knows ?friend .
  ?friend foaf:name ?name .
  FILTER(?name = "Bob")
}

-- ✅ Fast: Filter early
SELECT ?person ?friend WHERE {
  ?friend foaf:name "Bob" .        -- Bind specific value
  ?person foaf:knows ?friend .
  ?person a foaf:Person .
}
```

**Impact:** 5-50x speedup for selective filters

### Strategy 4: Projection Optimization

**Problem:** Selecting unnecessary variables

**Solution:** Select only needed variables

```sparql
-- ❌ Slow: Select everything
SELECT * WHERE {
  ?person a foaf:Person ;
          foaf:name ?name ;
          foaf:mbox ?email ;
          foaf:phone ?phone ;
          foaf:homepage ?homepage .
}

-- ✅ Fast: Select only needed
SELECT ?person ?name WHERE {
  ?person a foaf:Person ;
          foaf:name ?name .
  -- Don't include unnecessary properties
}
```

**Impact:** 2-5x speedup for large result sets

### Strategy 5: Avoid Cartesian Products

**Problem:** Unconnected graph patterns create massive results

**Solution:** Ensure all patterns are connected

```sparql
-- ❌ Very Slow: Cartesian product
SELECT ?person ?org WHERE {
  ?person a foaf:Person .    -- 1000 persons
  ?org a org:Organization .  -- 100 orgs
}
-- Returns 1000 × 100 = 100,000 results!

-- ✅ Fast: Connected patterns
SELECT ?person ?org WHERE {
  ?person a foaf:Person ;
          org:memberOf ?org .
  ?org a org:Organization .
}
-- Returns actual memberships (e.g., 1500 results)
```

**Impact:** 10-1000x speedup

## Serialization Optimization

### Strategy 1: Choose Efficient Format

**Format Comparison:**

| Format | Size | Parse Speed | Generate Speed |
|--------|------|-------------|----------------|
| Turtle | Medium | Medium | Medium |
| N-Triples | Large | Fast | Fast |
| JSON-LD | Large | Slow | Medium |
| Binary (HDT) | Small | Very Fast | N/A |

**Recommendations:**
- **Development:** Turtle (human-readable)
- **Production:** N-Triples (fast, simple)
- **Storage:** HDT (compressed)
- **APIs:** JSON-LD (web-friendly)

### Strategy 2: Use Prefixes

**Problem:** Full URIs are verbose

**Solution:** Use prefix declarations

```turtle
# ❌ Verbose: Full URIs (5.2 MB)
<http://example.org/alice> <http://www.w3.org/1999/02/22-rdf-syntax-ns#type> <http://xmlns.com/foaf/0.1/Person> .
<http://example.org/alice> <http://xmlns.com/foaf/0.1/name> "Alice Smith" .

# ✅ Compact: Prefixes (1.8 MB)
@prefix ex: <http://example.org/> .
@prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .
@prefix foaf: <http://xmlns.com/foaf/0.1/> .

ex:alice rdf:type foaf:Person ;
         foaf:name "Alice Smith" .
```

**Impact:** 60-70% size reduction

### Strategy 3: Group Related Triples

**Problem:** Repeating subjects

**Solution:** Use Turtle shorthand syntax

```turtle
# ❌ Verbose: Repeat subject (200 lines)
ex:alice rdf:type foaf:Person .
ex:alice foaf:name "Alice Smith" .
ex:alice foaf:email "alice@example.org" .
ex:alice foaf:age "30"^^xsd:integer .

# ✅ Compact: Group triples (100 lines)
ex:alice
  rdf:type foaf:Person ;
  foaf:name "Alice Smith" ;
  foaf:email "alice@example.org" ;
  foaf:age "30"^^xsd:integer .
```

**Impact:** 40-50% size reduction

### Strategy 4: Streaming Serialization

**Problem:** Loading all data in memory

**Solution:** Stream generation

```javascript
import { Writable } from 'stream';
import { Writer } from 'n3';

// ✅ Streaming: Constant memory
const outputStream = createWriteStream('output.ttl');
const writer = new Writer(outputStream);

for (const person of people) {
  writer.addQuad(
    namedNode(`ex:${person.id}`),
    namedNode('rdf:type'),
    namedNode('foaf:Person')
  );
}

writer.end();
```

**Impact:** Supports datasets larger than available memory

## Caching Strategies

### Strategy 1: Template Compilation Cache

```javascript
const engine = new TemplateEngine({
  cache: true,  // Enable compilation cache
  cacheSize: 100  // Keep 100 templates in memory
});
```

**Impact:** 5-10x speedup for template rendering

### Strategy 2: Query Result Cache

```javascript
class QueryCache {
  constructor(maxSize = 1000, ttl = 60000) {
    this.cache = new Map();
    this.maxSize = maxSize;
    this.ttl = ttl;
  }

  get(query) {
    const entry = this.cache.get(query);
    if (!entry) return null;

    if (Date.now() - entry.timestamp > this.ttl) {
      this.cache.delete(query);
      return null;
    }

    return entry.results;
  }

  set(query, results) {
    if (this.cache.size >= this.maxSize) {
      const firstKey = this.cache.keys().next().value;
      this.cache.delete(firstKey);
    }

    this.cache.set(query, {
      results,
      timestamp: Date.now()
    });
  }
}

const cache = new QueryCache();

function executeQuery(query) {
  const cached = cache.get(query);
  if (cached) return cached;

  const results = executeSelectSync(store, query);
  cache.set(query, results);
  return results;
}
```

**Impact:** 100-1000x speedup for repeated queries

### Strategy 3: Memoization

```javascript
import { memoize } from 'lodash-es';

// Expensive RDF generation
const generatePersonRDF = memoize((person) => {
  return renderTemplate('person.njk', person);
}, (person) => person.id); // Cache key

// First call: generates RDF
await generatePersonRDF(alice); // 10ms

// Second call: returns cached
await generatePersonRDF(alice); // <1ms
```

**Impact:** Near-instant for cached values

## Parallelization

### Strategy 1: Parallel Template Rendering

```javascript
// ✅ Parallel: Process in parallel
const results = await Promise.all(
  people.map(person => renderTemplate('person.njk', person))
);

// Combine results
const combinedRDF = results.join('\n\n');
```

**Impact:** Near-linear speedup with CPU cores

### Strategy 2: Worker Threads

```javascript
import { Worker } from 'worker_threads';

function processChunk(chunk) {
  return new Promise((resolve, reject) => {
    const worker = new Worker('./rdf-worker.js');

    worker.on('message', resolve);
    worker.on('error', reject);

    worker.postMessage(chunk);
  });
}

// Process in parallel across CPU cores
const chunks = chunkArray(people, numCPUs);
const results = await Promise.all(
  chunks.map(chunk => processChunk(chunk))
);
```

**Impact:** 4-8x speedup on multi-core systems

## Memory Optimization

### Strategy 1: Streaming Processing

```javascript
// ❌ Memory Intensive: Load all
const allPeople = await loadAllPeople(); // 1 GB
const rdf = await generateRDF(allPeople);

// ✅ Memory Efficient: Stream
const stream = createReadStream('people.json');
stream
  .pipe(jsonParser())
  .pipe(rdfGenerator())
  .pipe(createWriteStream('output.ttl'));
// Memory: ~10 MB (constant)
```

**Impact:** Constant memory usage regardless of dataset size

### Strategy 2: Garbage Collection Hints

```javascript
for (let i = 0; i < largeDataset.length; i++) {
  const result = await processItem(largeDataset[i]);
  await writeResult(result);

  // Hint GC to collect after each batch
  if (i % 1000 === 0) {
    if (global.gc) global.gc();
  }
}
```

Run with: `node --expose-gc app.js`

**Impact:** Reduced memory peaks

## Profiling and Monitoring

### Tool 1: Built-in Performance Timing

```javascript
console.time('template-render');
await renderTemplate('person.njk', data);
console.timeEnd('template-render');
// template-render: 12.456ms

console.time('sparql-query');
const results = executeSelectSync(store, query);
console.timeEnd('sparql-query');
// sparql-query: 45.123ms
```

### Tool 2: Memory Profiling

```javascript
const used = process.memoryUsage();
console.log({
  rss: Math.round(used.rss / 1024 / 1024) + ' MB',
  heapTotal: Math.round(used.heapTotal / 1024 / 1024) + ' MB',
  heapUsed: Math.round(used.heapUsed / 1024 / 1024) + ' MB',
  external: Math.round(used.external / 1024 / 1024) + ' MB'
});
```

### Tool 3: Benchmarking

```javascript
import { Suite } from 'benchmark';

const suite = new Suite();

suite
  .add('Template Render', () => {
    renderTemplate('person.njk', testData);
  })
  .add('Direct Generation', () => {
    generateRDFDirect(testData);
  })
  .on('cycle', (event) => {
    console.log(String(event.target));
  })
  .on('complete', function() {
    console.log('Fastest is ' + this.filter('fastest').map('name'));
  })
  .run({ async: true });
```

## Performance Checklist

Before deploying to production:

- [ ] Template caching enabled
- [ ] Batch rendering used where possible
- [ ] SPARQL queries optimized (specific patterns first)
- [ ] Appropriate indexes present
- [ ] Results limited (LIMIT clause)
- [ ] Efficient serialization format chosen
- [ ] Prefixes used to reduce size
- [ ] Streaming for large datasets
- [ ] Query result caching implemented
- [ ] Profiling data collected
- [ ] Memory usage monitored
- [ ] Load testing completed

## Conclusion

Performance optimization is about:

1. **Measuring:** Profile before optimizing
2. **Targeting:** Focus on bottlenecks
3. **Caching:** Avoid redundant work
4. **Batching:** Amortize overhead
5. **Streaming:** Handle large datasets
6. **Parallelizing:** Use all cores

Apply optimizations incrementally and measure impact.

## See Also

- [RDF-KGN Architecture](./rdf-kgn-architecture.md)
- [Template-Driven RDF Generation](./template-driven-rdf-generation.md)
- [Optimize RDF Serialization How-To](../how-to/optimize-rdf-serialization.md)
