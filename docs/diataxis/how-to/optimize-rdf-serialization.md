# How to Optimize RDF Serialization

**Goal:** Improve RDF serialization performance and reduce output size
**Time:** 10-15 minutes
**Difficulty:** Advanced

## Problem

Large RDF datasets are slow to serialize and consume significant disk space or network bandwidth.

## Solution

Use optimized serialization formats, compression, streaming, and efficient prefix handling.

## Prerequisites

- `@unrdf/core` package installed
- Large RDF dataset to optimize
- Understanding of RDF formats

## Quick Wins

### 1. Choose Efficient Format

```javascript
// ❌ Slow: Turtle with verbose URIs
<http://example.org/person/12345>
  <http://www.w3.org/1999/02/22-rdf-syntax-ns#type>
  <http://xmlns.com/foaf/0.1/Person> .

// ✅ Fast: Turtle with prefixes
@prefix ex: <http://example.org/person/> .
@prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .
@prefix foaf: <http://xmlns.com/foaf/0.1/> .

ex:12345 a foaf:Person .
```

**Size reduction:** 60-70%

### 2. Use Compact Serialization

```javascript
// ❌ Slow: One triple per line
ex:alice rdf:type foaf:Person .
ex:alice foaf:name "Alice" .
ex:alice foaf:age "30"^^xsd:integer .

// ✅ Fast: Grouped triples
ex:alice
  a foaf:Person ;
  foaf:name "Alice" ;
  foaf:age "30"^^xsd:integer .
```

**Size reduction:** 30-40%

## Method 1: Optimize Prefix Handling

### Collect Common Prefixes

```javascript
import { PREFIXES } from '@unrdf/kgn/src/doc-generator/rdf-builder.mjs';

// Reuse standard prefixes
const commonPrefixes = {
  ...PREFIXES,
  foaf: 'http://xmlns.com/foaf/0.1/',
  dc: 'http://purl.org/dc/elements/1.1/',
  dcterms: 'http://purl.org/dc/terms/',
  skos: 'http://www.w3.org/2004/02/skos/core#',
  schema: 'https://schema.org/'
};

// Generate prefix declarations
function generatePrefixes(prefixes) {
  return Object.entries(prefixes)
    .map(([prefix, uri]) => `@prefix ${prefix}: <${uri}> .`)
    .join('\n');
}

const prefixBlock = generatePrefixes(commonPrefixes);
```

### Auto-Detect Required Prefixes

```javascript
/**
 * Extract prefixes used in RDF data
 */
function detectPrefixes(rdfData, knownPrefixes) {
  const usedPrefixes = {};

  for (const [prefix, namespace] of Object.entries(knownPrefixes)) {
    if (rdfData.includes(namespace) || rdfData.includes(`${prefix}:`)) {
      usedPrefixes[prefix] = namespace;
    }
  }

  return usedPrefixes;
}

// Only include used prefixes
const detectedPrefixes = detectPrefixes(rdfContent, commonPrefixes);
const minimalPrefixes = generatePrefixes(detectedPrefixes);

console.log(`Reduced from ${Object.keys(commonPrefixes).length} to ${Object.keys(detectedPrefixes).length} prefixes`);
```

## Method 2: Streaming Serialization

For large datasets, use streaming to avoid loading everything in memory.

```javascript
import { Writable } from 'stream';
import { Writer } from 'n3';

/**
 * Streaming RDF writer
 */
class StreamingRDFWriter {
  constructor(outputStream, options = {}) {
    this.writer = new Writer(outputStream, {
      format: options.format || 'text/turtle',
      prefixes: options.prefixes || {}
    });
    this.triplesWritten = 0;
  }

  /**
   * Write single triple
   */
  writeTriple(subject, predicate, object) {
    this.writer.addQuad(subject, predicate, object);
    this.triplesWritten++;
  }

  /**
   * Write batch of triples
   */
  writeBatch(triples) {
    for (const triple of triples) {
      this.writer.addQuad(
        triple.subject,
        triple.predicate,
        triple.object
      );
    }
    this.triplesWritten += triples.length;
  }

  /**
   * Finalize writing
   */
  end(callback) {
    this.writer.end((error, result) => {
      if (error) {
        callback(error);
      } else {
        callback(null, {
          triplesWritten: this.triplesWritten,
          output: result
        });
      }
    });
  }
}

// Usage
import { createWriteStream } from 'fs';

const outputStream = createWriteStream('output.ttl');
const writer = new StreamingRDFWriter(outputStream, {
  format: 'text/turtle',
  prefixes: commonPrefixes
});

// Write triples in batches
const batch1 = [
  { subject: 'ex:alice', predicate: 'a', object: 'foaf:Person' },
  { subject: 'ex:alice', predicate: 'foaf:name', object: '"Alice"' }
];

writer.writeBatch(batch1);

writer.end((error, result) => {
  if (error) {
    console.error('Write failed:', error);
  } else {
    console.log(`✅ Wrote ${result.triplesWritten} triples`);
  }
});
```

## Method 3: Batch Processing

Process large datasets in chunks to control memory usage.

```javascript
/**
 * Batch RDF generator
 */
class BatchRDFGenerator {
  constructor(batchSize = 1000) {
    this.batchSize = batchSize;
    this.currentBatch = [];
  }

  /**
   * Add item to batch
   */
  add(item) {
    this.currentBatch.push(item);

    if (this.currentBatch.length >= this.batchSize) {
      return this.flush();
    }

    return null;
  }

  /**
   * Generate RDF for current batch
   */
  flush() {
    if (this.currentBatch.length === 0) {
      return null;
    }

    const rdf = this.generateBatchRDF(this.currentBatch);
    this.currentBatch = [];
    return rdf;
  }

  /**
   * Generate RDF for batch of items
   */
  generateBatchRDF(items) {
    const triples = items.map(item => {
      return `ex:${item.id}
  a foaf:Person ;
  foaf:name "${item.name}" ;
  foaf:email "${item.email}" .`;
    });

    return triples.join('\n\n');
  }

  /**
   * Process large dataset
   */
  async processDataset(items, outputPath) {
    const stream = createWriteStream(outputPath);

    // Write prefixes first
    stream.write(generatePrefixes(commonPrefixes));
    stream.write('\n\n');

    let totalProcessed = 0;

    for (const item of items) {
      const batchRDF = this.add(item);

      if (batchRDF) {
        stream.write(batchRDF);
        stream.write('\n\n');
        totalProcessed += this.batchSize;
        console.log(`Processed ${totalProcessed} items...`);
      }
    }

    // Flush remaining
    const remaining = this.flush();
    if (remaining) {
      stream.write(remaining);
      totalProcessed += this.currentBatch.length;
    }

    stream.end();
    console.log(`✅ Completed: ${totalProcessed} items`);
  }
}

// Usage
const generator = new BatchRDFGenerator(1000);

const largeDataset = [/* ... 1 million items ... */];
await generator.processDataset(largeDataset, 'output.ttl');
```

## Method 4: Compression Strategies

### Strategy 1: Blank Node Optimization

```javascript
// ❌ Verbose: Multiple blank nodes
ex:person1 foaf:address _:addr1 .
_:addr1 vcard:street "123 Main St" .
_:addr1 vcard:city "Springfield" .

ex:person2 foaf:address _:addr2 .
_:addr2 vcard:street "456 Oak Ave" .
_:addr2 vcard:city "Springfield" .

// ✅ Compact: Inline blank nodes
ex:person1
  foaf:address [
    vcard:street "123 Main St" ;
    vcard:city "Springfield"
  ] .

ex:person2
  foaf:address [
    vcard:street "456 Oak Ave" ;
    vcard:city "Springfield"
  ] .
```

### Strategy 2: Shared Value Optimization

```javascript
// ❌ Redundant: Repeated values
ex:person1 vcard:city "Springfield"^^xsd:string .
ex:person2 vcard:city "Springfield"^^xsd:string .
ex:person3 vcard:city "Springfield"^^xsd:string .

// ✅ Optimized: Reuse constants
ex:Springfield a vcard:City ;
  rdfs:label "Springfield" .

ex:person1 vcard:city ex:Springfield .
ex:person2 vcard:city ex:Springfield .
ex:person3 vcard:city ex:Springfield .
```

### Strategy 3: Datatype Inference

```javascript
// ❌ Explicit datatypes
ex:person1 foaf:age "30"^^xsd:integer .
ex:person2 foaf:age "25"^^xsd:integer .

// ✅ Inferred (if context allows)
ex:person1 foaf:age 30 .
ex:person2 foaf:age 25 .
```

## Method 5: Format Selection

### Format Comparison

| Format | Size | Parse Speed | Human Readable |
|--------|------|-------------|----------------|
| Turtle | Medium | Medium | ✅ High |
| N-Triples | Large | Fast | ⚠️ Medium |
| JSON-LD | Large | Slow | ✅ High |
| RDF/XML | Large | Slow | ❌ Low |
| Binary (HDT) | **Small** | **Fast** | ❌ None |

### Choose Format by Use Case

```javascript
function selectFormat(useCase) {
  const formats = {
    // Development: readability matters
    development: 'text/turtle',

    // Production API: compact and fast
    production: 'application/n-triples',

    // Web/JSON ecosystem: compatibility
    web: 'application/ld+json',

    // Large datasets: maximum compression
    bigdata: 'application/hdt',

    // Interop with XML systems
    legacy: 'application/rdf+xml'
  };

  return formats[useCase] || 'text/turtle';
}
```

## Performance Benchmarks

### Before Optimization

```javascript
// Naive approach
function generateRDFNaive(items) {
  let rdf = '';

  // Full URIs (no prefixes)
  for (const item of items) {
    rdf += `<http://example.org/${item.id}> `;
    rdf += `<http://www.w3.org/1999/02/22-rdf-syntax-ns#type> `;
    rdf += `<http://xmlns.com/foaf/0.1/Person> .\n`;

    rdf += `<http://example.org/${item.id}> `;
    rdf += `<http://xmlns.com/foaf/0.1/name> `;
    rdf += `"${item.name}" .\n`;
  }

  return rdf;
}

// Benchmark: 10,000 items
// Time: ~2.5 seconds
// Size: 5.2 MB
```

### After Optimization

```javascript
function generateRDFOptimized(items) {
  const prefixes = generatePrefixes({
    ex: 'http://example.org/',
    rdf: 'http://www.w3.org/1999/02/22-rdf-syntax-ns#',
    foaf: 'http://xmlns.com/foaf/0.1/'
  });

  const triples = items.map(item => `ex:${item.id}
  a foaf:Person ;
  foaf:name "${item.name}" .`);

  return `${prefixes}\n\n${triples.join('\n\n')}`;
}

// Benchmark: 10,000 items
// Time: ~0.8 seconds (3x faster)
// Size: 1.8 MB (65% smaller)
```

## Best Practices

1. **Use prefixes everywhere:** Reduces size by 60-70%
2. **Group related triples:** Improves readability and compression
3. **Stream large datasets:** Avoid loading everything in memory
4. **Batch processing:** Process in chunks (1000-10000 items)
5. **Choose right format:** Turtle for dev, N-Triples for production
6. **Compress output:** gzip can reduce size by additional 70-80%
7. **Reuse blank nodes:** Inline when possible

## Advanced Optimizations

### Optimization 1: Parallel Processing

```javascript
import { Worker } from 'worker_threads';

async function parallelRDFGeneration(items, numWorkers = 4) {
  const chunkSize = Math.ceil(items.length / numWorkers);
  const chunks = [];

  for (let i = 0; i < items.length; i += chunkSize) {
    chunks.push(items.slice(i, i + chunkSize));
  }

  const promises = chunks.map((chunk, i) => {
    return new Promise((resolve, reject) => {
      const worker = new Worker('./rdf-worker.js');

      worker.on('message', result => resolve(result));
      worker.on('error', reject);

      worker.postMessage({ chunk, workerId: i });
    });
  });

  const results = await Promise.all(promises);
  return results.join('\n\n');
}
```

### Optimization 2: Memoization

```javascript
const memoized = new Map();

function generateWithMemo(item) {
  const key = `${item.type}-${item.id}`;

  if (memoized.has(key)) {
    return memoized.get(key);
  }

  const rdf = generateRDFForItem(item);
  memoized.set(key, rdf);

  return rdf;
}
```

### Optimization 3: Template Caching

```javascript
import { TemplateEngine } from '@unrdf/kgn';

const engine = new TemplateEngine({
  deterministicMode: true,
  cache: true // Enable template caching
});

// First render: compiles template
await engine.render('person.njk', data1);

// Subsequent renders: uses cached compiled template
await engine.render('person.njk', data2); // Faster!
await engine.render('person.njk', data3); // Faster!
```

## Troubleshooting

### Problem: Out of memory errors

**Solution:** Use streaming instead of loading all data:

```javascript
// ❌ Loads everything
const allData = loadAllItems();
const rdf = generateRDF(allData);

// ✅ Streams data
const stream = createReadStream('data.json');
stream.pipe(rdfGenerator).pipe(outputStream);
```

### Problem: Slow serialization

**Solution:** Profile and optimize hot paths:

```javascript
console.time('prefix-generation');
const prefixes = generatePrefixes(commonPrefixes);
console.timeEnd('prefix-generation');

console.time('triple-generation');
const triples = generateTriples(items);
console.timeEnd('triple-generation');
```

### Problem: Large output files

**Solution:** Enable compression:

```javascript
import { createGzip } from 'zlib';
import { pipeline } from 'stream';

pipeline(
  rdfGeneratorStream,
  createGzip(),
  createWriteStream('output.ttl.gz'),
  (error) => {
    if (error) console.error('Compression failed:', error);
    else console.log('✅ Compressed successfully');
  }
);
```

## Related Guides

- [Generate RDF from Templates](./generate-rdf-from-templates.md)
- [Build SPARQL Queries](./build-sparql-queries.md)
- [Visualize RDF Graphs](./visualize-rdf-graphs.md)

## Reference

- [RDF-KGN API](../reference/rdf-kgn-api.md)
- [Performance Optimization Strategies](../explanation/performance-optimization-strategies.md)
