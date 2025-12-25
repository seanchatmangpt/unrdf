# Installation & Setup Guide

## Prerequisites

- Node.js >= 18.0.0
- pnpm (recommended) or npm
- 2GB+ RAM for transformer model

## Installation

### From Workspace

```bash
# Navigate to semantic-search package
cd packages/semantic-search

# Install dependencies
pnpm install

# Or from workspace root
pnpm add --filter @unrdf/semantic-search @xenova/transformers vectra
```

### Standalone Installation

```bash
# Install the package
npm install @unrdf/semantic-search @unrdf/oxigraph

# Dependencies will be installed automatically:
# - @xenova/transformers (transformer models)
# - vectra (vector database)
# - zod (validation)
```

## Quick Start

### 1. Create RDF Store

```javascript
import { createStore, dataFactory } from '@unrdf/oxigraph';

const { namedNode, literal, triple } = dataFactory;
const store = createStore();

// Add triples
store.add(triple(
  namedNode('http://example.org/JavaScript'),
  namedNode('http://schema.org/description'),
  literal('programming language for web development')
));
```

### 2. Initialize Semantic Search

```javascript
import { SemanticQueryEngine } from '@unrdf/semantic-search';

const engine = new SemanticQueryEngine(store);
await engine.initialize();  // Load transformer model
await engine.indexStore();   // Build vector index
```

### 3. Search

```javascript
const results = await engine.search('web development language', {
  limit: 5,
  threshold: 0.5,
});

results.forEach(result => {
  console.log(`${result.text} (score: ${result.score})`);
});
```

## Running Tests

```bash
# Run all tests
pnpm test

# Run tests with coverage
pnpm test:coverage

# Run benchmarks
pnpm bench

# Watch mode
pnpm test:watch
```

## Running Demos

```bash
# Basic semantic search demo
pnpm demo

# Output:
# 🚀 Semantic Search Demo - UNRDF
# Creating sample knowledge graph...
# Indexed 35 triples
# ...
```

## Troubleshooting

### Issue: Transformer model download fails

**Symptom**: Error downloading model from HuggingFace

**Solution**:
```bash
# Set cache directory
export HF_HOME=/path/to/cache

# Or disable cache
export TRANSFORMERS_CACHE=false
```

### Issue: Out of memory

**Symptom**: Node.js heap out of memory

**Solution**:
```bash
# Increase Node.js memory
export NODE_OPTIONS="--max-old-space-size=4096"

# Or reduce batch size in code
const embedder = new RDFEmbedder({ cache: false });
```

### Issue: Slow indexing

**Symptom**: Indexing takes >30s for 250 triples

**Solution**:
```bash
# Check system resources
top

# Reduce concurrent operations
# Use batch processing
const triples = store.match(null, null, null);
for (let i = 0; i < triples.length; i += 10) {
  await embedder.embedTriples(triples.slice(i, i + 10));
}
```

### Issue: Vitest timeout errors

**Symptom**: Tests timeout after 5s

**Solution**:
Already configured in `vitest.config.mjs` with 60s timeout.
If still failing:
```javascript
// In test file
it('should work', async () => {
  // ...
}, 120000); // 120s timeout
```

## Configuration Options

### RDFEmbedder Options

```javascript
const embedder = new RDFEmbedder({
  model: 'Xenova/all-MiniLM-L6-v2',  // HuggingFace model
  pooling: 'mean',                    // 'mean' or 'cls'
  normalize: true,                    // L2 normalization
  cache: true,                        // Enable caching
});
```

### SemanticQueryEngine Options

```javascript
const engine = new SemanticQueryEngine(store, {
  embedder: {
    model: 'Xenova/all-MiniLM-L6-v2',
    cache: true,
  },
});
```

### Search Options

```javascript
await engine.search(query, {
  limit: 10,         // Max results
  threshold: 0.5,    // Minimum similarity (0-1)
});

await engine.hybridSearch(nlQuery, sparqlPattern, {
  limit: 10,
  threshold: 0.5,
  hybridWeight: 0.7, // Semantic weight (0-1)
});
```

### Recommendation Options

```javascript
await recommender.findSimilarEntities(entityUri, {
  limit: 10,
  threshold: 0.6,
  diversityWeight: 0.3,  // Diversity importance (0-1)
});
```

## Performance Tuning

### 1. Enable Caching

```javascript
const embedder = new RDFEmbedder({ cache: true });
// Cache hit rate: >90% for repeated queries
```

### 2. Batch Processing

```javascript
// Bad: Sequential
for (const triple of triples) {
  await embedder.embedTriple(triple);
}

// Good: Batch
await embedder.embedTriples(triples);
```

### 3. Pre-warm Cache

```javascript
// Index common queries upfront
const commonQueries = [
  'programming language',
  'web development',
  'data science',
];

for (const query of commonQueries) {
  await engine.search(query);
}
```

### 4. Adjust Threshold

```javascript
// Higher threshold = fewer, more relevant results
await engine.search(query, { threshold: 0.7 });

// Lower threshold = more results, lower quality
await engine.search(query, { threshold: 0.3 });
```

## Development Workflow

### 1. Setup

```bash
git clone <repo>
cd packages/semantic-search
pnpm install
```

### 2. Make Changes

```bash
# Edit source files
vim src/embeddings/rdf-embedder.mjs

# Run tests
pnpm test

# Run specific test
pnpm test test/embeddings.test.mjs
```

### 3. Verify

```bash
# Lint
pnpm lint

# Format
pnpm format

# Run demo
pnpm demo
```

### 4. Benchmark

```bash
# Run performance tests
pnpm bench

# Check output for regressions
```

## Production Deployment

### 1. Build

```bash
# No build step needed (pure ESM)
# Just ensure dependencies are installed
pnpm install --prod
```

### 2. Environment Variables

```bash
# Optional: Set cache directory
export HF_HOME=/var/cache/huggingface

# Optional: Increase memory
export NODE_OPTIONS="--max-old-space-size=4096"
```

### 3. Warmup

```javascript
// In production startup script
const engine = new SemanticQueryEngine(store);
await engine.initialize();
await engine.indexStore();

// Pre-cache common queries
for (const query of commonQueries) {
  await engine.search(query);
}

console.log('Semantic search ready');
```

### 4. Monitoring

```javascript
// Check cache effectiveness
const stats = engine.getStats();
console.log('Cache hit rate:', stats.cacheStats.size);

// Monitor query latency
const start = performance.now();
await engine.search(query);
console.log('Search time:', performance.now() - start, 'ms');
```

## Next Steps

- Read [ARCHITECTURE.md](./ARCHITECTURE.md) for system design
- See [README.md](./README.md) for API reference
- Run `pnpm demo` to see examples
- Check [test/benchmark.test.mjs](./test/benchmark.test.mjs) for performance targets
