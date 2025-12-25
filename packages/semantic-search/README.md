# @unrdf/semantic-search

AI-powered semantic search over RDF knowledge graphs using vector embeddings and transformer models.

## Features

- **Vector Embeddings**: Convert RDF triples to semantic vectors using transformer models
- **Natural Language Queries**: Search knowledge graphs with plain English queries
- **Hybrid Search**: Combine semantic search with SPARQL for precise filtering
- **Knowledge Discovery**: Find similar entities and recommend related concepts
- **Fast & Efficient**: Sub-second query times with intelligent caching

## Installation

```bash
pnpm add @unrdf/semantic-search
```

## Quick Start

```javascript
import { createStore, dataFactory } from '@unrdf/oxigraph';
import { SemanticQueryEngine } from '@unrdf/semantic-search';

const { namedNode, literal, triple } = dataFactory;

// Create and populate RDF store
const store = createStore();
store.add(triple(
  namedNode('http://example.org/JavaScript'),
  namedNode('http://schema.org/description'),
  literal('programming language for web development')
));

// Initialize semantic search
const engine = new SemanticQueryEngine(store);
await engine.initialize();
await engine.indexStore();

// Search with natural language
const results = await engine.search('web development language', {
  limit: 5,
  threshold: 0.5,
});

results.forEach(result => {
  console.log(`${result.text} (score: ${result.score})`);
});
```

## Architecture

### Components

1. **RDFEmbedder** - Converts RDF triples to vector embeddings
   - Uses `Xenova/all-MiniLM-L6-v2` transformer model (384 dimensions)
   - Intelligent caching for performance
   - Batch processing support

2. **SemanticQueryEngine** - Natural language queries over RDF
   - Vector similarity search
   - Hybrid semantic + SPARQL queries
   - Autocomplete suggestions
   - Similar triple discovery

3. **KnowledgeRecommender** - Discover related concepts
   - Entity similarity analysis
   - Concept recommendations
   - Diversity-aware results

### Model Details

- **Model**: `Xenova/all-MiniLM-L6-v2` (HuggingFace)
- **Type**: Sentence Transformer
- **Embedding Dimension**: 384
- **Inference**: Browser + Node.js via @xenova/transformers
- **Performance**: ~50-100ms per embedding (cached: <1ms)

## Examples

### Semantic Search

```javascript
const results = await engine.search('machine learning framework', {
  limit: 10,
  threshold: 0.6,
});
```

### Hybrid Search (Semantic + SPARQL)

```javascript
const results = await engine.hybridSearch(
  'web development',
  '?s <http://example.org/language> "JavaScript"',
  { limit: 5, hybridWeight: 0.7 }
);
```

### Find Similar Entities

```javascript
const similar = await recommender.findSimilarEntities(
  'http://example.org/Python',
  { limit: 5, threshold: 0.7 }
);
```

### Recommend Concepts

```javascript
const recommendations = await recommender.recommendConcepts(
  'data science tools',
  { limit: 5, threshold: 0.6 }
);
```

## Performance Benchmarks

Based on 250 triples (50 entities):

| Operation | Time | Rate |
|-----------|------|------|
| Single triple embedding | ~50-100ms | - |
| Batch 10 triples | ~300-500ms | - |
| Index 250 triples | ~15-25s | ~12-15 triples/sec |
| Semantic search | <500ms | - |
| Hybrid search | <1000ms | - |
| Entity similarity | <1500ms | - |

Cache speedup: **10-100x** for repeated queries

## API Reference

### RDFEmbedder

```javascript
const embedder = new RDFEmbedder({
  model: 'Xenova/all-MiniLM-L6-v2',
  pooling: 'mean',
  normalize: true,
  cache: true,
});

await embedder.initialize();
const embedding = await embedder.embedTriple(triple);
const embeddings = await embedder.embedTriples(triples);
const entityEmbedding = await embedder.embedEntity(triples);
```

### SemanticQueryEngine

```javascript
const engine = new SemanticQueryEngine(store, options);

await engine.initialize();
await engine.indexStore();

const results = await engine.search(query, { limit, threshold });
const hybrid = await engine.hybridSearch(nlQuery, sparqlPattern, options);
const similar = await engine.findSimilar(triple, limit);
const suggestions = await engine.autocomplete(partialQuery, limit);
```

### KnowledgeRecommender

```javascript
const recommender = new KnowledgeRecommender(store, options);

await recommender.initialize();

const similar = await recommender.findSimilarEntities(entityUri, options);
const concepts = await recommender.recommendConcepts(query, options);
```

## Development

```bash
# Install dependencies
pnpm install

# Run tests
pnpm test

# Run benchmarks
pnpm bench

# Run demo
pnpm demo

# Lint & format
pnpm lint
pnpm format
```

## License

MIT

## Links

- [Repository](https://github.com/unrdf/unrdf)
- [Issues](https://github.com/unrdf/unrdf/issues)
- [Documentation](https://github.com/unrdf/unrdf#readme)
