# Semantic Search Architecture

## Overview

AI-powered semantic search system for RDF knowledge graphs using transformer-based vector embeddings.

## System Architecture

```
┌─────────────────────────────────────────────────────────────┐
│                  Semantic Search System                      │
├─────────────────────────────────────────────────────────────┤
│                                                               │
│  ┌──────────────┐    ┌──────────────┐    ┌──────────────┐  │
│  │ RDFEmbedder  │───▶│   Vectra     │◀───│  Oxigraph    │  │
│  │              │    │ Vector Store │    │  RDF Store   │  │
│  └──────────────┘    └──────────────┘    └──────────────┘  │
│         │                    │                    │          │
│         ▼                    ▼                    ▼          │
│  ┌──────────────┐    ┌──────────────┐    ┌──────────────┐  │
│  │  Transformer │    │    Search    │    │   SPARQL     │  │
│  │    Model     │    │    Index     │    │   Queries    │  │
│  │ (MiniLM-L6)  │    │              │    │              │  │
│  └──────────────┘    └──────────────┘    └──────────────┘  │
│         │                    │                    │          │
│         └────────────────────┴────────────────────┘          │
│                              │                                │
│  ┌───────────────────────────▼──────────────────────────┐   │
│  │         Semantic Query Engine & Recommender          │   │
│  │  - Natural Language Queries                          │   │
│  │  - Hybrid Search (Semantic + SPARQL)                 │   │
│  │  - Entity Similarity                                 │   │
│  │  - Concept Recommendations                           │   │
│  │  - Autocomplete                                      │   │
│  └──────────────────────────────────────────────────────┘   │
│                                                               │
└─────────────────────────────────────────────────────────────┘
```

## Component Details

### 1. RDFEmbedder (`src/embeddings/rdf-embedder.mjs`)

**Purpose**: Convert RDF triples to vector embeddings

**Architecture**:
- Uses `@xenova/transformers` for transformer model inference
- Model: `Xenova/all-MiniLM-L6-v2` (384-dimensional embeddings)
- Intelligent caching for repeated embeddings
- Batch processing support

**Key Methods**:
```javascript
tripleToText(triple)          // Convert triple to readable text
embedText(text)               // Generate embedding for text
embedTriple(triple)           // Generate embedding for triple
embedTriples(triples)         // Batch embedding
embedEntity(triples)          // Aggregate entity embedding
```

**Performance**:
- Single embedding: ~50-100ms (first call)
- Cached embedding: <1ms
- Batch processing: ~30-50ms per triple

**Caching Strategy**:
- In-memory Map-based cache
- Key: text representation of triple
- Value: 384-dim float array
- Cache hit rate: >90% for repeated queries

### 2. SemanticQueryEngine (`src/search/semantic-query-engine.mjs`)

**Purpose**: Natural language queries over RDF using vector similarity

**Architecture**:
- Vector index: Vectra (HNSW-based)
- Hybrid search: Semantic + SPARQL filtering
- Streaming results with thresholding

**Key Methods**:
```javascript
indexStore()                  // Build vector index from RDF store
search(query, options)        // Semantic search
hybridSearch(nl, sparql)      // Combined semantic + SPARQL
findSimilar(triple, limit)    // Similar triple discovery
autocomplete(partial, limit)  // Query suggestions
```

**Search Algorithm**:
1. Query → Embedding (via RDFEmbedder)
2. Vector similarity search (cosine similarity)
3. Filter by threshold
4. Optional SPARQL filtering
5. Rank by combined score

**Hybrid Scoring**:
```
hybridScore = semanticScore * weight + sparqlMatch * (1 - weight)
Default weight: 0.7
```

### 3. KnowledgeRecommender (`src/discovery/knowledge-recommender.mjs`)

**Purpose**: Discover related concepts and recommend entities

**Architecture**:
- Entity-level embeddings (aggregated from triples)
- Cosine similarity for entity comparison
- Diversity-aware result ranking

**Key Methods**:
```javascript
findSimilarEntities(uri)      // Find similar entities
recommendConcepts(query)      // Recommend concepts for query
```

**Entity Embedding**:
1. Get all triples about entity (subject or object)
2. Embed each triple independently
3. Average pooling over all embeddings
4. L2 normalization

**Diversity Algorithm**:
- Maximal Marginal Relevance (MMR)
- Balance: relevance vs. diversity
- Avoids redundant recommendations

### 4. Vector Store (Vectra)

**Why Vectra**:
- Pure JavaScript (no native dependencies)
- In-memory index for speed
- HNSW algorithm for fast similarity search
- Metadata storage for triple info

**Index Structure**:
```javascript
{
  id: "triple-{index}",
  vector: [384-dim embedding],
  metadata: {
    subject: "http://...",
    predicate: "http://...",
    object: "literal or URI"
  }
}
```

## Data Flow

### Indexing Flow
```
RDF Triples → RDFEmbedder → Vector Embeddings → Vectra Index
                                                       ↓
                                              Metadata Store
```

### Search Flow
```
NL Query → RDFEmbedder → Query Embedding
                                ↓
                         Vectra Search ← Vector Index
                                ↓
                      Similarity Scores
                                ↓
                    Filter by Threshold
                                ↓
              Optional SPARQL Filtering (Hybrid)
                                ↓
                          Ranked Results
```

### Recommendation Flow
```
Entity URI → Get Entity Triples → Entity Embedding
                                         ↓
                              Compare with all entities
                                         ↓
                              Cosine Similarity
                                         ↓
                              Diversity Filtering
                                         ↓
                              Ranked Recommendations
```

## Performance Characteristics

### Time Complexity

| Operation | Time Complexity | Actual Time |
|-----------|----------------|-------------|
| Embed single triple | O(1) cached, O(n) uncached | 1ms / 80ms |
| Index N triples | O(N * log N) | ~15-25s for 250 |
| Search | O(log N) | <500ms |
| Hybrid search | O(log N + S) | <1000ms |
| Find similar | O(E) | <1500ms |

Where:
- N = number of triples
- E = number of entities
- S = SPARQL filter cost

### Space Complexity

| Component | Space | Notes |
|-----------|-------|-------|
| Embedding cache | O(T * D) | T=unique texts, D=384 |
| Vector index | O(N * D) | N=triples, D=384 |
| Entity cache | O(E * D) | E=entities, D=384 |
| RDF store | O(N) | Native Oxigraph |

**Memory Footprint** (250 triples):
- Embedding cache: ~1-2 MB
- Vector index: ~400 KB
- Entity cache: ~200 KB
- Total: ~2-3 MB

### Scalability

**Small Scale** (< 1K triples):
- Index time: <30s
- Search time: <100ms
- Memory: <10 MB

**Medium Scale** (1K - 10K triples):
- Index time: <5 min
- Search time: <500ms
- Memory: <50 MB

**Large Scale** (10K - 100K triples):
- Index time: <30 min
- Search time: <1s
- Memory: <500 MB

**Scaling Strategies**:
1. Incremental indexing (add new triples without rebuild)
2. Distributed indexing (shard by entity)
3. Approximate search (lower HNSW precision)
4. Cache warm-up (pre-compute common embeddings)

## Model Details

### Transformer Model

**Model**: `Xenova/all-MiniLM-L6-v2`
- **Type**: Sentence Transformer
- **Base**: MiniLM (distilled BERT)
- **Layers**: 6
- **Hidden size**: 384
- **Parameters**: 22.7M
- **Training**: Sentence similarity task
- **Performance**: Good balance of speed and quality

**Why MiniLM-L6-v2**:
1. Fast inference (~50-100ms)
2. Small model size (~90 MB)
3. Runs in browser + Node.js
4. Good semantic understanding
5. Widely used and validated

**Alternatives**:
- `Xenova/all-MiniLM-L12-v2` (better quality, slower)
- `Xenova/gte-small` (multilingual)
- `Xenova/bge-small-en-v1.5` (better retrieval)

## Design Decisions

### Why Not Use GraphRAG?
- RDF has structure - exploit it
- SPARQL provides symbolic reasoning
- Hybrid > pure vector search

### Why Vectra Over Others?
- Pure JS (no Python/native deps)
- In-memory = fast
- Simple API
- Good for <100K vectors

### Why Not Fine-tune Model?
- General model works well for RDF
- Pre-trained on diverse text
- No training data needed
- Faster time-to-production

### Why Average Pooling for Entities?
- Simple and effective
- Captures all entity information
- More robust than single triple
- Computational efficient

## Extension Points

### 1. Custom Embedders
```javascript
class CustomEmbedder extends RDFEmbedder {
  async embedText(text) {
    // Use different model or API
    return await myCustomEmbedding(text);
  }
}
```

### 2. Custom Similarity Metrics
```javascript
class CustomRecommender extends KnowledgeRecommender {
  cosineSimilarity(a, b) {
    // Use different similarity metric
    return myCustomSimilarity(a, b);
  }
}
```

### 3. Custom Indexing
```javascript
class CustomQueryEngine extends SemanticQueryEngine {
  async indexStore() {
    // Use different vector store or indexing strategy
    return await myCustomIndexing(this.store);
  }
}
```

## Testing Strategy

### Unit Tests
- `test/embeddings.test.mjs`: RDFEmbedder tests
- `test/semantic-query.test.mjs`: Query engine tests
- `test/knowledge-recommender.test.mjs`: Recommender tests

### Benchmarks
- `test/benchmark.test.mjs`: Performance tests
- Indexing speed
- Search latency
- Memory usage
- Cache effectiveness

### Integration Tests
- End-to-end search workflows
- Hybrid search validation
- Multi-entity graphs

## Future Enhancements

1. **Incremental Indexing**: Add new triples without rebuild
2. **Persistent Index**: Save/load vector index
3. **Multi-modal Embeddings**: Images, diagrams in RDF
4. **Graph Neural Networks**: Learn from RDF structure
5. **Active Learning**: Improve based on user feedback
6. **Distributed Search**: Shard across multiple nodes
7. **Real-time Updates**: Watch RDF store for changes
8. **Query Rewriting**: Optimize NL queries

## References

- [Sentence Transformers](https://www.sbert.net/)
- [Vectra Documentation](https://github.com/Stevenic/vectra)
- [HNSW Algorithm](https://arxiv.org/abs/1603.09320)
- [Semantic Search Best Practices](https://www.pinecone.io/learn/semantic-search/)
