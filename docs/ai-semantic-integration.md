# AI Semantic Analysis Module - Integration Guide

## Overview

The AI Semantic Analysis module for UNRDF 2028 Phase 1 provides advanced semantic analysis capabilities for RDF knowledge graphs:

- **Semantic Analyzer**: Analyzes RDF graphs to understand semantic relationships, extract concepts, and suggest ontology improvements
- **NLP Query Builder**: Translates natural language queries to SPARQL
- **Embeddings Manager**: Generates and manages graph embeddings using TransE, ComplEx, and RotatE algorithms
- **Anomaly Detector**: Detects unexpected patterns, missing links, and data quality issues

All modules integrate seamlessly with UNRDF's Knowledge Hook system and provide comprehensive OTEL observability.

## Installation

No additional dependencies required beyond UNRDF's existing packages. All AI/ML algorithms are implemented in lightweight JavaScript.

```bash
# The module is included in UNRDF 3.1.1+
pnpm install
```

## Quick Start

### 1. Semantic Analysis

Analyze an RDF graph to extract concepts, relationships, and patterns:

```javascript
import { Store, DataFactory } from 'n3';
import { createSemanticAnalyzer } from './src/knowledge-engine/ai-semantic/index.mjs';

const { namedNode, literal, quad } = DataFactory;

// Create store with data
const store = new Store();
store.addQuad(quad(
  namedNode('http://example.org/alice'),
  namedNode('http://example.org/knows'),
  namedNode('http://example.org/bob')
));

// Create analyzer
const analyzer = createSemanticAnalyzer({
  cacheSize: 1000,
  maxConcepts: 100
});

// Analyze
const result = await analyzer.analyze(store);

console.log('Concepts:', result.concepts);
console.log('Relationships:', result.relationships);
console.log('Patterns:', result.patterns);
console.log('Suggestions:', result.suggestions);
console.log('Statistics:', result.statistics);
```

**Performance**: <500ms for moderate graphs (<1000 triples)

### 2. Natural Language Query Building

Translate natural language to SPARQL queries:

```javascript
import { createNLPQueryBuilder } from './src/knowledge-engine/ai-semantic/index.mjs';

const builder = createNLPQueryBuilder({
  enableCache: true,
  enableLLM: false // Set true with API key for LLM-powered translation
});

// Build query from natural language
const result = await builder.buildQuery('list all people', store);

console.log('SPARQL:', result.sparql);
console.log('Intent:', result.intent);
console.log('Confidence:', result.confidence);
console.log('Entities:', result.entities);

// Execute the generated SPARQL
import { query } from './src/knowledge-engine/query.mjs';
const queryResult = await query(store, result.sparql);
console.log('Results:', queryResult);
```

**Performance**: <300ms for query translation

**Supported patterns**:
- "list all X" → SELECT query
- "who is X" → DESCRIBE query
- "how many X" → COUNT query
- "does X have Y" → ASK query
- "X related to Y" → relationship query

### 3. Graph Embeddings

Generate embeddings for entities and relations:

```javascript
import { createEmbeddingsManager } from './src/knowledge-engine/ai-semantic/index.mjs';

const manager = createEmbeddingsManager({
  embeddingDim: 128,
  algorithm: 'transe', // or 'complex', 'rotate'
  epochs: 100,
  learningRate: 0.01
});

// Generate embeddings
await manager.generateEmbeddings(store);

// Get embedding for entity
const aliceEmbedding = manager.getEmbedding('http://example.org/alice', 'entity');
console.log('Alice embedding:', aliceEmbedding);

// Compute similarity
const similarity = manager.computeSimilarity(
  'http://example.org/alice',
  'http://example.org/bob'
);
console.log('Similarity:', similarity);

// Batch operations
const embeddings = manager.batchGetEmbeddings([
  'http://example.org/alice',
  'http://example.org/bob',
  'http://example.org/charlie'
]);
```

**Algorithms**:
- **TransE**: Translational embeddings (h + r ≈ t)
- **ComplEx**: Complex-valued embeddings
- **RotatE**: Rotation-based embeddings

### 4. Anomaly Detection

Detect anomalies and data quality issues:

```javascript
import { createAnomalyDetector } from './src/knowledge-engine/ai-semantic/index.mjs';

const detector = createAnomalyDetector({
  enableStatistical: true,
  enableMLBased: true,
  outlierThreshold: 2.5,
  minConfidence: 0.6
});

// Detect anomalies
const result = await detector.detectAnomalies(store);

console.log('Total anomalies:', result.statistics.total);
console.log('By severity:', result.statistics.bySeverity);
console.log('By type:', result.statistics.byType);

// Process anomalies
for (const anomaly of result.anomalies) {
  console.log(`[${anomaly.severity}] ${anomaly.type}: ${anomaly.description}`);
  console.log('  Confidence:', anomaly.confidence);
  console.log('  Evidence:', anomaly.evidence);
}
```

**Anomaly types**:
- `missing_link`: Missing relationships or inverse properties
- `data_quality`: Missing labels, types, or incomplete data
- `outlier`: Statistical outliers in graph structure
- `structural_anomaly`: Disconnected components, self-loops
- `unexpected_pattern`: Unusual patterns detected
- `inconsistency`: Domain/range inconsistencies

## Integration with Knowledge Hooks

The AI semantic modules integrate with UNRDF's Knowledge Hook system:

```javascript
import { KnowledgeHookManager } from './src/knowledge-engine/knowledge-hook-manager.mjs';
import { defineHook } from './src/knowledge-engine/define-hook.mjs';
import { createSemanticAnalyzer } from './src/knowledge-engine/ai-semantic/index.mjs';

const manager = new KnowledgeHookManager();
const analyzer = createSemanticAnalyzer();

// Define hook that runs semantic analysis before transactions
const semanticHook = defineHook({
  meta: {
    name: 'semantic-analysis',
    description: 'Analyze semantic quality before committing'
  },
  when: {
    kind: 'sparql-ask',
    ref: {
      uri: 'file://hooks/always.ask.rq',
      sha256: 'abc123...',
      mediaType: 'application/sparql-query'
    }
  },
  async run({ context }) {
    // Analyze graph semantics
    const analysis = await analyzer.analyze(context.graph);

    // Check for critical issues
    const critical = analysis.suggestions.filter(s => s.priority === 'high');

    if (critical.length > 0) {
      console.warn('Critical semantic issues:', critical);
    }

    return {
      result: analysis,
      assertions: [] // Could add semantic metadata as RDF
    };
  }
});

manager.addKnowledgeHook(semanticHook);
```

## OTEL Observability

All modules provide comprehensive OpenTelemetry instrumentation:

```javascript
import { trace } from '@opentelemetry/api';

// Spans are automatically created for:
// - semantic.analyze
// - semantic.extract_concepts
// - semantic.analyze_relationships
// - nlp.build_query
// - nlp.extract_entities
// - embeddings.generate
// - embeddings.train
// - anomaly.detect
// - anomaly.statistical
// - anomaly.ml_based

// Access spans in your monitoring system (Jaeger, Grafana, etc.)
```

**Attributes tracked**:
- Operation duration
- Store size
- Cache hit/miss rates
- Confidence scores
- Entity counts
- Anomaly counts

## Performance Tuning

### Semantic Analyzer

```javascript
const analyzer = createSemanticAnalyzer({
  cacheSize: 1000,           // Increase for larger graphs
  maxConcepts: 100,          // Limit concept extraction
  minConceptFrequency: 2,    // Filter rare concepts
  similarityThreshold: 0.7   // Similarity threshold
});
```

### NLP Query Builder

```javascript
const builder = createNLPQueryBuilder({
  cacheSize: 500,            // Cache query translations
  enableLLM: false,          // Enable for better accuracy
  llmApiKey: 'sk-...',       // OpenAI API key (optional)
  timeout: 300,              // Translation timeout (ms)
  minConfidence: 0.6         // Filter low-confidence results
});
```

### Embeddings Manager

```javascript
const manager = createEmbeddingsManager({
  embeddingDim: 128,         // Reduce for faster training
  epochs: 100,               // Increase for better quality
  batchSize: 64,             // Adjust for memory
  learningRate: 0.01,        // Fine-tune convergence
  negativeRatio: 2           // Negative samples per positive
});
```

### Anomaly Detector

```javascript
const detector = createAnomalyDetector({
  enableStatistical: true,   // Statistical methods
  enableMLBased: true,       // ML-based methods
  outlierThreshold: 2.5,     // Z-score threshold
  minConfidence: 0.6,        // Filter low-confidence
  maxAnomalies: 100          // Limit results
});
```

## Advanced Usage

### Combining Multiple Modules

```javascript
import {
  createSemanticAnalyzer,
  createNLPQueryBuilder,
  createEmbeddingsManager,
  createAnomalyDetector
} from './src/knowledge-engine/ai-semantic/index.mjs';

// Create full AI semantic pipeline
const analyzer = createSemanticAnalyzer();
const nlpBuilder = createNLPQueryBuilder();
const embeddings = createEmbeddingsManager();
const detector = createAnomalyDetector();

// 1. Generate embeddings
await embeddings.generateEmbeddings(store);

// 2. Analyze semantics
const analysis = await analyzer.analyze(store);

// 3. Detect anomalies
const anomalies = await detector.detectAnomalies(store);

// 4. Query with natural language
const nlQuery = await nlpBuilder.buildQuery(
  'show entities with anomalies',
  store
);

// 5. Use embeddings to find similar concepts
const similar = analysis.concepts
  .map(c1 => ({
    concept: c1,
    similarity: embeddings.computeSimilarity(c1.uri, targetUri)
  }))
  .filter(s => s.similarity > 0.7)
  .sort((a, b) => b.similarity - a.similarity);
```

### Custom Anomaly Detection

```javascript
class CustomAnomalyDetector extends AnomalyDetector {
  async _detectCustomAnomalies(store) {
    // Add your custom anomaly detection logic
    const anomalies = [];

    // Example: detect entities without specific properties
    const required = 'http://example.org/required-prop';

    for (const quad of store) {
      const hasRequired = Array.from(store).some(q =>
        q.subject.value === quad.subject.value &&
        q.predicate.value === required
      );

      if (!hasRequired) {
        anomalies.push({
          type: 'data_quality',
          severity: 'medium',
          description: 'Missing required property',
          subject: quad.subject.value,
          confidence: 0.85
        });
      }
    }

    return anomalies;
  }
}
```

## Testing

Run the comprehensive test suite:

```bash
# All AI semantic tests
pnpm test test/ai-semantic/

# Individual modules
pnpm test test/ai-semantic/semantic-analyzer.test.mjs
pnpm test test/ai-semantic/nlp-query-builder.test.mjs
pnpm test test/ai-semantic/embeddings-manager.test.mjs
pnpm test test/ai-semantic/anomaly-detector.test.mjs
```

## Performance Targets

All modules meet the following targets:

✅ Semantic Analysis: <500ms for graphs with <1000 triples
✅ NL→SPARQL Translation: <300ms
✅ Embedding Generation: Variable (depends on graph size and epochs)
✅ Anomaly Detection: <2s for moderate graphs

## Troubleshooting

### Issue: Semantic analysis is slow

**Solution**: Reduce `maxConcepts`, increase `minConceptFrequency`, or enable caching

### Issue: NLP query builder returns low confidence

**Solution**: Add more `rdfs:label` annotations to your graph, or enable LLM with API key

### Issue: Embeddings don't converge

**Solution**: Adjust `learningRate`, increase `epochs`, or use a different `algorithm`

### Issue: Too many false positive anomalies

**Solution**: Increase `minConfidence`, adjust `outlierThreshold`, or disable specific detection methods

## API Reference

See JSDoc documentation in source files:
- `/src/knowledge-engine/ai-semantic/semantic-analyzer.mjs`
- `/src/knowledge-engine/ai-semantic/nlp-query-builder.mjs`
- `/src/knowledge-engine/ai-semantic/embeddings-manager.mjs`
- `/src/knowledge-engine/ai-semantic/anomaly-detector.mjs`

## License

MIT - See LICENSE file

## Support

- GitHub Issues: https://github.com/unrdf/unrdf/issues
- Documentation: https://github.com/unrdf/unrdf/docs
