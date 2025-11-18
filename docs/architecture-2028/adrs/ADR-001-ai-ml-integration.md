# ADR-001: AI/ML Integration Layer

**Status:** Proposed
**Date:** 2025-11-18
**Deciders:** System Architecture Team
**Technical Story:** Enable intelligent knowledge graph operations through AI/ML

## Context and Problem Statement

Knowledge graphs contain rich semantic information, but extracting insights, translating natural language to SPARQL, and predicting missing links require AI/ML capabilities. How do we integrate AI/ML into unrdf while maintaining modularity and performance?

## Decision Drivers

- **Performance**: AI/ML operations should not degrade core query performance
- **Modularity**: AI features must be completely optional
- **Accessibility**: Support both cloud APIs and local models
- **Privacy**: Enable on-device inference for sensitive data
- **Compatibility**: Work in both Node.js and browser environments

## Considered Options

### Option 1: External AI Service Only
- **Pros**: Simple integration, no model management, latest models
- **Cons**: Requires internet, privacy concerns, API costs, latency

### Option 2: Local Models Only (TensorFlow.js, ONNX)
- **Pros**: Privacy-preserving, offline capable, no API costs
- **Cons**: Limited model selection, resource intensive, model management

### Option 3: Hybrid Approach (Recommended)
- **Pros**: Flexibility, privacy when needed, performance when available
- **Cons**: More complex architecture, multiple code paths

## Decision Outcome

**Chosen option:** Option 3 - Hybrid Approach

AI/ML integration as an optional, pluggable layer supporting both local and remote inference with a unified interface.

### Architecture Design

#### Component Structure

```
src/ai-ml/
├── core/
│   ├── ai-engine.mjs              # Main orchestrator
│   ├── model-registry.mjs         # Model management
│   ├── inference-adapter.mjs      # Unified inference interface
│   └── schemas.mjs                # Zod schemas
│
├── embeddings/
│   ├── graph-embedder.mjs         # Main embedding coordinator
│   ├── algorithms/
│   │   ├── node2vec.mjs           # Random walk embeddings
│   │   ├── graph-sage.mjs         # Inductive graph embeddings
│   │   ├── transformer-embed.mjs  # Transformer-based (BERT, RoBERTa)
│   │   └── rdf2vec.mjs            # RDF-specific embeddings
│   ├── backends/
│   │   ├── tfjs-backend.mjs       # TensorFlow.js
│   │   ├── onnx-backend.mjs       # ONNX Runtime
│   │   └── api-backend.mjs        # External APIs (OpenAI, Cohere)
│   └── index.mjs
│
├── nl-sparql/
│   ├── translator.mjs             # Natural language → SPARQL
│   ├── query-refiner.mjs          # Query optimization & validation
│   ├── schema-context.mjs         # Schema-aware translation
│   ├── examples/
│   │   ├── few-shot-learner.mjs   # Few-shot learning
│   │   └── example-store.mjs      # Example management
│   └── index.mjs
│
├── reasoning/
│   ├── neural-reasoner.mjs        # Neural reasoning engine
│   ├── semantic-enhancer.mjs      # Semantic similarity
│   ├── ontology-alignment.mjs     # Ontology matching
│   └── index.mjs
│
├── completion/
│   ├── kg-completer.mjs           # Knowledge graph completion
│   ├── link-predictor.mjs         # Link prediction
│   ├── entity-resolver.mjs        # Entity resolution
│   ├── type-predictor.mjs         # Type inference
│   └── index.mjs
│
└── index.mjs
```

#### Core Interfaces

```javascript
/**
 * @typedef {Object} AIEngineConfig
 * @property {'local'|'remote'|'hybrid'} mode
 * @property {Object} models - Model configurations
 * @property {Object} backends - Backend preferences
 * @property {Object} cache - Caching configuration
 */

/**
 * AI/ML Engine for knowledge graph operations
 */
export class AIEngine {
  constructor(config = {}) {
    this.config = {
      mode: config.mode || 'hybrid',
      models: config.models || {},
      backends: config.backends || {},
      cache: config.cache || { enabled: true, ttl: 3600 }
    };

    this.modelRegistry = new ModelRegistry();
    this.inferenceAdapter = new InferenceAdapter(this.config);
  }

  /**
   * Generate graph embeddings
   * @param {Store} store - N3 store
   * @param {Object} options - Embedding options
   * @returns {Promise<Map<string, Float32Array>>} Node embeddings
   */
  async generateEmbeddings(store, options = {}) {
    const algorithm = options.algorithm || 'node2vec';
    const dimensions = options.dimensions || 128;

    const embedder = new GraphEmbedder({
      algorithm,
      dimensions,
      backend: this.inferenceAdapter
    });

    return await embedder.embed(store);
  }

  /**
   * Translate natural language to SPARQL
   * @param {string} naturalQuery - Natural language query
   * @param {Object} context - Schema and example context
   * @returns {Promise<string>} SPARQL query
   */
  async translateQuery(naturalQuery, context = {}) {
    const translator = new NLQueryTranslator({
      backend: this.inferenceAdapter,
      schemaContext: context.schema,
      examples: context.examples
    });

    return await translator.translate(naturalQuery);
  }

  /**
   * Predict missing links in knowledge graph
   * @param {Store} store - N3 store
   * @param {Object} options - Prediction options
   * @returns {Promise<Array<{subject, predicate, object, confidence}>>}
   */
  async predictLinks(store, options = {}) {
    const predictor = new LinkPredictor({
      backend: this.inferenceAdapter,
      threshold: options.threshold || 0.7,
      maxPredictions: options.maxPredictions || 100
    });

    return await predictor.predict(store);
  }

  /**
   * Enhance semantic reasoning
   * @param {Store} store - N3 store
   * @param {Object} query - Query object
   * @returns {Promise<Object>} Enhanced query results
   */
  async enhanceReasoning(store, query) {
    const reasoner = new NeuralReasoner({
      backend: this.inferenceAdapter
    });

    return await reasoner.reason(store, query);
  }
}
```

#### Graph Embedder Implementation

```javascript
/**
 * Graph embedding generator using various algorithms
 */
export class GraphEmbedder {
  constructor(config) {
    this.algorithm = config.algorithm || 'node2vec';
    this.dimensions = config.dimensions || 128;
    this.backend = config.backend;

    // Select algorithm implementation
    this.embedder = this._createEmbedder();
  }

  _createEmbedder() {
    switch (this.algorithm) {
      case 'node2vec':
        return new Node2Vec({
          dimensions: this.dimensions,
          walkLength: 80,
          walksPerNode: 10,
          p: 1,
          q: 1,
          backend: this.backend
        });
      case 'graph-sage':
        return new GraphSAGE({
          dimensions: this.dimensions,
          aggregator: 'mean',
          backend: this.backend
        });
      case 'rdf2vec':
        return new RDF2Vec({
          dimensions: this.dimensions,
          depth: 4,
          backend: this.backend
        });
      case 'transformer':
        return new TransformerEmbeddings({
          dimensions: this.dimensions,
          model: 'bert-base',
          backend: this.backend
        });
      default:
        throw new Error(`Unknown embedding algorithm: ${this.algorithm}`);
    }
  }

  /**
   * Generate embeddings for all nodes in the graph
   * @param {Store} store - N3 store
   * @returns {Promise<Map<string, Float32Array>>}
   */
  async embed(store) {
    const span = tracer.startSpan('ai.embeddings.generate');

    try {
      span.setAttribute('algorithm', this.algorithm);
      span.setAttribute('dimensions', this.dimensions);
      span.setAttribute('graph.size', store.size);

      // Convert RDF graph to adjacency structure
      const graph = this._buildAdjacencyList(store);

      span.setAttribute('graph.nodes', graph.nodes.size);
      span.setAttribute('graph.edges', graph.edges.length);

      // Generate embeddings using selected algorithm
      const embeddings = await this.embedder.train(graph);

      span.setStatus({ code: SpanStatusCode.OK });
      return embeddings;

    } catch (error) {
      span.recordException(error);
      span.setStatus({ code: SpanStatusCode.ERROR });
      throw error;
    } finally {
      span.end();
    }
  }

  _buildAdjacencyList(store) {
    const nodes = new Set();
    const edges = [];

    for (const quad of store) {
      const subject = quad.subject.value;
      const object = quad.object.value;

      nodes.add(subject);
      if (quad.object.termType === 'NamedNode') {
        nodes.add(object);
        edges.push([subject, object, quad.predicate.value]);
      }
    }

    return { nodes, edges };
  }
}
```

#### Natural Language to SPARQL Translator

```javascript
/**
 * Translates natural language queries to SPARQL
 */
export class NLQueryTranslator {
  constructor(config) {
    this.backend = config.backend;
    this.schemaContext = config.schemaContext;
    this.examples = config.examples || [];
  }

  /**
   * Translate natural language to SPARQL
   * @param {string} naturalQuery - Natural language question
   * @returns {Promise<string>} SPARQL query
   */
  async translate(naturalQuery) {
    const span = tracer.startSpan('ai.nl_sparql.translate');

    try {
      span.setAttribute('query.length', naturalQuery.length);

      // Build prompt with schema context and examples
      const prompt = this._buildPrompt(naturalQuery);

      // Call AI backend
      const sparqlQuery = await this.backend.complete({
        prompt,
        maxTokens: 512,
        temperature: 0.1,  // Low temperature for deterministic output
        stopSequences: ['\n\n']
      });

      // Validate and refine SPARQL
      const refined = await this._refineQuery(sparqlQuery);

      span.setAttribute('query.valid', true);
      span.setStatus({ code: SpanStatusCode.OK });

      return refined;

    } catch (error) {
      span.recordException(error);
      span.setStatus({ code: SpanStatusCode.ERROR });
      throw error;
    } finally {
      span.end();
    }
  }

  _buildPrompt(naturalQuery) {
    let prompt = 'Translate the following natural language question to SPARQL:\n\n';

    // Add schema context
    if (this.schemaContext) {
      prompt += 'Schema:\n';
      prompt += this._formatSchema(this.schemaContext);
      prompt += '\n\n';
    }

    // Add few-shot examples
    if (this.examples.length > 0) {
      prompt += 'Examples:\n';
      for (const example of this.examples) {
        prompt += `Q: ${example.question}\nA: ${example.sparql}\n\n`;
      }
    }

    // Add actual query
    prompt += `Q: ${naturalQuery}\nA:`;

    return prompt;
  }

  async _refineQuery(sparqlQuery) {
    const refiner = new QueryRefiner();

    // Parse and validate SPARQL
    const parsed = refiner.parse(sparqlQuery);

    // Apply optimizations
    const optimized = refiner.optimize(parsed);

    // Format consistently
    return refiner.format(optimized);
  }
}
```

### Integration with Knowledge Hooks

```javascript
// AI/ML hooks for the knowledge engine
defineHook('ai.embeddings.generated', {
  phase: 'post',
  condition: async ({ store }) => store.size > 100,
  effect: async ({ store, embeddings }) => {
    // Store embeddings for similarity search
    await embeddingStore.save(embeddings);

    // Trigger downstream analysis
    await analyzeEmbeddings(embeddings);
  }
});

defineHook('ai.query.translated', {
  phase: 'pre',
  condition: async ({ query }) => isNaturalLanguage(query),
  effect: async ({ query }) => {
    const sparql = await aiEngine.translateQuery(query);
    return { query: sparql };
  }
});

defineHook('ai.links.predicted', {
  phase: 'post',
  condition: async ({ predictions }) => predictions.length > 0,
  effect: async ({ store, predictions }) => {
    // Add high-confidence predictions to graph
    const highConfidence = predictions.filter(p => p.confidence > 0.9);
    for (const pred of highConfidence) {
      store.add(
        DataFactory.quad(
          DataFactory.namedNode(pred.subject),
          DataFactory.namedNode(pred.predicate),
          DataFactory.namedNode(pred.object)
        )
      );
    }
  }
});
```

### Model Management

```javascript
/**
 * Registry for AI/ML models
 */
export class ModelRegistry {
  constructor() {
    this.models = new Map();
    this.cache = new LRUCache({ max: 5 }); // Cache up to 5 loaded models
  }

  /**
   * Register a model
   * @param {string} name - Model identifier
   * @param {Object} config - Model configuration
   */
  register(name, config) {
    this.models.set(name, {
      name,
      type: config.type,  // 'local' | 'remote'
      source: config.source,  // URL or file path
      format: config.format,  // 'tfjs' | 'onnx' | 'api'
      metadata: config.metadata || {}
    });
  }

  /**
   * Load a model (with caching)
   * @param {string} name - Model identifier
   * @returns {Promise<Object>} Loaded model
   */
  async load(name) {
    // Check cache first
    if (this.cache.has(name)) {
      return this.cache.get(name);
    }

    const config = this.models.get(name);
    if (!config) {
      throw new Error(`Model not found: ${name}`);
    }

    // Load based on format
    let model;
    switch (config.format) {
      case 'tfjs':
        model = await tf.loadLayersModel(config.source);
        break;
      case 'onnx':
        model = await ort.InferenceSession.create(config.source);
        break;
      case 'api':
        model = { type: 'api', endpoint: config.source };
        break;
    }

    // Cache and return
    this.cache.set(name, model);
    return model;
  }
}
```

### Usage Examples

```javascript
// Example 1: Generate graph embeddings
import { AIEngine } from 'unrdf/ai-ml';
import { useGraph } from 'unrdf';

const { store } = useGraph();
const ai = new AIEngine({ mode: 'local' });

const embeddings = await ai.generateEmbeddings(store, {
  algorithm: 'node2vec',
  dimensions: 128
});

// Use embeddings for similarity search
const similar = findSimilar(embeddings, targetNode, topK = 10);

// Example 2: Natural language to SPARQL
const naturalQuery = "Find all researchers who published papers on knowledge graphs in 2024";

const sparqlQuery = await ai.translateQuery(naturalQuery, {
  schema: ontology,
  examples: fewShotExamples
});

const results = await query(store, sparqlQuery);

// Example 3: Knowledge graph completion
const predictions = await ai.predictLinks(store, {
  threshold: 0.8,
  maxPredictions: 50
});

console.log(`Predicted ${predictions.length} new links`);
```

## Consequences

### Positive

- **Enhanced Capabilities**: Natural language queries, intelligent search, link prediction
- **Flexibility**: Support for both local and cloud-based AI
- **Privacy**: On-device inference option for sensitive data
- **Progressive Enhancement**: Works without AI features, enhanced with them
- **Future-Proof**: Pluggable architecture supports new models

### Negative

- **Complexity**: Additional dependency management and code paths
- **Resource Usage**: AI models can be memory/CPU intensive
- **Model Management**: Need to handle model downloads, versioning
- **Latency**: AI operations add latency compared to direct queries
- **Accuracy**: AI-generated queries may not be 100% accurate

### Neutral

- **Bundle Size**: AI features are optional, only loaded when used
- **Browser Support**: Some models may not work in all browsers
- **API Costs**: Cloud-based inference incurs API costs

## Compliance and Security

- **Data Privacy**: Local inference option prevents data leakage
- **Model Security**: Validate model checksums before loading
- **Prompt Injection**: Sanitize natural language inputs
- **Rate Limiting**: Prevent abuse of AI features
- **Audit Logging**: Log all AI operations via lockchain

## Performance Targets

| Operation | Target Latency (P95) | Throughput |
|-----------|---------------------|------------|
| Embedding Generation | <500ms (1000 nodes) | 2k nodes/sec |
| NL→SPARQL Translation | <300ms | 100 queries/min |
| Link Prediction | <1s (10k triples) | 10k triples/min |
| Similarity Search | <50ms | 1k searches/sec |

## References

- [Graph Embeddings Survey](https://arxiv.org/abs/1709.07604)
- [KGQA: Question Answering on Knowledge Graphs](https://arxiv.org/abs/2202.11115)
- [TensorFlow.js](https://www.tensorflow.org/js)
- [ONNX Runtime](https://onnxruntime.ai/)
