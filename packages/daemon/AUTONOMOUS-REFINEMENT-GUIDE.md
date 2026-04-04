# Autonomous Knowledge Graph Refinement with UNRDF CLI

**Complete guide to using the autonomous refinement engine with all UNRDF tools.**

---

## Overview

The autonomous refinement engine integrates Groq LLM with the entire UNRDF ecosystem:

- **KGC 4D**: Graph snapshots with time-travel capabilities
- **Blockchain**: Immutable receipt chains for provenance
- **Hooks**: Reactive enrichment rules
- **KGC Probe**: Automated integrity scanning
- **Oxigraph**: Persistent RDF storage
- **Federation**: Multi-store queries
- **Semantic Search**: Context-aware enrichment
- **ML Inference**: Feature extraction for LLM reasoning
- **Observability**: Distributed tracing with OpenTelemetry
- **Caching**: High-performance query caching
- **Graph Analytics**: Quality metrics and monitoring

---

## Quick Start

### 1. Basic Refinement

```javascript
import { createAutonomousRefinementEngine } from '@unrdf/daemon';
import { createKnowledgeSubstrateCore } from '@unrdf/core';

const core = await createKnowledgeSubstrateCore();
const store = core.createStore('graph.db');

const engine = await createAutonomousRefinementEngine({
  graphId: 'my-graph',
  goalTriples: 5000,
  maxIterations: 100,
});

const report = await engine.refine(store);
console.log(`Refined graph from 2 to ${report.finalSize} triples in ${report.episodes} episodes`);
```

### 2. With Full UNRDF Integration

```javascript
const engine = await createAutonomousRefinementEngine(
  {
    graphId: 'enterprise-kg',
    goalTriples: 50000,
    maxIterations: 500,
    shaclValidation: true,
    enableSnapshots: true,
    enableBlockchain: true,
    enableObservability: true,
    enableCaching: true,
  },
  {
    // All optional - auto-loaded if available
    llmProvider: await getGroqProvider(),
    shacl: await core.shacl,
    kgc4d: await core.kgc4d,
    blockchain: await core.blockchain,
    hooks: await core.hooks,
    kgcProbe: await core.kgcProbe,
    federation: await core.federation,
    semanticSearch: await core.semanticSearch,
    analytics: await core.graphAnalytics,
    cache: await createCache(),
    tracer: await createTracer('autonomous-refinement'),
  }
);

engine.on('episode-complete', (episode) => {
  console.log(`Episode ${episode.episodeNumber}: +${episode.metrics.latencyMs}ms`);
});

engine.on('convergence', (result) => {
  console.log(`Graph converged: ${result.reason}`);
});

engine.on('refinement-complete', (report) => {
  console.log('Final Report:', report);
});

await engine.refine(store);
```

---

## Feature Integration Guide

### KGC 4D: Time-Travel Snapshots

Each refinement episode automatically snapshots the graph before enrichment:

```javascript
// Query snapshots to see how graph evolved
const snapshots = await kgc4d.listSnapshots(store);
snapshots.forEach((s) => {
  console.log(`Episode ${s.episode}: ${s.graphSize} triples`);
});

// Time-travel: restore graph to any previous state
const snapshot = snapshots[5]; // After 5 episodes
await store.restore(snapshot.id);

// Diff two snapshots to see what changed
const diff = await kgc4d.diff(snapshots[4].id, snapshots[5].id);
console.log('Triples added:', diff.additions.length);
console.log('Triples modified:', diff.modifications.length);
```

### Blockchain: Immutable Provenance

Link receipt chains to blockchain for tamper-proof provenance:

```javascript
// Each receipt is automatically recorded if blockchain is enabled
const episodes = engine.getEpisodes();
episodes.forEach((ep) => {
  if (ep.receipt) {
    console.log(`Receipt ${ep.receipt.id} on blockchain`);
    console.log(`  Previous: ${ep.receipt.previousHash}`);
    console.log(`  Links decision to graph mutation`);
  }
});

// Verify receipt chain integrity
const isValid = await blockchain.verifyChain(episodes[0].receipt.id);
console.log(`Receipt chain valid: ${isValid}`);
```

### Hooks: Reactive Enrichment

Register hooks that automatically react to LLM-induced changes:

```javascript
// Define hooks before refinement
const enrichmentHooks = [
  {
    name: 'auto-complete-foaf',
    condition: `
      ASK {
        ?person a foaf:Person .
        FILTER NOT EXISTS { ?person foaf:name ?n }
      }
    `,
    effect: `
      CONSTRUCT {
        ?person foaf:name ?name
      }
      WHERE {
        ?person a foaf:Person
        BIND(CONCAT('Person ', SUBSTR(STR(?person), 10)) AS ?name)
      }
    `,
  },
  {
    name: 'auto-link-colleagues',
    condition: `
      ASK {
        ?person1 foaf:workplaceHomepage ?company .
        ?person2 foaf:workplaceHomepage ?company .
        FILTER(?person1 != ?person2)
        FILTER NOT EXISTS { ?person1 foaf:knows ?person2 }
      }
    `,
    effect: `
      CONSTRUCT {
        ?person1 foaf:knows ?person2
      }
      WHERE {
        ?person1 foaf:workplaceHomepage ?company .
        ?person2 foaf:workplaceHomepage ?company .
        FILTER(?person1 != ?person2)
      }
    `,
  },
];

await hooks.registerAll(enrichmentHooks);

// Hooks automatically fire during refinement
// Triggered whenever conditions become satisfied
```

### KGC Probe: Integrity Scanning

Automatically scan graph integrity after each mutation:

```javascript
engine.on('episode-complete', async (episode) => {
  if (!episode.metrics.validationPassed) {
    console.warn(`Episode ${episode.episodeNumber} failed SHACL validation`);
  }

  // Detailed integrity report
  const probeResults = await kgcProbe.scan(store, {
    checks: [
      'schema-compliance',
      'type-consistency',
      'referential-integrity',
      'duplicate-detection',
    ],
  });

  if (probeResults.violations.length > 0) {
    console.error('Integrity violations:', probeResults.violations);
    // Optionally trigger remediation
  }
});
```

### Federation: Multi-Store Reasoning

Query across federated stores for enrichment context:

```javascript
const federated = await federation.create({
  stores: [
    store,  // Local graph
    'https://dbpedia.org/sparql',  // DBpedia
    'https://query.wikidata.org/sparql',  // Wikidata
  ],
});

// Refinement can use federated context
const engine = await createAutonomousRefinementEngine(
  { graphId: 'federated-kg', goalTriples: 10000 },
  {
    federation: federated,
    semanticSearch: await createSemanticSearch(federated),
  }
);

// LLM can query across all stores for context
await engine.refine(store);
```

### Semantic Search: Context-Aware Decisions

Use embeddings to provide semantic context to LLM:

```javascript
// Semantic search automatically integrated
// When LLM asks "what are similar entities?", semantic search returns:
// - Top-k similar triples by embedding similarity
// - Sorted by relevance to the violation

// Configure semantic search model
const semanticSearch = await createSemanticSearch(store, {
  embeddingModel: 'distilbert-base-uncased',
  similarity: 'cosine',
  k: 10,
});

// Refinement uses semantic context in prompts
const engine = await createAutonomousRefinementEngine(config, {
  semanticSearch,
});
```

### ML Inference: Feature Extraction

Extract graph features for more informed LLM reasoning:

```javascript
// ML inference is optional but enhances decision-making
const mlInference = await createMLInference(store, {
  models: {
    graph_embedding: 'graph2vec',
    node_classification: 'node2vec',
    link_prediction: 'distmult',
  },
});

const engine = await createAutonomousRefinementEngine(config, {
  mlInference,
});

// LLM receives feature vectors in prompts
// Example: "Node x has embedding [0.1, 0.2, ...], similar to nodes..."
```

### Observability: Distributed Tracing

Monitor refinement with OpenTelemetry:

```javascript
import { createTracer, createMeter } from '@unrdf/observability';

const tracer = await createTracer('autonomous-refinement', {
  exporter: 'jaeger', // or datadog, honeycomb, etc.
  service: 'refinement-service',
});

const meter = await createMeter('autonomous-refinement');

const engine = await createAutonomousRefinementEngine(config, { tracer });

// All episodes automatically traced:
// - RPC calls (LLM invocations)
// - Database operations (SPARQL queries)
// - Validation checks
// - Hook evaluations
// - Metrics recording

// View traces in Jaeger UI (localhost:6831)
// Analyze latencies, errors, and dependencies
```

### Caching: Performance Optimization

Cache SPARQL condition evaluations:

```javascript
const cache = await createCache({
  backend: 'redis',
  ttl: 300, // 5 minutes
  keyPrefix: 'refinement:',
});

const engine = await createAutonomousRefinementEngine(
  {
    graphId: 'cached-refinement',
    enableCaching: true,
    cacheKeyPrefix: 'my-graph:',
  },
  { cache }
);

// Engine automatically caches:
// - Condition evaluations (SPARQL ASK queries)
// - Semantic search results
// - Graph metrics
// - Violation scans

// Cache hit ratio visible in metrics
```

### Graph Analytics: Quality Monitoring

Track graph quality throughout refinement:

```javascript
import { createGraphAnalytics } from '@unrdf/graph-analytics';

const analytics = await createGraphAnalytics(store);

engine.on('episode-complete', async (episode) => {
  const metrics = await analytics.analyze(store);

  console.log('Graph Health:');
  console.log(`  Triples: ${metrics.tripleCount}`);
  console.log(`  Density: ${metrics.density.toFixed(4)}`);
  console.log(`  Components: ${metrics.componentCount}`);
  console.log(`  Avg Degree: ${metrics.avgDegree.toFixed(2)}`);
  console.log(`  Clustering: ${metrics.clusteringCoefficient.toFixed(4)}`);

  // Alert if quality degrades
  if (metrics.density < 0.1) {
    console.warn('WARNING: Graph density too low; enrichment may be diverging');
  }
});
```

---

## Advanced Patterns

### Multi-Episode Campaigns

Run multiple refinement campaigns with different strategies:

```javascript
async function multiCampaignRefinement(store) {
  // Campaign 1: Fill missing properties
  const campaign1 = await createAutonomousRefinementEngine({
    graphId: 'campaign-1-properties',
    goalTriples: 1000,
  });

  await campaign1.refine(store);

  // Campaign 2: Add relationships
  const campaign2 = await createAutonomousRefinementEngine({
    graphId: 'campaign-2-relationships',
    goalTriples: 5000,
  });

  await campaign2.refine(store);

  // Campaign 3: Validate and fix
  const campaign3 = await createAutonomousRefinementEngine({
    graphId: 'campaign-3-validation',
    goalTriples: 5000,
    shaclValidation: true,
    enableBlockchain: true,
  });

  await campaign3.refine(store);

  return store;
}

await multiCampaignRefinement(store);
```

### Export Refinement Results

Export complete refinement audit trail:

```javascript
async function exportAuditTrail(engine) {
  const episodes = engine.getEpisodes();

  const auditReport = {
    graphId: engine.config.graphId,
    timestamp: Date.now(),
    episodes: episodes.map((ep) => ({
      number: ep.episodeNumber,
      decision: ep.decision,
      receipt: ep.receipt,
      snapshot: ep.snapshot,
      metrics: ep.metrics,
    })),
    snapshots: await kgc4d.listSnapshots(),
    blockchain: episodes
      .filter((ep) => ep.receipt)
      .map((ep) => ({
        receipt: ep.receipt.id,
        blockNumber: ep.receipt.blockNumber,
      })),
  };

  // Export to JSON
  await fs.writeFile(
    'audit-trail.json',
    JSON.stringify(auditReport, null, 2)
  );

  // Export to RDF (Turtle format)
  const turtle = await rdfExport.toTurtle(auditReport);
  await fs.writeFile('audit-trail.ttl', turtle);
}

await exportAuditTrail(engine);
```

---

## Configuration Reference

```javascript
const config = {
  // Graph identification
  graphId: 'unique-graph-id',

  // Goal conditions
  goalTriples: 5000,
  maxIterations: 100,

  // Performance
  maxLatency: 30000, // ms per decision

  // Feature flags
  shaclValidation: true,
  enableSnapshots: true,
  enableBlockchain: false, // Expensive; use for auditable refinement
  enableObservability: true,
  enableCaching: true,

  // Observability
  cacheKeyPrefix: 'refinement:',
  observabilityProvider: 'otel', // otel | datadog | jaeger
};
```

---

## Event Reference

```javascript
engine.on('initialized', () => {});
engine.on('episode-complete', (episode) => {});
engine.on('convergence', (result) => {});
engine.on('parse-error', (error) => {});
engine.on('latency-warning', (warning) => {});
engine.on('error', (error) => {});
engine.on('refinement-complete', (report) => {});
```

---

## Troubleshooting

### LLM Producing Invalid Triples

**Problem**: LLM suggests triples that fail SHACL validation.

**Solution**: 
1. Enable `shaclValidation: true` to reject invalid mutations
2. Provide LLM with SHACL shape information in prompts
3. Use semantic search to guide LLM toward valid patterns

### Slow Refinement (High Latency)

**Problem**: Each episode takes >10 seconds.

**Solution**:
1. Enable caching: `enableCaching: true`
2. Reduce `k` in semantic search (fewer context triples)
3. Use lighter embedding model (DistilBERT instead of BERT)
4. Disable blockchain: `enableBlockchain: false`
5. Reduce `maxIterations` for faster feedback

### Graph Density Degradation

**Problem**: Graph metrics get worse after refinement.

**Solution**:
1. Add more reactive hooks to prevent duplicates
2. Lower goal (don't overgrow the graph)
3. Enable integrity scanning: use KGC Probe
4. Monitor graph analytics throughout

---

## See Also

- [PhD Thesis on Autonomous Refinement](../../docs/PHD-THESIS-LLM-RDF-AUTONOMY.md)
- [Groq Integration Guide](GROQ-INTEGRATION.md)
- [Local Agents Guide](LOCAL-AGENTS-GUIDE.md)
- [MCP Self-Play Documentation](MCP-SELF-PLAY.md)
- [UNRDF Core Documentation](../../docs/ARCHITECTURE.md)
