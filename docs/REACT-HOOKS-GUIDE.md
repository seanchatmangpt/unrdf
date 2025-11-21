# UNRDF React Hooks - Complete Guide

> **Production-ready React hooks for all aspects of UNRDF**
> 35 hooks across 8 categories • Type-safe • Composable • Optimized

## 📋 Table of Contents

1. [Overview](#overview)
2. [Installation](#installation)
3. [Quick Start](#quick-start)
4. [Hook Categories](#hook-categories)
5. [Federation Hooks](#federation-hooks)
6. [Streaming Hooks](#streaming-hooks)
7. [Dark Matter Hooks](#dark-matter-hooks)
8. [AI/Semantic Hooks](#aisemantic-hooks)
9. [Advanced Utility Hooks](#advanced-utility-hooks)
10. [Policy & Security Hooks](#policy--security-hooks)
11. [Error & Recovery Hooks](#error--recovery-hooks)
12. [Form & UI Hooks](#form--ui-hooks)
13. [Best Practices](#best-practices)
14. [Performance Tips](#performance-tips)

---

## Overview

UNRDF provides 35 production-ready React hooks covering **every aspect** of working with RDF knowledge graphs:

- **Federation**: Distributed RDF across multiple stores with consensus
- **Streaming**: Real-time updates, change feeds, windowing, validation
- **Dark Matter**: 80/20 optimization, query analysis, bottleneck detection
- **AI/Semantic**: NLP, embeddings, anomaly detection, semantic analysis
- **Advanced Utility**: Graph diff, isomorphism, reasoning, quality metrics
- **Policy & Security**: SHACL validation, access control, sandboxing
- **Error & Recovery**: Error boundaries, retry logic, reporting
- **Form & UI**: SPARQL editors, visualizers, pagination, validation

**Architecture:**
- ✅ **Type-safe**: Full TypeScript/Zod validation
- ✅ **Composable**: Hooks compose naturally
- ✅ **Optimized**: Built-in caching, debouncing, memoization
- ✅ **Standard**: `{data, loading, error}` pattern throughout

---

## Installation

```bash
npm install unrdf react
```

All hooks are exported from category-specific modules:

```javascript
// Federation
import { useFederatedSystem, useConsensusManager } from 'unrdf/react-hooks/federation';

// Streaming
import { useChangeFeed, useStreamProcessor } from 'unrdf/react-hooks/streaming';

// Dark Matter
import { useDarkMatterCore, useQueryAnalyzer } from 'unrdf/react-hooks/dark-matter';

// AI/Semantic
import { useSemanticAnalyzer, useNLPQueryBuilder } from 'unrdf/react-hooks/ai-semantic';

// Advanced Utility
import { useGraphDiff, useObservabilityManager } from 'unrdf/react-hooks/advanced-utility';

// Policy & Security
import { usePolicyPack, useSecurityValidator } from 'unrdf/react-hooks/policy-security';

// Error & Recovery
import { useErrorBoundary, useRecovery } from 'unrdf/react-hooks/error-recovery';

// Form & UI
import { useSPARQLEditor, useGraphVisualizer } from 'unrdf/react-hooks/form-ui';
```

---

## Quick Start

### Basic Query Hook

```javascript
import { useKnowledgeEngine } from 'unrdf/react-hooks/core';

function ProductList() {
  const { query, data, loading, error } = useKnowledgeEngine();

  useEffect(() => {
    query(`
      SELECT * WHERE {
        ?product a schema:Product ;
                 schema:name ?name ;
                 schema:price ?price .
      }
      LIMIT 10
    `);
  }, []);

  if (loading) return <div>Loading...</div>;
  if (error) return <div>Error: {error.message}</div>;

  return (
    <ul>
      {data.map(product => (
        <li key={product.product.value}>
          {product.name.value}: ${product.price.value}
        </li>
      ))}
    </ul>
  );
}
```

### Real-Time Updates

```javascript
import { useKnowledgeHook } from 'unrdf/react-hooks/core';

function LiveProductPrice({ productId }) {
  const { data } = useKnowledgeHook({
    hookId: 'product-price-updates',
    filter: { productId }
  });

  return <div className="price">${data?.price || '...'}</div>;
}
```

---

## Hook Categories

| Category | Hooks | Use Cases |
|----------|-------|-----------|
| **Federation** | 5 | Distributed RDF, consensus, replication |
| **Streaming** | 5 | Real-time updates, change feeds, windowing |
| **Dark Matter** | 4 | 80/20 optimization, query analysis |
| **AI/Semantic** | 4 | NLP, embeddings, anomaly detection |
| **Advanced Utility** | 6 | Graph operations, reasoning, quality |
| **Policy & Security** | 3 | Validation, access control, sandboxing |
| **Error & Recovery** | 3 | Error handling, retry, reporting |
| **Form & UI** | 5 | Editors, visualizers, pagination |

---

## Federation Hooks

### `useFederatedSystem`

Orchestrate distributed RDF across multiple stores.

```javascript
import { useFederatedSystem } from 'unrdf/react-hooks/federation';

function FederatedApp() {
  const {
    system,
    stores,
    registerStore,
    query,
    replicate,
    health
  } = useFederatedSystem({
    stores: ['store1', 'store2'],
    endpoints: {
      store1: 'http://localhost:3030/store1',
      store2: 'http://localhost:3030/store2'
    }
  });

  // Register new store
  const addStore = async () => {
    await registerStore({
      id: 'store3',
      endpoint: 'http://localhost:3030/store3'
    });
  };

  // Distributed query
  const runQuery = async () => {
    const result = await query(`
      SELECT * WHERE { ?s ?p ?o }
    `, {
      strategy: 'quorum', // 'fastest', 'all', 'leader'
      aggregation: 'union' // 'intersection'
    });
  };

  return (
    <div>
      <h2>Stores: {stores.length}</h2>
      <p>Health: {health.status}</p>
    </div>
  );
}
```

**Features:**
- Store registration/unregistration
- Distributed queries (fastest, quorum, all, leader)
- Data replication
- Health monitoring

### `useConsensusManager`

RAFT consensus protocol for distributed coordination.

```javascript
import { useConsensusManager } from 'unrdf/react-hooks/federation';

function ConsensusNode() {
  const {
    state,          // 'follower', 'candidate', 'leader'
    leader,
    term,
    propose,
    commitIndex
  } = useConsensusManager({
    protocol: 'raft',
    nodeId: 'node-1',
    peers: ['node-2', 'node-3']
  });

  const submitValue = async () => {
    if (state === 'leader') {
      await propose({ operation: 'insert', quads: [...] });
    }
  };

  return (
    <div>
      <p>State: {state}</p>
      <p>Leader: {leader}</p>
      <p>Term: {term}</p>
    </div>
  );
}
```

### `useDistributedQuery`

Execute SPARQL queries across federated stores.

```javascript
import { useDistributedQuery } from 'unrdf/react-hooks/federation';

function DistributedSearch() {
  const { data, execute, executionStats } = useDistributedQuery(null, {
    strategy: 'fastest',
    aggregation: 'union',
    timeout: 30000
  });

  const search = () => {
    execute(`SELECT * WHERE { ?s ?p ?o } LIMIT 100`);
  };

  return (
    <div>
      <button onClick={search}>Search</button>
      {executionStats && (
        <p>
          Queried {executionStats.storesQueried.length} stores
          in {executionStats.duration}ms
        </p>
      )}
    </div>
  );
}
```

### `useDataReplication`

Replicate data across stores with conflict resolution.

```javascript
import { useDataReplication } from 'unrdf/react-hooks/federation';

function ReplicationManager() {
  const {
    replicate,
    conflicts,
    resolveConflict,
    syncStatus,
    replicationStats
  } = useDataReplication({
    strategy: 'eventual',
    replicationFactor: 3,
    autoSync: true,
    conflictResolver: (versions) => versions[0] // Last-write-wins
  });

  const replicateChange = async () => {
    await replicate({
      operation: 'insert',
      quads: [...],
      vector: [1, 2, 3] // Version vector
    });
  };

  return (
    <div>
      <p>Pending: {syncStatus.pendingChanges}</p>
      <p>Conflicts: {conflicts.length}</p>
      <p>Success Rate: {
        Math.round((replicationStats.successCount / replicationStats.totalReplications) * 100)
      }%</p>
    </div>
  );
}
```

### `useFederationHealth`

Monitor federation system health and metrics.

```javascript
import { useFederationHealth } from 'unrdf/react-hooks/federation';

function HealthDashboard() {
  const {
    health,
    metrics,
    stores,
    consensus,
    isHealthy,
    refresh
  } = useFederationHealth({
    interval: 5000,
    onUnhealthy: (health) => {
      console.warn('System unhealthy:', health.score);
    }
  });

  return (
    <div>
      <h2>System Health: {health.score}/100</h2>
      <p>Status: {health.status}</p>
      <h3>Metrics</h3>
      <ul>
        <li>Latency (avg): {metrics.latency.avg}ms</li>
        <li>Throughput: {metrics.throughput.queries} queries/sec</li>
        <li>Errors: {metrics.errors.count}</li>
      </ul>
      <h3>Stores</h3>
      {stores.map(store => (
        <div key={store.id}>
          {store.id}: {store.status}
        </div>
      ))}
    </div>
  );
}
```

---

## Streaming Hooks

### `useSubscriptionManager`

Pattern-based subscriptions to graph changes.

```javascript
import { useSubscriptionManager } from 'unrdf/react-hooks/streaming';

function PriceMonitor() {
  const {
    subscribe,
    events,
    isActive
  } = useSubscriptionManager({
    pattern: `?s schema:price ?price`,
    filter: (delta) => {
      return delta.added.some(q => parseFloat(q.object.value) > 100);
    }
  });

  return (
    <div>
      <p>Active: {isActive ? 'Yes' : 'No'}</p>
      <p>Events: {events.length}</p>
      <ul>
        {events.slice(-10).map((event, i) => (
          <li key={i}>{JSON.stringify(event)}</li>
        ))}
      </ul>
    </div>
  );
}
```

### `useChangeFeed`

Real-time change stream with batching.

```javascript
import { useChangeFeed } from 'unrdf/react-hooks/streaming';

function ChangeFeedMonitor() {
  const {
    changes,
    start,
    stop,
    stats
  } = useChangeFeed({
    operations: ['insert', 'delete'],
    batchSize: 10,
    batchInterval: 1000
  });

  return (
    <div>
      <button onClick={start}>Start</button>
      <button onClick={stop}>Stop</button>
      <p>Total Changes: {stats.totalChanges}</p>
      <p>Inserts: {stats.inserts}</p>
      <p>Deletes: {stats.deletes}</p>
    </div>
  );
}
```

### `useStreamProcessor`

Window operations (tumbling, sliding, session).

```javascript
import { useStreamProcessor } from 'unrdf/react-hooks/streaming';

function StreamAnalyzer() {
  const {
    windows,
    currentWindow,
    start,
    stats
  } = useStreamProcessor({
    windowType: 'tumbling',
    windowSize: 5000,
    aggregator: (events) => ({
      count: events.length,
      avgPrice: events.reduce((sum, e) => sum + e.price, 0) / events.length
    })
  });

  return (
    <div>
      <h3>Stream Processor</h3>
      <p>Windows Processed: {stats.windowsProcessed}</p>
      <p>Events Processed: {stats.eventsProcessed}</p>
      {windows.map(window => (
        <div key={window.id}>
          Window {window.id}: {window.aggregated.count} events,
          avg price ${window.aggregated.avgPrice}
        </div>
      ))}
    </div>
  );
}
```

### `useRealTimeValidator`

Continuous SHACL validation on streaming data.

```javascript
import { useRealTimeValidator } from 'unrdf/react-hooks/streaming';

function ValidationMonitor() {
  const {
    violations,
    validChanges,
    invalidChanges,
    stats
  } = useRealTimeValidator({
    shapeGraph: 'http://example.org/shapes',
    autoValidate: true,
    onViolation: (violation) => {
      console.warn('Validation failed:', violation);
    }
  });

  return (
    <div>
      <h3>Validation Stats</h3>
      <p>Pass Rate: {stats.passRate}%</p>
      <p>Violations: {violations.length}</p>
      <ul>
        {violations.map((v, i) => (
          <li key={i}>
            {v.focusNode}: {v.message}
          </li>
        ))}
      </ul>
    </div>
  );
}
```

### `useStreamingPipeline`

Complete orchestration: subscriptions + processing + validation.

```javascript
import { useStreamingPipeline } from 'unrdf/react-hooks/streaming';

function StreamingApp() {
  const {
    start,
    stop,
    isPipelineRunning,
    changes,
    windows,
    violations,
    stats,
    health
  } = useStreamingPipeline({
    subscription: {
      pattern: `?s schema:price ?price`
    },
    processor: {
      windowType: 'tumbling',
      windowSize: 5000
    },
    validator: {
      shapeGraph: 'http://example.org/shapes'
    }
  });

  return (
    <div>
      <button onClick={start}>Start Pipeline</button>
      <button onClick={stop}>Stop Pipeline</button>
      <p>Running: {isPipelineRunning ? 'Yes' : 'No'}</p>
      <p>Health: {health.status}</p>
      <p>Events: {stats.pipeline.eventsProcessed}</p>
      <p>Windows: {stats.pipeline.windowsCompleted}</p>
      <p>Validations Passed: {stats.pipeline.validationsPassed}</p>
    </div>
  );
}
```

---

## Dark Matter Hooks

### `useDarkMatterCore`

80/20 analysis - identify critical 20% delivering 80% value.

```javascript
import { useDarkMatterCore } from 'unrdf/react-hooks/dark-matter';

function PerformanceAnalyzer() {
  const {
    analysis,
    criticalPaths,
    darkMatter,
    optimizationSuggestions,
    analyze
  } = useDarkMatterCore({
    sampleSize: 1000,
    onCriticalPathFound: (path) => {
      console.log('Critical:', path.id, 'delivers', path.valuePercentage, '%');
    }
  });

  return (
    <div>
      <button onClick={analyze}>Analyze</button>
      <h3>Pareto Score: {analysis.paretoScore}/100</h3>
      <p>Critical Paths: {criticalPaths.length}</p>
      <p>Dark Matter: {darkMatter.length}</p>
      <h4>Top Suggestions</h4>
      {optimizationSuggestions.slice(0, 5).map((sug, i) => (
        <div key={i}>
          <strong>{sug.type}</strong> - {sug.reason}
          <br />
          Estimated Gain: {sug.estimatedGain}
        </div>
      ))}
    </div>
  );
}
```

### `useQueryAnalyzer`

SPARQL query optimization and analysis.

```javascript
import { useQueryAnalyzer } from 'unrdf/react-hooks/dark-matter';

function QueryOptimizer() {
  const {
    analyzeQuery,
    optimizeQuery,
    slowQueries,
    suggestions
  } = useQueryAnalyzer({
    slowThreshold: 100,
    onSlowQuery: (query) => {
      console.warn('Slow query:', query.sparql);
    }
  });

  const optimize = async () => {
    const result = await optimizeQuery(`
      SELECT * WHERE {
        ?s ?p ?o .
        OPTIONAL { ?s rdfs:label ?label }
      }
    `, {
      strategies: ['add-limit', 'reorder-triples']
    });

    console.log('Original:', result.original);
    console.log('Optimized:', result.optimized);
    console.log('Estimated gain:', result.estimatedGain);
  };

  return (
    <div>
      <button onClick={optimize}>Optimize</button>
      <p>Slow Queries: {slowQueries.length}</p>
      <p>Suggestions: {suggestions.length}</p>
    </div>
  );
}
```

### `useOptimizer`

Comprehensive performance optimizer with auto-tune.

```javascript
import { useOptimizer } from 'unrdf/react-hooks/dark-matter';

function AutoOptimizer() {
  const {
    recommendations,
    applyOptimization,
    optimizationHistory,
    getTotalPotentialGain
  } = useOptimizer({
    autoTune: true,
    targets: ['queries', 'caching']
  });

  return (
    <div>
      <h3>Optimization Recommendations</h3>
      <p>Total Potential Gain: {getTotalPotentialGain()}%</p>
      {recommendations.slice(0, 5).map((rec, i) => (
        <div key={i}>
          <strong>{rec.title}</strong> ({rec.priority})
          <p>{rec.description}</p>
          <p>Estimated Gain: {rec.estimatedGainPercent}%</p>
          {rec.autoApplicable && (
            <button onClick={() => applyOptimization(rec.id)}>
              Apply
            </button>
          )}
        </div>
      ))}
      <h4>History</h4>
      <p>Applied: {optimizationHistory.length}</p>
    </div>
  );
}
```

### `useCriticalPath`

Critical path analysis with bottleneck identification.

```javascript
import { useCriticalPath } from 'unrdf/react-hooks/dark-matter';

function CriticalPathAnalyzer() {
  const {
    criticalPath,
    bottlenecks,
    visualization,
    optimizePath
  } = useCriticalPath({
    operations: ['query', 'render', 'network']
  });

  return (
    <div>
      {criticalPath && (
        <>
          <h3>Critical Path</h3>
          <p>Total Time: {criticalPath.totalTime}ms</p>
          <p>Operations: {criticalPath.operations.length}</p>
          <p>Total Value: {Math.round(criticalPath.totalValue * 100)}%</p>
        </>
      )}
      <h4>Bottlenecks</h4>
      {bottlenecks.map((b, i) => (
        <div key={i}>
          <strong>{b.operation}</strong> - {b.type}
          <p>{b.reason}</p>
          <p>Suggestions: {b.suggestions.join(', ')}</p>
        </div>
      ))}
    </div>
  );
}
```

---

## AI/Semantic Hooks

### `useSemanticAnalyzer`

Semantic relationship analysis and pattern detection.

```javascript
import { useSemanticAnalyzer } from 'unrdf/react-hooks/ai-semantic';

function SemanticExplorer() {
  const {
    analyze,
    findSimilar,
    relationships,
    concepts
  } = useSemanticAnalyzer({
    ontologies: ['schema.org', 'foaf']
  });

  const exploreEntity = async () => {
    const rels = await analyze('http://example.org/Product/123');
    const similar = await findSimilar('http://example.org/Product/123', 0.7);
  };

  return (
    <div>
      <h3>Relationships</h3>
      {relationships.map((rel, i) => (
        <div key={i}>
          {rel.predicate} → {rel.object} ({rel.type})
        </div>
      ))}
    </div>
  );
}
```

### `useNLPQueryBuilder`

Natural language to SPARQL conversion.

```javascript
import { useNLPQueryBuilder } from 'unrdf/react-hooks/ai-semantic';

function NaturalLanguageQuery() {
  const {
    executeNaturalQuery,
    queryHistory
  } = useNLPQueryBuilder();

  const search = async () => {
    const result = await executeNaturalQuery('show all products');
    console.log(result);
  };

  return (
    <div>
      <input
        placeholder="Ask a question..."
        onKeyPress={(e) => {
          if (e.key === 'Enter') {
            executeNaturalQuery(e.target.value);
          }
        }}
      />
      <h4>History</h4>
      {queryHistory.map((h, i) => (
        <div key={i}>
          <strong>{h.natural}</strong>
          <pre>{h.sparql}</pre>
        </div>
      ))}
    </div>
  );
}
```

### `useEmbeddingsManager`

Vector embeddings for semantic search.

```javascript
import { useEmbeddingsManager } from 'unrdf/react-hooks/ai-semantic';

function SemanticSearch() {
  const {
    indexEntity,
    searchSimilar,
    embeddings
  } = useEmbeddingsManager({
    model: 'text-embedding-ada-002',
    dimensions: 1536
  });

  const search = async () => {
    const results = await searchSimilar('laptop computer', 10);
    console.log('Similar entities:', results);
  };

  return (
    <div>
      <p>Indexed Entities: {embeddings.size}</p>
      <button onClick={search}>Search</button>
    </div>
  );
}
```

### `useAnomalyDetector`

Detect anomalies and outliers in graph data.

```javascript
import { useAnomalyDetector } from 'unrdf/react-hooks/ai-semantic';

function AnomalyMonitor() {
  const {
    detectAnomalies,
    anomalies,
    stats
  } = useAnomalyDetector({
    threshold: 2.5,
    onAnomaly: (anomaly) => {
      console.warn('Anomaly:', anomaly.subject, anomaly.value);
    }
  });

  const detect = async () => {
    await detectAnomalies('http://schema.org/price');
  };

  return (
    <div>
      <button onClick={detect}>Detect Anomalies</button>
      <p>Total: {stats.total}</p>
      <p>Anomalies: {stats.anomalies}</p>
      {anomalies.map((a, i) => (
        <div key={i}>
          {a.subject}: {a.value} (z-score: {a.zScore.toFixed(2)})
        </div>
      ))}
    </div>
  );
}
```

---

## Advanced Utility Hooks

### `useGraphDiff`, `useIsomorphism`, `useReasoningSession`, `useGraphMerge`, `useQualityMetrics`, `useObservabilityManager`

[See full API reference in source files]

---

## Best Practices

### 1. Use Composition

```javascript
// ✅ GOOD: Compose hooks
function AdvancedComponent() {
  const darkMatter = useDarkMatterCore();
  const queryAnalyzer = useQueryAnalyzer();
  const optimizer = useOptimizer(); // Composes both above

  // optimizer has recommendations from both sources
}
```

### 2. Leverage Auto-Features

```javascript
// ✅ GOOD: Let hooks handle optimization
const { data } = useDistributedQuery(sparql, {
  strategy: 'fastest',  // Auto-selects fastest store
  cache: true           // Auto-caches results
});
```

### 3. Handle Errors Properly

```javascript
// ✅ GOOD: Use error recovery
const { executeWithRecovery } = useRecovery({ maxRetries: 3 });

await executeWithRecovery(async () => {
  return await query(sparql);
});
```

### 4. Monitor Performance

```javascript
// ✅ GOOD: Use observability
const { startTrace, endTrace } = useObservabilityManager();

const traceId = startTrace('query-products');
await query(sparql);
endTrace(traceId);
```

---

## Performance Tips

1. **Use Dark Matter hooks** to identify bottlenecks
2. **Enable caching** for frequently accessed data
3. **Batch operations** with streaming hooks
4. **Use windowing** for large data streams
5. **Implement pagination** for result sets
6. **Monitor with observability** hooks

---

## Support

- Documentation: [UNRDF Docs](https://unrdf.dev)
- GitHub: [UNRDF Repository](https://github.com/unrdf/unrdf)
- Issues: [Report Issues](https://github.com/unrdf/unrdf/issues)

---

**Created with UNRDF v3.1.0**
© 2025 UNRDF Contributors
