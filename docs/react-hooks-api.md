# UNRDF React Hooks API Reference

Complete API reference for all 40+ hooks in the UNRDF React Hooks library, organized by usage tier following the 80/20 principle.

## Table of Contents

- [Installation](#installation)
- [Quick Start](#quick-start)
- [Tier 1: Essential Hooks (60% Usage)](#tier-1-essential-hooks-60-usage)
- [Tier 2: Important Hooks (20% Usage)](#tier-2-important-hooks-20-usage)
- [Tier 3: Standard Hooks (15% Usage)](#tier-3-standard-hooks-15-usage)
- [Tier 4: Advanced Hooks (5% Usage)](#tier-4-advanced-hooks-5-usage)
- [Composition Hooks](#composition-hooks)
- [Category Imports](#category-imports)

---

## Installation

```bash
pnpm add unrdf
```

## Quick Start

```jsx
import { useKnowledgeEngine, useChangeFeed, useErrorBoundary } from 'unrdf/react-hooks';

function App() {
  const { engine, query, loading, error } = useKnowledgeEngine();
  const { changes, start } = useChangeFeed();
  const { hasError, resetError } = useErrorBoundary();

  // 80% of applications only need these core hooks
}
```

---

## Tier 1: Essential Hooks (60% Usage)

These hooks cover the majority of use cases. Most applications only need Tier 1.

### useKnowledgeEngine

**Purpose**: Primary hook for initializing and managing the UNRDF Knowledge Engine.

**Import**:
```javascript
import { useKnowledgeEngine } from 'unrdf/react-hooks';
```

**Parameters**:
| Parameter | Type | Default | Description |
|-----------|------|---------|-------------|
| `basePath` | `string` | `process.cwd()` | Base path for file resolution |
| `enableKnowledgeHooks` | `boolean` | `true` | Enable knowledge hook execution |
| `enableObservability` | `boolean` | `true` | Enable OpenTelemetry observability |
| `strictMode` | `boolean` | `false` | Enable strict error handling |
| `initialStore` | `Store` | `null` | Initial RDF store |
| `lockchainOptions` | `Object` | `{}` | Lockchain configuration |
| `autoInit` | `boolean` | `true` | Automatically initialize engine on mount |

**Returns**:
| Property | Type | Description |
|----------|------|-------------|
| `engine` | `KnowledgeHookManager` | Knowledge engine instance |
| `store` | `Store` | N3 RDF store |
| `query` | `Function` | Execute SPARQL query |
| `addKnowledgeHook` | `Function` | Register a knowledge hook |
| `removeKnowledgeHook` | `Function` | Remove a knowledge hook |
| `applyTransaction` | `Function` | Apply transaction to store |
| `loading` | `boolean` | Loading state |
| `error` | `Error\|null` | Error state |
| `reinitialize` | `Function` | Reinitialize engine |
| `stats` | `Object` | Engine statistics |

**Example**:
```jsx
import { useKnowledgeEngine } from 'unrdf/react-hooks';

function MyComponent() {
  const { engine, store, query, loading, error } = useKnowledgeEngine({
    enableKnowledgeHooks: true,
    enableObservability: true
  });

  useEffect(() => {
    if (!loading && engine) {
      query('SELECT * WHERE { ?s ?p ?o } LIMIT 10')
        .then(results => console.log(results));
    }
  }, [loading, engine]);

  return <div>Engine loaded: {!loading}</div>;
}
```

---

### useKnowledgeEngineContext

**Purpose**: Access Knowledge Engine instance from React context. Must be used within `KnowledgeEngineProvider`.

**Import**:
```javascript
import { useKnowledgeEngineContext } from 'unrdf/react-hooks';
```

**Parameters**: None

**Returns**:
| Property | Type | Description |
|----------|------|-------------|
| `engine` | `KnowledgeHookManager` | Knowledge engine instance |
| `store` | `Store` | N3 RDF store |
| `query` | `Function` | Execute SPARQL query |
| `applyTransaction` | `Function` | Apply transaction |

**Example**:
```jsx
import { useKnowledgeEngineContext } from 'unrdf/react-hooks';

function MyComponent() {
  const { engine, store, query } = useKnowledgeEngineContext();
  return <div>Store size: {store.size}</div>;
}
```

---

### useTransaction

**Purpose**: ACID-compliant transaction management for RDF operations.

**Import**:
```javascript
import { useTransaction } from 'unrdf/react-hooks';
```

**Parameters**: None (uses context)

**Returns**:
| Property | Type | Description |
|----------|------|-------------|
| `apply` | `Function` | Apply transaction delta |
| `pending` | `boolean` | Transaction in progress |
| `lastReceipt` | `Object\|null` | Last transaction receipt |

**Example**:
```jsx
import { useTransaction } from 'unrdf/react-hooks';

function TransactionComponent() {
  const { apply, pending, lastReceipt } = useTransaction();

  const handleUpdate = async () => {
    const result = await apply({
      insert: [{ subject: 'ex:s', predicate: 'ex:p', object: 'ex:o' }],
      delete: []
    });
    console.log('Transaction complete:', result.receipt);
  };

  return (
    <button onClick={handleUpdate} disabled={pending}>
      {pending ? 'Processing...' : 'Update'}
    </button>
  );
}
```

---

### useKnowledgeHook

**Purpose**: Register and manage a single knowledge hook with lifecycle management.

**Import**:
```javascript
import { useKnowledgeHook } from 'unrdf/react-hooks';
```

**Parameters**:
| Parameter | Type | Default | Description |
|-----------|------|---------|-------------|
| `hookDefinition` | `Object` | required | Knowledge hook definition |
| `options.autoRegister` | `boolean` | `true` | Auto-register on mount |
| `options.autoUnregister` | `boolean` | `true` | Auto-unregister on unmount |

**Returns**:
| Property | Type | Description |
|----------|------|-------------|
| `register` | `Function` | Register the hook |
| `unregister` | `Function` | Unregister the hook |
| `execute` | `Function` | Manually execute the hook |
| `isRegistered` | `boolean` | Registration status |
| `error` | `Error\|null` | Error state |
| `lastExecution` | `Object\|null` | Last execution result |
| `status` | `string` | 'active' or 'inactive' |

**Example**:
```jsx
import { useKnowledgeHook } from 'unrdf/react-hooks';

function ValidationHook() {
  const { register, unregister, status } = useKnowledgeHook({
    meta: { name: 'validator', version: '1.0.0' },
    when: { event: 'pre-transaction' },
    run: async (event) => {
      // Validation logic
      return { success: true };
    }
  });

  return <div>Hook status: {status}</div>;
}
```

---

### useChangeFeed

**Purpose**: Real-time change stream consumption with filtering and batching.

**Import**:
```javascript
import { useChangeFeed } from 'unrdf/react-hooks';
```

**Parameters**:
| Parameter | Type | Default | Description |
|-----------|------|---------|-------------|
| `config.filter` | `Function` | `undefined` | Filter function for changes |
| `config.operations` | `string[]` | `['insert', 'delete']` | Operations to track |
| `config.includeMetadata` | `boolean` | `false` | Include transaction metadata |
| `config.batchSize` | `number` | `10` | Batch size for change events |
| `config.batchInterval` | `number` | `1000` | Batch interval (ms) |

**Returns**:
| Property | Type | Description |
|----------|------|-------------|
| `changes` | `Array` | Array of change events |
| `start` | `Function` | Start change feed |
| `stop` | `Function` | Stop change feed |
| `clear` | `Function` | Clear changes history |
| `isRunning` | `boolean` | Feed running status |
| `stats` | `Object` | Change feed statistics |
| `loading` | `boolean` | Loading state |
| `error` | `Error\|null` | Error state |
| `getChangesByOperation` | `Function` | Filter by operation type |
| `getChangesByPredicate` | `Function` | Filter by predicate |
| `getChangesBySubject` | `Function` | Filter by subject |
| `getRecentChanges` | `Function` | Get recent changes |
| `replay` | `Function` | Replay historical changes |

**Example**:
```jsx
import { useChangeFeed } from 'unrdf/react-hooks';

function RealTimeDashboard() {
  const { changes, start, stop, isRunning, stats } = useChangeFeed({
    filter: (change) => {
      return change.operation === 'insert' &&
             change.quads.some(q => q.predicate.value.includes('price'));
    },
    operations: ['insert', 'delete'],
    batchSize: 10,
    batchInterval: 1000
  });

  return (
    <div>
      <button onClick={isRunning ? stop : start}>
        {isRunning ? 'Stop' : 'Start'} Feed
      </button>
      <p>Total changes: {stats.totalChanges}</p>
      <ul>
        {changes.map((c, i) => (
          <li key={i}>{c.operation}: {c.quads.length} quads</li>
        ))}
      </ul>
    </div>
  );
}
```

---

### useDarkMatterCore

**Purpose**: 80/20 analysis - identify critical code paths that deliver maximum value.

**Import**:
```javascript
import { useDarkMatterCore } from 'unrdf/react-hooks';
```

**Parameters**:
| Parameter | Type | Default | Description |
|-----------|------|---------|-------------|
| `config.targets` | `string[]` | `undefined` | Specific queries/operations to analyze |
| `config.sampleSize` | `number` | `1000` | Number of operations to sample |
| `config.autoAnalyze` | `boolean` | `true` | Auto-analyze on changes |
| `config.onCriticalPathFound` | `Function` | `undefined` | Callback when critical path identified |

**Returns**:
| Property | Type | Description |
|----------|------|-------------|
| `analysis` | `Object` | Full analysis results |
| `criticalPaths` | `Array` | 20% of operations delivering 80% value |
| `darkMatter` | `Array` | 80% of operations delivering 20% value |
| `optimizationSuggestions` | `Array` | Prioritized optimization suggestions |
| `loading` | `boolean` | Loading state |
| `error` | `Error\|null` | Error state |
| `analyze` | `Function` | Trigger manual analysis |
| `getOperationsByPercentile` | `Function` | Get operations by value percentile |
| `getDarkMatterByPriority` | `Function` | Get dark matter sorted by removal priority |
| `focusOnCritical` | `Function` | Focus optimization on specific operation |
| `getParetoChartData` | `Function` | Get data for Pareto chart visualization |

**Example**:
```jsx
import { useDarkMatterCore } from 'unrdf/react-hooks';

function PerformanceAnalyzer() {
  const {
    analysis,
    criticalPaths,
    darkMatter,
    optimizationSuggestions,
    analyze
  } = useDarkMatterCore({
    targets: ['products', 'orders'],
    sampleSize: 1000,
    onCriticalPathFound: (path) => {
      console.log('Critical path:', path, 'delivers', path.valuePercentage, '% of value');
    }
  });

  return (
    <div>
      <h3>Pareto Score: {analysis.paretoScore}/100</h3>
      <h4>Critical Paths (Optimize First)</h4>
      <ul>
        {criticalPaths.map(path => (
          <li key={path.id}>{path.id}: {Math.round(path.value * 100)}% value</li>
        ))}
      </ul>
      <h4>Dark Matter (Consider Removing)</h4>
      <ul>
        {darkMatter.slice(0, 5).map(op => (
          <li key={op.id}>{op.id}: {Math.round(op.value * 100)}% value</li>
        ))}
      </ul>
    </div>
  );
}
```

---

### useQueryAnalyzer

**Purpose**: SPARQL query optimization analysis with slow query detection.

**Import**:
```javascript
import { useQueryAnalyzer } from 'unrdf/react-hooks';
```

**Parameters**:
| Parameter | Type | Default | Description |
|-----------|------|---------|-------------|
| `config.autoOptimize` | `boolean` | `false` | Auto-apply safe optimizations |
| `config.slowThreshold` | `number` | `100` | Slow query threshold (ms) |
| `config.onSlowQuery` | `Function` | `undefined` | Callback for slow queries |

**Returns**:
| Property | Type | Description |
|----------|------|-------------|
| `analyzeQuery` | `Function` | Analyze a SPARQL query |
| `optimizeQuery` | `Function` | Optimize a SPARQL query |
| `slowQueries` | `Array` | List of detected slow queries |
| `suggestions` | `Array` | Optimization suggestions |
| `queryStats` | `Object` | Query statistics |
| `loading` | `boolean` | Loading state |
| `error` | `Error\|null` | Error state |
| `getTopSlowQueries` | `Function` | Get top N slow queries |
| `getSuggestionsBySeverity` | `Function` | Filter suggestions by severity |
| `getQueryPatternAnalysis` | `Function` | Analyze query patterns |
| `clear` | `Function` | Clear history |

**Example**:
```jsx
import { useQueryAnalyzer } from 'unrdf/react-hooks';

function QueryOptimizer() {
  const {
    analyzeQuery,
    optimizeQuery,
    slowQueries,
    suggestions,
    queryStats
  } = useQueryAnalyzer({
    slowThreshold: 100,
    onSlowQuery: (query) => {
      console.warn('Slow query detected:', query.sparql, query.executionTime);
    }
  });

  const handleOptimize = async () => {
    const result = await optimizeQuery(`
      SELECT * WHERE { ?s ?p ?o }
    `, { autoApply: true });
    console.log('Optimized:', result.optimized);
    console.log('Estimated gain:', result.estimatedGain);
  };

  return (
    <div>
      <p>Total queries: {queryStats.totalQueries}</p>
      <p>Avg execution time: {queryStats.avgExecutionTime}ms</p>
      <button onClick={handleOptimize}>Optimize Query</button>
    </div>
  );
}
```

---

### useErrorBoundary

**Purpose**: Error boundary functionality for graceful error handling.

**Import**:
```javascript
import { useErrorBoundary } from 'unrdf/react-hooks';
```

**Parameters**:
| Parameter | Type | Default | Description |
|-----------|------|---------|-------------|
| `config.onError` | `Function` | `undefined` | Error callback |
| `config.fallback` | `Function` | `undefined` | Fallback render function |

**Returns**:
| Property | Type | Description |
|----------|------|-------------|
| `hasError` | `boolean` | Error state |
| `error` | `Error\|null` | Captured error |
| `errorInfo` | `Object\|null` | Error info (component stack) |
| `resetError` | `Function` | Reset error state |
| `captureError` | `Function` | Manually capture error |

**Example**:
```jsx
import { useErrorBoundary } from 'unrdf/react-hooks';

function SafeComponent() {
  const { hasError, error, resetError, captureError } = useErrorBoundary({
    onError: (error, errorInfo) => {
      console.error('Caught error:', error);
    }
  });

  if (hasError) {
    return (
      <div>
        <p>Something went wrong: {error.message}</p>
        <button onClick={resetError}>Try Again</button>
      </div>
    );
  }

  return <div>Content</div>;
}
```

---

## Tier 2: Important Hooks (20% Usage)

These hooks extend functionality for more complex use cases.

### useGraphDiff

**Purpose**: Compute differences between RDF graphs with patch generation.

**Import**:
```javascript
import { useGraphDiff } from 'unrdf/react-hooks';
```

**Parameters**:
| Parameter | Type | Default | Description |
|-----------|------|---------|-------------|
| `config` | `Object` | `{}` | Configuration options |

**Returns**:
| Property | Type | Description |
|----------|------|-------------|
| `computeDiff` | `Function` | Compute diff between two graphs |
| `applyPatch` | `Function` | Apply a patch to target graph |
| `diff` | `Object\|null` | Computed diff result |
| `added` | `Array` | Added quads |
| `removed` | `Array` | Removed quads |
| `modified` | `Array` | Modified quads |
| `loading` | `boolean` | Loading state |
| `error` | `Error\|null` | Error state |

**Example**:
```jsx
import { useGraphDiff } from 'unrdf/react-hooks';

function GraphCompare() {
  const { computeDiff, diff, added, removed, applyPatch } = useGraphDiff();

  const handleCompare = async () => {
    const result = await computeDiff('graph:v1', 'graph:v2');
    console.log(`Added: ${result.added.length}, Removed: ${result.removed.length}`);
  };

  return (
    <div>
      <button onClick={handleCompare}>Compare Graphs</button>
      {diff && (
        <div>
          <p>Added: {added.length} quads</p>
          <p>Removed: {removed.length} quads</p>
        </div>
      )}
    </div>
  );
}
```

---

### useSPARQLEditor

**Purpose**: SPARQL editor with syntax validation and formatting.

**Import**:
```javascript
import { useSPARQLEditor } from 'unrdf/react-hooks';
```

**Parameters**:
| Parameter | Type | Default | Description |
|-----------|------|---------|-------------|
| `config` | `Object` | `{}` | Configuration options |

**Returns**:
| Property | Type | Description |
|----------|------|-------------|
| `query` | `string` | Current query string |
| `setQuery` | `Function` | Set query string |
| `validate` | `Function` | Validate query syntax |
| `execute` | `Function` | Execute query |
| `format` | `Function` | Format query |
| `errors` | `Array` | Validation errors |
| `suggestions` | `Array` | Auto-complete suggestions |
| `loading` | `boolean` | Loading state |

**Example**:
```jsx
import { useSPARQLEditor } from 'unrdf/react-hooks';

function QueryEditor() {
  const { query, setQuery, validate, execute, format, errors } = useSPARQLEditor();

  return (
    <div>
      <textarea
        value={query}
        onChange={e => setQuery(e.target.value)}
      />
      <button onClick={() => validate(query)}>Validate</button>
      <button onClick={() => setQuery(format(query))}>Format</button>
      <button onClick={() => execute()}>Execute</button>
      {errors.map((e, i) => (
        <p key={i} style={{color: 'red'}}>{e.message}</p>
      ))}
    </div>
  );
}
```

---

## Tier 3: Standard Hooks (15% Usage)

### useFederatedSystem

**Purpose**: Manage federated RDF systems across multiple distributed stores.

**Import**:
```javascript
import { useFederatedSystem } from 'unrdf/react-hooks';
```

**Parameters**:
| Parameter | Type | Default | Description |
|-----------|------|---------|-------------|
| `config.stores` | `string[]` | `[]` | Array of store identifiers |
| `config.consensusProtocol` | `string` | `'raft'` | Consensus protocol (raft, gossip, byzantine) |
| `config.replicationFactor` | `number` | `3` | Number of replicas |
| `config.syncStrategy` | `Object` | `{}` | Synchronization strategy |

**Returns**:
| Property | Type | Description |
|----------|------|-------------|
| `system` | `Object` | Federation system instance |
| `stores` | `Array` | Registered stores |
| `health` | `Object` | Federation health status |
| `loading` | `boolean` | Loading state |
| `error` | `Error\|null` | Error state |
| `registerStore` | `Function` | Register new store |
| `unregisterStore` | `Function` | Unregister store |
| `query` | `Function` | Execute distributed query |
| `replicate` | `Function` | Replicate data across stores |
| `getStats` | `Function` | Get federation statistics |
| `refreshHealth` | `Function` | Refresh health status |

**Example**:
```jsx
import { useFederatedSystem } from 'unrdf/react-hooks';

function FederatedDashboard() {
  const {
    system,
    registerStore,
    query,
    replicate,
    health,
    loading
  } = useFederatedSystem({
    stores: ['store1', 'store2', 'store3'],
    consensusProtocol: 'raft',
    replicationFactor: 2
  });

  const handleQuery = async () => {
    const results = await query(`
      SELECT * WHERE { ?s ?p ?o } LIMIT 100
    `, {
      storeSelection: 'quorum',
      aggregation: 'union'
    });
    console.log(results);
  };

  return (
    <div>
      <p>Status: {health.status}</p>
      <button onClick={handleQuery}>Query All Stores</button>
    </div>
  );
}
```

---

### useStreamProcessor

**Purpose**: Process change streams with windowing and aggregation operations.

**Import**:
```javascript
import { useStreamProcessor } from 'unrdf/react-hooks';
```

**Parameters**:
| Parameter | Type | Default | Description |
|-----------|------|---------|-------------|
| `config.windowType` | `string` | `'tumbling'` | Window type: tumbling, sliding, session |
| `config.windowSize` | `number` | `5000` | Window size (ms) |
| `config.windowSlide` | `number` | `undefined` | Slide interval for sliding windows |
| `config.sessionGap` | `number` | `30000` | Gap for session windows |
| `config.aggregator` | `Function` | `undefined` | Custom aggregation function |

**Returns**:
| Property | Type | Description |
|----------|------|-------------|
| `windows` | `Array` | Completed windows |
| `currentWindow` | `Object\|null` | Window being built |
| `start` | `Function` | Start processing |
| `stop` | `Function` | Stop processing |
| `clear` | `Function` | Clear windows |
| `isProcessing` | `boolean` | Processing status |
| `stats` | `Object` | Processing statistics |
| `getWindowsByTimeRange` | `Function` | Filter windows by time |

**Example**:
```jsx
import { useStreamProcessor } from 'unrdf/react-hooks';

function StreamAnalytics() {
  const { windows, currentWindow, start, stop } = useStreamProcessor({
    windowType: 'tumbling',
    windowSize: 5000,
    aggregator: (events) => ({
      count: events.length,
      priceChanges: events.filter(e =>
        e.quads.some(q => q.predicate.value.includes('price'))
      ).length
    })
  });

  return (
    <div>
      <button onClick={start}>Start</button>
      <button onClick={stop}>Stop</button>
      <p>Windows processed: {windows.length}</p>
    </div>
  );
}
```

---

### useOptimizer

**Purpose**: Automated performance optimization with recommendations.

**Import**:
```javascript
import { useOptimizer } from 'unrdf/react-hooks';
```

**Parameters**:
| Parameter | Type | Default | Description |
|-----------|------|---------|-------------|
| `config.autoTune` | `boolean` | `false` | Auto-apply safe optimizations |
| `config.targets` | `string[]` | `undefined` | Specific targets to optimize |
| `config.onOptimization` | `Function` | `undefined` | Callback when optimization applied |

**Returns**:
| Property | Type | Description |
|----------|------|-------------|
| `recommendations` | `Array` | Prioritized optimization recommendations |
| `applyOptimization` | `Function` | Apply specific optimization |
| `optimizationHistory` | `Array` | History of applied optimizations |
| `getTopRecommendations` | `Function` | Get top N recommendations |
| `getTotalPotentialGain` | `Function` | Calculate total potential improvement |

---

### useSemanticAnalyzer

**Purpose**: Analyze semantic relationships and patterns in RDF graphs.

**Import**:
```javascript
import { useSemanticAnalyzer } from 'unrdf/react-hooks';
```

**Parameters**:
| Parameter | Type | Default | Description |
|-----------|------|---------|-------------|
| `config.ontologies` | `string[]` | `[]` | Ontologies to use for analysis |
| `config.inferRelationships` | `boolean` | `true` | Auto-infer relationships |

**Returns**:
| Property | Type | Description |
|----------|------|-------------|
| `analyze` | `Function` | Analyze entity relationships |
| `relationships` | `Array` | Discovered relationships |
| `concepts` | `Array` | Extracted concepts |
| `insights` | `Array` | Semantic insights |
| `findSimilar` | `Function` | Find semantically similar entities |

---

### useGraphMerge

**Purpose**: Merge multiple RDF graphs with conflict resolution.

**Import**:
```javascript
import { useGraphMerge } from 'unrdf/react-hooks';
```

**Parameters**:
| Parameter | Type | Default | Description |
|-----------|------|---------|-------------|
| `config.conflictResolver` | `Function` | `undefined` | Custom conflict resolution |

**Returns**:
| Property | Type | Description |
|----------|------|-------------|
| `mergeGraphs` | `Function` | Merge multiple graphs |
| `conflicts` | `Array` | Detected conflicts |
| `resolveConflict` | `Function` | Resolve specific conflict |
| `mergedGraph` | `Object\|null` | Resulting merged graph |

---

### usePolicyPack

**Purpose**: Load and manage SHACL policy packs for validation.

**Import**:
```javascript
import { usePolicyPack } from 'unrdf/react-hooks';
```

**Returns**:
| Property | Type | Description |
|----------|------|-------------|
| `loadPolicyPack` | `Function` | Load policy pack from URI |
| `validate` | `Function` | Validate data against policies |
| `policies` | `Array` | Loaded policies |
| `violations` | `Array` | Detected violations |

---

### useRecovery

**Purpose**: Automatic recovery with exponential backoff retry logic.

**Import**:
```javascript
import { useRecovery } from 'unrdf/react-hooks';
```

**Parameters**:
| Parameter | Type | Default | Description |
|-----------|------|---------|-------------|
| `config.maxRetries` | `number` | `3` | Maximum retry attempts |
| `config.retryDelay` | `number` | `1000` | Base delay between retries |

**Returns**:
| Property | Type | Description |
|----------|------|-------------|
| `executeWithRecovery` | `Function` | Execute function with retry logic |
| `retryCount` | `number` | Current retry count |
| `isRecovering` | `boolean` | Recovery in progress |
| `lastError` | `Error\|null` | Last error encountered |
| `reset` | `Function` | Reset recovery state |

---

### useGraphVisualizer

**Purpose**: Transform RDF query results into visualization-ready graph data.

**Import**:
```javascript
import { useGraphVisualizer } from 'unrdf/react-hooks';
```

**Returns**:
| Property | Type | Description |
|----------|------|-------------|
| `visualize` | `Function` | Convert query results to graph |
| `nodes` | `Array` | Graph nodes |
| `edges` | `Array` | Graph edges |
| `layout` | `string` | Current layout algorithm |
| `setLayout` | `Function` | Set layout algorithm |

---

### useResultsPaginator

**Purpose**: Paginate large query result sets.

**Import**:
```javascript
import { useResultsPaginator } from 'unrdf/react-hooks';
```

**Parameters**:
| Parameter | Type | Default | Description |
|-----------|------|---------|-------------|
| `data` | `Array` | `[]` | Data to paginate |
| `config.pageSize` | `number` | `10` | Items per page |

**Returns**:
| Property | Type | Description |
|----------|------|-------------|
| `currentPage` | `number` | Current page number |
| `pageData` | `Array` | Current page data |
| `nextPage` | `Function` | Go to next page |
| `prevPage` | `Function` | Go to previous page |
| `goToPage` | `Function` | Go to specific page |
| `totalPages` | `number` | Total number of pages |
| `hasNext` | `boolean` | Has next page |
| `hasPrev` | `boolean` | Has previous page |

---

## Tier 4: Advanced Hooks (5% Usage)

These hooks are available via category imports for specialized use cases.

### Federation Category

**Import**: `import * as Federation from 'unrdf/react-hooks/federation'`

| Hook | Description |
|------|-------------|
| `useConsensusManager` | Manage distributed consensus (RAFT, Gossip, Byzantine) |
| `useDistributedQuery` | Execute distributed SPARQL queries |
| `useDataReplication` | Manage data replication across stores |
| `useFederationHealth` | Monitor federation system health |

### Streaming Category

**Import**: `import * as Streaming from 'unrdf/react-hooks/streaming'`

| Hook | Description |
|------|-------------|
| `useSubscriptionManager` | Manage multiple subscriptions |
| `useRealTimeValidator` | Real-time data validation |
| `useStreamingPipeline` | Build complex streaming pipelines |

### Dark Matter Category

**Import**: `import * as DarkMatter from 'unrdf/react-hooks/dark-matter'`

| Hook | Description |
|------|-------------|
| `useCriticalPath` | Focus on critical path optimization |

### AI Semantic Category

**Import**: `import * as AISemantic from 'unrdf/react-hooks/ai-semantic'`

| Hook | Description |
|------|-------------|
| `useNLPQueryBuilder` | Build queries from natural language |
| `useEmbeddingsManager` | Manage vector embeddings |
| `useAnomalyDetector` | Detect anomalies in data |

### Advanced Utility Category

**Import**: `import * as AdvancedUtility from 'unrdf/react-hooks/advanced-utility'`

| Hook | Description |
|------|-------------|
| `useIsomorphism` | Check graph isomorphism |
| `useReasoningSession` | Manage reasoning sessions |
| `useQualityMetrics` | Track data quality metrics |
| `useObservabilityManager` | Manage observability |

### Policy Security Category

**Import**: `import * as PolicySecurity from 'unrdf/react-hooks/policy-security'`

| Hook | Description |
|------|-------------|
| `useSecurityValidator` | Validate security constraints |
| `useSandbox` | Sandbox execution environment |

### Error Recovery Category

**Import**: `import * as ErrorRecovery from 'unrdf/react-hooks/error-recovery'`

| Hook | Description |
|------|-------------|
| `useErrorReporting` | Report errors to external services |

### Form UI Category

**Import**: `import * as FormUI from 'unrdf/react-hooks/form-ui'`

| Hook | Description |
|------|-------------|
| `useQueryBuilder` | Visual query builder |
| `useFormValidation` | Form validation utilities |

---

## Composition Hooks

Pre-configured hook bundles for common patterns.

### useKnowledgeStack

**Purpose**: Pre-configured hook composition for common use cases.

**Import**:
```javascript
import { useKnowledgeStack } from 'unrdf/react-hooks';
```

**Presets**:
| Preset | Features |
|--------|----------|
| `'basic'` | Queries only, minimal overhead |
| `'realtime'` | Live updates for dashboards |
| `'resilient'` | Auto-retry for unreliable networks |
| `'full'` | All features enabled |

**Example**:
```jsx
// Basic usage
const { query, data, loading } = useKnowledgeStack({ preset: 'basic' });

// Real-time dashboard
const { query, data, changes, startLive } = useKnowledgeStack({ preset: 'realtime' });

// Production-resilient app
const { query, data, executeWithRecovery, hasError } = useKnowledgeStack({ preset: 'resilient' });

// Full-featured app
const stack = useKnowledgeStack({ preset: 'full' });
```

---

### useCRUDStack

**Purpose**: Pre-built stack for CRUD applications.

```javascript
import { useCRUDStack } from 'unrdf/react-hooks';
const { query, insert, delete: del, loading } = useCRUDStack();
```

---

### useDashboardStack

**Purpose**: Pre-built stack for real-time dashboards with auto-start live mode.

```javascript
import { useDashboardStack } from 'unrdf/react-hooks';
const { query, changes, data, loading } = useDashboardStack();
```

---

### useProductionStack

**Purpose**: Pre-built stack for production apps with resilience (5 retries, 2s delay).

```javascript
import { useProductionStack } from 'unrdf/react-hooks';
const { query, data, retryCount, isRecovering } = useProductionStack();
```

---

### useOfflineStore

**Purpose**: Offline-first IndexedDB persistence with sync queue.

**Import**:
```javascript
import { useOfflineStore } from 'unrdf/react-hooks';
```

**Parameters**:
| Parameter | Type | Default | Description |
|-----------|------|---------|-------------|
| `dbName` | `string` | `'unrdf-offline'` | IndexedDB database name |
| `syncInterval` | `number` | `30000` | Sync interval (ms) |
| `autoSync` | `boolean` | `true` | Auto-sync when online |
| `onSync` | `Function` | `undefined` | Callback when sync completes |
| `onConflict` | `Function` | `undefined` | Conflict resolution callback |

**Returns**:
| Property | Type | Description |
|----------|------|-------------|
| `quads` | `Array` | Local quads |
| `insert` | `Function` | Insert quads (offline-first) |
| `delete` | `Function` | Delete quads (offline-first) |
| `sync` | `Function` | Manually trigger sync |
| `pendingCount` | `number` | Pending mutations count |
| `isOnline` | `boolean` | Online status |
| `isSyncing` | `boolean` | Sync in progress |
| `lastSynced` | `Date\|null` | Last sync timestamp |

**Example**:
```jsx
import { useOfflineStore } from 'unrdf/react-hooks';

function OfflineApp() {
  const {
    quads,
    insert,
    sync,
    pendingCount,
    isOnline,
    lastSynced
  } = useOfflineStore({
    autoSync: true,
    onSync: (result) => console.log('Synced:', result.synced)
  });

  const handleAdd = async () => {
    await insert([{
      subject: 'ex:item1',
      predicate: 'ex:name',
      object: 'Test Item'
    }]);
    // Works offline! Queued for sync.
  };

  return (
    <div>
      <p>{isOnline ? 'Online' : 'Offline'}</p>
      <p>Pending sync: {pendingCount}</p>
      <button onClick={handleAdd}>Add Item</button>
      <button onClick={sync} disabled={!isOnline}>Sync Now</button>
    </div>
  );
}
```

---

## Category Imports

For advanced users who need access to all hooks in a category:

```javascript
// Import all hooks from a category
import * as Federation from 'unrdf/react-hooks/federation';
import * as Streaming from 'unrdf/react-hooks/streaming';
import * as DarkMatter from 'unrdf/react-hooks/dark-matter';
import * as AISemantic from 'unrdf/react-hooks/ai-semantic';
import * as AdvancedUtility from 'unrdf/react-hooks/advanced-utility';
import * as PolicySecurity from 'unrdf/react-hooks/policy-security';
import * as ErrorRecovery from 'unrdf/react-hooks/error-recovery';
import * as FormUI from 'unrdf/react-hooks/form-ui';
import * as Composition from 'unrdf/react-hooks/composition';

// Usage
const consensus = Federation.useConsensusManager({ ... });
const nlpBuilder = AISemantic.useNLPQueryBuilder({ ... });
```

---

## Usage Guide Summary

**80% of applications** only need Tier 1:
```javascript
import {
  useKnowledgeEngine,
  useChangeFeed,
  useDarkMatterCore,
  useQueryAnalyzer,
  useErrorBoundary
} from 'unrdf/react-hooks';
```

**15% of applications** add Tier 2:
```javascript
import {
  useGraphDiff,
  useSPARQLEditor
} from 'unrdf/react-hooks';
```

**4% of applications** explore Tier 3:
```javascript
import {
  useFederatedSystem,
  useStreamProcessor,
  useOptimizer
} from 'unrdf/react-hooks';
```

**1% of applications** need Tier 4 (advanced):
```javascript
import { useConsensusManager } from 'unrdf/react-hooks/federation';
import { useEmbeddingsManager } from 'unrdf/react-hooks/ai-semantic';
```
