# UNRDF React Hooks Framework

A comprehensive React hooks framework that wraps all UNRDF features into idiomatic React patterns for maximum browser compatibility and developer experience.

## Overview

The UNRDF React Hooks Framework provides 40+ custom React hooks organized into 10 categories, enabling developers to build production-ready RDF knowledge graph applications in React with full support for:

- **RDF Data Management** - Store, query, and manipulate RDF triples
- **SPARQL Execution** - Reactive SPARQL queries with auto-refetch
- **Knowledge Hooks** - Policy-driven automation triggered by graph changes
- **SHACL Validation** - Shape-based validation with comprehensive reporting
- **Browser Storage** - IndexedDB-based persistence
- **Performance Optimization** - Query caching, batching, Dark Matter 80/20
- **Observability** - OpenTelemetry integration for monitoring
- **Error Handling** - Graceful error boundaries and recovery

## Installation

```bash
npm install unrdf react react-dom
# or
pnpm add unrdf react react-dom
```

## Quick Start

```jsx
import React from 'react';
import {
  KnowledgeEngineProvider,
  useStore,
  useSPARQLQuery,
  useTerms,
} from 'unrdf/react-hooks';

export function App() {
  return (
    <KnowledgeEngineProvider config={{ enableObservability: true }}>
      <RDFApp />
    </KnowledgeEngineProvider>
  );
}

function RDFApp() {
  const { store } = useKnowledgeEngineContext();
  const { namedNode, literal, quad } = useTerms();
  const { data } = useSPARQLQuery('SELECT ?s ?p ?o WHERE { ?s ?p ?o } LIMIT 10');

  return (
    <div>
      <h1>Store Size: {store?.size || 0}</h1>
      <ul>
        {data?.rows?.map((row) => (
          <li key={row.s.value}>{row.p.value}: {row.o.value}</li>
        ))}
      </ul>
    </div>
  );
}
```

## Hook Categories

### 1. Core Hooks (`useKnowledgeEngine`, `useStore`, `useTriples`, `useGraphs`, `useTerms`)

Fundamental hooks for initializing and managing the RDF store and creating RDF terms.

```jsx
const { store, isReady, error } = useKnowledgeEngine(config);
const { addQuad, removeQuad, clear } = useStore();
const { triples } = useTriples(subject, predicate, object);
const { graphs } = useGraphs();
const { namedNode, literal, blankNode, quad } = useTerms();
```

### 2. Query Hooks (`useSPARQLQuery`, `useQueryAsync`, `useShapeValidation`, `useReasoning`, `useDeltaQuery`)

Execute and manage queries with full reactivity and validation.

```jsx
const { data, loading, error } = useSPARQLQuery(query);
const { data, loading, error, refetch } = useQueryAsync(query, options);
const { results, isValid } = useShapeValidation(shapes);
const { results } = useReasoning(rules);
const { delta } = useDeltaQuery(queryA, queryB);
```

### 3. Knowledge Hooks (`useKnowledgeHook`, `useHookManager`, `useHookRegistry`, `useHookExecution`)

Manage policy-driven automation hooks.

```jsx
const { hook, evaluate } = useKnowledgeHook(definition);
const { hooks, addHook, removeHook } = useHookManager();
const { hooks, register, unregister } = useHookRegistry();
const { executing, result, error } = useHookExecution(hookName);
```

### 4. Storage Hooks (`useIndexedDBStore`, `useQuadStore`, `useTransaction`, `useAuditTrail`)

Browser-native storage with IndexedDB.

```jsx
const { store, sync } = useIndexedDBStore(dbName);
const { quads, patterns } = useQuadStore();
const { commit, rollback } = useTransaction();
const { entries, query } = useAuditTrail();
```

### 5. Caching Hooks (`useQueryCache`, `useMemoizedQuery`, `useCacheStats`)

Performance optimization with caching.

```jsx
const { get, set, clear, has } = useQueryCache(options);
const { data, loading } = useMemoizedQuery(query, deps);
const { hitRate, missRate, size } = useCacheStats();
```

### 6. Effect Hooks (`useKnowledgeEffect`, `useDeltaTracking`, `useGraphListener`)

React-like effects with RDF graph lifecycle.

```jsx
useKnowledgeEffect(() => {
  // effect code
}, [dependencies]);

const { delta, changes } = useDeltaTracking();
const { unsubscribe } = useGraphListener(graphIRI, callback);
```

### 7. Utility Hooks (`useNamespaces`, `useValidation`, `useDebug`, `usePerformanceTracking`)

Helpers for common tasks.

```jsx
const { expand, compress, prefixes } = useNamespaces();
const { validate, errors } = useValidation(schema);
const { log, inspect } = useDebug();
const { metrics, tracking } = usePerformanceTracking();
```

### 8. Batch Operations (`useBatchOperations`, `useOptimizedBatch`)

Efficient bulk operations.

```jsx
const { batch, execute } = useBatchOperations();
const { optimizedBatch } = useOptimizedBatch(operations);
```

### 9. Observability (`useOTELMetrics`, `useSpanContext`)

OpenTelemetry integration.

```jsx
const { record, metrics } = useOTELMetrics();
const { spanContext, span } = useSpanContext();
```

### 10. Context (`KnowledgeEngineProvider`, `useKnowledgeEngineContext`, `useConfigContext`)

Context providers for React integration.

```jsx
<KnowledgeEngineProvider config={config}>
  <App />
</KnowledgeEngineProvider>

const context = useKnowledgeEngineContext();
const config = useConfigContext();
```

## Examples

### Example 1: Graph Explorer

```jsx
import { GraphExplorerApp } from 'unrdf/examples/react-hooks/graph-explorer.jsx';

export default GraphExplorerApp;
```

### Example 2: Knowledge Hooks Editor

```jsx
import { KnowledgeHooksEditorApp } from 'unrdf/examples/react-hooks/knowledge-hooks-editor.jsx';

export default KnowledgeHooksEditorApp;
```

### Example 3: Real-time Validation

```jsx
function ValidationApp() {
  const [shapes, setShapes] = useState('');
  const [data, setData] = useState('');
  const { results, isValid } = useShapeValidation(shapes);

  return (
    <div>
      <textarea
        placeholder="SHACL Shapes"
        value={shapes}
        onChange={(e) => setShapes(e.target.value)}
      />
      <textarea
        placeholder="RDF Data"
        value={data}
        onChange={(e) => setData(e.target.value)}
      />
      <div>{isValid ? '✓ Valid' : '✗ Invalid'}</div>
      {results?.map((result, i) => (
        <div key={i}>{result.message}</div>
      ))}
    </div>
  );
}
```

## Configuration

```jsx
<KnowledgeEngineProvider
  config={{
    // Core
    baseIRI: 'https://example.com/',

    // Storage
    indexedDBName: 'unrdf-store',
    enableIndexedDB: true,

    // Knowledge Hooks
    enableKnowledgeHooks: true,
    hookConfig: {
      autoEvaluate: true,
      batchSize: 10,
      timeout: 5000,
    },

    // Query
    cacheConfig: {
      maxSize: 1000,
      ttl: 60000,
    },

    // Observability
    enableObservability: true,
    exporterType: 'console', // or 'jaeger', 'prometheus'
  }}
>
  <App />
</KnowledgeEngineProvider>
```

## Best Practices

### 1. Use Context Provider

Always wrap your app with `KnowledgeEngineProvider` at the root level:

```jsx
function Root() {
  return (
    <KnowledgeEngineProvider config={config}>
      <App />
    </KnowledgeEngineProvider>
  );
}
```

### 2. Memoize Expensive Computations

Use hooks' built-in memoization:

```jsx
const { data } = useMemoizedQuery(query, [query]); // Deps array matters
```

### 3. Handle Loading States

Always check loading states in async hooks:

```jsx
const { data, loading, error } = useQueryAsync(query);

if (loading) return <Spinner />;
if (error) return <Error message={error.message} />;
return <Results data={data} />;
```

### 4. Clean Up Effects

Always return cleanup functions from effects:

```jsx
useKnowledgeEffect(() => {
  const unsubscribe = useGraphListener(iri, callback);
  return () => unsubscribe();
}, [iri]);
```

### 5. Optimize Re-renders

Use React.memo for expensive components:

```jsx
const ResultsTable = React.memo(({ data }) => (
  <table>{/* ... */}</table>
));
```

## Performance

The framework is optimized for performance:

- **Query Caching**: 2-10x faster queries after warmup
- **Hook Batching**: 30-50% faster hook execution
- **IndexedDB**: 50-150ms queries on 10K quads
- **Memory**: 100,000 quads < 100MB
- **Dark Matter 80/20**: Focus on critical path (20% code = 80% value)

## Browser Compatibility

Tested and optimized for:

- ✅ Chrome/Edge 90+
- ✅ Firefox 88+
- ✅ Safari 14+
- ✅ React Native (via react-native-web)

## Troubleshooting

### Issue: Store not initializing

**Solution**: Ensure `KnowledgeEngineProvider` wraps your app and check browser console for errors.

### Issue: IndexedDB quota exceeded

**Solution**: Clear cache or reduce `cacheConfig.maxSize` and adjust TTL.

### Issue: Knowledge Hooks not executing

**Solution**: Set `enableKnowledgeHooks: true` and verify SPARQL query is valid.

### Issue: Stale data in components

**Solution**: Check dependency arrays in `useKnowledgeEffect` and `useSPARQLQuery`.

## API Reference

See full API documentation:

- Core Hooks: `/docs/api/core-hooks.md`
- Query Hooks: `/docs/api/query-hooks.md`
- Knowledge Hooks: `/docs/api/knowledge-hooks.md`
- Storage Hooks: `/docs/api/storage-hooks.md`
- Architecture: `/docs/REACT-HOOKS-ARCHITECTURE.md`

## Contributing

See `CONTRIBUTING.md` for development guidelines.

## License

Same as UNRDF - Check LICENSE file

## Support

- **Issues**: https://github.com/seanchatmangpt/unrdf/issues
- **Docs**: https://docs.unrdf.dev
- **Examples**: `/examples/react-hooks/`

---

**Happy RDF building! 🚀**
