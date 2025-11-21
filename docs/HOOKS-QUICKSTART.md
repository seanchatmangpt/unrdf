# UNRDF React Hooks - Quick Start (80/20 Guide)

> **Master the 20% of hooks that handle 80% of use cases**

## 🎯 The Essential 7 Hooks

Most apps only need these 7 hooks. Start here:

### 1. `useKnowledgeEngine` - Basic CRUD (40% of usage)

```javascript
import { useKnowledgeEngine } from 'unrdf/react-hooks';

function ProductList() {
  const { query, insert, delete: del, data, loading, error } = useKnowledgeEngine();

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
      {data.map(p => (
        <li key={p.product.value}>
          {p.name.value}: ${p.price.value}
        </li>
      ))}
    </ul>
  );
}
```

**Use when:** You need basic query/insert/delete operations (every app)

---

### 2. `useChangeFeed` - Real-time Updates (20% of usage)

```javascript
import { useChangeFeed } from 'unrdf/react-hooks';

function LivePrices() {
  const { changes, start, stop, stats } = useChangeFeed({
    operations: ['insert', 'delete'],
    batchSize: 10
  });

  useEffect(() => {
    start();
    return () => stop();
  }, []);

  return (
    <div>
      <p>Live Updates: {stats.totalChanges}</p>
      {changes.slice(-5).map((change, i) => (
        <div key={i}>
          {change.operation}: {JSON.stringify(change.quads)}
        </div>
      ))}
    </div>
  );
}
```

**Use when:** You need real-time data updates (most interactive apps)

---

### 3. `useDarkMatterCore` - Find Bottlenecks (15% of usage)

```javascript
import { useDarkMatterCore } from 'unrdf/react-hooks';

function PerformanceAnalyzer() {
  const { analysis, criticalPaths, optimizationSuggestions, analyze } = useDarkMatterCore();

  const runAnalysis = async () => {
    await analyze();
  };

  return (
    <div>
      <button onClick={runAnalysis}>Analyze Performance</button>
      {analysis && (
        <>
          <h3>Pareto Score: {analysis.paretoScore}/100</h3>
          <p>Critical Paths: {criticalPaths.length}</p>
          <h4>Top 3 Optimizations:</h4>
          {optimizationSuggestions.slice(0, 3).map((sug, i) => (
            <div key={i}>
              <strong>{sug.type}:</strong> {sug.reason}
              <br />
              <em>Gain: {sug.estimatedGain}</em>
            </div>
          ))}
        </>
      )}
    </div>
  );
}
```

**Use when:** Your app feels slow and you want to identify bottlenecks (optimization phase)

---

### 4. `useQueryAnalyzer` - Optimize Queries (10% of usage)

```javascript
import { useQueryAnalyzer } from 'unrdf/react-hooks';

function QueryOptimizer() {
  const { optimizeQuery, slowQueries, suggestions } = useQueryAnalyzer({
    slowThreshold: 100
  });

  const optimize = async () => {
    const result = await optimizeQuery(`
      SELECT * WHERE {
        ?s ?p ?o .
        OPTIONAL { ?s rdfs:label ?label }
      }
    `);

    console.log('Original:', result.original);
    console.log('Optimized:', result.optimized);
    console.log('Expected gain:', result.estimatedGain);
  };

  return (
    <div>
      <button onClick={optimize}>Optimize Query</button>
      <p>Slow Queries: {slowQueries.length}</p>
      <ul>
        {suggestions.map((sug, i) => (
          <li key={i}>
            <strong>{sug.type}:</strong> {sug.reason}
          </li>
        ))}
      </ul>
    </div>
  );
}
```

**Use when:** You have slow queries that need optimization

---

### 5. `useErrorBoundary` - Error Handling (15% of usage)

```javascript
import { useErrorBoundary } from 'unrdf/react-hooks';

function App() {
  const { hasError, error, resetError, captureError } = useErrorBoundary({
    onError: (error, errorInfo) => {
      console.error('Caught error:', error);
      // Send to error tracking service
    }
  });

  if (hasError) {
    return (
      <div>
        <h2>Something went wrong</h2>
        <p>{error?.message}</p>
        <button onClick={resetError}>Try Again</button>
      </div>
    );
  }

  return <YourApp />;
}
```

**Use when:** You need production-grade error handling (always)

---

### 6. `useGraphDiff` - Version Control (10% of usage)

```javascript
import { useGraphDiff } from 'unrdf/react-hooks';

function VersionControl() {
  const { computeDiff, diff, applyPatch } = useGraphDiff();

  const showDiff = async () => {
    const result = await computeDiff(
      'http://example.org/graph/v1',
      'http://example.org/graph/v2'
    );

    console.log('Added:', result.added.length);
    console.log('Removed:', result.removed.length);
  };

  return (
    <div>
      <button onClick={showDiff}>Compare Versions</button>
      {diff && (
        <div>
          <p>Added: {diff.stats.addedCount}</p>
          <p>Removed: {diff.stats.removedCount}</p>
        </div>
      )}
    </div>
  );
}
```

**Use when:** You need to track changes between graph versions (collaborative editing, auditing)

---

### 7. `useSPARQLEditor` - Query Interface (10% of usage)

```javascript
import { useSPARQLEditor } from 'unrdf/react-hooks';

function QueryEditor() {
  const { query, setQuery, validate, execute, errors } = useSPARQLEditor();

  const runQuery = async () => {
    if (validate(query)) {
      const results = await execute();
      console.log(results);
    }
  };

  return (
    <div>
      <textarea
        value={query}
        onChange={(e) => setQuery(e.target.value)}
        placeholder="Enter SPARQL query..."
        rows={10}
        cols={80}
      />
      <button onClick={runQuery}>Execute</button>
      {errors.map((err, i) => (
        <div key={i} style={{ color: 'red' }}>
          Line {err.line}: {err.message}
        </div>
      ))}
    </div>
  );
}
```

**Use when:** You need a SPARQL query interface (developer tools, admin panels)

---

## 📊 Usage Timeline

### Week 1: Learn the Basics
Focus on: `useKnowledgeEngine` + `useErrorBoundary`

```javascript
import { useKnowledgeEngine, useErrorBoundary } from 'unrdf/react-hooks';

function MyFirstApp() {
  const { hasError, error, resetError } = useErrorBoundary();
  const { query, data, loading } = useKnowledgeEngine();

  useEffect(() => {
    query('SELECT * WHERE { ?s ?p ?o } LIMIT 10');
  }, []);

  if (hasError) return <div>Error: {error?.message}</div>;
  if (loading) return <div>Loading...</div>;

  return (
    <ul>
      {data.map((row, i) => (
        <li key={i}>{JSON.stringify(row)}</li>
      ))}
    </ul>
  );
}
```

### Week 2: Add Real-time
Add: `useChangeFeed`

```javascript
import { useKnowledgeEngine, useChangeFeed } from 'unrdf/react-hooks';

function LiveApp() {
  const { data } = useKnowledgeEngine();
  const { changes, start } = useChangeFeed();

  useEffect(() => {
    start();
  }, []);

  return (
    <div>
      <h2>Static Data: {data.length}</h2>
      <h2>Live Updates: {changes.length}</h2>
    </div>
  );
}
```

### Week 3-4: Optimize
Add: `useDarkMatterCore` + `useQueryAnalyzer`

```javascript
import {
  useKnowledgeEngine,
  useDarkMatterCore,
  useQueryAnalyzer
} from 'unrdf/react-hooks';

function OptimizedApp() {
  const { query } = useKnowledgeEngine();
  const { analysis } = useDarkMatterCore();
  const { optimizeQuery } = useQueryAnalyzer();

  // Use analyzer to optimize queries
  // Use dark matter to find bottlenecks
}
```

### Month 2+: Advanced Features
Add: `useGraphDiff`, `useSPARQLEditor`, and explore Tier 3 hooks

---

## 🎓 Learning Path

1. **Day 1-2:** Master `useKnowledgeEngine` (CRUD operations)
2. **Day 3-4:** Add `useErrorBoundary` (error handling)
3. **Day 5-7:** Implement `useChangeFeed` (real-time updates)
4. **Week 2:** Learn `useDarkMatterCore` (performance analysis)
5. **Week 3:** Add `useQueryAnalyzer` (query optimization)
6. **Week 4:** Explore `useGraphDiff` (version control)
7. **Month 2:** Add `useSPARQLEditor` (query interface)
8. **Month 3+:** Explore advanced hooks as needed

---

## 💡 Common Patterns

### Pattern 1: Basic App (80% of apps)

```javascript
import {
  useKnowledgeEngine,
  useErrorBoundary
} from 'unrdf/react-hooks';

function BasicApp() {
  const { hasError, error } = useErrorBoundary();
  const { query, data, loading } = useKnowledgeEngine();

  // Your app logic
}
```

### Pattern 2: Real-time App (15% of apps)

```javascript
import {
  useKnowledgeEngine,
  useChangeFeed,
  useErrorBoundary
} from 'unrdf/react-hooks';

function RealTimeApp() {
  const { hasError, error } = useErrorBoundary();
  const { query, data } = useKnowledgeEngine();
  const { changes, start } = useChangeFeed();

  // Your real-time logic
}
```

### Pattern 3: Optimized App (4% of apps)

```javascript
import {
  useKnowledgeEngine,
  useChangeFeed,
  useDarkMatterCore,
  useQueryAnalyzer,
  useErrorBoundary
} from 'unrdf/react-hooks';

function OptimizedApp() {
  // All 5 essential hooks + optimization
}
```

### Pattern 4: Full-Featured App (1% of apps)

```javascript
import {
  useKnowledgeEngine,
  useChangeFeed,
  useDarkMatterCore,
  useQueryAnalyzer,
  useErrorBoundary,
  useGraphDiff,
  useSPARQLEditor
} from 'unrdf/react-hooks';

function FullFeaturedApp() {
  // All 7 core hooks + advanced features
}
```

---

## 🚀 Next Steps

**Mastered the 7 core hooks?** Explore Tier 3 (standard) hooks:

```javascript
import {
  useFederatedSystem,    // Distributed queries
  useStreamProcessor,    // Windowing operations
  useOptimizer,          // Auto-optimization
  useSemanticAnalyzer,   // Semantic analysis
  useGraphMerge,         // Graph merging
  usePolicyPack,         // SHACL validation
  useRecovery,           // Retry logic
  useGraphVisualizer,    // Visualization
  useResultsPaginator    // Pagination
} from 'unrdf/react-hooks';
```

**Need advanced features?** Import by category:

```javascript
import { useConsensusManager } from 'unrdf/react-hooks/federation';
import { useEmbeddingsManager } from 'unrdf/react-hooks/ai-semantic';
```

---

## 📚 Full Documentation

- **Complete Guide:** [REACT-HOOKS-GUIDE.md](./REACT-HOOKS-GUIDE.md)
- **80/20 Analysis:** [HOOKS-80-20-ANALYSIS.md](./HOOKS-80-20-ANALYSIS.md)
- **API Reference:** See individual hook files

**Remember:** 80% of apps only need the 7 core hooks. Start simple, add complexity as needed!
