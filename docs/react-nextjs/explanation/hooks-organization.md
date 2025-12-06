# Explanation: Hooks Organization & Categories

**Purpose:** Understand how UNRDF React's 101 hooks are organized and when to use each category.

---

## Overview

UNRDF React provides **101 React hooks** organized into **19 categories**. This document explains the organization strategy and helps you find the right hook for your use case.

---

## Category Breakdown

### Core (7 hooks) - Essential Operations

**Purpose:** Fundamental access to the RDF knowledge engine.

**When to use:** Every UNRDF React application uses at least one core hook.

| Hook | Purpose | Tier |
|------|---------|------|
| `useKnowledgeEngine` | Access engine instance | Tier 1 (60%) |
| `useStore` | Direct store access | Tier 3 |
| `useTriples` | Query triples by pattern | Tier 1 |
| `useGraphs` | List named graphs | Tier 3 |
| `useTerms` | Get distinct terms | Tier 3 |
| `useDataFactory` | Create RDF terms | Tier 3 |
| `useQuads` | Query quads with graphs | Tier 4 |

**Example:**
```jsx
import { useKnowledgeEngine, useTriples } from 'unrdf-react';

function PeopleList() {
  const { ready } = useKnowledgeEngine();
  const { data: people } = useTriples({
    predicate: 'http://xmlns.com/foaf/0.1/name'
  });

  return ready ? <ul>{people?.map(...)}</ul> : <div>Loading...</div>;
}
```

---

### HTF - Hyper-Thesis Framework (12 hooks)

**Purpose:** Mathematical operators for knowledge graph transformations.

**When to use:** Advanced semantic operations, inference, transformations.

**Operators:**
- **Γ (Gamma):** Graph composition and merging
- **Λ (Lambda):** Logical inference and reasoning
- **Π (Pi):** Projection and filtering
- **τ (Tau):** Temporal operations
- **Entropy:** Information-theoretic measures

| Hook | Purpose |
|------|---------|
| `useGammaOperator` | Graph composition |
| `useLambdaOperator` | Logical inference |
| `usePiOperator` | Projection/filtering |
| `useTauOperator` | Temporal operations |
| `useEntropyCalculation` | Information entropy |
| `useHyperThesisBuilder` | Construct hypotheses |
| ...and 6 more |

**Example:**
```jsx
import { useGammaOperator } from 'unrdf-react/htf';

function GraphMerger({ graph1, graph2 }) {
  const { compose } = useGammaOperator();

  const merged = compose(graph1, graph2, {
    conflictResolution: 'union'
  });

  return <KnowledgeGraph triples={merged} />;
}
```

---

### AI-Semantic (9 hooks)

**Purpose:** AI and machine learning integration for RDF data.

**When to use:** Semantic search, embeddings, NLP, anomaly detection.

| Hook | Purpose |
|------|---------|
| `useEmbeddings` | Generate vector embeddings |
| `useSemanticSearch` | Vector similarity search |
| `useNLPExtraction` | Extract entities from text |
| `useSemanticAnalysis` | Analyze semantic relationships |
| `useAnomalyDetection` | Detect graph anomalies |
| `useClusterDetection` | Find communities/clusters |
| `useEntityLinker` | Link entities across graphs |
| `useOntologyAlignment` | Align schemas |
| `useInferenceEngine` | Run inference rules |

**Example:**
```jsx
import { useSemanticSearch } from 'unrdf-react/ai-semantic';

function SearchInterface() {
  const { search, results } = useSemanticSearch({
    embeddingModel: 'all-MiniLM-L6-v2'
  });

  return (
    <div>
      <input onChange={(e) => search(e.target.value)} />
      <Results data={results} />
    </div>
  );
}
```

---

### Advanced-Utility (7 hooks)

**Purpose:** Advanced graph operations and analysis.

**When to use:** Graph comparison, quality assessment, structural analysis.

| Hook | Purpose |
|------|---------|
| `useGraphDiff` | Compare two graphs | Tier 2 (20%) |
| `useGraphMerge` | Merge graphs intelligently |
| `useIsomorphism` | Test graph equivalence |
| `useQualityMetrics` | Assess data quality |
| `useStructuralAnalysis` | Analyze graph structure |
| `usePathFinding` | Find paths between nodes |
| `useSubgraphExtraction` | Extract subgraphs |

**Example:**
```jsx
import { useGraphDiff } from 'unrdf-react/advanced-utility';

function VersionComparison({ v1, v2 }) {
  const { added, deleted, modified } = useGraphDiff(v1, v2);

  return (
    <div>
      <p>Added: {added.length} triples</p>
      <p>Deleted: {deleted.length} triples</p>
      <p>Modified: {modified.length} triples</p>
    </div>
  );
}
```

---

### Federation (6 hooks)

**Purpose:** Distributed RDF operations across multiple sources.

**When to use:** Multi-source queries, consensus protocols, distributed graphs.

| Hook | Purpose |
|------|---------|
| `useFederatedQuery` | Query multiple endpoints |
| `useConsensus` | Achieve distributed consensus |
| `useReplication` | Replicate across nodes |
| `useDistributedCache` | Shared caching |
| `usePeerDiscovery` | Find federation peers |
| `useSharding` | Partition large graphs |

**Example:**
```jsx
import { useFederatedQuery } from 'unrdf-react/federation';

function FederatedSearch() {
  const { data } = useFederatedQuery({
    endpoints: [
      'http://dbpedia.org/sparql',
      'http://wikidata.org/sparql'
    ],
    query: 'SELECT ?person WHERE { ?person a foaf:Person }'
  });

  return <Results data={data} />;
}
```

---

### Streaming (6 hooks)

**Purpose:** Real-time updates and change feeds.

**When to use:** Collaborative apps, live dashboards, reactive UIs.

| Hook | Purpose | Tier |
|------|---------|------|
| `useChangeFeed` | Subscribe to changes | Tier 1 (60%) |
| `useSubscription` | Pattern-based subscriptions |
| `useStreamProcessor` | Process change streams |
| `useRealtimeQuery` | Auto-refreshing queries |
| `useDeltaTracking` | Track deltas for undo/redo |
| `usePresence` | User presence tracking |

**Example:**
```jsx
import { useChangeFeed } from 'unrdf-react';

function LiveFeed() {
  const changes = useChangeFeed({
    filter: (change) => change.type === 'add',
    onUpdate: (change) => console.log('New triple:', change)
  });

  return <ChangeList changes={changes} />;
}
```

---

### Form-UI (6 hooks)

**Purpose:** UI components for RDF interaction.

**When to use:** Query builders, SPARQL editors, graph visualizers.

| Hook | Purpose | Tier |
|------|---------|------|
| `useQueryBuilder` | Visual SPARQL builder |
| `useSPARQLEditor` | Code editor with autocomplete | Tier 2 |
| `useVisualizer` | Graph visualization state |
| `usePaginator` | Pagination controls |
| `useFilterPanel` | Dynamic filters |
| `useFormGenerator` | Generate forms from RDF |

**Example:**
```jsx
import { useSPARQLEditor } from 'unrdf-react/form-ui';

function QueryEditor() {
  const { query, setQuery, execute, results } = useSPARQLEditor({
    autocomplete: true,
    syntax Highlighting: true
  });

  return (
    <div>
      <textarea value={query} onChange={e => setQuery(e.target.value)} />
      <button onClick={execute}>Run Query</button>
      <Results data={results} />
    </div>
  );
}
```

---

### Dark-Matter (5 hooks)

**Purpose:** Query optimization and performance analysis.

**When to use:** Slow queries, performance tuning, query planning.

| Hook | Purpose | Tier |
|------|---------|------|
| `useDarkMatterCore` | Core optimizer | Tier 1 (60%) |
| `useQueryAnalyzer` | Analyze query performance | Tier 1 |
| `useQueryOptimizer` | Optimize SPARQL queries |
| `useCriticalPath` | Identify bottlenecks |
| `useIndexSuggestions` | Suggest indexes |

**Example:**
```jsx
import { useQueryAnalyzer } from 'unrdf-react/dark-matter';

function PerformanceDashboard() {
  const { analyze } = useQueryAnalyzer();

  const stats = analyze(`
    SELECT ?s ?p ?o WHERE { ?s ?p ?o }
  `);

  return (
    <div>
      <p>Estimated time: {stats.estimatedTime}ms</p>
      <p>Bottlenecks: {stats.bottlenecks.join(', ')}</p>
    </div>
  );
}
```

---

### Query (5 hooks)

**Purpose:** SPARQL query execution.

**When to use:** SELECT, CONSTRUCT, ASK, DESCRIBE queries.

| Hook | Purpose |
|------|---------|
| `useSPARQLQuery` | Execute SELECT queries |
| `useSPARQLConstruct` | Execute CONSTRUCT |
| `useSPARQLAsk` | Execute ASK |
| `useSPARQLDescribe` | Execute DESCRIBE |
| `useParameterizedQuery` | Queries with variables |

**Example:**
```jsx
import { useSPARQLQuery } from 'unrdf-react';

function PeopleList() {
  const { data } = useSPARQLQuery(`
    PREFIX foaf: <http://xmlns.com/foaf/0.1/>
    SELECT ?name WHERE {
      ?person a foaf:Person .
      ?person foaf:name ?name
    }
  `);

  return <ul>{data?.map(row => <li>{row.name}</li>)}</ul>;
}
```

---

### Knowledge-Hooks (4 hooks)

**Purpose:** Execute and manage Knowledge Hooks.

**When to use:** Validation, transformation, governance.

| Hook | Purpose |
|------|---------|
| `useKnowledgeHooks` | Register/execute hooks |
| `useHookExecution` | Control hook execution |
| `useHookRegistry` | Manage hook registry |
| `useHookMetrics` | Monitor hook performance |

---

### Context (4 hooks)

**Purpose:** React context providers.

**When to use:** Provide engine, config, theme to components.

| Hook | Purpose |
|------|---------|
| `useEngineContext` | Access engine context |
| `useConfigContext` | Access configuration |
| `useThemeContext` | Access UI theme |
| `useI18nContext` | Access internationalization |

---

### Error-Recovery (4 hooks)

**Purpose:** Error handling and recovery.

**When to use:** Production error handling, retries, logging.

| Hook | Purpose | Tier |
|------|---------|------|
| `useErrorBoundary` | React error boundary | Tier 1 (60%) |
| `useErrorReporting` | Send errors to service |
| `useRetry` | Retry failed operations |
| `useRecovery` | Automatic recovery |

---

### Policy-Security (4 hooks)

**Purpose:** Security and access control.

**When to use:** Multi-tenant apps, access control, validation.

| Hook | Purpose |
|------|---------|
| `useSecurityPolicy` | Enforce security rules |
| `useAccessControl` | Role-based access |
| `useValidation` | Schema validation |
| `useAuditLog` | Track operations |

---

### Storage (4 hooks)

**Purpose:** Persistence and caching.

**When to use:** Offline support, data persistence.

| Hook | Purpose |
|------|---------|
| `useStorage` | IndexedDB persistence |
| `useIndexedDB` | Direct IndexedDB access |
| `useTransactions` | Transactional updates |
| `useAuditTrail` | Track history |

---

### Utils (4 hooks)

**Purpose:** Utility functions.

**When to use:** Common operations, helpers.

| Hook | Purpose |
|------|---------|
| `useDebounce` | Debounce values |
| `useThrottle` | Throttle callbacks |
| `useAsync` | Async state management |
| `usePrevious` | Track previous value |

---

### Effects (3 hooks)

**Purpose:** Side effects management.

**When to use:** Listeners, external integrations.

| Hook | Purpose |
|------|---------|
| `useListener` | Event listeners |
| `useObserver` | Observable patterns |
| `useSideEffect` | Managed side effects |

---

### Composition (3 hooks)

**Purpose:** Combine multiple hooks.

**When to use:** Complex workflows, reusable patterns.

| Hook | Purpose |
|------|---------|
| `useComposedQuery` | Combine multiple queries |
| `usePipeline` | Hook pipelines |
| `useWorkflow` | Multi-step workflows |

---

### Cache (3 hooks)

**Purpose:** Caching and memoization.

**When to use:** Performance optimization.

| Hook | Purpose |
|------|---------|
| `useQueryCache` | Cache query results |
| `useMemoizedTriples` | Memoize triple patterns |
| `useCacheInvalidation` | Invalidate caches |

---

### Batch (2 hooks)

**Purpose:** Batch operations.

**When to use:** Bulk inserts, bulk deletes.

| Hook | Purpose |
|------|---------|
| `useBatchInsert` | Batch triple insertion |
| `useBatchDelete` | Batch triple deletion |

---

## Import Strategy

### Tier 1 Hooks (Top-Level Import)

**60% of use cases** - Essential hooks exported from `unrdf-react`:

```jsx
import {
  useKnowledgeEngine,
  useChangeFeed,
  useDarkMatterCore,
  useQueryAnalyzer,
  useErrorBoundary
} from 'unrdf-react';
```

---

### Tier 2 Hooks (Category Import)

**20% of use cases** - Important but specialized:

```jsx
import { useGraphDiff } from 'unrdf-react/advanced-utility';
import { useSPARQLEditor } from 'unrdf-react/form-ui';
```

---

### Tier 3 & 4 (Explicit Category Import)

**20% of use cases** - Advanced features:

```jsx
import { useFederatedQuery } from 'unrdf-react/federation';
import { useGammaOperator } from 'unrdf-react/htf';
```

---

## Hook Selection Guide

**Use this decision tree to find the right hook:**

1. **Do you need basic RDF operations?** → Core hooks
2. **Real-time updates?** → Streaming hooks
3. **Query optimization?** → Dark-Matter hooks
4. **AI/ML features?** → AI-Semantic hooks
5. **Distributed graphs?** → Federation hooks
6. **Advanced math operators?** → HTF hooks
7. **UI components?** → Form-UI hooks
8. **Error handling?** → Error-Recovery hooks
9. **Access control?** → Policy-Security hooks
10. **Offline support?** → Storage hooks

---

## Related

- [Reference: Core Hooks](../reference/core-hooks.md)
- [Reference: Streaming Hooks](../reference/streaming-hooks.md)
- [Explanation: Architecture](./architecture.md)
