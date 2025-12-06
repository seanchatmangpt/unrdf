# Explanation: UNRDF React Architecture

**Purpose:** Understand the design principles and architecture of UNRDF React.

---

## Overview

UNRDF React is a comprehensive React hooks library for building RDF-powered applications. It bridges the gap between semantic web technologies and modern React development.

```
┌─────────────────────────────────────────────────────────────┐
│                    React Application                         │
├─────────────────────────────────────────────────────────────┤
│                                                              │
│  ┌──────────────┐  ┌──────────────┐  ┌──────────────┐     │
│  │   Core       │  │  Streaming   │  │  Federation  │     │
│  │   Hooks      │  │   Hooks      │  │    Hooks     │     │
│  └──────┬───────┘  └──────┬───────┘  └──────┬───────┘     │
│         │                  │                  │              │
│         └──────────────────┴──────────────────┘              │
│                            │                                 │
│                   ┌────────▼────────┐                       │
│                   │  Knowledge      │                       │
│                   │  Engine Core    │                       │
│                   └────────┬────────┘                       │
│                            │                                 │
│                   ┌────────▼────────┐                       │
│                   │  @unrdf/        │                       │
│                   │  oxigraph       │                       │
│                   └─────────────────┘                       │
└─────────────────────────────────────────────────────────────┘
```

---

## Core Design Principles

### 1. **Progressive Enhancement**

UNRDF React follows the 80/20 principle - commonly used features are easily accessible, advanced features require explicit imports.

**Tier 1 (5 hooks)** - 60% of use cases:
- `useKnowledgeEngine` - Core engine access
- `useChangeFeed` - Real-time updates
- `useDarkMatterCore` - Query optimization
- `useQueryAnalyzer` - Performance insights
- `useErrorBoundary` - Error handling

**Tier 2-4** - Specialized imports for advanced needs.

```jsx
// ✅ Common case - top-level import
import { useKnowledgeEngine, useChangeFeed } from 'unrdf-react';

// ✅ Advanced case - category import
import { useFederatedQuery } from 'unrdf-react/federation';
```

---

### 2. **Declarative Data Fetching**

React hooks provide declarative access to RDF data, similar to GraphQL clients like Apollo or React Query.

```jsx
// Declarative - React automatically manages lifecycle
const { data, isLoading, error } = useTriples({
  predicate: 'foaf:name'
});

// vs Imperative (manual management)
const [data, setData] = useState([]);
useEffect(() => {
  engine.query('...').then(setData);
}, []);
```

**Benefits:**
- Automatic loading/error states
- Built-in caching
- Deduplication of identical queries
- Lifecycle management

---

### 3. **Reactive by Default**

Changes to the knowledge graph automatically trigger React re-renders.

```jsx
function PeopleList() {
  // Automatically re-renders when people are added/removed
  const { data: people } = useTriples({
    predicate: 'foaf:name'
  });

  return <ul>{people?.map(...)}</ul>;
}
```

**Implementation:**
- `useChangeFeed` subscribes to graph mutations
- Hooks register subscriptions with the engine
- Engine notifies subscribed hooks on changes
- Hooks trigger React state updates

---

### 4. **Context-Based Architecture**

The `KnowledgeEngineProvider` uses React Context to share the engine instance across components.

```jsx
<KnowledgeEngineProvider>
  <App />  {/* All children can access engine via hooks */}
</KnowledgeEngineProvider>
```

**Why Context?**
- Single engine instance per application
- No prop drilling
- Automatic cleanup on unmount
- Supports nested providers for multiple graphs

---

## Layer Architecture

### Layer 1: Data Factory & Store

**Responsibility:** Low-level RDF operations

```
@unrdf/oxigraph
├── createStore()      - RDF quad store
├── dataFactory        - Term creation (NamedNode, Literal, etc.)
└── SPARQL engine      - Query execution
```

**Not exposed directly to React components** - accessed via Knowledge Engine.

---

### Layer 2: Knowledge Engine Core

**Responsibility:** Business logic and graph operations

```
@unrdf/core (KnowledgeEngine)
├── addTriples()       - Insert RDF data
├── deleteTriples()    - Remove RDF data
├── query()            - SPARQL execution
├── subscribe()        - Change notifications
└── hooks system       - Knowledge Hooks (validation, transforms)
```

**Singleton per application** - managed by KnowledgeEngineProvider.

---

### Layer 3: React Hooks (Adapters)

**Responsibility:** React integration and state management

```
unrdf-react/src/
├── core/              - useKnowledgeEngine, useTriples, useStore
├── query/             - useSPARQLQuery, useSPARQLConstruct
├── streaming/         - useChangeFeed, useSubscription
├── federation/        - useFederatedQuery, useConsensus
├── storage/           - useStorage, useIndexedDB
└── context/           - KnowledgeEngineProvider
```

Each hook category maps to a specific concern.

---

### Layer 4: Components (Visualizations)

**Responsibility:** UI rendering

```
unrdf-react/components/
├── KnowledgeGraph          - Cytoscape visualization
├── PerformanceMetricsChart - Chart.js metrics
├── GeospatialVisualization - deck.gl maps
└── ...
```

Components consume hooks and render data.

---

## Data Flow

### Query Execution Flow

```
1. Component renders
   useTriples({ predicate: 'foaf:name' })
        │
        ▼
2. Hook calls engine.query(SPARQL)
        │
        ▼
3. Engine executes via Oxigraph
        │
        ▼
4. Results returned to hook
        │
        ▼
5. Hook updates React state
        │
        ▼
6. Component re-renders with data
```

---

### Mutation Flow

```
1. User action (e.g., button click)
   engine.addTriples([...])
        │
        ▼
2. Engine writes to Oxigraph store
        │
        ▼
3. Engine notifies subscribers
        │
        ▼
4. useChangeFeed hooks receive notification
        │
        ▼
5. Hooks trigger React re-renders
        │
        ▼
6. UI updates automatically
```

---

## State Management Strategy

UNRDF React uses **server state management** (similar to React Query, SWR):

| State Type | Managed By | Example |
|------------|-----------|---------|
| **Server state** | UNRDF Hooks | RDF triples, query results |
| **Client state** | React useState | UI toggles, form inputs |
| **Derived state** | useMemo | Filtered/transformed data |

**Why not global state (Redux, Zustand)?**
- RDF store is the source of truth
- Hooks provide reactive access
- No need for manual synchronization

```jsx
// ❌ Don't store RDF data in React state
const [people, setPeople] = useState([]);

// ✅ Query directly via hooks
const { data: people } = useTriples({ predicate: 'foaf:name' });
```

---

## Caching Strategy

### Query Caching

Queries are cached by pattern/SPARQL hash:

```jsx
// First call - executes query
useTriples({ predicate: 'foaf:name' });

// Second call (same pattern) - returns cached result
useTriples({ predicate: 'foaf:name' });
```

**Cache invalidation:**
- Automatic when related triples change
- Manual via `refetch()`
- Time-based with `cacheTime` option

---

### Subscription Management

```
Component A: useTriples({ predicate: 'foaf:name' })
Component B: useTriples({ predicate: 'foaf:name' })
                    │
                    ▼
        Single subscription to engine
                    │
                    ▼
        Both components receive updates
```

Identical subscriptions are **deduplicated** for performance.

---

## Performance Optimizations

### 1. **Lazy Loading**

Components and hooks are tree-shakeable:

```jsx
// Only imports what you use
import { useKnowledgeEngine } from 'unrdf-react';

// Advanced features are separate
import { useFederatedQuery } from 'unrdf-react/federation';
```

---

### 2. **Memoization**

Results are memoized to prevent unnecessary re-renders:

```jsx
// Same reference returned until data changes
const { data } = useTriples(pattern);
```

---

### 3. **Batching**

Multiple triple additions are batched:

```jsx
// Single transaction
await engine.addTriples([
  triple1,
  triple2,
  triple3,
]);
```

---

### 4. **Pagination**

Large result sets support pagination:

```jsx
const { data, loadMore } = useTriplesPaginated(pattern, {
  pageSize: 100
});
```

---

## Extensibility Points

### 1. **Custom Hooks**

Build your own hooks using primitives:

```jsx
function usePersonCount() {
  const { data } = useSPARQLQuery(`
    SELECT (COUNT(?person) as ?count) WHERE {
      ?person a foaf:Person
    }
  `);

  return data?.[0]?.count || 0;
}
```

---

### 2. **Knowledge Hooks Integration**

React hooks can trigger Knowledge Hooks:

```jsx
const { engine } = useKnowledgeEngine();

await engine.addTriples([...], {
  hooks: {
    before: [(triple) => validateTriple(triple)],
    after: [(triple) => indexTriple(triple)]
  }
});
```

---

### 3. **Custom Components**

Build visualizations using hooks:

```jsx
function CustomVisualization() {
  const { data } = useTriples(pattern);

  return (
    <svg>
      {data?.map(triple => <Node key={triple.subject} />)}
    </svg>
  );
}
```

---

## Security Considerations

### 1. **No Direct Store Access**

Components can't bypass the engine:

```jsx
// ❌ Not possible - store is private
const store = useStore();
store.add(...); // Bypasses hooks and validation

// ✅ Must go through engine
const { engine } = useKnowledgeEngine();
await engine.addTriples(...); // Validated and audited
```

---

### 2. **SPARQL Injection Prevention**

Use parameterized queries:

```jsx
// ❌ Vulnerable to injection
useSPARQLQuery(`SELECT * WHERE { ?s foaf:name "${userInput}" }`);

// ✅ Safe - uses variables
useSPARQLQuery(
  `SELECT * WHERE { ?s foaf:name $name }`,
  { variables: { name: userInput } }
);
```

---

### 3. **Policy Enforcement**

Security hooks validate operations:

```jsx
import { useSecurityPolicy } from 'unrdf-react/policy-security';

function SecureEditor() {
  const { validateOperation } = useSecurityPolicy({
    allowedPredicates: ['foaf:name', 'foaf:email'],
  });

  // Operations are validated before execution
}
```

---

## Comparison to Other Approaches

| Approach | UNRDF React | GraphQL | REST |
|----------|------------|---------|------|
| **Query Language** | SPARQL | GraphQL | N/A |
| **Type System** | RDF Schema / OWL | GraphQL Schema | OpenAPI |
| **Caching** | Built-in | Apollo Client | Manual |
| **Real-time** | useChangeFeed | Subscriptions | WebSockets |
| **Offline** | useStorage | Apollo Cache | Service Workers |
| **Linking** | Native (URIs) | Manual | HATEOAS |

**Key advantage:** RDF provides **semantic linking** and **schema flexibility** without code generation.

---

## Next.js Integration

UNRDF React works seamlessly with Next.js:

```jsx
// app/layout.jsx
import { KnowledgeEngineProvider } from 'unrdf-react';

export default function RootLayout({ children }) {
  return (
    <html>
      <body>
        <KnowledgeEngineProvider backend="indexeddb">
          {children}
        </KnowledgeEngineProvider>
      </body>
    </html>
  );
}

// app/page.jsx (Server or Client Component)
'use client'; // Required for hooks

import { useTriples } from 'unrdf-react';

export default function Page() {
  const { data } = useTriples({ predicate: 'foaf:name' });
  return <ul>{data?.map(...)}</ul>;
}
```

**Server-Side Rendering (SSR):**
- Engine initialization happens client-side
- Use `initialData` prop for hydration
- Or fetch data in Server Components and pass as props

---

## Design Decisions

### Why Hooks?

**Reasons:**
- Composability - combine hooks for complex behavior
- Reusability - share logic across components
- Type safety - TypeScript support
- React ecosystem alignment

**Alternative considered:** Render props, HOCs (rejected for complexity)

---

### Why React Context?

**Reasons:**
- Single source of truth
- No prop drilling
- Easy testing (wrap in provider)
- Aligns with React best practices

**Alternative considered:** Singleton module (rejected for testability)

---

### Why Oxigraph?

**Reasons:**
- WASM-based (runs in browser)
- Full SPARQL 1.1 support
- High performance
- Active maintenance

**Alternative considered:** N3.js (limited SPARQL), Quadstore (complex setup)

---

## Related

- [Tutorial: Getting Started](../tutorials/01-getting-started.md)
- [Reference: Core Hooks](../reference/core-hooks.md)
- [Explanation: Streaming Architecture](./streaming-architecture.md)
