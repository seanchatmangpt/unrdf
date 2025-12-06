# Reference: Core Hooks API

Complete API reference for core UNRDF React hooks.

**Package:** `unrdf-react`

---

## useKnowledgeEngine

Access the RDF knowledge engine instance.

### Signature

```typescript
function useKnowledgeEngine(options?: EngineOptions): EngineContext
```

### Parameters

| Name | Type | Default | Description |
|------|------|---------|-------------|
| `options.backend` | `'memory' \| 'indexeddb'` | `'memory'` | Storage backend |
| `options.named Graph` | `string` | `undefined` | Default named graph URI |

### Returns

```typescript
interface EngineContext {
  engine: KnowledgeEngine | null;
  ready: boolean;
  error: Error | null;
  addTriples: (triples: Triple[]) => Promise<void>;
  deleteTriples: (triples: Triple[]) => Promise<void>;
  query: (sparql: string) => Promise<any>;
}
```

| Property | Type | Description |
|----------|------|-------------|
| `engine` | `KnowledgeEngine \| null` | The engine instance (null until ready) |
| `ready` | `boolean` | True when engine is initialized |
| `error` | `Error \| null` | Initialization error if any |
| `addTriples` | `Function` | Add triples to the graph |
| `deleteTriples` | `Function` | Remove triples from the graph |
| `query` | `Function` | Execute SPARQL queries |

### Example

```jsx
import { useKnowledgeEngine } from 'unrdf-react';

function MyComponent() {
  const { engine, ready, addTriples } = useKnowledgeEngine();

  const addPerson = async () => {
    if (!ready) return;

    await addTriples([
      {
        subject: 'http://example.org/alice',
        predicate: 'http://xmlns.com/foaf/0.1/name',
        object: 'Alice'
      }
    ]);
  };

  return ready ? <button onClick={addPerson}>Add Person</button> : <div>Loading...</div>;
}
```

---

## useTriples

Query and subscribe to triples in the knowledge graph.

### Signature

```typescript
function useTriples(pattern?: TriplePattern, options?: QueryOptions): QueryResult<Triple[]>
```

### Parameters

| Name | Type | Default | Description |
|------|------|---------|-------------|
| `pattern.subject` | `string \| undefined` | `undefined` | Filter by subject URI |
| `pattern.predicate` | `string \| undefined` | `undefined` | Filter by predicate URI |
| `pattern.object` | `string \| undefined` | `undefined` | Filter by object value |
| `pattern.graph` | `string \| undefined` | `undefined` | Filter by named graph |
| `options.limit` | `number \| undefined` | `undefined` | Maximum results |
| `options.offset` | `number \| undefined` | `0` | Skip first N results |
| `options.enabled` | `boolean` | `true` | Enable/disable query |

### Returns

```typescript
interface QueryResult<T> {
  data: T | undefined;
  isLoading: boolean;
  error: Error | null;
  refetch: () => void;
}
```

| Property | Type | Description |
|----------|------|-------------|
| `data` | `Triple[] \| undefined` | Array of matching triples |
| `isLoading` | `boolean` | True while query is executing |
| `error` | `Error \| null` | Query error if any |
| `refetch` | `Function` | Manually re-execute query |

### Example

```jsx
import { useTriples } from 'unrdf-react';

function PeopleList() {
  const { data: people, isLoading } = useTriples({
    predicate: 'http://xmlns.com/foaf/0.1/name'
  });

  if (isLoading) return <div>Loading...</div>;

  return (
    <ul>
      {people?.map((triple, idx) => (
        <li key={idx}>{triple.object}</li>
      ))}
    </ul>
  );
}
```

---

## useGraphs

Query available named graphs.

### Signature

```typescript
function useGraphs(options?: QueryOptions): QueryResult<string[]>
```

### Returns

```typescript
{
  data: string[] | undefined;  // Array of graph URIs
  isLoading: boolean;
  error: Error | null;
  refetch: () => void;
}
```

### Example

```jsx
import { useGraphs } from 'unrdf-react';

function GraphSelector() {
  const { data: graphs } = useGraphs();

  return (
    <select>
      {graphs?.map(graph => (
        <option key={graph} value={graph}>{graph}</option>
      ))}
    </select>
  );
}
```

---

## useTerms

Query distinct subjects, predicates, or objects.

### Signature

```typescript
function useTerms(
  position: 'subject' | 'predicate' | 'object',
  filter?: TriplePattern,
  options?: QueryOptions
): QueryResult<string[]>
```

### Parameters

| Name | Type | Description |
|------|------|-------------|
| `position` | `'subject' \| 'predicate' \| 'object'` | Which term position to query |
| `filter` | `TriplePattern \| undefined` | Additional filters |
| `options` | `QueryOptions \| undefined` | Query options |

### Example

```jsx
import { useTerms } from 'unrdf-react';

function PredicateList() {
  const { data: predicates } = useTerms('predicate');

  return (
    <ul>
      {predicates?.map(pred => (
        <li key={pred}>{pred.split('/').pop()}</li>
      ))}
    </ul>
  );
}
```

---

## useStore

Direct access to the underlying RDF store.

### Signature

```typescript
function useStore(): {
  store: Store | null;
  ready: boolean;
}
```

### Returns

| Property | Type | Description |
|----------|------|-------------|
| `store` | `Store \| null` | Oxigraph store instance |
| `ready` | `boolean` | True when store is initialized |

### Example

```jsx
import { useStore } from 'unrdf-react';

function StoreInfo() {
  const { store, ready } = useStore();

  if (!ready) return <div>Loading...</div>;

  return <div>Store backend: {store.backend}</div>;
}
```

---

## useDataFactory

Access RDF data factory for creating terms.

### Signature

```typescript
function useDataFactory(): DataFactory
```

### Returns

```typescript
interface DataFactory {
  namedNode: (uri: string) => NamedNode;
  blankNode: (id?: string) => BlankNode;
  literal: (value: string, languageOrDatatype?: string | NamedNode) => Literal;
  quad: (subject: Term, predicate: Term, object: Term, graph?: Term) => Quad;
}
```

### Example

```jsx
import { useDataFactory, useKnowledgeEngine } from 'unrdf-react';

function AddPerson() {
  const df = useDataFactory();
  const { addTriples, ready } = useKnowledgeEngine();

  const addAlice = async () => {
    const alice = df.namedNode('http://example.org/alice');
    const foafName = df.namedNode('http://xmlns.com/foaf/0.1/name');
    const name = df.literal('Alice');

    await addTriples([
      {
        subject: alice.value,
        predicate: foafName.value,
        object: name.value
      }
    ]);
  };

  return <button onClick={addAlice} disabled={!ready}>Add Alice</button>;
}
```

---

## Type Definitions

### Triple

```typescript
interface Triple {
  subject: string;      // URI or blank node
  predicate: string;    // URI
  object: string;       // URI, blank node, or literal value
  graph?: string;       // Optional named graph URI
}
```

### TriplePattern

```typescript
interface TriplePattern {
  subject?: string;
  predicate?: string;
  object?: string;
  graph?: string;
}
```

### QueryOptions

```typescript
interface QueryOptions {
  limit?: number;
  offset?: number;
  enabled?: boolean;
  refetchInterval?: number;
  refetchOnFocus?: boolean;
  refetchOnReconnect?: boolean;
  cacheTime?: number;
}
```

---

## Provider Configuration

All core hooks require the `KnowledgeEngineProvider`:

```jsx
import { KnowledgeEngineProvider } from 'unrdf-react';

function App() {
  return (
    <KnowledgeEngineProvider
      backend="indexeddb"  // 'memory' or 'indexeddb'
      storeName="my-app"
      onReady={() => console.log('Engine ready!')}
      onError={(error) => console.error('Engine error:', error)}
    >
      <YourApp />
    </KnowledgeEngineProvider>
  );
}
```

### Provider Props

| Prop | Type | Default | Description |
|------|------|---------|-------------|
| `backend` | `'memory' \| 'indexeddb'` | `'memory'` | Storage backend |
| `storeName` | `string` | `'unrdf-default'` | IndexedDB store name |
| `onReady` | `() => void` | `undefined` | Callback when engine is ready |
| `onError` | `(error: Error) => void` | `undefined` | Callback on errors |

---

## Best Practices

1. **Always check `ready` before operations:**
   ```jsx
   const { engine, ready } = useKnowledgeEngine();
   if (!ready) return <div>Loading...</div>;
   ```

2. **Use triple patterns for efficient queries:**
   ```jsx
   // ✅ Good - specific query
   useTriples({ predicate: 'http://xmlns.com/foaf/0.1/name' })

   // ❌ Bad - fetches all triples
   useTriples()
   ```

3. **Enable/disable queries conditionally:**
   ```jsx
   useTriples(pattern, { enabled: isActive })
   ```

4. **Handle loading and error states:**
   ```jsx
   const { data, isLoading, error } = useTriples(pattern);
   if (isLoading) return <Spinner />;
   if (error) return <Error message={error.message} />;
   ```

---

## Related

- [Tutorial: Getting Started](../tutorials/01-getting-started.md)
- [How-to: Query with SPARQL](../how-to/query-with-sparql.md)
- [Reference: Query Hooks](./query-hooks.md)
