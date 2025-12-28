# UNRDF v6 API Reference

**Version**: 6.0.0-alpha.1
**Last Updated**: January 2025

Complete API reference for all UNRDF v6 packages.

---

## Table of Contents

1. [@unrdf/oxigraph](#unrdfoxigraph)
2. [@unrdf/core](#unrdfcore)
3. [@unrdf/v6-core](#unrdfv6-core)
4. [@unrdf/hooks](#unrdfhooks)
5. [@unrdf/streaming](#unrdfstreaming)
6. [@unrdf/federation](#unrdffederation)
7. [@unrdf/v6-compat](#unrdfv6-compat)

---

## @unrdf/oxigraph

Rust-based triple store backend with WASM support.

### `createStore(options?)`

Creates a new Oxigraph store instance.

**Parameters**:
- `options` (Object, optional):
  - `backend` (String): `'memory'` | `'sqlite'` (default: `'memory'`)
  - `path` (String): Path to SQLite database (required if `backend: 'sqlite'`)
  - `options` (Object):
    - `cacheSize` (Number): Cache size in KB (default: 10000)

**Returns**: `Promise<Store>`

**Example**:
```javascript
import { createStore } from '@unrdf/oxigraph';

// Memory backend
const store = await createStore();

// Persistent backend
const persistentStore = await createStore({
  backend: 'sqlite',
  path: '/path/to/data.db',
  options: { cacheSize: 20000 }
});
```

---

### `Store`

Triple store interface.

#### `add(quad)`

Adds a quad to the store.

**Parameters**:
- `quad` (Quad): RDF quad to add

**Returns**: `Promise<void>`

**Example**:
```javascript
import { dataFactory } from '@unrdf/core/rdf';

const { namedNode, literal, quad } = dataFactory;

await store.add(
  quad(
    namedNode('http://example.org/Alice'),
    namedNode('http://xmlns.com/foaf/0.1/name'),
    literal('Alice')
  )
);
```

---

#### `addAll(quads)`

Adds multiple quads in batch (10-100x faster than individual `add()`).

**Parameters**:
- `quads` (Array<Quad>): Array of quads to add

**Returns**: `Promise<void>`

**Example**:
```javascript
const quads = [
  quad(namedNode('...'), namedNode('...'), literal('...')),
  quad(namedNode('...'), namedNode('...'), literal('...'))
];

await store.addAll(quads); // Batch insert
```

---

#### `match(subject?, predicate?, object?, graph?)`

Queries the store for matching quads.

**Parameters**:
- `subject` (NamedNode | null): Subject to match (null = wildcard)
- `predicate` (NamedNode | null): Predicate to match (null = wildcard)
- `object` (NamedNode | Literal | null): Object to match (null = wildcard)
- `graph` (NamedNode | null): Graph to match (null = default graph)

**Returns**: `Promise<Array<Quad>>`

**Example**:
```javascript
// Find all triples with specific subject
const quads = await store.match(
  namedNode('http://example.org/Alice'),
  null,
  null
);

// Find all foaf:name properties
const names = await store.match(
  null,
  namedNode('http://xmlns.com/foaf/0.1/name'),
  null
);
```

---

#### `delete(quad)`

Removes a quad from the store.

**Parameters**:
- `quad` (Quad): Quad to remove

**Returns**: `Promise<void>`

**Example**:
```javascript
await store.delete(
  quad(
    namedNode('http://example.org/Alice'),
    namedNode('http://xmlns.com/foaf/0.1/name'),
    literal('Alice')
  )
);
```

---

#### `clear()`

Removes all quads from the store.

**Returns**: `Promise<void>`

**Example**:
```javascript
await store.clear(); // Empties store
```

---

#### `size`

Number of quads in the store.

**Type**: `Number` (read-only)

**Example**:
```javascript
console.log(`Store contains ${store.size} triples`);
```

---

## @unrdf/core

RDF operations, SPARQL execution, and validation.

### `dataFactory`

RDF term factory (re-export from `@rdfjs/data-model`).

**Methods**:
- `namedNode(uri)` - Creates a named node
- `literal(value, languageOrDatatype?)` - Creates a literal
- `blankNode(id?)` - Creates a blank node
- `quad(subject, predicate, object, graph?)` - Creates a quad
- `defaultGraph()` - Default graph reference

**Example**:
```javascript
import { dataFactory } from '@unrdf/core/rdf';

const { namedNode, literal, quad } = dataFactory;

const triple = quad(
  namedNode('http://example.org/Alice'),
  namedNode('http://xmlns.com/foaf/0.1/name'),
  literal('Alice', 'en')
);
```

---

### `executeSparql(store, query, options?)`

Executes a SPARQL query against a store.

**Parameters**:
- `store` (Store): Store to query
- `query` (String): SPARQL query string
- `options` (Object, optional):
  - `timeout` (Number): Timeout in milliseconds (default: 5000)
  - `maxResults` (Number): Maximum results to return (default: 1000)

**Returns**: `Promise<Array<Bindings>>`

**Example**:
```javascript
import { executeSparql } from '@unrdf/core/sparql';

const results = await executeSparql(
  store,
  `
  PREFIX foaf: <http://xmlns.com/foaf/0.1/>

  SELECT ?name
  WHERE {
    ?person foaf:name ?name .
  }
  `,
  { timeout: 10000 }
);

for (const binding of results) {
  console.log('Name:', binding.get('name')?.value);
}
```

---

### `parseRdf(data, format?, baseIRI?)`

Parses RDF data from string.

**Parameters**:
- `data` (String): RDF data to parse
- `format` (String, optional): Format (`'turtle'`, `'ntriples'`, `'json-ld'`, etc.)
- `baseIRI` (String, optional): Base IRI for relative URIs

**Returns**: `Promise<Array<Quad>>`

**Example**:
```javascript
import { parseRdf } from '@unrdf/core/rdf';

const quads = await parseRdf(
  `
  @prefix ex: <http://example.org/> .
  ex:Alice ex:name "Alice" .
  `,
  'turtle'
);

console.log(`Parsed ${quads.length} triples`);
```

---

### `serializeRdf(quads, format?)`

Serializes quads to RDF string.

**Parameters**:
- `quads` (Array<Quad>): Quads to serialize
- `format` (String, optional): Format (default: `'turtle'`)

**Returns**: `Promise<String>`

**Example**:
```javascript
import { serializeRdf } from '@unrdf/core/rdf';

const turtle = await serializeRdf(quads, 'turtle');
console.log(turtle);
```

---

### `validateShacl(store, shapesGraph)`

Validates a store against SHACL shapes.

**Parameters**:
- `store` (Store): Store to validate
- `shapesGraph` (Store): Store containing SHACL shapes

**Returns**: `Promise<ValidationReport>`

**Example**:
```javascript
import { validateShacl } from '@unrdf/core/validation';

const shapesStore = await createStore();
// ... load SHACL shapes into shapesStore ...

const report = await validateShacl(dataStore, shapesStore);

if (report.conforms) {
  console.log('✅ Validation passed');
} else {
  console.log('❌ Validation failed:', report.results);
}
```

---

## @unrdf/v6-core

Receipt-driven operations, deltas, and CLI spine.

### `createReceipt(operation, metadata?)`

Creates a cryptographic receipt for an operation.

**Parameters**:
- `operation` (String): Operation name
- `metadata` (Object, optional): Operation metadata

**Returns**: `Receipt`

**Receipt Shape**:
```typescript
{
  id: string;              // Unique receipt ID
  operation: string;       // Operation name
  timestamp: string;       // ISO 8601 timestamp
  merkleRoot: string;      // Merkle root hash
  proof: string[];         // Merkle proof
  metadata: object;        // Operation metadata
}
```

**Example**:
```javascript
import { createReceipt } from '@unrdf/v6-core/receipts';

const receipt = createReceipt('add-triple', {
  subject: 'http://example.org/Alice',
  predicate: 'http://xmlns.com/foaf/0.1/name',
  object: 'Alice'
});

console.log('Receipt:', receipt.id);
console.log('Merkle root:', receipt.merkleRoot);
```

---

### `verifyReceipt(receipt)`

Verifies the integrity of a receipt.

**Parameters**:
- `receipt` (Receipt): Receipt to verify

**Returns**: `Boolean`

**Example**:
```javascript
import { verifyReceipt } from '@unrdf/v6-core/receipts';

const isValid = verifyReceipt(receipt);
console.log('Receipt valid:', isValid);
```

---

### `createDeltaProposal(fromVersion, toVersion, operations)`

Creates a delta proposal for version transition.

**Parameters**:
- `fromVersion` (String): Source version
- `toVersion` (String): Target version
- `operations` (Array<Operation>): Delta operations

**Operation Shape**:
```typescript
{
  type: 'add' | 'remove';
  quad: {
    subject: string;
    predicate: string;
    object: string;
    graph?: string;
  };
}
```

**Returns**: `DeltaProposal`

**Example**:
```javascript
import { createDeltaProposal } from '@unrdf/v6-core/delta';

const delta = createDeltaProposal('v1.0', 'v1.1', [
  {
    type: 'add',
    quad: {
      subject: 'http://example.org/Alice',
      predicate: 'http://xmlns.com/foaf/0.1/mbox',
      object: 'alice@example.org'
    }
  }
]);
```

---

### `applyDelta(store, proposal)`

Applies a delta proposal to a store.

**Parameters**:
- `store` (Store): Store to modify
- `proposal` (DeltaProposal): Delta to apply

**Returns**: `Promise<Receipt>`

**Example**:
```javascript
import { applyDelta } from '@unrdf/v6-core/delta';

const receipt = await applyDelta(store, delta);
console.log('Delta applied:', receipt.id);
```

---

### `MerkleTree`

Merkle tree for cryptographic proofs.

#### `constructor(leaves)`

**Parameters**:
- `leaves` (Array<String>): Leaf values

**Example**:
```javascript
import { MerkleTree } from '@unrdf/v6-core/receipts';

const tree = new MerkleTree(['op1', 'op2', 'op3']);
console.log('Root:', tree.root);
```

---

#### `getProof(index)`

Generates Merkle proof for a leaf.

**Parameters**:
- `index` (Number): Leaf index

**Returns**: `Array<String>`

**Example**:
```javascript
const proof = tree.getProof(1); // Proof for 'op2'
console.log('Proof:', proof);
```

---

#### `static verify(leaf, proof, root)`

Verifies a Merkle proof.

**Parameters**:
- `leaf` (String): Leaf value
- `proof` (Array<String>): Merkle proof
- `root` (String): Expected root hash

**Returns**: `Boolean`

**Example**:
```javascript
const isValid = MerkleTree.verify('op2', proof, tree.root);
console.log('Proof valid:', isValid);
```

---

## @unrdf/hooks

Knowledge hooks and policy execution.

### `defineHook(config)`

Defines a knowledge hook.

**Parameters**:
- `config` (Object):
  - `name` (String): Hook name
  - `schema` (ZodSchema): Input validation schema
  - `handler` (Function): Hook handler
  - `receipt` (Boolean): Generate receipts (default: false)

**Returns**: `Hook`

**Example**:
```javascript
import { z } from 'zod';
import { defineHook } from '@unrdf/hooks';

const myHook = defineHook({
  name: 'validate-person',
  schema: z.object({
    person: z.string().url(),
    name: z.string().min(1)
  }),
  handler: async (ctx) => {
    console.log('Validating:', ctx.person);
    // Custom logic
  },
  receipt: true
});
```

---

### `activateHook(hook, context)`

Activates a hook with context.

**Parameters**:
- `hook` (Hook): Hook to activate
- `context` (Object): Hook context

**Returns**: `Promise<Receipt | void>`

**Example**:
```javascript
import { activateHook } from '@unrdf/hooks';

const receipt = await activateHook(myHook, {
  person: 'http://example.org/Alice',
  name: 'Alice Smith'
});

console.log('Hook receipt:', receipt?.id);
```

---

## @unrdf/streaming

AsyncIterator-based streaming.

### `createReadStream(path, options?)`

Creates a read stream from file.

**Parameters**:
- `path` (String): File path
- `options` (Object, optional):
  - `format` (String): RDF format (default: auto-detect)
  - `bufferSize` (Number): Buffer size (default: 1000)

**Returns**: `AsyncIterableIterator<Quad>`

**Example**:
```javascript
import { createReadStream } from '@unrdf/streaming';

const stream = createReadStream('/path/to/data.ttl', {
  format: 'turtle',
  bufferSize: 5000
});

for await (const quad of stream) {
  console.log('Quad:', quad);
}

const receipt = await stream.receipt();
```

---

### `createWriteStream(path, options?)`

Creates a write stream to file.

**Parameters**:
- `path` (String): File path
- `options` (Object, optional):
  - `format` (String): RDF format (default: `'ntriples'`)

**Returns**: `WritableStream<Quad>`

**Example**:
```javascript
import { createWriteStream } from '@unrdf/streaming';

const writeStream = createWriteStream('/path/to/output.nt', {
  format: 'ntriples'
});

for (const quad of quads) {
  await writeStream.write(quad);
}

await writeStream.end();
```

---

## @unrdf/federation

Federated SPARQL queries.

### `Federation`

Federated query executor.

#### `constructor(stores, options?)`

**Parameters**:
- `stores` (Array<Store | RemoteEndpoint>): Stores to federate
- `options` (Object, optional):
  - `optimizeJoins` (Boolean): Enable join optimization (default: true)
  - `parallel` (Boolean): Parallel query execution (default: true)

**Example**:
```javascript
import { Federation } from '@unrdf/federation';

const federation = new Federation([store1, store2], {
  optimizeJoins: true
});
```

---

#### `query(sparqlQuery)`

Executes a federated SPARQL query.

**Parameters**:
- `sparqlQuery` (SparqlQuery): Tagged template query

**Returns**: `Promise<Array<Bindings>>`

**Example**:
```javascript
import { sparql } from '@unrdf/federation';

const results = await federation.query(
  sparql`SELECT ?s ?p ?o WHERE { ?s ?p ?o } LIMIT 10`
    .timeout(5000)
    .receipt(true)
);

const receipt = results.receipt();
```

---

### `sparql` (tagged template)

Creates a typed SPARQL query.

**Methods**:
- `.timeout(ms)` - Set timeout
- `.receipt(boolean)` - Enable/disable receipt

**Example**:
```javascript
import { sparql } from '@unrdf/federation';

const query = sparql`
  PREFIX foaf: <http://xmlns.com/foaf/0.1/>

  SELECT ?name
  WHERE {
    ?person foaf:name ?name .
  }
`
  .timeout(10000)
  .receipt(true);
```

---

### `RemoteEndpoint`

Remote SPARQL endpoint.

#### `constructor(url, options?)`

**Parameters**:
- `url` (String): SPARQL endpoint URL
- `options` (Object, optional):
  - `headers` (Object): HTTP headers
  - `timeout` (Number): Request timeout (ms)

**Example**:
```javascript
import { RemoteEndpoint } from '@unrdf/federation';

const endpoint = new RemoteEndpoint('https://dbpedia.org/sparql', {
  headers: { 'User-Agent': 'UNRDF/6.0' },
  timeout: 30000
});
```

---

## @unrdf/v6-compat

Compatibility layer for v5 → v6 migration.

### `createStore()`

v5-compatible store creation.

**Returns**: `Promise<Store>`

**Example**:
```javascript
import { createStore } from '@unrdf/v6-compat/adapters';

// Works with both v5 and v6
const store = await createStore();
```

---

### `wrapWorkflow(workflow)`

Wraps v5 workflow to generate receipts.

**Parameters**:
- `workflow` (Object): v5 workflow object

**Returns**: `WrappedWorkflow`

**Example**:
```javascript
import { wrapWorkflow } from '@unrdf/v6-compat/adapters';

const wrapped = wrapWorkflow(myV5Workflow);
const receipt = await wrapped.execute(task); // Now generates receipt
```

---

### `querySparql(federation, query, options?)`

v5-compatible SPARQL query.

**Parameters**:
- `federation` (Federation): Federation instance
- `query` (String): SPARQL query string
- `options` (Object, optional):
  - `timeout` (Number): Timeout (ms)

**Returns**: `Promise<Array<Bindings>>`

**Example**:
```javascript
import { querySparql } from '@unrdf/v6-compat/adapters';

const results = await querySparql(
  federation,
  'SELECT * WHERE { ?s ?p ?o }',
  { timeout: 5000 }
);
```

---

### `migrationTracker`

Tracks migration progress.

#### `.summary()`

Prints migration summary.

**Example**:
```javascript
import { migrationTracker } from '@unrdf/v6-compat';

// At app shutdown
migrationTracker.summary();
// Prints:
// Migration Summary:
// - createStore: 15 calls (v6-compatible)
// - wrapWorkflow: 3 calls (needs migration)
```

---

## Type Definitions

All packages export TypeScript type definitions via JSDoc.

**Import types**:
```typescript
import type { Store, Quad } from '@unrdf/oxigraph';
import type { Receipt, DeltaProposal } from '@unrdf/v6-core';
import type { Hook } from '@unrdf/hooks';
```

---

## Error Handling

All APIs throw typed errors:

- `ZodError` - Validation failure
- `TimeoutError` - Operation timeout
- `StoreError` - Store operation failure
- `SparqlError` - SPARQL execution failure

**Example**:
```javascript
import { ZodError } from 'zod';

try {
  const validated = schema.parse(data);
} catch (error) {
  if (error instanceof ZodError) {
    console.error('Validation errors:', error.errors);
  }
}
```

---

## See Also

- [Quick Start Guide](/home/user/unrdf/docs/v6/QUICK-START.md)
- [Advanced Patterns](/home/user/unrdf/docs/v6/ADVANCED-PATTERNS.md)
- [Migration Guide](/home/user/unrdf/docs/v6/MIGRATION_PLAN.md)
- [Core Concepts](/home/user/unrdf/docs/v6/CORE-CONCEPTS.md)

---

**Questions?** Open an issue on [GitHub](https://github.com/unrdf/unrdf/issues) 🚀
