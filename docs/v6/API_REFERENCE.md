# UNRDF v6.0 API Reference

**Version**: 6.0.0
**Last Updated**: 2025-12-27
**Status**: Production Ready

Complete API reference for all UNRDF v6 packages with type signatures, examples, and migration guidance.

---

## Table of Contents

1. [@unrdf/oxigraph](#unrdfoxigraph) - Rust-based triple store
2. [@unrdf/core](#unrdfcore) - RDF operations & SPARQL
3. [@unrdf/v6-core](#unrdfv6-core) - Receipts & deltas
4. [@unrdf/hooks](#unrdfhooks) - Knowledge hooks
5. [@unrdf/streaming](#unrdfstreaming) - AsyncIterator streams
6. [@unrdf/federation](#unrdffederation) - Federated queries
7. [@unrdf/v6-compat](#unrdfv6-compat) - Migration compatibility

[Type Definitions](#type-definitions) | [Error Handling](#error-handling) | [Examples](#complete-examples) | [See Also](#see-also)

---

## @unrdf/oxigraph

**Rust-based triple store with WASM support providing 71.7x query performance improvements.**

### Installation

```bash
pnpm add @unrdf/oxigraph@latest
```

### createStore(options?)

Creates a new Oxigraph store instance with configurable backends.

**Signature**:
```typescript
function createStore(options?: StoreOptions): Promise<Store>

interface StoreOptions {
  backend?: 'memory' | 'sqlite';
  path?: string;                    // Required if backend: 'sqlite'
  options?: {
    cacheSize?: number;             // Cache size in KB (default: 10000)
    readOnly?: boolean;             // Read-only mode (default: false)
  };
}
```

**Parameters**:
- `options` (Object, optional): Store configuration
  - `backend` (String): `'memory'` (default) | `'sqlite'`
  - `path` (String): Database path for SQLite backend
  - `options.cacheSize` (Number): Cache size in KB (default: 10000)
  - `options.readOnly` (Boolean): Read-only mode (default: false)

**Returns**: `Promise<Store>`

**Examples**:

```javascript
// Memory backend (default)
import { createStore } from '@unrdf/oxigraph';

const store = await createStore();
console.log('Store created with memory backend');

// Persistent SQLite backend
const persistentStore = await createStore({
  backend: 'sqlite',
  path: './data/knowledge.db',
  options: { cacheSize: 20000 }
});

// Read-only mode
const readOnlyStore = await createStore({
  backend: 'sqlite',
  path: './data/archive.db',
  options: { readOnly: true }
});
```

**Performance**: Store creation is <1ms for memory, <10ms for SQLite.

**Migration from v5**:
```javascript
// v5 (N3.js)
import { Store } from 'n3';
const store = new Store(); // Synchronous

// v6 (Oxigraph)
import { createStore } from '@unrdf/oxigraph';
const store = await createStore(); // Async, 10x faster queries
```

---

### Store

The main store interface providing CRUD operations on RDF quads.

#### store.add(quad)

Adds a single quad to the store.

**Signature**:
```typescript
add(quad: Quad): Promise<void>
```

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
    literal('Alice Smith')
  )
);
```

**Performance**: ~0.1ms per quad (memory backend)

---

#### store.addAll(quads)

Batch adds multiple quads (10-100x faster than individual adds).

**Signature**:
```typescript
addAll(quads: Quad[]): Promise<void>
```

**Parameters**:
- `quads` (Array<Quad>): Array of quads to add

**Returns**: `Promise<void>`

**Example**:
```javascript
const quads = [
  quad(namedNode('ex:Alice'), namedNode('foaf:name'), literal('Alice')),
  quad(namedNode('ex:Bob'), namedNode('foaf:name'), literal('Bob')),
  quad(namedNode('ex:Alice'), namedNode('foaf:knows'), namedNode('ex:Bob'))
];

await store.addAll(quads); // Batch insert (100x faster)
console.log(`Added ${quads.length} quads in one transaction`);
```

**Performance**: ~0.001ms per quad in batch mode

---

#### store.match(subject?, predicate?, object?, graph?)

Queries the store for matching quads using quad pattern matching.

**Signature**:
```typescript
match(
  subject?: NamedNode | null,
  predicate?: NamedNode | null,
  object?: NamedNode | Literal | null,
  graph?: NamedNode | null
): Promise<Quad[]>
```

**Parameters**:
- `subject` (NamedNode | null): Subject to match (null = wildcard)
- `predicate` (NamedNode | null): Predicate to match (null = wildcard)
- `object` (NamedNode | Literal | null): Object to match (null = wildcard)
- `graph` (NamedNode | null): Named graph (null = default graph)

**Returns**: `Promise<Quad[]>`

**Examples**:
```javascript
// Find all triples with specific subject
const aliceTriples = await store.match(
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

// Find who Alice knows
const friends = await store.match(
  namedNode('http://example.org/Alice'),
  namedNode('http://xmlns.com/foaf/0.1/knows'),
  null
);

// Find all people named "Alice"
const alices = await store.match(
  null,
  namedNode('http://xmlns.com/foaf/0.1/name'),
  literal('Alice')
);
```

**Performance**: 71.7x faster than v5 with caching enabled

---

#### store.delete(quad)

Removes a quad from the store.

**Signature**:
```typescript
delete(quad: Quad): Promise<void>
```

**Parameters**:
- `quad` (Quad): Quad to remove

**Returns**: `Promise<void>`

**Example**:
```javascript
// Remove a specific triple
await store.delete(
  quad(
    namedNode('http://example.org/Alice'),
    namedNode('http://xmlns.com/foaf/0.1/age'),
    literal('30')
  )
);
```

---

#### store.deleteMatches(subject?, predicate?, object?, graph?)

Removes all quads matching the pattern.

**Signature**:
```typescript
deleteMatches(
  subject?: NamedNode | null,
  predicate?: NamedNode | null,
  object?: NamedNode | Literal | null,
  graph?: NamedNode | null
): Promise<number>
```

**Parameters**: Same as `match()`

**Returns**: `Promise<number>` - Count of deleted quads

**Example**:
```javascript
// Delete all triples about Alice
const deleted = await store.deleteMatches(
  namedNode('http://example.org/Alice'),
  null,
  null
);
console.log(`Deleted ${deleted} triples`);
```

---

#### store.clear()

Removes all quads from the store.

**Signature**:
```typescript
clear(): Promise<void>
```

**Returns**: `Promise<void>`

**Example**:
```javascript
await store.clear();
console.log('Store is now empty');
```

---

#### store.size

Number of quads in the store.

**Signature**:
```typescript
get size(): Promise<number>
```

**Type**: `Promise<number>` (read-only)

**Example**:
```javascript
const count = await store.size;
console.log(`Store contains ${count} triples`);
```

**Note**: In v6, `size` is a getter returning a Promise (async). In v5, it was a synchronous property.

---

#### store.query(sparql, options?)

Executes a SPARQL query against the store.

**Signature**:
```typescript
query(
  sparql: string,
  options?: QueryOptions
): Promise<QueryResults>

interface QueryOptions {
  timeout?: number;      // Milliseconds (default: 5000)
  maxResults?: number;   // Maximum results (default: 1000)
}
```

**Parameters**:
- `sparql` (String): SPARQL query string
- `options` (Object, optional): Query configuration

**Returns**: `Promise<QueryResults>`

**Example**:
```javascript
const results = await store.query(
  `
  PREFIX foaf: <http://xmlns.com/foaf/0.1/>

  SELECT ?name ?email
  WHERE {
    ?person foaf:name ?name .
    OPTIONAL { ?person foaf:mbox ?email }
  }
  LIMIT 100
  `,
  { timeout: 10000 }
);

for (const row of results) {
  console.log(`${row.name?.value} <${row.email?.value}>`);
}
```

---

### dataFactory

RDF term factory (re-export from `@rdfjs/data-model`).

**Signature**:
```typescript
const dataFactory: DataFactory

interface DataFactory {
  namedNode(value: string): NamedNode;
  blankNode(value?: string): BlankNode;
  literal(value: string, languageOrDatatype?: string | NamedNode): Literal;
  quad(subject: Term, predicate: Term, object: Term, graph?: Term): Quad;
  defaultGraph(): DefaultGraph;
}
```

**Methods**:

#### namedNode(uri)
Creates a named node (IRI).

```javascript
import { dataFactory } from '@unrdf/oxigraph';

const alice = dataFactory.namedNode('http://example.org/Alice');
const foafName = dataFactory.namedNode('http://xmlns.com/foaf/0.1/name');
```

#### literal(value, languageOrDatatype?)
Creates a literal value.

```javascript
// Plain literal
const name = dataFactory.literal('Alice');

// Language-tagged literal
const nameEn = dataFactory.literal('Alice', 'en');
const nameFr = dataFactory.literal('Alice', 'fr');

// Typed literal
const age = dataFactory.literal('30', dataFactory.namedNode('http://www.w3.org/2001/XMLSchema#integer'));
```

#### blankNode(id?)
Creates a blank node.

```javascript
// Auto-generated ID
const blank1 = dataFactory.blankNode();

// Custom ID
const blank2 = dataFactory.blankNode('b1');
```

#### quad(subject, predicate, object, graph?)
Creates a quad.

```javascript
const triple = dataFactory.quad(
  dataFactory.namedNode('http://example.org/Alice'),
  dataFactory.namedNode('http://xmlns.com/foaf/0.1/name'),
  dataFactory.literal('Alice')
);

// Named graph
const quadInGraph = dataFactory.quad(
  dataFactory.namedNode('http://example.org/Alice'),
  dataFactory.namedNode('http://xmlns.com/foaf/0.1/name'),
  dataFactory.literal('Alice'),
  dataFactory.namedNode('http://example.org/graph1')
);
```

---

## @unrdf/core

**RDF operations, SPARQL execution, parsing, serialization, and validation.**

### Installation

```bash
pnpm add @unrdf/core@latest
```

---

### executeSparql(store, query, options?)

Executes a SPARQL query with receipt generation.

**Signature**:
```typescript
function executeSparql(
  store: Store,
  query: string,
  options?: SparqlOptions
): Promise<SparqlResults>

interface SparqlOptions {
  timeout?: number;          // Timeout in milliseconds (default: 5000)
  maxResults?: number;       // Maximum results (default: 1000)
  receipt?: boolean;         // Generate receipt (default: false)
}

interface SparqlResults extends Array<Bindings> {
  receipt?: Receipt;         // Receipt if options.receipt = true
}
```

**Parameters**:
- `store` (Store): Store to query
- `query` (String): SPARQL query string
- `options` (Object, optional): Query options

**Returns**: `Promise<SparqlResults>`

**Examples**:

```javascript
import { createStore } from '@unrdf/oxigraph';
import { executeSparql } from '@unrdf/core/sparql';

const store = await createStore();

// Basic SELECT query
const results = await executeSparql(
  store,
  `
  PREFIX foaf: <http://xmlns.com/foaf/0.1/>

  SELECT ?person ?name
  WHERE {
    ?person foaf:name ?name .
  }
  `
);

for (const binding of results) {
  console.log('Person:', binding.get('person')?.value);
  console.log('Name:', binding.get('name')?.value);
}

// Query with receipt
const resultsWithReceipt = await executeSparql(
  store,
  `SELECT * WHERE { ?s ?p ?o } LIMIT 10`,
  { receipt: true }
);

console.log('Receipt:', resultsWithReceipt.receipt?.id);

// CONSTRUCT query
const constructResults = await executeSparql(
  store,
  `
  PREFIX foaf: <http://xmlns.com/foaf/0.1/>

  CONSTRUCT {
    ?person foaf:name ?name .
  }
  WHERE {
    ?person foaf:name ?name .
  }
  `
);

// ASK query
const askResults = await executeSparql(
  store,
  `
  PREFIX foaf: <http://xmlns.com/foaf/0.1/>

  ASK {
    ?person foaf:name "Alice" .
  }
  `
);

console.log('Alice exists:', askResults.boolean);
```

**Performance**: 54,311 ops/sec with warm cache (71.7x improvement over v5)

---

### parseRdf(data, format?, baseIRI?)

Parses RDF data from string into quads.

**Signature**:
```typescript
function parseRdf(
  data: string,
  format?: RdfFormat,
  baseIRI?: string
): Promise<Quad[]>

type RdfFormat = 'turtle' | 'ntriples' | 'nquads' | 'trig' | 'json-ld' | 'rdf-xml';
```

**Parameters**:
- `data` (String): RDF data to parse
- `format` (String, optional): Format (auto-detected if omitted)
- `baseIRI` (String, optional): Base IRI for relative URIs

**Returns**: `Promise<Quad[]>`

**Examples**:

```javascript
import { parseRdf } from '@unrdf/core/rdf';
import { createStore } from '@unrdf/oxigraph';

// Parse Turtle
const turtleData = `
@prefix ex: <http://example.org/> .
@prefix foaf: <http://xmlns.com/foaf/0.1/> .

ex:Alice foaf:name "Alice Smith" ;
         foaf:knows ex:Bob .

ex:Bob foaf:name "Bob Johnson" .
`;

const quads = await parseRdf(turtleData, 'turtle');
console.log(`Parsed ${quads.length} quads`);

// Load into store
const store = await createStore();
await store.addAll(quads);

// Parse JSON-LD
const jsonld = `
{
  "@context": "http://schema.org/",
  "@type": "Person",
  "name": "Alice Smith",
  "knows": {
    "@type": "Person",
    "name": "Bob Johnson"
  }
}
`;

const jsonldQuads = await parseRdf(jsonld, 'json-ld');

// Parse with base IRI
const relativeData = `
<Alice> <knows> <Bob> .
`;

const quadsWithBase = await parseRdf(
  relativeData,
  'ntriples',
  'http://example.org/'
);
```

---

### serializeRdf(quads, format?)

Serializes quads to RDF string.

**Signature**:
```typescript
function serializeRdf(
  quads: Quad[],
  format?: RdfFormat
): Promise<string>
```

**Parameters**:
- `quads` (Array<Quad>): Quads to serialize
- `format` (String, optional): Output format (default: `'turtle'`)

**Returns**: `Promise<string>`

**Examples**:

```javascript
import { serializeRdf } from '@unrdf/core/rdf';
import { dataFactory } from '@unrdf/oxigraph';

const { namedNode, literal, quad } = dataFactory;

const quads = [
  quad(
    namedNode('http://example.org/Alice'),
    namedNode('http://xmlns.com/foaf/0.1/name'),
    literal('Alice')
  )
];

// Serialize to Turtle (default)
const turtle = await serializeRdf(quads);
console.log(turtle);
// Output:
// <http://example.org/Alice> <http://xmlns.com/foaf/0.1/name> "Alice" .

// Serialize to N-Triples
const ntriples = await serializeRdf(quads, 'ntriples');

// Serialize to JSON-LD
const jsonld = await serializeRdf(quads, 'json-ld');
```

---

### validateShacl(store, shapesGraph)

Validates a store against SHACL shapes.

**Signature**:
```typescript
function validateShacl(
  store: Store,
  shapesGraph: Store
): Promise<ValidationReport>

interface ValidationReport {
  conforms: boolean;
  results: ValidationResult[];
}

interface ValidationResult {
  focusNode: NamedNode;
  path: NamedNode;
  message: string;
  severity: 'Violation' | 'Warning' | 'Info';
}
```

**Parameters**:
- `store` (Store): Data store to validate
- `shapesGraph` (Store): Store containing SHACL shapes

**Returns**: `Promise<ValidationReport>`

**Example**:

```javascript
import { validateShacl } from '@unrdf/core/validation';
import { createStore } from '@unrdf/oxigraph';
import { parseRdf } from '@unrdf/core/rdf';

// Load data
const dataStore = await createStore();
const dataQuads = await parseRdf(`
  @prefix ex: <http://example.org/> .
  @prefix foaf: <http://xmlns.com/foaf/0.1/> .

  ex:Alice a foaf:Person ;
           foaf:name "Alice" .

  ex:InvalidPerson a foaf:Person .
`, 'turtle');
await dataStore.addAll(dataQuads);

// Load SHACL shapes
const shapesStore = await createStore();
const shapesQuads = await parseRdf(`
  @prefix sh: <http://www.w3.org/ns/shacl#> .
  @prefix foaf: <http://xmlns.com/foaf/0.1/> .

  ex:PersonShape a sh:NodeShape ;
    sh:targetClass foaf:Person ;
    sh:property [
      sh:path foaf:name ;
      sh:minCount 1 ;
      sh:message "Person must have a name" ;
    ] .
`, 'turtle');
await shapesStore.addAll(shapesQuads);

// Validate
const report = await validateShacl(dataStore, shapesStore);

if (report.conforms) {
  console.log('✅ Validation passed');
} else {
  console.log('❌ Validation failed:');
  for (const result of report.results) {
    console.log(`  - ${result.focusNode.value}: ${result.message}`);
  }
}
```

---

### Namespace Management

**Signature**:
```typescript
import { namespace } from '@unrdf/core/rdf';

function namespace(baseIRI: string): (localName: string) => NamedNode;
```

**Example**:

```javascript
import { namespace } from '@unrdf/core/rdf';

// Create namespace helpers
const ex = namespace('http://example.org/');
const foaf = namespace('http://xmlns.com/foaf/0.1/');
const schema = namespace('http://schema.org/');

// Use namespaces
const alice = ex('Alice');        // http://example.org/Alice
const name = foaf('name');        // http://xmlns.com/foaf/0.1/name
const person = schema('Person');  // http://schema.org/Person
```

---

## @unrdf/v6-core

**Receipt-driven operations, delta proposals, and deterministic execution.**

### Installation

```bash
pnpm add @unrdf/v6-core@latest
```

---

### createReceipt(operation, metadata?)

Creates a cryptographic receipt for an operation.

**Signature**:
```typescript
function createReceipt(
  operation: string,
  metadata?: Record<string, any>
): Receipt

interface Receipt {
  id: string;              // Unique receipt ID (UUIDv4)
  operation: string;       // Operation name
  timestamp: string;       // ISO 8601 timestamp
  merkleRoot: string;      // Merkle tree root hash
  proof: string[];         // Merkle proof path
  metadata: object;        // Operation metadata
}
```

**Parameters**:
- `operation` (String): Operation name
- `metadata` (Object, optional): Operation-specific data

**Returns**: `Receipt`

**Examples**:

```javascript
import { createReceipt } from '@unrdf/v6-core/receipts';

// Simple receipt
const receipt1 = createReceipt('add-triple', {
  subject: 'http://example.org/Alice',
  predicate: 'http://xmlns.com/foaf/0.1/name',
  object: 'Alice Smith'
});

console.log('Receipt ID:', receipt1.id);
console.log('Operation:', receipt1.operation);
console.log('Merkle Root:', receipt1.merkleRoot);

// Receipt with rich metadata
const receipt2 = createReceipt('query-execution', {
  query: 'SELECT * WHERE { ?s ?p ?o }',
  resultCount: 42,
  executionTime: 0.015,
  cacheHit: true
});

// Chain receipts
const receipt3 = createReceipt('workflow-step', {
  step: 'data-validation',
  previousReceipt: receipt2.id  // Links to previous receipt
});
```

**Use Cases**:
- Audit trails for compliance
- Deterministic replay
- Cryptographic proof of execution
- Event sourcing

---

### verifyReceipt(receipt)

Verifies the integrity of a receipt.

**Signature**:
```typescript
function verifyReceipt(receipt: Receipt): boolean
```

**Parameters**:
- `receipt` (Receipt): Receipt to verify

**Returns**: `boolean` - `true` if valid, `false` if tampered

**Example**:

```javascript
import { createReceipt, verifyReceipt } from '@unrdf/v6-core/receipts';

const receipt = createReceipt('operation', { data: 'value' });

// Verify original receipt
console.log('Valid:', verifyReceipt(receipt)); // true

// Tamper with receipt
receipt.metadata.data = 'tampered';

// Verification fails
console.log('Valid:', verifyReceipt(receipt)); // false
```

---

### MerkleTree

Merkle tree implementation for cryptographic proofs.

**Signature**:
```typescript
class MerkleTree {
  constructor(leaves: string[])
  get root(): string
  getProof(index: number): string[]
  static verify(leaf: string, proof: string[], root: string): boolean
}
```

#### Constructor

```javascript
import { MerkleTree } from '@unrdf/v6-core/receipts';

const operations = ['op1', 'op2', 'op3', 'op4'];
const tree = new MerkleTree(operations);

console.log('Merkle Root:', tree.root);
```

#### getProof(index)

Generates Merkle proof for a specific leaf.

```javascript
const proof = tree.getProof(1); // Proof for 'op2'
console.log('Proof path:', proof);
```

#### MerkleTree.verify(leaf, proof, root)

Verifies a Merkle proof.

```javascript
const isValid = MerkleTree.verify('op2', proof, tree.root);
console.log('Proof valid:', isValid); // true

// Invalid leaf
const isInvalid = MerkleTree.verify('invalid', proof, tree.root);
console.log('Proof valid:', isInvalid); // false
```

---

### createDeltaProposal(fromVersion, toVersion, operations)

Creates a delta proposal for version transitions.

**Signature**:
```typescript
function createDeltaProposal(
  fromVersion: string,
  toVersion: string,
  operations: DeltaOperation[]
): DeltaProposal

interface DeltaOperation {
  type: 'add' | 'remove';
  quad: {
    subject: string;
    predicate: string;
    object: string;
    graph?: string;
  };
}

interface DeltaProposal {
  id: string;
  from: string;
  to: string;
  operations: DeltaOperation[];
  timestamp: string;
  receipt?: Receipt;
}
```

**Parameters**:
- `fromVersion` (String): Source version
- `toVersion` (String): Target version
- `operations` (Array<DeltaOperation>): Delta operations

**Returns**: `DeltaProposal`

**Example**:

```javascript
import { createDeltaProposal } from '@unrdf/v6-core/delta';

// Update Alice's email
const delta = createDeltaProposal('v1.0', 'v1.1', [
  {
    type: 'remove',
    quad: {
      subject: 'http://example.org/Alice',
      predicate: 'http://xmlns.com/foaf/0.1/mbox',
      object: 'alice@old.org'
    }
  },
  {
    type: 'add',
    quad: {
      subject: 'http://example.org/Alice',
      predicate: 'http://xmlns.com/foaf/0.1/mbox',
      object: 'alice@new.org'
    }
  }
]);

console.log('Delta ID:', delta.id);
console.log('Operations:', delta.operations.length);
```

---

### applyDelta(store, proposal)

Applies a delta proposal to a store.

**Signature**:
```typescript
function applyDelta(
  store: Store,
  proposal: DeltaProposal
): Promise<Receipt>
```

**Parameters**:
- `store` (Store): Store to modify
- `proposal` (DeltaProposal): Delta to apply

**Returns**: `Promise<Receipt>` - Receipt of the delta application

**Example**:

```javascript
import { createStore } from '@unrdf/oxigraph';
import { createDeltaProposal, applyDelta } from '@unrdf/v6-core/delta';
import { dataFactory } from '@unrdf/oxigraph';

const store = await createStore();
const { namedNode, literal, quad } = dataFactory;

// Initial state
await store.add(
  quad(
    namedNode('http://example.org/Alice'),
    namedNode('http://xmlns.com/foaf/0.1/mbox'),
    literal('alice@old.org')
  )
);

// Create and apply delta
const delta = createDeltaProposal('v1.0', 'v1.1', [
  {
    type: 'remove',
    quad: {
      subject: 'http://example.org/Alice',
      predicate: 'http://xmlns.com/foaf/0.1/mbox',
      object: 'alice@old.org'
    }
  },
  {
    type: 'add',
    quad: {
      subject: 'http://example.org/Alice',
      predicate: 'http://xmlns.com/foaf/0.1/mbox',
      object: 'alice@new.org'
    }
  }
]);

const receipt = await applyDelta(store, delta);
console.log('Delta applied:', receipt.id);

// Verify change
const emails = await store.match(
  namedNode('http://example.org/Alice'),
  namedNode('http://xmlns.com/foaf/0.1/mbox'),
  null
);
console.log('New email:', emails[0].object.value); // alice@new.org
```

---

## @unrdf/hooks

**Knowledge hooks for policy execution and reactive workflows.**

### Installation

```bash
pnpm add @unrdf/hooks@latest
```

---

### defineHook(config)

Defines a knowledge hook with Zod validation.

**Signature**:
```typescript
function defineHook<T>(config: HookConfig<T>): Hook<T>

interface HookConfig<T> {
  name: string;
  schema: ZodSchema<T>;
  handler: (context: T) => Promise<void> | void;
  receipt?: boolean;
}

interface Hook<T> {
  name: string;
  activate(context: T): Promise<Receipt | void>;
}
```

**Parameters**:
- `config` (Object): Hook configuration
  - `name` (String): Hook name
  - `schema` (ZodSchema): Input validation schema
  - `handler` (Function): Hook handler function
  - `receipt` (Boolean): Generate receipts (default: false)

**Returns**: `Hook<T>`

**Example**:

```javascript
import { z } from 'zod';
import { defineHook } from '@unrdf/hooks';

// Define validation schema
const PersonValidationSchema = z.object({
  person: z.string().url(),
  name: z.string().min(1).max(100),
  email: z.string().email().optional()
});

// Define hook
const validatePerson = defineHook({
  name: 'validate-person',
  schema: PersonValidationSchema,
  handler: async (ctx) => {
    console.log(`Validating person: ${ctx.name}`);

    // Validation logic
    if (ctx.email && !ctx.email.includes('@')) {
      throw new Error('Invalid email format');
    }

    console.log('✅ Person validated');
  },
  receipt: true  // Generate receipt for audit
});

// Use hook
try {
  const receipt = await validatePerson.activate({
    person: 'http://example.org/Alice',
    name: 'Alice Smith',
    email: 'alice@example.org'
  });

  console.log('Receipt:', receipt?.id);
} catch (error) {
  console.error('Validation failed:', error.message);
}
```

---

### activateHook(hook, context)

Activates a hook with provided context.

**Signature**:
```typescript
function activateHook<T>(
  hook: Hook<T>,
  context: T
): Promise<Receipt | void>
```

**Parameters**:
- `hook` (Hook): Hook to activate
- `context` (T): Hook context (validated against schema)

**Returns**: `Promise<Receipt | void>`

**Example**:

```javascript
import { activateHook } from '@unrdf/hooks';

const receipt = await activateHook(validatePerson, {
  person: 'http://example.org/Bob',
  name: 'Bob Johnson',
  email: 'bob@example.org'
});

console.log('Hook activated:', receipt?.operation);
```

---

## @unrdf/streaming

**AsyncIterator-based streaming for large RDF datasets.**

### Installation

```bash
pnpm add @unrdf/streaming@latest
```

---

### createReadStream(path, options?)

Creates a read stream from RDF file.

**Signature**:
```typescript
function createReadStream(
  path: string,
  options?: ReadStreamOptions
): AsyncIterableIterator<Quad> & { receipt(): Receipt }

interface ReadStreamOptions {
  format?: RdfFormat;
  bufferSize?: number;     // Buffer size (default: 1000)
  baseIRI?: string;
}
```

**Parameters**:
- `path` (String): File path
- `options` (Object, optional):
  - `format` (String): RDF format (auto-detected if omitted)
  - `bufferSize` (Number): Buffer size (default: 1000)
  - `baseIRI` (String): Base IRI

**Returns**: `AsyncIterableIterator<Quad> & { receipt(): Receipt }`

**Example**:

```javascript
import { createReadStream } from '@unrdf/streaming';
import { createStore } from '@unrdf/oxigraph';

const stream = createReadStream('/data/large-dataset.ttl', {
  format: 'turtle',
  bufferSize: 5000
});

const store = await createStore();
let count = 0;

// Process stream
for await (const quad of stream) {
  await store.add(quad);
  count++;

  if (count % 10000 === 0) {
    console.log(`Processed ${count} quads...`);
  }
}

console.log(`Total: ${count} quads`);

// Get receipt after completion
const receipt = stream.receipt();
console.log('Stream receipt:', receipt.id);
```

---

### createWriteStream(path, options?)

Creates a write stream to RDF file.

**Signature**:
```typescript
function createWriteStream(
  path: string,
  options?: WriteStreamOptions
): WritableStream<Quad> & { receipt(): Receipt }

interface WriteStreamOptions {
  format?: RdfFormat;
}
```

**Parameters**:
- `path` (String): Output file path
- `options` (Object, optional):
  - `format` (String): Output format (default: `'ntriples'`)

**Returns**: `WritableStream<Quad> & { receipt(): Receipt }`

**Example**:

```javascript
import { createWriteStream } from '@unrdf/streaming';
import { createStore } from '@unrdf/oxigraph';
import { dataFactory } from '@unrdf/oxigraph';

const { namedNode, literal, quad } = dataFactory;

// Create quads
const quads = [
  quad(namedNode('ex:Alice'), namedNode('foaf:name'), literal('Alice')),
  quad(namedNode('ex:Bob'), namedNode('foaf:name'), literal('Bob'))
];

// Write to file
const writeStream = createWriteStream('/data/output.nt', {
  format: 'ntriples'
});

for (const q of quads) {
  await writeStream.write(q);
}

await writeStream.end();

const receipt = writeStream.receipt();
console.log('Write complete:', receipt.id);
```

---

## @unrdf/federation

**Federated SPARQL queries across multiple endpoints.**

### Installation

```bash
pnpm add @unrdf/federation@latest
```

---

### Federation

Federated query executor.

**Signature**:
```typescript
class Federation {
  constructor(stores: (Store | RemoteEndpoint)[], options?: FederationOptions)
  query(sparqlQuery: SparqlQuery): Promise<SparqlResults>
}

interface FederationOptions {
  optimizeJoins?: boolean;    // Join optimization (default: true)
  parallel?: boolean;         // Parallel execution (default: true)
  timeout?: number;           // Default timeout (default: 5000)
}
```

#### Constructor

```javascript
import { Federation } from '@unrdf/federation';
import { createStore } from '@unrdf/oxigraph';

const store1 = await createStore();
const store2 = await createStore();

const federation = new Federation([store1, store2], {
  optimizeJoins: true,
  parallel: true
});
```

#### query(sparqlQuery)

Executes a federated SPARQL query.

**Example**:

```javascript
import { sparql } from '@unrdf/federation';

const results = await federation.query(
  sparql`
    PREFIX foaf: <http://xmlns.com/foaf/0.1/>

    SELECT ?name
    WHERE {
      ?person foaf:name ?name .
    }
    LIMIT 100
  `
    .timeout(10000)
    .receipt(true)
);

for (const binding of results) {
  console.log('Name:', binding.get('name')?.value);
}

const receipt = results.receipt();
console.log('Query receipt:', receipt?.id);
```

---

### sparql (Tagged Template)

Creates a typed SPARQL query.

**Signature**:
```typescript
function sparql(
  strings: TemplateStringsArray,
  ...values: any[]
): SparqlQuery

interface SparqlQuery {
  timeout(ms: number): SparqlQuery;
  receipt(enabled: boolean): SparqlQuery;
}
```

**Example**:

```javascript
import { sparql } from '@unrdf/federation';

// Template literal prevents injection
const name = 'Alice';
const query = sparql`
  PREFIX foaf: <http://xmlns.com/foaf/0.1/>

  SELECT ?person
  WHERE {
    ?person foaf:name "${name}" .
  }
`
  .timeout(5000)
  .receipt(true);

const results = await federation.query(query);
```

---

### RemoteEndpoint

Remote SPARQL endpoint connection.

**Signature**:
```typescript
class RemoteEndpoint {
  constructor(url: string, options?: EndpointOptions)
}

interface EndpointOptions {
  headers?: Record<string, string>;
  timeout?: number;
  auth?: {
    username: string;
    password: string;
  };
}
```

**Example**:

```javascript
import { RemoteEndpoint, Federation } from '@unrdf/federation';

const dbpedia = new RemoteEndpoint('https://dbpedia.org/sparql', {
  headers: { 'User-Agent': 'UNRDF/6.0' },
  timeout: 30000
});

const wikidata = new RemoteEndpoint('https://query.wikidata.org/sparql', {
  timeout: 30000
});

// Federate across remote endpoints
const federation = new Federation([dbpedia, wikidata]);

const results = await federation.query(
  sparql`SELECT * WHERE { ?s ?p ?o } LIMIT 10`
);
```

---

## @unrdf/v6-compat

**Compatibility layer for v5 → v6 migration.**

### Installation

```bash
pnpm add @unrdf/v6-compat@latest
```

---

### createStore()

v5-compatible store creation (migrates automatically).

**Signature**:
```typescript
function createStore(): Promise<Store>
```

**Example**:

```javascript
import { createStore } from '@unrdf/v6-compat/adapters';

// Works with both v5 and v6 code
const store = await createStore();

// Logs deprecation warning:
// "Using v6-compat adapter. Migrate to @unrdf/oxigraph for best performance."
```

---

### wrapWorkflow(workflow)

Wraps v5 workflow to generate receipts.

**Signature**:
```typescript
function wrapWorkflow<T>(
  workflow: V5Workflow<T>
): V6Workflow<T>

interface V5Workflow<T> {
  run(task: T): Promise<any>;
}

interface V6Workflow<T> {
  execute(task: T): Promise<{ result: any; receipt: Receipt }>;
}
```

**Example**:

```javascript
import { wrapWorkflow } from '@unrdf/v6-compat/adapters';

// v5 workflow (no receipts)
const myV5Workflow = {
  run: async (task) => {
    // Process task...
    return { success: true };
  }
};

// Wrap to add receipt support
const myV6Workflow = wrapWorkflow(myV5Workflow);

// Now generates receipts
const { result, receipt } = await myV6Workflow.execute({ data: 'test' });
console.log('Result:', result);
console.log('Receipt:', receipt.id);
```

---

### querySparql(federation, query, options?)

v5-compatible SPARQL query (string-based).

**Signature**:
```typescript
function querySparql(
  federation: Federation,
  query: string,
  options?: { timeout?: number }
): Promise<SparqlResults>
```

**Example**:

```javascript
import { querySparql } from '@unrdf/v6-compat/adapters';
import { Federation } from '@unrdf/federation';
import { createStore } from '@unrdf/oxigraph';

const store = await createStore();
const federation = new Federation([store]);

// v5 style (string query)
const results = await querySparql(
  federation,
  'SELECT * WHERE { ?s ?p ?o }',
  { timeout: 5000 }
);

// Works but logs deprecation warning
```

---

### migrationTracker

Tracks migration progress and usage.

**Signature**:
```typescript
const migrationTracker: {
  summary(): void;
  reset(): void;
}
```

**Example**:

```javascript
import { migrationTracker } from '@unrdf/v6-compat';

// At app shutdown
migrationTracker.summary();

// Prints:
// Migration Summary:
// - createStore: 15 calls (v6-compatible)
// - wrapWorkflow: 3 calls (needs migration)
// - querySparql: 42 calls (deprecated, use sparql template)
```

---

## Type Definitions

All packages export TypeScript type definitions via JSDoc.

### Importing Types

```typescript
// Store and Quad types
import type { Store, Quad } from '@unrdf/oxigraph';

// Receipt and Delta types
import type { Receipt, DeltaProposal } from '@unrdf/v6-core';

// Hook types
import type { Hook } from '@unrdf/hooks';

// Federation types
import type { Federation, RemoteEndpoint } from '@unrdf/federation';
```

### Common Types

```typescript
interface Quad {
  subject: NamedNode | BlankNode;
  predicate: NamedNode;
  object: NamedNode | BlankNode | Literal;
  graph: NamedNode | DefaultGraph;
}

interface NamedNode {
  termType: 'NamedNode';
  value: string;
}

interface Literal {
  termType: 'Literal';
  value: string;
  language?: string;
  datatype: NamedNode;
}

interface BlankNode {
  termType: 'BlankNode';
  value: string;
}
```

---

## Error Handling

All APIs throw typed errors for better error handling.

### Error Types

```typescript
class ZodError extends Error {
  errors: ZodIssue[];
}

class TimeoutError extends Error {
  timeout: number;
}

class StoreError extends Error {
  operation: string;
}

class SparqlError extends Error {
  query: string;
  line?: number;
  column?: number;
}

class ValidationError extends Error {
  violations: ValidationResult[];
}
```

### Error Handling Examples

```javascript
import { ZodError } from 'zod';
import { TimeoutError, StoreError, SparqlError } from '@unrdf/core';

// Zod validation errors
try {
  const validated = schema.parse(data);
} catch (error) {
  if (error instanceof ZodError) {
    console.error('Validation errors:');
    for (const issue of error.errors) {
      console.error(`  - ${issue.path.join('.')}: ${issue.message}`);
    }
  }
}

// Timeout errors
try {
  const results = await executeSparql(store, query, { timeout: 1000 });
} catch (error) {
  if (error instanceof TimeoutError) {
    console.error(`Query timeout after ${error.timeout}ms`);
  }
}

// Store errors
try {
  await store.add(quad);
} catch (error) {
  if (error instanceof StoreError) {
    console.error(`Store operation failed: ${error.operation}`);
  }
}

// SPARQL errors
try {
  const results = await executeSparql(store, invalidQuery);
} catch (error) {
  if (error instanceof SparqlError) {
    console.error(`SPARQL error at line ${error.line}: ${error.message}`);
  }
}
```

---

## Complete Examples

### Example 1: Basic Knowledge Graph

```javascript
import { createStore, dataFactory } from '@unrdf/oxigraph';
import { executeSparql } from '@unrdf/core/sparql';
import { createReceipt } from '@unrdf/v6-core/receipts';

const { namedNode, literal, quad } = dataFactory;

// Create store
const store = await createStore();

// Add data with receipt
async function addPersonWithReceipt(name, email) {
  const quads = [
    quad(
      namedNode(`http://example.org/${name}`),
      namedNode('http://xmlns.com/foaf/0.1/name'),
      literal(name)
    ),
    quad(
      namedNode(`http://example.org/${name}`),
      namedNode('http://xmlns.com/foaf/0.1/mbox'),
      literal(email)
    )
  ];

  await store.addAll(quads);

  return createReceipt('add-person', { name, email });
}

// Add people
const r1 = await addPersonWithReceipt('Alice', 'alice@example.org');
const r2 = await addPersonWithReceipt('Bob', 'bob@example.org');

console.log('Receipts:', r1.id, r2.id);

// Query
const results = await executeSparql(
  store,
  `
  PREFIX foaf: <http://xmlns.com/foaf/0.1/>

  SELECT ?name ?email
  WHERE {
    ?person foaf:name ?name ;
            foaf:mbox ?email .
  }
  `
);

for (const binding of results) {
  console.log(`${binding.get('name').value}: ${binding.get('email').value}`);
}
```

### Example 2: Streaming Large Dataset

```javascript
import { createReadStream } from '@unrdf/streaming';
import { createStore } from '@unrdf/oxigraph';
import { executeSparql } from '@unrdf/core/sparql';

async function loadLargeDataset(path) {
  const store = await createStore();
  const stream = createReadStream(path, { bufferSize: 10000 });

  let count = 0;
  for await (const quad of stream) {
    await store.add(quad);
    count++;

    if (count % 100000 === 0) {
      console.log(`Loaded ${count} triples...`);
    }
  }

  const receipt = stream.receipt();
  console.log(`Loaded ${count} triples. Receipt: ${receipt.id}`);

  return store;
}

const store = await loadLargeDataset('/data/dbpedia-subset.nt');

// Query loaded data
const results = await executeSparql(
  store,
  'SELECT (COUNT(*) as ?count) WHERE { ?s ?p ?o }'
);

console.log('Total triples:', results[0].get('count').value);
```

### Example 3: Delta-Based Versioning

```javascript
import { createStore, dataFactory } from '@unrdf/oxigraph';
import { createDeltaProposal, applyDelta } from '@unrdf/v6-core/delta';

const { namedNode, literal, quad } = dataFactory;
const store = await createStore();

// Initial state (v1.0)
await store.add(
  quad(
    namedNode('http://example.org/Alice'),
    namedNode('http://xmlns.com/foaf/0.1/age'),
    literal('30')
  )
);

console.log('v1.0 state: Alice is 30 years old');

// Create delta: v1.0 → v1.1 (birthday)
const delta = createDeltaProposal('v1.0', 'v1.1', [
  {
    type: 'remove',
    quad: {
      subject: 'http://example.org/Alice',
      predicate: 'http://xmlns.com/foaf/0.1/age',
      object: '30'
    }
  },
  {
    type: 'add',
    quad: {
      subject: 'http://example.org/Alice',
      predicate: 'http://xmlns.com/foaf/0.1/age',
      object: '31'
    }
  }
]);

// Apply delta
const receipt = await applyDelta(store, delta);
console.log('Delta applied:', receipt.id);

// Verify new state
const ages = await store.match(
  namedNode('http://example.org/Alice'),
  namedNode('http://xmlns.com/foaf/0.1/age'),
  null
);

console.log('v1.1 state: Alice is', ages[0].object.value, 'years old');
```

---

## Performance Best Practices

### 1. Use Batch Operations

```javascript
// ❌ Slow (individual adds)
for (const quad of quads) {
  await store.add(quad);  // 1000 ops/sec
}

// ✅ Fast (batch add)
await store.addAll(quads);  // 100,000 ops/sec (100x faster)
```

### 2. Enable Query Caching

```javascript
import { executeSparql } from '@unrdf/core/sparql';

// Warm cache with common queries
const query = 'SELECT * WHERE { ?s ?p ?o } LIMIT 100';

const results1 = await executeSparql(store, query); // 757 ops/sec (cold)
const results2 = await executeSparql(store, query); // 54,311 ops/sec (warm, 71.7x faster)
```

### 3. Use Streaming for Large Datasets

```javascript
// ❌ Slow (load entire file into memory)
const quads = await parseRdf(largeFile);  // 110 MB memory
await store.addAll(quads);

// ✅ Fast (stream, constant memory)
const stream = createReadStream(largeFile);
for await (const quad of stream) {
  await store.add(quad);  // Constant ~10 MB memory
}
```

### 4. Optimize SPARQL Queries

```javascript
// ❌ Slow (large result set)
const results = await executeSparql(
  store,
  'SELECT * WHERE { ?s ?p ?o }'  // Returns everything
);

// ✅ Fast (limit results)
const results = await executeSparql(
  store,
  'SELECT * WHERE { ?s ?p ?o } LIMIT 1000'
);
```

---

## See Also

- **[Quick Start Guide](/home/user/unrdf/docs/v6/QUICK-START.md)** - Get started in 15 minutes
- **[Core Concepts](/home/user/unrdf/docs/v6/CORE-CONCEPTS.md)** - Understand v6 architecture
- **[Advanced Patterns](/home/user/unrdf/docs/v6/ADVANCED-PATTERNS.md)** - Deep dive examples
- **[Migration Guide](/home/user/unrdf/docs/v6/MIGRATION_GUIDE.md)** - Upgrade from v5
- **[Breaking Changes](/home/user/unrdf/docs/v6/BREAKING-CHANGES.md)** - All breaking changes
- **[Performance](/home/user/unrdf/docs/v6/PERFORMANCE.md)** - Benchmarks and optimization
- **[Release Notes](/home/user/unrdf/docs/v6/RELEASE_NOTES.md)** - What's new in v6

---

## Support

- **GitHub Issues**: [github.com/unrdf/unrdf/issues](https://github.com/unrdf/unrdf/issues)
- **Discussions**: [github.com/unrdf/unrdf/discussions](https://github.com/unrdf/unrdf/discussions)
- **Documentation**: [/docs](/home/user/unrdf/docs/)

---

**Version**: 6.0.0
**License**: MIT
**Maintained By**: UNRDF Core Team
