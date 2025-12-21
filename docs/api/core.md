# @unrdf/core API Reference

Complete API documentation for the `@unrdf/core` package - the foundation of UNRDF.

## Table of Contents

- [High-Level API](#high-level-api)
- [RDF Operations](#rdf-operations)
- [SPARQL Queries](#sparql-queries)
- [SHACL Validation](#shacl-validation)
- [Transactions](#transactions)
- [Type Definitions](#type-definitions)

---

## High-Level API

### `createKnowledgeSubstrateCore(options?)`

Creates a fully configured knowledge substrate with all features enabled.

**Signature:**
```typescript
function createKnowledgeSubstrateCore(
  options?: KnowledgeSubstrateOptions
): Promise<KnowledgeSubstrate>
```

**Parameters:**

| Parameter | Type | Default | Description |
|-----------|------|---------|-------------|
| `options.enableTransactionManager` | `boolean` | `true` | Enable ACID transactions |
| `options.enableKnowledgeHookManager` | `boolean` | `true` | Enable Knowledge Hooks |
| `options.enableValidation` | `boolean` | `true` | Enable SHACL validation |
| `options.enableObservability` | `boolean` | `true` | Enable OTEL tracing |
| `options.enableFederation` | `boolean` | `false` | Enable federated queries |
| `options.enableStreaming` | `boolean` | `false` | Enable streaming support |
| `options.backend` | `'memory' \| 'oxigraph'` | `'memory'` | RDF store backend |
| `options.logLevel` | `'debug' \| 'info' \| 'warn' \| 'error'` | `'info'` | Logging level |

**Returns:** `Promise<KnowledgeSubstrate>` - Configured knowledge substrate instance.

**Example:**

```javascript
import { createKnowledgeSubstrateCore } from '@unrdf/core';

// Default configuration (all features enabled)
const core = await createKnowledgeSubstrateCore();

// Custom configuration
const core = await createKnowledgeSubstrateCore({
  backend: 'oxigraph',
  enableFederation: true,
  enableStreaming: true,
  logLevel: 'debug'
});
```

**Related:**
- [RDF Operations](#rdf-operations)
- [SPARQL Queries](#sparql-queries)

---

## RDF Operations

### `parseTurtle(turtleData, options?)`

Parse RDF Turtle format into a quad store.

**Signature:**
```typescript
function parseTurtle(
  turtleData: string,
  options?: ParseOptions
): Promise<Store>
```

**Parameters:**

| Parameter | Type | Description |
|-----------|------|-------------|
| `turtleData` | `string` | RDF Turtle string |
| `options.baseIRI` | `string?` | Base IRI for relative URIs |
| `options.blankNodePrefix` | `string?` | Prefix for blank node identifiers |

**Returns:** `Promise<Store>` - Quad store containing parsed triples.

**Throws:**
- `TurtleParseError` - If Turtle syntax is invalid

**Example:**

```javascript
import { parseTurtle } from '@unrdf/core/rdf';

const turtleData = `
  @prefix ex: <http://example.org/> .
  @prefix foaf: <http://xmlns.com/foaf/0.1/> .

  ex:Alice foaf:name "Alice Smith" ;
           foaf:knows ex:Bob .

  ex:Bob foaf:name "Bob Johnson" .
`;

try {
  const store = await parseTurtle(turtleData);
  console.log('Parsed', store.size, 'triples');
} catch (error) {
  console.error('Parse error:', error.message);
}
```

**Related:**
- `parseNTriples()` - Parse N-Triples format
- `parseJSONLD()` - Parse JSON-LD format

---

### `serializeTurtle(store, options?)`

Serialize a quad store to RDF Turtle format.

**Signature:**
```typescript
function serializeTurtle(
  store: Store,
  options?: SerializeOptions
): Promise<string>
```

**Parameters:**

| Parameter | Type | Description |
|-----------|------|-------------|
| `store` | `Store` | Quad store to serialize |
| `options.prefixes` | `Record<string, string>?` | Custom namespace prefixes |

**Returns:** `Promise<string>` - Turtle-formatted RDF string.

**Example:**

```javascript
import { serializeTurtle } from '@unrdf/core/rdf';

const turtle = await serializeTurtle(store, {
  prefixes: {
    ex: 'http://example.org/',
    foaf: 'http://xmlns.com/foaf/0.1/'
  }
});

console.log(turtle);
// Output:
// @prefix ex: <http://example.org/> .
// @prefix foaf: <http://xmlns.com/foaf/0.1/> .
// ex:Alice foaf:name "Alice Smith" .
```

---

### `createStore(backend?)`

Create an empty RDF quad store.

**Signature:**
```typescript
function createStore(
  backend?: 'memory' | 'oxigraph'
): Store
```

**Parameters:**

| Parameter | Type | Default | Description |
|-----------|------|---------|-------------|
| `backend` | `'memory' \| 'oxigraph'` | `'memory'` | Storage backend |

**Returns:** `Store` - Empty quad store.

**Example:**

```javascript
import { createStore } from '@unrdf/oxigraph';
import { namedNode, literal } from '@rdfjs/data-model';

const store = createStore();

// Add triples
store.addQuad(
  namedNode('http://example.org/Alice'),
  namedNode('http://xmlns.com/foaf/0.1/name'),
  literal('Alice')
);

console.log('Store size:', store.size);
// Output: Store size: 1
```

---

## SPARQL Queries

### `query(store, sparqlQuery, options?)`

Execute a SPARQL query against an RDF store.

**Signature:**
```typescript
function query(
  store: Store,
  sparqlQuery: string,
  options?: QueryOptions
): Promise<SPARQLResult[]>
```

**Parameters:**

| Parameter | Type | Description |
|-----------|------|-------------|
| `store` | `Store` | RDF quad store to query |
| `sparqlQuery` | `string` | SPARQL 1.1 query string |
| `options.timeout` | `number?` | Query timeout in milliseconds (default: 30000) |
| `options.baseIRI` | `string?` | Base IRI for relative URIs |

**Returns:** `Promise<SPARQLResult[]>` - Array of query result bindings.

Each result is a `Map<string, RDFTerm>` where:
- Key: Variable name (without `?` prefix)
- Value: RDF term (Named Node, Literal, or Blank Node)

**Throws:**
- `SPARQLSyntaxError` - If query has syntax errors
- `QueryTimeoutError` - If query exceeds timeout

**Example:**

```javascript
import { query } from '@unrdf/core/sparql';

const sparqlQuery = `
  PREFIX foaf: <http://xmlns.com/foaf/0.1/>

  SELECT ?name ?email
  WHERE {
    ?person foaf:name ?name .
    OPTIONAL { ?person foaf:mbox ?email }
  }
  ORDER BY ?name
`;

try {
  const results = await query(store, sparqlQuery);

  for (const row of results) {
    const name = row.get('name')?.value;
    const email = row.get('email')?.value || 'No email';
    console.log(`${name}: ${email}`);
  }
} catch (error) {
  if (error.name === 'SPARQLSyntaxError') {
    console.error('Invalid SPARQL:', error.message);
  } else {
    console.error('Query failed:', error);
  }
}
```

**Supported SPARQL Features:**
- SELECT queries
- CONSTRUCT queries
- ASK queries
- DESCRIBE queries
- FILTER expressions
- OPTIONAL patterns
- UNION patterns
- Property paths
- Aggregates (COUNT, SUM, AVG, etc.)
- Subqueries
- Named graphs

**Related:**
- `querySync()` - Synchronous query execution
- `queryStream()` - Streaming query results (from `@unrdf/streaming`)

---

### `construct(store, sparqlQuery, options?)`

Execute a SPARQL CONSTRUCT query to generate new triples.

**Signature:**
```typescript
function construct(
  store: Store,
  sparqlQuery: string,
  options?: QueryOptions
): Promise<Store>
```

**Parameters:**

| Parameter | Type | Description |
|-----------|------|-------------|
| `store` | `Store` | RDF quad store to query |
| `sparqlQuery` | `string` | SPARQL CONSTRUCT query |
| `options` | `QueryOptions?` | Query options (timeout, etc.) |

**Returns:** `Promise<Store>` - New store containing constructed triples.

**Example:**

```javascript
import { construct } from '@unrdf/core/sparql';

const constructQuery = `
  PREFIX foaf: <http://xmlns.com/foaf/0.1/>
  PREFIX ex: <http://example.org/>

  CONSTRUCT {
    ?person a ex:ContactInfo ;
            ex:fullName ?name ;
            ex:emailAddress ?email .
  }
  WHERE {
    ?person foaf:name ?name ;
            foaf:mbox ?email .
  }
`;

const newStore = await construct(store, constructQuery);
console.log('Constructed', newStore.size, 'triples');
```

---

## SHACL Validation

### `validateShacl(dataStore, shapesStore, options?)`

Validate RDF data against SHACL shapes.

**Signature:**
```typescript
function validateShacl(
  dataStore: Store,
  shapesStore: Store,
  options?: ValidationOptions
): Promise<ValidationReport>
```

**Parameters:**

| Parameter | Type | Description |
|-----------|------|-------------|
| `dataStore` | `Store` | RDF data to validate |
| `shapesStore` | `Store` | SHACL shapes definitions |
| `options.focusNodes` | `NamedNode[]?` | Only validate specific nodes |

**Returns:** `Promise<ValidationReport>` - Validation report.

**ValidationReport Structure:**
```typescript
interface ValidationReport {
  conforms: boolean;
  results: ValidationResult[];
}

interface ValidationResult {
  focusNode: NamedNode;
  resultPath: NamedNode;
  resultMessage: string;
  resultSeverity: 'Violation' | 'Warning' | 'Info';
  sourceConstraintComponent: NamedNode;
}
```

**Example:**

```javascript
import { validateShacl } from '@unrdf/core/validation';
import { parseTurtle } from '@unrdf/core/rdf';

// Define SHACL shapes
const shapesData = `
  @prefix sh: <http://www.w3.org/ns/shacl#> .
  @prefix ex: <http://example.org/> .
  @prefix xsd: <http://www.w3.org/2001/XMLSchema#> .

  ex:PersonShape a sh:NodeShape ;
    sh:targetClass ex:Person ;
    sh:property [
      sh:path ex:name ;
      sh:minCount 1 ;
      sh:datatype xsd:string ;
      sh:maxLength 100 ;
    ] ;
    sh:property [
      sh:path ex:age ;
      sh:datatype xsd:integer ;
      sh:minInclusive 0 ;
      sh:maxInclusive 150 ;
    ] .
`;

const shapesStore = await parseTurtle(shapesData);

// Validate data
const report = await validateShacl(dataStore, shapesStore);

if (report.conforms) {
  console.log('✅ Data is valid');
} else {
  console.log('❌ Validation failed:');
  for (const result of report.results) {
    console.log(`  - ${result.focusNode.value}: ${result.resultMessage}`);
  }
}
```

---

## Transactions

### `beginTransaction(store, options?)`

Begin an ACID transaction on an RDF store.

**Signature:**
```typescript
function beginTransaction(
  store: Store,
  options?: TransactionOptions
): Promise<Transaction>
```

**Parameters:**

| Parameter | Type | Description |
|-----------|------|-------------|
| `store` | `Store` | RDF store to transact on |
| `options.isolationLevel` | `'serializable' \| 'snapshot'?` | Isolation level |

**Returns:** `Promise<Transaction>` - Transaction object.

**Transaction Methods:**
- `commit(): Promise<void>` - Commit transaction
- `rollback(): Promise<void>` - Rollback transaction
- `addQuad(quad): void` - Add quad in transaction
- `deleteQuad(quad): void` - Delete quad in transaction

**Example:**

```javascript
import { beginTransaction } from '@unrdf/core';
import { namedNode, literal } from '@rdfjs/data-model';

const tx = await beginTransaction(store);

try {
  // Make changes within transaction
  tx.addQuad(
    namedNode('http://example.org/Alice'),
    namedNode('http://xmlns.com/foaf/0.1/name'),
    literal('Alice Smith')
  );

  tx.addQuad(
    namedNode('http://example.org/Alice'),
    namedNode('http://xmlns.com/foaf/0.1/age'),
    literal('30', namedNode('http://www.w3.org/2001/XMLSchema#integer'))
  );

  // Commit transaction (atomic)
  await tx.commit();
  console.log('✅ Transaction committed');

} catch (error) {
  // Rollback on error
  await tx.rollback();
  console.error('❌ Transaction rolled back:', error);
}
```

**Guarantees:**
- **Atomicity:** All changes commit together or none commit
- **Consistency:** SHACL constraints enforced (if enabled)
- **Isolation:** Configurable isolation levels
- **Durability:** Changes persisted (Oxigraph backend)

---

## Type Definitions

### RDFTerm

Union type representing any RDF term.

```typescript
type RDFTerm = NamedNode | BlankNode | Literal;

interface NamedNode {
  termType: 'NamedNode';
  value: string;  // IRI
}

interface BlankNode {
  termType: 'BlankNode';
  value: string;  // Blank node identifier
}

interface Literal {
  termType: 'Literal';
  value: string;
  language?: string;
  datatype: NamedNode;
}
```

### Quad

Represents an RDF triple or quad (with optional graph).

```typescript
interface Quad {
  subject: NamedNode | BlankNode;
  predicate: NamedNode;
  object: RDFTerm;
  graph?: NamedNode | BlankNode;
}
```

### Store

RDF quad store interface.

```typescript
interface Store {
  size: number;

  addQuad(quad: Quad): void;
  addQuads(quads: Quad[]): void;

  deleteQuad(quad: Quad): void;
  deleteQuads(quads: Quad[]): void;

  match(
    subject?: RDFTerm | null,
    predicate?: RDFTerm | null,
    object?: RDFTerm | null,
    graph?: RDFTerm | null
  ): Quad[];

  has(quad: Quad): boolean;

  destroy(): void;
}
```

---

## Error Types

### SPARQLSyntaxError

Thrown when SPARQL query has syntax errors.

```typescript
class SPARQLSyntaxError extends Error {
  name: 'SPARQLSyntaxError';
  message: string;
  line?: number;
  column?: number;
}
```

### TurtleParseError

Thrown when Turtle parsing fails.

```typescript
class TurtleParseError extends Error {
  name: 'TurtleParseError';
  message: string;
  line: number;
  column: number;
}
```

### QueryTimeoutError

Thrown when query exceeds timeout.

```typescript
class QueryTimeoutError extends Error {
  name: 'QueryTimeoutError';
  message: string;
  timeout: number;
}
```

---

## Constants

### Namespaces

Commonly used RDF namespace prefixes.

```javascript
import { namespaces } from '@unrdf/core/constants';

console.log(namespaces.rdf);   // http://www.w3.org/1999/02/22-rdf-syntax-ns#
console.log(namespaces.rdfs);  // http://www.w3.org/2000/01/rdf-schema#
console.log(namespaces.owl);   // http://www.w3.org/2002/07/owl#
console.log(namespaces.xsd);   // http://www.w3.org/2001/XMLSchema#
console.log(namespaces.foaf);  // http://xmlns.com/foaf/0.1/
console.log(namespaces.dct);   // http://purl.org/dc/terms/
```

---

## See Also

- [Troubleshooting Guide](../troubleshooting.md)
- [Production Deployment](../deployment/production.md)
- [SPARQL 1.1 Specification](https://www.w3.org/TR/sparql11-query/)
- [SHACL Specification](https://www.w3.org/TR/shacl/)
