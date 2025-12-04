# API Reference

Complete API documentation for UNRDF.

## Table of Contents

1. [Core API](#core-api)
2. [RDF Operations](#rdf-operations)
3. [SPARQL Queries](#sparql-queries)
4. [Validation](#validation)
5. [Knowledge Hooks](#knowledge-hooks)
6. [Transactions](#transactions)
7. [Streaming](#streaming)
8. [Federation](#federation)

---

## Core API

### `createKnowledgeSubstrateCore(options?)`

Initialize the main UNRDF interface.

**Parameters:**

| Name | Type | Default | Description |
|------|------|---------|-------------|
| `options` | `object` | `{}` | Configuration options |
| `options.backend` | `'memory' \| 'oxigraph'` | `'memory'` | Storage backend |
| `options.enableTransactions` | `boolean` | `true` | Enable transaction support |
| `options.enableHooks` | `boolean` | `true` | Enable Knowledge Hooks |
| `options.enableValidation` | `boolean` | `true` | Enable SHACL validation |

**Returns:** `Promise<KnowledgeSubstrate>`

**Example:**

```javascript
import { createKnowledgeSubstrateCore } from '@unrdf/core';

const core = await createKnowledgeSubstrateCore({
  backend: 'oxigraph',
  enableTransactions: true
});
```

---

## RDF Operations

### `parseRdf(data, options?)`

Parse RDF data in any supported format.

**Parameters:**

| Name | Type | Description |
|------|------|-------------|
| `data` | `string` | RDF data to parse |
| `options.format` | `'turtle' \| 'ntriples' \| 'jsonld'` | Format of the data |
| `options.baseIRI` | `string` | Base IRI for relative references |

**Returns:** `Promise<Store>`

**Example:**

```javascript
const store = await core.parseRdf(`
  @prefix ex: <http://example.org/> .
  ex:Alice ex:name "Alice" .
`, { format: 'turtle' });
```

### `store.addQuad(subject, predicate, object, graph?)`

Add a triple (quad) to the store.

**Parameters:**

| Name | Type | Description |
|------|------|-------------|
| `subject` | `NamedNode \| BlankNode` | Subject of the triple |
| `predicate` | `NamedNode` | Predicate of the triple |
| `object` | `Term` | Object of the triple |
| `graph` | `NamedNode \| DefaultGraph` | Optional graph (default graph if not specified) |

**Returns:** `void`

**Example:**

```javascript
import { namedNode, literal } from '@rdfjs/data-model';

store.addQuad(
  namedNode('http://example.org/Alice'),
  namedNode('http://xmlns.com/foaf/0.1/name'),
  literal('Alice')
);
```

### `store.deleteQuad(subject?, predicate?, object?, graph?)`

Remove quads matching the pattern (omit parameters to match any).

**Parameters:** (all optional)

| Name | Type | Description |
|------|------|-------------|
| `subject` | `Term` | Subject pattern |
| `predicate` | `Term` | Predicate pattern |
| `object` | `Term` | Object pattern |
| `graph` | `Term` | Graph pattern |

**Returns:** `void`

**Example:**

```javascript
// Delete all triples about Alice
store.deleteQuad(namedNode('http://example.org/Alice'));

// Delete all triples
store.deleteQuad();
```

### `store.match(subject?, predicate?, object?, graph?)`

Query quads matching the pattern.

**Returns:** `Quad[]`

**Example:**

```javascript
const quads = store.match(
  namedNode('http://example.org/Alice'),
  undefined,
  undefined
);

for (const quad of quads) {
  console.log(quad.predicate.value, quad.object.value);
}
```

### `store.size`

Get the number of quads in the store.

**Returns:** `number`

**Example:**

```javascript
console.log(`Store contains ${store.size} triples`);
```

---

## SPARQL Queries

### `core.query(store, sparql, options?)`

Execute a SPARQL SELECT or ASK query.

**Parameters:**

| Name | Type | Description |
|------|------|-------------|
| `store` | `Store` | RDF store to query |
| `sparql` | `string` | SPARQL query |
| `options.timeout` | `number` | Query timeout in milliseconds |
| `options.limit` | `number` | Maximum results to return |

**Returns:** `Promise<Binding[]>`

**Example:**

```javascript
const results = await core.query(store, `
  PREFIX foaf: <http://xmlns.com/foaf/0.1/>

  SELECT ?name ?email
  WHERE {
    ?person a foaf:Person ;
            foaf:name ?name ;
            foaf:email ?email .
  }
`, { timeout: 5000 });

for (const binding of results) {
  console.log(binding.get('name').value);
}
```

### SPARQL Binding

Result from SPARQL query.

**Methods:**

| Method | Returns | Description |
|--------|---------|-------------|
| `.get(variable)` | `Term \| undefined` | Get binding value |
| `.has(variable)` | `boolean` | Check if variable is bound |
| `.toJSON()` | `object` | Convert to JSON |

**Example:**

```javascript
for (const row of results) {
  if (row.has('name')) {
    console.log(row.get('name').value);
  }
}
```

---

## Validation

### `core.validateShacl(store, shapes, options?)`

Validate RDF data against SHACL shapes.

**Parameters:**

| Name | Type | Description |
|------|------|-------------|
| `store` | `Store` | Data to validate |
| `shapes` | `Store` | SHACL shapes store |
| `options.focus` | `Term` | Validate specific node |

**Returns:** `Promise<ValidationReport>`

**Example:**

```javascript
const report = await core.validateShacl(store, shapesStore);

if (report.conforms) {
  console.log('Data is valid!');
} else {
  for (const result of report.results) {
    console.log(result.message);
  }
}
```

### ValidationReport

Report from SHACL validation.

**Properties:**

| Property | Type | Description |
|----------|------|-------------|
| `.conforms` | `boolean` | Whether validation passed |
| `.results` | `ValidationResult[]` | Validation failures |

**Example:**

```javascript
if (!report.conforms) {
  const count = report.results.length;
  console.log(`Found ${count} validation errors`);
}
```

---

## Knowledge Hooks

### `defineHook(config)`

Define a Knowledge Hook for reactive behaviors.

**Parameters:**

| Name | Type | Required | Description |
|------|------|----------|-------------|
| `config.meta` | `object` | Yes | Hook metadata |
| `config.meta.name` | `string` | Yes | Unique hook identifier |
| `config.meta.description` | `string` | No | Human-readable description |
| `config.trigger` | `'INSERT' \| 'DELETE'` | Yes | When to trigger |
| `config.pattern` | `string` | No | SPARQL pattern to match |
| `config.run` | `(event) => void` | Yes | Main hook function |
| `config.before` | `(event) => boolean` | No | Pre-hook gate |
| `config.after` | `(result) => void` | No | Post-hook cleanup |

**Returns:** `Hook`

**Example:**

```javascript
import { defineHook } from '@unrdf/hooks';

const myHook = defineHook({
  meta: { name: 'log-changes' },
  trigger: 'INSERT',
  pattern: '?person foaf:name ?name .',

  run(event) {
    console.log('New person:', event.quad.object.value);
  }
});
```

### `registerHook(hook)`

Register a hook to make it active.

**Parameters:**

| Name | Type | Description |
|------|------|-------------|
| `hook` | `Hook` | Hook to register |

**Returns:** `string` (hook ID)

**Example:**

```javascript
import { registerHook } from '@unrdf/hooks';

const hookId = registerHook(myHook);
```

### `unregisterHook(hookId)`

Deactivate a hook.

**Parameters:**

| Name | Type | Description |
|------|------|-------------|
| `hookId` | `string` | Hook ID from registerHook |

**Returns:** `void`

**Example:**

```javascript
import { unregisterHook } from '@unrdf/hooks';

unregisterHook(hookId);
```

---

## Transactions

### `core.beginTransaction()`

Start a transaction for atomic operations.

**Returns:** `Promise<Transaction>`

**Example:**

```javascript
const tx = await core.beginTransaction();

try {
  store.addQuad(...);
  store.addQuad(...);
  await tx.commit();
} catch (error) {
  await tx.rollback();
}
```

### Transaction

Transaction object for atomic operations.

**Methods:**

| Method | Returns | Description |
|--------|---------|-------------|
| `.commit()` | `Promise<void>` | Commit changes |
| `.rollback()` | `Promise<void>` | Undo changes |
| `.isActive()` | `boolean` | Check if transaction is active |

**Example:**

```javascript
const tx = await core.beginTransaction();

if (tx.isActive()) {
  await tx.commit();
}
```

---

## Streaming

### `createReadStream(store)`

Create a readable stream from an RDF store.

**Parameters:**

| Name | Type | Description |
|------|------|-------------|
| `store` | `Store` | Store to stream from |

**Returns:** `ReadableStream<Quad>`

**Example:**

```javascript
import { createReadStream } from '@unrdf/streaming';

const stream = createReadStream(store);

stream.on('data', (quad) => {
  console.log(quad.subject.value);
});

stream.on('end', () => {
  console.log('Done streaming');
});
```

### `createWriteStream(store)`

Create a writable stream to an RDF store.

**Returns:** `WritableStream<Quad>`

**Example:**

```javascript
import { createWriteStream } from '@unrdf/streaming';

const writeStream = createWriteStream(store);

writeStream.write(quad1);
writeStream.write(quad2);
writeStream.end();
```

---

## Federation

### `createFederatedStore(stores)`

Combine multiple stores into a single federated store.

**Parameters:**

| Name | Type | Description |
|------|------|-------------|
| `stores` | `Store[]` | Stores to federate |

**Returns:** `FederatedStore`

**Example:**

```javascript
import { createFederatedStore } from '@unrdf/federation';

const fedStore = createFederatedStore([store1, store2, store3]);

const results = await core.query(fedStore, sparqlQuery);
```

---

## Data Model

### NamedNode

Represents a URI reference.

**Properties:**

| Property | Type | Description |
|----------|------|-------------|
| `.termType` | `'NamedNode'` | Always "NamedNode" |
| `.value` | `string` | The IRI value |

**Creation:**

```javascript
import { namedNode } from '@rdfjs/data-model';

const iri = namedNode('http://example.org/Alice');
console.log(iri.value);  // "http://example.org/Alice"
```

### Literal

Represents a literal value (string, number, etc.).

**Properties:**

| Property | Type | Description |
|----------|------|-------------|
| `.termType` | `'Literal'` | Always "Literal" |
| `.value` | `string` | The literal value |
| `.language` | `string` | Language tag (if any) |
| `.datatype` | `NamedNode` | Data type IRI |

**Creation:**

```javascript
import { literal } from '@rdfjs/data-model';

const str = literal('Alice');
const num = literal(42, namedNode('http://www.w3.org/2001/XMLSchema#integer'));
const lang = literal('Bonjour', 'fr');
```

### BlankNode

Represents an anonymous node.

**Creation:**

```javascript
import { blankNode } from '@rdfjs/data-model';

const blank = blankNode();  // _:b1
const namedBlank = blankNode('x');  // _:x
```

### Quad

Represents an RDF statement (subject, predicate, object, graph).

**Properties:**

| Property | Type | Description |
|----------|------|-------------|
| `.subject` | `Term` | Subject |
| `.predicate` | `NamedNode` | Predicate |
| `.object` | `Term` | Object |
| `.graph` | `Term` | Graph (DefaultGraph if in default graph) |

---

## Utilities

### `toTurtle(store, options?)`

Serialize RDF store to Turtle format.

**Returns:** `Promise<string>`

**Example:**

```javascript
const turtle = await core.toTurtle(store);
console.log(turtle);
```

### `toNTriples(store)`

Serialize to N-Triples format.

**Returns:** `Promise<string>`

### `toJsonLD(store)`

Serialize to JSON-LD format.

**Returns:** `Promise<object>`

---

## Common Patterns

### Parse and Query

```javascript
const store = await core.parseRdf(data);
const results = await core.query(store, sparqlQuery);
```

### Validate and Update

```javascript
const report = await core.validateShacl(store, shapes);
if (report.conforms) {
  store.addQuad(...);
}
```

### Transaction with Hooks

```javascript
registerHook(myHook);

const tx = await core.beginTransaction();
try {
  store.addQuad(...);  // Hook fires
  await tx.commit();
} catch (e) {
  await tx.rollback();
}
```

### Stream Large File

```javascript
const stream = createReadStream(largeStore);
stream.pipe(transformStream).pipe(writeStream);
```

---

**Need more?** Check [EXAMPLES.md](EXAMPLES.md) for complete working examples.
