# Utilities API Reference

**Version**: v4.1.1
**Package**: `unrdf/utils`
**Module**: `utils`

This reference documents utility functions organized by domain: ID generation, namespace management, graph operations, and quality assessment.

---

## Table of Contents

- [ID Generation](#id-generation)
- [Namespace Management](#namespace-management)
- [Graph Operations](#graph-operations)
- [Quality Assessment](#quality-assessment)

---

## ID Generation

UNRDF provides 20+ ID generation functions for various use cases.

### UUID Generation

#### generateUUID

**Signature**: `generateUUID(): string`

**Description**: Generates a UUID v4 string.

**Returns**: UUID v4 string in format `xxxxxxxx-xxxx-4xxx-yxxx-xxxxxxxxxxxx`

**Example**:
```javascript
import { generateUUID } from 'unrdf/utils';

const id = generateUUID();
console.log(id); // '550e8400-e29b-41d4-a716-446655440000'
```

**Since**: v4.1.1

---

#### generateShortUUID

**Signature**: `generateShortUUID(): string`

**Description**: Generates a short UUID using base36 encoding (shorter than standard UUID).

**Returns**: Short UUID string

**Example**:
```javascript
import { generateShortUUID } from 'unrdf/utils';

const id = generateShortUUID();
console.log(id); // 'k5n8p7q2r1'
```

**Since**: v4.1.1

---

#### generateId

**Signature**: `generateId(prefix?: string): string`

**Description**: Generates a generic ID with optional prefix.

**Parameters**:
- `prefix` (string, optional) - Prefix for the ID (default: 'id')

**Returns**: Prefixed UUID string

**Example**:
```javascript
import { generateId } from 'unrdf/utils';

const id = generateId('user');
console.log(id); // 'user-550e8400-e29b-41d4-a716-446655440000'
```

**Since**: v4.1.1

---

### Hash-Based IDs

#### generateHashId

**Signature**: `generateHashId(input: string): string`

**Description**: Generates a deterministic hash-based ID from input string.

**Parameters**:
- `input` (string) - Input string to hash

**Returns**: SHA-256 hash as hexadecimal string

**Example**:
```javascript
import { generateHashId } from 'unrdf/utils';

const id = generateHashId('alice@example.org');
console.log(id); // 'e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855'
```

**Since**: v4.1.1

---

#### createHashIRI

**Signature**: `createHashIRI(content: string, baseIRI?: string, algorithm?: string): string`

**Description**: Creates a hash-based IRI from content.

**Parameters**:
- `content` (string) - Content to hash
- `baseIRI` (string, optional) - Base IRI (default: 'http://example.org/hash/')
- `algorithm` (string, optional) - Hash algorithm (default: 'sha256')

**Returns**: Hash-based IRI

**Example**:
```javascript
import { createHashIRI } from 'unrdf/utils';

const iri = createHashIRI('alice', 'http://example.org/user/');
console.log(iri);
// 'http://example.org/user/sha256/2bd806c97f0e00af1a1fc3328fa763a9269723c8db8fac4f93af71db186d6e90'
```

**Since**: v4.1.1

---

#### createHashNamedNode

**Signature**: `createHashNamedNode(content: string, baseIRI?: string, algorithm?: string): NamedNode`

**Description**: Creates a named node with hash-based IRI.

**Parameters**:
- `content` (string) - Content to hash
- `baseIRI` (string, optional) - Base IRI (default: 'http://example.org/hash/')
- `algorithm` (string, optional) - Hash algorithm (default: 'sha256')

**Returns**: N3 NamedNode with hash-based IRI

**Example**:
```javascript
import { createHashNamedNode } from 'unrdf/utils';

const node = createHashNamedNode('alice');
console.log(node.value);
```

**Since**: v4.1.1

---

### Blank Node IDs

#### generateRandomBNodeId

**Signature**: `generateRandomBNodeId(length?: number): string`

**Description**: Generates a random blank node ID.

**Parameters**:
- `length` (number, optional) - Length of random ID (default: 16)

**Returns**: Random hexadecimal string

**Example**:
```javascript
import { generateRandomBNodeId } from 'unrdf/utils';

const id = generateRandomBNodeId();
console.log(id); // 'a7f3c9e2b1d4f8e6'
```

**Since**: v4.1.1

---

#### generateDeterministicBNodeId

**Signature**: `generateDeterministicBNodeId(content: string, length?: number): string`

**Description**: Generates a deterministic blank node ID from content hash.

**Parameters**:
- `content` (string) - Content to hash
- `length` (number, optional) - Length of hash to use (default: 16)

**Returns**: Deterministic hexadecimal string

**Example**:
```javascript
import { generateDeterministicBNodeId } from 'unrdf/utils';

const id = generateDeterministicBNodeId('alice');
console.log(id); // '2bd806c97f0e00af' (always same for 'alice')
```

**Since**: v4.1.1

---

#### createRandomBlankNode

**Signature**: `createRandomBlankNode(length?: number): BlankNode`

**Description**: Creates a blank node with random ID.

**Parameters**:
- `length` (number, optional) - Length of random ID (default: 16)

**Returns**: N3 BlankNode with random ID

**Example**:
```javascript
import { createRandomBlankNode } from 'unrdf/utils';

const bn = createRandomBlankNode();
console.log(bn.value); // '_:a7f3c9e2b1d4f8e6'
```

**Since**: v4.1.1

---

#### createDeterministicBlankNode

**Signature**: `createDeterministicBlankNode(content: string, length?: number): BlankNode`

**Description**: Creates a blank node with deterministic ID based on content.

**Parameters**:
- `content` (string) - Content to hash for ID
- `length` (number, optional) - Length of hash to use (default: 16)

**Returns**: N3 BlankNode with deterministic ID

**Example**:
```javascript
import { createDeterministicBlankNode } from 'unrdf/utils';

const bn = createDeterministicBlankNode('alice');
console.log(bn.value); // '_:2bd806c97f0e00af' (always same for 'alice')
```

**Since**: v4.1.1

---

#### makeBNodeGenerator

**Signature**: `makeBNodeGenerator(prefix?: string): Function`

**Description**: Creates a deterministic blank node generator with sequential IDs.

**Parameters**:
- `prefix` (string, optional) - Prefix for blank node IDs (default: 'bn')

**Returns**: Function that generates sequential blank nodes

**Example**:
```javascript
import { makeBNodeGenerator } from 'unrdf/utils';

const generateBNode = makeBNodeGenerator('b');
const bn1 = generateBNode(); // BlankNode('b0')
const bn2 = generateBNode(); // BlankNode('b1')
const bn3 = generateBNode(); // BlankNode('b2')
```

**Since**: v4.1.1

---

### Skolemization

#### skolemize

**Signature**: `skolemize(id: string, baseIRI?: string): string`

**Description**: Creates a skolemized IRI for a blank node (converts blank node to IRI).

**Parameters**:
- `id` (string) - Blank node identifier
- `baseIRI` (string, optional) - Base IRI for skolemization (default: 'http://example.org/.well-known/genid/')

**Returns**: Skolemized IRI

**Example**:
```javascript
import { skolemize } from 'unrdf/utils';

const iri = skolemize('b1');
console.log(iri); // 'http://example.org/.well-known/genid/b1'
```

**Since**: v4.1.1

---

### Named Node Creation

#### createUUIDNamedNode

**Signature**: `createUUIDNamedNode(baseIRI?: string): NamedNode`

**Description**: Creates a named node with UUID-based IRI.

**Parameters**:
- `baseIRI` (string, optional) - Base IRI (default: 'http://example.org/id/')

**Returns**: N3 NamedNode with UUID IRI

**Example**:
```javascript
import { createUUIDNamedNode } from 'unrdf/utils';

const node = createUUIDNamedNode('http://example.org/user/');
console.log(node.value);
// 'http://example.org/user/550e8400-e29b-41d4-a716-446655440000'
```

**Since**: v4.1.1

---

#### createShortUUIDNamedNode

**Signature**: `createShortUUIDNamedNode(baseIRI?: string): NamedNode`

**Description**: Creates a named node with short UUID-based IRI.

**Parameters**:
- `baseIRI` (string, optional) - Base IRI (default: 'http://example.org/id/')

**Returns**: N3 NamedNode with short UUID IRI

**Example**:
```javascript
import { createShortUUIDNamedNode } from 'unrdf/utils';

const node = createShortUUIDNamedNode();
console.log(node.value); // 'http://example.org/id/k5n8p7q2r1'
```

**Since**: v4.1.1

---

### Timestamp IDs

#### generateTimestampId

**Signature**: `generateTimestampId(prefix?: string): string`

**Description**: Generates a timestamp-based ID.

**Parameters**:
- `prefix` (string, optional) - Prefix for the ID (default: 'ts')

**Returns**: Timestamp-based ID string

**Example**:
```javascript
import { generateTimestampId } from 'unrdf/utils';

const id = generateTimestampId('event');
console.log(id); // 'event1701234567890'
```

**Since**: v4.1.1

---

### Counter IDs

#### makeCounterIdGenerator

**Signature**: `makeCounterIdGenerator(prefix?: string): Function`

**Description**: Creates a counter-based ID generator with sequential numbers.

**Parameters**:
- `prefix` (string, optional) - Prefix for IDs (default: 'id')

**Returns**: Function that generates sequential IDs

**Example**:
```javascript
import { makeCounterIdGenerator } from 'unrdf/utils';

const generateId = makeCounterIdGenerator('item');
const id1 = generateId(); // 'item0'
const id2 = generateId(); // 'item1'
const id3 = generateId(); // 'item2'
```

**Since**: v4.1.1

---

### Stable IDs

#### generateStableId

**Signature**: `generateStableId(...values: any[]): string`

**Description**: Generates a stable deterministic ID from multiple values.

**Parameters**:
- `...values` (any[]) - Values to combine for ID generation

**Returns**: Deterministic ID based on input values

**Example**:
```javascript
import { generateStableId } from 'unrdf/utils';

const id = generateStableId('alice', 'example.org', 30);
console.log(id); // '7e8c9a1b2d3f4e5a' (always same for same inputs)
```

**Since**: v4.1.1

---

#### createStableNamedNode

**Signature**: `createStableNamedNode(baseIRI: string, ...values: any[]): NamedNode`

**Description**: Creates a named node with stable deterministic IRI based on values.

**Parameters**:
- `baseIRI` (string) - Base IRI
- `...values` (any[]) - Values to combine for ID generation

**Returns**: N3 NamedNode with stable IRI

**Example**:
```javascript
import { createStableNamedNode } from 'unrdf/utils';

const node = createStableNamedNode('http://example.org/user/', 'alice', 30);
console.log(node.value);
// 'http://example.org/user/7e8c9a1b2d3f4e5a' (always same for same inputs)
```

**Since**: v4.1.1

---

## Namespace Management

### createNamespaceId

**Signature**: `createNamespaceId(namespace: string, localName: string): string`

**Description**: Creates a namespaced IRI by combining namespace and local name.

**Parameters**:
- `namespace` (string) - Namespace IRI
- `localName` (string) - Local name

**Returns**: Full IRI

**Example**:
```javascript
import { createNamespaceId } from 'unrdf/utils';

const iri = createNamespaceId('http://xmlns.com/foaf/0.1/', 'name');
console.log(iri); // 'http://xmlns.com/foaf/0.1/name'
```

**Since**: v4.1.1

---

### createNamespaceNamedNode

**Signature**: `createNamespaceNamedNode(namespace: string, localName: string): NamedNode`

**Description**: Creates a named node with namespaced IRI.

**Parameters**:
- `namespace` (string) - Namespace IRI
- `localName` (string) - Local name

**Returns**: N3 NamedNode with namespaced IRI

**Example**:
```javascript
import { createNamespaceNamedNode } from 'unrdf/utils';

const node = createNamespaceNamedNode('http://xmlns.com/foaf/0.1/', 'name');
console.log(node.value); // 'http://xmlns.com/foaf/0.1/name'
```

**Since**: v4.1.1

---

### extractLocalName

**Signature**: `extractLocalName(iri: string): string`

**Description**: Extracts the local name from an IRI (part after last # or /).

**Parameters**:
- `iri` (string) - IRI to extract from

**Returns**: Local name

**Example**:
```javascript
import { extractLocalName } from 'unrdf/utils';

const localName = extractLocalName('http://xmlns.com/foaf/0.1/name');
console.log(localName); // 'name'

const localName2 = extractLocalName('http://www.w3.org/1999/02/22-rdf-syntax-ns#type');
console.log(localName2); // 'type'
```

**Since**: v4.1.1

---

### extractNamespace

**Signature**: `extractNamespace(iri: string): string`

**Description**: Extracts the namespace from an IRI (part before last # or /).

**Parameters**:
- `iri` (string) - IRI to extract from

**Returns**: Namespace

**Example**:
```javascript
import { extractNamespace } from 'unrdf/utils';

const namespace = extractNamespace('http://xmlns.com/foaf/0.1/name');
console.log(namespace); // 'http://xmlns.com/foaf/0.1/'

const namespace2 = extractNamespace('http://www.w3.org/1999/02/22-rdf-syntax-ns#type');
console.log(namespace2); // 'http://www.w3.org/1999/02/22-rdf-syntax-ns#'
```

**Since**: v4.1.1

---

### Common Vocabularies

**Constant**: `COMMON_VOCABULARIES`

**Description**: Object containing common RDF vocabulary namespace URIs.

**Properties**:
- `RDF` - 'http://www.w3.org/1999/02/22-rdf-syntax-ns#'
- `RDFS` - 'http://www.w3.org/2000/01/rdf-schema#'
- `OWL` - 'http://www.w3.org/2002/07/owl#'
- `XSD` - 'http://www.w3.org/2001/XMLSchema#'
- `DC` - 'http://purl.org/dc/elements/1.1/'
- `DCTERMS` - 'http://purl.org/dc/terms/'
- `FOAF` - 'http://xmlns.com/foaf/0.1/'
- `SKOS` - 'http://www.w3.org/2004/02/skos/core#'
- `SCHEMA` - 'https://schema.org/'
- `PROV` - 'http://www.w3.org/ns/prov#'
- `SHACL` - 'http://www.w3.org/ns/shacl#'
- And more...

**Example**:
```javascript
import { COMMON_VOCABULARIES } from 'unrdf/utils';

console.log(COMMON_VOCABULARIES.FOAF);
// 'http://xmlns.com/foaf/0.1/'
```

**Since**: v4.1.1

---

### Common Prefixes

**Constant**: `COMMON_PREFIXES`

**Description**: Object containing common prefix-to-namespace mappings for serialization.

**Example**:
```javascript
import { COMMON_PREFIXES } from 'unrdf/utils';

console.log(COMMON_PREFIXES);
// { rdf: 'http://...', rdfs: '...', foaf: '...', ... }
```

**Since**: v4.1.1

---

## Graph Operations

### getObjects

**Signature**: `getObjects(store: Store, subject: string | NamedNode, predicate: string | NamedNode): Term[]`

**Description**: Gets all objects for a given subject-predicate pair.

**Parameters**:
- `store` (Store) - RDF store to query
- `subject` (string | NamedNode) - Subject IRI or NamedNode
- `predicate` (string | NamedNode) - Predicate IRI or NamedNode

**Returns**: Array of object terms

**Example**:
```javascript
import { getObjects, Store } from 'unrdf/utils';

const store = new Store();
// ... add quads

const objects = getObjects(store, 'http://example.org/alice', 'http://xmlns.com/foaf/0.1/knows');
console.log('Alice knows:', objects.map(o => o.value));
```

**Since**: v4.1.1

---

### getSubjects

**Signature**: `getSubjects(store: Store, predicate: string | NamedNode, object: string | Term): Term[]`

**Description**: Gets all subjects for a given predicate-object pair.

**Parameters**:
- `store` (Store) - RDF store to query
- `predicate` (string | NamedNode) - Predicate IRI or NamedNode
- `object` (string | Term) - Object value or term

**Returns**: Array of subject terms

**Example**:
```javascript
import { getSubjects } from 'unrdf/utils';

const subjects = getSubjects(store, 'http://www.w3.org/1999/02/22-rdf-syntax-ns#type', 'http://xmlns.com/foaf/0.1/Person');
console.log('People:', subjects.map(s => s.value));
```

**Since**: v4.1.1

---

### getPredicates

**Signature**: `getPredicates(store: Store, subject: string | NamedNode, object: string | Term): Term[]`

**Description**: Gets all predicates for a given subject-object pair.

**Parameters**:
- `store` (Store) - RDF store to query
- `subject` (string | NamedNode) - Subject IRI or NamedNode
- `object` (string | Term) - Object value or term

**Returns**: Array of predicate terms

**Example**:
```javascript
import { getPredicates } from 'unrdf/utils';

const predicates = getPredicates(store, 'http://example.org/alice', 'http://example.org/bob');
console.log('Relationships between Alice and Bob:', predicates.map(p => p.value));
```

**Since**: v4.1.1

---

### isA

**Signature**: `isA(store: Store, subject: string | NamedNode, typeIRI: string | NamedNode): boolean`

**Description**: Checks if a subject has a specific rdf:type.

**Parameters**:
- `store` (Store) - RDF store to query
- `subject` (string | NamedNode) - Subject IRI or NamedNode
- `typeIRI` (string | NamedNode) - Type IRI or NamedNode

**Returns**: True if subject has the specified type

**Example**:
```javascript
import { isA } from 'unrdf/utils';

const isPerson = isA(store, 'http://example.org/alice', 'http://xmlns.com/foaf/0.1/Person');
console.log('Alice is a Person:', isPerson);
```

**Since**: v4.1.1

---

### getTypes

**Signature**: `getTypes(store: Store, subject: string | NamedNode): string[]`

**Description**: Gets all rdf:type values for a subject.

**Parameters**:
- `store` (Store) - RDF store to query
- `subject` (string | NamedNode) - Subject IRI or NamedNode

**Returns**: Array of type IRIs

**Example**:
```javascript
import { getTypes } from 'unrdf/utils';

const types = getTypes(store, 'http://example.org/alice');
console.log('Alice types:', types);
```

**Since**: v4.1.1

---

### pluck

**Signature**: `pluck(store: Store, predicateIRI: string | NamedNode): Quad[]`

**Description**: Extracts all quads with a specific predicate across all subjects.

**Parameters**:
- `store` (Store) - RDF store to query
- `predicateIRI` (string | NamedNode) - Predicate IRI or NamedNode

**Returns**: Array of quads with the specified predicate

**Example**:
```javascript
import { pluck } from 'unrdf/utils';

const nameQuads = pluck(store, 'http://xmlns.com/foaf/0.1/name');
console.log('All name quads:', nameQuads);
```

**Since**: v4.1.1

---

### indexByPredicate

**Signature**: `indexByPredicate(store: Store, predicateIRI: string | NamedNode): Map<string, string[]>`

**Description**: Creates an index mapping subjects to their object values for a specific predicate.

**Parameters**:
- `store` (Store) - RDF store to query
- `predicateIRI` (string | NamedNode) - Predicate IRI or NamedNode

**Returns**: Map of subject IRIs to arrays of object values

**Example**:
```javascript
import { indexByPredicate } from 'unrdf/utils';

const nameIndex = indexByPredicate(store, 'http://xmlns.com/foaf/0.1/name');
console.log('Alice names:', nameIndex.get('http://example.org/alice'));
```

**Since**: v4.1.1

---

### getProperties

**Signature**: `getProperties(store: Store, subject: string | NamedNode): Map<string, string[]>`

**Description**: Gets all properties (predicate-object pairs) for a subject.

**Parameters**:
- `store` (Store) - RDF store to query
- `subject` (string | NamedNode) - Subject IRI or NamedNode

**Returns**: Map of predicate IRIs to arrays of object values

**Example**:
```javascript
import { getProperties } from 'unrdf/utils';

const properties = getProperties(store, 'http://example.org/alice');
console.log('Alice properties:', properties);
```

**Since**: v4.1.1

---

## Quality Assessment

### assessDataQuality

**Signature**: `assessDataQuality(store: Store): Object`

**Description**: Assesses the quality of RDF data in a store.

**Parameters**:
- `store` (Store) - RDF store to assess

**Returns**: Quality assessment object with metrics

**Example**:
```javascript
import { assessDataQuality } from 'unrdf/utils';

const quality = assessDataQuality(store);
console.log('Data quality score:', quality.score);
console.log('Issues found:', quality.issues);
```

**Since**: v4.1.1

---

### findBrokenLinks

**Signature**: `findBrokenLinks(store: Store): string[]`

**Description**: Finds broken IRI references (IRIs that are referenced but not defined).

**Parameters**:
- `store` (Store) - RDF store to check

**Returns**: Array of broken IRI strings

**Example**:
```javascript
import { findBrokenLinks } from 'unrdf/utils';

const broken = findBrokenLinks(store);
console.log('Broken links:', broken);
```

**Since**: v4.1.1

---

### findDanglingReferences

**Signature**: `findDanglingReferences(store: Store): string[]`

**Description**: Finds dangling references (orphaned nodes with no incoming edges).

**Parameters**:
- `store` (Store) - RDF store to check

**Returns**: Array of dangling node IRIs

**Example**:
```javascript
import { findDanglingReferences } from 'unrdf/utils';

const dangling = findDanglingReferences(store);
console.log('Dangling references:', dangling);
```

**Since**: v4.1.1

---

### suggestImprovements

**Signature**: `suggestImprovements(store: Store): string[]`

**Description**: Suggests quality improvements for RDF data.

**Parameters**:
- `store` (Store) - RDF store to analyze

**Returns**: Array of improvement suggestion strings

**Example**:
```javascript
import { suggestImprovements } from 'unrdf/utils';

const suggestions = suggestImprovements(store);
suggestions.forEach(s => console.log('Suggestion:', s));
```

**Since**: v4.1.1

---

## Related Documentation

- [Core RDF API Reference](./core-rdf-api.md) - Core RDF operations
- [Knowledge Hooks API Reference](./knowledge-hooks-api.md) - Hook system
- [Composables API Reference](./composables-api.md) - High-level composables
- [Schemas Reference](./schemas.md) - Zod validation schemas
