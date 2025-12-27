# ID Utils

Blank nodes, UUIDs, and IRI generation utilities.

## Overview

The `id-utils` module provides utilities for generating unique identifiers, managing blank nodes, and creating stable IRIs for RDF data.

## Functions

### `makeBNodeGenerator(prefix?)`

Creates a deterministic blank node generator.

```javascript
import { makeBNodeGenerator } from 'unrdf/utils';

const generator = makeBNodeGenerator('bn');
const bnode1 = generator(); // BlankNode with ID 'bn0'
const bnode2 = generator(); // BlankNode with ID 'bn1'
```

**Parameters:**
- `prefix` (string, optional) - Prefix for blank node IDs (default: 'bn')

**Returns:** Function that generates BlankNode instances

### `skolemize(id, baseIRI?)`

Creates a skolemized IRI for a blank node.

```javascript
import { skolemize } from 'unrdf/utils';

const iri = skolemize('bn0', 'http://example.org/.well-known/genid/');
// Returns: 'http://example.org/.well-known/genid/bn0'
```

**Parameters:**
- `id` (string) - Blank node ID
- `baseIRI` (string, optional) - Base IRI for skolemization

**Returns:** string

### `generateUUID()`

Generates a random UUID.

```javascript
import { generateUUID } from 'unrdf/utils';

const uuid = generateUUID();
// Returns: '550e8400-e29b-41d4-a716-446655440000'
```

**Returns:** string

### `generateShortUUID()`

Generates a short UUID.

```javascript
import { generateShortUUID } from 'unrdf/utils';

const shortUuid = generateShortUUID();
// Returns: 'abc123def456'
```

**Returns:** string

### `createHashIRI(input, baseIRI?)`

Creates a hash-based IRI from input.

```javascript
import { createHashIRI } from 'unrdf/utils';

const iri = createHashIRI('Hello World', 'http://example.org/hash/');
// Returns: 'http://example.org/hash/a591a6d40bf420404a011733cfb7b190d62c65bf0bcda32b57b277d9ad9f146e'
```

**Parameters:**
- `input` (string) - Input to hash
- `baseIRI` (string, optional) - Base IRI for hash

**Returns:** string

### `createHashNamedNode(input, baseIRI?)`

Creates a hash-based NamedNode from input.

```javascript
import { createHashNamedNode } from 'unrdf/utils';

const namedNode = createHashNamedNode('Hello World', 'http://example.org/hash/');
// Returns: NamedNode with hash-based IRI
```

**Parameters:**
- `input` (string) - Input to hash
- `baseIRI` (string, optional) - Base IRI for hash

**Returns:** NamedNode

### `createNamespaceId(namespace, localName)`

Creates a namespace-scoped ID.

```javascript
import { createNamespaceId } from 'unrdf/utils';

const id = createNamespaceId('http://example.org/', 'person1');
// Returns: 'http://example.org/person1'
```

**Parameters:**
- `namespace` (string) - Namespace IRI
- `localName` (string) - Local name

**Returns:** string

### `extractLocalName(iri)`

Extracts local name from IRI.

```javascript
import { extractLocalName } from 'unrdf/utils';

const localName = extractLocalName('http://example.org/person1');
// Returns: 'person1'
```

**Parameters:**
- `iri` (string) - IRI to extract from

**Returns:** string

### `extractNamespace(iri)`

Extracts namespace from IRI.

```javascript
import { extractNamespace } from 'unrdf/utils';

const namespace = extractNamespace('http://example.org/person1');
// Returns: 'http://example.org/'
```

**Parameters:**
- `iri` (string) - IRI to extract from

**Returns:** string

### `isBlankNodeIRI(iri)`

Checks if an IRI represents a blank node.

```javascript
import { isBlankNodeIRI } from 'unrdf/utils';

const isBlank = isBlankNodeIRI('http://example.org/.well-known/genid/bn0');
// Returns: boolean
```

**Parameters:**
- `iri` (string) - IRI to check

**Returns:** boolean

### `blankNodeIdToIRI(id, baseIRI?)`

Converts blank node ID to IRI.

```javascript
import { blankNodeIdToIRI } from 'unrdf/utils';

const iri = blankNodeIdToIRI('bn0');
// Returns: 'http://example.org/.well-known/genid/bn0'
```

**Parameters:**
- `id` (string) - Blank node ID
- `baseIRI` (string, optional) - Base IRI

**Returns:** string

### `IRItoBlankNodeId(iri)`

Converts IRI to blank node ID.

```javascript
import { IRItoBlankNodeId } from 'unrdf/utils';

const id = IRItoBlankNodeId('http://example.org/.well-known/genid/bn0');
// Returns: 'bn0'
```

**Parameters:**
- `iri` (string) - IRI to convert

**Returns:** string

### `generateStableId(input, baseIRI?)`

Generates a stable ID from input.

```javascript
import { generateStableId } from 'unrdf/utils';

const stableId = generateStableId('Hello World', 'http://example.org/stable/');
// Returns: Deterministic ID based on input
```

**Parameters:**
- `input` (string) - Input to generate ID from
- `baseIRI` (string, optional) - Base IRI

**Returns:** string

### `createStableNamedNode(input, baseIRI?)`

Creates a stable NamedNode from input.

```javascript
import { createStableNamedNode } from 'unrdf/utils';

const namedNode = createStableNamedNode('Hello World', 'http://example.org/stable/');
// Returns: NamedNode with stable IRI
```

**Parameters:**
- `input` (string) - Input to generate ID from
- `baseIRI` (string, optional) - Base IRI

**Returns:** NamedNode

## ID Generation Strategies

### Random IDs
- **UUID**: Standard UUID v4 format
- **Short UUID**: Compact random identifier
- **Use Case**: When uniqueness is required but determinism is not

### Hash-based IDs
- **Deterministic**: Same input always produces same ID
- **Collision Resistant**: Different inputs produce different IDs
- **Use Case**: When you need stable, reproducible identifiers

### Namespace-scoped IDs
- **Hierarchical**: Organized within namespaces
- **Human Readable**: Meaningful local names
- **Use Case**: When you need organized, predictable identifiers

### Blank Node Management
- **Skolemization**: Convert blank nodes to IRIs
- **ID Extraction**: Convert between blank node IDs and IRIs
- **Use Case**: When working with blank nodes in RDF

## Examples

### Basic ID Generation

```javascript
import { 
  generateUUID, generateShortUUID, 
  createHashIRI, createNamespaceId 
} from 'unrdf/utils';

// Generate random IDs
const uuid = generateUUID();
const shortUuid = generateShortUUID();

console.log('UUID:', uuid);
console.log('Short UUID:', shortUuid);

// Generate hash-based ID
const hashId = createHashIRI('Hello World');
console.log('Hash ID:', hashId);

// Generate namespace-scoped ID
const nsId = createNamespaceId('http://example.org/', 'person1');
console.log('Namespace ID:', nsId);
```

### Blank Node Management

```javascript
import { 
  makeBNodeGenerator, skolemize, 
  isBlankNodeIRI, blankNodeIdToIRI, IRItoBlankNodeId 
} from 'unrdf/utils';

// Create blank node generator
const generator = makeBNodeGenerator('person');
const bnode1 = generator(); // person0
const bnode2 = generator(); // person1

console.log('Blank nodes:', bnode1.value, bnode2.value);

// Skolemize blank nodes
const iri1 = skolemize(bnode1.value);
const iri2 = skolemize(bnode2.value);

console.log('Skolemized IRIs:', iri1, iri2);

// Check if IRI is blank node
console.log('Is blank node IRI:', isBlankNodeIRI(iri1));

// Convert between formats
const backToId = IRItoBlankNodeId(iri1);
const backToIri = blankNodeIdToIRI(backToId);

console.log('Round trip:', backToId, backToIri);
```

### Stable ID Generation

```javascript
import { 
  generateStableId, createStableNamedNode,
  createHashNamedNode 
} from 'unrdf/utils';

// Generate stable IDs
const stableId1 = generateStableId('Alice', 'http://example.org/people/');
const stableId2 = generateStableId('Alice', 'http://example.org/people/');
const stableId3 = generateStableId('Bob', 'http://example.org/people/');

console.log('Stable IDs:', stableId1, stableId2, stableId3);
console.log('Same input, same ID:', stableId1 === stableId2); // true
console.log('Different input, different ID:', stableId1 !== stableId3); // true

// Create stable NamedNodes
const aliceNode = createStableNamedNode('Alice', 'http://example.org/people/');
const bobNode = createStableNamedNode('Bob', 'http://example.org/people/');

console.log('Alice node:', aliceNode.value);
console.log('Bob node:', bobNode.value);
```

### IRI Analysis

```javascript
import { 
  extractLocalName, extractNamespace,
  createNamespaceId 
} from 'unrdf/utils';

// Analyze IRIs
const iri = 'http://example.org/people/alice';
const localName = extractLocalName(iri);
const namespace = extractNamespace(iri);

console.log('IRI:', iri);
console.log('Local name:', localName);
console.log('Namespace:', namespace);

// Reconstruct IRI
const reconstructed = createNamespaceId(namespace, localName);
console.log('Reconstructed:', reconstructed);
console.log('Match:', iri === reconstructed); // true
```

### Batch ID Generation

```javascript
import { 
  makeBNodeGenerator, generateStableId,
  createNamespaceId 
} from 'unrdf/utils';

// Generate IDs for a batch of data
const people = ['Alice', 'Bob', 'Charlie'];
const baseNamespace = 'http://example.org/people/';

// Method 1: Stable IDs
const stableIds = people.map(name => 
  generateStableId(name, baseNamespace)
);

// Method 2: Namespace-scoped IDs
const nsIds = people.map((name, index) => 
  createNamespaceId(baseNamespace, name.toLowerCase())
);

// Method 3: Blank nodes
const generator = makeBNodeGenerator('person');
const blankNodes = people.map(() => generator());

console.log('Stable IDs:', stableIds);
console.log('Namespace IDs:', nsIds);
console.log('Blank nodes:', blankNodes.map(bn => bn.value));
```

### ID Validation and Conversion

```javascript
import { 
  isBlankNodeIRI, blankNodeIdToIRI, IRItoBlankNodeId,
  extractLocalName, extractNamespace 
} from 'unrdf/utils';

function analyzeIRI(iri) {
  const analysis = {
    iri,
    isBlankNode: isBlankNodeIRI(iri),
    localName: extractLocalName(iri),
    namespace: extractNamespace(iri)
  };
  
  if (analysis.isBlankNode) {
    analysis.blankNodeId = IRItoBlankNodeId(iri);
  }
  
  return analysis;
}

// Analyze different types of IRIs
const iris = [
  'http://example.org/people/alice',
  'http://example.org/.well-known/genid/bn0',
  'urn:uuid:550e8400-e29b-41d4-a716-446655440000'
];

iris.forEach(iri => {
  const analysis = analyzeIRI(iri);
  console.log('Analysis:', analysis);
});
```

## Performance Notes

- **Hash Generation**: Uses efficient SHA-256 hashing
- **UUID Generation**: Uses cryptographically secure random generation
- **String Operations**: Optimized for frequent IRI manipulation
- **Memory Usage**: Minimal memory overhead for ID operations

## Security Considerations

- **UUID Security**: Uses cryptographically secure random generation
- **Hash Security**: SHA-256 provides strong collision resistance
- **ID Predictability**: Hash-based IDs are deterministic but not easily guessable

## Related Modules

- [Term Utils](./term-utils.md) - Term creation and manipulation
- [Namespace Utils](./namespace-utils.md) - Namespace management
- [Validation Utils](./validation-utils.md) - IRI validation
