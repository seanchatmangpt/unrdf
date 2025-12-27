# Namespace Utils

RDF vocabulary and namespace management utilities.

## Overview

The `namespace-utils` module provides utilities for managing RDF namespaces, vocabularies, and IRI operations. It includes a `NamespaceManager` class for centralized namespace management and predefined common RDF namespaces.

## NamespaceManager Class

### Constructor

```javascript
import { NamespaceManager } from 'unrdf/utils';

const manager = new NamespaceManager();
```

### Methods

#### `register(prefix, namespace)`

Registers a namespace with a prefix.

```javascript
manager.register('ex', 'http://example.org/');
manager.register('foaf', 'http://xmlns.com/foaf/0.1/');
```

**Parameters:**
- `prefix` (string) - Namespace prefix
- `namespace` (string) - Namespace IRI

#### `get(prefix)`

Gets a namespace by prefix.

```javascript
const namespace = manager.get('ex');
// Returns: 'http://example.org/'
```

**Parameters:**
- `prefix` (string) - Namespace prefix

**Returns:** string | undefined

#### `expand(term)`

Expands a prefixed term to full IRI.

```javascript
const iri = manager.expand('ex:person');
// Returns: 'http://example.org/person'
```

**Parameters:**
- `term` (string) - Prefixed term (e.g., 'ex:person')

**Returns:** string

#### `contract(iri)`

Contracts a full IRI to prefixed form.

```javascript
const prefixed = manager.contract('http://example.org/person');
// Returns: 'ex:person'
```

**Parameters:**
- `iri` (string) - Full IRI

**Returns:** string

#### `getPrefix(iri)`

Gets the prefix for a namespace IRI.

```javascript
const prefix = manager.getPrefix('http://example.org/');
// Returns: 'ex'
```

**Parameters:**
- `iri` (string) - Namespace IRI

**Returns:** string | undefined

#### `getVocabularyTerm(vocabulary, term)`

Gets a term from a predefined vocabulary.

```javascript
const term = manager.getVocabularyTerm('foaf', 'Person');
// Returns: 'http://xmlns.com/foaf/0.1/Person'
```

**Parameters:**
- `vocabulary` (string) - Vocabulary name
- `term` (string) - Term name

**Returns:** string

#### `listNamespaces()`

Lists all registered namespaces.

```javascript
const namespaces = manager.listNamespaces();
// Returns: { 'ex': 'http://example.org/', 'foaf': 'http://xmlns.com/foaf/0.1/' }
```

**Returns:** Object

#### `listVocabularies()`

Lists all available vocabularies.

```javascript
const vocabularies = manager.listVocabularies();
// Returns: ['foaf', 'dc', 'rdf', 'rdfs', 'owl', 'skos', 'schema', 'vcard']
```

**Returns:** Array<string>

## Standalone Functions

### `createNamespaceManager()`

Creates a new NamespaceManager with common namespaces pre-registered.

```javascript
import { createNamespaceManager } from 'unrdf/utils';

const manager = createNamespaceManager();
// Includes: rdf, rdfs, owl, foaf, dc, skos, schema, vcard
```

**Returns:** NamespaceManager

### `expandPrefixedTerm(term, namespaces)`

Expands a prefixed term using a namespace map.

```javascript
import { expandPrefixedTerm } from 'unrdf/utils';

const namespaces = {
  'ex': 'http://example.org/',
  'foaf': 'http://xmlns.com/foaf/0.1/'
};

const iri = expandPrefixedTerm('ex:person', namespaces);
// Returns: 'http://example.org/person'
```

**Parameters:**
- `term` (string) - Prefixed term
- `namespaces` (Object) - Namespace map

**Returns:** string

### `contractIRI(iri, namespaces)`

Contracts an IRI to prefixed form using a namespace map.

```javascript
import { contractIRI } from 'unrdf/utils';

const namespaces = {
  'ex': 'http://example.org/',
  'foaf': 'http://xmlns.com/foaf/0.1/'
};

const prefixed = contractIRI('http://example.org/person', namespaces);
// Returns: 'ex:person'
```

**Parameters:**
- `iri` (string) - Full IRI
- `namespaces` (Object) - Namespace map

**Returns:** string

### `getNamespacePrefix(iri, namespaces)`

Gets the prefix for a namespace IRI.

```javascript
import { getNamespacePrefix } from 'unrdf/utils';

const namespaces = {
  'ex': 'http://example.org/',
  'foaf': 'http://xmlns.com/foaf/0.1/'
};

const prefix = getNamespacePrefix('http://example.org/', namespaces);
// Returns: 'ex'
```

**Parameters:**
- `iri` (string) - Namespace IRI
- `namespaces` (Object) - Namespace map

**Returns:** string | undefined

### `validateNamespace(namespace)`

Validates a namespace IRI.

```javascript
import { validateNamespace } from 'unrdf/utils';

const isValid = validateNamespace('http://example.org/');
// Returns: boolean
```

**Parameters:**
- `namespace` (string) - Namespace IRI

**Returns:** boolean

### `normalizeNamespace(namespace)`

Normalizes a namespace IRI.

```javascript
import { normalizeNamespace } from 'unrdf/utils';

const normalized = normalizeNamespace('http://example.org');
// Returns: 'http://example.org/'
```

**Parameters:**
- `namespace` (string) - Namespace IRI

**Returns:** string

## Predefined Namespaces

The module includes predefined common RDF namespaces:

- **rdf**: `http://www.w3.org/1999/02/22-rdf-syntax-ns#`
- **rdfs**: `http://www.w3.org/2000/01/rdf-schema#`
- **owl**: `http://www.w3.org/2002/07/owl#`
- **foaf**: `http://xmlns.com/foaf/0.1/`
- **dc**: `http://purl.org/dc/elements/1.1/`
- **skos**: `http://www.w3.org/2004/02/skos/core#`
- **schema**: `http://schema.org/`
- **vcard**: `http://www.w3.org/2006/vcard/ns#`

## Examples

### Basic Namespace Management

```javascript
import { createNamespaceManager } from 'unrdf/utils';

// Create manager with common namespaces
const manager = createNamespaceManager();

// Register custom namespaces
manager.register('ex', 'http://example.org/');
manager.register('my', 'http://mycompany.com/ns/');

// Expand prefixed terms
console.log(manager.expand('ex:person')); // http://example.org/person
console.log(manager.expand('foaf:Person')); // http://xmlns.com/foaf/0.1/Person
console.log(manager.expand('my:Employee')); // http://mycompany.com/ns/Employee

// Contract IRIs
console.log(manager.contract('http://example.org/person')); // ex:person
console.log(manager.contract('http://xmlns.com/foaf/0.1/Person')); // foaf:Person
```

### Vocabulary Term Access

```javascript
import { createNamespaceManager } from 'unrdf/utils';

const manager = createNamespaceManager();

// Access vocabulary terms
const personClass = manager.getVocabularyTerm('foaf', 'Person');
const nameProperty = manager.getVocabularyTerm('foaf', 'name');
const typeProperty = manager.getVocabularyTerm('rdf', 'type');

console.log('Person class:', personClass);
console.log('Name property:', nameProperty);
console.log('Type property:', typeProperty);

// List available vocabularies
const vocabularies = manager.listVocabularies();
console.log('Available vocabularies:', vocabularies);
```

### Custom Namespace Operations

```javascript
import { 
  expandPrefixedTerm, contractIRI, 
  getNamespacePrefix, validateNamespace 
} from 'unrdf/utils';

// Define custom namespaces
const namespaces = {
  'ex': 'http://example.org/',
  'my': 'http://mycompany.com/ns/',
  'data': 'http://data.example.org/'
};

// Expand terms
const terms = ['ex:person', 'my:Employee', 'data:Product'];
const expanded = terms.map(term => expandPrefixedTerm(term, namespaces));

console.log('Expanded terms:', expanded);

// Contract IRIs
const iris = [
  'http://example.org/person',
  'http://mycompany.com/ns/Employee',
  'http://data.example.org/Product'
];
const contracted = iris.map(iri => contractIRI(iri, namespaces));

console.log('Contracted IRIs:', contracted);

// Get namespace prefixes
const prefixes = iris.map(iri => getNamespacePrefix(iri, namespaces));
console.log('Namespace prefixes:', prefixes);
```

### Namespace Validation and Normalization

```javascript
import { validateNamespace, normalizeNamespace } from 'unrdf/utils';

// Validate namespaces
const testNamespaces = [
  'http://example.org/',
  'https://schema.org/',
  'urn:uuid:123',
  'invalid-namespace',
  'http://example.org' // Missing trailing slash
];

testNamespaces.forEach(ns => {
  const isValid = validateNamespace(ns);
  console.log(`Namespace "${ns}" is valid:`, isValid);
});

// Normalize namespaces
const toNormalize = [
  'http://example.org',
  'https://schema.org',
  'http://example.org/',
  'https://schema.org/'
];

toNormalize.forEach(ns => {
  const normalized = normalizeNamespace(ns);
  console.log(`"${ns}" -> "${normalized}"`);
});
```

### Working with RDF Data

```javascript
import { createNamespaceManager } from 'unrdf/utils';
import { DataFactory } from 'n3';

const manager = createNamespaceManager();
const { namedNode, quad } = DataFactory;

// Create quads using namespace manager
const personIRI = manager.expand('ex:person');
const nameIRI = manager.expand('foaf:name');
const typeIRI = manager.expand('rdf:type');

const quads = [
  quad(
    namedNode('http://example.org/alice'),
    namedNode(typeIRI),
    namedNode(personIRI)
  ),
  quad(
    namedNode('http://example.org/alice'),
    namedNode(nameIRI),
    namedNode('"Alice"')
  )
];

console.log('Created quads using namespace manager');

// Convert quads back to prefixed form
quads.forEach(q => {
  const subject = manager.contract(q.subject.value);
  const predicate = manager.contract(q.predicate.value);
  const object = manager.contract(q.object.value);
  
  console.log(`${subject} ${predicate} ${object}`);
});
```

### Batch Namespace Operations

```javascript
import { createNamespaceManager } from 'unrdf/utils';

const manager = createNamespaceManager();

// Batch register namespaces
const customNamespaces = {
  'ex': 'http://example.org/',
  'my': 'http://mycompany.com/ns/',
  'data': 'http://data.example.org/',
  'geo': 'http://www.w3.org/2003/01/geo/wgs84_pos#',
  'time': 'http://www.w3.org/2006/time#'
};

Object.entries(customNamespaces).forEach(([prefix, namespace]) => {
  manager.register(prefix, namespace);
});

// Batch expand terms
const prefixedTerms = [
  'ex:person', 'ex:organization', 'ex:event',
  'my:Employee', 'my:Department', 'my:Project',
  'data:Product', 'data:Order', 'data:Customer',
  'geo:lat', 'geo:long', 'geo:location',
  'time:instant', 'time:interval', 'time:duration'
];

const expandedTerms = prefixedTerms.map(term => ({
  original: term,
  expanded: manager.expand(term)
}));

console.log('Batch expanded terms:');
expandedTerms.forEach(({ original, expanded }) => {
  console.log(`  ${original} -> ${expanded}`);
});
```

### Namespace Analysis

```javascript
import { createNamespaceManager } from 'unrdf/utils';

const manager = createNamespaceManager();

// Analyze namespace usage
function analyzeNamespaces(iris) {
  const analysis = {
    total: iris.length,
    namespaces: {},
    unknown: []
  };
  
  iris.forEach(iri => {
    const prefixed = manager.contract(iri);
    if (prefixed === iri) {
      // Could not be contracted
      analysis.unknown.push(iri);
    } else {
      const prefix = prefixed.split(':')[0];
      analysis.namespaces[prefix] = (analysis.namespaces[prefix] || 0) + 1;
    }
  });
  
  return analysis;
}

// Test with sample IRIs
const testIRIs = [
  'http://example.org/person',
  'http://xmlns.com/foaf/0.1/Person',
  'http://www.w3.org/1999/02/22-rdf-syntax-ns#type',
  'http://schema.org/Person',
  'http://unknown.org/term'
];

const analysis = analyzeNamespaces(testIRIs);
console.log('Namespace analysis:', analysis);
```

## Performance Notes

- **Namespace Lookup**: O(1) hash table lookups for prefix resolution
- **Term Expansion**: Efficient string operations for IRI construction
- **Memory Usage**: Minimal overhead for namespace storage
- **Caching**: Built-in caching for frequently used operations

## Best Practices

### Namespace Design
- Use consistent trailing slashes in namespace IRIs
- Choose meaningful, short prefixes
- Follow established naming conventions
- Document namespace usage

### Vocabulary Selection
- Use established vocabularies when possible
- Prefer well-maintained, stable vocabularies
- Consider vocabulary overlap and conflicts
- Document custom vocabulary extensions

### IRI Management
- Use namespace managers for consistent IRI handling
- Validate namespaces before registration
- Normalize namespaces for consistency
- Handle namespace conflicts gracefully

## Related Modules

- [Term Utils](./term-utils.md) - Term creation and manipulation
- [ID Utils](./id-utils.md) - IRI generation and management
- [Validation Utils](./validation-utils.md) - IRI validation
