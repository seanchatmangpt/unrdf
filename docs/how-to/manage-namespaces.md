# How-To: Manage Namespaces

**Problem**: You need to work with RDF namespaces, expand prefixed names, compact IRIs, or manage prefix registries across your application.

## Solution

UNRDF provides namespace utilities for managing prefixes, expanding/compacting IRIs, and integrating with Turtle serialization.

### Create Namespace Manager

Use `createNamespaceManager()` for centralized namespace handling:

```javascript
import { createNamespaceManager } from 'unrdf/utils';

const ns = createNamespaceManager();

// Register common namespaces
ns.register('rdf', 'http://www.w3.org/1999/02/22-rdf-syntax-ns#');
ns.register('rdfs', 'http://www.w3.org/2000/01/rdf-schema#');
ns.register('xsd', 'http://www.w3.org/2001/XMLSchema#');
ns.register('schema', 'http://schema.org/');
ns.register('foaf', 'http://xmlns.com/foaf/0.1/');
ns.register('ex', 'http://example.org/');

// Get registered namespaces
const prefixes = ns.getPrefixes();
console.log(prefixes);
// { rdf: 'http://...', rdfs: '...', ... }
```

### Expand Prefixed Names

Convert compact prefixed names to full IRIs:

```javascript
import { expandPrefix } from 'unrdf/utils';

const ns = createNamespaceManager();
ns.register('schema', 'http://schema.org/');

// Expand prefixed name
const fullIRI = ns.expand('schema:Person');
console.log(fullIRI);
// → http://schema.org/Person

// Multiple expansions
const terms = ['schema:name', 'schema:email', 'schema:knows'];
const expanded = terms.map(t => ns.expand(t));
// → ['http://schema.org/name', 'http://schema.org/email', 'http://schema.org/knows']

// Handle undefined prefixes
try {
  ns.expand('unknown:term');  // Throws error
} catch (err) {
  console.error('Prefix not registered');
}
```

### Compact IRIs

Convert full IRIs to prefixed names:

```javascript
import { compactIRI } from 'unrdf/utils';

const ns = createNamespaceManager();
ns.register('schema', 'http://schema.org/');

// Compact full IRI
const prefixed = ns.compact('http://schema.org/Person');
console.log(prefixed);
// → schema:Person

// Compact multiple IRIs
const iris = [
  'http://schema.org/name',
  'http://schema.org/email',
  'http://example.org/alice'
];

const compacted = iris.map(iri => ns.compact(iri));
// → ['schema:name', 'schema:email', 'ex:alice']

// Handle IRIs without registered prefix
const unknown = ns.compact('http://unknown.org/term');
// Returns full IRI if no matching prefix
```

### Use with Turtle Serialization

Integrate namespace manager with Turtle output:

```javascript
import { createNamespaceManager, toTurtle, parseTurtle } from 'unrdf';

const ns = createNamespaceManager();
ns.register('ex', 'http://example.org/');
ns.register('schema', 'http://schema.org/');

const store = parseTurtle(`
  <http://example.org/alice> <http://www.w3.org/1999/02/22-rdf-syntax-ns#type> <http://schema.org/Person> .
  <http://example.org/alice> <http://schema.org/name> "Alice" .
`);

// Serialize with prefixes
const ttl = toTurtle(store, {
  prefixes: ns.getPrefixes()
});

console.log(ttl);
// Output:
// @prefix ex: <http://example.org/> .
// @prefix schema: <http://schema.org/> .
//
// ex:alice a schema:Person ;
//   schema:name "Alice" .
```

### Namespace Utilities with Composables

Combine with `usePrefixes()` composable:

```javascript
import { initStore, usePrefixes, useGraph } from 'unrdf';

await initStore();
const prefixes = usePrefixes();
const graph = useGraph();

// Register namespaces
prefixes.register('ex', 'http://example.org/');
prefixes.register('schema', 'http://schema.org/');

// Use in SPARQL queries
const results = graph.select(`
  PREFIX ex: <http://example.org/>
  PREFIX schema: <http://schema.org/>

  SELECT ?name WHERE {
    ex:alice schema:name ?name .
  }
`);

// Expand/compact with composable
const expanded = prefixes.expand('ex:alice');
const compacted = prefixes.compact('http://example.org/alice');
```

### Generate IRIs with Namespace

Create terms using namespace utilities:

```javascript
import { createNamespaceManager, generateIRI } from 'unrdf/utils';
import { DataFactory } from 'unrdf/knowledge-engine';

const ns = createNamespaceManager();
ns.register('ex', 'http://example.org/');

// Generate IRI with namespace
const baseURI = ns.getNamespaceURI('ex');
const personIRI = generateIRI(baseURI, 'alice');

// Create NamedNode
const alice = DataFactory.namedNode(personIRI);
console.log(alice.value);
// → http://example.org/alice

// Generate multiple IRIs
const ids = ['alice', 'bob', 'charlie'];
const nodes = ids.map(id =>
  DataFactory.namedNode(generateIRI(baseURI, id))
);
```

### Validate IRIs

Check IRI validity before use:

```javascript
import { isValidIRI } from 'unrdf/utils';

// Valid IRIs
console.log(isValidIRI('http://example.org/alice'));  // true
console.log(isValidIRI('https://schema.org/Person')); // true
console.log(isValidIRI('urn:uuid:12345'));            // true

// Invalid IRIs
console.log(isValidIRI('not a valid iri'));           // false
console.log(isValidIRI('http://'));                   // false
console.log(isValidIRI(''));                          // false

// Use in validation
function safeCreateNode(iri) {
  if (!isValidIRI(iri)) {
    throw new Error(`Invalid IRI: ${iri}`);
  }
  return DataFactory.namedNode(iri);
}
```

## Variations

### Common Namespace Presets

Create reusable namespace configurations:

```javascript
function createCommonNamespaces() {
  const ns = createNamespaceManager();

  // W3C Standards
  ns.register('rdf', 'http://www.w3.org/1999/02/22-rdf-syntax-ns#');
  ns.register('rdfs', 'http://www.w3.org/2000/01/rdf-schema#');
  ns.register('owl', 'http://www.w3.org/2002/07/owl#');
  ns.register('xsd', 'http://www.w3.org/2001/XMLSchema#');
  ns.register('skos', 'http://www.w3.org/2004/02/skos/core#');

  // Popular vocabularies
  ns.register('schema', 'http://schema.org/');
  ns.register('foaf', 'http://xmlns.com/foaf/0.1/');
  ns.register('dc', 'http://purl.org/dc/terms/');
  ns.register('dcat', 'http://www.w3.org/ns/dcat#');

  // SHACL
  ns.register('sh', 'http://www.w3.org/ns/shacl#');

  return ns;
}

const ns = createCommonNamespaces();
```

### Auto-Register from Turtle

Extract prefixes from Turtle documents:

```javascript
function extractPrefixes(turtleString) {
  const ns = createNamespaceManager();
  const prefixRegex = /@prefix\s+(\w+):\s+<([^>]+)>/g;

  let match;
  while ((match = prefixRegex.exec(turtleString)) !== null) {
    const [, prefix, uri] = match;
    ns.register(prefix, uri);
  }

  return ns;
}

const ttl = `
  @prefix ex: <http://example.org/> .
  @prefix schema: <http://schema.org/> .

  ex:alice a schema:Person .
`;

const ns = extractPrefixes(ttl);
console.log(ns.getPrefixes());
// { ex: 'http://example.org/', schema: 'http://schema.org/' }
```

### Namespace Builder Pattern

Create namespace-aware term builders:

```javascript
class NamespaceBuilder {
  constructor(prefix, baseURI) {
    this.prefix = prefix;
    this.baseURI = baseURI;
  }

  term(localName) {
    return DataFactory.namedNode(`${this.baseURI}${localName}`);
  }

  prefixed(localName) {
    return `${this.prefix}:${localName}`;
  }
}

// Create namespace builders
const ex = new NamespaceBuilder('ex', 'http://example.org/');
const schema = new NamespaceBuilder('schema', 'http://schema.org/');

// Use in code
const alice = ex.term('alice');
const Person = schema.term('Person');

console.log(alice.value);   // http://example.org/alice
console.log(ex.prefixed('alice'));  // ex:alice
```

### Dynamic Namespace Loading

Load namespaces from configuration:

```javascript
import { createNamespaceManager } from 'unrdf/utils';
import { readFile } from 'fs/promises';

async function loadNamespaces(configPath) {
  const config = JSON.parse(await readFile(configPath, 'utf-8'));
  const ns = createNamespaceManager();

  Object.entries(config.namespaces).forEach(([prefix, uri]) => {
    ns.register(prefix, uri);
  });

  return ns;
}

// config.json:
// {
//   "namespaces": {
//     "ex": "http://example.org/",
//     "schema": "http://schema.org/"
//   }
// }

const ns = await loadNamespaces('./config.json');
```

## Related Guides

- [How-To: Parse RDF Formats](./parse-rdf-formats.md) - Namespace in Turtle serialization
- [How-To: Generate IDs](./generate-ids.md) - IRI generation utilities
- [How-To: Use Composables](./use-composables.md) - usePrefixes() composable
- [Reference: Utilities](../reference/api/utilities.md) - Full namespace API
