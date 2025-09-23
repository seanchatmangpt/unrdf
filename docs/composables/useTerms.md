# useTerms

The `useTerms` composable provides RDF term creation and manipulation using N3 DataFactory as the single term creation method.

## Overview

```javascript
import { useTerms } from 'unrdf';

const terms = useTerms(options);
```

## API Reference

### `useTerms(options?)`

Creates a terms composable for RDF term creation.

**Parameters:**
- `options` (Object, optional): Terms configuration options
  - `baseIRI` (string, optional): Base IRI for relative IRIs (default: "http://example.org/")
  - `defaultDatatype` (string, optional): Default datatype for literals (default: "http://www.w3.org/2001/XMLSchema#string")

**Returns:**
- `Object`: Terms creation interface

### Terms Interface

#### `iri(iri)`
Creates a named node (IRI).

```javascript
const subject = terms.iri('http://example.org/person');
const relative = terms.iri('person'); // Expands to baseIRI + 'person'
```

#### `lit(value, datatype?, language?)`
Creates a literal.

```javascript
const name = terms.lit('John Doe');
const age = terms.lit(30, 'http://www.w3.org/2001/XMLSchema#integer');
const nameEn = terms.lit('John Doe', null, 'en');
```

#### `bnode(id?)`
Creates a blank node.

```javascript
const bnode1 = terms.bnode(); // Auto-generated ID
const bnode2 = terms.bnode('person1'); // Custom ID
```

#### `quad(subject, predicate, object, graph?)`
Creates a quad (statement).

```javascript
const quad = terms.quad(
  terms.iri('ex:person'),
  terms.iri('ex:name'),
  terms.lit('John Doe')
);
```

#### `defaultGraph()`
Creates a default graph term.

```javascript
const graph = terms.defaultGraph();
```

#### `getBaseIRI()`
Gets the base IRI.

```javascript
const baseIRI = terms.getBaseIRI();
console.log(baseIRI); // "http://example.org/"
```

#### `getDefaultDatatype()`
Gets the default datatype.

```javascript
const defaultDatatype = terms.getDefaultDatatype();
console.log(defaultDatatype); // "http://www.w3.org/2001/XMLSchema#string"
```

## Examples

### Basic Term Creation

```javascript
import { useTerms } from 'unrdf';

const terms = useTerms();

// Create IRIs
const person = terms.iri('http://example.org/person');
const name = terms.iri('http://example.org/name');

// Create literals
const nameValue = terms.lit('John Doe');
const ageValue = terms.lit(30, 'http://www.w3.org/2001/XMLSchema#integer');
const nameEn = terms.lit('John Doe', null, 'en');

// Create blank nodes
const bnode1 = terms.bnode(); // Auto-generated
const bnode2 = terms.bnode('person1'); // Custom ID

// Create quads
const quad = terms.quad(person, name, nameValue);
```

### Working with Base IRI

```javascript
const terms = useTerms({
  baseIRI: 'http://example.org/'
});

// Relative IRI expands to full IRI
const person = terms.iri('person'); // http://example.org/person
const name = terms.iri('name'); // http://example.org/name

// Absolute IRIs are not expanded
const external = terms.iri('http://other.org/resource'); // http://other.org/resource
```

### Different Datatypes

```javascript
const terms = useTerms();

// String literal (default)
const name = terms.lit('John Doe');

// Typed literals
const age = terms.lit(30, 'http://www.w3.org/2001/XMLSchema#integer');
const height = terms.lit(1.75, 'http://www.w3.org/2001/XMLSchema#decimal');
const birthDate = terms.lit('1990-01-01', 'http://www.w3.org/2001/XMLSchema#date');
const isActive = terms.lit(true, 'http://www.w3.org/2001/XMLSchema#boolean');

// Language-tagged literals
const nameEn = terms.lit('John Doe', null, 'en');
const nameEs = terms.lit('Juan Pérez', null, 'es');
```

### Creating Complex Quads

```javascript
const terms = useTerms();

// Simple quad
const simpleQuad = terms.quad(
  terms.iri('ex:person'),
  terms.iri('ex:name'),
  terms.lit('John Doe')
);

// Quad with graph
const graphQuad = terms.quad(
  terms.iri('ex:person'),
  terms.iri('ex:name'),
  terms.lit('John Doe'),
  terms.iri('ex:graph1')
);

// Quad with blank node
const bnodeQuad = terms.quad(
  terms.bnode('person1'),
  terms.iri('ex:name'),
  terms.lit('John Doe')
);
```

### Working with Other Composables

```javascript
import { useTerms, useStore, useGraph } from 'unrdf';

const terms = useTerms();
const store = useStore();
const graph = useGraph(store);

// Create terms and add to store
const quads = [
  terms.quad(
    terms.iri('ex:person'),
    terms.iri('rdf:type'),
    terms.iri('ex:Person')
  ),
  terms.quad(
    terms.iri('ex:person'),
    terms.iri('ex:name'),
    terms.lit('John Doe')
  )
];

store.add(quads);

// Query using the same terms
const results = await graph.select(`
  PREFIX ex: <http://example.org/>
  SELECT ?name WHERE {
    ?person a ex:Person ;
      ex:name ?name .
  }
`);
```

## Advanced Examples

### Custom Base IRI and Datatype

```javascript
const terms = useTerms({
  baseIRI: 'http://mycompany.com/',
  defaultDatatype: 'http://www.w3.org/2001/XMLSchema#string'
});

// All relative IRIs use the custom base
const employee = terms.iri('employee'); // http://mycompany.com/employee
const department = terms.iri('department'); // http://mycompany.com/department

// Default datatype is used when none specified
const name = terms.lit('John Doe'); // Uses custom default datatype
```

### Working with Different Graph Types

```javascript
const terms = useTerms();

// Default graph
const defaultGraph = terms.defaultGraph();

// Named graph
const namedGraph = terms.iri('ex:graph1');

// Quad in default graph
const defaultQuad = terms.quad(
  terms.iri('ex:person'),
  terms.iri('ex:name'),
  terms.lit('John Doe'),
  null // or undefined for default graph
);

// Quad in named graph
const namedQuad = terms.quad(
  terms.iri('ex:person'),
  terms.iri('ex:name'),
  terms.lit('John Doe'),
  namedGraph
);
```

### Error Handling

```javascript
const terms = useTerms();

try {
  // Invalid IRI
  const invalidIRI = terms.iri(null);
} catch (error) {
  console.error('IRI error:', error.message);
}

try {
  // Invalid literal value
  const invalidLiteral = terms.lit(null);
} catch (error) {
  console.error('Literal error:', error.message);
}

try {
  // Invalid quad components
  const invalidQuad = terms.quad(null, null, null);
} catch (error) {
  console.error('Quad error:', error.message);
}
```

## Performance Considerations

### Term Reuse
```javascript
const terms = useTerms();

// Reuse terms for better performance
const personType = terms.iri('rdf:type');
const personClass = terms.iri('ex:Person');

// Create multiple quads with reused terms
const quads = [
  terms.quad(terms.iri('ex:person1'), personType, personClass),
  terms.quad(terms.iri('ex:person2'), personType, personClass),
  terms.quad(terms.iri('ex:person3'), personType, personClass)
];
```

### Memory Management
```javascript
// Terms are lightweight objects, but consider reuse for large datasets
const terms = useTerms();

// Good: Reuse common terms
const rdfType = terms.iri('rdf:type');
const personClass = terms.iri('ex:Person');

// Create many quads efficiently
const quads = Array.from({ length: 1000 }, (_, i) => 
  terms.quad(
    terms.iri(`ex:person${i}`),
    rdfType,
    personClass
  )
);
```

## Integration with Other Composables

### useStore
```javascript
import { useTerms, useStore } from 'unrdf';

const terms = useTerms();
const store = useStore();

// Create terms and add to store
const quads = [
  terms.quad(
    terms.iri('ex:person'),
    terms.iri('ex:name'),
    terms.lit('John Doe')
  )
];

store.add(quads);
```

### useGraph
```javascript
import { useTerms, useStore, useGraph } from 'unrdf';

const terms = useTerms();
const store = useStore();
const graph = useGraph(store);

// Create data using terms
const quads = [
  terms.quad(
    terms.iri('ex:person'),
    terms.iri('rdf:type'),
    terms.iri('ex:Person')
  )
];

store.add(quads);

// Query using the same terms
const results = await graph.select(`
  PREFIX ex: <http://example.org/>
  SELECT ?person WHERE {
    ?person a ex:Person .
  }
`);
```

### usePrefixes
```javascript
import { useTerms, usePrefixes } from 'unrdf';

const terms = useTerms();
const prefixes = usePrefixes({
  'ex': 'http://example.org/',
  'foaf': 'http://xmlns.com/foaf/0.1/'
});

// Use prefixes for term creation
const person = terms.iri('ex:person');
const name = terms.iri('foaf:name');

// Expand CURIEs
const fullIRI = prefixes.expand('ex:person');
const personTerm = terms.iri(fullIRI);
```

## Best Practices

### 1. Use Appropriate Term Types
```javascript
const terms = useTerms();

// Use IRIs for resources
const person = terms.iri('ex:person');

// Use literals for values
const name = terms.lit('John Doe');
const age = terms.lit(30, 'http://www.w3.org/2001/XMLSchema#integer');

// Use blank nodes for anonymous resources
const bnode = terms.bnode();
```

### 2. Handle Datatypes Correctly
```javascript
// Use appropriate datatypes
const age = terms.lit(30, 'http://www.w3.org/2001/XMLSchema#integer');
const height = terms.lit(1.75, 'http://www.w3.org/2001/XMLSchema#decimal');
const name = terms.lit('John Doe'); // Uses default string datatype
```

### 3. Use Language Tags Appropriately
```javascript
// Use language tags for multilingual content
const nameEn = terms.lit('John Doe', null, 'en');
const nameEs = terms.lit('Juan Pérez', null, 'es');
```

### 4. Reuse Common Terms
```javascript
// Reuse common terms for better performance
const rdfType = terms.iri('rdf:type');
const personClass = terms.iri('ex:Person');

// Use in multiple quads
const quads = [
  terms.quad(terms.iri('ex:person1'), rdfType, personClass),
  terms.quad(terms.iri('ex:person2'), rdfType, personClass)
];
```

## Troubleshooting

### Common Issues

#### Invalid IRI
```javascript
// Make sure IRIs are valid strings
try {
  const iri = terms.iri('valid-iri');
} catch (error) {
  console.error('Invalid IRI:', error.message);
}
```

#### Invalid Literal Value
```javascript
// Make sure literal values are not null/undefined
try {
  const literal = terms.lit('valid value');
} catch (error) {
  console.error('Invalid literal:', error.message);
}
```

#### Invalid Quad Components
```javascript
// Make sure all quad components are valid terms
try {
  const quad = terms.quad(
    terms.iri('ex:subject'),
    terms.iri('ex:predicate'),
    terms.lit('object')
  );
} catch (error) {
  console.error('Invalid quad:', error.message);
}
```

## See Also

- [useStore](./useStore.md) - Store management
- [useGraph](./useGraph.md) - SPARQL operations
- [usePrefixes](./usePrefixes.md) - Prefix management
- [Core Concepts](../core-concepts.md) - Understanding unrdf's philosophy
