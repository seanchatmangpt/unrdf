# Term Utils

RDF term manipulation and conversion utilities.

## Overview

The `term-utils` module provides low-level helpers for working with RDF terms, including conversion functions, type checks, and smart literal creation.

## Functions

### `asNamedNode(iri)`

Ensures any input is a NamedNode.

```javascript
import { asNamedNode } from 'unrdf/utils';

const node1 = asNamedNode('http://example.org/person');
const node2 = asNamedNode(existingNamedNode); // Passes through
```

**Parameters:**
- `iri` (string|NamedNode) - IRI string or existing NamedNode

**Returns:** NamedNode

**Throws:** Error if IRI is null or undefined

### `asLiteral(value, datatype?, language?)`

Ensures any input is a Literal.

```javascript
import { asLiteral } from 'unrdf/utils';

const lit1 = asLiteral('Hello');
const lit2 = asLiteral('42', 'http://www.w3.org/2001/XMLSchema#integer');
const lit3 = asLiteral('Bonjour', null, 'fr');
```

**Parameters:**
- `value` (string|Literal) - Value string or existing Literal
- `datatype` (string, optional) - Datatype IRI
- `language` (string, optional) - Language tag

**Returns:** Literal

**Throws:** Error if value is null or undefined

### `asBlankNode(id?)`

Ensures any input is a BlankNode.

```javascript
import { asBlankNode } from 'unrdf/utils';

const bnode1 = asBlankNode('myId');
const bnode2 = asBlankNode(); // Auto-generated ID
const bnode3 = asBlankNode(existingBlankNode); // Passes through
```

**Parameters:**
- `id` (string|BlankNode, optional) - Blank node ID or existing BlankNode

**Returns:** BlankNode

### `asString(term)`

Safe string coercion for any RDF term.

```javascript
import { asString } from 'unrdf/utils';

const str1 = asString(namedNode); // Returns IRI string
const str2 = asString(literal); // Returns literal value
const str3 = asString(blankNode); // Returns blank node ID
```

**Parameters:**
- `term` (Term) - Any RDF term

**Returns:** string

### `isNamedNode(term)`

Checks if a term is a NamedNode.

```javascript
import { isNamedNode } from 'unrdf/utils';

const isNamed = isNamedNode(term); // boolean
```

### `isLiteral(term)`

Checks if a term is a Literal.

```javascript
import { isLiteral } from 'unrdf/utils';

const isLit = isLiteral(term); // boolean
```

### `isBlankNode(term)`

Checks if a term is a BlankNode.

```javascript
import { isBlankNode } from 'unrdf/utils';

const isBlank = isBlankNode(term); // boolean
```

### `getIRI(term)`

Extracts IRI string from any term.

```javascript
import { getIRI } from 'unrdf/utils';

const iri1 = getIRI(namedNode); // Returns IRI string
const iri2 = getIRI(literal); // Returns literal value
const iri3 = getIRI(blankNode); // Returns blank node ID
```

**Parameters:**
- `term` (Term) - Any RDF term

**Returns:** string

### `smartLiteral(value)`

Creates a literal with automatic datatype detection.

```javascript
import { smartLiteral } from 'unrdf/utils';

const lit1 = smartLiteral('42'); // xsd:integer
const lit2 = smartLiteral('3.14'); // xsd:decimal
const lit3 = smartLiteral('true'); // xsd:boolean
const lit4 = smartLiteral('2023-01-01'); // xsd:date
const lit5 = smartLiteral('Hello'); // xsd:string
```

**Parameters:**
- `value` (string) - Value to convert

**Returns:** Literal with appropriate datatype

## Type Detection

The module automatically detects and sets appropriate XSD datatypes:

- **Integers**: `xsd:integer`
- **Decimals**: `xsd:decimal`
- **Booleans**: `xsd:boolean`
- **Dates**: `xsd:date`
- **DateTimes**: `xsd:dateTime`
- **Strings**: `xsd:string` (default)

## Error Handling

- **Null/Undefined**: `asNamedNode` and `asLiteral` throw descriptive errors
- **Invalid Inputs**: Type checking functions return false for invalid inputs
- **Graceful Fallbacks**: `asBlankNode` and `asString` handle edge cases gracefully

## Examples

### Basic Term Creation

```javascript
import { asNamedNode, asLiteral, asBlankNode } from 'unrdf/utils';

// Create terms
const subject = asNamedNode('http://example.org/person');
const predicate = asNamedNode('http://example.org/name');
const object = asLiteral('Alice');
const graph = asBlankNode('graph1');

// Use in quad
const quad = DataFactory.quad(subject, predicate, object, graph);
```

### Smart Literal Detection

```javascript
import { smartLiteral } from 'unrdf/utils';

const literals = [
  smartLiteral('42'),           // xsd:integer
  smartLiteral('3.14'),         // xsd:decimal
  smartLiteral('true'),         // xsd:boolean
  smartLiteral('2023-01-01'),   // xsd:date
  smartLiteral('Hello World')   // xsd:string
];

literals.forEach(lit => {
  console.log(`${lit.value} -> ${lit.datatype.value}`);
});
```

### Type Checking

```javascript
import { isNamedNode, isLiteral, isBlankNode, getIRI } from 'unrdf/utils';

function processTerm(term) {
  if (isNamedNode(term)) {
    return `NamedNode: ${getIRI(term)}`;
  } else if (isLiteral(term)) {
    return `Literal: ${getIRI(term)}`;
  } else if (isBlankNode(term)) {
    return `BlankNode: ${getIRI(term)}`;
  }
  return 'Unknown term type';
}
```

## Performance Notes

- **Caching**: Term creation functions are optimized for repeated calls
- **Memory**: Minimal memory overhead for term operations
- **Speed**: Fast type checking and conversion operations

## Related Modules

- [Quad Utils](./quad-utils.md) - Quad creation and manipulation
- [Graph Utils](./graph-utils.md) - Store operations using terms
- [Validation Utils](./validation-utils.md) - Term validation
