# Graph Utils

Store operations and query helpers for RDF graphs.

## Overview

The `graph-utils` module provides boilerplate-killing functions for working with N3 stores, including query helpers, type checking, and data extraction utilities.

## Functions

### `getObjects(store, subject, predicate)`

Gets all objects for a subject-predicate pair.

```javascript
import { getObjects } from 'unrdf/utils';

const objects = getObjects(store, 'http://example.org/person', 'http://example.org/name');
// Returns: Array of object terms
```

**Parameters:**
- `store` (Store) - RDF store to query
- `subject` (string|NamedNode) - Subject IRI or NamedNode
- `predicate` (string|NamedNode) - Predicate IRI or NamedNode

**Returns:** Array of object terms

### `getSubjects(store, predicate, object)`

Gets all subjects for a predicate-object pair.

```javascript
import { getSubjects } from 'unrdf/utils';

const subjects = getSubjects(store, 'http://example.org/name', 'Alice');
// Returns: Array of subject terms
```

**Parameters:**
- `store` (Store) - RDF store to query
- `predicate` (string|NamedNode) - Predicate IRI or NamedNode
- `object` (string|Term) - Object value or term

**Returns:** Array of subject terms

### `getPredicates(store, subject, object)`

Gets all predicates for a subject-object pair.

```javascript
import { getPredicates } from 'unrdf/utils';

const predicates = getPredicates(store, 'http://example.org/person', 'Alice');
// Returns: Array of predicate terms
```

**Parameters:**
- `store` (Store) - RDF store to query
- `subject` (string|NamedNode) - Subject IRI or NamedNode
- `object` (string|Term) - Object value or term

**Returns:** Array of predicate terms

### `isA(store, subject, typeIRI)`

Checks if a subject has a specific rdf:type.

```javascript
import { isA } from 'unrdf/utils';

const isPerson = isA(store, 'http://example.org/person', 'http://example.org/Person');
// Returns: boolean
```

**Parameters:**
- `store` (Store) - RDF store to query
- `subject` (string|NamedNode) - Subject IRI or NamedNode
- `typeIRI` (string|NamedNode) - Type IRI or NamedNode

**Returns:** boolean

### `getTypes(store, subject)`

Gets all rdf:type values for a subject.

```javascript
import { getTypes } from 'unrdf/utils';

const types = getTypes(store, 'http://example.org/person');
// Returns: Array of type IRIs
```

**Parameters:**
- `store` (Store) - RDF store to query
- `subject` (string|NamedNode) - Subject IRI or NamedNode

**Returns:** Array of type IRI strings

### `pluck(store, predicateIRI)`

Gets all quads with a specific predicate (predicate pluck).

```javascript
import { pluck } from 'unrdf/utils';

const nameQuads = pluck(store, 'http://example.org/name');
// Returns: Array of quads with the specified predicate
```

**Parameters:**
- `store` (Store) - RDF store to query
- `predicateIRI` (string|NamedNode) - Predicate IRI or NamedNode

**Returns:** Array of quads

### `indexByPredicate(store, predicateIRI)`

Creates an index mapping subjects to their objects for a specific predicate.

```javascript
import { indexByPredicate } from 'unrdf/utils';

const nameIndex = indexByPredicate(store, 'http://example.org/name');
// Returns: Map<subjectIRI, objectValues[]>
```

**Parameters:**
- `store` (Store) - RDF store to query
- `predicateIRI` (string|NamedNode) - Predicate IRI or NamedNode

**Returns:** Map with subject IRIs as keys and arrays of object values as values

### `getProperties(store, subject)`

Gets all properties (predicates) for a subject.

```javascript
import { getProperties } from 'unrdf/utils';

const properties = getProperties(store, 'http://example.org/person');
// Returns: Map<predicateIRI, objectValues[]>
```

**Parameters:**
- `store` (Store) - RDF store to query
- `subject` (string|NamedNode) - Subject IRI or NamedNode

**Returns:** Map with predicate IRIs as keys and arrays of object values as values

### `hasSubject(store, subject)`

Checks if a subject exists in the store.

```javascript
import { hasSubject } from 'unrdf/utils';

const exists = hasSubject(store, 'http://example.org/person');
// Returns: boolean
```

**Parameters:**
- `store` (Store) - RDF store to query
- `subject` (string|NamedNode) - Subject IRI or NamedNode

**Returns:** boolean

### `findByProperty(store, predicate, object)`

Finds all subjects that have a specific predicate-object pair.

```javascript
import { findByProperty } from 'unrdf/utils';

const subjects = findByProperty(store, 'http://example.org/name', 'Alice');
// Returns: Array of subject terms
```

**Parameters:**
- `store` (Store) - RDF store to query
- `predicate` (string|NamedNode) - Predicate IRI or NamedNode
- `object` (string|Term) - Object value or term

**Returns:** Array of subject terms

### `getFirstObject(store, subject, predicate)`

Gets the first object for a subject-predicate pair.

```javascript
import { getFirstObject } from 'unrdf/utils';

const firstName = getFirstObject(store, 'http://example.org/person', 'http://example.org/name');
// Returns: First object value or null if not found
```

**Parameters:**
- `store` (Store) - RDF store to query
- `subject` (string|NamedNode) - Subject IRI or NamedNode
- `predicate` (string|NamedNode) - Predicate IRI or NamedNode

**Returns:** First object value or null

## Common Patterns

### Type Checking

```javascript
import { isA, getTypes } from 'unrdf/utils';

// Check if something is a person
if (isA(store, subject, 'http://example.org/Person')) {
  console.log('This is a person');
}

// Get all types
const types = getTypes(store, subject);
console.log('Types:', types);
```

### Property Access

```javascript
import { getFirstObject, getProperties } from 'unrdf/utils';

// Get a specific property
const name = getFirstObject(store, subject, 'http://example.org/name');

// Get all properties
const properties = getProperties(store, subject);
for (const [predicate, values] of properties) {
  console.log(`${predicate}: ${values.join(', ')}`);
}
```

### Data Indexing

```javascript
import { indexByPredicate } from 'unrdf/utils';

// Create an index of all names
const nameIndex = indexByPredicate(store, 'http://example.org/name');

// Find all people named Alice
const aliceSubjects = [];
for (const [subject, names] of nameIndex) {
  if (names.includes('Alice')) {
    aliceSubjects.push(subject);
  }
}
```

### Cross-Reference Queries

```javascript
import { findByProperty, getObjects } from 'unrdf/utils';

// Find all people who work at a specific company
const employees = findByProperty(store, 'http://example.org/worksFor', 'http://example.org/company');

// Get their names
const employeeNames = employees.map(emp => 
  getFirstObject(store, emp, 'http://example.org/name')
);
```

## Examples

### Basic Store Operations

```javascript
import { getObjects, isA, getTypes } from 'unrdf/utils';

// Check if a resource is a person
const personIRI = 'http://example.org/person1';
if (isA(store, personIRI, 'http://example.org/Person')) {
  console.log('This is a person');
  
  // Get their name
  const names = getObjects(store, personIRI, 'http://example.org/name');
  console.log('Names:', names);
  
  // Get all their types
  const types = getTypes(store, personIRI);
  console.log('Types:', types);
}
```

### Data Analysis

```javascript
import { pluck, indexByPredicate } from 'unrdf/utils';

// Analyze all name properties
const nameQuads = pluck(store, 'http://example.org/name');
console.log(`Found ${nameQuads.length} name statements`);

// Create a name index
const nameIndex = indexByPredicate(store, 'http://example.org/name');
console.log(`Found ${nameIndex.size} subjects with names`);

// Find duplicate names
const nameCounts = new Map();
for (const [subject, names] of nameIndex) {
  for (const name of names) {
    nameCounts.set(name, (nameCounts.get(name) || 0) + 1);
  }
}

const duplicates = Array.from(nameCounts.entries())
  .filter(([name, count]) => count > 1);
console.log('Duplicate names:', duplicates);
```

### Property Extraction

```javascript
import { getProperties, getFirstObject } from 'unrdf/utils';

function extractPersonData(store, personIRI) {
  const properties = getProperties(store, personIRI);
  
  return {
    name: getFirstObject(store, personIRI, 'http://example.org/name'),
    email: getFirstObject(store, personIRI, 'http://example.org/email'),
    age: getFirstObject(store, personIRI, 'http://example.org/age'),
    allProperties: Object.fromEntries(properties)
  };
}

const personData = extractPersonData(store, 'http://example.org/person1');
console.log(personData);
```

## Performance Notes

- **Efficient Queries**: Uses N3 store's optimized query methods
- **Indexed Access**: `indexByPredicate` creates efficient lookup structures
- **Memory Usage**: Minimal memory overhead for query operations
- **Large Stores**: Functions are optimized for large RDF stores

## Error Handling

- **Null Inputs**: Functions handle null subjects gracefully in `isA` but throw in `getObjects`
- **Missing Data**: Return empty arrays or null for non-existent data
- **Type Safety**: Input validation with descriptive error messages

## Related Modules

- [Term Utils](./term-utils.md) - Term creation and manipulation
- [Quad Utils](./quad-utils.md) - Quad operations
- [Validation Utils](./validation-utils.md) - Store validation
