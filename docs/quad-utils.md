# Quad Utils

Quad/JSON transformations and filtering utilities.

## Overview

The `quad-utils` module provides functions for converting between RDF quads and JSON representations, as well as filtering and grouping operations on quad collections.

## Functions

### `quadToJSON(quad)`

Converts a single quad to plain JSON.

```javascript
import { quadToJSON } from 'unrdf/utils';

const json = quadToJSON(quad);
// Returns: { subject: "...", predicate: "...", object: "...", graph: "..." }
```

**Parameters:**
- `quad` (Quad) - RDF quad to convert

**Returns:** Object with subject, predicate, object, and graph properties

### `jsonToQuad(obj)`

Converts JSON object to RDF quad.

```javascript
import { jsonToQuad } from 'unrdf/utils';

const quad = jsonToQuad({
  subject: 'http://example.org/s',
  predicate: 'http://example.org/p',
  object: 'http://example.org/o',
  graph: 'http://example.org/g'
});
```

**Parameters:**
- `obj` (Object) - Object with subject, predicate, object, and optional graph

**Returns:** Quad

**Throws:** Error if required properties are missing

### `quadsToJSON(quads)`

Converts array of quads to JSON array.

```javascript
import { quadsToJSON } from 'unrdf/utils';

const jsonArray = quadsToJSON([quad1, quad2, quad3]);
```

**Parameters:**
- `quads` (Quad[]) - Array of quads

**Returns:** Array of JSON objects

### `jsonToQuads(jsonArray)`

Converts JSON array to array of quads.

```javascript
import { jsonToQuads } from 'unrdf/utils';

const quads = jsonToQuads([
  { subject: 'http://example.org/s1', predicate: 'http://example.org/p', object: 'o1' },
  { subject: 'http://example.org/s2', predicate: 'http://example.org/p', object: 'o2' }
]);
```

**Parameters:**
- `jsonArray` (Object[]) - Array of JSON objects

**Returns:** Array of quads

### `extractSubjects(quads)`

Extracts unique subjects from quad array.

```javascript
import { extractSubjects } from 'unrdf/utils';

const subjects = extractSubjects(quads);
// Returns: Set of subject terms
```

### `extractPredicates(quads)`

Extracts unique predicates from quad array.

```javascript
import { extractPredicates } from 'unrdf/utils';

const predicates = extractPredicates(quads);
// Returns: Set of predicate terms
```

### `extractObjects(quads)`

Extracts unique objects from quad array.

```javascript
import { extractObjects } from 'unrdf/utils';

const objects = extractObjects(quads);
// Returns: Set of object terms
```

### `filterBySubject(quads, subject)`

Filters quads by subject.

```javascript
import { filterBySubject } from 'unrdf/utils';

const filtered = filterBySubject(quads, 'http://example.org/person');
```

**Parameters:**
- `quads` (Quad[]) - Array of quads
- `subject` (string|NamedNode) - Subject to filter by

**Returns:** Array of matching quads

### `filterByPredicate(quads, predicate)`

Filters quads by predicate.

```javascript
import { filterByPredicate } from 'unrdf/utils';

const filtered = filterByPredicate(quads, 'http://example.org/name');
```

**Parameters:**
- `quads` (Quad[]) - Array of quads
- `predicate` (string|NamedNode) - Predicate to filter by

**Returns:** Array of matching quads

### `filterByObject(quads, object)`

Filters quads by object.

```javascript
import { filterByObject } from 'unrdf/utils';

const filtered = filterByObject(quads, 'Alice');
```

**Parameters:**
- `quads` (Quad[]) - Array of quads
- `object` (string|Term) - Object to filter by

**Returns:** Array of matching quads

### `groupBySubject(quads)`

Groups quads by subject.

```javascript
import { groupBySubject } from 'unrdf/utils';

const grouped = groupBySubject(quads);
// Returns: Map<subject, Quad[]>
```

**Returns:** Map with subjects as keys and quad arrays as values

### `groupByPredicate(quads)`

Groups quads by predicate.

```javascript
import { groupByPredicate } from 'unrdf/utils';

const grouped = groupByPredicate(quads);
// Returns: Map<predicate, Quad[]>
```

**Returns:** Map with predicates as keys and quad arrays as values

## JSON Format

The JSON representation of quads uses the following structure:

```javascript
{
  subject: "http://example.org/subject",    // IRI string
  predicate: "http://example.org/predicate", // IRI string  
  object: "http://example.org/object",      // IRI string or literal value
  graph: "http://example.org/graph"         // IRI string or null for default graph
}
```

### Special Cases

- **Blank Nodes**: Represented as their ID string (without `_:` prefix)
- **Literals**: Represented as their value string
- **Default Graph**: Represented as `null` in the graph field
- **Named Graphs**: Represented as their IRI string

## Examples

### Basic Quad/JSON Conversion

```javascript
import { quadToJSON, jsonToQuad, DataFactory } from 'unrdf/utils';

const { namedNode, literal, quad } = DataFactory;

// Create a quad
const originalQuad = quad(
  namedNode('http://example.org/person'),
  namedNode('http://example.org/name'),
  literal('Alice')
);

// Convert to JSON
const json = quadToJSON(originalQuad);
console.log(json);
// { subject: "http://example.org/person", predicate: "http://example.org/name", object: "Alice", graph: null }

// Convert back to quad
const restoredQuad = jsonToQuad(json);
console.log(restoredQuad.equals(originalQuad)); // true
```

### Batch Processing

```javascript
import { quadsToJSON, jsonToQuads } from 'unrdf/utils';

// Convert multiple quads
const quads = [quad1, quad2, quad3];
const jsonArray = quadsToJSON(quads);

// Process JSON (e.g., filter, transform)
const filteredJson = jsonArray.filter(q => q.predicate.includes('name'));

// Convert back to quads
const filteredQuads = jsonToQuads(filteredJson);
```

### Filtering Operations

```javascript
import { filterBySubject, filterByPredicate, groupBySubject } from 'unrdf/utils';

// Filter by subject
const personQuads = filterBySubject(quads, 'http://example.org/person');

// Filter by predicate
const nameQuads = filterByPredicate(quads, 'http://example.org/name');

// Group by subject
const grouped = groupBySubject(quads);
for (const [subject, subjectQuads] of grouped) {
  console.log(`Subject ${subject} has ${subjectQuads.length} quads`);
}
```

### Data Analysis

```javascript
import { extractSubjects, extractPredicates, extractObjects } from 'unrdf/utils';

// Analyze quad collection
const subjects = extractSubjects(quads);
const predicates = extractPredicates(quads);
const objects = extractObjects(quads);

console.log(`Found ${subjects.size} unique subjects`);
console.log(`Found ${predicates.size} unique predicates`);
console.log(`Found ${objects.size} unique objects`);
```

## Error Handling

- **Missing Properties**: `jsonToQuad` throws error for missing required properties
- **Invalid Types**: Functions validate input types and provide clear error messages
- **Empty Arrays**: Functions handle empty arrays gracefully

## Performance Notes

- **Efficient Filtering**: Uses optimized array methods for filtering operations
- **Memory Usage**: Minimal memory overhead for JSON conversion
- **Large Datasets**: Functions are optimized for processing large quad collections

## Related Modules

- [Term Utils](./term-utils.md) - Term creation and manipulation
- [Graph Utils](./graph-utils.md) - Store operations
- [Validation Utils](./validation-utils.md) - Quad validation
