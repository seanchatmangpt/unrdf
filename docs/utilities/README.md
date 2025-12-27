# Utilities Documentation

This directory contains detailed documentation for unrdf utility functions.

## Overview

unrdf provides a comprehensive set of utility functions that cover the "dark matter" 80/20 tasks that don't fit neatly into composables. These utilities handle common, repetitive glue code in RDF workflows.

## Core Utilities

### [Term Utilities](./term-utils.md)
Functions for creating and manipulating RDF terms (named nodes, literals, blank nodes).

### [Graph Utilities](./graph-utils.md)
Helper functions for common graph operations and queries.

### [Validation Utilities](./validation-utils.md)
Utilities for validating RDF data and structures.

### [ID Utilities](./id-utils.md)
Functions for generating and managing RDF identifiers.

### [Namespace Utilities](./namespace-utils.md)
Utilities for working with RDF namespaces and prefixes.

### [SPARQL Utilities](./sparql-utils.md)
Helper functions for building and manipulating SPARQL queries.

### [Transform Utilities](./transform-utils.md)
Functions for transforming RDF data between different formats.

### [Merge Utilities](./merge-utils.md)
Utilities for merging and combining RDF stores.

### [Quality Utilities](./quality-utils.md)
Functions for assessing and improving RDF data quality.

### [Debug Utilities](./debug-utils.md)
Utilities for debugging and inspecting RDF data.

### [IO Utilities](./io-utils.md)
Functions for reading and writing RDF data to various sources.

### [Quad Utilities](./quad-utils.md)
Utilities for working with RDF quads and triple patterns.

## Usage Patterns

### Basic Usage

```javascript
import { asNamedNode, asLiteral, getObjects, getSubjects } from 'unrdf/utils';

// Create terms
const subject = asNamedNode('http://example.org/alice');
const object = asLiteral('Alice');

// Query graph
const objects = getObjects(store, subject, predicate);
const subjects = getSubjects(store, predicate, object);
```

### Advanced Usage

```javascript
import { 
  asNamedNode, 
  asLiteral, 
  getObjects, 
  getSubjects,
  isValidIRI,
  validateRDFConstraints
} from 'unrdf/utils';

// Validate data
if (!isValidIRI(iri)) {
  throw new Error('Invalid IRI');
}

// Validate store
const constraints = validateRDFConstraints(store);
if (!constraints.valid) {
  console.error('RDF constraints violated:', constraints.errors);
}
```

## Common Patterns

### Term Creation

```javascript
import { asNamedNode, asLiteral, smartLiteral } from 'unrdf/utils';

// Create named nodes
const subject = asNamedNode('http://example.org/alice');
const predicate = asNamedNode('http://xmlns.com/foaf/0.1/name');

// Create literals
const name = asLiteral('Alice');
const age = asLiteral('30', 'http://www.w3.org/2001/XMLSchema#integer');

// Smart literal creation
const smartName = smartLiteral('Alice'); // string
const smartAge = smartLiteral(30); // number
const smartDate = smartLiteral(new Date()); // date
```

### Graph Queries

```javascript
import { getObjects, getSubjects, getPredicates, findByProperty } from 'unrdf/utils';

// Get objects for subject+predicate
const names = getObjects(store, subject, namePredicate);

// Get subjects for predicate+object
const people = getSubjects(store, typePredicate, personClass);

// Get predicates for subject+object
const predicates = getPredicates(store, subject, object);

// Find by property value
const alices = findByProperty(store, namePredicate, 'Alice');
```

### Validation

```javascript
import { 
  isValidIRI, 
  isValidTerm, 
  isValidQuad, 
  isValidStore,
  validateRDFConstraints
} from 'unrdf/utils';

// Validate IRIs
if (!isValidIRI(iri)) {
  throw new Error('Invalid IRI format');
}

// Validate terms
if (!isValidTerm(term)) {
  throw new Error('Invalid RDF term');
}

// Validate quads
if (!isValidQuad(quad)) {
  throw new Error('Invalid RDF quad');
}

// Validate stores
if (!isValidStore(store)) {
  throw new Error('Invalid store structure');
}

// Validate RDF constraints
const result = validateRDFConstraints(store);
if (!result.valid) {
  console.error('RDF constraints violated:', result.errors);
}
```

### ID Management

```javascript
import { 
  generateBlankNodeID, 
  generateUUID, 
  generateShortUUID,
  createNamespaceID,
  extractLocalName,
  extractNamespace,
  isBlankNodeIRI,
  blankNodeIDToIRI,
  iriToBlankNodeID
} from 'unrdf/utils';

// Generate IDs
const bnodeID = generateBlankNodeID();
const uuid = generateUUID();
const shortUUID = generateShortUUID();

// Namespace management
const namespace = createNamespaceID('http://example.org/');
const localName = extractLocalName('http://example.org/alice');
const ns = extractNamespace('http://example.org/alice');

// Blank node handling
const isBlank = isBlankNodeIRI('http://example.org/.well-known/genid/123');
const iri = blankNodeIDToIRI('123');
const id = iriToBlankNodeID('http://example.org/.well-known/genid/123');
```

### Namespace Operations

```javascript
import { 
  expandCURIE, 
  shrinkIRI, 
  getNamespace, 
  getLocalName,
  isValidCURIE,
  isValidNamespace
} from 'unrdf/utils';

// CURIE operations
const iri = expandCURIE('foaf:Person', { foaf: 'http://xmlns.com/foaf/0.1/' });
const curie = shrinkIRI('http://xmlns.com/foaf/0.1/Person', { foaf: 'http://xmlns.com/foaf/0.1/' });

// Namespace analysis
const namespace = getNamespace('http://xmlns.com/foaf/0.1/Person');
const localName = getLocalName('http://xmlns.com/foaf/0.1/Person');

// Validation
const isValidCurie = isValidCURIE('foaf:Person');
const isValidNs = isValidNamespace('http://xmlns.com/foaf/0.1/');
```

### SPARQL Building

```javascript
import { 
  buildSelectQuery, 
  buildAskQuery, 
  buildConstructQuery,
  buildUpdateQuery,
  addPrefix,
  addWhereClause,
  addFilter,
  addLimit,
  addOffset
} from 'unrdf/utils';

// Build queries
const selectQuery = buildSelectQuery(['?s', '?p', '?o'], {
  where: '?s ?p ?o',
  prefixes: { ex: 'http://example.org/' }
});

const askQuery = buildAskQuery({
  where: '?s a ex:Person',
  prefixes: { ex: 'http://example.org/' }
});

// Add clauses
let query = 'SELECT ?s WHERE { ?s ?p ?o }';
query = addPrefix(query, 'ex', 'http://example.org/');
query = addFilter(query, '?s = ex:alice');
query = addLimit(query, 10);
```

### Data Transformation

```javascript
import { 
  transformStore, 
  mapTerms, 
  filterQuads,
  groupBySubject,
  groupByPredicate,
  groupByObject
} from 'unrdf/utils';

// Transform store
const transformed = transformStore(store, (quad) => {
  // Transform each quad
  return quad;
});

// Map terms
const mapped = mapTerms(store, (term) => {
  if (term.termType === 'NamedNode') {
    return asNamedNode(term.value.replace('example.org', 'newdomain.org'));
  }
  return term;
});

// Filter quads
const filtered = filterQuads(store, (quad) => {
  return quad.predicate.value.includes('foaf');
});

// Group operations
const bySubject = groupBySubject(store);
const byPredicate = groupByPredicate(store);
const byObject = groupByObject(store);
```

### Quality Assessment

```javascript
import { 
  assessDataQuality, 
  findDuplicateQuads,
  findInconsistentData,
  generateQualityReport,
  suggestImprovements
} from 'unrdf/utils';

// Assess quality
const quality = assessDataQuality(store);
console.log('Data quality score:', quality.score);

// Find issues
const duplicates = findDuplicateQuads(store);
const inconsistencies = findInconsistentData(store);

// Generate report
const report = generateQualityReport(store);
console.log('Quality report:', report);

// Get suggestions
const suggestions = suggestImprovements(store);
console.log('Improvement suggestions:', suggestions);
```

## Best Practices

### 1. Use Appropriate Utilities

```javascript
// Good: Use specific utilities
const subject = asNamedNode(iri);
const objects = getObjects(store, subject, predicate);

// Avoid: Manual term creation
const subject = new NamedNode(iri);
```

### 2. Validate Inputs

```javascript
// Always validate inputs
if (!isValidIRI(iri)) {
  throw new Error('Invalid IRI');
}

const term = asNamedNode(iri);
```

### 3. Handle Errors Gracefully

```javascript
try {
  const result = utilityFunction(input);
} catch (error) {
  console.error('Utility function failed:', error.message);
  // Provide fallback
}
```

### 4. Use Consistent Patterns

```javascript
// Use consistent naming
const subject = asNamedNode(subjectIRI);
const predicate = asNamedNode(predicateIRI);
const object = asLiteral(objectValue);
```

## Performance Considerations

### 1. Batch Operations

```javascript
// Batch multiple operations
const terms = iris.map(iri => asNamedNode(iri));
const objects = terms.map(term => getObjects(store, term, predicate));
```

### 2. Cache Results

```javascript
// Cache expensive operations
const cache = new Map();
function getCachedObjects(subject, predicate) {
  const key = `${subject.value}-${predicate.value}`;
  if (!cache.has(key)) {
    cache.set(key, getObjects(store, subject, predicate));
  }
  return cache.get(key);
}
```

### 3. Use Streaming for Large Datasets

```javascript
// For large stores, use streaming
function processLargeStore(store) {
  const batchSize = 1000;
  let processed = 0;
  
  for (const quad of store) {
    // Process quad
    processed++;
    
    if (processed % batchSize === 0) {
      console.log(`Processed ${processed} quads`);
    }
  }
}
```

## Error Handling

### Common Error Types

- **Invalid IRI**: Malformed IRI format
- **Invalid Term**: Invalid RDF term structure
- **Invalid Quad**: Invalid RDF quad structure
- **Invalid Store**: Invalid store structure
- **Validation Errors**: RDF constraint violations

### Error Recovery

```javascript
try {
  const result = utilityFunction(input);
} catch (error) {
  if (error.message.includes('Invalid IRI')) {
    // Fix IRI format
    const fixedIRI = fixIRIFormat(input);
    return utilityFunction(fixedIRI);
  }
  throw error;
}
```

## Testing

### Unit Testing

```javascript
import { describe, it, expect } from 'vitest';
import { asNamedNode, asLiteral, getObjects } from 'unrdf/utils';

describe('Term Utilities', () => {
  it('should create named nodes', () => {
    const node = asNamedNode('http://example.org/alice');
    expect(node.termType).toBe('NamedNode');
    expect(node.value).toBe('http://example.org/alice');
  });
});
```

### Integration Testing

```javascript
import { useStore } from 'unrdf';
import { getObjects, getSubjects } from 'unrdf/utils';

describe('Graph Utilities Integration', () => {
  it('should work with useStore', () => {
    const store = useStore();
    store.add(quad);
    
    const objects = getObjects(store, subject, predicate);
    expect(objects).toHaveLength(1);
  });
});
```

## Contributing

When adding new utilities:

1. Follow the established patterns
2. Include comprehensive JSDoc documentation
3. Add proper error handling
4. Include unit tests
5. Update this documentation

See [CONTRIBUTING.md](../../CONTRIBUTING.md) for more details.
