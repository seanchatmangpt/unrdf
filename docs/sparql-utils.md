# SPARQL Utils

SPARQL query building, analysis, and validation utilities.

## Overview

The `sparql-utils` module provides utilities for working with SPARQL queries, including query building, analysis, validation, and optimization. It includes a `SPARQLBuilder` class for fluent query construction and various helper functions for query manipulation.

## SPARQLBuilder Class

### Constructor

```javascript
import { SPARQLBuilder } from 'unrdf/utils';

const builder = new SPARQLBuilder();
```

### Methods

#### `select(variables)`

Adds SELECT clause to the query.

```javascript
builder.select('?s ?p ?o');
builder.select(['?s', '?p', '?o']);
builder.select('*'); // Select all variables
```

**Parameters:**
- `variables` (string | Array<string>) - Variables to select

**Returns:** SPARQLBuilder (for chaining)

#### `distinct()`

Adds DISTINCT modifier to SELECT.

```javascript
builder.select('?s').distinct();
```

**Returns:** SPARQLBuilder (for chaining)

#### `where(patterns)`

Adds WHERE clause with triple patterns.

```javascript
builder.where(`
  ?s ?p ?o .
  ?s rdf:type ?type .
`);
```

**Parameters:**
- `patterns` (string) - SPARQL triple patterns

**Returns:** SPARQLBuilder (for chaining)

#### `filter(condition)`

Adds FILTER condition.

```javascript
builder.filter('?age > 18');
builder.filter('regex(?name, "John")');
```

**Parameters:**
- `condition` (string) - Filter condition

**Returns:** SPARQLBuilder (for chaining)

#### `optional(patterns)`

Adds OPTIONAL clause.

```javascript
builder.optional(`
  ?s foaf:name ?name .
  ?s foaf:email ?email .
`);
```

**Parameters:**
- `patterns` (string) - Optional triple patterns

**Returns:** SPARQLBuilder (for chaining)

#### `union(patterns)`

Adds UNION clause.

```javascript
builder.union(`
  { ?s rdf:type foaf:Person . }
  { ?s rdf:type schema:Person . }
`);
```

**Parameters:**
- `patterns` (string) - Union patterns

**Returns:** SPARQLBuilder (for chaining)

#### `groupBy(variables)`

Adds GROUP BY clause.

```javascript
builder.groupBy('?type');
builder.groupBy(['?type', '?category']);
```

**Parameters:**
- `variables` (string | Array<string>) - Variables to group by

**Returns:** SPARQLBuilder (for chaining)

#### `having(condition)`

Adds HAVING clause.

```javascript
builder.having('COUNT(?s) > 5');
```

**Parameters:**
- `condition` (string) - Having condition

**Returns:** SPARQLBuilder (for chaining)

#### `orderBy(variables)`

Adds ORDER BY clause.

```javascript
builder.orderBy('?name');
builder.orderBy(['?name', 'DESC(?age)']);
```

**Parameters:**
- `variables` (string | Array<string>) - Variables to order by

**Returns:** SPARQLBuilder (for chaining)

#### `limit(count)`

Adds LIMIT clause.

```javascript
builder.limit(10);
```

**Parameters:**
- `count` (number) - Maximum number of results

**Returns:** SPARQLBuilder (for chaining)

#### `offset(count)`

Adds OFFSET clause.

```javascript
builder.offset(20);
```

**Parameters:**
- `count` (number) - Number of results to skip

**Returns:** SPARQLBuilder (for chaining)

#### `build()`

Builds the final SPARQL query string.

```javascript
const query = builder.build();
```

**Returns:** string

## Standalone Functions

### `buildSelectQuery(options)`

Builds a SELECT query from options.

```javascript
import { buildSelectQuery } from 'unrdf/utils';

const query = buildSelectQuery({
  select: ['?s', '?p', '?o'],
  distinct: true,
  where: '?s ?p ?o .',
  filter: '?age > 18',
  limit: 10
});
```

**Parameters:**
- `options` (Object) - Query options

**Returns:** string

### `buildConstructQuery(options)`

Builds a CONSTRUCT query from options.

```javascript
import { buildConstructQuery } from 'unrdf/utils';

const query = buildConstructQuery({
  construct: '?s ?p ?o .',
  where: '?s ?p ?o .',
  filter: '?age > 18'
});
```

**Parameters:**
- `options` (Object) - Query options

**Returns:** string

### `buildAskQuery(options)`

Builds an ASK query from options.

```javascript
import { buildAskQuery } from 'unrdf/utils';

const query = buildAskQuery({
  where: '?s rdf:type foaf:Person .',
  filter: '?age > 18'
});
```

**Parameters:**
- `options` (Object) - Query options

**Returns:** string

### `buildDescribeQuery(options)`

Builds a DESCRIBE query from options.

```javascript
import { buildDescribeQuery } from 'unrdf/utils';

const query = buildDescribeQuery({
  describe: '?s',
  where: '?s rdf:type foaf:Person .'
});
```

**Parameters:**
- `options` (Object) - Query options

**Returns:** string

### `analyzeQuery(query)`

Analyzes a SPARQL query and returns metadata.

```javascript
import { analyzeQuery } from 'unrdf/utils';

const analysis = analyzeQuery(`
  SELECT ?s ?p ?o
  WHERE {
    ?s ?p ?o .
    ?s rdf:type ?type .
  }
  LIMIT 10
`);

console.log(analysis);
// {
//   type: 'SELECT',
//   variables: ['?s', '?p', '?o'],
//   patterns: ['?s ?p ?o .', '?s rdf:type ?type .'],
//   hasFilter: false,
//   hasOptional: false,
//   hasUnion: false,
//   hasGroupBy: false,
//   hasOrderBy: false,
//   limit: 10,
//   offset: 0
// }
```

**Parameters:**
- `query` (string) - SPARQL query

**Returns:** Object

### `validateQuery(query)`

Validates a SPARQL query.

```javascript
import { validateQuery } from 'unrdf/utils';

const validation = validateQuery(`
  SELECT ?s ?p ?o
  WHERE {
    ?s ?p ?o .
  }
`);

console.log(validation);
// {
//   valid: true,
//   errors: [],
//   warnings: []
// }
```

**Parameters:**
- `query` (string) - SPARQL query

**Returns:** Object

### `optimizeQuery(query)`

Optimizes a SPARQL query.

```javascript
import { optimizeQuery } from 'unrdf/utils';

const optimized = optimizeQuery(`
  SELECT ?s ?p ?o
  WHERE {
    ?s ?p ?o .
    ?s rdf:type ?type .
  }
  LIMIT 10
`);
```

**Parameters:**
- `query` (string) - SPARQL query

**Returns:** string

### `extractVariables(query)`

Extracts variables from a SPARQL query.

```javascript
import { extractVariables } from 'unrdf/utils';

const variables = extractVariables(`
  SELECT ?s ?p ?o
  WHERE {
    ?s ?p ?o .
    ?s rdf:type ?type .
  }
`);

console.log(variables);
// ['?s', '?p', '?o', '?type']
```

**Parameters:**
- `query` (string) - SPARQL query

**Returns:** Array<string>

### `extractPatterns(query)`

Extracts triple patterns from a SPARQL query.

```javascript
import { extractPatterns } from 'unrdf/utils';

const patterns = extractPatterns(`
  SELECT ?s ?p ?o
  WHERE {
    ?s ?p ?o .
    ?s rdf:type ?type .
  }
`);

console.log(patterns);
// ['?s ?p ?o .', '?s rdf:type ?type .']
```

**Parameters:**
- `query` (string) - SPARQL query

**Returns:** Array<string>

### `extractFilters(query)`

Extracts FILTER conditions from a SPARQL query.

```javascript
import { extractFilters } from 'unrdf/utils';

const filters = extractFilters(`
  SELECT ?s ?p ?o
  WHERE {
    ?s ?p ?o .
    FILTER(?age > 18)
    FILTER(regex(?name, "John"))
  }
`);

console.log(filters);
// ['?age > 18', 'regex(?name, "John")']
```

**Parameters:**
- `query` (string) - SPARQL query

**Returns:** Array<string>

### `extractNamespaces(query)`

Extracts namespace prefixes from a SPARQL query.

```javascript
import { extractNamespaces } from 'unrdf/utils';

const namespaces = extractNamespaces(`
  PREFIX foaf: <http://xmlns.com/foaf/0.1/>
  PREFIX ex: <http://example.org/>
  
  SELECT ?s ?p ?o
  WHERE {
    ?s ?p ?o .
  }
`);

console.log(namespaces);
// {
//   'foaf': 'http://xmlns.com/foaf/0.1/',
//   'ex': 'http://example.org/'
// }
```

**Parameters:**
- `query` (string) - SPARQL query

**Returns:** Object

### `addPrefixes(query, prefixes)`

Adds namespace prefixes to a SPARQL query.

```javascript
import { addPrefixes } from 'unrdf/utils';

const prefixes = {
  'foaf': 'http://xmlns.com/foaf/0.1/',
  'ex': 'http://example.org/'
};

const queryWithPrefixes = addPrefixes(`
  SELECT ?s ?p ?o
  WHERE {
    ?s ?p ?o .
  }
`, prefixes);
```

**Parameters:**
- `query` (string) - SPARQL query
- `prefixes` (Object) - Namespace prefixes

**Returns:** string

### `removePrefixes(query)`

Removes namespace prefixes from a SPARQL query.

```javascript
import { removePrefixes } from 'unrdf/utils';

const queryWithoutPrefixes = removePrefixes(`
  PREFIX foaf: <http://xmlns.com/foaf/0.1/>
  PREFIX ex: <http://example.org/>
  
  SELECT ?s ?p ?o
  WHERE {
    ?s ?p ?o .
  }
`);
```

**Parameters:**
- `query` (string) - SPARQL query

**Returns:** string

### `expandPrefixes(query, namespaces)`

Expands prefixed terms in a SPARQL query.

```javascript
import { expandPrefixes } from 'unrdf/utils';

const namespaces = {
  'foaf': 'http://xmlns.com/foaf/0.1/',
  'ex': 'http://example.org/'
};

const expandedQuery = expandPrefixes(`
  SELECT ?s ?p ?o
  WHERE {
    ?s rdf:type foaf:Person .
    ?s foaf:name ?name .
  }
`, namespaces);
```

**Parameters:**
- `query` (string) - SPARQL query
- `namespaces` (Object) - Namespace prefixes

**Returns:** string

### `contractPrefixes(query, namespaces)`

Contracts full IRIs to prefixed form in a SPARQL query.

```javascript
import { contractPrefixes } from 'unrdf/utils';

const namespaces = {
  'foaf': 'http://xmlns.com/foaf/0.1/',
  'ex': 'http://example.org/'
};

const contractedQuery = contractPrefixes(`
  SELECT ?s ?p ?o
  WHERE {
    ?s <http://www.w3.org/1999/02/22-rdf-syntax-ns#type> <http://xmlns.com/foaf/0.1/Person> .
    ?s <http://xmlns.com/foaf/0.1/name> ?name .
  }
`, namespaces);
```

**Parameters:**
- `query` (string) - SPARQL query
- `namespaces` (Object) - Namespace prefixes

**Returns:** string

## Examples

### Basic Query Building

```javascript
import { SPARQLBuilder } from 'unrdf/utils';

// Build a simple SELECT query
const query = new SPARQLBuilder()
  .select('?s ?p ?o')
  .where(`
    ?s ?p ?o .
    ?s rdf:type ?type .
  `)
  .limit(10)
  .build();

console.log(query);
```

### Advanced Query Building

```javascript
import { SPARQLBuilder } from 'unrdf/utils';

// Build a complex query with multiple clauses
const complexQuery = new SPARQLBuilder()
  .select(['?s', '?name', '?email'])
  .distinct()
  .where(`
    ?s rdf:type foaf:Person .
    ?s foaf:name ?name .
  `)
  .optional(`
    ?s foaf:email ?email .
  `)
  .filter('?age > 18')
  .groupBy('?type')
  .having('COUNT(?s) > 5')
  .orderBy(['?name', 'DESC(?age)'])
  .limit(20)
  .offset(10)
  .build();

console.log(complexQuery);
```

### Query Analysis

```javascript
import { analyzeQuery, validateQuery, extractVariables } from 'unrdf/utils';

const query = `
  PREFIX foaf: <http://xmlns.com/foaf/0.1/>
  PREFIX ex: <http://example.org/>
  
  SELECT ?s ?name ?email
  WHERE {
    ?s rdf:type foaf:Person .
    ?s foaf:name ?name .
    OPTIONAL { ?s foaf:email ?email . }
    FILTER(?age > 18)
  }
  ORDER BY ?name
  LIMIT 10
`;

// Analyze the query
const analysis = analyzeQuery(query);
console.log('Query analysis:', analysis);

// Validate the query
const validation = validateQuery(query);
console.log('Query validation:', validation);

// Extract variables
const variables = extractVariables(query);
console.log('Variables:', variables);
```

### Namespace Management

```javascript
import { 
  extractNamespaces, addPrefixes, 
  expandPrefixes, contractPrefixes 
} from 'unrdf/utils';

const query = `
  SELECT ?s ?name
  WHERE {
    ?s rdf:type foaf:Person .
    ?s foaf:name ?name .
  }
`;

// Extract existing namespaces
const namespaces = extractNamespaces(query);
console.log('Extracted namespaces:', namespaces);

// Add prefixes
const prefixes = {
  'foaf': 'http://xmlns.com/foaf/0.1/',
  'ex': 'http://example.org/'
};

const queryWithPrefixes = addPrefixes(query, prefixes);
console.log('Query with prefixes:', queryWithPrefixes);

// Expand prefixes
const expandedQuery = expandPrefixes(query, prefixes);
console.log('Expanded query:', expandedQuery);

// Contract prefixes
const contractedQuery = contractPrefixes(expandedQuery, prefixes);
console.log('Contracted query:', contractedQuery);
```

### Query Optimization

```javascript
import { optimizeQuery, extractPatterns, extractFilters } from 'unrdf/utils';

const query = `
  SELECT ?s ?p ?o
  WHERE {
    ?s ?p ?o .
    ?s rdf:type ?type .
    ?s foaf:name ?name .
    FILTER(?age > 18)
    FILTER(regex(?name, "John"))
  }
  ORDER BY ?name
  LIMIT 10
`;

// Optimize the query
const optimized = optimizeQuery(query);
console.log('Optimized query:', optimized);

// Extract patterns and filters
const patterns = extractPatterns(query);
const filters = extractFilters(query);

console.log('Patterns:', patterns);
console.log('Filters:', filters);
```

### Batch Query Operations

```javascript
import { 
  buildSelectQuery, buildConstructQuery, 
  buildAskQuery, buildDescribeQuery 
} from 'unrdf/utils';

// Build different types of queries
const selectQuery = buildSelectQuery({
  select: ['?s', '?p', '?o'],
  where: '?s ?p ?o .',
  limit: 10
});

const constructQuery = buildConstructQuery({
  construct: '?s ?p ?o .',
  where: '?s ?p ?o .',
  filter: '?age > 18'
});

const askQuery = buildAskQuery({
  where: '?s rdf:type foaf:Person .'
});

const describeQuery = buildDescribeQuery({
  describe: '?s',
  where: '?s rdf:type foaf:Person .'
});

console.log('Select query:', selectQuery);
console.log('Construct query:', constructQuery);
console.log('Ask query:', askQuery);
console.log('Describe query:', describeQuery);
```

### Query Templates

```javascript
import { SPARQLBuilder } from 'unrdf/utils';

// Create query templates
function createPersonQuery(filters = {}) {
  const builder = new SPARQLBuilder()
    .select(['?s', '?name', '?email'])
    .where(`
      ?s rdf:type foaf:Person .
      ?s foaf:name ?name .
    `)
    .optional(`
      ?s foaf:email ?email .
    `);
  
  if (filters.minAge) {
    builder.filter(`?age > ${filters.minAge}`);
  }
  
  if (filters.namePattern) {
    builder.filter(`regex(?name, "${filters.namePattern}")`);
  }
  
  if (filters.limit) {
    builder.limit(filters.limit);
  }
  
  return builder.build();
}

// Use templates
const youngPeopleQuery = createPersonQuery({
  minAge: 18,
  namePattern: 'John',
  limit: 20
});

const allPeopleQuery = createPersonQuery({
  limit: 100
});

console.log('Young people query:', youngPeopleQuery);
console.log('All people query:', allPeopleQuery);
```

### Query Validation and Error Handling

```javascript
import { validateQuery } from 'unrdf/utils';

function safeQueryExecution(query) {
  const validation = validateQuery(query);
  
  if (!validation.valid) {
    console.error('Query validation failed:', validation.errors);
    return null;
  }
  
  if (validation.warnings.length > 0) {
    console.warn('Query warnings:', validation.warnings);
  }
  
  // Execute query here
  console.log('Query is valid, executing...');
  return query;
}

// Test with valid and invalid queries
const validQuery = `
  SELECT ?s ?p ?o
  WHERE {
    ?s ?p ?o .
  }
`;

const invalidQuery = `
  SELECT ?s ?p ?o
  WHERE {
    ?s ?p ?o
  }
`;

safeQueryExecution(validQuery);
safeQueryExecution(invalidQuery);
```

## Performance Notes

- **Query Building**: Efficient string concatenation for query construction
- **Query Analysis**: Fast regex-based pattern extraction
- **Namespace Handling**: Optimized prefix resolution
- **Memory Usage**: Minimal overhead for query manipulation

## Best Practices

### Query Construction
- Use the SPARQLBuilder for complex queries
- Validate queries before execution
- Use meaningful variable names
- Optimize queries for performance

### Namespace Management
- Define prefixes at the beginning of queries
- Use consistent namespace prefixes
- Avoid namespace conflicts
- Document namespace usage

### Query Optimization
- Use FILTER clauses efficiently
- Minimize OPTIONAL clauses
- Use appropriate LIMIT and OFFSET
- Consider query execution order

## Related Modules

- [Namespace Utils](./namespace-utils.md) - Namespace management
- [Validation Utils](./validation-utils.md) - Query validation
- [Graph Utils](./graph-utils.md) - RDF graph operations
