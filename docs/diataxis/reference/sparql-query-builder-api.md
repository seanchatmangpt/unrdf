# SPARQL Query Builder API Reference

Complete API reference for programmatic SPARQL query construction.

## SPARQLBuilder Class

Fluent API for building SPARQL queries programmatically.

### Constructor

```javascript
new SPARQLBuilder()
```

Creates a new SPARQL query builder instance.

**Example:**

```javascript
const query = new SPARQLBuilder()
  .prefix('foaf', 'http://xmlns.com/foaf/0.1/')
  .select('person', 'name')
  .where('?person', 'a', 'foaf:Person')
  .where('?person', 'foaf:name', '?name')
  .build();
```

### Methods

#### `prefix(name, namespace)`

Add a namespace prefix declaration.

**Parameters:**
- `name` (string) - Prefix name
- `namespace` (string) - Namespace URI

**Returns:** `SPARQLBuilder` - Builder instance (for chaining)

**Example:**

```javascript
builder
  .prefix('foaf', 'http://xmlns.com/foaf/0.1/')
  .prefix('dc', 'http://purl.org/dc/elements/1.1/')
  .prefix('schema', 'https://schema.org/');
```

#### `select(...variables)`

Add variables to SELECT clause.

**Parameters:**
- `variables` (...string) - Variable names (without ? prefix)

**Returns:** `SPARQLBuilder` - Builder instance

**Example:**

```javascript
builder.select('person', 'name', 'email');
// SELECT ?person ?name ?email
```

#### `selectAll()`

Select all variables (SELECT *).

**Returns:** `SPARQLBuilder` - Builder instance

**Example:**

```javascript
builder.selectAll();
// SELECT *
```

#### `distinct()`

Add DISTINCT modifier to SELECT.

**Returns:** `SPARQLBuilder` - Builder instance

**Example:**

```javascript
builder
  .distinct()
  .select('name');
// SELECT DISTINCT ?name
```

#### `where(subject, predicate, object)`

Add a triple pattern to WHERE clause.

**Parameters:**
- `subject` (string) - Subject (variable or URI)
- `predicate` (string) - Predicate (variable or URI)
- `object` (string) - Object (variable, URI, or literal)

**Returns:** `SPARQLBuilder` - Builder instance

**Example:**

```javascript
builder
  .where('?person', 'a', 'foaf:Person')
  .where('?person', 'foaf:name', '?name')
  .where('?person', 'foaf:age', '?age');
```

#### `optional(subject, predicate, object)`

Add an OPTIONAL triple pattern.

**Parameters:**
- `subject` (string) - Subject
- `predicate` (string) - Predicate
- `object` (string) - Object

**Returns:** `SPARQLBuilder` - Builder instance

**Example:**

```javascript
builder
  .where('?person', 'foaf:name', '?name')
  .optional('?person', 'foaf:nick', '?nickname');
// WHERE {
//   ?person foaf:name ?name .
//   OPTIONAL { ?person foaf:nick ?nickname }
// }
```

#### `filter(expression)`

Add a FILTER expression.

**Parameters:**
- `expression` (string) - SPARQL filter expression

**Returns:** `SPARQLBuilder` - Builder instance

**Example:**

```javascript
builder
  .where('?person', 'foaf:age', '?age')
  .filter('?age >= 18')
  .filter('?age < 65');
// WHERE {
//   ?person foaf:age ?age .
//   FILTER(?age >= 18)
//   FILTER(?age < 65)
// }
```

#### `union(callback)`

Add a UNION clause.

**Parameters:**
- `callback` (Function) - Function that builds union patterns

**Returns:** `SPARQLBuilder` - Builder instance

**Example:**

```javascript
builder
  .select('entity', 'name')
  .union(b => {
    b.where('?entity', 'a', 'foaf:Person')
     .where('?entity', 'foaf:name', '?name');
  })
  .union(b => {
    b.where('?entity', 'a', 'org:Organization')
     .where('?entity', 'org:name', '?name');
  });
```

#### `orderBy(variable, direction)`

Add ORDER BY clause.

**Parameters:**
- `variable` (string) - Variable to order by (without ? prefix)
- `direction` (string) - 'ASC' or 'DESC' (default: 'ASC')

**Returns:** `SPARQLBuilder` - Builder instance

**Example:**

```javascript
builder
  .select('name', 'age')
  .orderBy('age', 'DESC')
  .orderBy('name', 'ASC');
// ORDER BY DESC(?age) ASC(?name)
```

#### `groupBy(...variables)`

Add GROUP BY clause.

**Parameters:**
- `variables` (...string) - Variables to group by

**Returns:** `SPARQLBuilder` - Builder instance

**Example:**

```javascript
builder
  .select('type')
  .select('(COUNT(?item) AS ?count)')
  .where('?item', 'a', '?type')
  .groupBy('type');
// GROUP BY ?type
```

#### `having(expression)`

Add HAVING clause.

**Parameters:**
- `expression` (string) - HAVING expression

**Returns:** `SPARQLBuilder` - Builder instance

**Example:**

```javascript
builder
  .select('type', '(COUNT(?item) AS ?count)')
  .where('?item', 'a', '?type')
  .groupBy('type')
  .having('COUNT(?item) > 10');
// HAVING(COUNT(?item) > 10)
```

#### `limit(n)`

Set LIMIT clause.

**Parameters:**
- `n` (number) - Maximum number of results

**Returns:** `SPARQLBuilder` - Builder instance

**Example:**

```javascript
builder
  .select('person')
  .where('?person', 'a', 'foaf:Person')
  .limit(100);
// LIMIT 100
```

#### `offset(n)`

Set OFFSET clause.

**Parameters:**
- `n` (number) - Number of results to skip

**Returns:** `SPARQLBuilder` - Builder instance

**Example:**

```javascript
builder
  .select('person')
  .where('?person', 'a', 'foaf:Person')
  .orderBy('person')
  .limit(20)
  .offset(40);
// Page 3 (results 41-60)
```

#### `build()`

Build the final SPARQL query string.

**Returns:** `string` - Complete SPARQL query

**Example:**

```javascript
const queryString = builder.build();
console.log(queryString);
```

## Query Factory Patterns

### QueryFactory Class

Pre-built query patterns for common operations.

#### `QueryFactory.findByType(type, properties, options)`

Find resources by type.

**Parameters:**
- `type` (string) - RDF type to query
- `properties` (Array<Object>) - Properties to select
  - `predicate` (string) - Property predicate
  - `variable` (string) - Variable name for property value
- `options` (Object) - Query options
  - `limit` (number) - Result limit
  - `orderBy` (string) - Variable to order by
  - `direction` (string) - Order direction ('ASC'/'DESC')

**Returns:** `string` - SPARQL query

**Example:**

```javascript
const query = QueryFactory.findByType(
  'foaf:Person',
  [
    { predicate: 'foaf:name', variable: 'name' },
    { predicate: 'foaf:email', variable: 'email' }
  ],
  { limit: 50, orderBy: 'name' }
);
```

#### `QueryFactory.findRelated(subject, predicate, options)`

Find resources related to a subject.

**Parameters:**
- `subject` (string) - Subject URI or variable
- `predicate` (string) - Relationship predicate
- `options` (Object)
  - `type` (string) - Filter by type
  - `limit` (number) - Result limit

**Returns:** `string` - SPARQL query

**Example:**

```javascript
const query = QueryFactory.findRelated(
  'ex:alice',
  'foaf:knows',
  { type: 'foaf:Person', limit: 10 }
);
```

#### `QueryFactory.textSearch(searchTerm, properties, options)`

Full-text search across properties.

**Parameters:**
- `searchTerm` (string) - Search term
- `properties` (Array<string>) - Properties to search
- `options` (Object)
  - `limit` (number) - Result limit
  - `caseInsensitive` (boolean) - Case-insensitive search (default: true)

**Returns:** `string` - SPARQL query

**Example:**

```javascript
const query = QueryFactory.textSearch(
  'alice',
  ['foaf:name', 'foaf:nick'],
  { limit: 20, caseInsensitive: true }
);
```

#### `QueryFactory.propertyPath(start, path, options)`

Query using property paths.

**Parameters:**
- `start` (string) - Start node
- `path` (string) - SPARQL property path expression
- `options` (Object)
  - `limit` (number) - Result limit
  - `distinct` (boolean) - Use DISTINCT

**Returns:** `string` - SPARQL query

**Example:**

```javascript
// Find friends of friends
const query = QueryFactory.propertyPath(
  'ex:alice',
  'foaf:knows/foaf:knows',
  { limit: 50, distinct: true }
);
```

#### `QueryFactory.aggregate(type, aggregates, groupBy)`

Aggregation queries.

**Parameters:**
- `type` (string) - Resource type
- `aggregates` (Array<Object>) - Aggregation expressions
  - `function` (string) - Aggregate function (COUNT, SUM, AVG, etc.)
  - `variable` (string) - Variable to aggregate
  - `alias` (string) - Result variable name
- `groupBy` (Array<string>) - Variables to group by

**Returns:** `string` - SPARQL query

**Example:**

```javascript
const query = QueryFactory.aggregate(
  'foaf:Person',
  [
    { function: 'COUNT', variable: '*', alias: 'total' },
    { function: 'AVG', variable: 'age', alias: 'avgAge' }
  ],
  ['country']
);
```

## Template-Based Queries

### Query Templates

Nunjucks templates for SPARQL generation.

#### SELECT Template

```nunjucks
PREFIX {{ prefix }}: <{{ namespace }}>

SELECT {% if distinct %}DISTINCT {% endif %}{% for v in variables %}?{{ v }}{% if not loop.last %} {% endif %}{% endfor %}
WHERE {
  {% for pattern in patterns %}
  {{ pattern.subject }} {{ pattern.predicate }} {{ pattern.object }} .
  {% endfor %}
}
{% if orderBy %}ORDER BY {{ orderBy }}{% endif %}
{% if limit %}LIMIT {{ limit }}{% endif %}
```

**Usage:**

```javascript
import { TemplateEngine } from '@unrdf/kgn';

const engine = new TemplateEngine({ templatesDir: './queries' });

const query = await engine.render('select.sparql.njk', {
  prefix: 'foaf',
  namespace: 'http://xmlns.com/foaf/0.1/',
  distinct: true,
  variables: ['person', 'name'],
  patterns: [
    { subject: '?person', predicate: 'a', object: 'foaf:Person' },
    { subject: '?person', predicate: 'foaf:name', object: '?name' }
  ],
  orderBy: '?name',
  limit: 100
});
```

#### CONSTRUCT Template

```nunjucks
PREFIX {{ prefix }}: <{{ namespace }}>

CONSTRUCT {
  {% for triple in constructPatterns %}
  {{ triple.subject }} {{ triple.predicate }} {{ triple.object }} .
  {% endfor %}
}
WHERE {
  {% for pattern in wherePatterns %}
  {{ pattern.subject }} {{ pattern.predicate }} {{ pattern.object }} .
  {% endfor %}
}
```

#### ASK Template

```nunjucks
PREFIX {{ prefix }}: <{{ namespace }}>

ASK {
  {% for pattern in patterns %}
  {{ pattern.subject }} {{ pattern.predicate }} {{ pattern.object }} .
  {% endfor %}
}
```

## Utility Functions

### `validateQuery(queryString)`

Validate SPARQL query syntax.

**Parameters:**
- `queryString` (string) - SPARQL query to validate

**Returns:** `Object`
- `valid` (boolean) - Whether query is valid
- `type` (string) - Query type if valid
- `variables` (Array<string>) - Query variables if valid
- `error` (string) - Error message if invalid

**Example:**

```javascript
import { prepareQuerySync } from '@unrdf/core/sparql/executor-sync.mjs';

function validateQuery(queryString) {
  try {
    const metadata = prepareQuerySync(queryString);
    return {
      valid: true,
      type: metadata.type,
      variables: metadata.variables
    };
  } catch (error) {
    return {
      valid: false,
      error: error.message
    };
  }
}

const result = validateQuery('SELECT ?s WHERE { ?s ?p ?o }');
console.log(result);
// { valid: true, type: 'SELECT', variables: ['s'] }
```

### `escapeString(str)`

Escape string for SPARQL literal.

**Parameters:**
- `str` (string) - String to escape

**Returns:** `string` - Escaped string

**Example:**

```javascript
function escapeString(str) {
  return str
    .replace(/\\/g, '\\\\')
    .replace(/"/g, '\\"')
    .replace(/\n/g, '\\n')
    .replace(/\r/g, '\\r')
    .replace(/\t/g, '\\t');
}

const escaped = escapeString('Line 1\nLine 2');
console.log(escaped); // Line 1\\nLine 2
```

### `formatURI(uri)`

Format URI for SPARQL.

**Parameters:**
- `uri` (string) - URI to format

**Returns:** `string` - Formatted URI with angle brackets

**Example:**

```javascript
function formatURI(uri) {
  if (uri.startsWith('http://') || uri.startsWith('https://')) {
    return `<${uri}>`;
  }
  return uri; // Assume it's a prefixed name
}

console.log(formatURI('http://example.org/alice')); // <http://example.org/alice>
console.log(formatURI('foaf:Person')); // foaf:Person
```

## Best Practices

### 1. Use Builder for Complex Queries

```javascript
// ✅ Good: Clear, maintainable
const query = new SPARQLBuilder()
  .prefix('foaf', 'http://xmlns.com/foaf/0.1/')
  .select('person', 'name', 'email')
  .where('?person', 'a', 'foaf:Person')
  .where('?person', 'foaf:name', '?name')
  .optional('?person', 'foaf:mbox', '?email')
  .filter('REGEX(?name, "^A", "i")')
  .orderBy('name')
  .limit(50)
  .build();

// ❌ Bad: Error-prone string concatenation
const query = `
  PREFIX foaf: <http://xmlns.com/foaf/0.1/>
  SELECT ?person ?name ?email
  WHERE {
    ?person a foaf:Person .
    ?person foaf:name ?name .
    OPTIONAL { ?person foaf:mbox ?email }
    FILTER(REGEX(?name, "^A", "i"))
  }
  ORDER BY ?name
  LIMIT 50
`;
```

### 2. Validate Before Execution

```javascript
const builder = new SPARQLBuilder()
  .select('person', 'name')
  .where('?person', 'foaf:name', '?name');

const queryString = builder.build();
const validation = validateQuery(queryString);

if (!validation.valid) {
  console.error('Invalid query:', validation.error);
  return;
}

const results = executeSelectSync(store, queryString);
```

### 3. Reuse Query Patterns

```javascript
// Define reusable patterns
const personPattern = (builder) => {
  builder
    .where('?person', 'a', 'foaf:Person')
    .where('?person', 'foaf:name', '?name');
};

// Use in multiple queries
const query1 = new SPARQLBuilder()
  .select('person', 'name')
  .apply(personPattern)
  .filter('?name = "Alice"')
  .build();

const query2 = new SPARQLBuilder()
  .select('person', 'name', 'email')
  .apply(personPattern)
  .optional('?person', 'foaf:mbox', '?email')
  .build();
```

### 4. Use Templates for Complex Patterns

```javascript
// Template for complex query
const complexQueryTemplate = `
PREFIX foaf: <http://xmlns.com/foaf/0.1/>

SELECT ?person ?name ?friendCount
WHERE {
  ?person a foaf:Person ;
          foaf:name ?name .
  {
    SELECT ?person (COUNT(?friend) AS ?friendCount)
    WHERE {
      ?person foaf:knows ?friend .
    }
    GROUP BY ?person
  }
  FILTER(?friendCount > {{ minFriends }})
}
ORDER BY DESC(?friendCount)
LIMIT {{ limit }}
`;

// Render with parameters
const query = await engine.renderString(complexQueryTemplate, {
  minFriends: 5,
  limit: 20
});
```

## See Also

- [RDF-KGN API](./rdf-kgn-api.md)
- [RDF Filters Reference](./rdf-filters-reference.md)
- [Build SPARQL Queries How-To](../how-to/build-sparql-queries.md)
- [SPARQL Query Generation Tutorial](../tutorials/sparql-query-generation.md)
