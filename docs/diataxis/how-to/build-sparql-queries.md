# How to Build SPARQL Queries

**Goal:** Construct SPARQL queries programmatically
**Time:** 5-10 minutes
**Difficulty:** Intermediate

## Problem

Writing SPARQL queries as strings is error-prone. You need a programmatic way to build queries with validation and reusability.

## Solution

Use template-based query builders and fluent APIs to construct SPARQL queries safely.

## Prerequisites

- `@unrdf/core` package installed
- Understanding of SPARQL syntax
- Completed [RDF-KGN Quickstart](../tutorials/rdf-kgn-quickstart.md)

## Quick Example

```javascript
import { executeSelectSync } from '@unrdf/core/sparql/executor-sync.mjs';

// Build query programmatically
const query = `
  PREFIX foaf: <http://xmlns.com/foaf/0.1/>

  SELECT ?person ?name
  WHERE {
    ?person a foaf:Person ;
            foaf:name ?name .
    FILTER(STRLEN(?name) > 5)
  }
  ORDER BY ?name
  LIMIT 10
`;

const results = executeSelectSync(store, query);
```

## Method 1: Template-Based Queries

### Create Query Template

Create `select-query.njk`:

```nunjucks
PREFIX {{ prefix }}: <{{ namespace }}>
{% for p in additionalPrefixes %}
PREFIX {{ p.prefix }}: <{{ p.namespace }}>
{% endfor %}

SELECT {% if distinct %}DISTINCT {% endif %}{% for v in variables %}{{ v | sparqlVar }}{% if not loop.last %} {% endif %}{% endfor %}
WHERE {
  {% for pattern in patterns %}
  {{ pattern.subject }} {{ pattern.predicate }} {{ pattern.object }} .
  {% endfor %}
  {% if optionalPatterns %}
  OPTIONAL {
    {% for pattern in optionalPatterns %}
    {{ pattern.subject }} {{ pattern.predicate }} {{ pattern.object }} .
    {% endfor %}
  }
  {% endif %}
  {% if filters %}
  {% for filter in filters %}
  FILTER({{ filter }})
  {% endfor %}
  {% endif %}
}
{% if orderBy %}
ORDER BY {{ orderBy }}
{% endif %}
{% if limit %}
LIMIT {{ limit }}
{% endif %}
```

### Use the Template

```javascript
import { TemplateEngine } from '@unrdf/kgn';
import { rdfFilters } from '@unrdf/kgn/src/filters/rdf.js';

const engine = new TemplateEngine({
  templatesDir: './queries',
  deterministicMode: true
});

// Register SPARQL filter
engine.env.addFilter('sparqlVar', rdfFilters.sparqlVar);

const queryData = {
  prefix: 'foaf',
  namespace: 'http://xmlns.com/foaf/0.1/',
  variables: ['person', 'name', 'email'],
  distinct: true,
  patterns: [
    { subject: '?person', predicate: 'a', object: 'foaf:Person' },
    { subject: '?person', predicate: 'foaf:name', object: '?name' },
    { subject: '?person', predicate: 'foaf:mbox', object: '?email' }
  ],
  filters: ['REGEX(?email, "@example\\\\.org$")'],
  orderBy: '?name',
  limit: 20
};

const query = await engine.render('select-query.njk', queryData);
console.log(query);
```

## Method 2: Query Builder Class

### Create Builder

```javascript
class SPARQLBuilder {
  constructor() {
    this.prefixes = new Map();
    this.selectVars = [];
    this.patterns = [];
    this.optionalPatterns = [];
    this.filterExpressions = [];
    this.orderByClause = null;
    this.limitValue = null;
    this.offsetValue = null;
    this.distinctFlag = false;
  }

  /**
   * Add namespace prefix
   */
  prefix(name, namespace) {
    this.prefixes.set(name, namespace);
    return this;
  }

  /**
   * Select variables
   */
  select(...vars) {
    this.selectVars.push(...vars);
    return this;
  }

  /**
   * Add SELECT DISTINCT
   */
  distinct() {
    this.distinctFlag = true;
    return this;
  }

  /**
   * Add triple pattern
   */
  where(subject, predicate, object) {
    this.patterns.push({ subject, predicate, object });
    return this;
  }

  /**
   * Add optional pattern
   */
  optional(subject, predicate, object) {
    this.optionalPatterns.push({ subject, predicate, object });
    return this;
  }

  /**
   * Add filter
   */
  filter(expression) {
    this.filterExpressions.push(expression);
    return this;
  }

  /**
   * Set ordering
   */
  orderBy(variable, direction = 'ASC') {
    this.orderByClause = `${direction}(?${variable})`;
    return this;
  }

  /**
   * Set limit
   */
  limit(n) {
    this.limitValue = n;
    return this;
  }

  /**
   * Set offset
   */
  offset(n) {
    this.offsetValue = n;
    return this;
  }

  /**
   * Build the query string
   */
  build() {
    let query = '';

    // Prefixes
    for (const [prefix, namespace] of this.prefixes) {
      query += `PREFIX ${prefix}: <${namespace}>\n`;
    }
    if (this.prefixes.size > 0) query += '\n';

    // SELECT
    query += 'SELECT ';
    if (this.distinctFlag) query += 'DISTINCT ';
    query += this.selectVars.map(v => `?${v}`).join(' ');
    query += '\n';

    // WHERE
    query += 'WHERE {\n';
    for (const p of this.patterns) {
      query += `  ${p.subject} ${p.predicate} ${p.object} .\n`;
    }

    if (this.optionalPatterns.length > 0) {
      query += '  OPTIONAL {\n';
      for (const p of this.optionalPatterns) {
        query += `    ${p.subject} ${p.predicate} ${p.object} .\n`;
      }
      query += '  }\n';
    }

    for (const filter of this.filterExpressions) {
      query += `  FILTER(${filter})\n`;
    }

    query += '}';

    // ORDER BY
    if (this.orderByClause) {
      query += `\nORDER BY ${this.orderByClause}`;
    }

    // LIMIT
    if (this.limitValue !== null) {
      query += `\nLIMIT ${this.limitValue}`;
    }

    // OFFSET
    if (this.offsetValue !== null) {
      query += `\nOFFSET ${this.offsetValue}`;
    }

    return query;
  }
}

export { SPARQLBuilder };
```

### Use the Builder

```javascript
import { SPARQLBuilder } from './sparql-builder.mjs';

const query = new SPARQLBuilder()
  .prefix('foaf', 'http://xmlns.com/foaf/0.1/')
  .prefix('dc', 'http://purl.org/dc/elements/1.1/')
  .distinct()
  .select('person', 'name', 'email')
  .where('?person', 'a', 'foaf:Person')
  .where('?person', 'foaf:name', '?name')
  .where('?person', 'foaf:mbox', '?email')
  .optional('?person', 'dc:description', '?bio')
  .filter('STRLEN(?name) > 3')
  .orderBy('name')
  .limit(50)
  .build();

console.log(query);
```

**Output:**
```sparql
PREFIX foaf: <http://xmlns.com/foaf/0.1/>
PREFIX dc: <http://purl.org/dc/elements/1.1/>

SELECT DISTINCT ?person ?name ?email
WHERE {
  ?person a foaf:Person .
  ?person foaf:name ?name .
  ?person foaf:mbox ?email .
  OPTIONAL {
    ?person dc:description ?bio .
  }
  FILTER(STRLEN(?name) > 3)
}
ORDER BY ASC(?name)
LIMIT 50
```

## Method 3: Parameterized Queries

### Define Query Factory

```javascript
/**
 * Query factory for common patterns
 */
class QueryFactory {
  /**
   * Find resources by type
   */
  static findByType(type, properties = [], options = {}) {
    const builder = new SPARQLBuilder()
      .prefix('rdf', 'http://www.w3.org/1999/02/22-rdf-syntax-ns#')
      .select('resource')
      .where('?resource', 'a', type);

    // Add properties
    properties.forEach(prop => {
      builder.select(prop.variable);
      builder.where('?resource', prop.predicate, `?${prop.variable}`);
    });

    // Apply options
    if (options.limit) builder.limit(options.limit);
    if (options.orderBy) builder.orderBy(options.orderBy);

    return builder.build();
  }

  /**
   * Find relationships
   */
  static findRelated(subject, predicate, options = {}) {
    const builder = new SPARQLBuilder()
      .select('related')
      .where(subject, predicate, '?related');

    if (options.type) {
      builder.where('?related', 'a', options.type);
    }

    if (options.limit) builder.limit(options.limit);

    return builder.build();
  }

  /**
   * Text search query
   */
  static textSearch(searchTerm, properties, options = {}) {
    const builder = new SPARQLBuilder()
      .select('resource', 'match');

    properties.forEach((prop, i) => {
      if (i === 0) {
        builder.where('?resource', prop, '?match');
      } else {
        builder.optional('?resource', prop, '?match');
      }
    });

    builder.filter(`REGEX(?match, "${searchTerm}", "i")`);

    if (options.limit) builder.limit(options.limit);

    return builder.build();
  }
}

// Usage examples
const query1 = QueryFactory.findByType(
  'foaf:Person',
  [
    { predicate: 'foaf:name', variable: 'name' },
    { predicate: 'foaf:mbox', variable: 'email' }
  ],
  { limit: 10, orderBy: 'name' }
);

const query2 = QueryFactory.findRelated(
  'ex:alice',
  'foaf:knows',
  { type: 'foaf:Person', limit: 5 }
);

const query3 = QueryFactory.textSearch(
  'smith',
  ['foaf:name', 'foaf:nick'],
  { limit: 20 }
);

console.log('Query 1:', query1);
console.log('Query 2:', query2);
console.log('Query 3:', query3);
```

## Common Query Patterns

### Pattern 1: Pagination

```javascript
function paginatedQuery(page, perPage) {
  return new SPARQLBuilder()
    .prefix('foaf', 'http://xmlns.com/foaf/0.1/')
    .select('person', 'name')
    .where('?person', 'a', 'foaf:Person')
    .where('?person', 'foaf:name', '?name')
    .orderBy('name')
    .limit(perPage)
    .offset(page * perPage)
    .build();
}

// Get page 2 (20 results per page)
const query = paginatedQuery(2, 20);
```

### Pattern 2: Aggregation

```javascript
const countQuery = `
  PREFIX foaf: <http://xmlns.com/foaf/0.1/>

  SELECT (COUNT(?person) AS ?total)
  WHERE {
    ?person a foaf:Person .
  }
`;
```

### Pattern 3: Property Path

```javascript
const pathQuery = `
  PREFIX foaf: <http://xmlns.com/foaf/0.1/>

  SELECT ?person ?friend
  WHERE {
    ?person foaf:knows/foaf:knows ?friend .
    FILTER(?person != ?friend)
  }
`;
```

### Pattern 4: UNION

```javascript
const unionQuery = `
  PREFIX foaf: <http://xmlns.com/foaf/0.1/>
  PREFIX org: <http://www.w3.org/ns/org#>

  SELECT ?entity ?name
  WHERE {
    {
      ?entity a foaf:Person ;
              foaf:name ?name .
    }
    UNION
    {
      ?entity a org:Organization ;
              org:name ?name .
    }
  }
`;
```

## Validation

### Validate Query Syntax

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

const result = validateQuery(query);
if (result.valid) {
  console.log(`✅ Valid ${result.type} query`);
  console.log(`Variables: ${result.variables.join(', ')}`);
} else {
  console.error(`❌ Invalid query: ${result.error}`);
}
```

## Best Practices

1. **Use builders for complex queries:** More maintainable than string concatenation
2. **Validate inputs:** Sanitize user input to prevent injection
3. **Cache prepared queries:** Reuse query metadata
4. **Use prefixes consistently:** Define once, use everywhere
5. **Test queries:** Verify against known data
6. **Document query purpose:** Add comments explaining intent

## Troubleshooting

### Problem: Invalid variable names

**Solution:** Use `sparqlVar` filter to ensure valid names:

```javascript
engine.env.addFilter('sparqlVar', name => {
  const clean = name.replace(/[^a-zA-Z0-9_]/g, '_');
  return `?${clean}`;
});
```

### Problem: Missing prefixes

**Solution:** Always define prefixes in builder:

```javascript
builder
  .prefix('rdf', 'http://www.w3.org/1999/02/22-rdf-syntax-ns#')
  .prefix('rdfs', 'http://www.w3.org/2000/01/rdf-schema#');
```

### Problem: Query returns no results

**Solution:** Validate each part:

```javascript
// Test patterns individually
const testQuery = `
  SELECT * WHERE {
    ?s ?p ?o .
  } LIMIT 10
`;

// Verify prefixes resolve
// Check filter expressions
// Validate property paths
```

## Related Guides

- [Generate RDF from Templates](./generate-rdf-from-templates.md)
- [Validate RDF with SHACL](./validate-rdf-with-shacl.md)
- [Optimize RDF Serialization](./optimize-rdf-serialization.md)

## Reference

- [SPARQL Query Builder API](../reference/sparql-query-builder-api.md)
- [RDF-KGN API](../reference/rdf-kgn-api.md)
- [Performance Optimization Strategies](../explanation/performance-optimization-strategies.md)
