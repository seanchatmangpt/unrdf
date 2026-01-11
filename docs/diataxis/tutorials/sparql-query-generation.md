# SPARQL Query Generation Tutorial

**Duration:** 15 minutes
**Level:** Intermediate
**Prerequisites:** Complete [RDF-KGN Quickstart](./rdf-kgn-quickstart.md)

## What You'll Learn

Learn to generate SPARQL queries programmatically:

- Build SPARQL SELECT queries from templates
- Generate CONSTRUCT queries for RDF transformation
- Create parametrized queries with variables
- Use query templates for common patterns

## Why Generate SPARQL?

Hand-writing SPARQL queries is error-prone. Template-based generation provides:

- **Type safety:** Validate variables and patterns
- **Reusability:** Define query patterns once
- **Maintainability:** Update queries in one place
- **Documentation:** Self-documenting query templates

## Step 1: Basic SELECT Query Template (3 min)

Create `/tmp/select-query.njk`:

```nunjucks
PREFIX {{ prefix }}: <{{ namespace }}>
PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>

SELECT {% for var in selectVars %}{{ var | sparqlVar }}{% if not loop.last %} {% endif %}{% endfor %}
WHERE {
  {% for pattern in wherePatterns %}
  {{ pattern.subject }} {{ pattern.predicate }} {{ pattern.object }} .
  {% endfor %}
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
{% if offset %}
OFFSET {{ offset }}
{% endif %}
```

Create `/tmp/generate-select.mjs`:

```javascript
import { TemplateEngine } from '@unrdf/kgn';
import { rdfFilters } from '@unrdf/kgn/src/filters/rdf.js';

const engine = new TemplateEngine({
  templatesDir: '/tmp',
  deterministicMode: true
});

// Register RDF filters
Object.entries(rdfFilters).forEach(([name, filter]) => {
  engine.env.addFilter(name, filter);
});

// Define query structure
const queryData = {
  prefix: 'code',
  namespace: 'http://unrdf.org/vocab/code#',
  selectVars: ['name', 'description', 'paramCount'],
  wherePatterns: [
    {
      subject: '?func',
      predicate: 'a',
      object: 'code:Function'
    },
    {
      subject: '?func',
      predicate: 'code:name',
      object: '?name'
    },
    {
      subject: '?func',
      predicate: 'doc:description',
      object: '?description'
    },
    {
      subject: '?func',
      predicate: 'code:paramCount',
      object: '?paramCount'
    }
  ],
  filters: ['?paramCount > 0'],
  orderBy: 'DESC(?paramCount)',
  limit: 10
};

const query = await engine.render('select-query.njk', queryData);

console.log('Generated SPARQL Query:');
console.log(query);
console.log('\nQuery can be executed with:');
console.log('executeSelectSync(store, query)');
```

Run it:

```bash
cd /home/user/unrdf/packages/kgn
timeout 5s node /tmp/generate-select.mjs
```

**Expected Output:**
```sparql
PREFIX code: <http://unrdf.org/vocab/code#>
PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>

SELECT ?name ?description ?paramCount
WHERE {
  ?func a code:Function .
  ?func code:name ?name .
  ?func doc:description ?description .
  ?func code:paramCount ?paramCount .
  FILTER(?paramCount > 0)
}
ORDER BY DESC(?paramCount)
LIMIT 10
```

## Step 2: CONSTRUCT Query Template (4 min)

CONSTRUCT queries transform RDF data. Let's create a template.

Create `/tmp/construct-query.njk`:

```nunjucks
PREFIX {{ prefix }}: <{{ namespace }}>
PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>

CONSTRUCT {
  {% for triple in constructPatterns %}
  {{ triple.subject }} {{ triple.predicate }} {{ triple.object }} .
  {% endfor %}
}
WHERE {
  {% for pattern in wherePatterns %}
  {{ pattern.subject }} {{ pattern.predicate }} {{ pattern.object }} .
  {% endfor %}
  {% if optional %}
  OPTIONAL {
    {% for opt in optional %}
    {{ opt.subject }} {{ opt.predicate }} {{ opt.object }} .
    {% endfor %}
  }
  {% endif %}
}
```

Create `/tmp/generate-construct.mjs`:

```javascript
import { TemplateEngine } from '@unrdf/kgn';
import { rdfFilters } from '@unrdf/kgn/src/filters/rdf.js';

const engine = new TemplateEngine({
  templatesDir: '/tmp',
  deterministicMode: true
});

Object.entries(rdfFilters).forEach(([name, filter]) => {
  engine.env.addFilter(name, filter);
});

// Transform functions to a simplified representation
const queryData = {
  prefix: 'api',
  namespace: 'http://example.org/api#',

  // What we want to construct (simplified API documentation)
  constructPatterns: [
    {
      subject: '?endpoint',
      predicate: 'a',
      object: 'api:Endpoint'
    },
    {
      subject: '?endpoint',
      predicate: 'api:functionName',
      object: '?name'
    },
    {
      subject: '?endpoint',
      predicate: 'api:summary',
      object: '?description'
    },
    {
      subject: '?endpoint',
      predicate: 'api:parameters',
      object: '?paramCount'
    }
  ],

  // Source data (from code analysis)
  wherePatterns: [
    {
      subject: '?func',
      predicate: 'a',
      object: 'code:Function'
    },
    {
      subject: '?func',
      predicate: 'code:name',
      object: '?name'
    },
    {
      subject: '?func',
      predicate: 'code:paramCount',
      object: '?paramCount'
    }
  ],

  optional: [
    {
      subject: '?func',
      predicate: 'doc:description',
      object: '?description'
    }
  ]
};

// Bind ?endpoint based on function name
queryData.constructPatterns[0].subject = 'api:?name';
queryData.constructPatterns[1].subject = 'api:?name';
queryData.constructPatterns[2].subject = 'api:?name';
queryData.constructPatterns[3].subject = 'api:?name';

const query = await engine.render('construct-query.njk', queryData);

console.log('Generated CONSTRUCT Query:');
console.log(query);
console.log('\n✅ This query transforms code metadata → API documentation');
```

Run it:

```bash
cd /home/user/unrdf/packages/kgn
timeout 5s node /tmp/generate-construct.mjs
```

**Expected Output:**
```sparql
PREFIX api: <http://example.org/api#>

CONSTRUCT {
  api:?name a api:Endpoint .
  api:?name api:functionName ?name .
  api:?name api:summary ?description .
  api:?name api:parameters ?paramCount .
}
WHERE {
  ?func a code:Function .
  ?func code:name ?name .
  ?func code:paramCount ?paramCount .
  OPTIONAL {
    ?func doc:description ?description .
  }
}
```

## Step 3: Parametrized Query Builder (4 min)

Create reusable query builders for common patterns.

Create `/tmp/query-builder.mjs`:

```javascript
import { rdfFilters } from '@unrdf/kgn/src/filters/rdf.js';

/**
 * Query builder for finding resources by type
 */
class ResourceQueryBuilder {
  constructor(prefix, namespace) {
    this.prefix = prefix;
    this.namespace = namespace;
    this.type = null;
    this.properties = [];
    this.filterExpressions = [];
    this.limitValue = null;
    this.orderByValue = null;
  }

  /**
   * Set resource type to query
   */
  ofType(type) {
    this.type = type;
    return this;
  }

  /**
   * Add property to select
   */
  selectProperty(property, variable) {
    this.properties.push({ property, variable });
    return this;
  }

  /**
   * Add filter expression
   */
  filter(expression) {
    this.filterExpressions.push(expression);
    return this;
  }

  /**
   * Set result limit
   */
  limit(n) {
    this.limitValue = n;
    return this;
  }

  /**
   * Set ordering
   */
  orderBy(variable, direction = 'ASC') {
    this.orderByValue = `${direction}(?${variable})`;
    return this;
  }

  /**
   * Build the SPARQL query
   */
  build() {
    const vars = this.properties.map(p => `?${p.variable}`).join(' ');

    let query = `PREFIX ${this.prefix}: <${this.namespace}>\n\n`;
    query += `SELECT ?resource ${vars}\n`;
    query += `WHERE {\n`;
    query += `  ?resource a ${this.prefix}:${this.type} .\n`;

    for (const prop of this.properties) {
      query += `  ?resource ${this.prefix}:${prop.property} ?${prop.variable} .\n`;
    }

    for (const filter of this.filterExpressions) {
      query += `  FILTER(${filter})\n`;
    }

    query += `}`;

    if (this.orderByValue) {
      query += `\nORDER BY ${this.orderByValue}`;
    }

    if (this.limitValue) {
      query += `\nLIMIT ${this.limitValue}`;
    }

    return query;
  }
}

// Example usage
const query1 = new ResourceQueryBuilder('code', 'http://unrdf.org/vocab/code#')
  .ofType('Function')
  .selectProperty('name', 'funcName')
  .selectProperty('paramCount', 'params')
  .filter('?params > 2')
  .orderBy('params', 'DESC')
  .limit(5)
  .build();

console.log('=== Query 1: Functions with >2 parameters ===');
console.log(query1);
console.log();

const query2 = new ResourceQueryBuilder('code', 'http://unrdf.org/vocab/code#')
  .ofType('Class')
  .selectProperty('name', 'className')
  .selectProperty('methodCount', 'methods')
  .filter('?methods >= 1')
  .orderBy('className')
  .build();

console.log('=== Query 2: Classes with methods ===');
console.log(query2);
```

Run it:

```bash
cd /home/user/unrdf/packages/kgn
timeout 5s node /tmp/query-builder.mjs
```

**Expected Output:**
```sparql
=== Query 1: Functions with >2 parameters ===
PREFIX code: <http://unrdf.org/vocab/code#>

SELECT ?resource ?funcName ?params
WHERE {
  ?resource a code:Function .
  ?resource code:name ?funcName .
  ?resource code:paramCount ?params .
  FILTER(?params > 2)
}
ORDER BY DESC(?params)
LIMIT 5

=== Query 2: Classes with methods ===
PREFIX code: <http://unrdf.org/vocab/code#>

SELECT ?resource ?className ?methods
WHERE {
  ?resource a code:Class .
  ?resource code:name ?className .
  ?resource code:methodCount ?methods .
  FILTER(?methods >= 1)
}
ORDER BY ASC(?className)
```

## Step 4: Execute Generated Queries (4 min)

Let's execute the generated queries on real data.

Create `/tmp/execute-generated.mjs`:

```javascript
import { createStore } from '@unrdf/oxigraph';
import { executeSelectSync } from '@unrdf/core/sparql/executor-sync.mjs';
import { Parser } from 'n3';

// Sample RDF data
const rdfData = `
@prefix code: <http://unrdf.org/vocab/code#> .

code:func1 a code:Function ;
  code:name "calculateTotal" ;
  code:paramCount 3 .

code:func2 a code:Function ;
  code:name "processData" ;
  code:paramCount 5 .

code:func3 a code:Function ;
  code:name "init" ;
  code:paramCount 0 .

code:class1 a code:Class ;
  code:name "UserService" ;
  code:methodCount 8 .

code:class2 a code:Class ;
  code:name "DataProcessor" ;
  code:methodCount 12 .
`;

// Load data
const store = createStore();
const parser = new Parser({ format: 'text/turtle' });

parser.parse(rdfData).forEach(quad => {
  store.add(quad);
});

// Generated query from previous step
const query = `
PREFIX code: <http://unrdf.org/vocab/code#>

SELECT ?resource ?funcName ?params
WHERE {
  ?resource a code:Function .
  ?resource code:name ?funcName .
  ?resource code:paramCount ?params .
  FILTER(?params > 2)
}
ORDER BY DESC(?params)
LIMIT 5
`;

console.log('Executing generated query...\n');

const results = executeSelectSync(store, query);

console.log(`Found ${results.length} results:\n`);

results.forEach((row, i) => {
  console.log(`${i + 1}. Function: ${row.funcName.value}`);
  console.log(`   Parameters: ${row.params.value}`);
  console.log();
});
```

Run it:

```bash
timeout 5s node /tmp/execute-generated.mjs
```

**Expected Output:**
```
Executing generated query...

Found 2 results:

1. Function: processData
   Parameters: 5

2. Function: calculateTotal
   Parameters: 3
```

## Summary

You've learned how to:

✅ Create reusable SPARQL query templates
✅ Generate SELECT and CONSTRUCT queries
✅ Build parametrized query builders
✅ Execute generated queries on RDF data

## Next Steps

- **Tutorial:** [RDF Validation Workflow](./rdf-validation-workflow.md) - 25 min
- **How-to:** [Build SPARQL Queries](../how-to/build-sparql-queries.md)
- **Reference:** [SPARQL Query Builder API](../reference/sparql-query-builder-api.md)

## Key Takeaways

1. **Templates reduce errors:** Pre-validated structure
2. **Builders improve ergonomics:** Fluent API for query construction
3. **Parametrization enables reuse:** One template, many queries
4. **Generated queries are maintainable:** Update template, regenerate all

## Best Practices

- **Validate templates:** Test with known data first
- **Use filters correctly:** `sparqlVar` ensures valid variable names
- **Document query purpose:** Add comments to templates
- **Version templates:** Track changes with git

## Reference

- [SPARQL Query Builder API](../reference/sparql-query-builder-api.md)
- [RDF Filters Reference](../reference/rdf-filters-reference.md)
- [Performance Optimization Strategies](../explanation/performance-optimization-strategies.md)
