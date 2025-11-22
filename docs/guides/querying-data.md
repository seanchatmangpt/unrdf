# How-To: Query Data with SPARQL

Task-oriented guide for common SPARQL querying patterns in UNRDF.

## Quick Reference

```javascript
import { query, select, ask, construct, describe } from 'unrdf/knowledge-engine';

// Generic query
const results = await query(store, sparqlString);

// Typed queries
const rows = await select(store, 'SELECT ...');
const bool = await ask(store, 'ASK { ... }');
const graph = await construct(store, 'CONSTRUCT { ... }');
const about = await describe(store, 'DESCRIBE <uri>');
```

## How to Find All Instances of a Type

```javascript
const people = await select(store, `
  PREFIX foaf: <http://xmlns.com/foaf/0.1/>

  SELECT ?person ?name
  WHERE {
    ?person a foaf:Person ;
            foaf:name ?name .
  }
`);

// Process results
people.forEach(row => {
  console.log(`${row.person.value}: ${row.name.value}`);
});
```

## How to Filter by Property Values

### Numeric Filters

```javascript
const highValue = await select(store, `
  PREFIX ex: <http://example.org/>

  SELECT ?item ?price
  WHERE {
    ?item ex:price ?price .
    FILTER (?price > 100)
  }
`);
```

### String Filters

```javascript
// Case-insensitive search
const matches = await select(store, `
  PREFIX foaf: <http://xmlns.com/foaf/0.1/>

  SELECT ?person ?name
  WHERE {
    ?person foaf:name ?name .
    FILTER (CONTAINS(LCASE(?name), "alice"))
  }
`);
```

### Date Filters

```javascript
const recent = await select(store, `
  PREFIX xsd: <http://www.w3.org/2001/XMLSchema#>
  PREFIX ex: <http://example.org/>

  SELECT ?event ?date
  WHERE {
    ?event ex:date ?date .
    FILTER (?date > "2024-01-01"^^xsd:date)
  }
`);
```

## How to Join Related Data

```javascript
const employeeDepts = await select(store, `
  PREFIX ex: <http://example.org/>
  PREFIX foaf: <http://xmlns.com/foaf/0.1/>

  SELECT ?name ?deptName ?salary
  WHERE {
    ?person a foaf:Person ;
            foaf:name ?name ;
            ex:department ?dept ;
            ex:salary ?salary .
    ?dept ex:name ?deptName .
  }
`);
```

## How to Handle Optional Data

```javascript
const withOptional = await select(store, `
  PREFIX foaf: <http://xmlns.com/foaf/0.1/>

  SELECT ?name ?email ?phone
  WHERE {
    ?person foaf:name ?name .
    OPTIONAL { ?person foaf:mbox ?email }
    OPTIONAL { ?person foaf:phone ?phone }
  }
`);

// Check for optional values
withOptional.forEach(row => {
  const email = row.email?.value || 'N/A';
  const phone = row.phone?.value || 'N/A';
  console.log(`${row.name.value}: ${email}, ${phone}`);
});
```

## How to Aggregate Data

### Count Items

```javascript
const counts = await select(store, `
  PREFIX ex: <http://example.org/>

  SELECT ?dept (COUNT(?person) AS ?count)
  WHERE {
    ?person ex:department ?dept .
  }
  GROUP BY ?dept
`);
```

### Sum and Average

```javascript
const stats = await select(store, `
  PREFIX ex: <http://example.org/>

  SELECT ?dept
         (SUM(?salary) AS ?total)
         (AVG(?salary) AS ?average)
  WHERE {
    ?person ex:department ?dept ;
            ex:salary ?salary .
  }
  GROUP BY ?dept
`);
```

## How to Check Existence

```javascript
const exists = await ask(store, `
  PREFIX ex: <http://example.org/>

  ASK {
    ex:alice ex:knows ex:bob .
  }
`);

if (exists) {
  console.log('Alice knows Bob');
}
```

## How to Navigate Relationships

### Direct Relationships

```javascript
const friends = await select(store, `
  PREFIX foaf: <http://xmlns.com/foaf/0.1/>
  PREFIX ex: <http://example.org/>

  SELECT ?friend ?friendName
  WHERE {
    ex:alice foaf:knows ?friend .
    ?friend foaf:name ?friendName .
  }
`);
```

### Transitive Relationships (Property Paths)

```javascript
// Friends of friends
const fof = await select(store, `
  PREFIX foaf: <http://xmlns.com/foaf/0.1/>

  SELECT DISTINCT ?person
  WHERE {
    ex:alice foaf:knows/foaf:knows ?person .
    FILTER (?person != ex:alice)
  }
`);

// All reachable via any path length
const allReachable = await select(store, `
  PREFIX foaf: <http://xmlns.com/foaf/0.1/>

  SELECT DISTINCT ?person
  WHERE {
    ex:alice foaf:knows+ ?person .
  }
`);
```

## How to Create Derived Data

```javascript
const derivedGraph = await construct(store, `
  PREFIX foaf: <http://xmlns.com/foaf/0.1/>
  PREFIX ex: <http://example.org/>

  CONSTRUCT {
    ?person ex:colleague ?colleague .
  }
  WHERE {
    ?person ex:department ?dept .
    ?colleague ex:department ?dept .
    FILTER (?person != ?colleague)
  }
`);
```

## How to Paginate Results

```javascript
async function getPage(store, pageNum, pageSize = 10) {
  return await select(store, `
    PREFIX foaf: <http://xmlns.com/foaf/0.1/>

    SELECT ?person ?name
    WHERE {
      ?person a foaf:Person ;
              foaf:name ?name .
    }
    ORDER BY ?name
    LIMIT ${pageSize}
    OFFSET ${(pageNum - 1) * pageSize}
  `);
}

const page1 = await getPage(store, 1);
const page2 = await getPage(store, 2);
```

## How to Use Query Parameters Safely

```javascript
// WARNING: Never interpolate user input directly
// Use parameterized queries or escape values

function escapeString(value) {
  return value.replace(/\\/g, '\\\\').replace(/"/g, '\\"');
}

async function findByName(store, searchName) {
  const escaped = escapeString(searchName);
  return await select(store, `
    PREFIX foaf: <http://xmlns.com/foaf/0.1/>

    SELECT ?person ?name
    WHERE {
      ?person foaf:name ?name .
      FILTER (CONTAINS(?name, "${escaped}"))
    }
  `);
}
```

## Troubleshooting

### Empty Results

1. Check prefixes are declared
2. Verify data is loaded (`store.size`)
3. Test simpler query first
4. Check for typos in IRIs

### Slow Queries

1. Add more specific patterns first
2. Use LIMIT during development
3. Avoid `SELECT *` in production
4. Index frequently-used predicates

### Type Mismatches

```javascript
// Wrong: comparing string to number
FILTER (?age > 30)  // Fails if age is "30" string

// Right: cast or use typed literals
FILTER (xsd:integer(?age) > 30)
```

## Related

- [SPARQL Tutorial](../tutorials/sparql.md) - In-depth learning
- [Performance Guide](./performance-optimization.md) - Query optimization
- [API Reference](../reference/api-reference.md) - Full query API
