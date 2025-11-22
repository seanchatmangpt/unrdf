# Tutorial: SPARQL Queries

Master SPARQL querying with UNRDF's Comunica-powered query engine.

## Learning Objectives

By the end of this tutorial, you will:

- Write SELECT, ASK, CONSTRUCT, and DESCRIBE queries
- Use FILTER, OPTIONAL, and UNION patterns
- Perform aggregations and subqueries
- Optimize queries for performance

## Prerequisites

- Completed [Creating RDF Documents](./creating-rdf-documents.md) tutorial
- Basic understanding of SQL concepts
- Sample data loaded (provided below)

## Sample Data Setup

Create `data/social-network.ttl`:

```turtle
@prefix ex: <http://example.org/> .
@prefix foaf: <http://xmlns.com/foaf/0.1/> .
@prefix xsd: <http://www.w3.org/2001/XMLSchema#> .

ex:alice a foaf:Person ;
    foaf:name "Alice Smith" ;
    foaf:age 30 ;
    foaf:knows ex:bob, ex:carol ;
    ex:department ex:engineering ;
    ex:salary 85000 .

ex:bob a foaf:Person ;
    foaf:name "Bob Jones" ;
    foaf:age 25 ;
    foaf:knows ex:alice, ex:david ;
    ex:department ex:sales ;
    ex:salary 65000 .

ex:carol a foaf:Person ;
    foaf:name "Carol White" ;
    foaf:age 35 ;
    foaf:knows ex:alice ;
    ex:department ex:engineering ;
    ex:salary 95000 .

ex:david a foaf:Person ;
    foaf:name "David Brown" ;
    foaf:age 28 ;
    ex:department ex:marketing ;
    ex:salary 70000 .

ex:engineering a ex:Department ;
    ex:name "Engineering" ;
    ex:budget 500000 .

ex:sales a ex:Department ;
    ex:name "Sales" ;
    ex:budget 300000 .

ex:marketing a ex:Department ;
    ex:name "Marketing" ;
    ex:budget 250000 .
```

Load it:

```javascript
// src/sparql-tutorial.mjs
import { readFileSync } from 'node:fs';
import { parseTurtle, select, ask, construct, describe } from 'unrdf/knowledge-engine';

const data = readFileSync('./data/social-network.ttl', 'utf-8');
const store = await parseTurtle(data);

console.log(`Loaded ${store.size} triples`);
```

## Step 1: Basic SELECT Queries

### Simple Pattern Matching

```javascript
// Find all people
const people = await select(store, `
  PREFIX foaf: <http://xmlns.com/foaf/0.1/>

  SELECT ?person ?name
  WHERE {
    ?person a foaf:Person ;
            foaf:name ?name .
  }
`);

console.log('People:', people.map(r => r.name.value));
// ['Alice Smith', 'Bob Jones', 'Carol White', 'David Brown']
```

### Multiple Patterns

```javascript
// Find people with their departments
const employees = await select(store, `
  PREFIX foaf: <http://xmlns.com/foaf/0.1/>
  PREFIX ex: <http://example.org/>

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

## Step 2: OPTIONAL Patterns

Handle missing data gracefully:

```javascript
// Some people might not know anyone
const peopleWithFriends = await select(store, `
  PREFIX foaf: <http://xmlns.com/foaf/0.1/>

  SELECT ?name (COUNT(?friend) AS ?friendCount)
  WHERE {
    ?person a foaf:Person ;
            foaf:name ?name .
    OPTIONAL { ?person foaf:knows ?friend }
  }
  GROUP BY ?person ?name
`);

// David has no foaf:knows, but still appears with friendCount=0
```

## Step 3: FILTER Expressions

### Numeric Filters

```javascript
// High earners
const highEarners = await select(store, `
  PREFIX foaf: <http://xmlns.com/foaf/0.1/>
  PREFIX ex: <http://example.org/>

  SELECT ?name ?salary
  WHERE {
    ?person foaf:name ?name ;
            ex:salary ?salary .
    FILTER (?salary > 70000)
  }
  ORDER BY DESC(?salary)
`);
```

### String Filters

```javascript
// Names containing 'a'
const namesWithA = await select(store, `
  PREFIX foaf: <http://xmlns.com/foaf/0.1/>

  SELECT ?name
  WHERE {
    ?person foaf:name ?name .
    FILTER (CONTAINS(LCASE(?name), "a"))
  }
`);
```

### Regex Filters

```javascript
// Names starting with 'A' or 'B'
const filtered = await select(store, `
  PREFIX foaf: <http://xmlns.com/foaf/0.1/>

  SELECT ?name
  WHERE {
    ?person foaf:name ?name .
    FILTER (REGEX(?name, "^[AB]", "i"))
  }
`);
```

## Step 4: UNION Patterns

Combine alternative patterns:

```javascript
// Find engineers OR people over 30
const engineersOrSenior = await select(store, `
  PREFIX foaf: <http://xmlns.com/foaf/0.1/>
  PREFIX ex: <http://example.org/>

  SELECT DISTINCT ?name ?reason
  WHERE {
    ?person foaf:name ?name .
    {
      ?person ex:department ex:engineering .
      BIND ("engineer" AS ?reason)
    }
    UNION
    {
      ?person foaf:age ?age .
      FILTER (?age >= 30)
      BIND ("senior" AS ?reason)
    }
  }
`);
```

## Step 5: Aggregations

### COUNT

```javascript
// Count people per department
const deptCounts = await select(store, `
  PREFIX foaf: <http://xmlns.com/foaf/0.1/>
  PREFIX ex: <http://example.org/>

  SELECT ?deptName (COUNT(?person) AS ?employeeCount)
  WHERE {
    ?person a foaf:Person ;
            ex:department ?dept .
    ?dept ex:name ?deptName .
  }
  GROUP BY ?dept ?deptName
`);
```

### SUM, AVG, MIN, MAX

```javascript
// Department salary stats
const salaryStats = await select(store, `
  PREFIX foaf: <http://xmlns.com/foaf/0.1/>
  PREFIX ex: <http://example.org/>

  SELECT ?deptName
         (SUM(?salary) AS ?totalSalary)
         (AVG(?salary) AS ?avgSalary)
         (MIN(?salary) AS ?minSalary)
         (MAX(?salary) AS ?maxSalary)
  WHERE {
    ?person a foaf:Person ;
            ex:department ?dept ;
            ex:salary ?salary .
    ?dept ex:name ?deptName .
  }
  GROUP BY ?dept ?deptName
`);
```

### HAVING

```javascript
// Departments with more than 1 employee
const largeDepts = await select(store, `
  PREFIX ex: <http://example.org/>

  SELECT ?deptName (COUNT(?person) AS ?count)
  WHERE {
    ?person ex:department ?dept .
    ?dept ex:name ?deptName .
  }
  GROUP BY ?dept ?deptName
  HAVING (COUNT(?person) > 1)
`);
```

## Step 6: ASK Queries

Boolean existence checks:

```javascript
import { ask } from 'unrdf/knowledge-engine';

// Does anyone earn over 100k?
const hasHighEarner = await ask(store, `
  PREFIX ex: <http://example.org/>

  ASK {
    ?person ex:salary ?salary .
    FILTER (?salary > 100000)
  }
`);

console.log(`High earner exists: ${hasHighEarner}`); // false
```

## Step 7: CONSTRUCT Queries

Create new graphs from patterns:

```javascript
import { construct, toTurtle } from 'unrdf/knowledge-engine';

// Create a manager relationship graph
const managerGraph = await construct(store, `
  PREFIX foaf: <http://xmlns.com/foaf/0.1/>
  PREFIX ex: <http://example.org/>

  CONSTRUCT {
    ?person ex:hasColleague ?colleague .
    ?colleague ex:hasColleague ?person .
  }
  WHERE {
    ?person ex:department ?dept .
    ?colleague ex:department ?dept .
    FILTER (?person != ?colleague)
  }
`);

const turtle = await toTurtle(managerGraph);
console.log(turtle);
```

## Step 8: DESCRIBE Queries

Get all information about a resource:

```javascript
import { describe } from 'unrdf/knowledge-engine';

// Describe Alice
const aliceInfo = await describe(store, `
  PREFIX ex: <http://example.org/>

  DESCRIBE ex:alice
`);

// Returns all triples where ex:alice is subject or object
```

## Step 9: Subqueries

Nested queries for complex logic:

```javascript
// Find people who know someone in their department
const sameDeptFriends = await select(store, `
  PREFIX foaf: <http://xmlns.com/foaf/0.1/>
  PREFIX ex: <http://example.org/>

  SELECT ?name ?friendName ?deptName
  WHERE {
    ?person foaf:name ?name ;
            foaf:knows ?friend ;
            ex:department ?dept .
    ?friend foaf:name ?friendName ;
            ex:department ?dept .
    ?dept ex:name ?deptName .
  }
`);
```

### With Subquery

```javascript
// People earning above average
const aboveAverage = await select(store, `
  PREFIX ex: <http://example.org/>
  PREFIX foaf: <http://xmlns.com/foaf/0.1/>

  SELECT ?name ?salary ?avgSalary
  WHERE {
    ?person foaf:name ?name ;
            ex:salary ?salary .
    {
      SELECT (AVG(?s) AS ?avgSalary)
      WHERE { ?p ex:salary ?s }
    }
    FILTER (?salary > ?avgSalary)
  }
`);
```

## Step 10: Property Paths

Navigate graph relationships:

```javascript
// Friends of friends (path length 2)
const fof = await select(store, `
  PREFIX foaf: <http://xmlns.com/foaf/0.1/>

  SELECT ?person ?fof
  WHERE {
    ?person foaf:knows/foaf:knows ?fof .
    FILTER (?person != ?fof)
  }
`);

// Any path length (transitive closure)
const allConnected = await select(store, `
  PREFIX foaf: <http://xmlns.com/foaf/0.1/>

  SELECT ?person ?connected
  WHERE {
    ?person foaf:knows+ ?connected .
  }
`);
```

### Path Operators

| Operator | Meaning |
|----------|---------|
| `p1/p2` | Sequence (p1 then p2) |
| `p*` | Zero or more |
| `p+` | One or more |
| `p?` | Zero or one |
| `p1\|p2` | Alternative |
| `^p` | Inverse path |
| `!(p1\|p2)` | Negated property set |

## Step 11: VALUES Clause

Inline data for filtering:

```javascript
// Find specific people
const specific = await select(store, `
  PREFIX foaf: <http://xmlns.com/foaf/0.1/>
  PREFIX ex: <http://example.org/>

  SELECT ?name ?salary
  WHERE {
    VALUES ?person { ex:alice ex:bob }
    ?person foaf:name ?name ;
            ex:salary ?salary .
  }
`);
```

## Step 12: Query Optimization

### Use Specific Patterns First

```javascript
// Slow: starts with broad pattern
const slow = `
  SELECT ?name WHERE {
    ?s ?p ?o .
    ?s foaf:name ?name .
    ?s a foaf:Person .
  }
`;

// Fast: starts with specific pattern
const fast = `
  SELECT ?name WHERE {
    ?s a foaf:Person .
    ?s foaf:name ?name .
  }
`;
```

### Limit Results

```javascript
// Pagination
const page1 = await select(store, `
  PREFIX foaf: <http://xmlns.com/foaf/0.1/>

  SELECT ?name
  WHERE { ?person foaf:name ?name }
  ORDER BY ?name
  LIMIT 10
  OFFSET 0
`);
```

### Use BIND for Computed Values

```javascript
const withBonus = await select(store, `
  PREFIX ex: <http://example.org/>
  PREFIX foaf: <http://xmlns.com/foaf/0.1/>

  SELECT ?name ?salary ?bonus
  WHERE {
    ?person foaf:name ?name ;
            ex:salary ?salary .
    BIND (?salary * 0.1 AS ?bonus)
  }
`);
```

## Exercise: Analytics Dashboard

Build a complete analytics query suite:

```javascript
// exercises/analytics.mjs
import { parseTurtle, select } from 'unrdf/knowledge-engine';
import { readFileSync } from 'node:fs';

const store = await parseTurtle(readFileSync('./data/social-network.ttl', 'utf-8'));

async function getDashboardData() {
  // Total headcount
  const headcount = await select(store, `
    PREFIX foaf: <http://xmlns.com/foaf/0.1/>
    SELECT (COUNT(DISTINCT ?person) AS ?total)
    WHERE { ?person a foaf:Person }
  `);

  // Department breakdown
  const deptBreakdown = await select(store, `
    PREFIX ex: <http://example.org/>
    PREFIX foaf: <http://xmlns.com/foaf/0.1/>

    SELECT ?deptName
           (COUNT(?person) AS ?headcount)
           (SUM(?salary) AS ?totalCost)
           (AVG(?salary) AS ?avgSalary)
    WHERE {
      ?person a foaf:Person ;
              ex:department ?dept ;
              ex:salary ?salary .
      ?dept ex:name ?deptName .
    }
    GROUP BY ?dept ?deptName
    ORDER BY DESC(?headcount)
  `);

  // Network connectivity
  const connectivity = await select(store, `
    PREFIX foaf: <http://xmlns.com/foaf/0.1/>

    SELECT ?name (COUNT(?friend) AS ?connections)
    WHERE {
      ?person foaf:name ?name .
      OPTIONAL { ?person foaf:knows ?friend }
    }
    GROUP BY ?person ?name
    ORDER BY DESC(?connections)
  `);

  // Age distribution
  const ageDistribution = await select(store, `
    PREFIX foaf: <http://xmlns.com/foaf/0.1/>

    SELECT
      (MIN(?age) AS ?youngest)
      (MAX(?age) AS ?oldest)
      (AVG(?age) AS ?average)
    WHERE {
      ?person foaf:age ?age
    }
  `);

  return {
    headcount: headcount[0]?.total?.value,
    departments: deptBreakdown.map(d => ({
      name: d.deptName.value,
      headcount: parseInt(d.headcount.value),
      totalCost: parseInt(d.totalCost.value),
      avgSalary: parseFloat(d.avgSalary.value).toFixed(2)
    })),
    topConnectors: connectivity.slice(0, 3).map(c => ({
      name: c.name.value,
      connections: parseInt(c.connections.value)
    })),
    ageStats: {
      youngest: ageDistribution[0]?.youngest?.value,
      oldest: ageDistribution[0]?.oldest?.value,
      average: parseFloat(ageDistribution[0]?.average?.value).toFixed(1)
    }
  };
}

const dashboard = await getDashboardData();
console.log(JSON.stringify(dashboard, null, 2));
```

## Common Mistakes

### Mistake 1: Forgetting Prefixes

```sparql
# Wrong - undefined prefix
SELECT ?name WHERE { ?person foaf:name ?name }

# Correct
PREFIX foaf: <http://xmlns.com/foaf/0.1/>
SELECT ?name WHERE { ?person foaf:name ?name }
```

### Mistake 2: Variable Scope in OPTIONAL

```sparql
# Wrong - ?age not bound outside OPTIONAL
SELECT ?name ?age WHERE {
  ?person foaf:name ?name .
}
OPTIONAL { ?person foaf:age ?age }

# Correct
SELECT ?name ?age WHERE {
  ?person foaf:name ?name .
  OPTIONAL { ?person foaf:age ?age }
}
```

### Mistake 3: GROUP BY Missing Variables

```sparql
# Wrong - ?name not in GROUP BY
SELECT ?name (COUNT(?friend) AS ?count) WHERE {
  ?person foaf:name ?name ; foaf:knows ?friend
}
GROUP BY ?person

# Correct
SELECT ?name (COUNT(?friend) AS ?count) WHERE {
  ?person foaf:name ?name ; foaf:knows ?friend
}
GROUP BY ?person ?name
```

## Summary

You learned:

- SELECT, ASK, CONSTRUCT, DESCRIBE queries
- FILTER, OPTIONAL, UNION patterns
- Aggregations (COUNT, SUM, AVG, GROUP BY)
- Property paths for graph traversal
- Query optimization techniques

## Next Steps

- [Validation Tutorial](./validation.md) - SHACL shapes
- [Performance Guide](../guides/performance-optimization.md) - Query tuning
- [API Reference](../reference/api-reference.md) - Full query API
