# How To: Optimize SPARQL Queries

**Time estimate:** 6-8 hours to fully understand
**Difficulty:** Intermediate to Advanced
**Context:** You have queries that work, but run slowly

---

## Overview

This guide shows practical techniques to make SPARQL queries run faster. Optimization is about:
1. Understanding query cost
2. Identifying bottlenecks
3. Applying targeted optimizations
4. Measuring improvements

We'll show common patterns that cause slowness and how to fix them.

---

## Problem 1: Expensive Patterns First

### The Issue

```sparql
SELECT ?person ?friend ?friendName WHERE {
  ?person foaf:name ?name .
  ?person foaf:knows ?friend .
  ?friend foaf:name ?friendName .
}
```

If you have 1 million people, this query might:
1. Find all people with names (1M matches)
2. For each, find everyone they know (potentially billions of relationships)
3. For each friend, get their name

This is multiplicative work.

### The Fix: Start with Selective Patterns

Reorder to match most-selective patterns first:

```sparql
SELECT ?person ?friend ?friendName WHERE {
  ?friend foaf:name ?friendName .
  ?person foaf:knows ?friend .
  ?person foaf:name ?name .
}
```

Better:

```sparql
PREFIX ex: <http://example.com/>

SELECT ?person ?friend ?friendName WHERE {
  ex:alice foaf:knows ?friend .
  ?friend foaf:name ?friendName .
}
```

**Rule of thumb:** Start with the most specific/restrictive patterns first. Patterns that match fewer results narrow the search space for subsequent patterns.

### Comparison

```javascript
// ❌ SLOW: Matches all people first
const slow = `
  PREFIX foaf: <http://xmlns.com/foaf/0.1/>
  SELECT ?name WHERE {
    ?person foaf:name ?name .
    ?person foaf:age ?age .
    FILTER (?age > 30)
  }
`;

// ✅ FAST: Filter first, then get properties
const fast = `
  PREFIX foaf: <http://xmlns.com/foaf/0.1/>
  SELECT ?name WHERE {
    ?person foaf:age ?age .
    FILTER (?age > 30) .
    ?person foaf:name ?name .
  }
`;
```

---

## Problem 2: Overly Broad Joins

### The Issue

```sparql
SELECT ?personName ?companyName WHERE {
  ?person foaf:name ?personName .
  ?company foaf:name ?companyName .
  ?person workAt ?company .
}
```

This pattern can match millions of person-company combinations before the JOIN.

### The Fix: Constrain Early

Add a filter to reduce the search space:

```sparql
PREFIX ex: <http://example.com/>

SELECT ?personName ?companyName WHERE {
  ?person foaf:name ?personName .
  ?person workAt ?company .
  ?company foaf:name ?companyName .
  FILTER (CONTAINS(?personName, "John"))
}
```

Even better - constrain one side:

```sparql
PREFIX ex: <http://example.com/>

SELECT ?personName ?companyName WHERE {
  ex:company-a foaf:name ?companyName .
  ?person workAt ex:company-a .
  ?person foaf:name ?personName .
}
```

---

## Problem 3: Unnecessary OPTIONAL

### The Issue

```sparql
SELECT ?name ?email ?phone ?address WHERE {
  ?person foaf:name ?name .
  OPTIONAL { ?person foaf:email ?email . }
  OPTIONAL { ?person foaf:phone ?phone . }
  OPTIONAL { ?person foaf:address ?address . }
}
```

Each OPTIONAL adds a separate pattern match. If you have many optionals, the query engine has to explore many branches.

### The Fix 1: Only Use When Necessary

```sparql
/* Remove unnecessary OPTIONAL */
SELECT ?name ?email WHERE {
  ?person foaf:name ?name .
  OPTIONAL { ?person foaf:email ?email . }
  FILTER (BOUND(?email))  /* Only people WITH emails */
}
```

### The Fix 2: Use UNION Instead

For complex optionals, UNION might be more efficient:

```sparql
PREFIX foaf: <http://xmlns.com/foaf/0.1/>

SELECT ?name ?contact WHERE {
  ?person foaf:name ?name .
  {
    ?person foaf:email ?contact .
  }
  UNION
  {
    ?person foaf:phone ?contact .
  }
}
```

### The Fix 3: Pre-compute Optional Data

If optional data is rarely used, fetch it separately:

```javascript
// First query: get all people
const people = executeQuerySync(store, `
  SELECT ?person ?name WHERE {
    ?person a foaf:Person ;
            foaf:name ?name .
  }
`);

// Second query: get email addresses
const emails = executeQuerySync(store, `
  SELECT ?person ?email WHERE {
    ?person foaf:email ?email .
  }
`);

// Combine in JavaScript
const result = people.map(p => {
  const email = emails.find(e => e.person === p.person);
  return {
    name: p.name.value,
    email: email ? email.email.value : null
  };
});
```

---

## Problem 4: Expensive Filters

### The Issue

```sparql
SELECT ?name WHERE {
  ?person foaf:name ?name .
  FILTER (REGEX(?name, "^[A-Z][a-z]+ [A-Z][a-z]+$"))
}
```

This runs the regex on every person's name. With 1 million people, that's 1 million regex evaluations.

### The Fix 1: Filter After Narrowing

```sparql
SELECT ?name WHERE {
  ?person foaf:name ?name ;
          foaf:age ?age .
  FILTER (?age > 30)  /* Numeric filter is fast */
  FILTER (CONTAINS(?name, " "))  /* Cheaper than REGEX */
}
```

### The Fix 2: Use STRSTARTS/CONTAINS Instead of REGEX

```sparql
/* ❌ SLOW: Regex on every value */
FILTER (REGEX(?name, "^John"))

/* ✅ FAST: Optimized string match */
FILTER (STRSTARTS(?name, "John"))
```

### The Fix 3: Compute Filters in Application Code

For complex logic, pre-filter in JavaScript:

```javascript
const results = executeQuerySync(store, `
  SELECT ?person ?name WHERE {
    ?person foaf:name ?name .
  }
`);

// Complex filter in JavaScript (faster for few results)
const filtered = results.filter(binding => {
  const name = binding.name.value;
  const hasFirstName = name.includes(' ');
  const validFormat = /^[A-Z][a-z]+ [A-Z][a-z]+$/.test(name);
  return hasFirstName && validFormat;
});
```

---

## Problem 5: Multiple Aggregates on Same Data

### The Issue

```sparql
SELECT (COUNT(?person) AS ?count)
       (AVG(?age) AS ?avgAge)
       (MAX(?age) AS ?maxAge)
       (MIN(?age) AS ?minAge) WHERE {
  ?person foaf:age ?age .
}
```

This scans the data 4 times (once for each aggregate).

### The Fix: Combine Into One Query

```sparql
SELECT (COUNT(?person) AS ?count)
       (AVG(?age) AS ?avgAge)
       (MAX(?age) AS ?maxAge)
       (MIN(?age) AS ?minAge)
       (SUM(?age) AS ?sumAge) WHERE {
  ?person foaf:age ?age .
}
```

(Modern SPARQL engines optimize this automatically, but it's clearer)

### For JavaScript-Level Aggregates

```javascript
const results = executeQuerySync(store, `
  SELECT ?person ?age WHERE {
    ?person foaf:age ?age .
  }
`);

// Compute all aggregates in one pass
const stats = results.reduce(
  (acc, binding) => {
    const age = parseInt(binding.age.value);
    acc.count++;
    acc.sum += age;
    acc.max = Math.max(acc.max, age);
    acc.min = Math.min(acc.min, age);
    return acc;
  },
  { count: 0, sum: 0, max: -Infinity, min: Infinity }
);

console.log(`Count: ${stats.count}`);
console.log(`Average: ${stats.sum / stats.count}`);
console.log(`Min: ${stats.min}, Max: ${stats.max}`);
```

---

## Problem 6: Missing LIMIT on Large Result Sets

### The Issue

```sparql
SELECT ?person ?name WHERE {
  ?person foaf:name ?name .
}
```

This returns ALL people (millions). Your application only displays 10 per page, but you wait for all results.

### The Fix: Add LIMIT

```sparql
SELECT ?person ?name WHERE {
  ?person foaf:name ?name .
}
ORDER BY ?name
LIMIT 10
OFFSET 0  /* First page */
```

### For Pagination

```javascript
function getPage(pageNum, pageSize = 10) {
  const offset = pageNum * pageSize;
  return executeQuerySync(store, `
    SELECT ?person ?name WHERE {
      ?person foaf:name ?name .
    }
    ORDER BY ?name
    LIMIT ${pageSize}
    OFFSET ${offset}
  `);
}
```

---

## Problem 7: Redundant Joins

### The Issue

```sparql
SELECT ?name WHERE {
  ?person foaf:name ?name .
  ?person foaf:knows ?friend .
  ?friend foaf:knows ?person .  /* Redundant: proves relationship both ways */
}
```

### The Fix: Remove Redundancy

```sparql
SELECT ?name WHERE {
  ?person foaf:name ?name .
  ?person foaf:knows ?friend .
}
```

If you actually need bidirectional proof:

```sparql
SELECT ?name WHERE {
  {
    ?person foaf:knows ?friend .
    ?friend foaf:knows ?person .
  }
  ?person foaf:name ?name .
}
```

---

## Optimization Checklist

Use this checklist before assuming a query is slow:

- [ ] **Measure first** - Is it actually slow? Use timing tools
- [ ] **Order patterns** - Most selective first
- [ ] **Add filters early** - Reduce working set
- [ ] **Use LIMIT** - Don't fetch what you don't need
- [ ] **Avoid REGEX** - Use STRSTARTS, CONTAINS
- [ ] **Reduce OPTIONAL** - Only use when necessary
- [ ] **Combine aggregates** - One pass instead of many
- [ ] **Index properties** - See reference docs

---

## Real-World Examples

### Example 1: Find Active Users

```javascript
// ❌ SLOW: Matches all users, then filters
const slow = `
  PREFIX foaf: <http://xmlns.com/foaf/0.1/>
  SELECT ?name WHERE {
    ?user foaf:name ?name ;
          foaf:lastLogin ?date ;
          foaf:loginCount ?count .
    FILTER (?date > "2024-01-01"^^xsd:dateTime && ?count > 100)
  }
`;

// ✅ FAST: Add constraints before properties
const fast = `
  PREFIX foaf: <http://xmlns.com/foaf/0.1/>
  PREFIX xsd: <http://www.w3.org/2001/XMLSchema#>
  SELECT ?name WHERE {
    ?user foaf:loginCount ?count .
    FILTER (?count > 100)
    ?user foaf:lastLogin ?date .
    FILTER (?date > "2024-01-01"^^xsd:dateTime)
    ?user foaf:name ?name .
  }
  LIMIT 100
`;
```

### Example 2: Network Analysis

```javascript
// ❌ SLOW: Cartesian product of all relationships
const slow = `
  PREFIX foaf: <http://xmlns.com/foaf/0.1/>
  SELECT ?person ?friend ?friend2 WHERE {
    ?person foaf:name ?pName .
    ?person foaf:knows ?friend .
    ?friend foaf:name ?fName .
    ?friend foaf:knows ?friend2 .
    ?friend2 foaf:name ?f2Name .
  }
`;

// ✅ FAST: Constrain the root person
const fast = `
  PREFIX ex: <http://example.com/>
  PREFIX foaf: <http://xmlns.com/foaf/0.1/>
  SELECT ?friend ?friend2 ?fname ?f2name WHERE {
    ex:alice foaf:knows ?friend .
    ?friend foaf:name ?fname .
    ?friend foaf:knows ?friend2 .
    ?friend2 foaf:name ?f2name .
  }
`;
```

---

## Measuring Performance

### Simple Timing

```javascript
const start = Date.now();
const results = executeQuerySync(store, query);
const time = Date.now() - start;

console.log(`Query executed in ${time}ms`);
console.log(`Returned ${results.length} results`);
console.log(`${(results.length / time * 1000).toFixed(0)} results/sec`);
```

### Comparing Optimizations

```javascript
function benchmark(name, query, store, iterations = 5) {
  const times = [];
  for (let i = 0; i < iterations; i++) {
    const start = Date.now();
    executeQuerySync(store, query);
    times.push(Date.now() - start);
  }

  const avg = times.reduce((a, b) => a + b) / times.length;
  const min = Math.min(...times);
  const max = Math.max(...times);

  console.log(`${name}:`);
  console.log(`  Average: ${avg.toFixed(2)}ms`);
  console.log(`  Range: ${min}ms - ${max}ms`);
}

const slowQuery = `...`;
const fastQuery = `...`;

benchmark('Slow version', slowQuery, store);
benchmark('Optimized version', fastQuery, store);
```

---

## When NOT to Optimize

- Query is already fast (<100ms)
- Optimization makes query unreadable
- Bottleneck is somewhere else (network, storage)
- Optimization is premature (only optimize when needed)

---

## Summary

Key optimization techniques:
1. **Order patterns by selectivity** - Most restrictive first
2. **Add filters early** - Reduce the working set
3. **Use LIMIT** - Fetch only what you need
4. **Avoid expensive patterns** - REGEX, OPTIONAL, multiple aggregates
5. **Measure first** - Don't optimize blindly

Most queries are naturally efficient. Optimization is for the slow ones.

---

## Next Reading

- **query-execution** (Explanation) - Understand how queries actually execute
- **working-with-formats** (How-To) - Work with Turtle, N-Triples, etc.
- **troubleshooting** (How-To) - Debug queries that don't work
