# Advanced Query Patterns

**Time estimate:** 10-12 hours (if you code along)
**Difficulty:** Advanced
**Prerequisites:** Complete tutorials 01 and 02 first
**What you'll learn:** UNION queries, aggregation, grouping, pagination, and text operations

---

## What You'll Do

In this tutorial, you'll:
1. Combine multiple queries with UNION
2. Aggregate results (COUNT, SUM, MIN, MAX, AVG)
3. Group results and apply aggregates
4. Paginate large result sets
5. Use string operations for text matching
6. Order and limit results

By the end, you'll be able to write sophisticated queries that answer complex business questions.

---

## Part 1: UNION - Combining Queries

### The Problem

You want to find "everyone related to Alice" - either people she knows OR people who know her. Without UNION, you'd need two separate queries.

### The Solution

Use UNION to combine multiple patterns:

```javascript
import { createUnrdfStore, executeQuerySync } from '@unrdf/core';

const query = `
  PREFIX ex: <http://example.com/>
  PREFIX foaf: <http://xmlns.com/foaf/0.1/>

  SELECT DISTINCT ?person ?name WHERE {
    {
      ex:alice foaf:knows ?person .
    }
    UNION
    {
      ?person foaf:knows ex:alice .
    }
    ?person foaf:name ?name .
  }
`;

const results = executeQuerySync(store, query);

console.log('Everyone connected to Alice:');
results.forEach(binding => {
  console.log(`  - ${binding.name.value}`);
});
```

**Key points:**
- `UNION` combines two or more patterns
- `DISTINCT` removes duplicates (people who know Alice AND Alice knows them)
- Each branch can have different variables, but final SELECT must use common ones

### Real-World Example: Find Potential Collaborators

Find people who either:
- Work at the same company as Alice, OR
- Already know someone at Alice's company

```javascript
const query = `
  PREFIX ex: <http://example.com/>
  PREFIX foaf: <http://xmlns.com/foaf/0.1/>

  SELECT DISTINCT ?person ?name ?reason WHERE {
    ex:alice foaf:workplaceHomepage ?company .

    {
      ?person foaf:workplaceHomepage ?company .
      BIND("Same company" AS ?reason)
    }
    UNION
    {
      ?person foaf:knows ?coworker .
      ?coworker foaf:workplaceHomepage ?company .
      BIND("Knows someone at company" AS ?reason)
    }

    ?person foaf:name ?name .
    FILTER (?person != ex:alice)
  }
`;

const results = executeQuerySync(store, query);

console.log('Potential collaborators:');
results.forEach(binding => {
  console.log(`  - ${binding.name.value} (${binding.reason.value})`);
});
```

---

## Part 2: Aggregation Functions

### COUNT - Count Results

Count how many people Alice knows:

```javascript
const query = `
  PREFIX ex: <http://example.com/>
  PREFIX foaf: <http://xmlns.com/foaf/0.1/>

  SELECT (COUNT(?person) AS ?friendCount) WHERE {
    ex:alice foaf:knows ?person .
  }
`;

const results = executeQuerySync(store, query);
console.log(`Alice knows ${results[0].friendCount.value} people`);
```

### Other Aggregate Functions

```javascript
// Average age of all people
const query1 = `
  PREFIX foaf: <http://xmlns.com/foaf/0.1/>
  SELECT (AVG(?age) AS ?averageAge) WHERE {
    ?person a foaf:Person ;
            foaf:age ?age .
  }
`;

// Oldest person
const query2 = `
  PREFIX foaf: <http://xmlns.com/foaf/0.1/>
  SELECT (MAX(?age) AS ?oldestAge) WHERE {
    ?person foaf:age ?age .
  }
`;

// Youngest person
const query3 = `
  PREFIX foaf: <http://xmlns.com/foaf/0.1/>
  SELECT (MIN(?age) AS ?youngestAge) WHERE {
    ?person foaf:age ?age .
  }
`;

// Sum of all ages
const query4 = `
  PREFIX foaf: <http://xmlns.com/foaf/0.1/>
  SELECT (SUM(?age) AS ?totalAge) WHERE {
    ?person foaf:age ?age .
  }
`;
```

### GROUP_CONCAT - Collect Values

Collect all friends' names into a single string:

```javascript
const query = `
  PREFIX ex: <http://example.com/>
  PREFIX foaf: <http://xmlns.com/foaf/0.1/>

  SELECT ?person
         (GROUP_CONCAT(?friendName; separator=", ") AS ?friends) WHERE {
    ex:alice foaf:knows ?friend .
    ?friend foaf:name ?friendName .
    BIND(ex:alice AS ?person)
  }
  GROUP BY ?person
`;

const results = executeQuerySync(store, query);
console.log(`Alice's friends: ${results[0].friends.value}`);
// Output: Alice's friends: Bob Smith, Charlie Brown, Diana Prince
```

---

## Part 3: GROUP BY and Aggregates Together

### The Pattern

GROUP BY lets you compute aggregates for different groups:

```javascript
const query = `
  PREFIX foaf: <http://xmlns.com/foaf/0.1/>

  SELECT ?company ?count ?avgAge WHERE {
    ?person foaf:workplaceHomepage ?company ;
            foaf:age ?age .
  }
  GROUP BY ?company
  HAVING (COUNT(?person) > 1)
`;

const results = executeQuerySync(store, query);

console.log('Companies with multiple employees:');
results.forEach(binding => {
  console.log(`  - ${binding.company.value}`);
  console.log(`    Employees: ${binding.count.value}`);
  console.log(`    Average age: ${binding.avgAge.value}`);
});
```

### Real-World Example: Find Important People

Find people who know many others and are age 25-35:

```javascript
const query = `
  PREFIX foaf: <http://xmlns.com/foaf/0.1/>

  SELECT ?person ?name (COUNT(?knows) AS ?connectionCount) WHERE {
    ?person foaf:name ?name ;
            foaf:age ?age ;
            foaf:knows ?knows .
    FILTER (?age >= 25 && ?age <= 35)
  }
  GROUP BY ?person ?name
  HAVING (COUNT(?knows) > 2)
  ORDER BY DESC(?connectionCount)
`;

const results = executeQuerySync(store, query);

console.log('Most connected mid-career professionals:');
results.forEach(binding => {
  console.log(`  - ${binding.name.value} (${binding.connectionCount.value} connections)`);
});
```

---

## Part 4: Pagination with LIMIT and OFFSET

### The Problem

You have thousands of people. You can't return all at once. You need pagination.

### The Solution

Use LIMIT and OFFSET:

```javascript
const pageSize = 10;
let page = 0;

function getPage(pageNum) {
  const offset = pageNum * pageSize;

  const query = `
    PREFIX foaf: <http://xmlns.com/foaf/0.1/>

    SELECT ?name WHERE {
      ?person a foaf:Person ;
              foaf:name ?name .
    }
    ORDER BY ?name
    LIMIT ${pageSize}
    OFFSET ${offset}
  `;

  return executeQuerySync(store, query);
}

// Get page 0 (first 10 results)
const page0 = getPage(0);
console.log('Page 1:');
page0.forEach(binding => console.log(`  - ${binding.name.value}`));

// Get page 1 (next 10 results)
const page1 = getPage(1);
console.log('\nPage 2:');
page1.forEach(binding => console.log(`  - ${binding.name.value}`));
```

### Practical Implementation

```javascript
class PersonBrowser {
  constructor(store, pageSize = 10) {
    this.store = store;
    this.pageSize = pageSize;
  }

  async getPage(pageNum) {
    const offset = pageNum * this.pageSize;

    const query = `
      PREFIX foaf: <http://xmlns.com/foaf/0.1/>

      SELECT ?person ?name ?email WHERE {
        ?person a foaf:Person ;
                foaf:name ?name .
        OPTIONAL { ?person foaf:email ?email . }
      }
      ORDER BY ?name
      LIMIT ${this.pageSize}
      OFFSET ${offset}
    `;

    return executeQuerySync(this.store, query);
  }

  async getTotalCount() {
    const query = `
      PREFIX foaf: <http://xmlns.com/foaf/0.1/>
      SELECT (COUNT(?person) AS ?count) WHERE {
        ?person a foaf:Person .
      }
    `;

    const result = executeQuerySync(this.store, query);
    return parseInt(result[0].count.value);
  }
}

// Usage
const browser = new PersonBrowser(store);
const total = await browser.getTotalCount();
const firstPage = await browser.getPage(0);
const pageCount = Math.ceil(total / 10);

console.log(`Total people: ${total}`);
console.log(`Page 1 of ${pageCount}:`);
firstPage.forEach(p => console.log(`  - ${p.name.value}`));
```

---

## Part 5: String Operations

### CONTAINS - Text Search

Find all people whose name contains "John":

```javascript
const query = `
  PREFIX foaf: <http://xmlns.com/foaf/0.1/>

  SELECT ?name WHERE {
    ?person foaf:name ?name .
    FILTER (CONTAINS(?name, "John"))
  }
`;

const results = executeQuerySync(store, query);
console.log('People with "John" in name:');
results.forEach(binding => console.log(`  - ${binding.name.value}`));
```

### STARTS WITH and ENDS WITH

```javascript
// Names starting with "Alice"
const query1 = `
  PREFIX foaf: <http://xmlns.com/foaf/0.1/>
  SELECT ?name WHERE {
    ?person foaf:name ?name .
    FILTER (STRSTARTS(?name, "Alice"))
  }
`;

// Emails ending with "example.com"
const query2 = `
  PREFIX foaf: <http://xmlns.com/foaf/0.1/>
  SELECT ?email WHERE {
    ?person foaf:email ?email .
    FILTER (STRENDS(?email, "example.com"))
  }
`;
```

### STRLEN - String Length

Find people with short names:

```javascript
const query = `
  PREFIX foaf: <http://xmlns.com/foaf/0.1/>

  SELECT ?name WHERE {
    ?person foaf:name ?name .
    FILTER (STRLEN(?name) < 10)
  }
`;

const results = executeQuerySync(store, query);
console.log('People with short names:');
results.forEach(binding => console.log(`  - ${binding.name.value}`));
```

### REGEX - Regular Expressions

Find emails at specific domains:

```javascript
const query = `
  PREFIX foaf: <http://xmlns.com/foaf/0.1/>

  SELECT ?email WHERE {
    ?person foaf:email ?email .
    FILTER (REGEX(?email, "@.*\\.com$"))
  }
`;

const results = executeQuerySync(store, query);
console.log('People with .com emails:');
results.forEach(binding => console.log(`  - ${binding.email.value}`));
```

---

## Part 6: Ordering Results

### ORDER BY - Sort Results

```javascript
// Sort by name ascending (default)
const query1 = `
  PREFIX foaf: <http://xmlns.com/foaf/0.1/>
  SELECT ?name WHERE {
    ?person foaf:name ?name .
  }
  ORDER BY ?name
`;

// Sort by age descending
const query2 = `
  PREFIX foaf: <http://xmlns.com/foaf/0.1/>
  SELECT ?name ?age WHERE {
    ?person foaf:name ?name ;
            foaf:age ?age .
  }
  ORDER BY DESC(?age)
`;

// Sort by multiple columns
const query3 = `
  PREFIX foaf: <http://xmlns.com/foaf/0.1/>
  SELECT ?company ?name WHERE {
    ?person foaf:workplaceHomepage ?company ;
            foaf:name ?name .
  }
  ORDER BY ?company ?name
`;
```

---

## Part 7: Complex Real-World Query

Find the top 5 influencers (people who know many others) in each company:

```javascript
const query = `
  PREFIX foaf: <http://xmlns.com/foaf/0.1/>

  SELECT ?company ?name (COUNT(?knows) AS ?influence) WHERE {
    ?person foaf:name ?name ;
            foaf:workplaceHomepage ?company ;
            foaf:knows ?knows .
  }
  GROUP BY ?company ?person ?name
  HAVING (COUNT(?knows) > 2)
  ORDER BY ?company DESC(?influence)
  LIMIT 50  /* Get top 5 per company (approximately) */
`;

const results = executeQuerySync(store, query);

let currentCompany = null;
let count = 0;

results.forEach(binding => {
  if (currentCompany !== binding.company.value) {
    currentCompany = binding.company.value;
    count = 0;
    console.log(`\n=== ${binding.company.value} ===`);
  }

  if (count < 5) {
    console.log(
      `${count + 1}. ${binding.name.value} (${binding.influence.value} connections)`
    );
    count++;
  }
});
```

---

## Summary

You've learned:
- ✅ UNION for combining multiple query branches
- ✅ Aggregate functions (COUNT, AVG, MIN, MAX, SUM)
- ✅ GROUP BY for grouping results
- ✅ GROUP_CONCAT for collecting values
- ✅ LIMIT and OFFSET for pagination
- ✅ String operations (CONTAINS, STARTS WITH, REGEX)
- ✅ ORDER BY for sorting
- ✅ Complex real-world queries combining everything

### Advanced Patterns Reference

```sparql
/* Pattern 1: Union with distinct */
{ ?x knows ?y } UNION { ?y knows ?x }

/* Pattern 2: Group by with aggregates */
SELECT ?company (COUNT(?person) AS ?count)
WHERE { ?person worksAt ?company }
GROUP BY ?company

/* Pattern 3: Pagination */
ORDER BY ?name
LIMIT 10 OFFSET 20

/* Pattern 4: Full-text search */
FILTER (REGEX(?name, "^John", "i"))

/* Pattern 5: Aggregate with having */
HAVING (COUNT(?x) > 5)
```

### Next Steps

- Read **optimize-sparql-queries** (How-To) to speed up complex queries
- Read **query-execution** (Explanation) for deep understanding of how queries run
- Read **sparql-concepts** (Explanation) for SPARQL theory

---

## Troubleshooting

**Q: My GROUP BY query returns duplicates**
A: Make sure all non-aggregated variables in SELECT are in GROUP BY:
```sparql
SELECT ?company ?person (COUNT(?x) AS ?count)
GROUP BY ?company ?person  /* Both must be in GROUP BY */
```

**Q: UNION is returning the same result twice**
A: Use DISTINCT:
```sparql
SELECT DISTINCT ?result WHERE { ... UNION ... }
```

**Q: My REGEX query is too slow**
A: REGEX is slow on large datasets. Use CONTAINS or STRSTARTS if possible:
```sparql
FILTER (STRSTARTS(?name, "John"))  /* Fast */
FILTER (REGEX(?name, "^John"))     /* Slow */
```

**Q: I want to return only the first result from a GROUP**
A: SPARQL doesn't have a built-in way. You'll need application logic.

---

**Congratulations!** You've completed all three core tutorials. You now understand:
- Basic RDF and SPARQL concepts
- Real-world query patterns
- Advanced aggregation and grouping
- String operations and filtering

You're ready to move on to the How-To guides and deep-dive explanations.
