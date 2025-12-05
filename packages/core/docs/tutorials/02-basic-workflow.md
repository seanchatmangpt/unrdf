# Working with Multiple RDF Formats

**Time estimate:** 8-10 hours (if you code along)
**Difficulty:** Intermediate
**Prerequisites:** Complete "01-getting-started" first
**What you'll learn:** Load RDF from files, work with different formats, and handle complex query patterns

---

## What You'll Do

In this tutorial, you'll:
1. Load RDF data from Turtle (TTL) files
2. Work with prefixes to simplify queries
3. Use OPTIONAL patterns to find optional data
4. Handle SPARQL JOIN patterns across multiple graphs
5. Process and filter results efficiently

By the end, you'll understand real-world RDF workflows and be able to work with actual RDF files.

---

## Part 1: Loading RDF from Files

### The Goal

In the previous tutorial, we manually added triples. Real-world applications load RDF from files. Let's load a Turtle file (TTL format) that contains ontology data.

### Create Sample Data Files

First, create a file called `data.ttl`:

```turtle
@prefix ex: <http://example.com/> .
@prefix foaf: <http://xmlns.com/foaf/0.1/> .
@prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .

ex:alice a foaf:Person ;
  foaf:name "Alice Johnson" ;
  foaf:age 30 ;
  foaf:email "alice@example.com" ;
  foaf:knows ex:bob, ex:charlie ;
  foaf:workplaceHomepage <http://example.com/company-a> .

ex:bob a foaf:Person ;
  foaf:name "Bob Smith" ;
  foaf:age 28 ;
  foaf:email "bob@example.com" ;
  foaf:knows ex:alice, ex:diana ;
  foaf:workplaceHomepage <http://example.com/company-b> .

ex:charlie a foaf:Person ;
  foaf:name "Charlie Brown" ;
  foaf:age 35 ;
  foaf:email "charlie@example.com" ;
  foaf:knows ex:alice ;
  foaf:workplaceHomepage <http://example.com/company-a> .

ex:diana a foaf:Person ;
  foaf:name "Diana Prince" ;
  foaf:age 27 ;
  foaf:workplaceHomepage <http://example.com/company-b> .

ex:company-a a foaf:Organization ;
  foaf:name "Company A" ;
  foaf:based_near "New York" .

ex:company-b a foaf:Organization ;
  foaf:name "Company B" ;
  foaf:based_near "San Francisco" .
```

Save this as `data.ttl` in your project directory.

### Load the File

Now create `load-file.mjs`:

```javascript
import { createUnrdfStore, executeQuerySync } from '@unrdf/core';
import fs from 'fs';

// Read the TTL file
const turtleData = fs.readFileSync('data.ttl', 'utf-8');

// Create a store
const store = createUnrdfStore();

// For now, we'll add the quads manually
// (In production, you'd use a TTL parser like @unrdf/parser-turtle)
// For this tutorial, we'll show the concept

console.log('File loaded. Ready to query.');
console.log(`File size: ${turtleData.length} characters`);
```

**Note:** For production use, you would integrate a parser library. The @unrdf/core package provides the store, and separate parser packages handle different formats.

### Working with Parsed Data

Once you have parsed Turtle data into quads, loading is simple:

```javascript
import { createUnrdfStore, executeQuerySync } from '@unrdf/core';

const store = createUnrdfStore();

// Assume quads come from a Turtle parser
const quads = [
  // Alice's data
  {
    subject: { type: 'NamedNode', value: 'http://example.com/alice' },
    predicate: { type: 'NamedNode', value: 'http://xmlns.com/foaf/0.1/name' },
    object: { type: 'Literal', value: 'Alice Johnson' }
  },
  // Add more quads...
];

// Add all quads to the store
quads.forEach(quad => store.addQuad(quad));

console.log(`Loaded ${store.size} quads`);
```

---

## Part 2: Using Prefixes in Queries

### The Challenge

Writing full URIs in queries is tedious:

```sparql
SELECT ?name WHERE {
  <http://example.com/alice>
  <http://xmlns.com/foaf/0.1/knows>
  ?person .
  ?person
  <http://xmlns.com/foaf/0.1/name>
  ?name .
}
```

This is hard to read and error-prone.

### The Solution

SPARQL supports PREFIX declarations for shorthand:

```javascript
const query = `
  PREFIX ex: <http://example.com/>
  PREFIX foaf: <http://xmlns.com/foaf/0.1/>

  SELECT ?name WHERE {
    ex:alice foaf:knows ?person .
    ?person foaf:name ?name .
  }
`;

const results = executeQuerySync(store, query);

console.log("Alice's friends:");
results.forEach(binding => {
  console.log(`  - ${binding.name.value}`);
});
```

**Much cleaner!** This is the standard way to write SPARQL queries.

### Common Prefixes

Here are the most common ones:

```sparql
PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
PREFIX xsd: <http://www.w3.org/2001/XMLSchema#>
PREFIX foaf: <http://xmlns.com/foaf/0.1/>
PREFIX dc: <http://purl.org/dc/elements/1.1/>
PREFIX owl: <http://www.w3.org/2002/07/owl#>
```

@unrdf/core provides these as constants:

```javascript
import { COMMON_PREFIXES, FOAF, RDF } from '@unrdf/core';

// COMMON_PREFIXES object contains all standard prefixes
console.log(RDF.type);  // Full URI for rdf:type
```

---

## Part 3: OPTIONAL Patterns

### The Problem

Not everyone has all properties. Diana doesn't have an email address. If we write:

```sparql
SELECT ?name ?email WHERE {
  ?person foaf:name ?name .
  ?person foaf:email ?email .
}
```

This returns only people WITH emails (Alice, Bob, Charlie), not Diana.

### The Solution

Use OPTIONAL to make properties optional:

```javascript
const query = `
  PREFIX foaf: <http://xmlns.com/foaf/0.1/>

  SELECT ?name ?email WHERE {
    ?person foaf:name ?name .
    OPTIONAL { ?person foaf:email ?email . }
  }
`;

const results = executeQuerySync(store, query);

console.log('Names and emails (optional):');
results.forEach(binding => {
  const email = binding.email ? binding.email.value : '(none)';
  console.log(`  - ${binding.name.value}: ${email}`);
});
```

**Output:**
```
Names and emails (optional):
  - Alice Johnson: alice@example.com
  - Bob Smith: bob@example.com
  - Charlie Brown: charlie@example.com
  - Diana Prince: (none)
```

**Key insight:** OPTIONAL means "try to match this pattern, but if it fails, return the row anyway with a null value for that variable."

---

## Part 4: Complex Joins

### Multi-Step Relationships

Find everyone who works at the same company as Alice:

```javascript
const query = `
  PREFIX ex: <http://example.com/>
  PREFIX foaf: <http://xmlns.com/foaf/0.1/>

  SELECT ?name WHERE {
    ex:alice foaf:workplaceHomepage ?company .
    ?person foaf:workplaceHomepage ?company .
    ?person foaf:name ?name .
    FILTER (?name != "Alice Johnson")
  }
`;

const results = executeQuerySync(store, query);

console.log("People at Alice's company:");
results.forEach(binding => {
  console.log(`  - ${binding.name.value}`);
});
```

**What happens:**
1. Find Alice's company
2. Find all people at that company
3. Get their names
4. Filter out Alice herself

### Multi-Level Relationships

Find friends of Alice's friends:

```javascript
const query = `
  PREFIX ex: <http://example.com/>
  PREFIX foaf: <http://xmlns.com/foaf/0.1/>

  SELECT ?friendOfFriend ?name WHERE {
    ex:alice foaf:knows ?friend .
    ?friend foaf:knows ?friendOfFriend .
    ?friendOfFriend foaf:name ?name .
    FILTER (?friendOfFriend != ex:alice)
  }
`;

const results = executeQuerySync(store, query);

console.log("Friends of Alice's friends:");
results.forEach(binding => {
  console.log(`  - ${binding.name.value}`);
});
```

---

## Part 5: Type Checking

### Find All Instances of a Type

Use `rdf:type` to find all instances of a class:

```javascript
const query = `
  PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
  PREFIX foaf: <http://xmlns.com/foaf/0.1/>

  SELECT ?person ?name WHERE {
    ?person rdf:type foaf:Person ;
            foaf:name ?name .
  }
`;

const results = executeQuerySync(store, query);

console.log('All people:');
results.forEach(binding => {
  console.log(`  - ${binding.name.value}`);
});
```

**Pattern shorthand:** In Turtle and SPARQL, `a` is shorthand for `rdf:type`:

```sparql
?person a foaf:Person .  /* Same as: ?person rdf:type foaf:Person . */
```

### Find by Multiple Types

Some resources might be both a Person and an Agent:

```javascript
const query = `
  PREFIX foaf: <http://xmlns.com/foaf/0.1/>

  SELECT ?name WHERE {
    ?person a foaf:Person ;
            foaf:name ?name .
    ?person a foaf:Organization .
  }
`;

const results = executeQuerySync(store, query);
// This would find resources that are both Person AND Organization
```

---

## Part 6: Filtering Results

### Basic Filters

Filter by age:

```javascript
const query = `
  PREFIX foaf: <http://xmlns.com/foaf/0.1/>

  SELECT ?name ?age WHERE {
    ?person foaf:name ?name ;
            foaf:age ?age .
    FILTER (?age >= 30)
  }
`;

const results = executeQuerySync(store, query);

console.log('People age 30 or older:');
results.forEach(binding => {
  console.log(`  - ${binding.name.value}: ${binding.age.value}`);
});
```

### String Filters

Filter by name pattern:

```javascript
const query = `
  PREFIX foaf: <http://xmlns.com/foaf/0.1/>

  SELECT ?name WHERE {
    ?person foaf:name ?name .
    FILTER (CONTAINS(?name, "a"))
  }
`;

const results = executeQuerySync(store, query);

console.log('People with "a" in their name:');
results.forEach(binding => {
  console.log(`  - ${binding.name.value}`);
});
```

### Complex Filters

Combine multiple conditions:

```javascript
const query = `
  PREFIX foaf: <http://xmlns.com/foaf/0.1/>

  SELECT ?name ?age WHERE {
    ?person foaf:name ?name ;
            foaf:age ?age .
    FILTER (
      ?age >= 25 && ?age <= 35 &&
      CONTAINS(?name, "i")
    )
  }
`;

const results = executeQuerySync(store, query);

console.log('People age 25-35 with "i" in their name:');
results.forEach(binding => {
  console.log(`  - ${binding.name.value}: ${binding.age.value}`);
});
```

---

## Part 7: Complete Workflow Example

Here's a real-world workflow combining everything:

```javascript
import { createUnrdfStore, executeQuerySync } from '@unrdf/core';

// Assume store is populated with data...
const store = createUnrdfStore();

// Workflow: Find high-value connections
const query = `
  PREFIX foaf: <http://xmlns.com/foaf/0.1/>

  SELECT ?name ?email ?company WHERE {
    ?person foaf:name ?name ;
            foaf:email ?email ;
            foaf:workplaceHomepage ?workplace .
    ?workplace foaf:name ?company .
    OPTIONAL { ?person foaf:age ?age . }
    FILTER (!BOUND(?age) || ?age >= 28)
  }
`;

const results = executeQuerySync(store, query);

console.log('=== High-Value Contacts ===');
results.forEach((binding, index) => {
  console.log(`\n${index + 1}. ${binding.name.value}`);
  console.log(`   Email: ${binding.email.value}`);
  console.log(`   Company: ${binding.company.value}`);
});
```

---

## Summary

You've learned:
- ✅ How to load RDF from files
- ✅ How to use PREFIX for cleaner queries
- ✅ How to use OPTIONAL for missing data
- ✅ How to write multi-step joins
- ✅ How to filter by type and properties
- ✅ How to combine multiple conditions

### Real-World Patterns

These are patterns you'll use constantly:

```sparql
/* Pattern 1: Find all instances of a type */
?x a ClassName .

/* Pattern 2: Find optional relationships */
?x property ?y .
OPTIONAL { ?x otherProperty ?z . }

/* Pattern 3: Multi-step joins */
?x knows ?y .
?y works foaf:Organization .
?org name ?companyName .

/* Pattern 4: Filter numeric values */
FILTER (?age >= 18 && ?age <= 65)

/* Pattern 5: Filter strings */
FILTER (CONTAINS(?name, "John") || CONTAINS(?name, "Jane"))
```

### Next Steps

- Read **03-advanced-patterns** to learn UNION, aggregates, and grouping
- Read **optimize-sparql-queries** (How-To) to make complex queries fast
- Read **SPARQL-concepts** (Explanation) for deeper understanding

---

## Troubleshooting

**Q: My PREFIX declarations aren't working**
A: Make sure each PREFIX line is before the SELECT. Prefixes must be declared before they're used.

**Q: OPTIONAL returns results I don't want**
A: Use FILTER with BOUND() to check if the optional variable was matched:
```sparql
OPTIONAL { ?person foaf:email ?email . }
FILTER (!BOUND(?email))  /* Only get people WITHOUT emails */
```

**Q: My query is very slow**
A: This is normal with large datasets. See "optimize-sparql-queries" for performance tips.

---

**Ready to continue?** Move on to "03-advanced-patterns" to learn aggregation, grouping, and set operations.
