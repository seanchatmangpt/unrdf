# Your First SPARQL Query

**Time estimate:** 6-8 hours (if you code along)
**Difficulty:** Beginner
**What you'll learn:** Load RDF data and execute a SPARQL query to retrieve information

---

## What You'll Do

In this tutorial, you'll:
1. Create an RDF store and load sample data
2. Write and execute a simple SPARQL SELECT query
3. Process and display the results
4. Understand the basics of RDF relationships
5. Modify the query to ask different questions

By the end, you'll have a working program that queries RDF data and you'll understand how SPARQL finds information in RDF graphs.

---

## Part 1: Setting Up Your First Store

### The Goal

You want to load some RDF data into a store so you can query it. Think of RDF as a database of facts where each fact is a triple: **subject, predicate, object**.

### What You'll Need

First, make sure you have Node.js installed (version 18.19.0 or higher). Then create a new directory and install @unrdf/core:

```bash
mkdir my-rdf-app
cd my-rdf-app
npm init -y
npm install @unrdf/core
```

Create a file called `query.mjs`:

```javascript
import { createUnrdfStore, namedNode, literal } from '@unrdf/core';

// Step 1: Create a store
const store = createUnrdfStore();

console.log('Store created!');
```

Run it:

```bash
node query.mjs
```

You should see `Store created!` printed to your terminal.

**What just happened:**
- `createUnrdfStore()` created an in-memory RDF store
- The store is empty, but ready to accept data
- We'll add data in the next step

---

## Part 2: Adding RDF Data

### The Concept

RDF data is organized as **quads**: four parts that describe a relationship.

- **Subject:** Who or what is this about? (e.g., "Alice")
- **Predicate:** What property? (e.g., "knows" or "age")
- **Object:** What is the value? (e.g., "Bob" or "25")
- **Graph:** Which dataset? (can omit for default)

### Add Some People

Let's add facts about people. Update your code:

```javascript
import { createUnrdfStore, namedNode, literal } from '@unrdf/core';

const store = createUnrdfStore();

// Create URIs for our people (like URLs that identify them)
const alice = namedNode('http://example.com/alice');
const bob = namedNode('http://example.com/bob');
const charlie = namedNode('http://example.com/charlie');

// Create URIs for properties
const knows = namedNode('http://xmlns.com/foaf/0.1/knows');
const name = namedNode('http://xmlns.com/foaf/0.1/name');
const age = namedNode('http://xmlns.com/foaf/0.1/age');

// Add facts: Alice knows Bob
store.addQuad({
  subject: alice,
  predicate: knows,
  object: bob
});

// Add facts: Alice knows Charlie
store.addQuad({
  subject: alice,
  predicate: knows,
  object: charlie
});

// Add names
store.addQuad({
  subject: alice,
  predicate: name,
  object: literal('Alice')
});

store.addQuad({
  subject: bob,
  predicate: name,
  object: literal('Bob')
});

// Add ages
store.addQuad({
  subject: alice,
  predicate: age,
  object: literal('30', namedNode('http://www.w3.org/2001/XMLSchema#integer'))
});

store.addQuad({
  subject: bob,
  predicate: age,
  object: literal('25', namedNode('http://www.w3.org/2001/XMLSchema#integer'))
});

console.log('Data added!');
console.log(`Store has ${store.size} quads`);
```

Run it:

```bash
node query.mjs
```

Output:
```
Data added!
Store has 7 quads
```

**What just happened:**
- We created identifiers (URIs) for people and properties
- We added 7 facts to the store
- Each fact (quad) describes a relationship
- The store now contains: Alice knows Bob, Alice knows Charlie, plus their names and ages

---

## Part 3: Your First Query

### The Query

Now let's ask a question: "Who does Alice know?"

In SPARQL, we write:

```sparql
SELECT ?person WHERE {
  <http://example.com/alice> <http://xmlns.com/foaf/0.1/knows> ?person .
}
```

This means: "Find all values for ?person where Alice knows that person."

### Execute the Query

Update your code:

```javascript
import {
  createUnrdfStore,
  namedNode,
  literal,
  executeQuerySync  // Add this
} from '@unrdf/core';

const store = createUnrdfStore();

// ... (add the data like before) ...

// Execute a SPARQL query
const query = `
  SELECT ?person WHERE {
    <http://example.com/alice> <http://xmlns.com/foaf/0.1/knows> ?person .
  }
`;

const results = executeQuerySync(store, query);

console.log('Results:');
results.forEach(binding => {
  console.log('  Person:', binding.person.value);
});
```

Run it:

```bash
node query.mjs
```

Output:
```
Results:
  Person: http://example.com/bob
  Person: http://example.com/charlie
```

**What just happened:**
- We wrote a SPARQL SELECT query
- `executeQuerySync()` ran the query on the store
- The query found 2 matches: Bob and Charlie
- Each result is a "binding" that maps the variable ?person to a value

---

## Part 4: Getting Names Instead of URIs

### The Problem

The results show URIs like `http://example.com/bob`, but we want the friendly names "Bob" and "Charlie".

### The Solution

We need to join data: "Find the person AND their name." In SPARQL, we use multiple patterns:

```javascript
const query = `
  SELECT ?personName WHERE {
    <http://example.com/alice> <http://xmlns.com/foaf/0.1/knows> ?person .
    ?person <http://xmlns.com/foaf/0.1/name> ?personName .
  }
`;

const results = executeQuerySync(store, query);

console.log('People Alice knows:');
results.forEach(binding => {
  console.log('  -', binding.personName.value);
});
```

Run it:

```bash
node query.mjs
```

Output:
```
People Alice knows:
  - Bob
  - Charlie
```

**What just happened:**
- Line 1 of the query finds people that Alice knows
- Line 2 joins that with the names property
- The result is friendly names instead of URIs
- This is called a **JOIN** in database terms

---

## Part 5: Asking Different Questions

### Find Everyone's Age

Let's ask: "What are the names and ages of everyone?"

```javascript
const query = `
  SELECT ?name ?age WHERE {
    ?person <http://xmlns.com/foaf/0.1/name> ?name .
    ?person <http://xmlns.com/foaf/0.1/age> ?age .
  }
`;

const results = executeQuerySync(store, query);

console.log('Names and ages:');
results.forEach(binding => {
  console.log(`  - ${binding.name.value}: ${binding.age.value} years old`);
});
```

Output:
```
Names and ages:
  - Alice: 30 years old
  - Bob: 25 years old
```

### Find Who Knows Someone Over 25

Use a FILTER clause to add conditions:

```javascript
const query = `
  SELECT ?aliceFriends ?age WHERE {
    <http://example.com/alice> <http://xmlns.com/foaf/0.1/knows> ?friend .
    ?friend <http://xmlns.com/foaf/0.1/age> ?age .
    FILTER (?age >= 25)
  }
`;

const results = executeQuerySync(store, query);

console.log('Friends of Alice who are at least 25:');
results.forEach(binding => {
  console.log(`  - Age: ${binding.age.value}`);
});
```

---

## Part 6: Verify Your Understanding

### Complete Working Example

Here's the entire program you've built:

```javascript
import {
  createUnrdfStore,
  namedNode,
  literal,
  executeQuerySync
} from '@unrdf/core';

const store = createUnrdfStore();

// Create URIs
const alice = namedNode('http://example.com/alice');
const bob = namedNode('http://example.com/bob');
const charlie = namedNode('http://example.com/charlie');

const knows = namedNode('http://xmlns.com/foaf/0.1/knows');
const name = namedNode('http://xmlns.com/foaf/0.1/name');
const age = namedNode('http://xmlns.com/foaf/0.1/age');

// Add data
store.addQuad({ subject: alice, predicate: knows, object: bob });
store.addQuad({ subject: alice, predicate: knows, object: charlie });
store.addQuad({ subject: alice, predicate: name, object: literal('Alice') });
store.addQuad({ subject: bob, predicate: name, object: literal('Bob') });
store.addQuad({ subject: charlie, predicate: name, object: literal('Charlie') });
store.addQuad({
  subject: alice,
  predicate: age,
  object: literal('30', namedNode('http://www.w3.org/2001/XMLSchema#integer'))
});
store.addQuad({
  subject: bob,
  predicate: age,
  object: literal('25', namedNode('http://www.w3.org/2001/XMLSchema#integer'))
});

// Query: Who does Alice know?
const query1 = `
  SELECT ?personName WHERE {
    <http://example.com/alice> <http://xmlns.com/foaf/0.1/knows> ?person .
    ?person <http://xmlns.com/foaf/0.1/name> ?personName .
  }
`;

console.log('People Alice knows:');
executeQuerySync(store, query1).forEach(binding => {
  console.log('  -', binding.personName.value);
});

// Query: Names and ages
const query2 = `
  SELECT ?name ?age WHERE {
    ?person <http://xmlns.com/foaf/0.1/name> ?name .
    ?person <http://xmlns.com/foaf/0.1/age> ?age .
  }
`;

console.log('\nNames and ages:');
executeQuerySync(store, query2).forEach(binding => {
  console.log(`  - ${binding.name.value}: ${binding.age.value} years old`);
});
```

### Test Your Knowledge

Try these exercises:

1. **Add a property:** Add a "city" property to some people. Then query to find everyone from a specific city.

2. **More relationships:** Add "Alice works with Bob" and "Bob supervises Charlie". Then query to find everyone Alice indirectly connects to.

3. **Optional patterns:** Use the OPTIONAL keyword to find all people and their ages (even if some don't have ages specified).

---

## Summary

You've learned:
- ✅ How to create an RDF store
- ✅ How to add RDF data (quads with subject, predicate, object)
- ✅ How to write and execute SPARQL SELECT queries
- ✅ How to JOIN data across properties
- ✅ How to FILTER results
- ✅ How to access query results in JavaScript

### Next Steps

- Read **02-basic-workflow** to learn about multiple formats and complex patterns
- Read **optimize-sparql-queries** (How-To) to make your queries run faster
- Read **SPARQL-concepts** (Explanation) to understand SPARQL theory

---

## Troubleshooting

**Q: I get "Cannot find module '@unrdf/core'"**
A: Run `npm install @unrdf/core` in your project directory.

**Q: My query returns no results**
A: Check that your URIs match exactly (case-sensitive). Use `console.log()` to print the URIs you're using and comparing.

**Q: The code runs but prints nothing**
A: Check that `results` is not empty. Add `console.log('Total results:', results.length)` to debug.

---

**Ready to continue?** Move on to "02-basic-workflow" to learn about working with multiple RDF formats and complex query patterns.
