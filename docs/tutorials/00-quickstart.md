# UNRDF Quickstart Tutorial

**Get started with UNRDF in 5 minutes.** Copy, paste, and run.

## What You'll Learn

- Install UNRDF packages
- Create an RDF store
- Add triples to the store
- Query data with pattern matching
- Query data with SPARQL

**Prerequisites:** Node.js 18+ and pnpm installed.

---

## Step 1: Installation

```bash
pnpm add @unrdf/oxigraph
```

**Expected output:**
```
Packages: +1
+ @unrdf/oxigraph 5.0.0
```

---

## Step 2: Create Your First RDF Store

Create a file `quickstart.mjs`:

```javascript
import { createStore, dataFactory } from '@unrdf/oxigraph';

// Create a new RDF store
const store = createStore();

console.log('✅ Store created');
console.log('Store size:', store.size);
```

**Run it:**
```bash
node quickstart.mjs
```

**Expected output:**
```
✅ Store created
Store size: 0
```

---

## Step 3: Add Data to the Store

Update `quickstart.mjs`:

```javascript
import { createStore, dataFactory } from '@unrdf/oxigraph';

// Create a new RDF store
const store = createStore();

// Create RDF terms using dataFactory
const alice = dataFactory.namedNode('http://example.com/alice');
const bob = dataFactory.namedNode('http://example.com/bob');
const knows = dataFactory.namedNode('http://xmlns.com/foaf/0.1/knows');
const name = dataFactory.namedNode('http://xmlns.com/foaf/0.1/name');
const age = dataFactory.namedNode('http://xmlns.com/foaf/0.1/age');

// Create literals for values
const aliceName = dataFactory.literal('Alice');
const bobName = dataFactory.literal('Bob');
const aliceAge = dataFactory.literal('30', dataFactory.namedNode('http://www.w3.org/2001/XMLSchema#integer'));

// Create and add triples (subject-predicate-object)
store.add(dataFactory.triple(alice, name, aliceName));
store.add(dataFactory.triple(alice, age, aliceAge));
store.add(dataFactory.triple(alice, knows, bob));
store.add(dataFactory.triple(bob, name, bobName));

console.log('✅ Added 4 triples to the store');
console.log('Store size:', store.size);
```

**Run it:**
```bash
node quickstart.mjs
```

**Expected output:**
```
✅ Added 4 triples to the store
Store size: 4
```

---

## Step 4: Query Data with Pattern Matching

Add this to `quickstart.mjs`:

```javascript
import { createStore, dataFactory } from '@unrdf/oxigraph';

// ... previous code for creating store and adding data ...

// Query using pattern matching
// Get all triples about Alice
const alice = dataFactory.namedNode('http://example.com/alice');
const aliceTriples = store.match(alice, null, null);

console.log('\n📊 All facts about Alice:');
aliceTriples.forEach(quad => {
  console.log(`  ${quad.subject.value}`);
  console.log(`    ${quad.predicate.value}`);
  console.log(`    ${quad.object.value}\n`);
});

// Get all names in the store
const name = dataFactory.namedNode('http://xmlns.com/foaf/0.1/name');
const allNames = store.match(null, name, null);

console.log('📝 All names in the store:');
allNames.forEach(quad => {
  console.log(`  ${quad.subject.value} → ${quad.object.value}`);
});
```

**Expected output:**
```
✅ Added 4 triples to the store
Store size: 4

📊 All facts about Alice:
  http://example.com/alice
    http://xmlns.com/foaf/0.1/name
    Alice

  http://example.com/alice
    http://xmlns.com/foaf/0.1/age
    30

  http://example.com/alice
    http://xmlns.com/foaf/0.1/knows
    http://example.com/bob

📝 All names in the store:
  http://example.com/alice → Alice
  http://example.com/bob → Bob
```

---

## Step 5: Query with SPARQL

Add SPARQL queries to `quickstart.mjs`:

```javascript
import { createStore, dataFactory } from '@unrdf/oxigraph';

// ... previous code ...

// SPARQL SELECT query
const selectQuery = `
  PREFIX foaf: <http://xmlns.com/foaf/0.1/>

  SELECT ?person ?name ?age
  WHERE {
    ?person foaf:name ?name .
    OPTIONAL { ?person foaf:age ?age }
  }
`;

const results = store.query(selectQuery);

console.log('\n🔍 SPARQL SELECT Results:');
results.forEach(binding => {
  console.log(`  Person: ${binding.get('person').value}`);
  console.log(`  Name: ${binding.get('name').value}`);
  if (binding.has('age')) {
    console.log(`  Age: ${binding.get('age').value}`);
  }
  console.log('');
});

// SPARQL ASK query (boolean result)
const askQuery = `
  PREFIX foaf: <http://xmlns.com/foaf/0.1/>

  ASK {
    <http://example.com/alice> foaf:knows <http://example.com/bob>
  }
`;

const knows = store.query(askQuery);
console.log(`❓ Does Alice know Bob? ${knows ? 'Yes ✅' : 'No ❌'}`);

// SPARQL CONSTRUCT query (creates new graph)
const constructQuery = `
  PREFIX foaf: <http://xmlns.com/foaf/0.1/>
  PREFIX ex: <http://example.com/>

  CONSTRUCT {
    ?person a ex:Person .
    ?person ex:hasName ?name .
  }
  WHERE {
    ?person foaf:name ?name .
  }
`;

const constructedTriples = store.query(constructQuery);
console.log(`\n🏗️  CONSTRUCT created ${constructedTriples.length} new triples`);
```

**Expected output:**
```
🔍 SPARQL SELECT Results:
  Person: http://example.com/alice
  Name: Alice
  Age: 30

  Person: http://example.com/bob
  Name: Bob

❓ Does Alice know Bob? Yes ✅

🏗️  CONSTRUCT created 2 new triples
```

---

## Step 6: Load RDF from Turtle

Add Turtle data loading:

```javascript
import { createStore } from '@unrdf/oxigraph';

const store = createStore();

// Load Turtle-formatted RDF data
const turtleData = `
@prefix foaf: <http://xmlns.com/foaf/0.1/> .
@prefix ex: <http://example.com/> .

ex:alice
  a foaf:Person ;
  foaf:name "Alice" ;
  foaf:age 30 ;
  foaf:knows ex:bob .

ex:bob
  a foaf:Person ;
  foaf:name "Bob" ;
  foaf:age 28 .
`;

store.load(turtleData, {
  format: 'text/turtle',
  base_iri: 'http://example.com/',
});

console.log(`✅ Loaded ${store.size} triples from Turtle`);

// Export as N-Quads
const nquads = store.dump({ format: 'application/n-quads' });
console.log('\n📤 Exported as N-Quads:');
console.log(nquads);
```

**Expected output:**
```
✅ Loaded 6 triples from Turtle

📤 Exported as N-Quads:
<http://example.com/alice> <http://www.w3.org/1999/02/22-rdf-syntax-ns#type> <http://xmlns.com/foaf/0.1/Person> .
<http://example.com/alice> <http://xmlns.com/foaf/0.1/name> "Alice" .
<http://example.com/alice> <http://xmlns.com/foaf/0.1/age> "30"^^<http://www.w3.org/2001/XMLSchema#integer> .
<http://example.com/alice> <http://xmlns.com/foaf/0.1/knows> <http://example.com/bob> .
<http://example.com/bob> <http://www.w3.org/1999/02/22-rdf-syntax-ns#type> <http://xmlns.com/foaf/0.1/Person> .
<http://example.com/bob> <http://xmlns.com/foaf/0.1/name> "Bob" .
```

---

## Complete Working Example

Here's the complete `quickstart.mjs` file:

```javascript
import { createStore, dataFactory } from '@unrdf/oxigraph';

// ============================================
// 1. Create Store
// ============================================
const store = createStore();
console.log('✅ Store created');

// ============================================
// 2. Add Data Programmatically
// ============================================
const alice = dataFactory.namedNode('http://example.com/alice');
const bob = dataFactory.namedNode('http://example.com/bob');
const knows = dataFactory.namedNode('http://xmlns.com/foaf/0.1/knows');
const name = dataFactory.namedNode('http://xmlns.com/foaf/0.1/name');
const age = dataFactory.namedNode('http://xmlns.com/foaf/0.1/age');

store.add(dataFactory.triple(alice, name, dataFactory.literal('Alice')));
store.add(dataFactory.triple(alice, age, dataFactory.literal('30')));
store.add(dataFactory.triple(alice, knows, bob));
store.add(dataFactory.triple(bob, name, dataFactory.literal('Bob')));

console.log(`✅ Added ${store.size} triples`);

// ============================================
// 3. Query with Pattern Matching
// ============================================
const aliceTriples = store.match(alice, null, null);
console.log(`\n📊 Found ${aliceTriples.length} triples about Alice`);

// ============================================
// 4. Query with SPARQL
// ============================================
const results = store.query(`
  PREFIX foaf: <http://xmlns.com/foaf/0.1/>
  SELECT ?name WHERE {
    ?person foaf:name ?name
  }
`);

console.log('\n🔍 All names via SPARQL:');
results.forEach(binding => {
  console.log(`  - ${binding.get('name').value}`);
});

// ============================================
// 5. Load and Export Data
// ============================================
const newStore = createStore();
newStore.load(`
  @prefix ex: <http://example.com/> .
  ex:charlie ex:knows ex:alice .
`, {
  format: 'text/turtle',
  base_iri: 'http://example.com/',
});

const exported = newStore.dump({ format: 'application/n-triples' });
console.log('\n📤 Exported data:');
console.log(exported);

console.log('\n🎉 Quickstart complete!');
```

**Run it:**
```bash
node quickstart.mjs
```

---

## What's Next?

### Learn More

- **[RDF Operations Tutorial](./02-rdf-operations.md)** - Deep dive into RDF data manipulation
- **[SPARQL Tutorial](./sparql.md)** - Advanced SPARQL queries and updates
- **[Validation Tutorial](./validation.md)** - Validate data with SHACL shapes

### Common Tasks

- **Working with different RDF formats?** → Check [Creating RDF Documents](./creating-rdf-documents.md)
- **Need automated workflows?** → See [Knowledge Hooks](./knowledge-hooks.md)
- **Building reactive apps?** → Explore [Composables & Context](./03-composables-context.md)

### API Reference

- **[@unrdf/oxigraph](../../packages/oxigraph/README.md)** - Store implementation and SPARQL
- **[@unrdf/core](../../packages/core/README.md)** - Core RDF operations

---

## Troubleshooting

### Import Errors

**Problem:** `Cannot find package '@unrdf/oxigraph'`

**Solution:**
```bash
# Make sure you're using pnpm
pnpm add @unrdf/oxigraph

# Verify installation
pnpm list @unrdf/oxigraph
```

### SPARQL Query Errors

**Problem:** Query fails with "Parse error"

**Solution:**
- Check PREFIX declarations match your data
- Ensure proper syntax (. vs ; vs ,)
- Use single quotes in JavaScript strings for SPARQL

### Empty Results

**Problem:** Query returns no results

**Solution:**
```javascript
// Verify data was added
console.log('Store size:', store.size);

// Check what's in the store
const allTriples = store.match(null, null, null);
allTriples.forEach(q => console.log(q.subject.value, q.predicate.value, q.object.value));
```

---

## Key Takeaways

✅ **Always use `@unrdf/oxigraph` imports** - Not `n3` or other libraries
✅ **Use `createStore()` to create stores** - Not `new Store()`
✅ **Use `dataFactory` for RDF terms** - `namedNode()`, `literal()`, `triple()`
✅ **Two query methods:**
  - `store.match(s, p, o)` for pattern matching (fast)
  - `store.query(sparql)` for SPARQL (powerful)

✅ **Supported formats:** Turtle, N-Triples, N-Quads, JSON-LD, RDF/XML

---

**You're ready to build with UNRDF!** 🚀

Need help? Check the [GitHub Discussions](https://github.com/seanchatmangpt/unrdf/discussions) or [Troubleshooting Guide](../TROUBLESHOOTING.md).
