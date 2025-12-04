# Quick Start - Building Your First RDF Application

Get up and running with UNRDF in 15 minutes.

## Project Setup

### 1. Create a new directory

```bash
mkdir my-knowledge-base
cd my-knowledge-base
npm init -y
```

### 2. Add package.json configuration

Edit `package.json`:

```json
{
  "type": "module",
  "name": "my-knowledge-base",
  "version": "1.0.0",
  "dependencies": {
    "@unrdf/core": "^5.0.0"
  },
  "scripts": {
    "start": "node src/index.mjs",
    "query": "node src/query.mjs"
  }
}
```

### 3. Install dependencies

```bash
npm install
```

---

## Build Your First RDF Application

### Step 1: Create sample data

Create `src/data.ttl`:

```turtle
@prefix ex: <http://example.org/> .
@prefix foaf: <http://xmlns.com/foaf/0.1/> .
@prefix dct: <http://purl.org/dc/terms/> .

# People
ex:alice a foaf:Person ;
  foaf:name "Alice Johnson" ;
  foaf:email "alice@example.org" ;
  foaf:knows ex:bob, ex:charlie .

ex:bob a foaf:Person ;
  foaf:name "Bob Smith" ;
  foaf:email "bob@example.org" ;
  foaf:knows ex:alice .

ex:charlie a foaf:Person ;
  foaf:name "Charlie Brown" ;
  foaf:email "charlie@example.org" ;
  foaf:knows ex:alice .

# Projects
ex:proj1 a ex:Project ;
  dct:title "RDF Basics" ;
  dct:creator ex:alice ;
  ex:members ex:alice, ex:bob .

ex:proj2 a ex:Project ;
  dct:title "Knowledge Graphs" ;
  dct:creator ex:bob ;
  ex:members ex:bob, ex:charlie .
```

### Step 2: Create the main application

Create `src/index.mjs`:

```javascript
import { createKnowledgeSubstrateCore } from '@unrdf/core';
import { readFileSync } from 'fs';

// Initialize UNRDF
const core = await createKnowledgeSubstrateCore();

// Load RDF data
const rdfData = readFileSync('src/data.ttl', 'utf-8');
const store = core.parseRdf(rdfData, { format: 'turtle' });

console.log(`✓ Loaded ${store.size} triples`);

// Export for use in other modules
export { core, store };
```

### Step 3: Query the data

Create `src/query.mjs`:

```javascript
import { core, store } from './index.mjs';

// Query 1: Find all people
console.log('\n=== All People ===');
const people = await core.query(store, `
  PREFIX foaf: <http://xmlns.com/foaf/0.1/>

  SELECT ?name ?email
  WHERE {
    ?person a foaf:Person ;
            foaf:name ?name ;
            foaf:email ?email .
  }
  ORDER BY ?name
`);

for (const row of people) {
  console.log(`  ${row.get('name').value} (${row.get('email').value})`);
}

// Query 2: Find friend networks
console.log('\n=== Friend Networks ===');
const friends = await core.query(store, `
  PREFIX foaf: <http://xmlns.com/foaf/0.1/>

  SELECT ?person ?friend
  WHERE {
    ?p foaf:name ?person ;
       foaf:knows ?f .
    ?f foaf:name ?friend .
  }
  ORDER BY ?person
`);

for (const row of friends) {
  console.log(`  ${row.get('person').value} knows ${row.get('friend').value}`);
}

// Query 3: Find projects by creator
console.log('\n=== Projects by Creator ===');
const projects = await core.query(store, `
  PREFIX ex: <http://example.org/>
  PREFIX foaf: <http://xmlns.com/foaf/0.1/>
  PREFIX dct: <http://purl.org/dc/terms/>

  SELECT ?project ?title ?creator
  WHERE {
    ?proj a ex:Project ;
          dct:title ?title ;
          dct:creator ?c .
    ?c foaf:name ?creator .
    BIND(STRAFTER(STR(?proj), "/") AS ?project)
  }
  ORDER BY ?creator
`);

for (const row of projects) {
  console.log(`  ${row.get('project').value}: "${row.get('title').value}" by ${row.get('creator').value}`);
}

// Query 4: Complex query - projects with members' friends
console.log('\n=== Team Collaboration Opportunities ===');
const collab = await core.query(store, `
  PREFIX ex: <http://example.org/>
  PREFIX foaf: <http://xmlns.com/foaf/0.1/>
  PREFIX dct: <http://purl.org/dc/terms/>

  SELECT ?project ?member ?friend
  WHERE {
    ?proj a ex:Project ;
          dct:title ?project ;
          ex:members ?m .
    ?m foaf:name ?member ;
       foaf:knows ?f .
    ?f foaf:name ?friend .
    FILTER (?m != ?f)
  }
`);

for (const row of collab) {
  console.log(`  ${row.get('member').value} on "${row.get('project').value}" could invite ${row.get('friend').value}`);
}
```

### Step 4: Run it

```bash
npm start
npm run query
```

Expected output:

```
✓ Loaded 16 triples

=== All People ===
  Alice Johnson (alice@example.org)
  Bob Smith (bob@example.org)
  Charlie Brown (charlie@example.org)

=== Friend Networks ===
  Alice Johnson knows Bob Smith
  Alice Johnson knows Charlie Brown
  Bob Smith knows Alice Johnson
  Charlie Brown knows Alice Johnson

=== Projects by Creator ===
  proj1: "RDF Basics" by Alice Johnson
  proj2: "Knowledge Graphs" by Bob Smith

=== Team Collaboration Opportunities ===
  Alice Johnson on "RDF Basics" could invite Charlie Brown
  Bob Smith on "Knowledge Graphs" could invite Alice Johnson
```

---

## Next: Add Validation

### Define SHACL Shapes

Create `src/shapes.ttl`:

```turtle
@prefix sh: <http://www.w3.org/ns/shacl#> .
@prefix ex: <http://example.org/> .
@prefix foaf: <http://xmlns.com/foaf/0.1/> .

# Person shape
ex:PersonShape a sh:NodeShape ;
  sh:targetClass foaf:Person ;
  sh:property [
    sh:path foaf:name ;
    sh:minCount 1 ;
    sh:datatype xsd:string ;
  ] ;
  sh:property [
    sh:path foaf:email ;
    sh:minCount 1 ;
    sh:datatype xsd:string ;
  ] .

# Project shape
ex:ProjectShape a sh:NodeShape ;
  sh:targetClass ex:Project ;
  sh:property [
    sh:path dct:title ;
    sh:minCount 1 ;
    sh:datatype xsd:string ;
  ] ;
  sh:property [
    sh:path dct:creator ;
    sh:minCount 1 ;
  ] .
```

### Validate Data

Create `src/validate.mjs`:

```javascript
import { core, store } from './index.mjs';
import { readFileSync } from 'fs';

const shapesData = readFileSync('src/shapes.ttl', 'utf-8');
const shapesStore = core.parseRdf(shapesData, { format: 'turtle' });

const report = await core.validateShacl(store, shapesStore);

if (report.conforms) {
  console.log('✓ All data is valid!');
} else {
  console.log('✗ Validation errors:');
  for (const result of report.results || []) {
    console.log(`  - ${result.message}`);
  }
}
```

Run:

```bash
node src/validate.mjs
```

---

## Next: Add Transactions

Create `src/transact.mjs`:

```javascript
import { core, store } from './index.mjs';
import { namedNode, literal } from '@rdfjs/data-model';

// Add a new person inside a transaction
const tx = await core.beginTransaction();

try {
  store.addQuad(
    namedNode('http://example.org/diana'),
    namedNode('http://www.w3.org/1999/02/22-rdf-syntax-ns#type'),
    namedNode('http://xmlns.com/foaf/0.1/Person')
  );

  store.addQuad(
    namedNode('http://example.org/diana'),
    namedNode('http://xmlns.com/foaf/0.1/name'),
    literal('Diana Prince')
  );

  await tx.commit();
  console.log('✓ Transaction committed');
} catch (error) {
  await tx.rollback();
  console.log('✗ Transaction rolled back:', error.message);
}
```

---

## Next: Define Knowledge Hooks

Create `src/hooks.mjs`:

```javascript
import { defineHook, registerHook } from '@unrdf/hooks';
import { core, store } from './index.mjs';

// Define a hook that logs new people
const logNewPeople = defineHook({
  meta: { name: 'log-new-people' },
  trigger: 'INSERT',
  pattern: '?person a foaf:Person ; foaf:name ?name .',

  run(event) {
    console.log(`🎉 New person added: ${event.quad.object.value}`);
  }
});

registerHook(logNewPeople);

// Add a new person - hook will fire automatically
const { namedNode, literal } = require('@rdfjs/data-model');

store.addQuad(
  namedNode('http://example.org/eve'),
  namedNode('http://xmlns.com/foaf/0.1/name'),
  literal('Eve Wilson')
);
// Output: 🎉 New person added: Eve Wilson
```

---

## Project Structure

Final project layout:

```
my-knowledge-base/
├── package.json
├── src/
│   ├── index.mjs         # Initialize UNRDF
│   ├── data.ttl          # Sample RDF data
│   ├── shapes.ttl        # SHACL shapes
│   ├── query.mjs         # Query examples
│   ├── validate.mjs      # Validation examples
│   ├── transact.mjs      # Transaction examples
│   └── hooks.mjs         # Hook examples
└── node_modules/
```

---

## What You Learned

✓ Load RDF data
✓ Query with SPARQL
✓ Validate with SHACL
✓ Use transactions
✓ Define Knowledge Hooks

---

## Next Steps

1. **Explore more queries** → [../EXAMPLES.md](../EXAMPLES.md)
2. **Learn the API** → [../API-REFERENCE.md](../API-REFERENCE.md)
3. **Understand architecture** → [../ARCHITECTURE.md](../ARCHITECTURE.md)
4. **Check all packages** → [../PACKAGES.md](../PACKAGES.md)
5. **Join the community** → GitHub Discussions

---

**Happy knowledge graphing!** 🚀
