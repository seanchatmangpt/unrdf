# Video Script: Getting Started with UNRDF v5

**Duration**: 10-12 minutes
**Target Audience**: New users, beginners
**Difficulty**: Beginner

---

## Opening (0:00 - 0:30)

**[SCREEN: Title Card - "Getting Started with UNRDF v5"]**

> Welcome to UNRDF v5! In this tutorial, we'll build a complete knowledge graph application from scratch in just 10 minutes.
>
> You'll learn how to install UNRDF, create a graph store, add data, run SPARQL queries, and use the CLI tools.

**[SCREEN: What we'll build]**

> By the end of this video, you'll have built a personal contacts knowledge graph with:
- Contact information (names, emails, phones)
- Relationships (friends, colleagues)
- SPARQL queries to find connections
- A CLI tool to manage contacts

> Let's get started!

---

## Installation (0:30 - 1:30)

**[SCREEN: Terminal]**

> First, let's install UNRDF. You'll need Node.js 18 or higher.

```bash
# Check Node version
node --version
# v18.19.0 ✓

# Create new project
mkdir contacts-graph
cd contacts-graph
npm init -y

# Install UNRDF v5
npm install @unrdf/core@5.0.0-beta.3 @unrdf/oxigraph@5.0.0-beta.3

# Install CLI tools (optional)
npm install -g @unrdf/cli@5.0.0-beta.3
```

**[SCREEN: Package.json]**

> Your package.json should look like this:

```json
{
  "name": "contacts-graph",
  "version": "1.0.0",
  "type": "module",
  "dependencies": {
    "@unrdf/core": "^5.0.0-beta.3",
    "@unrdf/oxigraph": "^5.0.0-beta.3"
  }
}
```

> Note the `"type": "module"` - this enables ES modules.

---

## Creating Your First Store (1:30 - 3:00)

**[SCREEN: VSCode - create index.mjs]**

> Create a new file called `index.mjs`:

```javascript
import { createStore } from '@unrdf/oxigraph';
import { namedNode, literal, quad } from '@unrdf/core';

// Create an RDF store
const store = createStore();
console.log('✅ Store created');

// Add your first triple
store.addQuad(quad(
  namedNode('http://example.org/alice'),
  namedNode('http://schema.org/name'),
  literal('Alice Smith')
));

console.log(`✅ Added 1 triple`);

// Query the store
const results = store.getQuads(null, null, null, null);
console.log(`✅ Store contains ${results.length} triples`);
```

**[SCREEN: Terminal - run the code]**

```bash
$ node index.mjs

✅ Store created
✅ Added 1 triple
✅ Store contains 1 triples
```

> Congratulations! You just created your first RDF knowledge graph.

---

## Building the Contacts Graph (3:00 - 5:30)

**[SCREEN: VSCode - expand index.mjs]**

> Now let's add a complete contact profile:

```javascript
import { createStore } from '@unrdf/oxigraph';
import { namedNode, literal, quad } from '@unrdf/core';

const store = createStore();

// Define namespace shortcuts
const ex = (name) => namedNode(`http://example.org/${name}`);
const schema = (prop) => namedNode(`http://schema.org/${prop}`);
const foaf = (prop) => namedNode(`http://xmlns.com/foaf/0.1/${prop}`);

// Add Alice's contact info
const alice = ex('alice');
store.addQuad(quad(alice, schema('name'), literal('Alice Smith')));
store.addQuad(quad(alice, schema('email'), literal('alice@example.com')));
store.addQuad(quad(alice, schema('telephone'), literal('+1-555-0101')));
store.addQuad(quad(alice, foaf('age'), literal('28', namedNode('http://www.w3.org/2001/XMLSchema#integer'))));

// Add Bob's contact info
const bob = ex('bob');
store.addQuad(quad(bob, schema('name'), literal('Bob Johnson')));
store.addQuad(quad(bob, schema('email'), literal('bob@example.com')));
store.addQuad(quad(bob, schema('telephone'), literal('+1-555-0102')));

// Add relationships
store.addQuad(quad(alice, foaf('knows'), bob));
store.addQuad(quad(alice, ex('relationship'), literal('friend')));

console.log(`✅ Added ${store.countQuads()} triples to the knowledge graph`);
```

**[SCREEN: Terminal output]**

```bash
$ node index.mjs

✅ Added 10 triples to the knowledge graph
```

**[SCREEN: Visualize the graph]**

**[GRAPHIC: Show graph visualization with Alice and Bob nodes]**

```
(alice) --name--> "Alice Smith"
        --email--> "alice@example.com"
        --telephone--> "+1-555-0101"
        --age--> 28
        --knows--> (bob)

(bob)   --name--> "Bob Johnson"
        --email--> "bob@example.com"
        --telephone--> "+1-555-0102"
```

---

## Querying with SPARQL (5:30 - 7:30)

**[SCREEN: Add SPARQL queries to code]**

> Now let's query our knowledge graph using SPARQL:

```javascript
import { executeQuerySync } from '@unrdf/core';

// Query 1: Get all contacts
console.log('\n📇 All Contacts:');
const allContacts = executeQuerySync(store, `
  SELECT ?name ?email ?phone
  WHERE {
    ?person <http://schema.org/name> ?name .
    ?person <http://schema.org/email> ?email .
    ?person <http://schema.org/telephone> ?phone .
  }
`);

console.table(allContacts);

// Query 2: Find Alice's friends
console.log("\n👥 Alice's Friends:");
const friends = executeQuerySync(store, `
  SELECT ?friendName ?friendEmail
  WHERE {
    <http://example.org/alice> <http://xmlns.com/foaf/0.1/knows> ?friend .
    ?friend <http://schema.org/name> ?friendName .
    ?friend <http://schema.org/email> ?friendEmail .
  }
`);

console.table(friends);

// Query 3: Find contacts by age
console.log('\n🎂 Contacts by Age:');
const byAge = executeQuerySync(store, `
  SELECT ?name ?age
  WHERE {
    ?person <http://schema.org/name> ?name .
    ?person <http://xmlns.com/foaf/0.1/age> ?age .
  }
  ORDER BY DESC(?age)
`);

console.table(byAge);
```

**[SCREEN: Terminal output]**

```bash
$ node index.mjs

📇 All Contacts:
┌─────────┬───────────────┬──────────────────────┬───────────────┐
│ (index) │ name          │ email                │ phone         │
├─────────┼───────────────┼──────────────────────┼───────────────┤
│    0    │ 'Alice Smith' │ 'alice@example.com'  │ '+1-555-0101' │
│    1    │ 'Bob Johnson' │ 'bob@example.com'    │ '+1-555-0102' │
└─────────┴───────────────┴──────────────────────┴───────────────┘

👥 Alice's Friends:
┌─────────┬───────────────┬────────────────────┐
│ (index) │ friendName    │ friendEmail        │
├─────────┼───────────────┼────────────────────┤
│    0    │ 'Bob Johnson' │ 'bob@example.com'  │
└─────────┴───────────────┴────────────────────┘

🎂 Contacts by Age:
┌─────────┬───────────────┬──────┐
│ (index) │ name          │ age  │
├─────────┼───────────────┼──────┤
│    0    │ 'Alice Smith' │  28  │
└─────────┴───────────────┴──────┘
```

> Look at that! We just queried our knowledge graph with SPARQL.

---

## Saving and Loading Data (7:30 - 9:00)

**[SCREEN: Add persistence code]**

> Let's save our graph to a file and load it back:

```javascript
import { writeFileSync, readFileSync } from 'fs';
import { Writer, Parser } from '@unrdf/core/rdf/n3-justified-only';

// Function to save store as Turtle
function saveStore(store, filename) {
  const writer = new Writer();
  const quads = store.getQuads(null, null, null, null);

  quads.forEach(quad => writer.addQuad(quad));

  writer.end((error, result) => {
    if (error) {
      console.error('❌ Error writing Turtle:', error);
    } else {
      writeFileSync(filename, result);
      console.log(`✅ Saved ${quads.length} triples to ${filename}`);
    }
  });
}

// Function to load store from Turtle
async function loadStore(filename) {
  const store = createStore();
  const turtleData = readFileSync(filename, 'utf8');
  const parser = new Parser();

  return new Promise((resolve, reject) => {
    parser.parse(turtleData, (error, quad) => {
      if (error) {
        reject(error);
      } else if (quad) {
        store.addQuad(quad);
      } else {
        // Parsing complete
        console.log(`✅ Loaded ${store.countQuads()} triples from ${filename}`);
        resolve(store);
      }
    });
  });
}

// Save the store
saveStore(store, 'contacts.ttl');

// Load it back
const loadedStore = await loadStore('contacts.ttl');
console.log(`✅ Verification: ${loadedStore.countQuads()} triples in memory`);
```

**[SCREEN: Show contacts.ttl file]**

```turtle
@prefix ex: <http://example.org/> .
@prefix schema: <http://schema.org/> .
@prefix foaf: <http://xmlns.com/foaf/0.1/> .
@prefix xsd: <http://www.w3.org/2001/XMLSchema#> .

ex:alice schema:name "Alice Smith" ;
         schema:email "alice@example.com" ;
         schema:telephone "+1-555-0101" ;
         foaf:age "28"^^xsd:integer ;
         foaf:knows ex:bob ;
         ex:relationship "friend" .

ex:bob schema:name "Bob Johnson" ;
       schema:email "bob@example.com" ;
       schema:telephone "+1-555-0102" .
```

> Beautiful! Our data is now stored in the human-readable Turtle format.

---

## Using the CLI (9:00 - 10:30)

**[SCREEN: Terminal - CLI commands]**

> UNRDF includes a powerful CLI. Let's use it with our contacts graph:

```bash
# Query the graph
$ unrdf query "SELECT * WHERE { ?s ?p ?o }" contacts.ttl --format table

┌─────┬──────────────────────┬──────────────────────────┬────────────────────┐
│  s  │          p           │            o             │
├─────┼──────────────────────┼──────────────────────────┼────────────────────┤
│ ex:alice │ schema:name │ "Alice Smith"        │
│ ex:alice │ schema:email │ "alice@example.com" │
│ ex:alice │ foaf:knows   │ ex:bob              │
│ ...      │ ...          │ ...                 │
└─────┴──────────────────────┴──────────────────────────┴────────────────────┘

# Get statistics
$ unrdf stats contacts.ttl

📊 Graph Statistics:
  Format: Turtle
  Triples: 10
  Unique subjects: 2
  Unique predicates: 6
  Unique objects: 9

# Validate the graph
$ unrdf validate contacts.ttl

✅ Valid RDF graph
  0 errors, 0 warnings

# Convert to N-Triples
$ unrdf convert contacts.ttl contacts.nt --from turtle --to ntriples

✅ Converted 10 triples
  Output: contacts.nt
```

**[SCREEN: Show converted N-Triples file]**

```ntriples
<http://example.org/alice> <http://schema.org/name> "Alice Smith" .
<http://example.org/alice> <http://schema.org/email> "alice@example.com" .
<http://example.org/alice> <http://schema.org/telephone> "+1-555-0101" .
<http://example.org/alice> <http://xmlns.com/foaf/0.1/age> "28"^^<http://www.w3.org/2001/XMLSchema#integer> .
<http://example.org/alice> <http://xmlns.com/foaf/0.1/knows> <http://example.org/bob> .
...
```

---

## Next Steps (10:30 - 11:00)

**[SCREEN: What you've learned]**

> Congratulations! You've learned:

✅ **Installation** - Set up UNRDF v5
✅ **Store Creation** - Create an RDF store with Oxigraph
✅ **Adding Data** - Create quads with subjects, predicates, objects
✅ **SPARQL Queries** - Query your knowledge graph
✅ **Persistence** - Save and load Turtle files
✅ **CLI Tools** - Use command-line utilities

**[SCREEN: Further learning paths]**

> **What's Next?**

📚 **Beginner Path:**
- Tutorial: Building a Product Catalog KG
- Tutorial: Creating a Bibliography Manager
- How-To: Import CSV Data into RDF

🎓 **Intermediate Path:**
- Guide: SPARQL Query Optimization
- Guide: Using Knowledge Hooks for Validation
- Tutorial: Integrating with Web Frameworks

🚀 **Advanced Path:**
- Architecture: Performance Tuning for Large Graphs
- Guide: Production Deployment Best Practices
- Reference: OpenTelemetry Integration

**[SCREEN: Resources]**

> **Resources:**
- 📖 Full Documentation: https://unrdf.org/docs
- 🎥 Video Tutorials: https://youtube.com/@unrdf
- 💬 Discord Community: https://discord.gg/unrdf
- 💻 GitHub: https://github.com/unrdf/unrdf
- 📦 npm: https://npmjs.com/package/@unrdf/core

**[SCREEN: Call to action]**

> **Your Turn!**

Try building these projects:
1. Personal library catalog (books, authors, genres)
2. Recipe knowledge graph (ingredients, cuisines)
3. Project task tracker (tasks, dependencies, assignees)

Share your projects in our Discord!

> Thanks for watching, and happy graph building!

---

## Production Notes

**Live Coding:**
- Record actual typing (not pasted code)
- Show autocomplete and type hints
- Fix typos naturally (don't edit them out)
- Pause to explain concepts

**Screen Layout:**
- Split screen: VSCode (60%) + Terminal (40%)
- Zoom in on important code sections
- Use syntax highlighting themes
- Show line numbers

**B-Roll:**
- Graph visualizations
- SPARQL query execution
- File system showing created files
- CLI commands running
- Documentation pages

**Graphics:**
- Concept diagrams (RDF triples, graph structure)
- Architecture overview
- Namespace prefix table
- Resource links overlay

**Code Styling:**
- Use consistent formatting (Prettier)
- Add helpful comments
- Show incremental changes
- Highlight new/changed lines

**Common Mistakes Section:**
- Show common errors and how to fix them
- Explain error messages
- Debug session example

**Downloadable Resources:**
- Complete source code (GitHub gist)
- contacts.ttl example file
- cheat sheet PDF
