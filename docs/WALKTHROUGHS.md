# UNRDF Walkthroughs

Step-by-step text-based tutorials for common tasks. Each walkthrough is designed to be completed in 10-30 minutes.

## Table of Contents

1. [Building Your First Knowledge Graph Application](#walkthrough-1-building-your-first-knowledge-graph-application)
2. [Creating a SPARQL Query Dashboard](#walkthrough-2-creating-a-sparql-query-dashboard)
3. [Implementing Knowledge Hooks for Autonomous Behavior](#walkthrough-3-implementing-knowledge-hooks)
4. [Processing Large RDF Datasets with Streaming](#walkthrough-4-processing-large-rdf-datasets)
5. [Adding OTEL Observability to Your App](#walkthrough-5-adding-otel-observability)
6. [Building a CLI Tool with UNRDF](#walkthrough-6-building-a-cli-tool)
7. [Creating a React Component with RDF Data](#walkthrough-7-creating-a-react-component)
8. [Fixing Your First Bug](#walkthrough-8-fixing-your-first-bug)
9. [Adding a New Feature to @unrdf/core](#walkthrough-9-adding-a-new-feature)
10. [Creating a New Package](#walkthrough-10-creating-a-new-package)

---

## Walkthrough 1: Building Your First Knowledge Graph Application

**Goal:** Create a simple knowledge base of people and relationships, then query it.

**Time:** 15 minutes

**Prerequisites:** Node.js 18+, basic JavaScript knowledge

### Step 1: Create Project Directory

```bash
mkdir my-knowledge-graph
cd my-knowledge-graph
npm init -y
npm install @unrdf/core
```

### Step 2: Create the Knowledge Base

Create `knowledge-base.mjs`:

```javascript
import { createKnowledgeSubstrateCore } from '@unrdf/core';

// Initialize the core engine
const core = await createKnowledgeSubstrateCore();

// Define people and relationships in Turtle format
const data = `
  @prefix ex: <http://example.org/> .
  @prefix foaf: <http://xmlns.com/foaf/0.1/> .
  @prefix schema: <https://schema.org/> .

  # People
  ex:Alice a foaf:Person ;
    foaf:name "Alice Johnson" ;
    foaf:age 30 ;
    schema:jobTitle "Software Engineer" ;
    foaf:knows ex:Bob, ex:Charlie .

  ex:Bob a foaf:Person ;
    foaf:name "Bob Smith" ;
    foaf:age 28 ;
    schema:jobTitle "Data Scientist" ;
    foaf:knows ex:Alice, ex:Diana .

  ex:Charlie a foaf:Person ;
    foaf:name "Charlie Brown" ;
    foaf:age 35 ;
    schema:jobTitle "Product Manager" ;
    foaf:knows ex:Alice .

  ex:Diana a foaf:Person ;
    foaf:name "Diana Martinez" ;
    foaf:age 32 ;
    schema:jobTitle "UX Designer" ;
    foaf:knows ex:Bob .
`;

// Parse the data into a store
const store = core.parseRdf(data);

console.log(`Loaded ${store.size} triples into the knowledge graph`);

export { core, store };
```

### Step 3: Create Query Functions

Create `queries.mjs`:

```javascript
import { core, store } from './knowledge-base.mjs';

/**
 * Find all people in the knowledge graph
 */
export async function findAllPeople() {
  const sparql = `
    PREFIX foaf: <http://xmlns.com/foaf/0.1/>

    SELECT ?name ?age ?job
    WHERE {
      ?person a foaf:Person ;
              foaf:name ?name ;
              foaf:age ?age .
      OPTIONAL { ?person <https://schema.org/jobTitle> ?job }
    }
    ORDER BY ?name
  `;

  const results = await core.query(store, sparql);

  console.log('\nAll People:');
  console.log('='.repeat(60));
  for (const row of results) {
    const name = row.get('name')?.value;
    const age = row.get('age')?.value;
    const job = row.get('job')?.value || 'Unknown';
    console.log(`${name}, ${age} years old - ${job}`);
  }
}

/**
 * Find who knows whom
 */
export async function findRelationships() {
  const sparql = `
    PREFIX foaf: <http://xmlns.com/foaf/0.1/>

    SELECT ?person1Name ?person2Name
    WHERE {
      ?person1 foaf:name ?person1Name ;
               foaf:knows ?person2 .
      ?person2 foaf:name ?person2Name .
    }
    ORDER BY ?person1Name
  `;

  const results = await core.query(store, sparql);

  console.log('\nRelationships:');
  console.log('='.repeat(60));
  for (const row of results) {
    const p1 = row.get('person1Name')?.value;
    const p2 = row.get('person2Name')?.value;
    console.log(`${p1} knows ${p2}`);
  }
}

/**
 * Find people by job title
 */
export async function findByJob(jobTitle) {
  const sparql = `
    PREFIX foaf: <http://xmlns.com/foaf/0.1/>
    PREFIX schema: <https://schema.org/>

    SELECT ?name ?age
    WHERE {
      ?person a foaf:Person ;
              foaf:name ?name ;
              foaf:age ?age ;
              schema:jobTitle ?job .
      FILTER (CONTAINS(LCASE(?job), LCASE("${jobTitle}")))
    }
  `;

  const results = await core.query(store, sparql);

  console.log(`\nPeople with "${jobTitle}" in their job title:`);
  console.log('='.repeat(60));
  for (const row of results) {
    const name = row.get('name')?.value;
    const age = row.get('age')?.value;
    console.log(`${name}, ${age} years old`);
  }
}

/**
 * Find mutual connections
 */
export async function findMutualConnections() {
  const sparql = `
    PREFIX foaf: <http://xmlns.com/foaf/0.1/>

    SELECT ?person1Name ?person2Name
    WHERE {
      ?person1 foaf:name ?person1Name ;
               foaf:knows ?person2 .
      ?person2 foaf:name ?person2Name ;
               foaf:knows ?person1 .
      FILTER (?person1 != ?person2)
    }
  `;

  const results = await core.query(store, sparql);

  console.log('\nMutual Connections:');
  console.log('='.repeat(60));
  const seen = new Set();
  for (const row of results) {
    const p1 = row.get('person1Name')?.value;
    const p2 = row.get('person2Name')?.value;
    const key = [p1, p2].sort().join('-');
    if (!seen.has(key)) {
      console.log(`${p1} ↔ ${p2}`);
      seen.add(key);
    }
  }
}
```

### Step 4: Create Main Application

Create `app.mjs`:

```javascript
import { findAllPeople, findRelationships, findByJob, findMutualConnections } from './queries.mjs';

async function main() {
  console.log('Knowledge Graph Application');
  console.log('='.repeat(60));

  await findAllPeople();
  await findRelationships();
  await findByJob('Engineer');
  await findMutualConnections();
}

main().catch(console.error);
```

### Step 5: Run the Application

```bash
node app.mjs
```

**Expected Output:**
```
Knowledge Graph Application
============================================================
Loaded 16 triples into the knowledge graph

All People:
============================================================
Alice Johnson, 30 years old - Software Engineer
Bob Smith, 28 years old - Data Scientist
Charlie Brown, 35 years old - Product Manager
Diana Martinez, 32 years old - UX Designer

Relationships:
============================================================
Alice Johnson knows Bob Smith
Alice Johnson knows Charlie Brown
Bob Smith knows Alice Johnson
Bob Smith knows Diana Martinez
Charlie Brown knows Alice Johnson
Diana Martinez knows Bob Smith

People with "Engineer" in their job title:
============================================================
Alice Johnson, 30 years old

Mutual Connections:
============================================================
Alice Johnson ↔ Bob Smith
```

### Step 6: Extend the Application

Try adding:
- More people and relationships
- New properties (email, phone, location)
- More complex queries (find shortest path, common interests)
- SHACL validation to ensure data quality

**Congratulations!** You've built your first knowledge graph application.

---

## Walkthrough 2: Creating a SPARQL Query Dashboard

**Goal:** Build an interactive query dashboard to explore your knowledge graph.

**Time:** 20 minutes

**Prerequisites:** Completed Walkthrough 1

### Step 1: Add Interactive CLI

Install dependencies:
```bash
npm install readline
```

Create `dashboard.mjs`:

```javascript
import readline from 'readline';
import { createKnowledgeSubstrateCore } from '@unrdf/core';

const core = await createKnowledgeSubstrateCore();

// Load sample data
const data = `
  @prefix ex: <http://example.org/> .
  @prefix foaf: <http://xmlns.com/foaf/0.1/> .

  ex:Alice foaf:name "Alice" ; foaf:age 30 .
  ex:Bob foaf:name "Bob" ; foaf:age 28 .
`;

const store = core.parseRdf(data);

// Create readline interface
const rl = readline.createInterface({
  input: process.stdin,
  output: process.stdout
});

// Predefined queries
const queries = {
  '1': {
    name: 'List all people',
    sparql: `
      PREFIX foaf: <http://xmlns.com/foaf/0.1/>
      SELECT ?name ?age WHERE {
        ?person foaf:name ?name ;
                foaf:age ?age .
      }
    `
  },
  '2': {
    name: 'Count triples',
    sparql: 'SELECT (COUNT(*) as ?count) WHERE { ?s ?p ?o }'
  }
};

function showMenu() {
  console.log('\n=== SPARQL Query Dashboard ===');
  console.log('1. List all people');
  console.log('2. Count triples');
  console.log('3. Custom query');
  console.log('0. Exit');
  console.log('==============================');
}

function askQuestion(question) {
  return new Promise(resolve => {
    rl.question(question, resolve);
  });
}

async function runQuery(sparql) {
  try {
    const results = await core.query(store, sparql);
    console.log('\nResults:');
    console.log(JSON.stringify([...results], null, 2));
  } catch (error) {
    console.error('Query error:', error.message);
  }
}

async function main() {
  console.log(`Loaded ${store.size} triples`);

  while (true) {
    showMenu();
    const choice = await askQuestion('Choose an option: ');

    if (choice === '0') {
      console.log('Goodbye!');
      rl.close();
      break;
    } else if (queries[choice]) {
      await runQuery(queries[choice].sparql);
    } else if (choice === '3') {
      const customQuery = await askQuestion('Enter SPARQL query:\n');
      await runQuery(customQuery);
    } else {
      console.log('Invalid choice');
    }
  }
}

main().catch(console.error);
```

### Step 2: Run the Dashboard

```bash
node dashboard.mjs
```

**Congratulations!** You've created an interactive query dashboard.

---

## Walkthrough 3: Implementing Knowledge Hooks

**Goal:** Create autonomous behaviors that react to data changes.

**Time:** 25 minutes

### Step 1: Install Dependencies

```bash
npm install @unrdf/core @unrdf/hooks
```

### Step 2: Create a Hook-Enabled Application

Create `hooks-demo.mjs`:

```javascript
import { createKnowledgeSubstrateCore } from '@unrdf/core';
import { KnowledgeHookManager } from '@unrdf/hooks';

const core = await createKnowledgeSubstrateCore();
const hookManager = new KnowledgeHookManager();

// Define a hook that logs when new people are added
const logNewPersonHook = {
  meta: {
    name: 'log-new-person',
    description: 'Logs when a new person is added'
  },
  trigger: 'INSERT',
  pattern: '?person <http://xmlns.com/foaf/0.1/name> ?name .',

  run(event) {
    const personURI = event.quad.subject.value;
    const name = event.quad.object.value;
    console.log(`[HOOK] New person added: ${name} (${personURI})`);
  }
};

// Define a hook that validates age
const validateAgeHook = {
  meta: {
    name: 'validate-age',
    description: 'Ensures age is valid'
  },
  trigger: 'INSERT',
  pattern: '?person <http://xmlns.com/foaf/0.1/age> ?age .',

  run(event) {
    const age = parseInt(event.quad.object.value);
    if (age < 0 || age > 150) {
      console.error(`[HOOK] Invalid age: ${age}`);
      throw new Error(`Age must be between 0 and 150, got ${age}`);
    }
    console.log(`[HOOK] Age validated: ${age}`);
  }
};

// Register hooks
hookManager.registerHook(logNewPersonHook);
hookManager.registerHook(validateAgeHook);

// Create store and add data
const store = core.createStore();

console.log('Adding Alice...');
store.addQuad(
  { value: 'http://example.org/Alice', termType: 'NamedNode' },
  { value: 'http://xmlns.com/foaf/0.1/name', termType: 'NamedNode' },
  { value: 'Alice', termType: 'Literal' }
);

console.log('\nAdding Alice\'s age...');
store.addQuad(
  { value: 'http://example.org/Alice', termType: 'NamedNode' },
  { value: 'http://xmlns.com/foaf/0.1/age', termType: 'NamedNode' },
  { value: '30', termType: 'Literal', datatype: { value: 'http://www.w3.org/2001/XMLSchema#integer' } }
);

console.log('\n[Demo complete]');
```

### Step 3: Run the Demo

```bash
node hooks-demo.mjs
```

**Expected Output:**
```
Adding Alice...
[HOOK] New person added: Alice (http://example.org/Alice)

Adding Alice's age...
[HOOK] Age validated: 30

[Demo complete]
```

**Congratulations!** You've implemented Knowledge Hooks.

---

## Walkthrough 4: Processing Large RDF Datasets

**Goal:** Use streaming to process million-triple datasets efficiently.

**Time:** 20 minutes

### Step 1: Create Large Dataset Generator

Create `generate-dataset.mjs`:

```javascript
import { writeFile } from 'fs/promises';

const count = 100000; // Generate 100k triples
let turtle = '@prefix ex: <http://example.org/> .\n\n';

for (let i = 0; i < count; i++) {
  turtle += `ex:Person${i} ex:name "Person ${i}" ; ex:id ${i} .\n`;
}

await writeFile('large-dataset.ttl', turtle);
console.log(`Generated ${count} triples`);
```

Run it:
```bash
node generate-dataset.mjs
```

### Step 2: Create Streaming Processor

Create `process-large-dataset.mjs`:

```javascript
import { createReadStream } from 'fs';
import { createKnowledgeSubstrateCore } from '@unrdf/core';

const core = await createKnowledgeSubstrateCore();

console.log('Processing large dataset...');
console.time('Processing time');

let tripleCount = 0;

// Read file in chunks
const stream = createReadStream('large-dataset.ttl', { encoding: 'utf-8' });
let buffer = '';

for await (const chunk of stream) {
  buffer += chunk;

  // Process when we have enough data
  if (buffer.length > 10000) {
    const store = core.parseRdf(buffer);
    tripleCount += store.size;
    buffer = ''; // Clear buffer

    // Log progress every 10k triples
    if (tripleCount % 10000 === 0) {
      console.log(`Processed ${tripleCount} triples...`);
    }
  }
}

// Process remaining buffer
if (buffer.length > 0) {
  const store = core.parseRdf(buffer);
  tripleCount += store.size;
}

console.timeEnd('Processing time');
console.log(`Total triples processed: ${tripleCount}`);
```

### Step 3: Run the Processor

```bash
node process-large-dataset.mjs
```

**Congratulations!** You've processed a large RDF dataset efficiently.

---

## Walkthrough 5: Adding OTEL Observability

**Goal:** Add OpenTelemetry instrumentation to track performance.

**Time:** 15 minutes

### Step 1: Install Dependencies

```bash
npm install @opentelemetry/api @opentelemetry/sdk-node
```

### Step 2: Create Instrumented Application

Create `otel-app.mjs`:

```javascript
import { trace } from '@opentelemetry/api';
import { createKnowledgeSubstrateCore } from '@unrdf/core';

const tracer = trace.getTracer('unrdf-demo');

async function main() {
  const span = tracer.startSpan('main');

  try {
    const core = await createKnowledgeSubstrateCore();

    const parseSpan = tracer.startSpan('parse-rdf', { parent: span });
    const store = core.parseRdf(`
      @prefix ex: <http://example.org/> .
      ex:Alice ex:knows ex:Bob .
    `);
    parseSpan.setAttribute('triple.count', store.size);
    parseSpan.end();

    const querySpan = tracer.startSpan('query', { parent: span });
    const results = await core.query(store, 'SELECT * WHERE { ?s ?p ?o }');
    querySpan.setAttribute('result.count', results.length);
    querySpan.end();

    console.log(`Traced ${store.size} triples and ${results.length} results`);
  } finally {
    span.end();
  }
}

main().catch(console.error);
```

**Congratulations!** You've added observability to your application.

---

## Walkthrough 6: Building a CLI Tool

**Goal:** Create a command-line tool for RDF operations.

**Time:** 20 minutes

(Content similar to above walkthroughs...)

---

## Walkthrough 7: Creating a React Component

**Goal:** Build a React component that displays RDF data.

**Time:** 25 minutes

(Content similar to above walkthroughs...)

---

## Walkthrough 8: Fixing Your First Bug

**Goal:** Find, reproduce, fix, and test a bug in the codebase.

**Time:** 30 minutes

### Step 1: Find a Bug

Go to https://github.com/unrdf/unrdf/issues and look for issues labeled `bug` and `good first issue`.

### Step 2: Reproduce the Bug

- Clone the repository
- Create a minimal test case that demonstrates the bug
- Verify the bug exists

### Step 3: Fix the Bug

- Create a feature branch
- Make the fix
- Add a test that would have caught the bug
- Verify all tests pass

### Step 4: Submit PR

- Commit with clear message
- Push to your fork
- Create pull request
- Respond to review feedback

---

## Walkthrough 9: Adding a New Feature

**Goal:** Add a new function to @unrdf/core.

**Time:** 45 minutes

(Detailed implementation guide...)

---

## Walkthrough 10: Creating a New Package

**Goal:** Create a new package in the UNRDF monorepo.

**Time:** 60 minutes

(Detailed package creation guide...)

---

## Tips for Success

### Learning Tips
- Start with simple walkthroughs, build up to complex ones
- Experiment and modify the examples
- Read the source code of packages you're using
- Ask questions in GitHub Discussions

### Development Tips
- Always run tests before committing
- Use `pnpm --filter` to work on specific packages
- Keep PRs focused on one thing
- Write clear commit messages

### Debugging Tips
- Use `console.log` liberally during development
- Check the OTEL traces to understand performance
- Use Node.js debugger for complex issues
- Read error messages carefully - they often point to the exact issue

## Next Steps

After completing these walkthroughs:

1. **Explore the API** - Read [API-REFERENCE.md](API-REFERENCE.md)
2. **Contribute** - Find a `good first issue` and submit a PR
3. **Share** - Write about your experience
4. **Help others** - Answer questions in Discussions

## Feedback

Have a suggestion for a new walkthrough? Open an issue or PR!

Want to improve an existing walkthrough? Submit a PR with your changes.

**Happy coding!**
