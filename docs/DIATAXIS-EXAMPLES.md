# Diataxis Examples: Real UNRDF Content

This document shows concrete examples of what each Diataxis type looks like for UNRDF packages.

---

## 1. Tutorial Example: Getting Started with @unrdf/core

### Real Tutorial: "Your First SPARQL Query"

**File:** `packages/core/docs/TUTORIALS/01-getting-started.md`

```markdown
# Tutorial: Your First SPARQL Query

In this tutorial, you'll create your first RDF knowledge base and query it with SPARQL.

## What You'll Learn
- Parse RDF data in Turtle format
- Store triples in memory
- Write and execute a SPARQL SELECT query
- Read and interpret query results

## What You'll Build
A simple social network with people and their relationships, then query it to find who knows whom.

## Prerequisites
- Node.js 18 or higher
- Basic JavaScript knowledge
- A text editor
- 15 minutes of time

## Before You Start

Create a new directory and initialize it:

\`\`\`bash
mkdir my-first-rdf-app
cd my-first-rdf-app
npm init -y
npm install @unrdf/core
\`\`\`

Verify installation:
\`\`\`bash
node --version  # Should be 18+
npm list @unrdf/core
\`\`\`

## Step 1: Create Your First RDF File

Create a file called \`knowledge-base.mjs\` that will hold our RDF data:

\`\`\`javascript
import { createKnowledgeSubstrateCore } from '@unrdf/core';

// Initialize the knowledge substrate
// This gives us everything: RDF storage, SPARQL, validation
const core = await createKnowledgeSubstrateCore();

// Define some RDF data in Turtle format
const rdfData = \`
  @prefix ex: <http://example.org/> .
  @prefix foaf: <http://xmlns.com/foaf/0.1/> .
  @prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .

  # Alice - a person
  ex:Alice a foaf:Person ;
    foaf:name "Alice Smith" ;
    foaf:email "alice@example.org" ;
    foaf:knows ex:Bob, ex:Charlie .

  # Bob - Alice's friend
  ex:Bob a foaf:Person ;
    foaf:name "Bob Johnson" ;
    foaf:email "bob@example.org" ;
    foaf:knows ex:Alice .

  # Charlie - Alice's friend
  ex:Charlie a foaf:Person ;
    foaf:name "Charlie Brown" ;
    foaf:email "charlie@example.org" ;
    foaf:knows ex:Alice .
\`;

// Parse the RDF data into our store
const store = core.parseRdf(rdfData);

// Verify it loaded
console.log(\`✅ Knowledge base loaded with \${store.size} triples\`);
\`\`\`

**What just happened:**
- We imported `createKnowledgeSubstrateCore` from the core package
- We initialized a knowledge substrate (the main API in UNRDF)
- We defined RDF data describing people and their relationships
- We parsed the RDF into an in-memory store
- We verified the store contains the data

The \`@prefix\` lines are shortcuts for long URLs:
- \`ex:\` = \`http://example.org/\`
- \`foaf:\` = \`http://xmlns.com/foaf/0.1/\` (a standard vocabulary for people)

Run this step:
\`\`\`bash
node knowledge-base.mjs
\`\`\`

Expected output:
\`\`\`
✅ Knowledge base loaded with 8 triples
\`\`\`

## Step 2: Write Your First SPARQL Query

Now let's query the data. Create a new file called \`query.mjs\`:

\`\`\`javascript
import { createKnowledgeSubstrateCore } from '@unrdf/core';

// Setup (same as before)
const core = await createKnowledgeSubstrateCore();
const rdfData = \`
  @prefix ex: <http://example.org/> .
  @prefix foaf: <http://xmlns.com/foaf/0.1/> .
  @prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .
  ex:Alice a foaf:Person ;
    foaf:name "Alice Smith" ;
    foaf:knows ex:Bob, ex:Charlie .
  ex:Bob a foaf:Person ;
    foaf:name "Bob Johnson" ;
    foaf:knows ex:Alice .
  ex:Charlie a foaf:Person ;
    foaf:name "Charlie Brown" ;
    foaf:knows ex:Alice .
\`;

const store = core.parseRdf(rdfData);

// First query: Find all people
const sparqlQuery = \`
  PREFIX foaf: <http://xmlns.com/foaf/0.1/>

  SELECT ?person ?name
  WHERE {
    ?person a foaf:Person ;
            foaf:name ?name .
  }
  ORDER BY ?name
\`;

// Execute the query
const results = await core.query(store, sparqlQuery);

// Display results
console.log('All people in the knowledge base:');
for (const binding of results) {
  const personUri = binding.get('person').value;
  const name = binding.get('name').value;
  console.log(\`  - \${name} (\${personUri})\`);
}
\`\`\`

**What's happening in the query:**
- \`PREFIX\` = shorthand for long URLs
- \`SELECT ?person ?name\` = return these two variables
- \`WHERE { ... }\` = the pattern to match
- \`?person a foaf:Person\` = find things that are a Person
- \`?person foaf:name ?name\` = get their name
- \`ORDER BY ?name\` = sort by name

Run it:
\`\`\`bash
node query.mjs
\`\`\`

Expected output:
\`\`\`
All people in the knowledge base:
  - Alice Smith (http://example.org/Alice)
  - Bob Johnson (http://example.org/Bob)
  - Charlie Brown (http://example.org/Charlie)
\`\`\`

## Step 3: Query Relationships

Now let's find who knows whom. Create \`relationships.mjs\`:

\`\`\`javascript
import { createKnowledgeSubstrateCore } from '@unrdf/core';

const core = await createKnowledgeSubstrateCore();
const rdfData = \`
  @prefix ex: <http://example.org/> .
  @prefix foaf: <http://xmlns.com/foaf/0.1/> .
  ex:Alice foaf:name "Alice Smith" ;
    foaf:knows ex:Bob, ex:Charlie .
  ex:Bob foaf:name "Bob Johnson" ;
    foaf:knows ex:Alice .
  ex:Charlie foaf:name "Charlie Brown" ;
    foaf:knows ex:Alice .
\`;

const store = core.parseRdf(rdfData);

// Find all relationships
const sparqlQuery = \`
  PREFIX foaf: <http://xmlns.com/foaf/0.1/>

  SELECT ?person1Name ?person2Name
  WHERE {
    ?person1 foaf:name ?person1Name ;
             foaf:knows ?person2 .
    ?person2 foaf:name ?person2Name .
  }
  ORDER BY ?person1Name ?person2Name
\`;

const results = await core.query(store, sparqlQuery);

console.log('Who knows whom:');
for (const binding of results) {
  const p1 = binding.get('person1Name').value;
  const p2 = binding.get('person2Name').value;
  console.log(\`  ${p1} knows ${p2}\`);
}
\`\`\`

Run it:
\`\`\`bash
node relationships.mjs
\`\`\`

Expected output:
\`\`\`
Who knows whom:
  Alice Smith knows Bob Johnson
  Alice Smith knows Charlie Brown
  Bob Johnson knows Alice Smith
  Charlie Brown knows Alice Smith
\`\`\`

## Step 4: Add More Data Dynamically

Now add new people without editing the RDF string. Create \`add-data.mjs\`:

\`\`\`javascript
import { createKnowledgeSubstrateCore } from '@unrdf/core';
import { namedNode, literal } from '@rdfjs/data-model';

const core = await createKnowledgeSubstrateCore();

// Start with initial data
const rdfData = \`
  @prefix ex: <http://example.org/> .
  @prefix foaf: <http://xmlns.com/foaf/0.1/> .
  ex:Alice foaf:name "Alice Smith" ;
    foaf:knows ex:Bob .
  ex:Bob foaf:name "Bob Johnson" .
\`;

const store = core.parseRdf(rdfData);
console.log(\`Starting with \${store.size} triples\`);

// Add a new person dynamically
// Create the URIs and values we need
const dianaUri = namedNode('http://example.org/Diana');
const nameProperty = namedNode('http://xmlns.com/foaf/0.1/name');
const dianaName = literal('Diana Lee');

// Add the triple (Diana, name, "Diana Lee")
store.addQuad(dianaUri, nameProperty, dianaName);
console.log(\`✅ Added Diana\`);
console.log(\`Now have \${store.size} triples\`);

// Verify Diana was added
const sparqlQuery = \`
  PREFIX foaf: <http://xmlns.com/foaf/0.1/>
  SELECT ?name WHERE { ?person foaf:name ?name . }
\`;

const results = await core.query(store, sparqlQuery);
console.log('All people now:');
for (const binding of results) {
  console.log(\`  - \${binding.get('name').value}\`);
}
\`\`\`

Run it:
\`\`\`bash
node add-data.mjs
\`\`\`

Expected output:
\`\`\`
Starting with 4 triples
✅ Added Diana
Now have 5 triples
All people now:
  - Alice Smith
  - Bob Johnson
  - Diana Lee
\`\`\`

## Verify It Works

You now have:
- ✅ Parsed RDF data
- ✅ Executed SPARQL queries
- ✅ Retrieved and read results
- ✅ Added data dynamically
- ✅ Queried relationships

## What You've Learned

In this tutorial, you learned:
1. How to initialize the UNRDF knowledge substrate
2. How to parse RDF data in Turtle format
3. How to write SPARQL queries to find patterns
4. How to read query results in JavaScript
5. How to add data to the store programmatically

## Next Steps

Now that you understand the basics:

👉 **Learn workflows:** [Tutorial: Common SPARQL Patterns](./02-basic-workflow.md) - Build on what you learned

👉 **Solve problems:** [How-To Guides](../HOW-TO/) - When you have specific tasks

👉 **Understand concepts:** [Explanation: SPARQL Execution](../EXPLANATION/sparql-execution.md) - How queries actually work

👉 **Look up details:** [API Reference](../REFERENCE/API.md) - Complete function reference

## Pro Tips

- **Prefix shortcuts:** Define \`@prefix\` once to avoid long URIs
- **Query testing:** Write queries incrementally, adding patterns piece by piece
- **Variable names:** Start with \`?\` (e.g., \`?person\`, \`?name\`)
- **Debugging:** Print \`store.size\` to verify data is loaded
- **Comments:** Use \`#\` in SPARQL to document what you're querying

## Common Mistakes

❌ **Forgetting @prefix** - Query won't find matches
✅ Use \`PREFIX foaf: <http://xmlns.com/foaf/0.1/>\` in SPARQL

❌ **Variable naming** - \`name\` vs \`?name\`
✅ Always use \`?name\` in queries, not \`name\`

❌ **Expecting case-insensitive** - RDF is case-sensitive
✅ \`foaf:name\` != \`foaf:Name\`

## Troubleshooting

**Q: Query returns no results**
- Check prefixes match between RDF and SPARQL
- Verify the property exists (try \`SELECT * WHERE { ?s ?p ?o }\`)
- Check for case-sensitivity issues

**Q: Getting type errors**
- Ensure Node.js 18+ (\`node --version\`)
- Verify \`@unrdf/core\` installed (\`npm list @unrdf/core\`)

**Q: Can't add data**
- Use \`namedNode\` for URIs, \`literal\` for strings
- Check you have the right properties/subjects

---

## Summary

You've completed the first tutorial! You can now:
- Create an RDF knowledge base
- Query it with SPARQL
- Add and modify data
- Interpret results

This is 80% of what you need to build with UNRDF.

Ready to learn more? → [Basic Workflow Tutorial](./02-basic-workflow.md)
```

---

## 2. How-To Example: Optimizing SPARQL Queries

**File:** `packages/core/docs/HOW-TO/optimize-sparql-queries.md`

```markdown
# How To: Optimize Slow SPARQL Queries

## Problem
Your SPARQL queries are taking too long. You need to speed them up.

## Prerequisites
- Familiarity with basic SPARQL (see [Getting Started](../TUTORIALS/01-getting-started.md))
- Understanding of your dataset size
- Basic understanding of query patterns

## Quick Diagnosis

First, identify where the slowness is:

\`\`\`javascript
const start = performance.now();
const results = await core.query(store, slowQuery);
const elapsed = performance.now() - start;
console.log(\`Query took \${elapsed}ms\`);
\`\`\`

**< 10ms:** Already fast ✅
**10-100ms:** Acceptable for most uses
**100-1000ms:** Slow, needs optimization
**> 1s:** Very slow, critical optimization needed

## Solution 1: Simplify the Pattern (Recommended)

**Problem:** Complex nested patterns force the engine to evaluate many combinations.

**Before (Slow):**
\`\`\`sparql
SELECT ?name ?email ?phone
WHERE {
  ?person a foaf:Person ;
          foaf:name ?name ;
          foaf:email ?email ;
          foaf:phone ?phone ;
          foaf:knows ?friend .
  ?friend foaf:knows ?otherFriend .
  ?otherFriend foaf:name ?friendName .
}
\`\`\`

**After (Fast):**
\`\`\`sparql
# First, find people with all contact info
SELECT ?name ?email ?phone
WHERE {
  ?person foaf:name ?name ;
          foaf:email ?email ;
          foaf:phone ?phone .
}
\`\`\`

**Why it's faster:**
- Fewer variables to track
- No nested graph patterns
- Engine can use indexes directly

## Solution 2: Add Filters Strategically

**Problem:** Bringing back all results then filtering wastes memory.

**Before (Slow):**
\`\`\`sparql
SELECT ?name ?age
WHERE {
  ?person foaf:name ?name ;
          foaf:age ?age .
}
\`\`\`
Then filter in JavaScript: \`results.filter(r => r.age > 18)\`

**After (Fast):**
\`\`\`sparql
SELECT ?name ?age
WHERE {
  ?person foaf:name ?name ;
          foaf:age ?age .
  FILTER (?age > 18)
}
\`\`\`

**Why it's faster:**
- Filter happens on the server side
- Fewer results to transfer
- Memory usage is lower

## Solution 3: Use LIMIT for Pagination

**Problem:** Returning all results when you only need first 10.

**Before (Slow):**
\`\`\`javascript
const allResults = await core.query(store, sparqlQuery);
const firstTen = allResults.slice(0, 10);
\`\`\`

**After (Fast):**
\`\`\`sparql
SELECT ?name
WHERE {
  ?person foaf:name ?name .
}
ORDER BY ?name
LIMIT 10
OFFSET 0
\`\`\`

**For pagination:**
\`\`\`javascript
// Page 1
let page = 0;
const pageSize = 10;

while (hasMore) {
  const results = await core.query(store, \`
    SELECT ?name WHERE { ... }
    LIMIT \${pageSize}
    OFFSET \${page * pageSize}
  \`);

  // Process results...
  page++;
}
\`\`\`

## Solution 4: Avoid Expensive Patterns

**Problem:** Some patterns force evaluation of many combinations.

**Slow Pattern - Avoid:**
\`\`\`sparql
# This joins every person with every other person!
SELECT ?person1 ?person2
WHERE {
  ?person1 foaf:name ?name1 .
  ?person2 foaf:name ?name2 .
}
\`\`\`

**Fast Pattern - Use:**
\`\`\`sparql
# Specific relationship
SELECT ?person1 ?person2
WHERE {
  ?person1 foaf:knows ?person2 ;
           foaf:name ?name1 .
  ?person2 foaf:name ?name2 .
}
\`\`\`

## Solution 5: Use Appropriate Data Types

**Problem:** String comparisons are slow.

**Before (Slow):**
\`\`\`sparql
SELECT ?person ?age
WHERE {
  ?person foaf:age ?ageStr .
  FILTER (?ageStr > "18")  # String comparison!
}
\`\`\`

**After (Fast):**
\`\`\`sparql
SELECT ?person ?age
WHERE {
  ?person foaf:age ?age .
  FILTER (?age > 18)  # Numeric comparison
}
\`\`\`

**In your RDF:**
\`\`\`turtle
@prefix xsd: <http://www.w3.org/2001/XMLSchema#> .

ex:Alice foaf:age 25 .          # Untyped (slow)
ex:Bob foaf:age "25"^^xsd:int . # Typed (fast)
\`\`\`

## Performance Comparison

| Optimization | Impact | Difficulty |
|--------------|--------|-----------|
| Simplify pattern | 10-100x | Easy |
| Add FILTER | 5-10x | Easy |
| Use LIMIT | 2-5x | Easy |
| Avoid expensive patterns | 10-50x | Medium |
| Use typed data | 2-5x | Medium |
| Add indexes | 10-100x | Hard |

## Measuring Improvements

\`\`\`javascript
async function benchmarkQuery(store, name, sparql) {
  const iterations = 10;
  const times = [];

  for (let i = 0; i < iterations; i++) {
    const start = performance.now();
    await core.query(store, sparql);
    const elapsed = performance.now() - start;
    times.push(elapsed);
  }

  const avg = times.reduce((a, b) => a + b) / times.length;
  const min = Math.min(...times);
  const max = Math.max(...times);

  console.log(\`\${name}: avg=\${avg.toFixed(2)}ms min=\${min}ms max=\${max}ms\`);
}

// Compare your queries
await benchmarkQuery(store, 'Original', originalQuery);
await benchmarkQuery(store, 'Optimized', optimizedQuery);
\`\`\`

## When to Use Indexes

If you still have slow queries:

\`\`\`javascript
// Create an index for frequently queried properties
await core.createIndex({
  property: 'http://xmlns.com/foaf/0.1/name',
  type: 'fulltext'
});

// Now queries filtering by name will be much faster
\`\`\`

See [Reference: Indexes](../REFERENCE/INDEXES.md) for details.

## Profiling Tools

See what the query engine is actually doing:

\`\`\`javascript
const results = await core.query(store, sparql, {
  profile: true
});

console.log(results.profile);
// Shows: pattern evaluation order, time per step, index usage
\`\`\`

## See Also
- [API Reference: Query Options](../REFERENCE/API.md#queryoptions)
- [Explanation: SPARQL Execution Model](../EXPLANATION/sparql-execution.md)
- [How-To: Working with Large Graphs](./large-graphs.md)
\`\`\`

---

## 3. Reference Example: API Reference

**File:** `packages/core/docs/REFERENCE/API.md`

```markdown
# API Reference: @unrdf/core

Complete reference for all functions and options in @unrdf/core.

## Main Function: createKnowledgeSubstrateCore()

### Signature

\`\`\`javascript
async function createKnowledgeSubstrateCore(options?: CoreOptions): Promise<KnowledgeSubstrate>
\`\`\`

### Description
Creates and initializes the main UNRDF knowledge substrate. This is the primary API you'll use for all RDF operations.

### Parameters

**options** (optional): Configuration object
- Type: \`CoreOptions\`
- Default: \`{ backend: 'memory', enableValidation: true }\`

\`\`\`javascript
interface CoreOptions {
  // Storage backend
  backend: 'memory' | 'oxigraph' | 'remote';

  // Enable SHACL validation
  enableValidation: boolean;
  default: true;

  // Enable transaction support
  enableTransactions: boolean;
  default: true;

  // Enable knowledge hooks
  enableHooks: boolean;
  default: true;

  // Maximum number of triples (memory only)
  maxTriples: number;
  default: Infinity;

  // Enable observability (OTEL)
  enableObservability: boolean;
  default: false;
}
\`\`\`

### Returns
Promise that resolves to \`KnowledgeSubstrate\` object

### Examples

**Basic (defaults, memory backend):**
\`\`\`javascript
const core = await createKnowledgeSubstrateCore();
\`\`\`

**With persistent storage:**
\`\`\`javascript
const core = await createKnowledgeSubstrateCore({
  backend: 'oxigraph',
  enableValidation: true
});
\`\`\`

**Performance-optimized (large datasets):**
\`\`\`javascript
const core = await createKnowledgeSubstrateCore({
  backend: 'oxigraph',
  maxTriples: 10_000_000,
  enableObservability: true
});
\`\`\`

### Throws
- \`InvalidOptionError\` - Invalid option value
- \`BackendError\` - Backend initialization failed
- \`PermissionError\` - No permission to create backend (oxigraph)

---

## Core Object: KnowledgeSubstrate

### Method: parseRdf()

\`\`\`javascript
parseRdf(
  data: string,
  format?: 'turtle' | 'ntriples' | 'jsonld' | 'rdfxml',
  options?: ParseOptions
): Store
\`\`\`

**Description:** Parse RDF data in various formats and return a Store.

**Parameters:**
- \`data\` (string, required): RDF content
- \`format\` (optional): Input format (auto-detected if omitted)
- \`options\` (optional): Parsing options

**Returns:** \`Store\` - In-memory triple store

**Examples:**

Turtle format (default):
\`\`\`javascript
const store = core.parseRdf(\`
  @prefix ex: <http://example.org/> .
  ex:Alice foaf:name "Alice" .
\`);
\`\`\`

N-Triples format:
\`\`\`javascript
const store = core.parseRdf(
  '<http://example.org/Alice> <http://xmlns.com/foaf/0.1/name> "Alice" .',
  'ntriples'
);
\`\`\`

JSON-LD format:
\`\`\`javascript
const store = core.parseRdf(
  {
    "@context": { "foaf": "http://xmlns.com/foaf/0.1/" },
    "@id": "http://example.org/Alice",
    "foaf:name": "Alice"
  },
  'jsonld'
);
\`\`\`

**Throws:**
- \`ParseError\` - Malformed RDF
- \`UnsupportedFormatError\` - Format not supported

---

### Method: query()

\`\`\`javascript
async query(
  store: Store,
  sparql: string,
  options?: QueryOptions
): Promise<QueryResults>
\`\`\`

**Description:** Execute a SPARQL query and return results.

**Parameters:**
- \`store\` (required): Store to query
- \`sparql\` (required): SPARQL query string
- \`options\` (optional): Query execution options

\`\`\`javascript
interface QueryOptions {
  // Timeout in milliseconds
  timeout: number;
  default: 30000;

  // Enable query profiling
  profile: boolean;
  default: false;

  // Maximum result set size
  maxResults: number;
  default: Infinity;

  // Base URI for relative IRIs
  baseUri: string;
  default: undefined;
}
\`\`\`

**Returns:** \`QueryResults\` - Array-like object with bindings

**Example:**

\`\`\`javascript
const results = await core.query(store, \`
  PREFIX foaf: <http://xmlns.com/foaf/0.1/>
  SELECT ?name WHERE {
    ?person foaf:name ?name .
  }
\`);

// Access results
for (const binding of results) {
  console.log(binding.get('name').value);
}
\`\`\`

**Throws:**
- \`QueryError\` - Invalid SPARQL syntax
- \`QueryTimeoutError\` - Query exceeded timeout
- \`MemoryError\` - Out of memory

---

## Type Definitions

### Store

\`\`\`javascript
interface Store {
  // Number of triples
  size: number;

  // Add a quad (triple with optional graph)
  addQuad(subject, predicate, object, graph?): void;

  // Remove a quad
  removeQuad(subject, predicate, object, graph?): void;

  // Find matching quads
  match(subject?, predicate?, object?, graph?): Iterable<Quad>;

  // Check if quad exists
  has(subject, predicate, object, graph?): boolean;

  // Clear all quads
  clear(): void;
}
\`\`\`

### QueryResults

\`\`\`javascript
interface QueryResults extends Array<Binding> {
  // Get results as JSON
  toJSON(): Record<string, string>[];

  // Get results as CSV
  toCSV(): string;

  // Get results as TSV
  toTSV(): string;

  // Get results as XML (SPARQL Results XML Format)
  toXML(): string;
}

interface Binding {
  // Get variable value
  get(varName: string): RDFTerm;

  // Get all variables
  getAll(): Map<string, RDFTerm>;

  // Check if variable bound
  has(varName: string): boolean;
}

type RDFTerm = NamedNode | Literal | BlankNode | Variable;

interface NamedNode {
  termType: 'NamedNode';
  value: string;  // The full URI
}

interface Literal {
  termType: 'Literal';
  value: string;  // The text value
  language?: string;  // Language tag (e.g., "en")
  datatype?: NamedNode;  // Type (e.g., xsd:integer)
}

interface BlankNode {
  termType: 'BlankNode';
  value: string;  // Blank node identifier
}
\`\`\`

---

## Error Reference

| Error | When | Solution |
|-------|------|----------|
| \`ParseError: Unexpected token\` | Malformed RDF syntax | Check RDF format and syntax |
| \`QueryError: Undefined namespace\` | SPARQL prefix not defined | Add \`PREFIX\` clause |
| \`QueryError: Variable not in SELECT\` | Ordering by undefined var | Add variable to SELECT |
| \`QueryTimeoutError\` | Query exceeded timeout | Simplify query, increase timeout |
| \`BackendError: Database locked\` | Oxigraph DB in use | Ensure only one instance |

---

## Configuration Reference

All options for \`createKnowledgeSubstrateCore()\`:

\`\`\`javascript
const core = await createKnowledgeSubstrateCore({
  // Backend selection
  backend: 'memory',           // 'memory', 'oxigraph', 'remote'

  // Validation
  enableValidation: true,      // Enable SHACL validation

  // Features
  enableTransactions: true,    // ACID transactions
  enableHooks: true,           // Knowledge hooks
  enableStreaming: true,       // Stream processing
  enableFederation: false,     // Federated queries

  // Memory (memory backend only)
  maxTriples: Infinity,

  // Oxigraph (oxigraph backend only)
  dbPath: './knowledge.db',
  autoVacuum: true,

  // Remote (remote backend only)
  endpoint: 'http://...',
  authentication: { username, password },

  // Observability
  enableObservability: false,
  otelEndpoint: 'http://...',

  // Logging
  logLevel: 'info',            // 'debug', 'info', 'warn', 'error'
});
\`\`\`

---

## See Also
- [How-To: Optimize Queries](../HOW-TO/optimize-sparql-queries.md)
- [Explanation: SPARQL Execution](../EXPLANATION/sparql-execution.md)
- [Error Codes](./ERRORS.md)
```

---

## 4. Explanation Example: Architecture

**File:** `packages/core/docs/EXPLANATION/architecture.md`

```markdown
# Explanation: How @unrdf/core is Organized

This document explains the internal architecture of @unrdf/core and the design decisions behind it.

## The Big Picture

@unrdf/core is organized in layers:

\`\`\`
┌─────────────────────────────────────────┐
│  Public API Layer                       │
│  (createKnowledgeSubstrateCore)         │
└─────────────────────────────────────────┘
                    ↓
┌─────────────────────────────────────────┐
│  Knowledge Substrate                    │
│  (Transactions, Hooks, Validation)      │
└─────────────────────────────────────────┘
                    ↓
┌─────────────────────────────────────────┐
│  RDF Core                               │
│  (SPARQL, SHACL, Storage)               │
└─────────────────────────────────────────┘
                    ↓
┌─────────────────────────────────────────┐
│  Backend (Memory/Oxigraph/Remote)       │
│  (Physical storage)                     │
└─────────────────────────────────────────┘
\`\`\`

Each layer serves a specific purpose:

## Layer 1: Backend (Bottom)

**Responsibility:** Store and retrieve triples

**Implementations:**
- \`MemoryBackend\` - In-memory JavaScript store
- \`OxigraphBackend\` - Persistent Rust-based store
- \`RemoteBackend\` - SPARQL endpoint proxy

**Why separate?**
- Users can choose storage strategy
- Easy to add new backends
- Core logic doesn't know about storage details

## Layer 2: RDF Core

**Responsibility:** RDF operations and query execution

**Components:**
- \`Parser\` - Convert RDF formats to quads
- \`Serializer\` - Convert quads to RDF formats
- \`SPARQLExecutor\` - Execute SPARQL queries
- \`SHACLValidator\` - Validate against shapes
- \`Store\` - Triple store interface

**Why separate?**
- SPARQL is complex, isolated in executor
- SHACL is optional, can be skipped
- Parser/Serializer are format-agnostic

## Layer 3: Knowledge Substrate

**Responsibility:** High-level features on top of RDF

**Components:**
- \`TransactionManager\` - ACID transactions
- \`HookManager\` - React to data changes
- \`ValidationManager\` - Integrate with SHACL
- \`CompositeStore\` - Multi-backend access

**Why separate?**
- These are optional features
- Users can disable for performance
- Cleaner API than mixing with RDF

## Layer 4: Public API

**Responsibility:** User-friendly interface

**Functions:**
- \`createKnowledgeSubstrateCore()\` - Initialize
- \`parseRdf()\` - Parse data
- \`query()\` - Execute queries
- Etc.

**Why have it?**
- Single entry point
- Encapsulates configuration
- Users don't need to know about layers

---

## Why This Design?

### Design Decision 1: Layered Architecture

**Alternative:** Monolithic - Everything in one class

**Why we chose layers:**
- Each layer has one job
- Easy to test in isolation
- Users can understand each piece
- Adding features doesn't break existing code

### Design Decision 2: Backend Abstraction

**Alternative:** Hardcode one storage strategy

**Why we chose abstraction:**
- Different users need different backends
  - Development: In-memory (fast, simple)
  - Production: Oxigraph (persistent, ACID)
  - Integration: Remote (connect to existing stores)
- Switching backends shouldn't require code changes

### Design Decision 3: Optional Features

**Alternative:** All features always enabled

**Why we chose optional:**
- Transactions have overhead (~5-10% slower)
- Hooks have overhead (~10-20% slower)
- Users only pay for what they use
- Lighter default configuration

---

## Real-World Analogy

Think of UNRDF like a house:

- **Backend** = Foundation (where everything rests)
- **RDF Core** = Structure (walls, rooms, framework)
- **Knowledge Substrate** = Utilities (electricity, plumbing, HVAC)
- **Public API** = Front door (how you enter)

You can live in a house with no utilities (like in-memory backend), but most people want them (like Oxigraph).

---

## Data Flow: A Query

Here's what happens when you execute a query:

\`\`\`javascript
const results = await core.query(store, sparqlQuery);
\`\`\`

1. **API Layer** receives the query
   - Validates options
   - Checks store is initialized

2. **SPARQL Executor** parses the query
   - Converts \`PREFIX\` to full URIs
   - Builds graph pattern tree
   - Plans execution strategy

3. **Query Planner** optimizes the query
   - Reorders patterns for efficiency
   - Identifies indexes to use
   - Predicts result size

4. **Execution Engine** runs the plan
   - Evaluates each pattern
   - Joins results
   - Applies filters

5. **Backend** retrieves triples
   - Looks up by subject, predicate, object
   - Uses indexes if available
   - Returns matching quads

6. **Results Formatter** converts output
   - Binds variables to values
   - Applies ORDER BY, LIMIT, etc.
   - Returns as \`QueryResults\`

## Performance Implications

This architecture has specific performance characteristics:

### Memory Footprint
- Backend stores all triples (unavoidable)
- Query planner caches some metadata
- Results are held in memory until consumed

### Query Speed
- First query slower (compilation, planning)
- Subsequent identical queries use cache
- Complex queries with many joins are slower

### Update Speed
- Triples are updated directly in backend
- Hooks are notified sequentially
- Transactions add overhead (but guarantee consistency)

---

## When (and When Not) to Use This Design

### Good for:
- Applications that need ACID guarantees
- Systems with complex data relationships
- Interactive queries (users waiting for results)
- Multi-user systems (transactions prevent conflicts)

### Not suitable for:
- Streaming 1 billion triples (use @unrdf/streaming)
- Write-only logging (use a database)
- Read-only bulk analytics (might want Hadoop)

---

## Future Evolution

This design allows for future improvements:

- **Better query planner:** Different planner, same interfaces
- **Distributed backend:** Sharded storage, same API
- **GPU acceleration:** SPARQL on GPU, same execution model
- **Incremental computation:** Track dependencies, invalidate incrementally

The layers don't change; implementations improve.

---

## See Also
- [Tutorial: How Your First Query Works](../TUTORIALS/01-getting-started.md#step-2-write-your-first-sparql-query)
- [How-To: Optimize Queries](../HOW-TO/optimize-sparql-queries.md)
- [API Reference](../REFERENCE/API.md)
```

---

## Summary

These examples show how Diataxis types differ:

| Type | Tone | Goal | Length | Structure |
|------|------|------|--------|-----------|
| **Tutorial** | Encouraging | Learn by doing | 15-30 min | Numbered steps |
| **How-To** | Direct | Solve a problem | 5-10 min | Solution-focused |
| **Reference** | Formal | Complete info | No limit | Organized by function |
| **Explanation** | Educational | Understand concepts | 10-20 min | Why + how + implications |

Each serves a different need and should not try to be the others.
