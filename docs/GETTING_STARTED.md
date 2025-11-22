# Getting Started with UNRDF

This tutorial guides you from zero to your first working knowledge graph application. By the end, you'll understand UNRDF's core concepts and have a functional RDF workflow.

## Prerequisites

Before starting, ensure you have:

- **Node.js 18+**: Check with `node --version`
- **pnpm** (recommended): Install with `npm install -g pnpm`
- **Basic RDF knowledge**: Familiarity with triples (subject-predicate-object)

## Installation

### Step 1: Create a New Project

```bash
mkdir my-knowledge-app
cd my-knowledge-app
pnpm init
```

### Step 2: Install UNRDF

```bash
pnpm add unrdf
```

### Step 3: Configure ES Modules

Add `"type": "module"` to your `package.json`:

```json
{
  "name": "my-knowledge-app",
  "type": "module",
  "dependencies": {
    "unrdf": "^4.0.0"
  }
}
```

## Setting Up Your Development Environment

### Recommended VS Code Extensions

- **SPARQL** - Syntax highlighting for queries
- **Turtle** - RDF Turtle format support
- **ESLint** - Code quality

### Project Structure

```
my-knowledge-app/
  data/
    schema.ttl        # SHACL shapes
    data.ttl          # RDF data
  hooks/
    health-check.rq   # SPARQL queries for hooks
  src/
    index.mjs         # Main application
  package.json
```

## Your First RDF Document

Create `data/people.ttl`:

```turtle
@prefix ex: <http://example.org/> .
@prefix foaf: <http://xmlns.com/foaf/0.1/> .
@prefix xsd: <http://www.w3.org/2001/XMLSchema#> .

ex:alice a foaf:Person ;
    foaf:name "Alice Smith" ;
    foaf:age 30 ;
    foaf:knows ex:bob, ex:carol .

ex:bob a foaf:Person ;
    foaf:name "Bob Jones" ;
    foaf:age 25 .

ex:carol a foaf:Person ;
    foaf:name "Carol White" ;
    foaf:age 35 ;
    foaf:knows ex:alice .
```

## Loading and Querying Data

Create `src/index.mjs`:

```javascript
import { readFileSync } from 'node:fs';
import { parseTurtle, query, select } from 'unrdf/knowledge-engine';

// Load Turtle file
const turtleData = readFileSync('./data/people.ttl', 'utf-8');

// Parse into N3 Store
const store = await parseTurtle(turtleData);

console.log(`Loaded ${store.size} triples`);

// Query all people
const people = await select(store, `
  PREFIX foaf: <http://xmlns.com/foaf/0.1/>

  SELECT ?person ?name ?age
  WHERE {
    ?person a foaf:Person ;
            foaf:name ?name .
    OPTIONAL { ?person foaf:age ?age }
  }
  ORDER BY ?name
`);

console.log('People in graph:');
people.forEach(row => {
  console.log(`  - ${row.name.value} (age: ${row.age?.value || 'unknown'})`);
});
```

Run with:

```bash
node src/index.mjs
```

Expected output:

```
Loaded 11 triples
People in graph:
  - Alice Smith (age: 30)
  - Bob Jones (age: 25)
  - Carol White (age: 35)
```

## Running Your First Query

UNRDF supports all SPARQL 1.1 query forms:

### SELECT Queries

```javascript
import { select } from 'unrdf/knowledge-engine';

// Find friends of friends
const foaf = await select(store, `
  PREFIX foaf: <http://xmlns.com/foaf/0.1/>

  SELECT ?person ?friend ?friendOfFriend
  WHERE {
    ?person foaf:knows ?friend .
    ?friend foaf:knows ?friendOfFriend .
    FILTER(?person != ?friendOfFriend)
  }
`);
```

### ASK Queries

```javascript
import { ask } from 'unrdf/knowledge-engine';

// Check if Alice knows Bob
const aliceKnowsBob = await ask(store, `
  PREFIX ex: <http://example.org/>
  PREFIX foaf: <http://xmlns.com/foaf/0.1/>

  ASK { ex:alice foaf:knows ex:bob }
`);

console.log(`Alice knows Bob: ${aliceKnowsBob}`); // true
```

### CONSTRUCT Queries

```javascript
import { construct } from 'unrdf/knowledge-engine';

// Create a new graph with derived data
const friendGraph = await construct(store, `
  PREFIX foaf: <http://xmlns.com/foaf/0.1/>
  PREFIX ex: <http://example.org/>

  CONSTRUCT {
    ?person ex:hasFriend ?friend .
  }
  WHERE {
    ?person foaf:knows ?friend .
  }
`);
```

## Adding SHACL Validation

Create `data/schema.ttl`:

```turtle
@prefix sh: <http://www.w3.org/ns/shacl#> .
@prefix foaf: <http://xmlns.com/foaf/0.1/> .
@prefix xsd: <http://www.w3.org/2001/XMLSchema#> .
@prefix ex: <http://example.org/> .

ex:PersonShape a sh:NodeShape ;
    sh:targetClass foaf:Person ;
    sh:property [
        sh:path foaf:name ;
        sh:minCount 1 ;
        sh:maxCount 1 ;
        sh:datatype xsd:string ;
        sh:message "Every person must have exactly one name"
    ] ;
    sh:property [
        sh:path foaf:age ;
        sh:datatype xsd:integer ;
        sh:minInclusive 0 ;
        sh:maxInclusive 150 ;
        sh:message "Age must be between 0 and 150"
    ] .
```

Add validation to your code:

```javascript
import { validateShacl, formatValidationReport } from 'unrdf/knowledge-engine';

const shapesData = readFileSync('./data/schema.ttl', 'utf-8');

const report = await validateShacl(store, shapesData);

if (report.conforms) {
  console.log('Data is valid!');
} else {
  console.log('Validation errors:');
  const formatted = formatValidationReport(report);
  console.log(formatted);
}
```

## Common Pitfalls and Solutions

### FMEA-Identified Failure Modes

| Failure Mode | Symptom | Solution |
|--------------|---------|----------|
| Missing `type: module` | `ERR_REQUIRE_ESM` | Add to package.json |
| Wrong import path | `Cannot find module` | Use `unrdf/knowledge-engine` |
| Async not awaited | Empty results | Add `await` to parse/query |
| Invalid Turtle | Parse error | Validate syntax online |
| Missing prefixes | Unknown prefix error | Declare all prefixes |

### Debugging Tips

```javascript
// Enable debug logging
process.env.DEBUG = 'unrdf:*';

// Check store contents
console.log('Store size:', store.size);
console.log('All quads:', [...store].slice(0, 5));

// Validate Turtle before parsing
import { parseTurtle } from 'unrdf/knowledge-engine';

try {
  const store = await parseTurtle(data);
} catch (error) {
  console.error('Parse error:', error.message);
  // Check line number in error
}
```

### Performance Tips

```javascript
// Use streaming for large files
import { createReadStream } from 'node:fs';
import { parseTurtle } from 'unrdf/knowledge-engine';

// For files > 10MB, consider chunked processing
const store = await parseTurtle(largeData, {
  baseIRI: 'http://example.org/'
});

// Index frequently-queried predicates
// UNRDF auto-optimizes common patterns
```

## Next Steps

Now that you have the basics working:

1. **Learn Knowledge Hooks**: [Defining Hooks Guide](./guides/defining-hooks.md)
2. **Explore React Integration**: [React Hooks Guide](./guides/react-integration.md)
3. **Understand Architecture**: [System Design](./ARCHITECTURE.md)
4. **Deep Dive SPARQL**: [SPARQL Tutorial](./tutorials/sparql.md)

## Complete Example

Here's a full working example combining all concepts:

```javascript
// src/complete-example.mjs
import { readFileSync } from 'node:fs';
import {
  parseTurtle,
  select,
  ask,
  validateShacl,
  reason,
  toTurtle
} from 'unrdf/knowledge-engine';

async function main() {
  // 1. Load data
  const data = readFileSync('./data/people.ttl', 'utf-8');
  const store = await parseTurtle(data);
  console.log(`Loaded ${store.size} triples\n`);

  // 2. Query relationships
  const relationships = await select(store, `
    PREFIX foaf: <http://xmlns.com/foaf/0.1/>
    SELECT ?person ?knows WHERE {
      ?person foaf:knows ?knows .
    }
  `);
  console.log('Relationships:', relationships.length);

  // 3. Validate structure
  const shapes = readFileSync('./data/schema.ttl', 'utf-8');
  const report = await validateShacl(store, shapes);
  console.log(`Validation: ${report.conforms ? 'PASSED' : 'FAILED'}\n`);

  // 4. Apply reasoning rules
  const rules = `
    @prefix foaf: <http://xmlns.com/foaf/0.1/> .
    @prefix ex: <http://example.org/> .

    { ?a foaf:knows ?b . ?b foaf:knows ?c . }
    => { ?a ex:mayKnow ?c . } .
  `;
  const inferred = await reason(store, rules);
  console.log(`After reasoning: ${inferred.size} triples`);

  // 5. Check inferred data
  const mayKnow = await ask(inferred, `
    PREFIX ex: <http://example.org/>
    ASK { ex:alice ex:mayKnow ?someone }
  `);
  console.log(`Alice may know someone new: ${mayKnow}`);

  // 6. Export result
  const output = await toTurtle(inferred);
  console.log('\nInferred graph (first 500 chars):');
  console.log(output.slice(0, 500) + '...');
}

main().catch(console.error);
```

## Troubleshooting

### "Cannot find module 'unrdf'"

Ensure you're using ES modules:

```json
{
  "type": "module"
}
```

### "Store is empty after parsing"

Check that you're awaiting the parse:

```javascript
// Wrong
const store = parseTurtle(data);

// Right
const store = await parseTurtle(data);
```

### "SPARQL query returns no results"

1. Verify prefixes match your data
2. Check for typos in IRIs
3. Log the store size to confirm data loaded

### "Validation fails unexpectedly"

1. Ensure shapes target the correct class
2. Check datatype constraints match your data
3. Use `formatValidationReport()` for details

---

**Need help?** See [Troubleshooting](./TROUBLESHOOTING.md) or [open an issue](https://github.com/unrdf/unrdf/issues).
