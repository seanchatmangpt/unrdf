# UNRDF v6 Quick Start Guide

**🎯 Goal**: Get productive with UNRDF v6 in 15 minutes

This hands-on tutorial walks you through building your first knowledge graph application with UNRDF v6, featuring receipt-driven operations and modern JavaScript patterns.

---

## Prerequisites

- **Node.js**: 18.0.0 or higher
- **Package Manager**: pnpm (recommended), npm, or yarn
- **Knowledge**: Basic JavaScript/Node.js
- **Time**: 15 minutes

---

## Step 1: Installation (2 minutes)

### Create a New Project

```bash
mkdir my-unrdf-app
cd my-unrdf-app

# Initialize package.json
pnpm init

# Add type: module for ES Modules
echo '{"type": "module"}' > package.json
```

### Install UNRDF v6

```bash
# Core packages
pnpm add @unrdf/core@6.0.0-alpha.1
pnpm add @unrdf/oxigraph@latest
pnpm add @unrdf/v6-core@6.0.0-alpha.1

# Validation
pnpm add zod@latest
```

### Verify Installation

```bash
# Create test file
echo 'import { createStore } from "@unrdf/oxigraph"; console.log("✓ UNRDF installed");' > test.mjs

# Run
node test.mjs
# Output: ✓ UNRDF installed
```

---

## Step 2: Your First Knowledge Graph (5 minutes)

### Create a Simple Graph

Create `app.mjs`:

```javascript
import { createStore } from '@unrdf/oxigraph';
import { dataFactory } from '@unrdf/core/rdf';

// Initialize store (Rust-backed, 10x faster than v5)
const store = await createStore();

// Create RDF quads using dataFactory
const { namedNode, literal, quad } = dataFactory;

// Add some triples about people
await store.add(
  quad(
    namedNode('http://example.org/Alice'),
    namedNode('http://xmlns.com/foaf/0.1/name'),
    literal('Alice Smith')
  )
);

await store.add(
  quad(
    namedNode('http://example.org/Alice'),
    namedNode('http://xmlns.com/foaf/0.1/knows'),
    namedNode('http://example.org/Bob')
  )
);

await store.add(
  quad(
    namedNode('http://example.org/Bob'),
    namedNode('http://xmlns.com/foaf/0.1/name'),
    literal('Bob Johnson')
  )
);

console.log('✅ Knowledge graph created with 3 triples');

// Query the graph
const quads = await store.match(null, null, null);
console.log(`📊 Total triples: ${quads.length}`);

// List all people
for (const q of quads) {
  if (q.predicate.value.includes('name')) {
    console.log(`👤 Person: ${q.object.value}`);
  }
}
```

### Run the Application

```bash
node app.mjs
```

**Expected Output**:
```
✅ Knowledge graph created with 3 triples
📊 Total triples: 3
👤 Person: Alice Smith
👤 Person: Bob Johnson
```

**✅ Checkpoint**: You've created your first RDF knowledge graph!

---

## Step 3: SPARQL Queries (3 minutes)

### Add SPARQL Query Support

Update `app.mjs`:

```javascript
import { createStore } from '@unrdf/oxigraph';
import { dataFactory } from '@unrdf/core/rdf';
import { executeSparql } from '@unrdf/core/sparql';

const store = await createStore();
const { namedNode, literal, quad } = dataFactory;

// Add triples
await store.add(
  quad(
    namedNode('http://example.org/Alice'),
    namedNode('http://xmlns.com/foaf/0.1/name'),
    literal('Alice Smith')
  )
);

await store.add(
  quad(
    namedNode('http://example.org/Alice'),
    namedNode('http://xmlns.com/foaf/0.1/knows'),
    namedNode('http://example.org/Bob')
  )
);

await store.add(
  quad(
    namedNode('http://example.org/Bob'),
    namedNode('http://xmlns.com/foaf/0.1/name'),
    literal('Bob Johnson')
  )
);

// SPARQL query: Find all people and their names
const query = `
  PREFIX foaf: <http://xmlns.com/foaf/0.1/>

  SELECT ?person ?name
  WHERE {
    ?person foaf:name ?name .
  }
`;

const results = await executeSparql(store, query);

console.log('\n🔍 Query Results:');
for (const binding of results) {
  const person = binding.get('person')?.value;
  const name = binding.get('name')?.value;
  console.log(`  ${name} (${person})`);
}
```

### Run with Queries

```bash
node app.mjs
```

**Expected Output**:
```
🔍 Query Results:
  Alice Smith (http://example.org/Alice)
  Bob Johnson (http://example.org/Bob)
```

**✅ Checkpoint**: You can now query graphs with SPARQL!

---

## Step 4: Receipt-Driven Operations (3 minutes)

### Add Receipt Generation

UNRDF v6's killer feature: every operation produces a cryptographic receipt.

Update `app.mjs`:

```javascript
import { createStore } from '@unrdf/oxigraph';
import { dataFactory } from '@unrdf/core/rdf';
import { createReceipt } from '@unrdf/v6-core/receipts';

const store = await createStore();
const { namedNode, literal, quad } = dataFactory;

// Wrap operation with receipt
async function addTripleWithReceipt(subject, predicate, object) {
  const triple = quad(
    namedNode(subject),
    namedNode(predicate),
    typeof object === 'string' ? literal(object) : namedNode(object)
  );

  await store.add(triple);

  // Generate receipt
  const receipt = createReceipt('add-triple', {
    subject,
    predicate,
    object
  });

  return receipt;
}

// Add triple with receipt
const receipt1 = await addTripleWithReceipt(
  'http://example.org/Alice',
  'http://xmlns.com/foaf/0.1/name',
  'Alice Smith'
);

console.log('📝 Receipt generated:');
console.log(`  ID: ${receipt1.id}`);
console.log(`  Operation: ${receipt1.operation}`);
console.log(`  Timestamp: ${receipt1.timestamp}`);
console.log(`  Merkle Root: ${receipt1.merkleRoot}`);

const receipt2 = await addTripleWithReceipt(
  'http://example.org/Alice',
  'http://xmlns.com/foaf/0.1/knows',
  'http://example.org/Bob'
);

console.log('\n✅ All operations have receipts for audit trail');
```

### Run with Receipts

```bash
node app.mjs
```

**Expected Output**:
```
📝 Receipt generated:
  ID: receipt-abc123...
  Operation: add-triple
  Timestamp: 2025-01-15T10:30:00.123Z
  Merkle Root: sha256:def456...

✅ All operations have receipts for audit trail
```

**✅ Checkpoint**: Your operations are now auditable and deterministic!

---

## Step 5: Input Validation with Zod (2 minutes)

### Add Runtime Type Safety

UNRDF v6 enforces Zod validation for all inputs.

Update `app.mjs`:

```javascript
import { z } from 'zod';
import { createStore } from '@unrdf/oxigraph';
import { dataFactory } from '@unrdf/core/rdf';
import { createReceipt } from '@unrdf/v6-core/receipts';

const store = await createStore();
const { namedNode, literal, quad } = dataFactory;

// Define Zod schema
const TripleSchema = z.object({
  subject: z.string().url(),
  predicate: z.string().url(),
  object: z.union([z.string().url(), z.string().min(1)])
});

// Validated function
async function addTripleWithReceipt(data) {
  // Validate input (throws ZodError if invalid)
  const validated = TripleSchema.parse(data);

  const triple = quad(
    namedNode(validated.subject),
    namedNode(validated.predicate),
    typeof validated.object === 'string' && !validated.object.startsWith('http')
      ? literal(validated.object)
      : namedNode(validated.object)
  );

  await store.add(triple);

  const receipt = createReceipt('add-triple', validated);
  return receipt;
}

// Valid input
try {
  const receipt = await addTripleWithReceipt({
    subject: 'http://example.org/Alice',
    predicate: 'http://xmlns.com/foaf/0.1/name',
    object: 'Alice Smith'
  });
  console.log('✅ Valid input accepted');
} catch (error) {
  console.error('❌ Validation error:', error.message);
}

// Invalid input (will throw)
try {
  await addTripleWithReceipt({
    subject: 'invalid-url', // Not a valid URL
    predicate: 'http://xmlns.com/foaf/0.1/name',
    object: 'Bob'
  });
} catch (error) {
  console.error('❌ Validation caught:', error.errors[0].message);
}

console.log('\n✅ Runtime type safety enforced with Zod');
```

### Run with Validation

```bash
node app.mjs
```

**Expected Output**:
```
✅ Valid input accepted
❌ Validation caught: Invalid url

✅ Runtime type safety enforced with Zod
```

**✅ Checkpoint**: Your APIs are now type-safe at runtime!

---

## Complete Example

Here's the final `app.mjs` with all features:

```javascript
/**
 * UNRDF v6 Quick Start Example
 * Demonstrates: Store creation, SPARQL queries, receipts, and validation
 */

import { z } from 'zod';
import { createStore } from '@unrdf/oxigraph';
import { dataFactory } from '@unrdf/core/rdf';
import { executeSparql } from '@unrdf/core/sparql';
import { createReceipt } from '@unrdf/v6-core/receipts';

// --- Configuration ---

const TripleSchema = z.object({
  subject: z.string().url(),
  predicate: z.string().url(),
  object: z.string().min(1)
});

// --- Initialize Store ---

const store = await createStore();
const { namedNode, literal, quad } = dataFactory;

console.log('🚀 UNRDF v6 Quick Start\n');

// --- Add Data with Receipts ---

async function addTriple(data) {
  const validated = TripleSchema.parse(data);

  const triple = quad(
    namedNode(validated.subject),
    namedNode(validated.predicate),
    typeof validated.object === 'string' && !validated.object.startsWith('http')
      ? literal(validated.object)
      : namedNode(validated.object)
  );

  await store.add(triple);

  return createReceipt('add-triple', validated);
}

// Add sample data
const receipts = [];

receipts.push(await addTriple({
  subject: 'http://example.org/Alice',
  predicate: 'http://xmlns.com/foaf/0.1/name',
  object: 'Alice Smith'
}));

receipts.push(await addTriple({
  subject: 'http://example.org/Alice',
  predicate: 'http://xmlns.com/foaf/0.1/knows',
  object: 'http://example.org/Bob'
}));

receipts.push(await addTriple({
  subject: 'http://example.org/Bob',
  predicate: 'http://xmlns.com/foaf/0.1/name',
  object: 'Bob Johnson'
}));

console.log(`✅ Added ${receipts.length} triples with receipts\n`);

// --- Query with SPARQL ---

const query = `
  PREFIX foaf: <http://xmlns.com/foaf/0.1/>

  SELECT ?person ?name
  WHERE {
    ?person foaf:name ?name .
  }
`;

const results = await executeSparql(store, query);

console.log('🔍 Query Results:');
for (const binding of results) {
  const name = binding.get('name')?.value;
  console.log(`  👤 ${name}`);
}

// --- Show Receipts ---

console.log('\n📝 Operation Receipts:');
for (const receipt of receipts) {
  console.log(`  - ${receipt.id}: ${receipt.operation}`);
}

console.log('\n✨ Done! You\'ve built your first UNRDF v6 application.\n');
```

### Run Complete Example

```bash
node app.mjs
```

**Expected Output**:
```
🚀 UNRDF v6 Quick Start

✅ Added 3 triples with receipts

🔍 Query Results:
  👤 Alice Smith
  👤 Bob Johnson

📝 Operation Receipts:
  - receipt-abc123: add-triple
  - receipt-def456: add-triple
  - receipt-ghi789: add-triple

✨ Done! You've built your first UNRDF v6 application.
```

---

## Next Steps

🎉 **Congratulations!** You've completed the UNRDF v6 quick start.

### Learn More

- **[Advanced Patterns](/home/user/unrdf/docs/v6/ADVANCED-PATTERNS.md)** - Delta proposals, streaming, federation
- **[API Reference](/home/user/unrdf/docs/v6/API-REFERENCE.md)** - Complete API documentation
- **[Migration Guide](/home/user/unrdf/docs/v6/MIGRATION_PLAN.md)** - Upgrade from v5
- **[Examples](/home/user/unrdf/examples/)** - More code examples

### Common Tasks

- **Build a Blog**: Store posts and authors in RDF
- **Semantic Search**: Query related concepts
- **Event Sourcing**: Track all changes with receipts
- **Time Travel**: Replay operations from receipts

### Get Help

- **GitHub Issues**: [unrdf/unrdf/issues](https://github.com/unrdf/unrdf/issues)
- **Discussions**: [unrdf/unrdf/discussions](https://github.com/unrdf/unrdf/discussions)
- **Documentation**: [/docs](/home/user/unrdf/docs/)

---

## Troubleshooting

### Common Issues

**Q: `SyntaxError: Cannot use import statement outside a module`**

```bash
# Add "type": "module" to package.json
echo '{"type": "module"}' > package.json
```

**Q: `Error: Cannot find module '@unrdf/oxigraph'`**

```bash
# Install dependencies
pnpm install
```

**Q: `ZodError: Invalid url`**

```javascript
// Ensure URLs are valid
const data = {
  subject: 'http://example.org/Alice', // ✅ Valid
  // subject: 'invalid-url', // ❌ Invalid
  predicate: 'http://xmlns.com/foaf/0.1/name',
  object: 'Alice'
};
```

**Q: Store operations fail**

```javascript
// Ensure store is awaited
const store = await createStore(); // ✅
// const store = createStore(); // ❌ Missing await
```

---

## Code Checklist

Before moving on, verify:

- [ ] Node.js 18+ installed
- [ ] UNRDF v6 packages installed
- [ ] Example runs without errors
- [ ] SPARQL query returns results
- [ ] Receipts are generated
- [ ] Zod validation works

---

**Ready for more?** → [Advanced Patterns Guide](/home/user/unrdf/docs/v6/ADVANCED-PATTERNS.md) 🚀
