# UNRDF Examples

Collection of real-world examples demonstrating UNRDF capabilities.

## Table of Contents

1. [Basic Examples](#basic-examples)
2. [Knowledge Management](#knowledge-management)
3. [Advanced Queries](#advanced-queries)
4. [Validation Examples](#validation-examples)
5. [Transactions](#transactions)
6. [Knowledge Hooks](#knowledge-hooks)
7. [Streaming](#streaming)
8. [Federation](#federation)

---

## Basic Examples

### Example 1: Simple Graph Creation and Query

```javascript
import { createKnowledgeSubstrateCore } from '@unrdf/core';

const core = await createKnowledgeSubstrateCore();

// Create store from Turtle
const store = await core.parseRdf(`
  @prefix ex: <http://example.org/> .
  @prefix foaf: <http://xmlns.com/foaf/0.1/> .

  ex:alice a foaf:Person ; foaf:name "Alice" .
  ex:bob a foaf:Person ; foaf:name "Bob" .
  ex:alice foaf:knows ex:bob .
`);

// Query
const results = await core.query(store, `
  SELECT ?name WHERE {
    ?person foaf:name ?name .
  }
`);

results.forEach(row => console.log(row.get('name').value));
```

### Example 2: Add and Query Dynamic Data

```javascript
import { namedNode, literal } from '@rdfjs/data-model';

// Add triples dynamically
store.addQuad(
  namedNode('http://example.org/charlie'),
  namedNode('http://www.w3.org/1999/02/22-rdf-syntax-ns#type'),
  namedNode('http://xmlns.com/foaf/0.1/Person')
);

store.addQuad(
  namedNode('http://example.org/charlie'),
  namedNode('http://xmlns.com/foaf/0.1/name'),
  literal('Charlie')
);

// Query again - now includes Charlie
const results = await core.query(store, `SELECT ?name WHERE { ?person foaf:name ?name }`);
```

---

## Knowledge Management

### Example 3: Employee Directory

```javascript
const storeData = `
  @prefix emp: <http://example.org/employee/> .
  @prefix foaf: <http://xmlns.com/foaf/0.1/> .
  @prefix org: <http://www.w3.org/ns/org#> .
  @prefix vcard: <http://www.w3.org/2006/vcard/ns#> .

  emp:alice a foaf:Person ;
    foaf:name "Alice Johnson" ;
    org:workEmail "alice@company.com" ;
    org:department emp:engineering ;
    org:manager emp:carol .

  emp:bob a foaf:Person ;
    foaf:name "Bob Smith" ;
    org:workEmail "bob@company.com" ;
    org:department emp:sales ;
    org:manager emp:carol .

  emp:carol a foaf:Person ;
    foaf:name "Carol White" ;
    org:workEmail "carol@company.com" ;
    org:department emp:management .

  emp:engineering org:hasName "Engineering" .
  emp:sales org:hasName "Sales" .
  emp:management org:hasName "Management" .
`;

const store = await core.parseRdf(storeData);

// Find all employees in a department
const engineeringTeam = await core.query(store, `
  PREFIX emp: <http://example.org/employee/>
  PREFIX foaf: <http://xmlns.com/foaf/0.1/>
  PREFIX org: <http://www.w3.org/ns/org#>

  SELECT ?name ?email ?manager
  WHERE {
    ?person a foaf:Person ;
            foaf:name ?name ;
            org:workEmail ?email ;
            org:department emp:engineering ;
            org:manager ?mgr .
    ?mgr foaf:name ?manager .
  }
`);

engineeringTeam.forEach(row => {
  console.log(`${row.get('name').value} (${row.get('email').value}) reports to ${row.get('manager').value}`);
});
```

### Example 4: Organizational Hierarchy

```javascript
// Find reporting chains
const hierarchy = await core.query(store, `
  PREFIX foaf: <http://xmlns.com/foaf/0.1/>
  PREFIX org: <http://www.w3.org/ns/org#>

  SELECT ?employee ?manager ?director
  WHERE {
    ?emp foaf:name ?employee ;
         org:manager ?m .
    ?m foaf:name ?manager ;
       org:manager ?d .
    ?d foaf:name ?director .
  }
`);

hierarchy.forEach(row => {
  console.log(`${row.get('employee').value} → ${row.get('manager').value} → ${row.get('director').value}`);
});
```

---

## Advanced Queries

### Example 5: SPARQL Aggregation

```javascript
// Count employees per department
const deptStats = await core.query(store, `
  PREFIX org: <http://www.w3.org/ns/org#>

  SELECT ?department (COUNT(?person) AS ?count)
  WHERE {
    ?person org:department ?dept .
    ?dept org:hasName ?department .
  }
  GROUP BY ?department
  ORDER BY DESC(?count)
`);

deptStats.forEach(row => {
  console.log(`${row.get('department').value}: ${row.get('count').value} employees`);
});
```

### Example 6: SPARQL UNION

```javascript
// Find all contacts (email OR phone)
const contacts = await core.query(store, `
  PREFIX foaf: <http://xmlns.com/foaf/0.1/>
  PREFIX vcard: <http://www.w3.org/2006/vcard/ns#>

  SELECT ?person ?contact
  WHERE {
    ?person foaf:name ?name .
    {
      ?person foaf:mbox ?contact .
    } UNION {
      ?person vcard:hasPhone ?contact .
    }
  }
`);
```

### Example 7: Optional Patterns

```javascript
// Find people with optional phone numbers
const people = await core.query(store, `
  PREFIX foaf: <http://xmlns.com/foaf/0.1/>
  PREFIX vcard: <http://www.w3.org/2006/vcard/ns#>

  SELECT ?name ?phone
  WHERE {
    ?person foaf:name ?name .
    OPTIONAL { ?person vcard:hasPhone ?phone . }
  }
`);

people.forEach(row => {
  const phone = row.has('phone') ? row.get('phone').value : '(no phone)';
  console.log(`${row.get('name').value}: ${phone}`);
});
```

---

## Validation Examples

### Example 8: Basic SHACL Validation

```javascript
const shapes = `
  @prefix sh: <http://www.w3.org/ns/shacl#> .
  @prefix foaf: <http://xmlns.com/foaf/0.1/> .

  foaf:PersonShape a sh:NodeShape ;
    sh:targetClass foaf:Person ;
    sh:property [
      sh:path foaf:name ;
      sh:minCount 1 ;
      sh:datatype xsd:string ;
      sh:message "Person must have a name" ;
    ] ;
    sh:property [
      sh:path foaf:mbox ;
      sh:maxCount 1 ;
      sh:message "Person can have at most one email" ;
    ] .
`;

const shapesStore = await core.parseRdf(shapes);
const report = await core.validateShacl(store, shapesStore);

if (!report.conforms) {
  console.log('Validation errors:');
  report.results.forEach(result => {
    console.log(`  - ${result.message}`);
  });
}
```

### Example 9: Constraint Validation

```javascript
// Validate numeric constraints
const constraintShapes = `
  @prefix sh: <http://www.w3.org/ns/shacl#> .
  @prefix schema: <http://schema.org/> .

  schema:ProductShape a sh:NodeShape ;
    sh:targetClass schema:Product ;
    sh:property [
      sh:path schema:price ;
      sh:datatype xsd:decimal ;
      sh:minInclusive 0 ;
      sh:maxInclusive 99999.99 ;
    ] .
`;
```

---

## Transactions

### Example 10: Atomic Updates

```javascript
// Transfer employee between departments (atomic)
const tx = await core.beginTransaction();

try {
  // Remove from old department
  store.deleteQuad(
    namedNode('http://example.org/employee/alice'),
    namedNode('http://www.w3.org/ns/org#department'),
    namedNode('http://example.org/employee/engineering')
  );

  // Add to new department
  store.addQuad(
    namedNode('http://example.org/employee/alice'),
    namedNode('http://www.w3.org/ns/org#department'),
    namedNode('http://example.org/employee/management')
  );

  await tx.commit();
  console.log('✓ Transfer completed');
} catch (error) {
  await tx.rollback();
  console.log('✗ Transfer failed, rolled back');
}
```

### Example 11: Batch Operations

```javascript
const tx = await core.beginTransaction();

try {
  const newEmployees = [
    { id: 'diana', name: 'Diana' },
    { id: 'eve', name: 'Eve' },
    { id: 'frank', name: 'Frank' }
  ];

  for (const emp of newEmployees) {
    store.addQuad(
      namedNode(`http://example.org/employee/${emp.id}`),
      namedNode('http://www.w3.org/1999/02/22-rdf-syntax-ns#type'),
      namedNode('http://xmlns.com/foaf/0.1/Person')
    );
    store.addQuad(
      namedNode(`http://example.org/employee/${emp.id}`),
      namedNode('http://xmlns.com/foaf/0.1/name'),
      literal(emp.name)
    );
  }

  await tx.commit();
  console.log('✓ Added 3 employees');
} catch (error) {
  await tx.rollback();
  console.log('✗ Rollback - no employees added');
}
```

---

## Knowledge Hooks

### Example 12: Auto-Logging

```javascript
import { defineHook, registerHook } from '@unrdf/hooks';

const auditLog = defineHook({
  meta: { name: 'audit-log' },
  trigger: 'INSERT',

  run(event) {
    const timestamp = new Date().toISOString();
    console.log(`[${timestamp}] Added: ${event.quad.subject.value}`);
  }
});

registerHook(auditLog);

// Any addition to the store will now be logged
store.addQuad(...);  // Logs automatically
```

### Example 13: Data Enrichment

```javascript
const enricher = defineHook({
  meta: { name: 'auto-enrich' },
  trigger: 'INSERT',
  pattern: '?person foaf:name ?name .',

  run(event) {
    // Add a creation timestamp when a person is added
    const now = new Date().toISOString();
    store.addQuad(
      event.quad.subject,
      namedNode('http://schema.org/dateCreated'),
      literal(now)
    );
  }
});

registerHook(enricher);
```

### Example 14: Validation Hook

```javascript
const validator = defineHook({
  meta: { name: 'email-validator' },
  trigger: 'INSERT',
  pattern: '?person foaf:mbox ?email .',

  before(event) {
    // Reject invalid emails
    const email = event.quad.object.value;
    const isValid = /^[^\s@]+@[^\s@]+\.[^\s@]+$/.test(email);
    if (!isValid) {
      console.log(`✗ Invalid email rejected: ${email}`);
      return false;  // Prevent hook
    }
    return true;
  }
});

registerHook(validator);
```

---

## Streaming

### Example 15: Process Large Files

```javascript
import { createReadStream } from '@unrdf/streaming';

const largeStore = ...; // Large store loaded from file

const stream = createReadStream(largeStore);

let count = 0;
stream.on('data', (quad) => {
  count++;
  if (count % 1000000 === 0) {
    console.log(`Processed ${count} triples...`);
  }
});

stream.on('end', () => {
  console.log(`Total: ${count} triples`);
});
```

### Example 16: Transform Stream

```javascript
import { createReadStream, createWriteStream } from '@unrdf/streaming';

const input = createReadStream(inputStore);
const output = createWriteStream(outputStore);

// Filter: only keep triples about people
input.on('data', (quad) => {
  if (quad.predicate.value.includes('person') ||
      quad.predicate.value.includes('foaf')) {
    output.write(quad);
  }
});

input.on('end', () => output.end());
```

---

## Federation

### Example 17: Query Multiple Stores

```javascript
import { createFederatedStore } from '@unrdf/federation';

// Local stores
const storeA = await core.parseRdf(dataA);
const storeB = await core.parseRdf(dataB);

// Remote endpoint
const remoteStore = {
  query: async (sparql) => {
    const res = await fetch('https://data.example.org/sparql', {
      method: 'POST',
      body: new URLSearchParams({ query: sparql })
    });
    return res.json();
  }
};

// Federate
const fedStore = createFederatedStore([storeA, storeB, remoteStore]);

// Query across all sources
const results = await core.query(fedStore, `
  SELECT ?name WHERE {
    ?person a foaf:Person ; foaf:name ?name .
  }
`);
```

---

## Complete Project Example

See the **[GETTING-STARTED/QUICK-START.md](GETTING-STARTED/QUICK-START.md)** for a complete runnable example project with:

- File-based RDF data
- Multiple queries
- SHACL validation
- Transactions
- Knowledge Hooks

---

## Running Examples

All examples are runnable. To try one:

1. Create an `.mjs` file with the example code
2. Add `@unrdf/core` to your `package.json`
3. Run with `node example.mjs`

```bash
# Setup
npm init -y
npm install @unrdf/core

# Run example
node example.mjs
```

---

**More examples?** Check the [examples/](../examples/) directory in the repository.

**API Questions?** See [API-REFERENCE.md](API-REFERENCE.md).

**Getting started?** See [GETTING-STARTED/QUICK-START.md](GETTING-STARTED/QUICK-START.md).
