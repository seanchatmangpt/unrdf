# Tutorial 2: Working with Events

In this tutorial, you'll learn how KGC 4D tracks changes through an immutable event log. Events are the foundation of time travel and verification features.

**Time:** ~20 minutes
**Prerequisites:** [Tutorial 1: Getting Started](./01-getting-started.md)
**Goals:**
- Understand the event model
- Create events with payloads
- Append events with mutations
- Query event history

## Understanding Events

In KGC 4D, every change to your knowledge graph is recorded as an **event**. Events are stored in an immutable append-only log, which allows you to:

- Track *who* changed *what* and *when*
- Reconstruct the state at any point in time
- Verify the integrity of changes
- Audit all mutations

An event consists of:
- **Type**: CREATE, UPDATE, DELETE, SNAPSHOT, or HOOK_EXECUTION
- **Timestamp**: When the change occurred (nanosecond precision)
- **Payload**: Arbitrary metadata about the change
- **Mutations**: The actual RDF changes (quads to add/remove)
- **Vector Clock**: Causality tracking for distributed systems

## Step 1: Create Your First Event

Instead of directly adding quads, we'll use `appendEvent()` to record a change:

```javascript
import { KGCStore, EVENT_TYPES } from '@unrdf/kgc-4d';
import { dataFactory } from '@unrdf/oxigraph';

const store = new KGCStore();

// Define your entities
const alice = dataFactory.namedNode('http://example.org/alice');
const name = dataFactory.namedNode('http://example.org/name');
const rdfType = dataFactory.namedNode('http://www.w3.org/1999/02/22-rdf-syntax-ns#type');
const person = dataFactory.namedNode('http://example.org/Person');

// Create an event with metadata
const eventPayload = {
  description: 'Created Alice',
  source: 'tutorial',
  userId: 'user-123',
};

// Define mutations (quads to add)
const mutations = [
  {
    type: 'add',
    subject: alice,
    predicate: rdfType,
    object: person,
  },
  {
    type: 'add',
    subject: alice,
    predicate: name,
    object: dataFactory.literal('Alice'),
  },
];

// Append the event atomically
const receipt = await store.appendEvent(
  { type: EVENT_TYPES.CREATE, payload: eventPayload },
  mutations
);

console.log('✓ Event appended');
console.log('  Event ID:', receipt.eventId);
console.log('  Timestamp:', receipt.tNs);
```

## Step 2: Append Multiple Events

Let's create a sequence of changes:

```javascript
import { KGCStore, EVENT_TYPES } from '@unrdf/kgc-4d';
import { dataFactory } from '@unrdf/oxigraph';

const store = new KGCStore();

const alice = dataFactory.namedNode('http://example.org/alice');
const bob = dataFactory.namedNode('http://example.org/bob');
const name = dataFactory.namedNode('http://example.org/name');
const age = dataFactory.namedNode('http://example.org/age');
const rdfType = dataFactory.namedNode('http://www.w3.org/1999/02/22-rdf-syntax-ns#type');
const person = dataFactory.namedNode('http://example.org/Person');

// Event 1: Create Alice
await store.appendEvent(
  { type: EVENT_TYPES.CREATE, payload: { description: 'Created Alice' } },
  [
    { type: 'add', subject: alice, predicate: rdfType, object: person },
    { type: 'add', subject: alice, predicate: name, object: dataFactory.literal('Alice') },
  ]
);
console.log('✓ Event 1: Alice created');

// Event 2: Update Alice's age
await store.appendEvent(
  { type: EVENT_TYPES.UPDATE, payload: { description: 'Set Alice age' } },
  [
    { type: 'add', subject: alice, predicate: age, object: dataFactory.literal('30') },
  ]
);
console.log('✓ Event 2: Alice age updated');

// Event 3: Create Bob
await store.appendEvent(
  { type: EVENT_TYPES.CREATE, payload: { description: 'Created Bob' } },
  [
    { type: 'add', subject: bob, predicate: rdfType, object: person },
    { type: 'add', subject: bob, predicate: name, object: dataFactory.literal('Bob') },
  ]
);
console.log('✓ Event 3: Bob created');

// Query current state
const query = `
  PREFIX ex: <http://example.org/>
  SELECT ?person ?name ?age
  WHERE {
    GRAPH <kgc:Universe> {
      ?person a ex:Person ;
              ex:name ?name .
      OPTIONAL { ?person ex:age ?age }
    }
  }
`;

console.log('\nCurrent state:');
store.querySync(query).forEach(binding => {
  const p = binding.get('person').value.split('/').pop();
  const n = binding.get('name').value;
  const a = binding.get('age')?.value || 'unknown';
  console.log(`  ${p}: ${n} (age: ${a})`);
});
```

Output:
```
✓ Event 1: Alice created
✓ Event 2: Alice age updated
✓ Event 3: Bob created

Current state:
  alice: Alice (age: 30)
  bob: Bob (age: unknown)
```

## Step 3: Query the Event Log

Events are stored in the `EventLog` graph. You can query the history:

```javascript
import { KGCStore, EVENT_TYPES, GRAPHS } from '@unrdf/kgc-4d';
import { dataFactory } from '@unrdf/oxigraph';

const store = new KGCStore();

// ... (append events from previous step)

// Query all events
const eventQuery = `
  PREFIX kgc: <kgc:>
  SELECT ?eventId ?type ?timestamp
  WHERE {
    GRAPH <kgc:EventLog> {
      ?eventId kgc:type ?type ;
               kgc:T_NS ?timestamp .
    }
  }
  ORDER BY ?timestamp
`;

console.log('\nEvent history:');
store.querySync(eventQuery).forEach(binding => {
  const id = binding.get('eventId').value.split('#')[1] || binding.get('eventId').value;
  const type = binding.get('type').value;
  const ts = binding.get('timestamp').value;
  console.log(`  ${id}: ${type} at ${ts}`);
});
```

## Step 4: Inspect Event Payloads

Get detailed information about each event:

```javascript
// Query events with their payloads
const detailedQuery = `
  PREFIX kgc: <kgc:>
  SELECT ?eventId ?type ?payload
  WHERE {
    GRAPH <kgc:EventLog> {
      ?eventId kgc:type ?type ;
               kgc:payload ?payload .
    }
  }
  ORDER BY ?eventId
`;

console.log('\nDetailed events:');
store.querySync(detailedQuery).forEach(binding => {
  const id = binding.get('eventId').value;
  const type = binding.get('type').value;
  const payload = binding.get('payload').value;

  // Parse JSON payload
  const data = JSON.parse(payload);
  console.log(`  Event: ${type}`);
  console.log(`    Description: ${data.description}`);
  console.log(`    Source: ${data.source}`);
});
```

## Step 5: Handle Deletions

Events can also represent deletions:

```javascript
import { KGCStore, EVENT_TYPES } from '@unrdf/kgc-4d';
import { dataFactory } from '@unrdf/oxigraph';

const store = new KGCStore();

const alice = dataFactory.namedNode('http://example.org/alice');
const age = dataFactory.namedNode('http://example.org/age');

// ... (create events)

// Later: Delete Alice's age
await store.appendEvent(
  { type: EVENT_TYPES.DELETE, payload: { description: 'Removed age due to privacy' } },
  [
    { type: 'delete', subject: alice, predicate: age, object: dataFactory.literal('30') },
  ]
);

console.log('✓ Event: Alice age deleted');
```

## Understanding Event Types

| Type | Usage |
|------|-------|
| **CREATE** | Creating new entities or adding new triples |
| **UPDATE** | Modifying existing values |
| **DELETE** | Removing triples or entities |
| **SNAPSHOT** | Recording a frozen universe checkpoint |
| **HOOK_EXECUTION** | Tracking knowledge hook executions |

## Key Guarantees

KGC 4D provides **ACID semantics** for events:

- **Atomic**: All mutations in an event succeed or all fail together
- **Consistent**: The store remains in a valid RDF state
- **Isolated**: Events don't interfere with concurrent operations
- **Durable**: Events are persisted in the event log

## Summary

You've learned:
- ✓ Create events with metadata
- ✓ Append events with mutations
- ✓ Query the immutable event log
- ✓ Handle CREATE, UPDATE, and DELETE operations
- ✓ Understand ACID guarantees

## Next Steps

Now that you understand events, move to [Tutorial 3: Temporal Snapshots](./03-temporal-snapshots.md) to learn how to freeze state and travel through time.

## Key Concepts

| Term | Meaning |
|------|---------|
| **Event** | A recorded change with timestamp, type, payload, and mutations |
| **Event Log** | Immutable append-only history of all events |
| **Mutation** | A specific RDF change (add or delete a quad) |
| **Receipt** | The result of appending an event (eventId, timestamp, vector clock) |
| **Atomic** | All mutations in an event succeed or fail together |
