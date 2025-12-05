# Tutorial 3: Temporal Snapshots

In this tutorial, you'll learn how to freeze your knowledge graph, store it in Git, and travel back to any point in time. This is where KGC 4D's "4th dimension" comes alive.

**Time:** ~25 minutes
**Prerequisites:** [Tutorial 2: Working with Events](./02-working-with-events.md)
**Goals:**
- Freeze the universe to a Git snapshot
- Reconstruct state at a historical point
- Verify snapshot integrity
- Understand the zero-information invariant

## The Zero-Information Invariant

KGC 4D is built on a fundamental principle: **All state at any time is reconstructible from Event Log + Git snapshots.**

This means:
- You never need to store state separately
- You can always go back to any point in time
- You can verify that nothing was tampered with
- Snapshots are optional optimizations (not the source of truth)

## Step 1: Create a Git Repository

First, set up a Git repository to store snapshots:

```bash
mkdir kgc-demo
cd kgc-demo
git init
```

## Step 2: Freeze the Universe

Let's create some events and then freeze the universe:

```javascript
import { KGCStore, GitBackbone, freezeUniverse } from '@unrdf/kgc-4d';
import { dataFactory } from '@unrdf/oxigraph';

const store = new KGCStore();
const git = new GitBackbone('./repo');

const alice = dataFactory.namedNode('http://example.org/alice');
const name = dataFactory.namedNode('http://example.org/name');
const rdfType = dataFactory.namedNode('http://www.w3.org/1999/02/22-rdf-syntax-ns#type');
const person = dataFactory.namedNode('http://example.org/Person');

// Create events
await store.appendEvent(
  { type: 'CREATE', payload: { description: 'Initial data' } },
  [
    { type: 'add', subject: alice, predicate: rdfType, object: person },
    { type: 'add', subject: alice, predicate: name, object: dataFactory.literal('Alice') },
  ]
);

// Freeze the universe to Git
const frozen = await freezeUniverse(store, git);

console.log('✓ Universe frozen');
console.log('  Git ref:', frozen.gitRef);
console.log('  BLAKE3 hash:', frozen.hash);
console.log('  Snapshot ID:', frozen.snapshotId);
```

The `freezeUniverse()` function:
1. Exports the Universe graph to N-Quads format
2. Computes a BLAKE3 hash of the snapshot
3. Creates a Git commit with the N-Quads content
4. Stores the hash and Git reference as proof

## Step 3: Modify Data and Create Another Snapshot

Let's make changes and create a second snapshot:

```javascript
import { KGCStore, GitBackbone, freezeUniverse } from '@unrdf/kgc-4d';
import { dataFactory } from '@unrdf/oxigraph';

const store = new KGCStore();
const git = new GitBackbone('./repo');

const alice = dataFactory.namedNode('http://example.org/alice');
const bob = dataFactory.namedNode('http://example.org/bob');
const name = dataFactory.namedNode('http://example.org/name');
const age = dataFactory.namedNode('http://example.org/age');
const rdfType = dataFactory.namedNode('http://www.w3.org/1999/02/22-rdf-syntax-ns#type');
const person = dataFactory.namedNode('http://example.org/Person');

// Event 1: Create Alice
await store.appendEvent(
  { type: 'CREATE', payload: { description: 'Initial data' } },
  [
    { type: 'add', subject: alice, predicate: rdfType, object: person },
    { type: 'add', subject: alice, predicate: name, object: dataFactory.literal('Alice') },
  ]
);

// Snapshot 1
console.log('📸 Snapshot 1:');
const frozen1 = await freezeUniverse(store, git);
console.log('  Hash:', frozen1.hash);

// Event 2: Update Alice's age
await store.appendEvent(
  { type: 'UPDATE', payload: { description: 'Added age' } },
  [
    { type: 'add', subject: alice, predicate: age, object: dataFactory.literal('30') },
  ]
);

// Snapshot 2
console.log('📸 Snapshot 2:');
const frozen2 = await freezeUniverse(store, git);
console.log('  Hash:', frozen2.hash);

// Event 3: Create Bob
await store.appendEvent(
  { type: 'CREATE', payload: { description: 'Created Bob' } },
  [
    { type: 'add', subject: bob, predicate: rdfType, object: person },
    { type: 'add', subject: bob, predicate: name, object: dataFactory.literal('Bob') },
  ]
);

// Snapshot 3
console.log('📸 Snapshot 3:');
const frozen3 = await freezeUniverse(store, git);
console.log('  Hash:', frozen3.hash);

console.log('\n✓ Created 3 snapshots');
console.log(`  Snapshot 1: ${frozen1.hash.slice(0, 8)}...`);
console.log(`  Snapshot 2: ${frozen2.hash.slice(0, 8)}...`);
console.log(`  Snapshot 3: ${frozen3.hash.slice(0, 8)}...`);
```

Each snapshot has a unique hash because the data changed.

## Step 4: Time Travel

Now let's reconstruct the state at each point in time:

```javascript
import { KGCStore, GitBackbone, freezeUniverse, reconstructState } from '@unrdf/kgc-4d';
import { dataFactory } from '@unrdf/oxigraph';

const store = new KGCStore();
const git = new GitBackbone('./repo');

// ... (create 3 snapshots)

// Query helper
function queryPeople(store) {
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
  return store.querySync(query);
}

// Current state: Alice and Bob exist
console.log('Current state:');
queryPeople(store).forEach(binding => {
  console.log(`  ${binding.get('person').value.split('/').pop()}: ${binding.get('name').value}`);
});

// Time travel to snapshot 1 (only Alice exists)
console.log('\nState at Snapshot 1:');
const pastStore1 = await reconstructState(store, git, frozen1.tNs);
queryPeople(pastStore1).forEach(binding => {
  console.log(`  ${binding.get('person').value.split('/').pop()}: ${binding.get('name').value}`);
});

// Time travel to snapshot 2 (Alice with age)
console.log('\nState at Snapshot 2:');
const pastStore2 = await reconstructState(store, git, frozen2.tNs);
queryPeople(pastStore2).forEach(binding => {
  const person = binding.get('person').value.split('/').pop();
  const name = binding.get('name').value;
  const age = binding.get('age')?.value || 'unknown';
  console.log(`  ${person}: ${name} (age: ${age})`);
});
```

The `reconstructState()` function:
1. Finds the nearest snapshot before the target time
2. Loads that snapshot from Git
3. Replays all events after the snapshot
4. Returns a new store with the historical state

## Step 5: Verify Snapshot Integrity

Ensure that a snapshot hasn't been tampered with:

```javascript
import { KGCStore, GitBackbone, freezeUniverse, verifyReceipt } from '@unrdf/kgc-4d';
import { dataFactory } from '@unrdf/oxigraph';

const store = new KGCStore();
const git = new GitBackbone('./repo');

// ... (create snapshot)

// Verify that the snapshot is authentic
const isValid = await verifyReceipt(frozen1, git, store);

if (isValid) {
  console.log('✓ Snapshot integrity verified');
  console.log(`  BLAKE3 hash matches Git content`);
} else {
  console.log('✗ Snapshot verification failed!');
  console.log(`  Data may have been tampered with`);
}
```

The `verifyReceipt()` function:
1. Fetches the snapshot from Git using the stored ref
2. Recomputes the BLAKE3 hash
3. Compares it against the stored hash
4. Returns true if they match, false if tampered with

## Step 6: Understanding the Flow

Here's a visual representation:

```
Event 1 → Event 2 → Event 3 → Event 4
  ↓        ↓          ↓
[Snapshot 1] [Snapshot 2]  [Current]
  ↓          ↓
 Git       Git

Time Travel:
- Target time = T2
- Reconstruct from Snapshot 1
- Replay Events 2 and 3
- Result: Store at time T2
```

## Step 7: Complete Example

Here's a full working example:

```javascript
import { KGCStore, GitBackbone, freezeUniverse, reconstructState } from '@unrdf/kgc-4d';
import { dataFactory } from '@unrdf/oxigraph';

async function main() {
  const store = new KGCStore();
  const git = new GitBackbone('./demo-repo');

  const alice = dataFactory.namedNode('http://example.org/alice');
  const name = dataFactory.namedNode('http://example.org/name');
  const city = dataFactory.namedNode('http://example.org/city');
  const rdfType = dataFactory.namedNode('http://www.w3.org/1999/02/22-rdf-syntax-ns#type');
  const person = dataFactory.namedNode('http://example.org/Person');

  // Event 1: Create Alice
  const r1 = await store.appendEvent(
    { type: 'CREATE', payload: { description: 'Create Alice' } },
    [
      { type: 'add', subject: alice, predicate: rdfType, object: person },
      { type: 'add', subject: alice, predicate: name, object: dataFactory.literal('Alice') },
    ]
  );
  const frozen1 = await freezeUniverse(store, git);
  console.log('Snapshot 1:', frozen1.hash.slice(0, 16));

  // Event 2: Add city
  await store.appendEvent(
    { type: 'UPDATE', payload: { description: 'Add city' } },
    [
      { type: 'add', subject: alice, predicate: city, object: dataFactory.literal('NYC') },
    ]
  );
  const frozen2 = await freezeUniverse(store, git);
  console.log('Snapshot 2:', frozen2.hash.slice(0, 16));

  // Verify snapshot
  const valid = await verifyReceipt(frozen2, git, store);
  console.log('Verification:', valid ? '✓ Valid' : '✗ Invalid');

  // Time travel to snapshot 1
  const pastStore = await reconstructState(store, git, frozen1.tNs);
  const results = pastStore.querySync(`
    PREFIX ex: <http://example.org/>
    SELECT ?city WHERE {
      GRAPH <kgc:Universe> { ex:alice ex:city ?city }
    }
  `);
  console.log('Alice city at snapshot 1:', results.length === 0 ? 'not set' : results[0].get('city').value);
}

main().catch(console.error);
```

## Summary

You've learned:
- ✓ Freeze the universe to Git snapshots
- ✓ Create multiple snapshots over time
- ✓ Travel back to any historical point
- ✓ Verify snapshot integrity with BLAKE3
- ✓ Understand the zero-information invariant

## Next Steps

You've completed the tutorial series! Next, explore:
- [How-To Guides](../how-to-guides/index.md) - Task-specific instructions
- [API Reference](../references/01-api.md) - Detailed API documentation
- [Temporal Reconstruction Explanation](../explanations/03-temporal-reconstruction.md) - Deep dive into how time travel works

## Key Concepts

| Term | Meaning |
|------|---------|
| **Freeze** | Create a snapshot of the current Universe state |
| **Snapshot** | A point-in-time capture of all RDF triples |
| **Time Travel** | Reconstruct state at a historical timestamp |
| **Verify** | Cryptographically prove a snapshot hasn't changed |
| **Zero-Information Invariant** | All state is reconstructible from events + snapshots |
| **BLAKE3** | Cryptographic hash function for integrity verification |
