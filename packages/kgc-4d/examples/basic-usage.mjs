/**
 * KGC 4D Engine - Basic Usage Example
 * Demonstrates freeze, time-travel, and state reconstruction
 */

import { KGCStore, GitBackbone, freezeUniverse, reconstructState, verifyReceipt } from '../src/index.mjs';
import { dataFactory } from '@unrdf/oxigraph';
import { GRAPHS, EVENT_TYPES } from '../src/constants.mjs';

/**
 * Example 1: Simple append and freeze
 */
export async function exampleBasicFreeze() {
  console.log('=== Example 1: Basic Freeze ===\n');

  // Initialize store and Git backbone
  const store = new KGCStore();
  const git = new GitBackbone('./my-repo');

  // Create a simple RDF quad: Alice is a Person
  const alice = dataFactory.namedNode('http://example.org/Alice');
  const rdfType = dataFactory.namedNode('http://www.w3.org/1999/02/22-rdf-syntax-ns#type');
  const person = dataFactory.namedNode('http://example.org/Person');

  const aliceQuad = dataFactory.quad(alice, rdfType, person);

  // 1. Append event: CREATE Alice
  console.log('📝 Adding Alice to universe...');
  const receipt1 = await store.appendEvent(
    {
      type: EVENT_TYPES.CREATE,
      payload: { description: 'Added Alice to the universe' },
    },
    [{ type: 'add', ...aliceQuad }]
  );

  console.log(`  ✓ Event appended at ${receipt1.receipt.timestamp_iso}`);
  console.log(`  ✓ Event count: ${receipt1.receipt.event_count}\n`);

  // 2. Freeze universe
  console.log('❄️  Freezing universe...');
  const frozenReceipt = await freezeUniverse(store, git);
  console.log(`  ✓ Frozen at: ${frozenReceipt.timestamp_iso}`);
  console.log(`  ✓ Universe hash: ${frozenReceipt.universe_hash.substring(0, 16)}...`);
  console.log(`  ✓ Git commit: ${frozenReceipt.git_ref.substring(0, 8)}...`);
  console.log(`  ✓ N-Quads count: ${frozenReceipt.nquad_count}\n`);

  return { store, git, frozenReceipt };
}

/**
 * Example 2: Time travel and state reconstruction
 */
export async function exampleTimeTravel(store, git, frozenReceipt) {
  console.log('=== Example 2: Time Travel ===\n');

  const bob = dataFactory.namedNode('http://example.org/Bob');
  const rdfType = dataFactory.namedNode('http://www.w3.org/1999/02/22-rdf-syntax-ns#type');
  const person = dataFactory.namedNode('http://example.org/Person');

  const bobQuad = dataFactory.quad(bob, rdfType, person);

  // 3. Modify state AFTER freeze: Add Bob
  console.log('✏️  Modifying universe (adding Bob)...');
  const receipt2 = await store.appendEvent(
    {
      type: EVENT_TYPES.CREATE,
      payload: { description: 'Added Bob to the universe' },
    },
    [{ type: 'add', ...bobQuad }]
  );
  console.log(`  ✓ Event appended at ${receipt2.receipt.timestamp_iso}\n`);

  // 4. Time travel back to frozen state
  console.log('⏰ Time traveling back to frozen state...');
  const targetTime = BigInt(frozenReceipt.t_ns);
  const pastStore = await reconstructState(store, git, targetTime);
  console.log(`  ✓ Reconstructed state at ${frozenReceipt.timestamp_iso}\n`);

  // 5. Verify receipt cryptographically
  console.log('🔐 Verifying frozen receipt...');
  const verification = await verifyReceipt(frozenReceipt, git, store);
  if (verification.valid) {
    console.log(`  ✓ Receipt verified`);
    console.log(`  ✓ Receipt ID: ${verification.receipt_id}`);
    console.log(`  ✓ Hash matches Git commit\n`);
  } else {
    console.log(`  ✗ Verification failed: ${verification.reason}\n`);
  }

  return { pastStore, verification };
}

/**
 * Example 3: Query event log history
 */
export async function exampleEventLogQuery(store) {
  console.log('=== Example 3: Event Log Query ===\n');

  console.log('📋 Querying event log...');
  const results = await store.queryEventLog(`
    PREFIX kgc: <http://kgc.io/>
    SELECT ?event ?type ?timestamp
    WHERE {
      GRAPH <${GRAPHS.EVENT_LOG}> {
        ?event kgc:type ?type .
        ?event kgc:t_ns ?t_ns .
      }
    }
    ORDER BY ?t_ns
  `);

  console.log(`  ✓ Found ${results.length} events in log\n`);

  for (const result of results) {
    console.log(`    - Type: ${result.type.value}`);
  }
  console.log();
}

/**
 * Example 4: Multi-event sequence with snapshots
 */
export async function exampleMultiEventSequence() {
  console.log('=== Example 4: Multi-Event Sequence ===\n');

  const store = new KGCStore();
  const git = new GitBackbone('./my-repo');

  const foaf = 'http://xmlns.com/foaf/0.1/';
  const alice = dataFactory.namedNode('http://example.org/Alice');

  // Event 1: Create Alice
  console.log('Event 1: Creating Alice...');
  await store.appendEvent(
    { type: EVENT_TYPES.CREATE, payload: { subject: 'Alice', action: 'create' } },
    [
      {
        type: 'add',
        subject: alice,
        predicate: dataFactory.namedNode(foaf + 'name'),
        object: dataFactory.literal('Alice'),
      },
    ]
  );

  // Event 2: Set age
  console.log('Event 2: Setting Alice age...');
  await store.appendEvent(
    { type: EVENT_TYPES.UPDATE, payload: { subject: 'Alice', property: 'age', value: 30 } },
    [
      {
        type: 'add',
        subject: alice,
        predicate: dataFactory.namedNode(foaf + 'age'),
        object: dataFactory.literal('30'),
      },
    ]
  );

  // Freeze after 2 events
  console.log('Snapshot 1: Freezing after 2 events...');
  const snapshot1 = await freezeUniverse(store, git);
  console.log(`  ✓ Snapshot at event count: ${snapshot1.event_count}\n`);

  // Event 3: Update age
  console.log('Event 3: Updating Alice age...');
  await store.appendEvent(
    { type: EVENT_TYPES.UPDATE, payload: { subject: 'Alice', property: 'age', value: 31 } },
    [
      {
        type: 'delete',
        subject: alice,
        predicate: dataFactory.namedNode(foaf + 'age'),
        object: dataFactory.literal('30'),
      },
      {
        type: 'add',
        subject: alice,
        predicate: dataFactory.namedNode(foaf + 'age'),
        object: dataFactory.literal('31'),
      },
    ]
  );

  // Freeze after 3 events
  console.log('Snapshot 2: Freezing after 3 events...');
  const snapshot2 = await freezeUniverse(store, git);
  console.log(`  ✓ Snapshot at event count: ${snapshot2.event_count}\n`);

  console.log('Summary:');
  console.log(`  - Event count: ${store.getEventCount()}`);
  console.log(`  - Snapshots taken: 2`);
  console.log(`  - Can time-travel to either snapshot\n`);

  return { store, git, snapshot1, snapshot2 };
}

/**
 * Main execution
 */
export async function main() {
  console.log('\n🚀 KGC 4D Engine - Basic Usage Examples\n');
  console.log('This demonstrates the 80/20 MVP features:\n');
  console.log('  1. Atomic event appending with ACID semantics');
  console.log('  2. Universe freeze to Git with BLAKE3 hashing');
  console.log('  3. Time travel via snapshot + replay');
  console.log('  4. Cryptographic receipt verification\n');

  try {
    // Run examples
    const example1 = await exampleBasicFreeze();
    const example2 = await exampleTimeTravel(example1.store, example1.git, example1.frozenReceipt);
    await exampleEventLogQuery(example1.store);
    await exampleMultiEventSequence();

    console.log('✅ All examples completed successfully!\n');
  } catch (error) {
    console.error('❌ Error:', error.message);
    process.exit(1);
  }
}

// Run if called directly
if (import.meta.url === `file://${process.argv[1]}`) {
  main();
}
