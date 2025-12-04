// @ts-check
/**
 * @file Real-Time Sync Example
 * Demonstrates subscription management, real-time monitoring, concurrent updates, and peer sync
 */

import { Store, DataFactory } from 'n3';
import { createSubscriptionManager, createChangeFeed } from '@unrdf/streaming';

const { namedNode, literal, quad } = DataFactory;

/**
 * Example 1: Basic Subscription Management
 */
async function basicSubscription() {
  console.log('\n=== Example 1: Basic Subscription Management ===\n');

  const store = new Store();
  const manager = createSubscriptionManager(store);

  // Subscribe to specific pattern
  const alice = namedNode('http://example.org/alice');
  const name = namedNode('http://xmlns.com/foaf/0.1/name');

  const subscriptionId = manager.subscribe({
    subject: alice,
    predicate: name
  }, (quads) => {
    console.log(`Name updated for Alice: ${quads[0].object.value}`);
  });

  console.log(`Created subscription: ${subscriptionId}`);

  // Make matching change
  store.addQuad(quad(alice, name, literal('Alice')));
  await new Promise(resolve => setTimeout(resolve, 100));

  // Make non-matching change
  const age = namedNode('http://xmlns.com/foaf/0.1/age');
  store.addQuad(quad(alice, age, literal('30')));
  await new Promise(resolve => setTimeout(resolve, 100));

  // Update name again
  store.removeQuad(quad(alice, name, literal('Alice')));
  store.addQuad(quad(alice, name, literal('Alice Smith')));
  await new Promise(resolve => setTimeout(resolve, 100));

  manager.unsubscribe(subscriptionId);
  console.log('Unsubscribed');
}

/**
 * Example 2: Real-Time Graph Monitoring
 */
async function realTimeMonitoring() {
  console.log('\n=== Example 2: Real-Time Graph Monitoring ===\n');

  const store = new Store();
  const manager = createSubscriptionManager(store);

  // Monitor all changes
  const allChangesId = manager.subscribe({}, (quads) => {
    console.log(`Graph updated: ${quads.length} quads affected`);
    quads.forEach(q => {
      console.log(`  ${q.subject.value} ${q.predicate.value} ${q.object.value}`);
    });
  });

  // Monitor specific predicate
  const knows = namedNode('http://xmlns.com/foaf/0.1/knows');
  const socialId = manager.subscribe({
    predicate: knows
  }, (quads) => {
    console.log(`Social relationship updated`);
  });

  // Make various changes
  const alice = namedNode('http://example.org/alice');
  const bob = namedNode('http://example.org/bob');
  const charlie = namedNode('http://example.org/charlie');
  const name = namedNode('http://xmlns.com/foaf/0.1/name');

  store.addQuad(quad(alice, name, literal('Alice')));
  await new Promise(resolve => setTimeout(resolve, 100));

  store.addQuad(quad(alice, knows, bob));
  await new Promise(resolve => setTimeout(resolve, 100));

  store.addQuad(quad(bob, knows, charlie));
  await new Promise(resolve => setTimeout(resolve, 100));

  manager.unsubscribe(allChangesId);
  manager.unsubscribe(socialId);
}

/**
 * Example 3: Concurrent Updates and Conflict Resolution
 */
async function concurrentUpdates() {
  console.log('\n=== Example 3: Concurrent Updates and Conflict Resolution ===\n');

  const store = new Store();
  const manager = createSubscriptionManager(store);

  const alice = namedNode('http://example.org/alice');
  const age = namedNode('http://xmlns.com/foaf/0.1/age');

  // Track conflicts
  const conflicts = [];

  manager.subscribe({
    subject: alice,
    predicate: age
  }, (quads) => {
    const ageValues = quads.map(q => q.object.value);
    if (ageValues.length > 1) {
      conflicts.push({
        timestamp: Date.now(),
        values: ageValues
      });
      console.log(`⚠️  Conflict detected: Multiple age values [${ageValues.join(', ')}]`);
    } else {
      console.log(`Age updated: ${ageValues[0]}`);
    }
  });

  // Simulate concurrent updates
  console.log('Simulating concurrent updates...');

  // Client 1 updates age
  store.addQuad(quad(alice, age, literal('30', namedNode('http://www.w3.org/2001/XMLSchema#integer'))));

  // Client 2 updates age (conflict!)
  store.addQuad(quad(alice, age, literal('31', namedNode('http://www.w3.org/2001/XMLSchema#integer'))));

  await new Promise(resolve => setTimeout(resolve, 100));

  // Resolve conflict: use last-write-wins
  console.log('\nResolving conflict with last-write-wins...');
  const ageQuads = store.getQuads(alice, age, null, null);

  if (ageQuads.length > 1) {
    // Remove all but last
    ageQuads.slice(0, -1).forEach(q => store.removeQuad(q));
    console.log(`Kept most recent value: ${ageQuads[ageQuads.length - 1].object.value}`);
  }

  await new Promise(resolve => setTimeout(resolve, 100));

  console.log(`\nTotal conflicts detected: ${conflicts.length}`);
}

/**
 * Example 4: Multi-Peer Synchronization
 */
async function multiPeerSync() {
  console.log('\n=== Example 4: Multi-Peer Synchronization ===\n');

  // Create three peer stores
  const peer1 = new Store();
  const peer2 = new Store();
  const peer3 = new Store();

  const feed1 = createChangeFeed(peer1);
  const feed2 = createChangeFeed(peer2);
  const feed3 = createChangeFeed(peer3);

  // Setup bidirectional sync between peers
  console.log('Setting up peer-to-peer synchronization...');

  // Peer 1 -> Peer 2
  feed1.subscribe((change) => {
    if (change.type === 'add') {
      peer2.addQuad(change.quad);
    } else if (change.type === 'remove') {
      peer2.removeQuad(change.quad);
    }
  });

  // Peer 2 -> Peer 3
  feed2.subscribe((change) => {
    if (change.type === 'add') {
      peer3.addQuad(change.quad);
    } else if (change.type === 'remove') {
      peer3.removeQuad(change.quad);
    }
  });

  // Peer 3 -> Peer 1 (complete the ring)
  feed3.subscribe((change) => {
    if (change.type === 'add') {
      // Avoid circular updates
      if (!peer1.has(change.quad)) {
        peer1.addQuad(change.quad);
      }
    }
  });

  // Make changes on peer 1
  console.log('\nMaking changes on Peer 1...');
  const alice = namedNode('http://example.org/alice');
  const name = namedNode('http://xmlns.com/foaf/0.1/name');
  const bob = namedNode('http://example.org/bob');
  const knows = namedNode('http://xmlns.com/foaf/0.1/knows');

  peer1.addQuad(quad(alice, name, literal('Alice')));
  peer1.addQuad(quad(alice, knows, bob));

  await new Promise(resolve => setTimeout(resolve, 200));

  console.log(`\nPeer 1 size: ${peer1.size}`);
  console.log(`Peer 2 size: ${peer2.size}`);
  console.log(`Peer 3 size: ${peer3.size}`);

  // Verify sync
  const synced = peer1.size === peer2.size && peer2.size === peer3.size;
  console.log(`\n${synced ? '✅' : '❌'} Peers synchronized: ${synced}`);
}

/**
 * Example 5: Subscription Patterns
 */
async function subscriptionPatterns() {
  console.log('\n=== Example 5: Advanced Subscription Patterns ===\n');

  const store = new Store();
  const manager = createSubscriptionManager(store);

  const alice = namedNode('http://example.org/alice');
  const bob = namedNode('http://example.org/bob');
  const name = namedNode('http://xmlns.com/foaf/0.1/name');
  const knows = namedNode('http://xmlns.com/foaf/0.1/knows');
  const age = namedNode('http://xmlns.com/foaf/0.1/age');

  // Pattern 1: All properties of a subject
  console.log('Pattern 1: All properties of Alice');
  const aliceId = manager.subscribe({
    subject: alice
  }, (quads) => {
    console.log(`  Alice updated: ${quads.length} properties`);
  });

  // Pattern 2: All subjects with a predicate
  console.log('Pattern 2: All people with names');
  const namesId = manager.subscribe({
    predicate: name
  }, (quads) => {
    console.log(`  Name updated: ${quads[0].subject.value}`);
  });

  // Pattern 3: Specific object
  console.log('Pattern 3: Anyone who knows Bob');
  const bobFriendsId = manager.subscribe({
    predicate: knows,
    object: bob
  }, (quads) => {
    console.log(`  Someone knows Bob: ${quads[0].subject.value}`);
  });

  console.log('\nMaking updates...');

  store.addQuad(quad(alice, name, literal('Alice')));
  await new Promise(resolve => setTimeout(resolve, 50));

  store.addQuad(quad(alice, age, literal('30')));
  await new Promise(resolve => setTimeout(resolve, 50));

  store.addQuad(quad(alice, knows, bob));
  await new Promise(resolve => setTimeout(resolve, 50));

  store.addQuad(quad(bob, name, literal('Bob')));
  await new Promise(resolve => setTimeout(resolve, 50));

  manager.unsubscribe(aliceId);
  manager.unsubscribe(namesId);
  manager.unsubscribe(bobFriendsId);
}

/**
 * Main execution
 */
async function main() {
  console.log('🔄 UNRDF Streaming - Real-Time Sync Examples\n');

  try {
    await basicSubscription();
    await realTimeMonitoring();
    await concurrentUpdates();
    await multiPeerSync();
    await subscriptionPatterns();

    console.log('\n✅ All examples completed successfully!\n');
  } catch (error) {
    console.error('❌ Error running examples:', error);
    process.exit(1);
  }
}

// Run if called directly
if (import.meta.url === `file://${process.argv[1]}`) {
  main();
}

export {
  basicSubscription,
  realTimeMonitoring,
  concurrentUpdates,
  multiPeerSync,
  subscriptionPatterns
};
