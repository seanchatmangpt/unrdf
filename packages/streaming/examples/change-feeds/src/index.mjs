// @ts-check
/**
 * @file Change Feeds Example
 * Demonstrates change feed creation, subscription, history, and replay
 */

import { Store, DataFactory } from 'n3';
import { createChangeFeed } from '@unrdf/streaming';

const { namedNode, literal, quad } = DataFactory;

/**
 * Example 1: Create and Subscribe to Change Feed
 */
async function basicChangeFeed() {
  console.log('\n=== Example 1: Basic Change Feed ===\n');

  const store = new Store();
  const feed = createChangeFeed(store);

  // Subscribe to changes
  const unsubscribe = feed.subscribe((change) => {
    console.log(`Change detected: ${change.type}`);
    console.log(`  Subject: ${change.quad.subject.value}`);
    console.log(`  Predicate: ${change.quad.predicate.value}`);
    console.log(`  Object: ${change.quad.object.value}`);
    console.log(`  Timestamp: ${change.timestamp}`);
  });

  // Make some changes
  const alice = namedNode('http://example.org/alice');
  const bob = namedNode('http://example.org/bob');
  const knows = namedNode('http://xmlns.com/foaf/0.1/knows');
  const name = namedNode('http://xmlns.com/foaf/0.1/name');

  store.addQuad(quad(alice, name, literal('Alice')));
  store.addQuad(quad(alice, knows, bob));
  store.addQuad(quad(bob, name, literal('Bob')));

  // Wait a bit for async processing
  await new Promise(resolve => setTimeout(resolve, 100));

  // Remove a quad
  store.removeQuad(quad(alice, knows, bob));

  await new Promise(resolve => setTimeout(resolve, 100));

  unsubscribe();
  console.log('\nUnsubscribed from change feed');
}

/**
 * Example 2: Change History and Replay
 */
async function changeHistory() {
  console.log('\n=== Example 2: Change History and Replay ===\n');

  const store = new Store();
  const feed = createChangeFeed(store);

  // Make multiple changes
  const alice = namedNode('http://example.org/alice');
  const age = namedNode('http://xmlns.com/foaf/0.1/age');
  const name = namedNode('http://xmlns.com/foaf/0.1/name');

  store.addQuad(quad(alice, name, literal('Alice')));
  store.addQuad(quad(alice, age, literal('30', namedNode('http://www.w3.org/2001/XMLSchema#integer'))));
  store.removeQuad(quad(alice, age, literal('30', namedNode('http://www.w3.org/2001/XMLSchema#integer'))));
  store.addQuad(quad(alice, age, literal('31', namedNode('http://www.w3.org/2001/XMLSchema#integer'))));

  await new Promise(resolve => setTimeout(resolve, 100));

  // Get change history
  const history = feed.getHistory();
  console.log(`Total changes: ${history.length}`);

  history.forEach((change, index) => {
    console.log(`\nChange ${index + 1}:`);
    console.log(`  Type: ${change.type}`);
    console.log(`  Quad: ${change.quad.subject.value} ${change.quad.predicate.value} ${change.quad.object.value}`);
  });

  // Replay changes to a new store
  console.log('\n--- Replaying changes to new store ---');
  const newStore = new Store();

  feed.replay((change) => {
    if (change.type === 'add') {
      newStore.addQuad(change.quad);
      console.log(`Replayed ADD: ${change.quad.object.value}`);
    } else if (change.type === 'remove') {
      newStore.removeQuad(change.quad);
      console.log(`Replayed REMOVE: ${change.quad.object.value}`);
    }
  });

  console.log(`\nNew store size: ${newStore.size}`);
  console.log(`Original store size: ${store.size}`);
}

/**
 * Example 3: Filtered Subscriptions
 */
async function filteredSubscription() {
  console.log('\n=== Example 3: Filtered Subscriptions ===\n');

  const store = new Store();
  const feed = createChangeFeed(store);

  // Subscribe only to additions
  const unsubscribeAdds = feed.subscribe((change) => {
    if (change.type === 'add') {
      console.log(`Added: ${change.quad.object.value}`);
    }
  });

  // Subscribe only to removals
  const unsubscribeRemoves = feed.subscribe((change) => {
    if (change.type === 'remove') {
      console.log(`Removed: ${change.quad.object.value}`);
    }
  });

  const alice = namedNode('http://example.org/alice');
  const name = namedNode('http://xmlns.com/foaf/0.1/name');
  const email = namedNode('http://xmlns.com/foaf/0.1/mbox');

  store.addQuad(quad(alice, name, literal('Alice')));
  store.addQuad(quad(alice, email, literal('alice@example.org')));

  await new Promise(resolve => setTimeout(resolve, 100));

  store.removeQuad(quad(alice, email, literal('alice@example.org')));

  await new Promise(resolve => setTimeout(resolve, 100));

  unsubscribeAdds();
  unsubscribeRemoves();
}

/**
 * Example 4: Time-based Queries
 */
async function timeBasedQueries() {
  console.log('\n=== Example 4: Time-based Queries ===\n');

  const store = new Store();
  const feed = createChangeFeed(store);

  const alice = namedNode('http://example.org/alice');
  const name = namedNode('http://xmlns.com/foaf/0.1/name');

  // First change
  store.addQuad(quad(alice, name, literal('Alice')));
  await new Promise(resolve => setTimeout(resolve, 50));

  const midpoint = Date.now();

  // Wait a bit
  await new Promise(resolve => setTimeout(resolve, 50));

  // Second change
  store.removeQuad(quad(alice, name, literal('Alice')));
  store.addQuad(quad(alice, name, literal('Alice Smith')));

  await new Promise(resolve => setTimeout(resolve, 100));

  // Get all changes
  const allChanges = feed.getHistory();
  console.log(`Total changes: ${allChanges.length}`);

  // Get changes after midpoint
  const recentChanges = feed.getHistory({ since: midpoint });
  console.log(`Changes after midpoint: ${recentChanges.length}`);

  recentChanges.forEach((change, index) => {
    console.log(`\nRecent change ${index + 1}:`);
    console.log(`  Type: ${change.type}`);
    console.log(`  Value: ${change.quad.object.value}`);
  });
}

/**
 * Main execution
 */
async function main() {
  console.log('🌊 UNRDF Streaming - Change Feeds Examples\n');

  try {
    await basicChangeFeed();
    await changeHistory();
    await filteredSubscription();
    await timeBasedQueries();

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
  basicChangeFeed,
  changeHistory,
  filteredSubscription,
  timeBasedQueries
};
