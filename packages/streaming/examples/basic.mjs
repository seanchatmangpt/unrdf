/**
 * @unrdf/streaming - Basic Example
 *
 * Demonstrates basic usage of the streaming package.
 */

import dataModel from '@rdfjs/data-model';
const { namedNode, literal } = dataModel;
import {
  createChangeFeed,
  createSubscriptionManager,
  createStreamProcessor,
  createSyncMessage,
} from '../src/index.mjs';

console.log('=== @unrdf/streaming Basic Example ===\n');

// 1. Create a change feed
console.log('1. Creating change feed...');
const feed = createChangeFeed();

// 2. Subscribe to changes
console.log('2. Setting up subscriptions...');
const manager = createSubscriptionManager(feed);

// Subscribe to all changes
manager.subscribe((change) => {
  console.log(`   [All] ${change.type}:`, change.quad.subject.value);
});

// Subscribe to specific subject
const subject = namedNode('http://example.org/person/1');
manager.subscribe(
  (change) => {
    console.log(`   [Filtered] ${change.type}:`, change.quad.object.value);
  },
  { subject }
);

// 3. Create stream processor
console.log('3. Setting up stream processor...');
const processor = createStreamProcessor(feed);

// Filter and batch changes
processor
  .filter((c) => c.type === 'add')
  .batch(2)
  .subscribe((changes) => {
    console.log(`   [Batch] Received ${changes.length} changes`);
  });

// 4. Emit changes
console.log('\n4. Emitting changes...');

feed.emitChange({
  type: 'add',
  quad: {
    subject,
    predicate: namedNode('http://example.org/name'),
    object: literal('Alice'),
  },
});

feed.emitChange({
  type: 'add',
  quad: {
    subject: namedNode('http://example.org/person/2'),
    predicate: namedNode('http://example.org/name'),
    object: literal('Bob'),
  },
});

feed.emitChange({
  type: 'update',
  quad: {
    subject,
    predicate: namedNode('http://example.org/age'),
    object: literal('30', namedNode('http://www.w3.org/2001/XMLSchema#integer')),
  },
});

// 5. Replay changes
console.log('\n5. Replaying changes...');
feed.replay((change) => {
  console.log(`   [Replay] ${change.type} at ${new Date(change.timestamp).toISOString()}`);
});

// 6. Create sync message
console.log('\n6. Creating sync message...');
const changes = feed.getChanges();
const syncMessage = createSyncMessage(changes);

console.log(`   Message version: ${syncMessage.version}`);
console.log(`   Changes: ${syncMessage.changes.length}`);
console.log(`   Checksum: ${syncMessage.checksum.substring(0, 16)}...`);

// 7. List subscriptions
console.log('\n7. Active subscriptions:');
const subs = manager.listSubscriptions();
console.log(`   Total: ${subs.length}`);
subs.forEach((sub, i) => {
  const filterDesc = sub.filter ? JSON.stringify(sub.filter) : 'none';
  console.log(`   ${i + 1}. ID: ${sub.id}, Filter: ${filterDesc}`);
});

console.log('\n=== Example Complete ===');
