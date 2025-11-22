/**
 * @file Change Feed Integration Example
 * @description
 * Demonstrates how to integrate the change feed with the transaction system
 * to track and stream RDF graph changes.
 */

import { Store, DataFactory } from 'n3';
import { TransactionManager } from '../../src/knowledge-engine/transaction.mjs';
import {
  createChangeFeed,
  createChangeFeedHook,
} from '../../src/knowledge-engine/streaming/index.mjs';

const { namedNode, literal, quad } = DataFactory;

async function main() {
  // Create change feed
  const feed = createChangeFeed({
    enableHistory: true,
    historySize: 1000,
    batchMode: false,
  });

  // Start the feed
  feed.start();
  console.log('Change feed started');

  // Listen for changes
  feed.on('change', change => {
    console.log('Change detected:', {
      id: change.id,
      type: change.type,
      additionsCount: change.delta.additions.length,
      removalsCount: change.delta.removals.length,
      timestamp: new Date(change.timestamp).toISOString(),
    });

    if (change.metadata) {
      console.log('  Metadata:', change.metadata);
    }
  });

  // Create transaction manager
  const txManager = new TransactionManager({
    strictMode: false,
    enableLockchain: false,
  });

  // Add change feed hook to transaction manager
  const changeFeedHook = createChangeFeedHook(feed, {
    strict: false,
    metadata: {
      source: 'transaction-system',
    },
  });

  txManager.addHook(changeFeedHook);
  console.log('Change feed hook registered');

  // Create store
  const store = new Store();

  // Perform transactions
  console.log('\nPerforming transactions...\n');

  // Transaction 1: Add a person
  const delta1 = {
    additions: [
      quad(
        namedNode('http://example.org/alice'),
        namedNode('http://www.w3.org/1999/02/22-rdf-syntax-ns#type'),
        namedNode('http://example.org/Person')
      ),
      quad(
        namedNode('http://example.org/alice'),
        namedNode('http://example.org/name'),
        literal('Alice')
      ),
      quad(
        namedNode('http://example.org/alice'),
        namedNode('http://example.org/age'),
        literal('30', namedNode('http://www.w3.org/2001/XMLSchema#integer'))
      ),
    ],
    removals: [],
  };

  const result1 = await txManager.apply(store, delta1, { actor: 'system' });
  console.log('Transaction 1 committed:', result1.receipt.committed);

  // Transaction 2: Update person's age
  const delta2 = {
    additions: [
      quad(
        namedNode('http://example.org/alice'),
        namedNode('http://example.org/age'),
        literal('31', namedNode('http://www.w3.org/2001/XMLSchema#integer'))
      ),
    ],
    removals: [
      quad(
        namedNode('http://example.org/alice'),
        namedNode('http://example.org/age'),
        literal('30', namedNode('http://www.w3.org/2001/XMLSchema#integer'))
      ),
    ],
  };

  const result2 = await txManager.apply(result1.store, delta2, {
    actor: 'alice',
  });
  console.log('Transaction 2 committed:', result2.receipt.committed);

  // Transaction 3: Delete person
  const delta3 = {
    additions: [],
    removals: [
      quad(
        namedNode('http://example.org/alice'),
        namedNode('http://www.w3.org/1999/02/22-rdf-syntax-ns#type'),
        namedNode('http://example.org/Person')
      ),
      quad(
        namedNode('http://example.org/alice'),
        namedNode('http://example.org/name'),
        literal('Alice')
      ),
      quad(
        namedNode('http://example.org/alice'),
        namedNode('http://example.org/age'),
        literal('31', namedNode('http://www.w3.org/2001/XMLSchema#integer'))
      ),
    ],
  };

  const result3 = await txManager.apply(result2.store, delta3, {
    actor: 'admin',
  });
  console.log('Transaction 3 committed:', result3.receipt.committed);

  // Get change history
  console.log('\nChange History:\n');
  const history = feed.getHistory();
  console.log(`Total changes: ${history.length}`);

  for (const change of history) {
    console.log(`- ${change.type} at ${new Date(change.timestamp).toISOString()}`);
  }

  // Get history filtered by type
  console.log('\nAdd changes:');
  const adds = feed.getHistory({ type: 'add' });
  console.log(`Count: ${adds.length}`);

  console.log('\nUpdate changes:');
  const updates = feed.getHistory({ type: 'update' });
  console.log(`Count: ${updates.length}`);

  console.log('\nDelete changes:');
  const deletes = feed.getHistory({ type: 'delete' });
  console.log(`Count: ${deletes.length}`);

  // Get metrics
  console.log('\nChange Feed Metrics:');
  const metrics = feed.getMetrics();
  console.log(JSON.stringify(metrics, null, 2));

  // Cleanup
  await feed.cleanup();
  console.log('\nCleanup complete');
}

main().catch(console.error);
