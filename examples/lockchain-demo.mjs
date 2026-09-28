#!/usr/bin/env node

/**
 * @file Lockchain Demo
 * @description
 * Demonstrates the LockchainWriter functionality with Git anchoring
 * and verifiable audit trails.
 */

import { createStore, dataFactory } from '@unrdf/oxigraph';
import { createLockchainWriter as _createLockchainWriter } from '../packages/knowledge-engine/src/lockchain-writer.mjs';
import { TransactionManager } from '../packages/knowledge-engine/src/transaction.mjs';

console.log('🔗 Lockchain Demo\n');

async function demonstrateLockchain() {
  try {
    // Create a transaction manager with lockchain enabled
    const txManager = new TransactionManager({
      enableLockchain: true,
      lockchainConfig: {
        gitRepo: process.cwd(),
        refName: 'refs/notes/demo-lockchain',
        batchSize: 3,
        enableGitAnchoring: true,
      },
    });

    console.log('📋 Transaction Manager created with lockchain enabled');

    // Create a test store
    const store = createStore();

    // Add some initial data
    store.add(
      dataFactory.quad(
        dataFactory.namedNode('ex:alice'),
        dataFactory.namedNode('ex:hasRole'),
        dataFactory.namedNode('ex:admin')
      )
    );

    console.log('📊 Initial store size:', store.size);

    // Create a transaction delta
    const delta1 = {
      additions: [
        {
          subject: { value: 'ex:alice', termType: 'NamedNode' },
          predicate: { value: 'ex:knows', termType: 'NamedNode' },
          object: { value: 'ex:bob', termType: 'NamedNode' },
        },
      ],
      removals: [],
    };

    // Apply first transaction
    console.log('\n🔄 Applying first transaction...');
    const result1 = await txManager.apply(store, delta1, {
      actor: 'demo-user',
    });
    console.log(`✅ Transaction 1: ${result1.receipt.committed ? 'committed' : 'failed'}`);
    console.log(`📊 Store size after transaction 1: ${store.size}`);

    // Create second transaction
    const delta2 = {
      additions: [
        {
          subject: { value: 'ex:bob', termType: 'NamedNode' },
          predicate: { value: 'ex:hasRole', termType: 'NamedNode' },
          object: { value: 'ex:user', termType: 'NamedNode' },
        },
      ],
      removals: [],
    };

    // Apply second transaction
    console.log('\n🔄 Applying second transaction...');
    const result2 = await txManager.apply(store, delta2, {
      actor: 'demo-user',
    });
    console.log(`✅ Transaction 2: ${result2.receipt.committed ? 'committed' : 'failed'}`);
    console.log(`📊 Store size after transaction 2: ${store.size}`);

    // Create third transaction to trigger batch commit
    const delta3 = {
      additions: [
        {
          subject: { value: 'ex:charlie', termType: 'NamedNode' },
          predicate: { value: 'ex:hasRole', termType: 'NamedNode' },
          object: { value: 'ex:guest', termType: 'NamedNode' },
        },
      ],
      removals: [],
    };

    // Apply third transaction (should trigger batch commit)
    console.log('\n🔄 Applying third transaction (triggers batch commit)...');
    const result3 = await txManager.apply(store, delta3, {
      actor: 'demo-user',
    });
    console.log(`✅ Transaction 3: ${result3.receipt.committed ? 'committed' : 'failed'}`);
    console.log(`📊 Store size after transaction 3: ${store.size}`);

    // Manually commit any remaining entries
    console.log('\n💾 Committing remaining lockchain entries...');
    const commitResult = await txManager.commitLockchain();
    console.log(`✅ Lockchain commit: ${commitResult.committed ? 'success' : 'failed'}`);
    if (commitResult.committed) {
      console.log(`📝 Commit hash: ${commitResult.commitHash}`);
      console.log(`📊 Entries committed: ${commitResult.entryCount}`);
    }

    // Get lockchain statistics
    const stats = txManager.getStats();
    console.log('\n📊 Lockchain Statistics:');
    console.log(`  Enabled: ${stats.lockchainEnabled}`);
    if (stats.lockchain) {
      console.log(`  Pending entries: ${stats.lockchain.pendingEntries}`);
      console.log(`  Git enabled: ${stats.lockchain.gitEnabled}`);
      console.log(`  Storage path: ${stats.lockchain.storagePath}`);
    }

    console.log('\n🎉 Lockchain demo completed successfully!');
  } catch (error) {
    console.error('❌ Lockchain demo failed:', error.message);
    throw error;
  }
}

// Run the demo
demonstrateLockchain().catch(error => {
  console.error('💥 Demo failed:', error);
  process.exit(1);
});
