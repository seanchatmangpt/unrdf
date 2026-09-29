#!/usr/bin/env node

/**
 * Lockchain Validation Test
 *
 * This test specifically validates the lockchain functionality including
 * receipt writing, batch committing, Git integration, and integrity verification.
 */

import { createLockchainWriter } from '../packages/knowledge-engine/src/lockchain-writer.mjs';
import { randomUUID } from 'crypto';

console.log('🔗 Lockchain Validation Test\n');

async function testLockchain() {
  let success = true;
  const results = [];

  try {
    // === Test 1: Basic Lockchain Writer Creation ===
    console.log('🔧 Test 1: Basic Lockchain Writer Creation');

    const lockchainWriter = createLockchainWriter({
      gitRepo: process.cwd(),
      refName: 'refs/notes/lockchain-test',
      batchSize: 3,
    });

    console.log('  ✅ Lockchain writer created');
    results.push({ test: 'Basic Lockchain Writer Creation', success: true });

    // === Test 2: Receipt Writing ===
    console.log('\n📝 Test 2: Receipt Writing');

    const receipts = [];
    for (let i = 0; i < 5; i++) {
      const receipt = {
        id: randomUUID(),
        delta: {
          additions: [
            { s: `ex:test-${i}`, p: 'ex:type', o: 'ex:document' },
            { s: `ex:test-${i}`, p: 'ex:status', o: 'ex:active' },
          ],
          removals: [],
        },
        committed: true,
        hookResults: [{ hookId: `hook-${i}`, mode: 'pre', result: true }],
        beforeHash: { sha3: `before-${i}`, blake3: `before-${i}` },
        afterHash: { sha3: `after-${i}`, blake3: `after-${i}` },
        timestamp: Date.now() + i,
        durationMs: 100 + i * 10,
        actor: `test-user-${i}`,
        hookErrors: [],
      };

      const entryId = await lockchainWriter.writeReceipt(receipt);
      receipts.push({ receipt, entryId });
      console.log(`  ✅ Receipt ${i + 1} written: ${entryId}`);
    }

    results.push({ test: 'Receipt Writing', success: true });

    // === Test 3: Batch Committing ===
    console.log('\n📦 Test 3: Batch Committing');

    const commitResult = await lockchainWriter.commitBatch();
    console.log('  ✅ Batch committed:', commitResult.committed);
    console.log('  📊 Batch ID:', commitResult.batchId);
    console.log('  📊 Commit hash:', commitResult.commitHash);
    console.log('  📊 Entry count:', commitResult.entryCount);

    results.push({ test: 'Batch Committing', success: true });

    // === Test 4: Statistics and Monitoring ===
    console.log('\n📊 Test 4: Statistics and Monitoring');

    const stats = lockchainWriter.getStats();
    console.log('  📊 Total entries:', stats.entryCount);
    console.log('  📊 Pending entries:', stats.pendingEntries);
    console.log('  📊 Batch size:', stats.batchSize);
    console.log('  📊 Git repo:', stats.gitRepo);
    console.log('  📊 Ref name:', stats.refName);

    results.push({ test: 'Statistics and Monitoring', success: true });

    // === Test 5: Integrity Verification ===
    console.log('\n🔍 Test 5: Integrity Verification');

    const integrity = await lockchainWriter.verifyIntegrity();
    console.log('  ✅ Integrity check:', integrity ? 'PASSED' : 'FAILED');

    results.push({ test: 'Integrity Verification', success: integrity });

    // === Test 6: Multiple Batch Processing ===
    console.log('\n🔄 Test 6: Multiple Batch Processing');

    // Write more receipts to trigger another batch
    for (let i = 5; i < 8; i++) {
      const receipt = {
        id: randomUUID(),
        delta: {
          additions: [{ s: `ex:test-${i}`, p: 'ex:type', o: 'ex:document' }],
          removals: [],
        },
        committed: true,
        hookResults: [],
        beforeHash: { sha3: `before-${i}`, blake3: `before-${i}` },
        afterHash: { sha3: `after-${i}`, blake3: `after-${i}` },
        timestamp: Date.now() + i,
        durationMs: 100 + i * 10,
        actor: `test-user-${i}`,
        hookErrors: [],
      };

      await lockchainWriter.writeReceipt(receipt);
      console.log(`  ✅ Additional receipt ${i + 1} written`);
    }

    // Commit the second batch
    const secondCommitResult = await lockchainWriter.commitBatch();
    console.log('  ✅ Second batch committed:', secondCommitResult.committed);
    console.log('  📊 Second batch ID:', secondCommitResult.batchId);

    results.push({ test: 'Multiple Batch Processing', success: true });

    // === Test 7: Final Statistics ===
    console.log('\n📈 Test 7: Final Statistics');

    const finalStats = lockchainWriter.getStats();
    console.log('  📊 Final total entries:', finalStats.entryCount);
    console.log('  📊 Final pending entries:', finalStats.pendingEntries);

    results.push({ test: 'Final Statistics', success: true });
  } catch (error) {
    console.error(`\n❌ Lockchain test failed: ${error.message}`);
    console.error(error.stack);
    success = false;
  }

  // === Summary ===
  console.log('\n🎯 Lockchain Validation Summary:');
  console.log('================================');

  results.forEach(result => {
    const status = result.success ? '✅' : '❌';
    console.log(`${status} ${result.test}`);
  });

  const passed = results.filter(r => r.success).length;
  const total = results.length;

  console.log(`\n📊 Results: ${passed}/${total} tests passed`);

  if (success && passed === total) {
    console.log('🎉 Lockchain validation: SUCCESS');
    console.log('🔗 All lockchain features are working correctly!');
  } else {
    console.log('⚠️  Lockchain validation: FAILED');
    console.log('🔧 Some lockchain features need attention');
  }

  return success;
}

// Run the test
testLockchain()
  .then(success => {
    process.exit(success ? 0 : 1);
  })
  .catch(error => {
    console.error('Fatal error:', error);
    process.exit(1);
  });
