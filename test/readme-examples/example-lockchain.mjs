/**
 * README Example: Cryptographic Provenance (lines 247-268)
 * Tests LockchainWriter with Merkle verification
 */

import { LockchainWriter } from '../../src/knowledge-engine/index.mjs';
import { mkdirSync, rmSync } from 'fs';
import { join } from 'path';

/**
 *
 */
async function testLockchain() {
  console.log('🧪 Testing Lockchain Example...');

  const testDir = join(process.cwd(), 'test', 'readme-examples', 'lockchain-test-repo');

  try {
    // Clean up if exists
    try {
      rmSync(testDir, { recursive: true, force: true });
    } catch (e) {
      // Ignore if doesn't exist
    }

    mkdirSync(testDir, { recursive: true });

    const lockchain = new LockchainWriter({
      repoPath: testDir,
      enableMerkle: true,
    });

    await lockchain.init();
    console.log('✅ Initialized lockchain');

    // Write cryptographically signed receipt
    const receipt = await lockchain.writeReceipt({
      actor: 'alice@example.org',
      action: 'add-data',
      delta: { additions: [], removals: [] },
      timestamp: new Date(),
      metadata: { reason: 'User registration' },
    });
    console.log('✅ Wrote receipt');

    // Receipt includes SHA3-256 Merkle root for tamper detection
    if (!receipt.merkleRoot) {
      throw new Error('Receipt missing merkleRoot');
    }
    console.log('✅ Merkle root:', receipt.merkleRoot);

    // Clean up
    rmSync(testDir, { recursive: true, force: true });

    console.log('\n✅ Lockchain Example: PASSED\n');
    return true;
  } catch (error) {
    // Clean up on error
    try {
      rmSync(testDir, { recursive: true, force: true });
    } catch (e) {
      // Ignore cleanup errors
    }

    console.error('❌ Lockchain Example FAILED:', error.message);
    console.error(error.stack);
    return false;
  }
}

// Run test if executed directly
if (import.meta.url === `file://${process.argv[1]}`) {
  const success = await testLockchain();
  process.exit(success ? 0 : 1);
}

export { testLockchain };
