/**
 * README Example: Cryptographic Audit Trail (lines 430-462)
 * Tests full transaction audit workflow
 */

import { createDarkMatterCore, LockchainWriter, DataFactory } from '../../src/knowledge-engine/index.mjs';
import { mkdirSync, rmSync } from 'fs';
import { join } from 'path';
const { namedNode, quad, literal } = DataFactory;

async function testAuditTrail() {
  console.log('🧪 Testing Cryptographic Audit Trail Example...');

  const system = await createDarkMatterCore();
  const testDir = join(process.cwd(), 'test', 'readme-examples', 'audit-trail-test');

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
      enableMerkle: true
    });

    await lockchain.init();
    console.log('✅ Initialized lockchain');

    // Execute transaction with audit
    const result = await system.executeTransaction({
      additions: [
        quad(
          namedNode('http://example.org/newuser'),
          namedNode('http://example.org/name'),
          literal('New User')
        )
      ],
      removals: [],
      actor: 'alice@example.org'
    });
    console.log('✅ Executed transaction');

    // Write cryptographically signed receipt
    const receipt = await lockchain.writeReceipt({
      actor: 'alice@example.org',
      action: 'add-user',
      delta: result.delta,
      timestamp: new Date(),
      metadata: { ip: '192.168.1.1', reason: 'User registration' }
    });
    console.log('✅ Wrote receipt');

    // Verify integrity
    const isValid = await lockchain.verifyReceipt(receipt);
    console.log('✅ Audit trail valid:', isValid);

    if (!isValid) {
      throw new Error('Receipt verification failed');
    }

    await system.cleanup();

    // Clean up
    rmSync(testDir, { recursive: true, force: true });

    console.log('\n✅ Cryptographic Audit Trail Example: PASSED\n');
    return true;
  } catch (error) {
    await system.cleanup();

    // Clean up on error
    try {
      rmSync(testDir, { recursive: true, force: true });
    } catch (e) {
      // Ignore cleanup errors
    }

    console.error('❌ Cryptographic Audit Trail Example FAILED:', error.message);
    console.error(error.stack);
    return false;
  }
}

// Run test if executed directly
if (import.meta.url === `file://${process.argv[1]}`) {
  const success = await testAuditTrail();
  process.exit(success ? 0 : 1);
}

export { testAuditTrail };
