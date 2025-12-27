/**
 * Composition: Frozen Universe with Cryptographic Receipt
 * Atoms: A08 (Universe Freezing) + A09 (Receipt Generation)
 * Package: @unrdf/fusion
 * 
 * Emergent Capability: Time-travel snapshots with cryptographic audit trail
 * Use Cases: Compliance logging, blockchain anchoring, forensic analysis
 */

import { freezeUniverse, verifyReceipt, KGCStore, now } from '@unrdf/kgc-4d';

/**
 * Create a frozen universe snapshot with cryptographic receipt
 * @param {KGCStore} kgcStore - KGC 4D store instance
 * @param {Object} options - Freeze options
 * @returns {Promise<{receipt: Object, verification: Object}>}
 */
export async function createFrozenReceipt(kgcStore, options = {}) {
  // Freeze universe and generate receipt
  const receipt = await freezeUniverse(kgcStore.store, {
    tag: options.tag || 'snapshot',
    includeReceipt: true,
    metadata: options.metadata || {}
  });

  // Verify receipt cryptographically
  const verification = verifyReceipt(receipt);

  if (!verification.valid) {
    throw new Error('Receipt verification failed - cryptographic integrity compromised');
  }

  return { receipt, verification };
}

/**
 * Demo: Frozen universe with receipt
 */
export async function demo() {
  console.log('\n=== Frozen Universe + Receipt Demo ===\n');

  // Create KGC store
  const kgcStore = new KGCStore();

  // Log sample events
  await kgcStore.logEvent({
    type: 'workflow.started',
    data: { workflowId: 'demo-001', status: 'running' },
    timestamp: now()
  });

  await kgcStore.logEvent({
    type: 'workflow.completed',
    data: { workflowId: 'demo-001', status: 'success', duration: 1500 },
    timestamp: now()
  });

  // Create frozen snapshot with receipt
  const { receipt, verification } = await createFrozenReceipt(kgcStore, {
    tag: 'demo-snapshot',
    metadata: { reason: 'capability-demo', timestamp: new Date().toISOString() }
  });

  console.log('Receipt Hash:', receipt.hash.substring(0, 16) + '...');
  console.log('Timestamp:', receipt.timestamp);
  console.log('Verification:', verification.valid ? '✅ VALID' : '❌ INVALID');
  console.log('\nEmergent Capability: Time-travel + cryptographic audit trail');
  console.log('Status: ✅ Composition PROVEN\n');

  return { receipt, verification };
}

// Run demo if executed directly
if (import.meta.url === `file://${process.argv[1]}`) {
  demo().catch(console.error);
}
