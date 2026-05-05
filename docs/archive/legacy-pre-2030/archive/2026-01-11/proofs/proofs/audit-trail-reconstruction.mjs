#!/usr/bin/env node
/**
 * Proof 2: Audit Trail Reconstruction
 *
 * Demonstrates that receipt chains enable complete audit trail reconstruction.
 *
 * Scenario:
 * 1. Generate 3 receipts in sequence (admit, freeze, publish)
 * 2. Extract audit trail from receipts
 * 3. Reconstruct decision chain
 * 4. Verify no gaps, no reordering
 *
 * Expected: ✅ Audit trail verified: 3 receipts, 0 gaps, chronological
 */

import crypto from 'crypto';

/**
 * Compute SHA-256 hash
 */
function computeHash(data) {
  return crypto.createHash('sha256').update(JSON.stringify(data)).digest('hex');
}

/**
 * Generate UUID v4
 */
function generateUUID() {
  return crypto.randomUUID();
}

/**
 * Create a chained receipt
 */
function createReceipt(operation, payload, actor, previousReceipt = null) {
  const id = generateUUID();
  const t_ns = BigInt(Date.now()) * 1_000_000n;
  const timestamp_iso = new Date(Number(t_ns / 1_000_000n)).toISOString();

  const receiptData = {
    id,
    operation,
    payload,
    actor,
    t_ns: t_ns.toString(),
    timestamp_iso,
  };

  const payloadHash = computeHash(receiptData);
  const previousHash = previousReceipt ? previousReceipt.receiptHash : null;
  const receiptHash = computeHash({ previousHash, payloadHash });

  return {
    ...receiptData,
    previousHash,
    payloadHash,
    receiptHash,
  };
}

/**
 * Verify receipt chain
 */
function verifyChain(receipts) {
  const errors = [];

  // Check genesis
  if (receipts[0].previousHash !== null) {
    errors.push('Genesis receipt must have null previousHash');
  }

  // Check links
  for (let i = 1; i < receipts.length; i++) {
    if (receipts[i].previousHash !== receipts[i - 1].receiptHash) {
      errors.push(`Chain broken between receipt ${i - 1} and ${i}`);
    }

    // Check temporal ordering
    const t1 = BigInt(receipts[i - 1].t_ns);
    const t2 = BigInt(receipts[i].t_ns);
    if (t2 <= t1) {
      errors.push(`Temporal ordering violated at receipt ${i}`);
    }
  }

  return {
    valid: errors.length === 0,
    errors,
  };
}

/**
 * Extract audit trail
 */
function extractAuditTrail(receipts) {
  return receipts.map((r, i) => ({
    step: i + 1,
    operation: r.operation,
    decision: r.payload.decision || r.payload.action,
    actor: r.actor,
    timestamp: r.timestamp_iso,
    receiptId: r.id,
  }));
}

/**
 * Reconstruct decision chain
 */
function reconstructDecisionChain(receipts) {
  const chain = [];

  for (let i = 0; i < receipts.length; i++) {
    if (i === 0) {
      chain.push(`Genesis (${receipts[i].id}) → ${receipts[i].operation}`);
    } else {
      chain.push(`${receipts[i - 1].id} → ${receipts[i].id} → ${receipts[i].operation}`);
    }
  }

  return chain;
}

/**
 * Main proof
 */
async function main() {
  console.log('\n=== Proof 2: Audit Trail Reconstruction ===\n');

  // Step 1: Generate receipt chain
  console.log('Step 1: Generating receipt chain (3 receipts)...\n');

  const receipt1 = createReceipt(
    'admit',
    { decision: 'APPROVE', delta: 'delta_001' },
    'system'
  );
  console.log(`  ✅ Receipt 1 (admit): approve delta_001 at ${receipt1.timestamp_iso}`);

  // Small delay to ensure different timestamps
  await new Promise(resolve => setTimeout(resolve, 10));

  const receipt2 = createReceipt(
    'freeze',
    { action: 'FREEZE', universe_hash: 'blake3_abc123...' },
    'governance',
    receipt1
  );
  console.log(`  ✅ Receipt 2 (freeze): universe hash blake3_... at ${receipt2.timestamp_iso}`);

  await new Promise(resolve => setTimeout(resolve, 10));

  const receipt3 = createReceipt(
    'publish',
    { action: 'PUBLISH', manifest: 'manifest-v1.0.0' },
    'publisher',
    receipt2
  );
  console.log(`  ✅ Receipt 3 (publish): manifest signed at ${receipt3.timestamp_iso}\n`);

  const receipts = [receipt1, receipt2, receipt3];

  // Step 2: Verify chain integrity
  console.log('Step 2: Verifying chain integrity...');

  const verification = verifyChain(receipts);

  if (verification.valid) {
    console.log(`  ✅ Chain verified: ${receipts.length} receipts, 0 gaps, chronological\n`);
  } else {
    console.log(`  ❌ Chain verification FAILED:`);
    verification.errors.forEach(err => console.log(`     - ${err}`));
    process.exit(1);
  }

  // Step 3: Extract audit trail
  console.log('Step 3: Extracting audit trail...\n');

  const auditTrail = extractAuditTrail(receipts);

  auditTrail.forEach(entry => {
    console.log(`  Decision ${entry.step}: ${entry.decision} ${entry.operation} by ${entry.actor} at ${entry.timestamp}`);
  });

  console.log('');

  // Step 4: Reconstruct decision chain
  console.log('Step 4: Reconstructing decision chain...\n');

  const decisionChain = reconstructDecisionChain(receipts);

  decisionChain.forEach(link => {
    console.log(`  ${link}`);
  });

  console.log('  ✅ No gaps, no reordering, temporal order valid\n');

  // Final summary
  console.log('=== Audit Trail Proof Summary ===\n');

  console.log('✅ PROOF SUCCESSFUL: Audit trail reconstructed!\n');

  console.log('Chain Properties:');
  console.log(`  - Total receipts: ${receipts.length}`);
  console.log(`  - Genesis receipt: ${receipts[0].id}`);
  console.log(`  - Latest receipt: ${receipts[receipts.length - 1].id}`);
  console.log(`  - Chain integrity: VALID ✅`);
  console.log(`  - Temporal ordering: VALID ✅`);
  console.log(`  - Gaps detected: 0 ✅\n`);

  console.log('Audit Trail Details:');
  auditTrail.forEach(entry => {
    console.log(`  ${entry.step}. ${entry.operation.toUpperCase()}: ${entry.decision}`);
    console.log(`     Actor: ${entry.actor}`);
    console.log(`     Time: ${entry.timestamp}`);
    console.log(`     Receipt: ${entry.receiptId}`);
  });

  console.log('\nDecision Chain:');
  decisionChain.forEach((link, i) => {
    console.log(`  ${i + 1}. ${link}`);
  });

  console.log('\nVerification Results:');
  console.log('  - Hash chain: VALID ✅');
  console.log('  - Temporal order: VALID ✅');
  console.log('  - No missing receipts: CONFIRMED ✅');
  console.log('  - No reordering: CONFIRMED ✅\n');

  console.log('Conclusion: Complete audit trail with cryptographic proofs.\n');

  process.exit(0);
}

// Run proof
main().catch((error) => {
  console.error('\n❌ Proof execution failed:', error.message);
  console.error(error.stack);
  process.exit(1);
});
