/**
 * Blockchain-Verified Receipts Example
 * Demonstrates cryptographic workflow receipts with Ed25519 signatures
 */

import {
  generateSigningKey,
  createBlockchainReceipt,
  verifyBlockchainReceipt,
  createReceiptChain,
  verifyReceiptChain,
  createMerkleRoot,
  generateMerkleProof,
  verifyMerkleProof
} from '../src/blockchain-receipts.mjs';

/**
 * Example 1: Generate signing key
 */
async function keyGenerationExample() {
  console.log('\n=== Key Generation Example ===\n');

  const keyPair = await generateSigningKey('workflow-signer-001');

  console.log('Generated Key Pair:');
  console.log('  Key ID:', keyPair.keyId);
  console.log('  Public Key:', keyPair.publicKey);
  console.log('  Private Key:', keyPair.privateKey.substring(0, 16) + '...');
  console.log('  Created At:', keyPair.createdAt);

  console.log('\n⚠️  IMPORTANT: Store private key securely!');
  console.log('Never commit private keys to version control.');

  return keyPair;
}

/**
 * Example 2: Create and verify blockchain receipt
 */
async function createAndVerifyReceipt() {
  console.log('\n=== Create and Verify Receipt ===\n');

  // Generate key pair
  const keyPair = await generateSigningKey('task-signer');

  // Create workflow event
  const event = {
    type: 'TASK_COMPLETED',
    caseId: 'case-001',
    taskId: 'approve-loan'
  };

  // Create payload with decision
  const payload = {
    decision: 'APPROVE',
    justification: {
      reasoning: 'All approval criteria met',
      conditionChecked: 'creditScore >= 700 && income >= 50000',
      hookValidated: 'credit-check-hook'
    },
    actor: 'john.doe@example.com'
  };

  console.log('Creating blockchain receipt...');

  // Create blockchain receipt
  const receipt = await createBlockchainReceipt(event, payload, keyPair);

  console.log('\nBlockchain Receipt Created:');
  console.log('  Receipt Hash:', receipt.hash);
  console.log('  Signature:', receipt.signature.substring(0, 32) + '...');
  console.log('  Public Key:', receipt.publicKey);
  console.log('  Key ID:', receipt.keyId);
  console.log('  Signed At:', receipt.signedAt);

  // Verify receipt
  console.log('\nVerifying receipt...');
  const verification = await verifyBlockchainReceipt(receipt);

  console.log('\nVerification Result:');
  console.log('  Valid:', verification.valid ? '✅' : '❌');
  console.log('  Hash Valid:', verification.hashValid ? '✅' : '❌');
  console.log('  Signature Valid:', verification.signatureValid ? '✅' : '❌');
  console.log('  Verified At:', verification.verifiedAt);

  return receipt;
}

/**
 * Example 3: Create and verify receipt chain
 */
async function receiptChainExample() {
  console.log('\n=== Receipt Chain Example ===\n');

  const keyPair = await generateSigningKey('chain-signer');

  // Define workflow events in sequence
  const events = [
    { type: 'TASK_ENABLED', caseId: 'case-002', taskId: 'task-1' },
    { type: 'TASK_STARTED', caseId: 'case-002', taskId: 'task-1' },
    { type: 'TASK_COMPLETED', caseId: 'case-002', taskId: 'task-1' },
    { type: 'TASK_ENABLED', caseId: 'case-002', taskId: 'task-2' },
    { type: 'TASK_STARTED', caseId: 'case-002', taskId: 'task-2' },
    { type: 'TASK_COMPLETED', caseId: 'case-002', taskId: 'task-2' }
  ];

  console.log(`Creating receipt chain for ${events.length} events...`);

  // Create chain
  const chain = await createReceiptChain(events, keyPair);

  console.log(`\nReceipt Chain Created: ${chain.length} receipts`);
  chain.forEach((receipt, idx) => {
    console.log(`\n  Receipt ${idx + 1}:`);
    console.log(`    Event: ${receipt.eventType}`);
    console.log(`    Hash: ${receipt.hash}`);
    console.log(`    Signature: ${receipt.signature.substring(0, 16)}...`);
  });

  // Verify chain
  console.log('\n\nVerifying receipt chain...');
  const chainVerification = await verifyReceiptChain(chain);

  console.log('\nChain Verification Result:');
  console.log('  Chain Valid:', chainVerification.valid ? '✅' : '❌');
  console.log('  Chain Length:', chainVerification.chainLength);
  console.log('  Verified Count:', chainVerification.verifiedCount);
  console.log('  Verified At:', chainVerification.verifiedAt);

  if (chainVerification.valid) {
    console.log('\n✅ Entire chain is cryptographically valid!');
  }

  return chain;
}

/**
 * Example 4: Merkle tree for batch anchoring
 */
async function merkleTreeExample() {
  console.log('\n=== Merkle Tree Example ===\n');

  const keyPair = await generateSigningKey('merkle-signer');

  // Create multiple receipts
  const events = [
    { type: 'TASK_COMPLETED', caseId: 'case-003', taskId: 'task-a' },
    { type: 'TASK_COMPLETED', caseId: 'case-004', taskId: 'task-b' },
    { type: 'TASK_COMPLETED', caseId: 'case-005', taskId: 'task-c' },
    { type: 'TASK_COMPLETED', caseId: 'case-006', taskId: 'task-d' }
  ];

  const receipts = await createReceiptChain(events, keyPair);

  console.log(`Created ${receipts.length} receipts`);

  // Create Merkle root
  const merkleRoot = await createMerkleRoot(receipts);

  console.log('\nMerkle Root:', merkleRoot);
  console.log('\nThis root can be anchored on a blockchain for batch verification');

  // Generate proof for receipt at index 2
  const proofIndex = 2;
  const proof = await generateMerkleProof(receipts, proofIndex);

  console.log(`\nMerkle Proof for Receipt ${proofIndex}:`);
  console.log('  Receipt Hash:', proof.receiptHash);
  console.log('  Merkle Root:', proof.merkleRoot);
  console.log('  Proof Steps:', proof.proof.length);
  proof.proof.forEach((step, idx) => {
    console.log(`    Step ${idx + 1}: ${step.position} - ${step.hash.substring(0, 16)}...`);
  });

  // Verify proof
  const proofValid = await verifyMerkleProof(proof, merkleRoot);

  console.log(`\nProof Verification: ${proofValid ? '✅ Valid' : '❌ Invalid'}`);

  return { receipts, merkleRoot, proof };
}

/**
 * Example 5: Production workflow with blockchain receipts
 */
async function productionWorkflowExample() {
  console.log('\n=== Production Workflow Example ===\n');

  console.log('Production Workflow Pattern:');
  console.log(`
1. Initialize signing key (once per deployment):
   const keyPair = await generateSigningKey('production-key-001');
   // Store private key in secure vault (e.g., AWS Secrets Manager)

2. Create receipts during workflow execution:
   const receipt = await createBlockchainReceipt(event, payload, keyPair);
   // Store receipt in database

3. Periodically batch receipts for blockchain anchoring:
   const merkleRoot = await createMerkleRoot(receipts);
   // Anchor merkleRoot on Ethereum/Bitcoin/etc.
   // Store blockchain transaction ID with receipts

4. Verification (anytime):
   const verification = await verifyBlockchainReceipt(receipt);
   if (!verification.valid) {
     throw new Error('Receipt tampering detected!');
   }

5. Audit trail:
   const chain = await verifyReceiptChain(allReceipts);
   // Generate audit report
  `);

  console.log('\nBenefits:');
  console.log('  • Cryptographic proof of workflow execution');
  console.log('  • Non-repudiation with Ed25519 signatures');
  console.log('  • Tamper-evident audit trail');
  console.log('  • Optional blockchain anchoring for external verification');
  console.log('  • Compliant with SOC2, ISO 27001, GDPR audit requirements');
}

/**
 * Example 6: Receipt tampering detection
 */
async function tamperingDetectionExample() {
  console.log('\n=== Tampering Detection Example ===\n');

  const keyPair = await generateSigningKey('tampering-test');

  const event = {
    type: 'TASK_COMPLETED',
    caseId: 'case-007',
    taskId: 'critical-task'
  };

  const payload = {
    decision: 'APPROVE',
    justification: {
      reasoning: 'Original reasoning'
    }
  };

  // Create valid receipt
  const receipt = await createBlockchainReceipt(event, payload, keyPair);

  console.log('Original receipt created');

  // Verify original
  let verification = await verifyBlockchainReceipt(receipt);
  console.log('Original verification:', verification.valid ? '✅ Valid' : '❌ Invalid');

  // Tamper with receipt
  const tamperedReceipt = {
    ...receipt,
    payload: {
      ...receipt.payload,
      decision: 'REJECT' // Changed!
    }
  };

  console.log('\n⚠️  Receipt tampered - decision changed to REJECT');

  // Verify tampered receipt
  try {
    verification = await verifyBlockchainReceipt(tamperedReceipt);
    console.log('Tampered verification:', verification.valid ? '✅ Valid' : '❌ Invalid');

    if (!verification.valid) {
      console.log('\n✅ Tampering detected! Receipt is invalid.');
      console.log('   Hash mismatch between stored hash and computed hash');
    }
  } catch (error) {
    console.log('\n✅ Tampering detected! Error:', error.message);
  }
}

/**
 * Run all examples
 */
async function main() {
  try {
    console.log('Blockchain-Verified Workflow Receipts Examples');
    console.log('===============================================');

    await keyGenerationExample();
    await createAndVerifyReceipt();
    await receiptChainExample();
    await merkleTreeExample();
    productionWorkflowExample();
    await tamperingDetectionExample();

    console.log('\n\n✅ All examples completed successfully');
  } catch (error) {
    console.error('❌ Example failed:', error.message);
    console.error(error.stack);
    process.exit(1);
  }
}

// Run if executed directly
if (import.meta.url === `file://${process.argv[1]}`) {
  main();
}

export {
  keyGenerationExample,
  createAndVerifyReceipt,
  receiptChainExample,
  merkleTreeExample,
  productionWorkflowExample,
  tamperingDetectionExample
};
