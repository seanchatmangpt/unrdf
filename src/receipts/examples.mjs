/**
 * @fileoverview Example receipt generation and serialization
 *
 * **Purpose**: Demonstrate receipt usage and generate example outputs
 */

import { Receipt } from './receipt.mjs';
import { ReceiptChain } from './receipt-chain.mjs';
import { ReceiptGenerator } from './receipt-generator.mjs';
import { computeMerkleRoot, generateMerkleProof } from './merkle-root.mjs';

/**
 * Example 1: Create a single receipt
 */
async function example1_singleReceipt() {
  console.log('=== Example 1: Single Receipt ===\n');

  const receipt = await Receipt.create({
    inputHashes: {
      ontologyReleases: [
        'a3f5d8c9e1b2f4a7c8d9e0f1a2b3c4d5e6f7a8b9c0d1e2f3a4b5c6d7e8f9a0b1',
        'b4c6d9e0f2a3b5c7d8e9f0a1b2c3d4e5f6a7b8c9d0e1f2a3b4c5d6e7f8a9b0c1',
      ],
      deltaCapsule: 'c5d7e0f1a2b3c4d5e6f7a8b9c0d1e2f3a4b5c6d7e8f9a0b1c2d3e4f5a6b7c8d9',
    },
    decision: 'allow',
    outputHash: 'd6e8f1a2b3c4d5e6f7a8b9c0d1e2f3a4b5c6d7e8f9a0b1c2d3e4f5a6b7c8d9e0',
    toolchainVersion: {
      node: 'v18.19.0',
      packages: {
        '@unrdf/core': '^5.0.1',
        '@unrdf/oxigraph': 'workspace:*',
        'hash-wasm': '^4.12.0',
      },
    },
    timestamp: new Date('2025-12-26T14:30:00.123Z'),
  });

  console.log('Receipt Hash:', receipt.receiptHash);
  console.log('Epoch:', receipt.epoch);
  console.log('Decision:', receipt.decision);
  console.log('\n--- JSON-LD Format ---\n');
  console.log(JSON.stringify(receipt.toJSONLD(), null, 2));
  console.log('\n--- Turtle Format ---\n');
  console.log(receipt.toTurtle());
  console.log('\n');

  return receipt;
}

/**
 * Example 2: Create a receipt chain
 */
async function example2_receiptChain() {
  console.log('=== Example 2: Receipt Chain ===\n');

  const chain = new ReceiptChain();

  // Receipt 1: Allow decision
  const receipt1 = await Receipt.create({
    inputHashes: {
      ontologyReleases: ['ont_release_v1.0_hash'],
      deltaCapsule: 'delta_add_person_class_hash',
    },
    decision: 'allow',
    outputHash: 'universe_state_after_person_class_hash',
    toolchainVersion: {
      node: 'v18.19.0',
      packages: { '@unrdf/core': '^5.0.1' },
    },
    timestamp: new Date('2025-12-26T14:30:00.000Z'),
  });

  await chain.append(receipt1);

  // Receipt 2: Deny decision (linked to receipt1)
  const receipt2 = await Receipt.create({
    inputHashes: {
      ontologyReleases: ['ont_release_v1.0_hash'],
      deltaCapsule: 'delta_invalid_property_hash',
    },
    decision: 'deny',
    outputHash: 'universe_state_unchanged_hash',
    toolchainVersion: {
      node: 'v18.19.0',
      packages: { '@unrdf/core': '^5.0.1' },
    },
    beforeHash: receipt1.receiptHash,
    timestamp: new Date('2025-12-26T14:30:01.000Z'),
  });

  await chain.append(receipt2);

  // Receipt 3: Allow decision (linked to receipt2)
  const receipt3 = await Receipt.create({
    inputHashes: {
      ontologyReleases: ['ont_release_v1.0_hash', 'ont_release_v1.1_hash'],
      deltaCapsule: 'delta_add_employee_subclass_hash',
    },
    decision: 'allow',
    outputHash: 'universe_state_with_employee_hash',
    toolchainVersion: {
      node: 'v18.19.0',
      packages: { '@unrdf/core': '^5.0.1' },
    },
    beforeHash: receipt2.receiptHash,
    timestamp: new Date('2025-12-26T14:30:02.000Z'),
  });

  await chain.append(receipt3);

  console.log(`Chain length: ${chain.length}`);
  console.log('\nReceipt 1:');
  console.log('  Hash:', receipt1.receiptHash);
  console.log('  Decision:', receipt1.decision);
  console.log('  beforeHash:', receipt1.beforeHash);

  console.log('\nReceipt 2:');
  console.log('  Hash:', receipt2.receiptHash);
  console.log('  Decision:', receipt2.decision);
  console.log('  beforeHash:', receipt2.beforeHash);
  console.log('  ✓ Links to Receipt 1:', receipt2.beforeHash === receipt1.receiptHash);

  console.log('\nReceipt 3:');
  console.log('  Hash:', receipt3.receiptHash);
  console.log('  Decision:', receipt3.decision);
  console.log('  beforeHash:', receipt3.beforeHash);
  console.log('  ✓ Links to Receipt 2:', receipt3.beforeHash === receipt2.receiptHash);

  // Verify chain
  const verification = await chain.verify();
  console.log('\nChain verification:', verification.valid ? '✓ VALID' : '✗ INVALID');
  if (verification.errors.length > 0) {
    console.log('Errors:', verification.errors);
  }

  console.log('\n--- Chain JSON-LD ---\n');
  console.log(JSON.stringify(chain.toJSONLD(), null, 2));
  console.log('\n');

  return chain;
}

/**
 * Example 3: Merkle tree batching
 */
async function example3_merkleBatching() {
  console.log('=== Example 3: Merkle Tree Batching ===\n');

  // Create 10 receipts
  const receipts = [];
  for (let i = 0; i < 10; i++) {
    const receipt = await Receipt.create({
      inputHashes: {
        ontologyReleases: [`ont_v1.${i}_hash`],
        deltaCapsule: `delta_${i}_hash`,
      },
      decision: i % 3 === 0 ? 'deny' : 'allow',
      outputHash: `state_${i}_hash`,
      toolchainVersion: {
        node: 'v18.19.0',
        packages: { '@unrdf/core': '^5.0.1' },
      },
      timestamp: new Date(`2025-12-26T14:30:${String(i).padStart(2, '0')}.000Z`),
    });
    receipts.push(receipt);
  }

  // Compute Merkle root
  const receiptHashes = receipts.map(r => r.receiptHash);
  const merkleRoot = await computeMerkleRoot(receiptHashes);

  console.log(`Batched ${receipts.length} receipts`);
  console.log('Merkle Root:', merkleRoot);

  // Generate proof for receipt at index 5
  const { root, proof } = await generateMerkleProof(receiptHashes, 5);

  console.log('\nMerkle Proof for Receipt 5:');
  console.log('  Receipt Hash:', receiptHashes[5]);
  console.log('  Root:', root);
  console.log('  Proof steps:', proof.length);
  proof.forEach((step, idx) => {
    console.log(`    ${idx + 1}. ${step.position}: ${step.hash.substring(0, 16)}...`);
  });

  console.log('\n');

  return { receipts, merkleRoot, proof };
}

/**
 * Example 4: Receipt generator workflow
 */
async function example4_receiptGenerator() {
  console.log('=== Example 4: Receipt Generator Workflow ===\n');

  const generator = new ReceiptGenerator();

  // Emit admissibility receipt
  const admissibilityReceipt = await generator.emitAdmissibilityReceipt({
    ontologyReleases: ['ont_core_v1.0_hash', 'ont_domain_v2.3_hash'],
    deltaCapsule: 'delta_add_organization_hash',
    decision: 'allow',
    universeState: {
      classes: ['Person', 'Organization'],
      properties: ['hasName', 'hasMember'],
    },
    timestamp: new Date('2025-12-26T14:30:00.000Z'),
  });

  console.log('Admissibility Receipt:');
  console.log('  Hash:', admissibilityReceipt.receiptHash);
  console.log('  Decision:', admissibilityReceipt.decision);

  // Emit validation receipt
  const validationReceipt = await generator.emitValidationReceipt({
    ontologyReleases: ['ont_core_v1.0_hash', 'ont_domain_v2.3_hash'],
    validationReport: 'validation_report_shacl_hash',
    decision: 'allow',
    validationState: {
      validated: true,
      violations: 0,
    },
    timestamp: new Date('2025-12-26T14:30:01.000Z'),
  });

  console.log('\nValidation Receipt:');
  console.log('  Hash:', validationReceipt.receiptHash);
  console.log('  Decision:', validationReceipt.decision);
  console.log('  Linked to Admissibility:', validationReceipt.beforeHash === admissibilityReceipt.receiptHash);

  // Emit projection receipt
  const projectionReceipt = await generator.emitProjectionReceipt({
    ontologyReleases: ['ont_core_v1.0_hash', 'ont_domain_v2.3_hash'],
    projectionInput: 'projection_query_hash',
    decision: 'allow',
    projectionOutput: {
      resultCount: 42,
      bindings: ['?person', '?org'],
    },
    timestamp: new Date('2025-12-26T14:30:02.000Z'),
  });

  console.log('\nProjection Receipt:');
  console.log('  Hash:', projectionReceipt.receiptHash);
  console.log('  Decision:', projectionReceipt.decision);
  console.log('  Linked to Validation:', projectionReceipt.beforeHash === validationReceipt.receiptHash);

  // Verify chain
  const verification = await generator.verifyChain();
  console.log('\nGenerator Chain Verification:', verification.valid ? '✓ VALID' : '✗ INVALID');

  console.log('\nChain length:', generator.getChain().length);

  console.log('\n');

  return generator;
}

/**
 * Example 5: Determinism verification
 */
async function example5_determinismVerification() {
  console.log('=== Example 5: Determinism Verification ===\n');

  const timestamp = new Date('2025-12-26T14:30:00.000Z');
  const options = {
    inputHashes: {
      ontologyReleases: ['hash1', 'hash2'],
      deltaCapsule: 'hash3',
    },
    decision: 'allow',
    outputHash: 'hash4',
    toolchainVersion: {
      node: 'v18.19.0',
      packages: { '@unrdf/core': '^5.0.1' },
    },
    timestamp,
  };

  const receipt1 = await Receipt.create(options);
  const receipt2 = await Receipt.create(options);
  const receipt3 = await Receipt.create(options);

  console.log('Receipt 1 Hash:', receipt1.receiptHash);
  console.log('Receipt 2 Hash:', receipt2.receiptHash);
  console.log('Receipt 3 Hash:', receipt3.receiptHash);

  const allEqual = receipt1.receiptHash === receipt2.receiptHash && receipt2.receiptHash === receipt3.receiptHash;
  console.log('\n✓ Determinism verified:', allEqual ? 'PASS' : 'FAIL');

  if (!allEqual) {
    console.log('ERROR: Receipts should be identical!');
  }

  console.log('\n');
}

/**
 * Run all examples
 */
async function main() {
  console.log('\n╔══════════════════════════════════════════════════════╗');
  console.log('║  UNRDF Receipt System - Example Outputs             ║');
  console.log('╚══════════════════════════════════════════════════════╝\n');

  await example1_singleReceipt();
  await example2_receiptChain();
  await example3_merkleBatching();
  await example4_receiptGenerator();
  await example5_determinismVerification();

  console.log('✓ All examples completed successfully\n');
}

// Run if executed directly
if (import.meta.url === `file://${process.argv[1]}`) {
  main().catch(console.error);
}

export {
  example1_singleReceipt,
  example2_receiptChain,
  example3_merkleBatching,
  example4_receiptGenerator,
  example5_determinismVerification,
};
