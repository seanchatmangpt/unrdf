/**
 * Canary Test - UNRDF Multiverse
 * Creates 1 universe, applies morphism Φ, generates receipt
 *
 * Usage: node examples/canary-test.mjs
 */

import {
  UniverseManager,
  createPredicateRenameMorphism,
  applyMorphism,
} from '../src/index.mjs';
import {
  generateBatchReceipt,
  verifyBatchReceipt,
} from '../../receipts/src/index.mjs';

async function runCanaryTest() {
  console.log('🚀 UNRDF Multiverse Canary Test\n');

  const startTime = process.hrtime.bigint();

  // Step 1: Create Universe
  console.log('Step 1: Creating universe...');
  const manager = new UniverseManager();
  const universe = await manager.createUniverse({
    createdBy: 'canary-test',
  });

  console.log(`  ✓ Created universe ${universe.id.Q_ID}`);
  console.log(`  State: ${universe.state}`);
  console.log(`  Q*_RDF: ${universe.id.Q_RDF}\n`);

  // Step 2: Transition to ACTIVE state
  console.log('Step 2: Transitioning to ACTIVE...');
  manager.transitionState(universe.id.Q_ID, 'ACTIVE');
  console.log(`  ✓ Universe is now ACTIVE\n`);

  // Step 3: Create and apply morphism Φ
  console.log('Step 3: Creating morphism Φ (predicate rename)...');
  const phi = await createPredicateRenameMorphism(
    'http://old-schema.org/name',
    'http://new-schema.org/fullName'
  );

  console.log(`  ✓ Created morphism ${phi.id}`);
  console.log(`  Type: ${phi.type}`);
  console.log(`  Name: ${phi.name}\n`);

  // Step 4: Apply morphism to sample quads
  console.log('Step 4: Applying morphism to sample quads...');
  const sampleQuads = [
    {
      subject: { value: 'http://example.com/person1' },
      predicate: { value: 'http://old-schema.org/name' },
      object: { value: 'Alice', termType: 'Literal' },
    },
    {
      subject: { value: 'http://example.com/person2' },
      predicate: { value: 'http://old-schema.org/age' },
      object: { value: '30', termType: 'Literal' },
    },
  ];

  const deltas = applyMorphism(phi, sampleQuads, 'ACTIVE');

  console.log(`  ✓ Generated ${deltas.length} deltas`);
  deltas.forEach((delta, i) => {
    console.log(`    ${i + 1}. ${delta.type.toUpperCase()} ${delta.predicate}`);
  });
  console.log();

  // Step 5: Generate batch receipt
  console.log('Step 5: Generating batch receipt...');
  const receipt = await generateBatchReceipt({
    universeID: universe.id.Q_ID,
    operations: deltas,
    operationType: 'morphism-application',
  });

  console.log(`  ✓ Generated receipt ${receipt.Q_ID}`);
  console.log(`  Batch size: ${receipt.Q_PROV.batchSize}`);
  console.log(`  Content hash: ${receipt.Q_PROV.contentHash.slice(0, 16)}...`);
  console.log(`  Timestamp: ${receipt.Q_PROV.timestamp}\n`);

  // Step 6: Verify receipt
  console.log('Step 6: Verifying receipt...');
  const verification = await verifyBatchReceipt(receipt, deltas);

  console.log(`  ✓ Receipt valid: ${verification.valid}`);
  if (verification.valid) {
    console.log(`  Receipt ID: ${verification.receiptID}`);
  }
  console.log();

  // Step 7: Final stats
  const endTime = process.hrtime.bigint();
  const durationMs = Number(endTime - startTime) / 1_000_000;

  console.log('📊 Canary Test Complete!\n');
  console.log(`Execution time: ${durationMs.toFixed(2)}ms`);
  console.log(`Universe: ${universe.id.Q_ID}`);
  console.log(`Morphism: ${phi.id}`);
  console.log(`Receipt: ${receipt.Q_ID}`);
  console.log(`Verification: ${verification.valid ? '✅ PASS' : '❌ FAIL'}\n`);

  if (durationMs < 5000) {
    console.log('✅ Canary test PASSED (< 5s)\n');
    return 0;
  } else {
    console.log(`⚠️  Canary test SLOW (${durationMs}ms > 5000ms)\n`);
    return 1;
  }
}

// Run canary test
runCanaryTest()
  .then((exitCode) => {
    process.exit(exitCode);
  })
  .catch((error) => {
    console.error('❌ Canary test FAILED:', error.message);
    console.error(error.stack);
    process.exit(1);
  });
