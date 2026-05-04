/**
 * Performance Proof - Create 100 Universes
 * Target: <5s execution time
 *
 * Usage: node examples/proof-100-universes.mjs
 */

import {
  UniverseManager,
  createIdentityMorphism,
  applyMorphism,
} from '../src/index.mjs';
import {
  generateBatchReceipt,
  buildMerkleTree,
} from '../../receipts/src/index.mjs';

async function runProof100Universes() {
  console.log('🔬 UNRDF Multiverse - 100 Universe Performance Proof\n');
  console.log('Target: Create 100 universes, apply morphisms, generate receipts\n');
  console.log('SLA: <5s total execution time\n');

  const startTime = process.hrtime.bigint();

  const manager = new UniverseManager();
  const results = {
    universes: [],
    morphisms: [],
    receipts: [],
    merkleRoots: [],
  };

  // Step 1: Create 100 universes in parallel
  console.log('Step 1: Creating 100 universes...');
  const stepStart = process.hrtime.bigint();

  const universePromises = Array.from({ length: 100 }, (_, i) =>
    manager.createUniverse({ createdBy: `perf-test-${i}` })
  );

  const universes = await Promise.all(universePromises);
  results.universes = universes;

  const step1Duration = Number(process.hrtime.bigint() - stepStart) / 1_000_000;
  console.log(`  ✓ Created ${universes.length} universes in ${step1Duration.toFixed(2)}ms\n`);

  // Step 2: Transition all to ACTIVE
  console.log('Step 2: Transitioning all universes to ACTIVE...');
  const step2Start = process.hrtime.bigint();

  universes.forEach((u) => {
    manager.transitionState(u.id.Q_ID, 'ACTIVE');
  });

  const step2Duration = Number(process.hrtime.bigint() - step2Start) / 1_000_000;
  console.log(`  ✓ Transitioned ${universes.length} universes in ${step2Duration.toFixed(2)}ms\n`);

  // Step 3: Create morphisms
  console.log('Step 3: Creating morphisms...');
  const step3Start = process.hrtime.bigint();

  const morphismPromises = Array.from({ length: 100 }, () =>
    createIdentityMorphism()
  );

  const morphisms = await Promise.all(morphismPromises);
  results.morphisms = morphisms;

  const step3Duration = Number(process.hrtime.bigint() - step3Start) / 1_000_000;
  console.log(`  ✓ Created ${morphisms.length} morphisms in ${step3Duration.toFixed(2)}ms\n`);

  // Step 4: Apply morphisms and generate receipts
  console.log('Step 4: Applying morphisms and generating receipts...');
  const step4Start = process.hrtime.bigint();

  const sampleQuads = [
    {
      subject: { value: 'ex:s1' },
      predicate: { value: 'ex:p1' },
      object: { value: 'ex:o1', termType: 'Literal' },
    },
  ];

  const receiptPromises = universes.map(async (u, i) => {
    const deltas = applyMorphism(morphisms[i], sampleQuads, 'ACTIVE');
    return generateBatchReceipt({
      universeID: u.id.Q_ID,
      operations: deltas.length > 0 ? deltas : [{ type: 'add', subject: 'ex:s', predicate: 'ex:p', object: 'ex:o' }],
      operationType: 'perf-test',
    });
  });

  const receipts = await Promise.all(receiptPromises);
  results.receipts = receipts;

  const step4Duration = Number(process.hrtime.bigint() - step4Start) / 1_000_000;
  console.log(`  ✓ Generated ${receipts.length} receipts in ${step4Duration.toFixed(2)}ms\n`);

  // Step 5: Build Merkle tree of all receipts
  console.log('Step 5: Building Merkle tree of receipts...');
  const step5Start = process.hrtime.bigint();

  const merkleTree = await buildMerkleTree(
    receipts.map((r) => ({ receiptID: r.Q_ID, hash: r.Q_PROV.contentHash }))
  );
  results.merkleRoots.push(merkleTree.hash);

  const step5Duration = Number(process.hrtime.bigint() - step5Start) / 1_000_000;
  console.log(`  ✓ Built Merkle tree in ${step5Duration.toFixed(2)}ms`);
  console.log(`  Merkle root: ${merkleTree.hash.slice(0, 32)}...\n`);

  // Final statistics
  const endTime = process.hrtime.bigint();
  const totalDurationMs = Number(endTime - startTime) / 1_000_000;

  console.log('═══════════════════════════════════════════════════════════\n');
  console.log('📊 Performance Report\n');
  console.log(`Total execution time: ${totalDurationMs.toFixed(2)}ms`);
  console.log(`Target SLA: 5000ms`);
  console.log(`Performance ratio: ${((totalDurationMs / 5000) * 100).toFixed(1)}% of SLA\n`);

  console.log('Breakdown:');
  console.log(`  Universe creation:   ${step1Duration.toFixed(2)}ms (${(step1Duration / totalDurationMs * 100).toFixed(1)}%)`);
  console.log(`  State transitions:   ${step2Duration.toFixed(2)}ms (${(step2Duration / totalDurationMs * 100).toFixed(1)}%)`);
  console.log(`  Morphism creation:   ${step3Duration.toFixed(2)}ms (${(step3Duration / totalDurationMs * 100).toFixed(1)}%)`);
  console.log(`  Receipt generation:  ${step4Duration.toFixed(2)}ms (${(step4Duration / totalDurationMs * 100).toFixed(1)}%)`);
  console.log(`  Merkle tree build:   ${step5Duration.toFixed(2)}ms (${(step5Duration / totalDurationMs * 100).toFixed(1)}%)\n`);

  console.log('Throughput:');
  console.log(`  ${(100 / (totalDurationMs / 1000)).toFixed(2)} universes/second`);
  console.log(`  ${(100 / (totalDurationMs / 1000)).toFixed(2)} receipts/second\n`);

  console.log('Counts:');
  console.log(`  Universes: ${manager.count()}`);
  console.log(`  Morphisms: ${morphisms.length}`);
  console.log(`  Receipts:  ${receipts.length}\n`);

  if (totalDurationMs < 5000) {
    console.log('✅ Performance proof PASSED (<5s)\n');
    console.log(`  Completed in ${(totalDurationMs / 1000).toFixed(3)}s`);
    console.log(`  Margin: ${((5000 - totalDurationMs) / 1000).toFixed(3)}s under target\n`);
    return 0;
  } else {
    console.log(`⚠️  Performance proof FAILED (${totalDurationMs.toFixed(2)}ms > 5000ms)\n`);
    console.log(`  Exceeded target by ${((totalDurationMs - 5000) / 1000).toFixed(3)}s\n`);
    return 1;
  }
}

// Run performance proof
runProof100Universes()
  .then((exitCode) => {
    process.exit(exitCode);
  })
  .catch((error) => {
    console.error('❌ Performance proof FAILED:', error.message);
    console.error(error.stack);
    process.exit(1);
  });
