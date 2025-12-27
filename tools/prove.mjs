#!/usr/bin/env node
/**
 * Deterministic E2E Proof Command
 *
 * Executes unified fusion scenario and emits cryptographic proof.
 *
 * Usage:
 *   DETERMINISTIC=1 node tools/prove.mjs
 *
 * Exit codes:
 *   0 - Proof successful
 *   1 - Proof failed
 */

import { writeFile, mkdir } from 'node:fs/promises';
import { prove, createVisualizer, serializeVisualization } from '../packages/fusion/src/index.mjs';

try {
  console.log('🔬 Executing deterministic E2E proof...\n');

  const result = await prove();

  console.log('✅ Proof Complete\n');
  console.log('Final Hash:', result.hash);
  console.log('Merkle Root:', result.merkleRoot);
  console.log('Receipts Emitted:', result.artifacts.length);
  console.log('Verification:', result.ledger.scenario.verificationPassed ? 'PASSED' : 'FAILED');
  console.log('\nLedger:');
  console.log(JSON.stringify(result.ledger, null, 2));

  // Generate visualizations
  console.log('\n📊 Generating visualizations...');
  const viz = await createVisualizer({});

  // Render receipts visualization
  const receiptsViz = viz.renderReceipts(result.artifacts);
  const receiptsSerialized = await serializeVisualization({
    type: 'receipts',
    data: receiptsViz.json,
  });

  // Render allocation visualization
  const allocationViz = viz.renderAllocation(result.ledger.scenario);
  const allocationSerialized = await serializeVisualization({
    type: 'allocation',
    data: allocationViz,
  });

  // Create ARTIFACTS directory
  await mkdir('ARTIFACTS', { recursive: true });

  // Export visualization artifacts
  const visualizationArtifact = {
    proofHash: result.hash,
    merkleRoot: result.merkleRoot,
    receipts: receiptsSerialized,
    allocation: allocationSerialized,
    timestamp: result.ledger.timestamp,
  };

  await writeFile(
    'ARTIFACTS/visualization.json',
    JSON.stringify(visualizationArtifact, null, 2)
  );

  console.log('✅ Visualization exported to ARTIFACTS/visualization.json');
  console.log('   - Receipts hash:', receiptsSerialized.hash);
  console.log('   - Allocation hash:', allocationSerialized.hash);

  process.exit(result.success ? 0 : 1);
} catch (error) {
  console.error('❌ Proof failed:', error.message);
  console.error(error.stack);
  process.exit(1);
}
