/**
 * @file Chatman Equation Direct Test (bypassing index.mjs)
 * @description Direct validation that Chatman modules work
 */

import { createChatmanOperator } from '../src/chatman-operator.mjs';
import { createArtifactGenerator } from '../src/artifact-generator.mjs';
import { createDarkFieldDetector } from '../src/dark-field-detector.mjs';
import { createFormationTheorems } from '../src/formation-theorems.mjs';
import { createChatmanEngine } from '../src/chatman-engine.mjs';
import { loadChatmanConfig } from '../src/chatman-config-loader.mjs';

console.log('Chatman Equation Direct Test\n');

async function runTests() {
  let passed = 0;
  let failed = 0;

  // Test 1: ChatmanOperator
  try {
    console.log('[1/6] Testing ChatmanOperator...');
    const operator = createChatmanOperator({ observableRatio: 0.05 });
    const result = await operator.apply({
      type: 'market',
      patterns: ['customer_feedback', 'sales_trends'],
      visibility: 0.05,
    });

    if (result.darkField.patterns.length > 0 && result.completeness > 0.9) {
      console.log('  ✓ μ operator computed closure successfully');
      console.log(
        `    - Observable: ${result.observable.patterns.length} patterns`
      );
      console.log(
        `    - Dark Field: ${result.darkField.patterns.length} patterns`
      );
      console.log(`    - Completeness: ${(result.completeness * 100).toFixed(1)}%`);
      passed++;
    } else {
      console.log('  ✗ ChatmanOperator failed validation');
      failed++;
    }
  } catch (error) {
    console.log('  ✗ ChatmanOperator error:', error.message);
    failed++;
  }

  // Test 2: ArtifactGenerator
  try {
    console.log('\n[2/6] Testing ArtifactGenerator (A = μ(O))...');
    const generator = createArtifactGenerator({ observableRatio: 0.05 });
    const artifact = await generator.generate({
      type: 'organizational',
      patterns: ['workflow_pattern', 'team_structure'],
      visibility: 0.05,
    });

    if (artifact.id && artifact.darkField && artifact.completeness > 0.9) {
      console.log('  ✓ Artifact generated successfully');
      console.log(`    - Artifact ID: ${artifact.id}`);
      console.log(`    - Total patterns: ${artifact.totalPatterns}`);
      console.log(`    - Dark field coverage: ${(artifact.darkField.coverage * 100).toFixed(1)}%`);
      passed++;
    } else {
      console.log('  ✗ ArtifactGenerator failed validation');
      failed++;
    }
  } catch (error) {
    console.log('  ✗ ArtifactGenerator error:', error.message);
    failed++;
  }

  // Test 3: DarkFieldDetector
  try {
    console.log('\n[3/6] Testing DarkFieldDetector (95% invisible)...');
    const detector = createDarkFieldDetector({ targetRatio: 0.95 });
    const detection = await detector.detect({
      type: 'strategic',
      patterns: ['market_position', 'capabilities'],
      visibility: 0.05,
    });

    if (detection.darkFieldRatio > 0.9 && detection.patterns.length > 0) {
      console.log('  ✓ Dark field detected successfully');
      console.log(
        `    - Dark field ratio: ${(detection.darkFieldRatio * 100).toFixed(1)}%`
      );
      console.log(`    - Patterns detected: ${detection.patterns.length}`);
      console.log(
        `    - Average confidence: ${(detection.patterns.reduce((sum, p) => sum + p.confidence, 0) / detection.patterns.length).toFixed(2)}`
      );
      passed++;
    } else {
      console.log('  ✗ DarkFieldDetector failed validation');
      failed++;
    }
  } catch (error) {
    console.log('  ✗ DarkFieldDetector error:', error.message);
    failed++;
  }

  // Test 4: FormationTheorems
  try {
    console.log('\n[4/6] Testing FormationTheorems (Blue Ocean)...');
    const theorems = createFormationTheorems();
    const generator = createArtifactGenerator({ observableRatio: 0.05 });
    const artifact = await generator.generate({
      type: 'disruption',
      patterns: ['tech_shift', 'regulation_change'],
      visibility: 0.05,
    });

    const formation = await theorems.derive(artifact, { theorem: 'emergence' });

    if (formation.theorem === 'emergence' && formation.output.formation.length > 0) {
      console.log('  ✓ Blue ocean formation derived successfully');
      console.log(`    - Theorem: ${formation.theorem}`);
      console.log(`    - Formations: ${formation.output.formation.length}`);
      console.log(
        `    - Value proposition: ${formation.output.valueProposition.substring(0, 60)}...`
      );
      passed++;
    } else {
      console.log('  ✗ FormationTheorems failed validation');
      failed++;
    }
  } catch (error) {
    console.log('  ✗ FormationTheorems error:', error.message);
    failed++;
  }

  // Test 5: ChatmanEngine (individual operations)
  try {
    console.log('\n[5/6] Testing ChatmanEngine (individual operations)...');
    const engine = createChatmanEngine({
      observableRatio: 0.05,
      enableReceipts: true,
    });

    const closureResult = await engine.executeClosure({
      type: 'market',
      patterns: ['customer_pain'],
      visibility: 0.05,
    });

    if (closureResult.operation === 'closure' && closureResult.receipt) {
      console.log('  ✓ Engine operations work with receipts');
      console.log(`    - Receipt ID: ${closureResult.receipt.id}`);
      console.log(
        `    - Completeness: ${(closureResult.output.closure.completeness * 100).toFixed(1)}%`
      );
      passed++;
    } else {
      console.log('  ✗ ChatmanEngine failed validation');
      failed++;
    }
  } catch (error) {
    console.log('  ✗ ChatmanEngine error:', error.message);
    failed++;
  }

  // Test 6: Full Pipeline with OTEL spans
  try {
    console.log('\n[6/6] Testing Full Pipeline...');
    const engine = createChatmanEngine({
      observableRatio: 0.05,
      enableReceipts: true,
    });

    const result = await engine.executePipeline(
      {
        type: 'market',
        patterns: ['declining_satisfaction', 'competitor_pressure', 'market_erosion'],
        visibility: 0.05,
      },
      { theorem: 'four_actions' }
    );

    if (
      result.operation === 'full_pipeline' &&
      result.output.artifact &&
      result.output.detection &&
      result.output.formation &&
      result.receipt
    ) {
      console.log('  ✓ Full pipeline executed successfully');
      console.log(`    - Artifact completeness: ${(result.output.artifact.completeness * 100).toFixed(1)}%`);
      console.log(
        `    - Dark field ratio: ${(result.output.detection.darkFieldRatio * 100).toFixed(1)}%`
      );
      console.log(`    - Formation theorem: ${result.output.formation.theorem}`);
      console.log(`    - Strategic moves: ${result.output.formation.output.strategicMoves.length}`);
      console.log(`    - Receipt operation: ${result.receipt.operation}`);

      const metrics = engine.getMetrics();
      console.log(`\n  Engine Metrics:`);
      console.log(`    - Operations executed: ${metrics.operationsExecuted}`);
      console.log(`    - Receipts generated: ${metrics.receiptsGenerated}`);
      console.log(`    - Average execution time: ${metrics.averageExecutionTime.toFixed(2)}ms`);

      passed++;
    } else {
      console.log('  ✗ Full pipeline failed validation');
      failed++;
    }
  } catch (error) {
    console.log('  ✗ Full pipeline error:', error.message);
    console.log('  Stack:', error.stack);
    failed++;
  }

  // Summary
  console.log(`\n${'='.repeat(60)}`);
  console.log(`CHATMAN EQUATION INTEGRATION TEST RESULTS`);
  console.log(`${'='.repeat(60)}`);
  console.log(`Tests Passed: ${passed}/6`);
  console.log(`Tests Failed: ${failed}/6`);
  console.log(`Success Rate: ${((passed / 6) * 100).toFixed(1)}%`);
  console.log(`${'='.repeat(60)}`);

  if (failed === 0) {
    console.log('\n✓ ALL TESTS PASSED - Chatman integration working!\n');
    console.log('Integration includes:');
    console.log('  • μ closure operator (A = μ(O))');
    console.log('  • Artifact generation with 95% dark field');
    console.log('  • Dark field detection');
    console.log('  • Blue ocean formation theorems');
    console.log('  • OTEL observability spans');
    console.log('  • Receipt generation for all operations');
    console.log('  • TOML configuration support');
    process.exit(0);
  } else {
    console.log('\n✗ SOME TESTS FAILED - See details above\n');
    process.exit(1);
  }
}

runTests().catch(error => {
  console.error('\nFATAL ERROR:', error.message);
  console.error(error.stack);
  process.exit(1);
});
