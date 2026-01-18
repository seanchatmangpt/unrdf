/**
 * @file Chatman Equation Smoke Test
 * @description Quick validation that Chatman integration works
 */

import {
  createChatmanOperator,
  createArtifactGenerator,
  createDarkFieldDetector,
  createFormationTheorems,
  createChatmanEngine,
} from '../src/index.mjs';

console.log('Chatman Equation Smoke Test\n');

async function runTests() {
  let passed = 0;
  let failed = 0;

  // Test 1: ChatmanOperator
  try {
    console.log('[1/5] Testing ChatmanOperator...');
    const operator = createChatmanOperator({ observableRatio: 0.05 });
    const result = await operator.apply({
      type: 'market',
      patterns: ['customer_feedback'],
      visibility: 0.05,
    });

    if (result.darkField.patterns.length > 0 && result.completeness > 0.9) {
      console.log('  ✓ ChatmanOperator works');
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
    console.log('[2/5] Testing ArtifactGenerator...');
    const generator = createArtifactGenerator({ observableRatio: 0.05 });
    const artifact = await generator.generate({
      type: 'organizational',
      patterns: ['workflow'],
      visibility: 0.05,
    });

    if (artifact.id && artifact.darkField && artifact.completeness > 0.9) {
      console.log('  ✓ ArtifactGenerator works');
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
    console.log('[3/5] Testing DarkFieldDetector...');
    const detector = createDarkFieldDetector({ targetRatio: 0.95 });
    const detection = await detector.detect({
      type: 'strategic',
      patterns: ['strategy'],
      visibility: 0.05,
    });

    if (detection.darkFieldRatio > 0.9 && detection.patterns.length > 0) {
      console.log('  ✓ DarkFieldDetector works');
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
    console.log('[4/5] Testing FormationTheorems...');
    const theorems = createFormationTheorems();
    const generator = createArtifactGenerator({ observableRatio: 0.05 });
    const artifact = await generator.generate({
      type: 'disruption',
      patterns: ['tech_shift'],
      visibility: 0.05,
    });

    const formation = await theorems.derive(artifact, { theorem: 'emergence' });

    if (
      formation.theorem === 'emergence' &&
      formation.output.formation.length > 0
    ) {
      console.log('  ✓ FormationTheorems works');
      passed++;
    } else {
      console.log('  ✗ FormationTheorems failed validation');
      failed++;
    }
  } catch (error) {
    console.log('  ✗ FormationTheorems error:', error.message);
    failed++;
  }

  // Test 5: ChatmanEngine (full pipeline)
  try {
    console.log('[5/5] Testing ChatmanEngine (full pipeline)...');
    const engine = createChatmanEngine({
      observableRatio: 0.05,
      enableReceipts: true,
    });

    const result = await engine.executePipeline(
      {
        type: 'market',
        patterns: ['trend1', 'trend2'],
        visibility: 0.05,
      },
      { theorem: 'emergence' }
    );

    if (
      result.operation === 'full_pipeline' &&
      result.output.artifact &&
      result.output.detection &&
      result.output.formation &&
      result.receipt
    ) {
      console.log('  ✓ ChatmanEngine full pipeline works');
      passed++;
    } else {
      console.log('  ✗ ChatmanEngine failed validation');
      failed++;
    }
  } catch (error) {
    console.log('  ✗ ChatmanEngine error:', error.message);
    failed++;
  }

  // Summary
  console.log(`\n${'='.repeat(50)}`);
  console.log(`Tests Passed: ${passed}/5`);
  console.log(`Tests Failed: ${failed}/5`);
  console.log(`${'='.repeat(50)}`);

  if (failed === 0) {
    console.log('\n✓ All Chatman integration tests passed!\n');
    process.exit(0);
  } else {
    console.log('\n✗ Some tests failed. See details above.\n');
    process.exit(1);
  }
}

runTests().catch(error => {
  console.error('Fatal error:', error);
  process.exit(1);
});
