/**
 * @fileoverview Test kit generator functionality
 * @module agent-8/test-kit-generator
 */

import { generateDomainKit, generateKitSummary, validateAllKits } from './kit-generator.mjs';
import { validateKit } from './kit-template.mjs';

/**
 * Test contract for testing
 */
const testContracts = [
  {
    operation: 'testOperation',
    description: 'Test operation',
    params: [
      { name: 'input', type: 'string', description: 'Test input' }
    ],
    returns: { type: 'string', description: 'Test output' },
    lens: {
      type: 'identity',
      view: (x) => x,
      review: (x) => x
    }
  }
];

/**
 * Run all tests
 */
function runTests() {
  console.log('Running Agent 8 tests...\n');

  let passed = 0;
  let failed = 0;

  // Test 1: Generate domain kit
  try {
    console.log('Test 1: Generate domain kit');
    const kit = generateDomainKit('test', testContracts);
    console.log('  Domain:', kit.domain);
    console.log('  Version:', kit.version);
    console.log('  Adapters:', kit.adapters.length);
    console.log('  Scenarios:', kit.scenarios.length);
    console.log('  ✅ PASS\n');
    passed++;
  } catch (error) {
    console.log('  ❌ FAIL:', error.message, '\n');
    failed++;
  }

  // Test 2: Validate kit
  try {
    console.log('Test 2: Validate kit');
    const kit = generateDomainKit('test', testContracts);
    validateKit(kit);
    console.log('  ✅ PASS\n');
    passed++;
  } catch (error) {
    console.log('  ❌ FAIL:', error.message, '\n');
    failed++;
  }

  // Test 3: Generate kit summary
  try {
    console.log('Test 3: Generate kit summary');
    const kit = generateDomainKit('test', testContracts);
    const summary = generateKitSummary(kit);
    console.log('  Summary:', JSON.stringify(summary, null, 2));
    console.log('  ✅ PASS\n');
    passed++;
  } catch (error) {
    console.log('  ❌ FAIL:', error.message, '\n');
    failed++;
  }

  // Test 4: Generate scenarios
  try {
    console.log('Test 4: Generate scenarios');
    const kit = generateDomainKit('test', testContracts);
    const happyScenarios = kit.scenarios.filter(s => s.type === 'happy');
    const errorScenarios = kit.scenarios.filter(s => s.type === 'error');
    const edgeScenarios = kit.scenarios.filter(s => s.type === 'edge');

    console.log('  Happy path scenarios:', happyScenarios.length);
    console.log('  Error scenarios:', errorScenarios.length);
    console.log('  Edge case scenarios:', edgeScenarios.length);
    console.log('  Total scenarios:', kit.scenarios.length);

    if (kit.scenarios.length > 0) {
      console.log('  ✅ PASS\n');
      passed++;
    } else {
      console.log('  ❌ FAIL: No scenarios generated\n');
      failed++;
    }
  } catch (error) {
    console.log('  ❌ FAIL:', error.message, '\n');
    failed++;
  }

  // Test 5: Validate all kits
  try {
    console.log('Test 5: Validate multiple kits');
    const kits = {
      test1: generateDomainKit('test1', testContracts),
      test2: generateDomainKit('test2', testContracts)
    };

    const validation = validateAllKits(kits);
    console.log('  Validation:', JSON.stringify(validation, null, 2));

    if (validation.valid === 2 && validation.invalid === 0) {
      console.log('  ✅ PASS\n');
      passed++;
    } else {
      console.log('  ❌ FAIL: Validation failed\n');
      failed++;
    }
  } catch (error) {
    console.log('  ❌ FAIL:', error.message, '\n');
    failed++;
  }

  // Test 6: Determinism check
  try {
    console.log('Test 6: Deterministic generation');
    const kit1 = generateDomainKit('test', testContracts);
    const kit2 = generateDomainKit('test', testContracts);

    const summary1 = generateKitSummary(kit1);
    const summary2 = generateKitSummary(kit2);

    if (
      summary1.components.adapters === summary2.components.adapters &&
      summary1.components.scenarios === summary2.components.scenarios
    ) {
      console.log('  Same adapters:', summary1.components.adapters);
      console.log('  Same scenarios:', summary1.components.scenarios);
      console.log('  ✅ PASS\n');
      passed++;
    } else {
      console.log('  ❌ FAIL: Non-deterministic generation\n');
      failed++;
    }
  } catch (error) {
    console.log('  ❌ FAIL:', error.message, '\n');
    failed++;
  }

  // Summary
  console.log('='.repeat(50));
  console.log('Test Results:');
  console.log('  Passed:', passed);
  console.log('  Failed:', failed);
  console.log('  Total:', passed + failed);
  console.log('  Success rate:', `${((passed / (passed + failed)) * 100).toFixed(1)}%`);
  console.log('='.repeat(50));

  if (failed === 0) {
    console.log('\n✅ All tests passed!');
    return 0;
  } else {
    console.log('\n❌ Some tests failed');
    return 1;
  }
}

// Run if executed directly
if (import.meta.url === `file://${process.argv[1]}`) {
  const exitCode = runTests();
  process.exit(exitCode);
}

export { runTests };
