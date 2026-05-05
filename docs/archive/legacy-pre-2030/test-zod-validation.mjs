#!/usr/bin/env node

/**
 * Test file to demonstrate Zod validation in microframeworks
 * Tests that invalid inputs are properly rejected with ZodError
 */

import { GraphAwareRouter } from './microfw-9-graph-routing.mjs';
import { MegaFramework } from './max-combo-10-mega-framework-standalone.mjs';

console.log('╔════════════════════════════════════════════════════════════╗');
console.log('║ Zod Validation Tests - Microframeworks                    ║');
console.log('╚════════════════════════════════════════════════════════════╝\n');

let passCount = 0;
let failCount = 0;

/**
 * Test helper to run validation tests
 */
async function testValidation(testName, testFn) {
  try {
    console.log(`TEST: ${testName}`);
    const result = await testFn();

    // Check if result is an error response (status 400 or 4xx)
    if (result && result.status >= 400) {
      console.log(`  ✅ PASS: Invalid input rejected (HTTP ${result.status})`);
      console.log(`  Error: ${result.body?.error || 'Validation failed'}\n`);
      passCount++;
      return;
    }

    console.log('  ❌ FAIL: Expected error but test passed\n');
    failCount++;
  } catch (error) {
    if (error.name === 'ZodError') {
      console.log(`  ✅ PASS: ZodError thrown as expected`);
      const errorMsg = error.errors && error.errors[0] ? error.errors[0].message : error.message;
      console.log(`  Error: ${errorMsg}\n`);
      passCount++;
    } else if (error.message && (error.message.includes('validation') || error.message.includes('Invalid'))) {
      console.log(`  ✅ PASS: Validation error thrown`);
      console.log(`  Error: ${error.message}\n`);
      passCount++;
    } else {
      console.log(`  ❌ FAIL: Wrong error type: ${error.name}`);
      console.log(`  Message: ${error.message}\n`);
      failCount++;
    }
  }
}

/**
 * Test helper for valid inputs (should NOT throw)
 */
async function testValidInput(testName, testFn) {
  try {
    console.log(`TEST: ${testName}`);
    const result = await testFn();
    console.log(`  ✅ PASS: Valid input accepted`);
    console.log(`  Result: ${JSON.stringify(result).substring(0, 80)}...\n`);
    passCount++;
  } catch (error) {
    console.log(`  ❌ FAIL: Valid input rejected`);
    console.log(`  Error: ${error.message}\n`);
    failCount++;
  }
}

// ============================================================================
// GRAPH ROUTING TESTS
// ============================================================================

console.log('═══ Graph Routing Validation Tests ═══\n');

const router = new GraphAwareRouter({ production: false });

// Test 1: Invalid path (path traversal attack)
await testValidation('Graph Router: Path traversal attack blocked', async () => {
  await router.handleRequest({
    path: '/customers/../admin',
    method: 'GET'
  });
});

// Test 2: Invalid URI format (XSS attempt)
await testValidation('Graph Router: XSS in URI blocked', async () => {
  // This will throw during URI validation
  router.defineRelationship(
    'http://example.com/<script>alert(1)</script>',
    'http://example.com/predicate',
    'http://example.com/object'
  );
  return { status: 200 }; // Should not reach here
});

// Test 3: Invalid path format (null byte injection)
await testValidation('Graph Router: Null byte injection blocked', async () => {
  await router.handleRequest({
    path: '/customers\0admin',
    method: 'GET'
  });
});

// Test 4: Valid path should work
router.defineRoute('test_route', '/api/test', 'GET', async (ctx) => ({
  message: 'Valid route'
}));

await testValidInput('Graph Router: Valid path accepted', async () => {
  const result = await router.handleRequest({
    path: '/api/test',
    method: 'GET'
  });
  return result;
});

// ============================================================================
// MEGA FRAMEWORK TESTS
// ============================================================================

console.log('\n═══ Mega Framework Validation Tests ═══\n');

const framework = new MegaFramework();

// Test 5: Invalid dark execution query (process access)
await testValidation('Mega Framework: Process access in dark execution blocked', async () => {
  await framework.darkExecute('process.exit(1)', {});
});

// Test 6: Invalid dark execution query (require)
await testValidation('Mega Framework: require() in dark execution blocked', async () => {
  await framework.darkExecute("require('fs')", {});
});

// Test 7: Invalid dark execution query (import)
await testValidation('Mega Framework: import() in dark execution blocked', async () => {
  await framework.darkExecute("import('fs')", {});
});

// Test 8: Invalid time range (end before start)
await testValidation('Mega Framework: Invalid time range blocked', async () => {
  await framework.temporalQuery('SELECT * WHERE { ?s ?p ?o }', {
    start: 1000,
    end: 500  // End before start - should fail
  });
});

// Test 9: Invalid SPARQL query (not SELECT/ASK/CONSTRUCT)
await testValidation('Mega Framework: Invalid SPARQL query type blocked', async () => {
  await framework.federatedQuery('UPDATE DATA { }');
});

// Test 10: Invalid workflow ID (uppercase not allowed)
await testValidation('Mega Framework: Invalid workflow ID blocked', async () => {
  await framework.executeWorkflowWithLearning('INVALID-WORKFLOW', {});
});

// Test 11: Valid dark execution should work
await testValidInput('Mega Framework: Valid dark execution accepted', async () => {
  const result = await framework.darkExecute('42 + 42', { test: true });
  return result;
});

// Test 12: Valid SPARQL query should work
await testValidInput('Mega Framework: Valid SPARQL query accepted', async () => {
  const result = await framework.federatedQuery('SELECT ?s ?p ?o WHERE { ?s ?p ?o } LIMIT 5');
  return result;
});

// Test 13: Valid validation data
await testValidInput('Mega Framework: Valid validation data accepted', async () => {
  const result = await framework.validateAcrossFederation({
    subject: 'test-subject',
    predicate: 'test-predicate',
    object: 'test-object'
  });
  return result;
});

// ============================================================================
// TEST SUMMARY
// ============================================================================

console.log('\n╔════════════════════════════════════════════════════════════╗');
console.log('║ TEST SUMMARY                                               ║');
console.log('╚════════════════════════════════════════════════════════════╝\n');
console.log(`Total Tests: ${passCount + failCount}`);
console.log(`Passed:      ${passCount} ✅`);
console.log(`Failed:      ${failCount} ❌`);
console.log(`Success Rate: ${((passCount / (passCount + failCount)) * 100).toFixed(1)}%\n`);

if (failCount === 0) {
  console.log('🎉 All validation tests passed!');
  console.log('✓ Invalid inputs properly rejected with ZodError');
  console.log('✓ Valid inputs properly accepted');
  console.log('✓ Security constraints enforced\n');
  process.exit(0);
} else {
  console.log('⚠️  Some tests failed. Review validation logic.\n');
  process.exit(1);
}
