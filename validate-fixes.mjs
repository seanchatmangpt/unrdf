#!/usr/bin/env node
/**
 * Validation script to verify all 5 import fixes are working
 */

console.log('=== Validating Import Fixes ===\n');

let errorsFound = 0;

// Test 1: metrics.mjs exists and can be imported
console.log('1. Testing packages/federation/src/federation/metrics.mjs...');
try {
  const metricsPath = './packages/federation/src/federation/metrics.mjs';
  await import(metricsPath);
  console.log('   ✅ metrics.mjs imports successfully\n');
} catch (error) {
  console.error('   ❌ FAILED:', error.message, '\n');
  errorsFound++;
}

// Test 2: distributed-query-engine.mjs imports with fixed path
console.log('2. Testing packages/federation/src/federation/distributed-query-engine.mjs...');
try {
  const enginePath = './packages/federation/src/federation/distributed-query-engine.mjs';
  await import(enginePath);
  console.log('   ✅ distributed-query-engine.mjs imports successfully (sparql-utils path fixed)\n');
} catch (error) {
  console.error('   ❌ FAILED:', error.message, '\n');
  errorsFound++;
}

// Test 3: validate.mjs exists and can be imported
console.log('3. Testing packages/streaming/src/validate.mjs...');
try {
  const validatePath = './packages/streaming/src/validate.mjs';
  await import(validatePath);
  console.log('   ✅ validate.mjs imports successfully\n');
} catch (error) {
  console.error('   ❌ FAILED:', error.message, '\n');
  errorsFound++;
}

// Test 4: observability.mjs exists and can be imported
console.log('4. Testing packages/streaming/src/observability.mjs...');
try {
  const observabilityPath = './packages/streaming/src/observability.mjs';
  await import(observabilityPath);
  console.log('   ✅ observability.mjs imports successfully\n');
} catch (error) {
  console.error('   ❌ FAILED:', error.message, '\n');
  errorsFound++;
}

// Test 5: @opentelemetry/api is in federation package.json
console.log('5. Testing @opentelemetry/api in packages/federation/package.json...');
try {
  const { readFileSync } = await import('fs');
  const packageJson = JSON.parse(readFileSync('./packages/federation/package.json', 'utf8'));
  if (packageJson.dependencies['@opentelemetry/api']) {
    console.log('   ✅ @opentelemetry/api is in dependencies\n');
  } else {
    console.error('   ❌ FAILED: @opentelemetry/api not found in dependencies\n');
    errorsFound++;
  }
} catch (error) {
  console.error('   ❌ FAILED:', error.message, '\n');
  errorsFound++;
}

// Summary
console.log('=== Summary ===');
if (errorsFound === 0) {
  console.log('✅ All 5 import fixes verified successfully!');
  process.exit(0);
} else {
  console.log(`❌ ${errorsFound} errors found`);
  process.exit(1);
}
