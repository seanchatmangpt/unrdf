#!/usr/bin/env node
/**
 * @file Standalone test runner for validation 80/20 tests
 * @module validation/test-80-20
 */

import { createOTELValidator, createValidationRunner, createValidationHelpers } from './src/index.mjs';

// Simple assertion helpers
function assert(condition, message) {
  if (!condition) {
    throw new Error(message || 'Assertion failed');
  }
}

function assertEqual(actual, expected, message) {
  if (actual !== expected) {
    throw new Error(message || `Expected ${expected}, got ${actual}`);
  }
}

function assertTrue(value, message) {
  assert(value, message || `Expected true, got ${value}`);
}

function assertExists(obj, message) {
  if (!obj) {
    throw new Error(message || 'Expected object to exist');
  }
}

let passed = 0;
let failed = 0;

console.log('🧪 Running 80/20 Validation Tests...\n');

// Test 1: createOTELValidator - Basic instantiation
try {
  console.log('Test 1: OTELValidator - Basic Instantiation');

  const validator = createOTELValidator();
  assertExists(validator);
  assertExists(validator.config);
  assertEqual(validator.config.serviceName, 'unrdf-validator');
  assertEqual(validator.config.validationTimeout, 30000);
  assertEqual(validator.config.enableMetrics, true);
  assertEqual(validator.config.enableTracing, true);

  console.log('  ✅ Passed\n');
  passed++;
} catch (error) {
  console.error('  ❌ Failed:', error.message, '\n');
  failed++;
}

// Test 2: createValidationRunner - Basic instantiation
try {
  console.log('Test 2: ValidationRunner - Basic Instantiation');

  const runner = createValidationRunner();
  assertExists(runner);
  assertExists(runner.config);
  assertEqual(runner.config.timeout, 30000);
  assertEqual(runner.config.retries, 0);
  assertEqual(runner.config.parallel, false);
  assertExists(runner.validator);
  assertExists(runner.helpers);
  assertExists(runner.suiteCounter);
  assertExists(runner.featureCounter);
  assertExists(runner.validationDuration);
  assertExists(runner.validationScore);

  console.log('  ✅ Passed\n');
  passed++;
} catch (error) {
  console.error('  ❌ Failed:', error.message, '\n');
  failed++;
}

// Test 3: createValidationHelpers - Basic instantiation
try {
  console.log('Test 3: ValidationHelpers - Basic Instantiation');

  const helpers = createValidationHelpers();
  assertExists(helpers);
  assertExists(helpers.config);
  assertExists(helpers.tracer);
  assertExists(helpers.meter);

  console.log('  ✅ Passed\n');
  passed++;
} catch (error) {
  console.error('  ❌ Failed:', error.message, '\n');
  failed++;
}

// Test 4: Configuration validation schema
try {
  console.log('Test 4: Configuration Validation Schema');

  const FeatureValidationConfigSchema = (
    await import('./src/otel-validator-core.mjs')
  ).FeatureValidationConfigSchema;

  // Valid config
  const validConfig = FeatureValidationConfigSchema.parse({
    expectedSpans: ['feature.test'],
    requiredAttributes: [],
    performanceThresholds: {
      maxLatency: 1000,
      maxErrorRate: 0.01,
      minThroughput: 10,
      maxMemoryUsage: 100
    },
    validationRules: []
  });

  assertExists(validConfig.expectedSpans);
  assertExists(validConfig.performanceThresholds);

  // Test with validation rules
  const configWithRules = FeatureValidationConfigSchema.parse({
    expectedSpans: ['feature.test'],
    requiredAttributes: [],
    performanceThresholds: {
      maxLatency: 1000,
      maxErrorRate: 0.01,
      minThroughput: 10,
      maxMemoryUsage: 100
    },
    validationRules: [
      {
        name: 'rule-1',
        severity: 'warning'
      }
    ]
  });

  assertTrue(Array.isArray(configWithRules.validationRules));
  assertEqual(configWithRules.validationRules.length, 1);

  console.log('  ✅ Passed\n');
  passed++;
} catch (error) {
  console.error('  ❌ Failed:', error.message, '\n');
  failed++;
}

// Test 5: Suite management methods
try {
  console.log('Test 5: Suite Management Methods');

  const runner = createValidationRunner({ verbose: false });

  // Verify methods exist
  assertExists(runner.getReport);
  assertExists(runner.getAllReports);
  assertExists(runner.clearReports);
  assertExists(runner.getActiveSuites);

  // Clear reports (initially empty)
  assertEqual(runner.getReport('test'), null);
  assertTrue(runner.getAllReports().size >= 0);

  runner.clearReports();
  assertTrue(runner.getAllReports().size >= 0);

  console.log('  ✅ Passed\n');
  passed++;
} catch (error) {
  console.error('  ❌ Failed:', error.message, '\n');
  failed++;
}

// Summary
console.log('╔════════════════════════════════════════════╗');
console.log('║  80/20 Validation Test Results             ║');
console.log('╚════════════════════════════════════════════╝');
console.log(`Passed: ${passed}/5`);
console.log(`Failed: ${failed}/5`);
console.log('');

if (failed > 0) {
  process.exit(1);
}
