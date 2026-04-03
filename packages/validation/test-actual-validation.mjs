#!/usr/bin/env node
/**
 * @file Actual validation test - TDD style
 * @module validation/test-actual-validation
 */

import { createOTELValidator } from './src/index.mjs';

// Simple assertion
function assert(condition, message) {
  if (!condition) {
    throw new Error(message || 'Assertion failed');
  }
}

console.log('🧪 Test: Actually calling validateFeature()...\n');

try {
  const validator = createOTELValidator();
  console.log('✓ Created validator');

  // Try to call validateFeature with a fake feature
  const result = await validator.validateFeature('test-feature', {
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

  console.log('✓ Called validateFeature()');
  console.log('✓ Got result:', result);
  console.log('✓ Result type:', typeof result);

  assert(result, 'Should return a result');
  console.log('\n✅ Test PASSED - Validation actually executed');

} catch (error) {
  console.error('\n❌ Test FAILED:', error.message);
  console.error('\nStack trace:');
  console.error(error.stack);

  // Check what the actual error is
  if (error.message.includes('No spans collected')) {
    console.error('\n⚠️  Missing spans - OTEL provider not initialized');
  }
  if (error.message.includes('TracerProvider')) {
    console.error('\n⚠️  OTEL TracerProvider not configured');
  }

  process.exit(1);
}
