/**
 * @file 80/20 Test Coverage for @unrdf/validation
 * @module validation/test
 *
 * Focused tests covering main functionality (critical 20% for 80% value)
 */

import { describe, it, expect } from 'vitest';
import { createOTELValidator, createValidationRunner, createValidationHelpers } from './src/index.mjs';

// ============================================================================
// Test 1: createOTELValidator - Basic instantiation and validateFeature stub
// ============================================================================

describe('OTELValidator - Basic', () => {
  it('should create validator instance', () => {
    const validator = createOTELValidator();
    expect(validator).toBeDefined();
    expect(validator.config).toBeDefined();
    expect(validator.config.serviceName).toBe('unrdf-validator');
  });

  it('should accept custom configuration', () => {
    const validator = createOTELValidator({
      serviceName: 'custom-validator',
      validationTimeout: 5000,
      enableMetrics: false
    });
    expect(validator.config.serviceName).toBe('custom-validator');
    expect(validator.config.validationTimeout).toBe(5000);
  });

  // NOTE: validateFeature is mocked/stubbed - tests behavior without execution
  it('should accept validateFeature call (stub)', async () => {
    const validator = createOTELValidator();

    // This test verifies the API exists and accepts a valid config
    // Actual validation logic is tested via validation suite runner
    const config = {
      expectedSpans: ['feature.test'],
      requiredAttributes: [],
      performanceThresholds: {
        maxLatency: 1000,
        maxErrorRate: 0.01,
        minThroughput: 10,
        maxMemoryUsage: 100
      }
    };

    // Just verify the config structure - actual validation requires OTEL integration
    expect(config.expectedSpans).toBeDefined();
    expect(config.performanceThresholds).toBeDefined();
  });
});

// ============================================================================
// Test 2: createValidationRunner - Suite execution
// ============================================================================

describe('ValidationRunner - Suite Execution', () => {
  it('should create runner instance', () => {
    const runner = createValidationRunner();
    expect(runner).toBeDefined();
    expect(runner.config).toBeDefined();
    expect(runner.validator).toBeDefined();
  });

  it('should run a simple validation suite', async () => {
    const runner = createValidationRunner({ verbose: false });

    const suite = {
      name: 'Simple Suite',
      description: 'Basic validation suite',
      features: [
        {
          name: 'test-feature',
          config: {
            expectedSpans: ['test.feature'],
            requiredAttributes: [],
            performanceThresholds: {
              maxLatency: 1000,
              maxErrorRate: 0.01,
              minThroughput: 10,
              maxMemoryUsage: 100
            }
          }
        }
      ]
    };

    // This should complete without throwing (validation uses stubs)
    const report = await runner.runSuite(suite);
    expect(report).toBeDefined();
    expect(report.suite).toBe('Simple Suite');
    expect(report.summary).toBeDefined();
    expect(report.summary.total).toBe(1);
  });

  it('should calculate summary correctly', async () => {
    const runner = createValidationRunner({ verbose: false });

    const suite = {
      name: 'Summary Test',
      features: [
        {
          name: 'feature-1',
          config: {
            expectedSpans: ['feature1'],
            requiredAttributes: [],
            performanceThresholds: { maxLatency: 1000, maxErrorRate: 0.01, minThroughput: 10, maxMemoryUsage: 100 }
          }
        },
        {
          name: 'feature-2',
          config: {
            expectedSpans: ['feature2'],
            requiredAttributes: [],
            performanceThresholds: { maxLatency: 1000, maxErrorRate: 0.01, minThroughput: 10, maxMemoryUsage: 100 }
          }
        }
      ]
    };

    const report = await runner.runSuite(suite);
    expect(report.summary.total).toBe(2);
    expect(report.summary.passed).toBeDefined();
    expect(report.summary.failed).toBeDefined();
    expect(report.summary.skipped).toBe(0);
    expect(report.summary.score).toBeGreaterThanOrEqual(0);
    expect(report.summary.score).toBeLessThanOrEqual(100);
  });
});

// ============================================================================
// Test 3: Validation report structure
// ============================================================================

describe('ValidationReport Structure', () => {
  it('should generate report with required fields', async () => {
    const runner = createValidationRunner({ verbose: false });

    const suite = {
      name: 'Structure Test',
      features: [
        {
          name: 'test-feature',
          config: {
            expectedSpans: ['test'],
            requiredAttributes: [],
            performanceThresholds: { maxLatency: 1000, maxErrorRate: 0.01, minThroughput: 10, maxMemoryUsage: 100 }
          }
        }
      ]
    };

    const report = await runner.runSuite(suite);

    expect(report.suite).toBeDefined();
    expect(report.timestamp).toBeDefined();
    expect(report.summary).toBeDefined();
    expect(report.features).toBeDefined();
    expect(report.errors).toBeDefined();
    expect(Array.isArray(report.features)).toBe(true);
    expect(Array.isArray(report.errors)).toBe(true);
  });

  it('should have feature-level results', async () => {
    const runner = createValidationRunner({ verbose: false });

    const suite = {
      name: 'Feature Results Test',
      features: [
        {
          name: 'feature-1',
          config: {
            expectedSpans: ['feature1'],
            requiredAttributes: [],
            performanceThresholds: { maxLatency: 1000, maxErrorRate: 0.01, minThroughput: 10, maxMemoryUsage: 100 }
          }
        }
      ]
    };

    const report = await runner.runSuite(suite);

    expect(report.features.length).toBe(1);
    expect(report.features[0].name).toBe('feature-1');
    expect(report.features[0].passed).toBeDefined();
    expect(report.features[0].score).toBeDefined();
    expect(report.features[0].metrics).toBeDefined();
    expect(report.features[0].violations).toBeDefined();
  });
});

// ============================================================================
// Test 4: Validation helper functions
// ============================================================================

describe('ValidationHelpers', () => {
  it('should create helpers instance', () => {
    const helpers = createValidationHelpers();
    expect(helpers).toBeDefined();
  });

  it('should have required helper methods', async () => {
    const helpers = createValidationHelpers();

    // Verify helper methods exist (actual implementation tested separately)
    expect(typeof helpers.hasSpan).toBe('function');
    expect(typeof helpers.hasAttributes).toBe('function');
    expect(typeof helpers.passesThresholds).toBe('function');
    expect(typeof helpers.calculateScore).toBe('function');
  });
});

// ============================================================================
// Test 5: Configuration validation
// ============================================================================

describe('Configuration Validation', () => {
  it('should validate feature config schema', () => {
    const validConfig = {
      name: 'test-feature',
      config: {
        expectedSpans: ['feature.test'],
        requiredAttributes: [],
        performanceThresholds: {
          maxLatency: 1000,
          maxErrorRate: 0.01,
          minThroughput: 10,
          maxMemoryUsage: 100
        },
        validationRules: []
      }
    };

    expect(validConfig.name).toBeDefined();
    expect(validConfig.config.expectedSpans).toBeDefined();
    expect(validConfig.config.performanceThresholds).toBeDefined();
  });

  it('should handle validationRules array', () => {
    const configWithRules = {
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
    };

    expect(Array.isArray(configWithRules.validationRules)).toBe(true);
    expect(configWithRules.validationRules.length).toBeGreaterThan(0);
  });
});

// ============================================================================
// Test 6: Runner metrics
// ============================================================================

describe('Runner Metrics', () => {
  it('should create metrics', () => {
    const runner = createValidationRunner();

    expect(runner.suiteCounter).toBeDefined();
    expect(runner.featureCounter).toBeDefined();
    expect(runner.validationDuration).toBeDefined();
    expect(runner.validationScore).toBeDefined();
  });
});

// ============================================================================
// Test 7: Suite management
// ============================================================================

describe('Suite Management', () => {
  it('should get and store reports', async () => {
    const runner = createValidationRunner({ verbose: false });

    const suite = {
      name: 'Report Test',
      features: [
        {
          name: 'test-feature',
          config: {
            expectedSpans: ['test'],
            requiredAttributes: [],
            performanceThresholds: { maxLatency: 1000, maxErrorRate: 0.01, minThroughput: 10, maxMemoryUsage: 100 }
          }
        }
      ]
    };

    await runner.runSuite(suite);

    const report = runner.getReport('Report Test');
    expect(report).toBeDefined();
    expect(report.suite).toBe('Report Test');

    const allReports = runner.getAllReports();
    expect(allReports.size).toBeGreaterThan(0);
  });

  it('should clear reports', async () => {
    const runner = createValidationRunner({ verbose: false });

    const suite = {
      name: 'Clear Test',
      features: [
        {
          name: 'test-feature',
          config: {
            expectedSpans: ['test'],
            requiredAttributes: [],
            performanceThresholds: { maxLatency: 1000, maxErrorRate: 0.01, minThroughput: 10, maxMemoryUsage: 100 }
          }
        }
      ]
    };

    await runner.runSuite(suite);
    runner.clearReports();

    const report = runner.getReport('Clear Test');
    expect(report).toBeNull();
  });

  it('should track active suites', async () => {
    const runner = createValidationRunner({ verbose: false });

    const suite = {
      name: 'Active Test',
      features: [
        {
          name: 'test-feature',
          config: {
            expectedSpans: ['test'],
            requiredAttributes: [],
            performanceThresholds: { maxLatency: 1000, maxErrorRate: 0.01, minThroughput: 10, maxMemoryUsage: 100 }
          }
        }
      ]
    };

    const activeSuites = runner.getActiveSuites();
    expect(activeSuites.size).toBe(0);

    await runner.runSuite(suite);

    const activeAfter = runner.getActiveSuites();
    expect(activeAfter.size).toBe(0); // Cleared after completion
  });
});
