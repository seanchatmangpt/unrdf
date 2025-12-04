/**
 * @file Validation Runner for OTEL Span-Based Testing
 * @module validation/validation-runner
 *
 * @description
 * Replaces Vitest test runner with OTEL span-based validation runner.
 * Executes feature validations and generates reports based on OTEL spans.
 */

import { z } from 'zod';
import { createOTELValidator } from './otel-validator.mjs';
import { createValidationHelpers } from './validation-helpers.mjs';
import { trace, metrics } from '@opentelemetry/api';
import { randomUUID } from 'crypto';

// Validation schemas
const ValidationSuiteSchema = z.object({
  name: z.string(),
  description: z.string().optional(),
  features: z.array(
    z.object({
      name: z.string(),
      description: z.string().optional(),
      config: z.object({
        expectedSpans: z.array(z.string()),
        requiredAttributes: z.array(z.string()),
        performanceThresholds: z.object({
          maxLatency: z.number(),
          maxErrorRate: z.number(),
          minThroughput: z.number(),
          maxMemoryUsage: z.number(),
        }),
        validationRules: z.array(
          z.object({
            name: z.string(),
            condition: z.function(),
            severity: z.enum(['error', 'warning', 'info']),
          })
        ),
      }),
    })
  ),
  globalConfig: z
    .object({
      timeout: z.number().optional(),
      retries: z.number().optional(),
      parallel: z.boolean().optional(),
    })
    .optional(),
});

const ValidationReportSchema = z.object({
  suite: z.string(),
  timestamp: z.string(),
  summary: z.object({
    total: z.number(),
    passed: z.number(),
    failed: z.number(),
    skipped: z.number(),
    duration: z.number(),
    score: z.number(),
  }),
  features: z.array(
    z.object({
      name: z.string(),
      passed: z.boolean(),
      score: z.number(),
      duration: z.number(),
      violations: z.array(z.string()),
      metrics: z.object({
        latency: z.number(),
        errorRate: z.number(),
        throughput: z.number(),
        memoryUsage: z.number(),
      }),
    })
  ),
  errors: z.array(
    z.object({
      feature: z.string(),
      error: z.string(),
      stack: z.string().optional(),
    })
  ),
});

/**
 * OTEL-based validation runner
 * Replaces traditional test runners with span-based validation
 */
export class ValidationRunner {
  /**
   * Create a validation runner
   * @param {Object} [config] - Runner configuration
   */
  constructor(config = {}) {
    this.config = {
      timeout: 30000,
      retries: 0,
      parallel: false,
      verbose: false,
      ...config,
    };

    this.validator = createOTELValidator();
    this.helpers = createValidationHelpers();
    this.tracer = trace.getTracer('validation-runner');
    this.meter = metrics.getMeter('validation-runner');

    // Runner state
    this.activeSuites = new Map();
    this.validationResults = new Map();
    this.reports = new Map();

    // Create runner metrics
    this._createRunnerMetrics();
  }

  /**
   * Create runner-specific metrics
   * @private
   */
  _createRunnerMetrics() {
    this.suiteCounter = this.meter.createCounter('validation_suites_total', {
      description: 'Total number of validation suites executed',
    });

    this.featureCounter = this.meter.createCounter('validation_features_total', {
      description: 'Total number of features validated',
    });

    this.validationDuration = this.meter.createHistogram('validation_suite_duration_ms', {
      description: 'Validation suite execution duration in milliseconds',
      unit: 'ms',
    });

    this.validationScore = this.meter.createHistogram('validation_suite_score', {
      description: 'Validation suite score (0-100)',
    });
  }

  /**
   * Run a validation suite
   * @param {Object} suite - Validation suite configuration
   * @returns {Promise<Object>} Validation report
   */
  async runSuite(suite) {
    const suiteConfig = ValidationSuiteSchema.parse(suite);
    const suiteId = randomUUID();

    return await this.tracer.startActiveSpan(`validation.suite.${suiteConfig.name}`, async span => {
      try {
        span.setAttributes({
          'suite.name': suiteConfig.name,
          'suite.features': suiteConfig.features.length,
          'suite.start_time': Date.now(),
        });

        const startTime = Date.now();
        this.activeSuites.set(suiteId, {
          name: suiteConfig.name,
          startTime,
          features: suiteConfig.features,
        });

        console.log(`🚀 Running validation suite: ${suiteConfig.name}`);
        console.log(`   Features: ${suiteConfig.features.length}`);
        console.log(`   Description: ${suiteConfig.description || 'No description'}`);

        // Run feature validations
        const featureResults = await this._runFeatures(
          suiteConfig.features,
          suiteConfig.globalConfig
        );

        // Calculate summary
        const summary = this._calculateSummary(featureResults);
        const duration = Date.now() - startTime;

        // Create report
        const report = ValidationReportSchema.parse({
          suite: suiteConfig.name,
          timestamp: new Date().toISOString(),
          summary: {
            ...summary,
            duration,
          },
          features: featureResults.map(result => ({
            name: result.feature,
            passed: result.passed,
            score: result.score,
            duration: result.duration || 0,
            violations: result.violations || [],
            metrics: result.metrics || {
              latency: 0,
              errorRate: 0,
              throughput: 0,
              memoryUsage: 0,
            },
          })),
          errors: featureResults
            .filter(result => result.error)
            .map(result => ({
              feature: result.feature,
              error: result.error.message,
              stack: result.error.stack,
            })),
        });

        // Update metrics
        this.suiteCounter.add(1, {
          suite: suiteConfig.name,
          passed: summary.passed > 0 ? 'true' : 'false',
        });
        this.featureCounter.add(featureResults.length, {
          suite: suiteConfig.name,
        });
        this.validationDuration.record(duration);
        this.validationScore.record(summary.score);

        // Store report
        this.reports.set(suiteConfig.name, report);

        span.setAttributes({
          'suite.passed': summary.passed,
          'suite.failed': summary.failed,
          'suite.score': summary.score,
          'suite.duration': duration,
        });

        span.setStatus({
          code: summary.failed === 0 ? 1 : 2, // OK or ERROR
          message: `${summary.passed}/${summary.total} features passed`,
        });

        // Print results
        this._printResults(report);

        return report;
      } catch (error) {
        span.recordException(error);
        span.setStatus({ code: 2, message: error.message });
        throw error;
      } finally {
        span.end();
        this.activeSuites.delete(suiteId);
      }
    });
  }

  /**
   * Run feature validations
   * @param {Array} features - Feature configurations
   * @param {Object} globalConfig - Global configuration
   * @returns {Promise<Array>} Feature results
   * @private
   */
  async _runFeatures(features, globalConfig = {}) {
    const results = [];

    if (globalConfig.parallel) {
      // Run features in parallel
      const promises = features.map(feature => this._runFeature(feature, globalConfig));
      const featureResults = await Promise.allSettled(promises);

      for (let i = 0; i < features.length; i++) {
        const result = featureResults[i];
        if (result.status === 'fulfilled') {
          results.push(result.value);
        } else {
          results.push({
            feature: features[i].name,
            passed: false,
            score: 0,
            error: result.reason,
            violations: [`Feature execution failed: ${result.reason.message}`],
          });
        }
      }
    } else {
      // Run features sequentially
      for (const feature of features) {
        try {
          const result = await this._runFeature(feature, globalConfig);
          results.push(result);
        } catch (error) {
          results.push({
            feature: feature.name,
            passed: false,
            score: 0,
            error,
            violations: [`Feature execution failed: ${error.message}`],
          });
        }
      }
    }

    return results;
  }

  /**
   * Run a single feature validation
   * @param {Object} feature - Feature configuration
   * @param {Object} globalConfig - Global configuration
   * @returns {Promise<Object>} Feature result
   * @private
   */
  async _runFeature(feature, globalConfig = {}) {
    const timeout = globalConfig.timeout || this.config.timeout;
    const retries = globalConfig.retries || this.config.retries;

    let lastError;

    for (let attempt = 0; attempt <= retries; attempt++) {
      try {
        if (attempt > 0) {
          console.log(
            `   Retrying feature '${feature.name}' (attempt ${attempt + 1}/${retries + 1})`
          );
        }

        const result = await Promise.race([
          this.validator.validateFeature(feature.name, feature.config),
          new Promise((_, reject) =>
            setTimeout(
              () => reject(new Error(`Feature validation timeout after ${timeout}ms`)),
              timeout
            )
          ),
        ]);

        // Guard: fail if validator returned empty spans/zero throughput
        if (!result || (Array.isArray(result.spans) && result.spans.length === 0)) {
          throw new Error(
            `Validator returned zero spans for '${feature.name}'. Treating as failure.`
          );
        }
        if (result.metrics && (result.metrics.throughput || 0) <= 0) {
          throw new Error(
            `Validator reported zero throughput for '${feature.name}'. Treating as failure.`
          );
        }

        return result;
      } catch (error) {
        lastError = error;
        if (attempt < retries) {
          // Wait before retry
          await new Promise(resolve => setTimeout(resolve, 1000 * (attempt + 1)));
        }
      }
    }

    throw lastError;
  }

  /**
   * Calculate validation summary
   * @param {Array} results - Feature results
   * @returns {Object} Summary
   * @private
   */
  _calculateSummary(results) {
    const total = results.length;
    const passed = results.filter(r => r.passed).length;
    const failed = total - passed;
    const skipped = 0; // No skipped features in OTEL validation

    const scores = results.map(r => r.score || 0);
    const avgScore = scores.length > 0 ? scores.reduce((a, b) => a + b, 0) / scores.length : 0;

    return {
      total,
      passed,
      failed,
      skipped,
      score: Math.round(avgScore),
    };
  }

  /**
   * Print validation results
   * @param {Object} report - Validation report
   * @private
   */
  _printResults(report) {
    console.log('\n📊 Validation Results:');
    console.log(`   Suite: ${report.suite}`);
    console.log(`   Duration: ${report.summary.duration}ms`);
    console.log(`   Score: ${report.summary.score}/100`);
    console.log(`   Features: ${report.summary.passed}/${report.summary.total} passed`);

    if (report.summary.failed > 0) {
      console.log(`   ❌ Failed: ${report.summary.failed}`);
    }

    console.log('\n📋 Feature Details:');
    for (const feature of report.features) {
      const status = feature.passed ? '✅' : '❌';
      console.log(`   ${status} ${feature.name}: ${feature.score}/100 (${feature.duration}ms)`);

      if (feature.violations.length > 0) {
        console.log(`      Violations: ${feature.violations.length}`);
        for (const violation of feature.violations.slice(0, 3)) {
          // Show first 3 violations
          console.log(`        - ${violation}`);
        }
        if (feature.violations.length > 3) {
          console.log(`        ... and ${feature.violations.length - 3} more`);
        }
      }
    }

    if (report.errors.length > 0) {
      console.log('\n❌ Errors:');
      for (const error of report.errors) {
        console.log(`   ${error.feature}: ${error.error}`);
      }
    }

    console.log('\n🎯 Overall: ' + (report.summary.failed === 0 ? 'PASSED' : 'FAILED'));
  }

  /**
   * Get validation report for a suite
   * @param {string} suiteName - Suite name
   * @returns {Object|null} Validation report
   */
  getReport(suiteName) {
    return this.reports.get(suiteName) || null;
  }

  /**
   * Get all validation reports
   * @returns {Map} All reports
   */
  getAllReports() {
    return new Map(this.reports);
  }

  /**
   * Clear validation reports
   */
  clearReports() {
    this.reports.clear();
  }

  /**
   * Get active validation suites
   * @returns {Map} Active suites
   */
  getActiveSuites() {
    return new Map(this.activeSuites);
  }
}

/**
 * Create a validation runner instance
 * @param {Object} [config] - Configuration
 * @returns {ValidationRunner} Validation runner instance
 */
export function createValidationRunner(config = {}) {
  return new ValidationRunner(config);
}

/**
 * Default validation runner instance
 */
export const defaultValidationRunner = createValidationRunner();
