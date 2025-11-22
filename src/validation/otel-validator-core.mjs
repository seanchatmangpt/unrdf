/**
 * @file OpenTelemetry Validator Core
 * @module validation/otel-validator-core
 *
 * @description
 * Core OTELValidator class implementation.
 * Validates features by analyzing OTEL spans instead of unit tests.
 */

import { trace, metrics, SpanStatusCode } from '@opentelemetry/api';
import { z } from 'zod';
import { randomUUID } from 'crypto';

// Import span builder functions
import {
  executeKnowledgeEngine,
  executeCLIParse,
  executeCLIQuery,
  executeCLIValidate,
  executeCLIHook,
  executeTransactionManager,
  executeKnowledgeEngineCore,
  executeKnowledgeHooksAPI,
  executePolicyPacks,
  executeLockchainIntegrity,
  executeBrowserCompatibility,
} from './otel-span-builder.mjs';

// Validation schemas
export const ValidationResultSchema = z.object({
  feature: z.string(),
  passed: z.boolean(),
  score: z.number().min(0).max(100),
  metrics: z.object({
    latency: z.number(),
    errorRate: z.number(),
    throughput: z.number(),
    memoryUsage: z.number(),
  }),
  spans: z.array(
    z.object({
      name: z.string(),
      status: z.enum(['ok', 'error']),
      duration: z.number(),
      attributes: z.record(z.any()),
    })
  ),
  violations: z.array(z.string()),
  timestamp: z.string(),
});

export const FeatureValidationConfigSchema = z.object({
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
});

/**
 * OpenTelemetry-based feature validator
 * Validates features by analyzing OTEL spans instead of unit tests
 */
export class OTELValidator {
  /**
   * Create a new OTEL validator
   * @param {Object} [config] - Validator configuration
   */
  constructor(config = {}) {
    this.config = {
      serviceName: 'unrdf-validator',
      enableMetrics: true,
      enableTracing: true,
      validationTimeout: 30000,
      ...config,
    };

    this.tracer = trace.getTracer(this.config.serviceName);
    this.meter = metrics.getMeter(this.config.serviceName);

    // Validation state
    this.activeValidations = new Map();
    this.validationResults = new Map();
    this.spanCollector = new Map();
    this.metricCollector = new Map();

    // Temp spans per validation for parallel execution
    this._validationTempSpans = new Map();

    // Custom metrics for validation
    this._createValidationMetrics();
  }

  /**
   * Create validation-specific metrics
   * @private
   */
  _createValidationMetrics() {
    this.validationCounter = this.meter.createCounter('validation_total', {
      description: 'Total number of feature validations',
    });

    this.validationDuration = this.meter.createHistogram('validation_duration_ms', {
      description: 'Feature validation duration in milliseconds',
      unit: 'ms',
    });

    this.validationScore = this.meter.createHistogram('validation_score', {
      description: 'Feature validation score (0-100)',
    });

    this.featureHealthGauge = this.meter.createUpDownCounter('feature_health_score', {
      description: 'Current health score of features',
    });
  }

  /**
   * Start validating a feature using OTEL spans
   * @param {string} feature - Feature name to validate
   * @param {Object} config - Validation configuration
   * @returns {Promise<Object>} Validation result
   */
  async validateFeature(feature, config) {
    const validationConfig = FeatureValidationConfigSchema.parse(config);
    const validationId = randomUUID();

    return await this.tracer.startActiveSpan(`validation.${feature}`, async span => {
      try {
        span.setAttributes({
          'validation.id': validationId,
          'validation.feature': feature,
          'validation.start_time': Date.now(),
        });

        const startTime = Date.now();

        // Start collecting spans and metrics
        this._startSpanCollection(validationId);
        this._startMetricCollection(validationId);

        // Create temp spans array for THIS validation only
        this._validationTempSpans.set(validationId, []);

        // Execute the feature (this will generate OTEL spans)
        const _featureResult = await this._executeFeature(feature, validationConfig, validationId);

        // Collect manually tracked spans
        const collectedTempSpans = this._validationTempSpans.get(validationId) || [];
        if (collectedTempSpans.length > 0) {
          for (const tempSpan of collectedTempSpans) {
            this._addSpan(validationId, tempSpan);
          }
        }

        // Collect and analyze spans
        const collectedSpans = this._collectSpans(validationId);
        const collectedMetrics = this._collectMetrics(validationId);

        // Guard: prevent null-success
        if (!collectedSpans || collectedSpans.length === 0) {
          throw new Error(
            `No spans collected for feature '${feature}'. Ensure TracerProvider is initialized.`
          );
        }
        if (!collectedMetrics || (collectedMetrics.throughput || 0) <= 0) {
          throw new Error(
            `No operations recorded for feature '${feature}'. Validation cannot pass with zero throughput.`
          );
        }

        // Clear temp spans
        this._validationTempSpans.delete(validationId);

        // Validate against expected spans and attributes
        const spanValidation = this._validateSpans(collectedSpans, validationConfig);
        const metricValidation = this._validateMetrics(collectedMetrics, validationConfig);
        const ruleValidation = this._validateRules(
          collectedSpans,
          collectedMetrics,
          validationConfig
        );

        // Calculate overall score
        const score = this._calculateScore(spanValidation, metricValidation, ruleValidation);
        const passed = score >= 80;

        const duration = Date.now() - startTime;

        const result = ValidationResultSchema.parse({
          feature,
          passed,
          score,
          metrics: {
            latency: collectedMetrics.latency || 0,
            errorRate: collectedMetrics.errorRate || 0,
            throughput: collectedMetrics.throughput || 0,
            memoryUsage: collectedMetrics.memoryUsage || 0,
          },
          spans: collectedSpans.map(s => ({
            name: s.name,
            status: s.status,
            duration: s.duration,
            attributes: s.attributes,
          })),
          violations: [
            ...spanValidation.violations,
            ...metricValidation.violations,
            ...ruleValidation.violations,
          ],
          timestamp: new Date().toISOString(),
        });

        // Update metrics
        this.validationCounter.add(1, { feature, passed: passed.toString() });
        this.validationDuration.record(duration);
        this.validationScore.record(score);
        this.featureHealthGauge.add(score, { feature });

        // Store result
        this.validationResults.set(feature, result);

        span.setAttributes({
          'validation.passed': passed,
          'validation.score': score,
          'validation.duration': duration,
          'validation.violations': result.violations.length,
        });

        span.setStatus({
          code: passed ? SpanStatusCode.OK : SpanStatusCode.ERROR,
        });

        return result;
      } catch (error) {
        span.recordException(error);
        span.setStatus({
          code: SpanStatusCode.ERROR,
          message: error.message,
        });
        throw error;
      } finally {
        span.end();
        this._cleanupValidation(validationId);
      }
    });
  }

  /**
   * Execute a feature and collect its OTEL spans
   * @param {string} feature - Feature name
   * @param {Object} config - Validation config
   * @param {string} validationId - Validation ID
   * @returns {Promise<any>} Feature execution result
   * @private
   */
  async _executeFeature(feature, config, validationId) {
    return await this.tracer.startActiveSpan(`feature.${feature}`, async span => {
      span.setAttribute('feature.name', feature);

      try {
        const result = await this._executeRealFeature(feature, span, validationId);
        span.setStatus({ code: SpanStatusCode.OK });
        return result;
      } catch (error) {
        span.recordException(error);
        span.setStatus({
          code: SpanStatusCode.ERROR,
          message: error.message,
        });
        return { success: false, error: error.message };
      }
    });
  }

  /**
   * Execute real feature operations
   * @param {string} feature - Feature name
   * @param {Span} parentSpan - Parent span
   * @param {string} validationId - Validation ID
   * @returns {Promise<any>} Execution result
   * @private
   */
  async _executeRealFeature(feature, parentSpan, validationId) {
    // Feature execution map - delegates to span builder functions
    const featureExecutors = {
      'knowledge-engine': () => executeKnowledgeEngine(this, parentSpan, validationId),
      'knowledge-engine-core': () => executeKnowledgeEngineCore(this, parentSpan, validationId),
      'knowledge-hooks-api': () => executeKnowledgeHooksAPI(this, parentSpan, validationId),
      'policy-packs': () => executePolicyPacks(this, parentSpan, validationId),
      'lockchain-integrity': () => executeLockchainIntegrity(this, parentSpan, validationId),
      'browser-compatibility': () => executeBrowserCompatibility(this, parentSpan, validationId),
      'cli-parse': () => executeCLIParse(this, parentSpan, validationId),
      'cli-query': () => executeCLIQuery(this, parentSpan, validationId),
      'cli-validate': () => executeCLIValidate(this, parentSpan, validationId),
      'cli-hook': () => executeCLIHook(this, parentSpan, validationId),
      'transaction-manager': () => executeTransactionManager(this, parentSpan, validationId),
    };

    const executor = featureExecutors[feature];
    if (executor) {
      return await executor();
    }

    // Fallback to simulation
    await this._simulateFeatureOperations(feature, parentSpan);
    return { success: true };
  }

  // Feature execution methods are imported from otel-span-builder.mjs
  // These will be dynamically loaded

  /**
   * Start collecting spans for validation
   * @param {string} validationId - Validation ID
   * @private
   */
  _startSpanCollection(validationId) {
    this.spanCollector.set(validationId, []);
  }

  /**
   * Start collecting metrics for validation
   * @param {string} validationId - Validation ID
   * @private
   */
  _startMetricCollection(validationId) {
    this.metricCollector.set(validationId, {
      latency: [],
      errors: 0,
      operations: 0,
      memoryUsage: [],
    });
  }

  /**
   * Collect spans for validation
   * @param {string} validationId - Validation ID
   * @returns {Array} Collected spans
   * @private
   */
  _collectSpans(validationId) {
    return this.spanCollector.get(validationId) || [];
  }

  /**
   * Add a span to the collector
   * @param {string} validationId - Validation ID
   * @param {Object} spanData - Span data
   * @private
   */
  _addSpan(validationId, spanData) {
    const collector = this.spanCollector.get(validationId);
    if (collector) {
      collector.push(spanData);

      const metricsData = this.metricCollector.get(validationId);
      if (metricsData) {
        metricsData.operations++;
        if (spanData.duration) {
          metricsData.latency.push(spanData.duration);
        }
        if (spanData.status === 'error') {
          metricsData.errors++;
        }
      }
    }
  }

  /**
   * Collect metrics for validation
   * @param {string} validationId - Validation ID
   * @returns {Object} Collected metrics
   * @private
   */
  _collectMetrics(validationId) {
    const metricsData = this.metricCollector.get(validationId) || {
      latency: [],
      errors: 0,
      operations: 0,
    };

    const avgLatency =
      metricsData.latency.length > 0
        ? metricsData.latency.reduce((a, b) => a + b, 0) / metricsData.latency.length
        : 0;

    const errorRate = metricsData.operations > 0 ? metricsData.errors / metricsData.operations : 0;
    const throughput = metricsData.operations;

    return {
      latency: avgLatency,
      errorRate,
      throughput,
      memoryUsage: process.memoryUsage().heapUsed,
    };
  }

  /**
   * Validate collected spans against expected spans
   * @param {Array} spans - Collected spans
   * @param {Object} config - Validation config
   * @returns {Object} Validation result
   * @private
   */
  _validateSpans(spans, config) {
    const violations = [];
    const spanNames = spans.map(s => s.name);

    for (const expectedSpan of config.expectedSpans) {
      if (!spanNames.includes(expectedSpan)) {
        violations.push(`Missing expected span: ${expectedSpan}`);
      }
    }

    for (const span of spans) {
      for (const requiredAttr of config.requiredAttributes) {
        if (!(requiredAttr in span.attributes)) {
          violations.push(`Missing required attribute '${requiredAttr}' in span '${span.name}'`);
        }
      }
    }

    const errorSpans = spans.filter(s => s.status === 'error');
    if (errorSpans.length > 0) {
      violations.push(`Found ${errorSpans.length} spans with error status`);
    }

    return { violations, score: Math.max(0, 100 - violations.length * 10) };
  }

  /**
   * Validate collected metrics against thresholds
   * @param {Object} metricsData - Collected metrics
   * @param {Object} config - Validation config
   * @returns {Object} Validation result
   * @private
   */
  _validateMetrics(metricsData, config) {
    const violations = [];
    const thresholds = config.performanceThresholds;

    if (metricsData.latency > thresholds.maxLatency) {
      violations.push(
        `Latency ${metricsData.latency}ms exceeds threshold ${thresholds.maxLatency}ms`
      );
    }

    if (metricsData.errorRate > thresholds.maxErrorRate) {
      violations.push(
        `Error rate ${metricsData.errorRate} exceeds threshold ${thresholds.maxErrorRate}`
      );
    }

    if (metricsData.throughput < thresholds.minThroughput) {
      violations.push(
        `Throughput ${metricsData.throughput} below threshold ${thresholds.minThroughput}`
      );
    }

    if (metricsData.memoryUsage > thresholds.maxMemoryUsage) {
      violations.push(
        `Memory usage ${metricsData.memoryUsage} exceeds threshold ${thresholds.maxMemoryUsage}`
      );
    }

    return { violations, score: Math.max(0, 100 - violations.length * 15) };
  }

  /**
   * Validate against custom rules
   * @param {Array} spans - Collected spans
   * @param {Object} metricsData - Collected metrics
   * @param {Object} config - Validation config
   * @returns {Object} Validation result
   * @private
   */
  _validateRules(spans, metricsData, config) {
    const violations = [];

    for (const rule of config.validationRules) {
      try {
        const result = rule.condition(spans, metricsData);
        if (!result) {
          violations.push(`Rule '${rule.name}' failed`);
        }
      } catch (error) {
        violations.push(`Rule '${rule.name}' error: ${error.message}`);
      }
    }

    return { violations, score: Math.max(0, 100 - violations.length * 20) };
  }

  /**
   * Calculate overall validation score
   * @param {Object} spanValidation - Span validation result
   * @param {Object} metricValidation - Metric validation result
   * @param {Object} ruleValidation - Rule validation result
   * @returns {number} Overall score
   * @private
   */
  _calculateScore(spanValidation, metricValidation, ruleValidation) {
    const weights = { spans: 0.4, metrics: 0.4, rules: 0.2 };

    return Math.round(
      spanValidation.score * weights.spans +
        metricValidation.score * weights.metrics +
        ruleValidation.score * weights.rules
    );
  }

  /**
   * Clean up validation resources
   * @param {string} validationId - Validation ID
   * @private
   */
  _cleanupValidation(validationId) {
    this.spanCollector.delete(validationId);
    this.metricCollector.delete(validationId);
    this._validationTempSpans.delete(validationId);
  }

  /**
   * Get validation results for a feature
   * @param {string} feature - Feature name
   * @returns {Object|null} Validation result
   */
  getValidationResult(feature) {
    return this.validationResults.get(feature) || null;
  }

  /**
   * Get all validation results
   * @returns {Map} All validation results
   */
  getAllValidationResults() {
    return new Map(this.validationResults);
  }

  /**
   * Clear validation results
   */
  clearValidationResults() {
    this.validationResults.clear();
  }

  /**
   * Simulate feature operations (fallback)
   * @param {string} feature - Feature name
   * @param {Span} parentSpan - Parent span
   * @private
   */
  async _simulateFeatureOperations(feature, parentSpan) {
    const operations = this._getFeatureOperations(feature);

    for (const operation of operations) {
      await this.tracer.startActiveSpan(operation.name, { parent: parentSpan }, async span => {
        span.setAttributes(operation.attributes);
        await new Promise(resolve => setTimeout(resolve, operation.duration || 10));

        if (operation.shouldFail) {
          span.setStatus({
            code: SpanStatusCode.ERROR,
            message: 'Simulated failure',
          });
        } else {
          span.setStatus({ code: SpanStatusCode.OK });
        }
      });
    }
  }

  /**
   * Get operations for a specific feature
   * @param {string} feature - Feature name
   * @returns {Array} Operations to simulate
   * @private
   */
  _getFeatureOperations(feature) {
    const operationMap = {
      'knowledge-engine': [
        {
          name: 'parse.turtle',
          attributes: { format: 'turtle', size: 1000 },
          duration: 50,
        },
        {
          name: 'query.sparql',
          attributes: { 'query.type': 'select', results: 10 },
          duration: 30,
        },
        {
          name: 'validate.shacl',
          attributes: { shapes: 5, conforms: true },
          duration: 40,
        },
      ],
      'cli-parse': [
        {
          name: 'cli.parse',
          attributes: { 'input.file': 'test.ttl', format: 'turtle' },
          duration: 20,
        },
        {
          name: 'cli.output',
          attributes: { 'output.file': 'result.ttl', triples: 100 },
          duration: 10,
        },
        {
          name: 'parse.turtle',
          attributes: { format: 'turtle' },
          duration: 15,
        },
      ],
      'cli-query': [
        {
          name: 'cli.query',
          attributes: { query: 'SELECT * WHERE { ?s ?p ?o }', results: 50 },
          duration: 25,
        },
        {
          name: 'cli.format',
          attributes: { format: 'json', size: 1024 },
          duration: 5,
        },
        {
          name: 'query.sparql',
          attributes: { 'query.type': 'select' },
          duration: 20,
        },
      ],
      'cli-validate': [
        {
          name: 'cli.validate',
          attributes: { 'input.file': 'test.ttl', 'shapes.file': 'shapes.ttl' },
          duration: 30,
        },
        {
          name: 'validate.shacl',
          attributes: { conforms: true, violations: 0 },
          duration: 40,
        },
        { name: 'cli.report', attributes: { conforms: true }, duration: 10 },
      ],
      'cli-hook': [
        {
          name: 'cli.hook',
          attributes: { 'hook.name': 'test-hook', 'hook.kind': 'sparql-ask' },
          duration: 15,
        },
        {
          name: 'hook.evaluate',
          attributes: { 'hook.kind': 'sparql-ask', 'hook.fired': true },
          duration: 20,
        },
        {
          name: 'hook.result',
          attributes: { 'execution.time': 20, result: true },
          duration: 5,
        },
      ],
      'transaction-manager': [
        {
          name: 'transaction.start',
          attributes: {
            'transaction.id': 'tx-123',
            'transaction.type': 'rdf',
            'transaction.success': true,
          },
          duration: 10,
        },
        {
          name: 'transaction.commit',
          attributes: {
            'transaction.id': 'tx-123',
            'transaction.success': true,
          },
          duration: 15,
        },
      ],
    };

    return (
      operationMap[feature] || [
        { name: `${feature}.operation`, attributes: { feature }, duration: 15 },
      ]
    );
  }
}

/**
 * Create an OTEL validator instance
 * @param {Object} [config] - Configuration
 * @returns {OTELValidator} Validator instance
 */
export function createOTELValidator(config = {}) {
  return new OTELValidator(config);
}
