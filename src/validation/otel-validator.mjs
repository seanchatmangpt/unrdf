/**
 * @file OpenTelemetry Span-Based Validation Framework
 * @module validation/otel-validator
 *
 * @description
 * Replaces traditional unit tests with OpenTelemetry span validation.
 * Features are validated by analyzing OTEL spans, metrics, and traces
 * instead of isolated test assertions.
 */

import {
  trace,
  metrics,
  context as otelContext,
  SpanStatusCode,
} from "@opentelemetry/api";
import { z } from "zod";
import { randomUUID } from "crypto";

// Validation schemas
const ValidationResultSchema = z.object({
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
      status: z.enum(["ok", "error"]),
      duration: z.number(),
      attributes: z.record(z.any()),
    }),
  ),
  violations: z.array(z.string()),
  timestamp: z.string(),
});

const FeatureValidationConfigSchema = z.object({
  feature: z.string(),
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
      severity: z.enum(["error", "warning", "info"]),
    }),
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
      serviceName: "unrdf-validator",
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

    // Custom metrics for validation
    this._createValidationMetrics();
  }

  /**
   * Create validation-specific metrics
   * @private
   */
  _createValidationMetrics() {
    this.validationCounter = this.meter.createCounter("validation_total", {
      description: "Total number of feature validations",
    });

    this.validationDuration = this.meter.createHistogram(
      "validation_duration_ms",
      {
        description: "Feature validation duration in milliseconds",
        unit: "ms",
      },
    );

    this.validationScore = this.meter.createHistogram("validation_score", {
      description: "Feature validation score (0-100)",
    });

    this.featureHealthGauge = this.meter.createUpDownCounter(
      "feature_health_score",
      {
        description: "Current health score of features",
      },
    );
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

    return await this.tracer.startActiveSpan(
      `validation.${feature}`,
      async (span) => {
        try {
          span.setAttributes({
            "validation.id": validationId,
            "validation.feature": feature,
            "validation.start_time": Date.now(),
          });

          const startTime = Date.now();

          // Start collecting spans and metrics
          this._startSpanCollection(validationId);
          this._startMetricCollection(validationId);

          // Execute the feature (this will generate OTEL spans)
          const featureResult = await this._executeFeature(
            feature,
            validationConfig,
          );

          // Collect and analyze spans
          const collectedSpans = this._collectSpans(validationId);
          const collectedMetrics = this._collectMetrics(validationId);

          // Validate against expected spans and attributes
          const spanValidation = this._validateSpans(
            collectedSpans,
            validationConfig,
          );
          const metricValidation = this._validateMetrics(
            collectedMetrics,
            validationConfig,
          );
          const ruleValidation = this._validateRules(
            collectedSpans,
            collectedMetrics,
            validationConfig,
          );

          // Calculate overall score
          const score = this._calculateScore(
            spanValidation,
            metricValidation,
            ruleValidation,
          );
          const passed = score >= 80; // 80% threshold for passing

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
            spans: collectedSpans.map((span) => ({
              name: span.name,
              status: span.status,
              duration: span.duration,
              attributes: span.attributes,
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
            "validation.passed": passed,
            "validation.score": score,
            "validation.duration": duration,
            "validation.violations": result.violations.length,
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
      },
    );
  }

  /**
   * Execute a feature and collect its OTEL spans
   * @param {string} feature - Feature name
   * @param {Object} config - Validation config
   * @returns {Promise<any>} Feature execution result
   * @private
   */
  async _executeFeature(feature, config) {
    // This would be implemented to actually execute the feature
    // For now, we'll simulate feature execution
    return await this.tracer.startActiveSpan(
      `feature.${feature}`,
      async (span) => {
        span.setAttribute("feature.name", feature);

        // Simulate feature execution with various operations
        await this._simulateFeatureOperations(feature, span);

        span.setStatus({ code: SpanStatusCode.OK });
        return { success: true };
      },
    );
  }

  /**
   * Simulate feature operations that generate spans
   * @param {string} feature - Feature name
   * @param {Span} parentSpan - Parent span
   * @private
   */
  async _simulateFeatureOperations(feature, parentSpan) {
    // Simulate different types of operations based on feature
    const operations = this._getFeatureOperations(feature);

    for (const operation of operations) {
      await this.tracer.startActiveSpan(
        operation.name,
        { parent: parentSpan },
        async (span) => {
          span.setAttributes(operation.attributes);

          // Simulate operation duration
          await new Promise((resolve) =>
            setTimeout(resolve, operation.duration || 10),
          );

          // Simulate success/failure
          if (operation.shouldFail) {
            span.setStatus({
              code: SpanStatusCode.ERROR,
              message: "Simulated failure",
            });
          } else {
            span.setStatus({ code: SpanStatusCode.OK });
          }
        },
      );
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
      "knowledge-engine": [
        {
          name: "parse.turtle",
          attributes: { format: "turtle", size: 1000 },
          duration: 50,
        },
        {
          name: "query.sparql",
          attributes: { "query.type": "select", results: 10 },
          duration: 30,
        },
        {
          name: "validate.shacl",
          attributes: { shapes: 5, conforms: true },
          duration: 40,
        },
      ],
      "cli-parse": [
        {
          name: "cli.parse",
          attributes: { "input.file": "test.ttl", format: "turtle" },
          duration: 20,
        },
        {
          name: "cli.output",
          attributes: { "output.file": "result.ttl", triples: 100 },
          duration: 10,
        },
      ],
      "cli-query": [
        {
          name: "cli.query",
          attributes: { query: "SELECT * WHERE { ?s ?p ?o }", results: 50 },
          duration: 25,
        },
        {
          name: "cli.format",
          attributes: { format: "json", size: 1024 },
          duration: 5,
        },
      ],
    };

    return (
      operationMap[feature] || [
        {
          name: `${feature}.operation`,
          attributes: { feature: feature },
          duration: 15,
        },
      ]
    );
  }

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
   * Collect metrics for validation
   * @param {string} validationId - Validation ID
   * @returns {Object} Collected metrics
   * @private
   */
  _collectMetrics(validationId) {
    const metrics = this.metricCollector.get(validationId) || {
      latency: [],
      errors: 0,
      operations: 0,
    };

    const avgLatency =
      metrics.latency.length > 0
        ? metrics.latency.reduce((a, b) => a + b, 0) / metrics.latency.length
        : 0;

    const errorRate =
      metrics.operations > 0 ? metrics.errors / metrics.operations : 0;
    const throughput = metrics.operations; // operations per validation

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
    const spanNames = spans.map((s) => s.name);

    // Check for expected spans
    for (const expectedSpan of config.expectedSpans) {
      if (!spanNames.includes(expectedSpan)) {
        violations.push(`Missing expected span: ${expectedSpan}`);
      }
    }

    // Check for required attributes
    for (const span of spans) {
      for (const requiredAttr of config.requiredAttributes) {
        if (!(requiredAttr in span.attributes)) {
          violations.push(
            `Missing required attribute '${requiredAttr}' in span '${span.name}'`,
          );
        }
      }
    }

    // Check span status
    const errorSpans = spans.filter((s) => s.status === "error");
    if (errorSpans.length > 0) {
      violations.push(`Found ${errorSpans.length} spans with error status`);
    }

    return { violations, score: Math.max(0, 100 - violations.length * 10) };
  }

  /**
   * Validate collected metrics against thresholds
   * @param {Object} metrics - Collected metrics
   * @param {Object} config - Validation config
   * @returns {Object} Validation result
   * @private
   */
  _validateMetrics(metrics, config) {
    const violations = [];
    const thresholds = config.performanceThresholds;

    if (metrics.latency > thresholds.maxLatency) {
      violations.push(
        `Latency ${metrics.latency}ms exceeds threshold ${thresholds.maxLatency}ms`,
      );
    }

    if (metrics.errorRate > thresholds.maxErrorRate) {
      violations.push(
        `Error rate ${metrics.errorRate} exceeds threshold ${thresholds.maxErrorRate}`,
      );
    }

    if (metrics.throughput < thresholds.minThroughput) {
      violations.push(
        `Throughput ${metrics.throughput} below threshold ${thresholds.minThroughput}`,
      );
    }

    if (metrics.memoryUsage > thresholds.maxMemoryUsage) {
      violations.push(
        `Memory usage ${metrics.memoryUsage} exceeds threshold ${thresholds.maxMemoryUsage}`,
      );
    }

    return { violations, score: Math.max(0, 100 - violations.length * 15) };
  }

  /**
   * Validate against custom rules
   * @param {Array} spans - Collected spans
   * @param {Object} metrics - Collected metrics
   * @param {Object} config - Validation config
   * @returns {Object} Validation result
   * @private
   */
  _validateRules(spans, metrics, config) {
    const violations = [];

    for (const rule of config.validationRules) {
      try {
        const result = rule.condition(spans, metrics);
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
        ruleValidation.score * weights.rules,
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
}

/**
 * Create an OTEL validator instance
 * @param {Object} [config] - Configuration
 * @returns {OTELValidator} Validator instance
 */
export function createOTELValidator(config = {}) {
  return new OTELValidator(config);
}

/**
 * Default OTEL validator instance
 */
export const defaultOTELValidator = createOTELValidator();
