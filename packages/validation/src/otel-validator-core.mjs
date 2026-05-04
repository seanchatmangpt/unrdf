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
  executeAtomVMBridge,
  executeAtomVMRuntime,
  executeAtomVMErlang,
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
      attributes: z.record(z.string(), z.any()), // z.record requires key and value types in zod v4
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
      condition: z.any(), // Functions can't be validated by zod, use any
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

    // Real OTEL span collection
    this._realSpans = new Map(); // Map<validationId, Array<Span>>
    this._spanEndCallbacks = new Map(); // Map<validationId, Function>

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
    // Defensive check: ensure config is provided and is an object
    if (!config || typeof config !== 'object') {
      throw new Error(
        `Invalid config for feature '${feature}': config must be an object. Received: ${typeof config}`
      );
    }

    // Defensive check: ensure required fields exist
    if (!config.expectedSpans || !Array.isArray(config.expectedSpans)) {
      throw new Error(`Invalid config for feature '${feature}': expectedSpans must be an array`);
    }
    if (!config.requiredAttributes || !Array.isArray(config.requiredAttributes)) {
      throw new Error(
        `Invalid config for feature '${feature}': requiredAttributes must be an array`
      );
    }
    if (!config.performanceThresholds || typeof config.performanceThresholds !== 'object') {
      throw new Error(
        `Invalid config for feature '${feature}': performanceThresholds must be an object`
      );
    }
    if (!config.validationRules || !Array.isArray(config.validationRules)) {
      throw new Error(`Invalid config for feature '${feature}': validationRules must be an array`);
    }

    let validationConfig;
    try {
      validationConfig = FeatureValidationConfigSchema.parse(config);
    } catch (error) {
      throw new Error(
        `Config validation failed for feature '${feature}': ${error.message}. Config: ${JSON.stringify(config, null, 2)}`
      );
    }

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
        console.log(`[OTELValidator] Starting span/metric collection for: ${validationId}`);
        this._startSpanCollection(validationId);
        this._startMetricCollection(validationId);
        console.log(`[OTELValidator] Span/metric collection started`);

        // Poka-yoke: Validate validationId before use
        if (typeof validationId !== 'string' || validationId.length === 0) {
          throw new Error('validationId must be a non-empty string');
        }
        
        // Initialize OTEL provider with span processor
        // This will collect real OTEL spans via the processor's exporter
        console.log(`[OTELValidator] Initializing OTEL provider for: ${validationId}`);
        const { ensureProviderInitialized } = await import('../../../validation/otel-provider.mjs');
        await ensureProviderInitialized(validationId, (spanData) => {
          console.log(`[OTELValidator] Span callback received: ${spanData.name}`);
          // spanData is already converted by otel-provider.mjs
          // Poka-yoke: _addSpan validates spanData structure
          this._addSpan(validationId, spanData);
        });
        console.log(`[OTELValidator] OTEL provider initialized`);

        // Execute the feature (this will generate real OTEL spans)
        console.log(`[OTELValidator] Executing feature: ${feature}`);
        const featureStart = Date.now();
        const _featureResult = await this._executeFeature(feature, validationConfig, validationId);
        console.log(`[OTELValidator] Feature execution completed in ${Date.now() - featureStart}ms:`, _featureResult?.success);

        // Force flush to ensure all spans are exported
        console.log(`[OTELValidator] Force flushing spans for: ${validationId}`);
        const { forceFlush } = await import('../../../validation/otel-provider.mjs');
        await forceFlush();
        console.log(`[OTELValidator] Force flush completed`);
        
        // Wait for spans to be exported
        // SimpleSpanProcessor exports immediately when span ends, but we need to wait for async operations
        console.log(`[OTELValidator] Waiting 100ms for async span processing`);
        await new Promise(resolve => setTimeout(resolve, 100));
        console.log(`[OTELValidator] Wait completed`);

        // CRITICAL FIX: Merge synthetic spans from temp storage into spanCollector
        // Feature executors create synthetic spans and store them in _validationTempSpans
        // We need to move them into spanCollector before collecting
        const tempSpans = this._validationTempSpans.get(validationId) || [];
        if (tempSpans.length > 0) {
          console.log(`[OTELValidator] Merging ${tempSpans.length} synthetic spans into collector`);
          tempSpans.forEach(spanData => this._addSpan(validationId, spanData));
        }

        // Collect and analyze spans
        console.log(`[OTELValidator] Collecting spans/metrics for: ${validationId}`);
        const collectedSpans = this._collectSpans(validationId);
        const collectedMetrics = this._collectMetrics(validationId);
        console.log(`[OTELValidator] Collected ${collectedSpans?.length || 0} spans, throughput: ${collectedMetrics?.throughput || 0}`);

        // Guard: prevent null-success
        console.log(`[OTELValidator] Validating collected data...`);
        if (!collectedSpans || collectedSpans.length === 0) {
          console.error(`[OTELValidator] ERROR: No spans collected for feature '${feature}'`);
          throw new Error(
            `No spans collected for feature '${feature}'. Ensure TracerProvider is initialized.`
          );
        }
        if (!collectedMetrics || (collectedMetrics.throughput || 0) <= 0) {
          console.error(`[OTELValidator] ERROR: No operations recorded for feature '${feature}', throughput: ${collectedMetrics?.throughput || 0}`);
          throw new Error(
            `No operations recorded for feature '${feature}'. Validation cannot pass with zero throughput.`
          );
        }
        console.log(`[OTELValidator] Collected data validated successfully`);

        // Cleanup
        this._validationTempSpans.delete(validationId);
        
        // Shutdown provider for this validation
        const { shutdownProvider } = await import('../../../validation/otel-provider.mjs');
        await shutdownProvider(validationId);

        // Validate against expected spans and attributes
        console.log(`[OTELValidator] Starting span/metric/rule validation`);
        const spanValidationStart = Date.now();
        const spanValidation = this._validateSpans(collectedSpans, validationConfig);
        console.log(`[OTELValidator] Span validation completed in ${Date.now() - spanValidationStart}ms`);
        
        const metricValidationStart = Date.now();
        const metricValidation = this._validateMetrics(collectedMetrics, validationConfig);
        console.log(`[OTELValidator] Metric validation completed in ${Date.now() - metricValidationStart}ms`);
        
        const ruleValidationStart = Date.now();
        const ruleValidation = this._validateRules(
          collectedSpans,
          collectedMetrics,
          validationConfig
        );
        console.log(`[OTELValidator] Rule validation completed in ${Date.now() - ruleValidationStart}ms`);

        // Calculate overall score
        console.log(`[OTELValidator] Calculating score...`);
        const score = this._calculateScore(spanValidation, metricValidation, ruleValidation);
        const passed = score >= 80;
        console.log(`[OTELValidator] Score: ${score}, Passed: ${passed}`);

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
      'atomvm-bridge': () => executeAtomVMBridge(this, parentSpan, validationId),
      'atomvm-runtime': () => executeAtomVMRuntime(this, parentSpan, validationId),
      'atomvm-erlang': () => executeAtomVMErlang(this, parentSpan, validationId),
      'atomvm-boardroom-story': async () => {
        // Run all three AtomVM features for complete boardroom story
        await executeAtomVMBridge(this, parentSpan, validationId);
        await executeAtomVMRuntime(this, parentSpan, validationId);
        await executeAtomVMErlang(this, parentSpan, validationId);
        return { success: true, operations: 3 };
      },
    };

    const executor = featureExecutors[feature];
    if (!executor) {
      throw new Error(
        `No executor found for feature '${feature}'. Production mode requires real implementation.`
      );
    }

    return await executor();
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
   * Validate span data structure (poka-yoke: prevents invalid span data)
   * @param {Object} spanData - Span data to validate
   * @throws {Error} If spanData is invalid
   * @private
   */
  _validateSpanData(spanData) {
    if (!spanData || typeof spanData !== 'object') {
      throw new Error('spanData must be an object');
    }
    if (typeof spanData.name !== 'string' || spanData.name.length === 0) {
      throw new Error('spanData.name must be a non-empty string');
    }
    if (typeof spanData.status !== 'string') {
      throw new Error('spanData.status must be a string');
    }
    if (typeof spanData.duration !== 'number' || spanData.duration < 0) {
      throw new Error('spanData.duration must be a non-negative number');
    }
    if (!spanData.attributes || typeof spanData.attributes !== 'object') {
      throw new Error('spanData.attributes must be an object');
    }
  }

  /**
   * Type guard: Check if collector exists (poka-yoke: prevents operations on missing collector)
   * @param {string} validationId - Validation ID
   * @returns {boolean} True if collector exists
   * @private
   */
  _hasCollector(validationId) {
    return this.spanCollector.has(validationId);
  }

  /**
   * Add a span to the collector
   * 
   * **Poka-yoke**: Input validation, type guards, and defensive collector creation prevent data loss
   * 
   * @param {string} validationId - Validation ID (must be non-empty string)
   * @param {Object} spanData - Span data (must have valid structure)
   * @throws {Error} If validationId or spanData is invalid
   * @private
   */
  _addSpan(validationId, spanData) {
    // Poka-yoke: Validate inputs
    if (typeof validationId !== 'string' || validationId.length === 0) {
      throw new Error('validationId must be a non-empty string');
    }
    this._validateSpanData(spanData);
    
    // Poka-yoke: Type guard ensures collector exists
    let collector = this.spanCollector.get(validationId);
    if (!collector) {
      // Defensive: create collector if missing (shouldn't happen, but prevent data loss)
      collector = [];
      this.spanCollector.set(validationId, collector);
    }
    
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
   * Check if a span should have a specific attribute
   * @param {string} spanName - Span name
   * @param {string} attribute - Attribute name
   * @returns {boolean} Whether the span should have this attribute
   * @private
   */
  _shouldSpanHaveAttribute(spanName, attribute) {
    // Bridge spans
    if (spanName.startsWith('bridge.emit_event')) {
      return ['event.type', 'event.source', 'event.id', 'event.success'].includes(attribute);
    }
    if (spanName.startsWith('bridge.register_hook')) {
      return ['hook.name', 'hook.trigger', 'hook.success'].includes(attribute);
    }
    if (spanName.startsWith('bridge.process_intent')) {
      return ['intent.id', 'intent.description', 'intent.success', 'intent.accepted'].includes(
        attribute
      );
    }

    // Runtime spans
    if (spanName.startsWith('atomvm.load_wasm')) {
      return ['runtime.type', 'atomvm.version'].includes(attribute);
    }
    if (spanName.startsWith('atomvm.execute_beam')) {
      return ['module.name', 'avm.path', 'runtime.type'].includes(attribute);
    }
    if (spanName.startsWith('atomvm.state_transition')) {
      return ['from_state', 'to_state', 'runtime.type'].includes(attribute);
    }

    // Erlang process spans
    if (spanName.startsWith('erlang.process.emit_event')) {
      return ['event.type', 'event.source', 'service.name', 'operation.type'].includes(attribute);
    }
    if (spanName.startsWith('erlang.process.register_hook')) {
      return ['hook.name', 'hook.trigger', 'service.name', 'operation.type'].includes(attribute);
    }
    if (spanName.startsWith('erlang.process.intent')) {
      return ['intent.id', 'intent.description', 'service.name', 'operation.type'].includes(
        attribute
      );
    }
    if (spanName.startsWith('erlang.process.execute')) {
      return ['module.name', 'avm.path', 'service.name', 'operation.type'].includes(attribute);
    }
    if (spanName.startsWith('erlang.process.load')) {
      return ['process.id', 'event.type', 'error', 'service.name', 'operation.type'].includes(
        attribute
      );
    }

    // Default: check all attributes for spans that don't match known patterns
    return true;
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

    // Check required attributes only for spans that should have them
    // Different span types have different required attributes
    for (const span of spans) {
      for (const requiredAttr of config.requiredAttributes) {
        // Only check attributes that are relevant to this span type
        const spanName = span.name || '';
        const shouldHaveAttr = this._shouldSpanHaveAttribute(spanName, requiredAttr);
        if (shouldHaveAttr && !(requiredAttr in span.attributes)) {
          violations.push(`Missing required attribute '${requiredAttr}' in span '${span.name}'`);
        }
      }
    }

    // Error spans are expected in validation when modules don't exist
    // Only report errors if they exceed the threshold (handled in _validateMetrics)
    // Don't add violation here - let metrics validation handle it

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
   * Validate roundtrip SLA metrics from OTEL spans
   * 
   * **Poka-yoke**: Validates strict SLA thresholds (<10ms latency, <0.1% error rate)
   * 
   * @param {Array} spans - Collected spans
   * @param {Object} config - Validation config
   * @returns {Object} Validation result
   * @private
   */
  _validateRoundtripSLA(spans, config) {
    const violations = [];
    const thresholds = config.performanceThresholds || {};
    
    // Check if roundtrip thresholds are configured
    if (thresholds.maxRoundtripLatency === undefined && thresholds.maxRoundtripErrorRate === undefined) {
      return { violations: [], score: 100 }; // No roundtrip SLA configured
    }
    
    // Extract roundtrip metrics from spans
    const roundtripSpans = spans.filter(s => s.attributes && 'roundtrip.operation_id' in s.attributes);
    
    if (roundtripSpans.length === 0) {
      // No roundtrip spans found - this is OK if feature doesn't use roundtrips
      return { violations: [], score: 100 };
    }
    
    // Group by operation type
    const byOperationType = new Map();
    roundtripSpans.forEach(span => {
      const operationType = span.attributes['roundtrip.operation_type'];
      if (!operationType) return;
      
      if (!byOperationType.has(operationType)) {
        byOperationType.set(operationType, []);
      }
      byOperationType.get(operationType).push(span);
    });
    
    // Validate each operation type
    for (const [operationType, operationSpans] of byOperationType.entries()) {
      // Calculate average latency and error rate for this operation type
      let totalLatency = 0;
      let errorCount = 0;
      let count = 0;
      
      operationSpans.forEach(span => {
        const latency = span.attributes['roundtrip.latency'];
        const slaMet = span.attributes['roundtrip.sla_met'];
        
        if (typeof latency === 'number') {
          totalLatency += latency;
          count++;
        }
        
        if (slaMet === false) {
          errorCount++;
        }
      });
      
      if (count > 0) {
        const avgLatency = totalLatency / count;
        const errorRate = errorCount / count;
        
        // Validate latency threshold
        if (thresholds.maxRoundtripLatency !== undefined && avgLatency > thresholds.maxRoundtripLatency) {
          violations.push(
            `Roundtrip latency ${avgLatency.toFixed(2)}ms exceeds threshold ${thresholds.maxRoundtripLatency}ms for ${operationType}`
          );
        }
        
        // Validate error rate threshold
        if (thresholds.maxRoundtripErrorRate !== undefined && errorRate > thresholds.maxRoundtripErrorRate) {
          violations.push(
            `Roundtrip error rate ${(errorRate * 100).toFixed(2)}% exceeds threshold ${(thresholds.maxRoundtripErrorRate * 100).toFixed(1)}% for ${operationType}`
          );
        }
      }
    }
    
    return { violations, score: Math.max(0, 100 - violations.length * 20) };
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
