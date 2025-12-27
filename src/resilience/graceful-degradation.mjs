/**
 * @fileoverview Graceful Degradation Strategies
 *
 * **Purpose**: Handle partial failures without complete system collapse
 * - Partial agent failure handling (continue with successful agents)
 * - Quality-of-Service (QoS) levels (fast-but-lossy vs slow-but-perfect)
 * - Fallback strategies per operation type
 * - Degradation metrics and monitoring
 *
 * @module resilience/graceful-degradation
 */

import { trace, SpanStatusCode } from '@opentelemetry/api';
import { z } from 'zod';

/**
 * QoS level enumeration
 */
export const QoSLevel = {
  PERFECT: 'perfect', // 100% quality, may be slow
  HIGH: 'high', // 90%+ quality, balanced speed
  MEDIUM: 'medium', // 70%+ quality, faster
  LOW: 'low', // 50%+ quality, fastest
  MINIMAL: 'minimal', // Basic functionality only
};

/**
 * Degradation strategy schema
 */
export const DegradationStrategySchema = z.object({
  name: z.string(),
  qosLevel: z.enum(['perfect', 'high', 'medium', 'low', 'minimal']),
  fallbackFn: z.function(),
  qualityMetric: z.number().min(0).max(100).optional(),
  estimatedLatencyMs: z.number().min(0).optional(),
});

/**
 * Graceful Degradation Manager
 *
 * @example
 * const manager = new GracefulDegradationManager();
 * const result = await manager.executeWithDegradation(
 *   async () => complexOperation(),
 *   [simpleStrategy, cacheStrategy]
 * );
 */
export class GracefulDegradationManager {
  /**
   * Create a new graceful degradation manager
   * @param {Object} [config] - Manager configuration
   */
  constructor(config = {}) {
    this.config = {
      enableDegradation: config.enableDegradation ?? true,
      defaultQoS: config.defaultQoS ?? QoSLevel.HIGH,
      trackMetrics: config.trackMetrics ?? true,
    };

    this.tracer = trace.getTracer('unrdf-graceful-degradation');
    this.metrics = {
      totalOperations: 0,
      degradedOperations: 0,
      fallbacksUsed: 0,
      byQoSLevel: {
        perfect: 0,
        high: 0,
        medium: 0,
        low: 0,
        minimal: 0,
      },
    };
  }

  /**
   * Execute operation with graceful degradation
   * @param {Function} primaryOperation - Primary operation to attempt
   * @param {Array<Object>} fallbackStrategies - Ordered fallback strategies (best to worst)
   * @param {Object} [context] - Execution context
   * @returns {Promise<Object>} Result with QoS metadata
   */
  async executeWithDegradation(primaryOperation, fallbackStrategies = [], context = {}) {
    return await this.tracer.startActiveSpan(
      'degradation.execute',
      async (span) => {
        span.setAttributes({
          'degradation.enabled': this.config.enableDegradation,
          'degradation.fallback_count': fallbackStrategies.length,
          ...context,
        });

        this.metrics.totalOperations++;

        // Try primary operation first
        try {
          const result = await primaryOperation();

          span.setAttributes({
            'degradation.used': false,
            'degradation.qos_level': QoSLevel.PERFECT,
          });
          span.setStatus({ code: SpanStatusCode.OK });

          this.metrics.byQoSLevel.perfect++;

          return {
            result,
            qosLevel: QoSLevel.PERFECT,
            degraded: false,
            strategy: 'primary',
          };
        } catch (primaryError) {
          span.recordException(primaryError);
          span.setAttributes({
            'degradation.primary_failed': true,
            'degradation.error': primaryError.message,
          });

          // If degradation disabled, fail immediately
          if (!this.config.enableDegradation || fallbackStrategies.length === 0) {
            span.setStatus({
              code: SpanStatusCode.ERROR,
              message: 'No degradation strategies available',
            });
            span.end();
            throw primaryError;
          }

          // Try fallback strategies in order
          return await this._tryFallbacks(
            fallbackStrategies,
            primaryError,
            context,
            span
          );
        } finally {
          span.end();
        }
      }
    );
  }

  /**
   * Try fallback strategies in order
   * @param {Array<Object>} strategies - Fallback strategies
   * @param {Error} primaryError - Primary operation error
   * @param {Object} context - Execution context
   * @param {Object} span - OTEL span
   * @returns {Promise<Object>} Result from fallback
   * @private
   */
  async _tryFallbacks(strategies, primaryError, context, span) {
    this.metrics.degradedOperations++;

    for (let i = 0; i < strategies.length; i++) {
      const strategy = strategies[i];

      try {
        span.setAttributes({
          'degradation.fallback_index': i,
          'degradation.fallback_name': strategy.name,
          'degradation.qos_level': strategy.qosLevel,
        });

        console.warn(
          `[Degradation] Using fallback strategy '${strategy.name}' (QoS: ${strategy.qosLevel})`
        );

        const result = await strategy.fallbackFn(context);

        this.metrics.fallbacksUsed++;
        this.metrics.byQoSLevel[strategy.qosLevel]++;

        span.setAttributes({
          'degradation.used': true,
          'degradation.strategy': strategy.name,
          'degradation.success': true,
        });
        span.setStatus({ code: SpanStatusCode.OK });

        return {
          result,
          qosLevel: strategy.qosLevel,
          degraded: true,
          strategy: strategy.name,
          qualityMetric: strategy.qualityMetric,
          estimatedLatencyMs: strategy.estimatedLatencyMs,
        };
      } catch (fallbackError) {
        span.recordException(fallbackError);
        span.setAttributes({
          'degradation.fallback_failed': true,
          'degradation.fallback_error': fallbackError.message,
        });

        console.warn(
          `[Degradation] Fallback '${strategy.name}' failed: ${fallbackError.message}`
        );

        // Continue to next fallback
        continue;
      }
    }

    // All fallbacks failed
    span.setStatus({
      code: SpanStatusCode.ERROR,
      message: 'All degradation strategies exhausted',
    });

    throw new DegradationExhaustedError(
      'Primary operation and all fallback strategies failed',
      primaryError,
      strategies.length
    );
  }

  /**
   * Execute batch of operations with partial failure tolerance
   * @param {Array<Function>} operations - Operations to execute
   * @param {Object} [options] - Execution options
   * @returns {Promise<Object>} Batch result with successes and failures
   */
  async executeBatchWithTolerance(operations, options = {}) {
    const {
      continueOnFailure = true,
      minSuccessCount = 1,
      minSuccessRatio = 0.5,
    } = options;

    return await this.tracer.startActiveSpan(
      'degradation.batch',
      async (span) => {
        span.setAttributes({
          'batch.total_operations': operations.length,
          'batch.continue_on_failure': continueOnFailure,
          'batch.min_success_count': minSuccessCount,
        });

        const results = [];
        const failures = [];

        for (let i = 0; i < operations.length; i++) {
          try {
            const result = await operations[i]();
            results.push({ index: i, success: true, result });
          } catch (error) {
            failures.push({ index: i, error });

            if (!continueOnFailure) {
              span.setAttributes({
                'batch.stopped_at': i,
                'batch.stop_reason': 'failure_intolerant',
              });
              throw new BatchFailureError(
                `Batch stopped at index ${i}: ${error.message}`,
                results,
                failures
              );
            }

            results.push({ index: i, success: false, error });
          }
        }

        const successCount = results.filter((r) => r.success).length;
        const successRatio = successCount / operations.length;

        span.setAttributes({
          'batch.success_count': successCount,
          'batch.failure_count': failures.length,
          'batch.success_ratio': successRatio,
        });

        // Check minimum success requirements
        if (successCount < minSuccessCount) {
          span.setStatus({
            code: SpanStatusCode.ERROR,
            message: 'Minimum success count not met',
          });
          throw new BatchFailureError(
            `Only ${successCount}/${operations.length} operations succeeded (minimum: ${minSuccessCount})`,
            results,
            failures
          );
        }

        if (successRatio < minSuccessRatio) {
          span.setStatus({
            code: SpanStatusCode.ERROR,
            message: 'Minimum success ratio not met',
          });
          throw new BatchFailureError(
            `Success ratio ${(successRatio * 100).toFixed(1)}% below minimum ${(minSuccessRatio * 100).toFixed(1)}%`,
            results,
            failures
          );
        }

        span.setStatus({ code: SpanStatusCode.OK });
        span.end();

        return {
          totalOperations: operations.length,
          successCount,
          failureCount: failures.length,
          successRatio,
          results,
          failures,
          degraded: failures.length > 0,
        };
      }
    );
  }

  /**
   * Get degradation metrics
   * @returns {Object} Metrics
   */
  getMetrics() {
    return {
      ...this.metrics,
      degradationRate:
        this.metrics.totalOperations > 0
          ? (
              (this.metrics.degradedOperations / this.metrics.totalOperations) *
              100
            ).toFixed(2) + '%'
          : '0%',
      fallbackRate:
        this.metrics.totalOperations > 0
          ? ((this.metrics.fallbacksUsed / this.metrics.totalOperations) * 100).toFixed(
              2
            ) + '%'
          : '0%',
    };
  }

  /**
   * Reset metrics
   */
  resetMetrics() {
    this.metrics = {
      totalOperations: 0,
      degradedOperations: 0,
      fallbacksUsed: 0,
      byQoSLevel: {
        perfect: 0,
        high: 0,
        medium: 0,
        low: 0,
        minimal: 0,
      },
    };
  }
}

/**
 * Error thrown when all degradation strategies fail
 */
export class DegradationExhaustedError extends Error {
  /**
   * Create a degradation exhausted error
   * @param {string} message - Error message
   * @param {Error} primaryError - Primary operation error
   * @param {number} strategiesAttempted - Number of strategies attempted
   */
  constructor(message, primaryError, strategiesAttempted) {
    super(message);
    this.name = 'DegradationExhaustedError';
    this.primaryError = primaryError;
    this.strategiesAttempted = strategiesAttempted;
  }
}

/**
 * Error thrown when batch operations fail minimum requirements
 */
export class BatchFailureError extends Error {
  /**
   * Create a batch failure error
   * @param {string} message - Error message
   * @param {Array} results - Results array
   * @param {Array} failures - Failures array
   */
  constructor(message, results, failures) {
    super(message);
    this.name = 'BatchFailureError';
    this.results = results;
    this.failures = failures;
  }
}

/**
 * Common fallback strategies
 */
export const FallbackStrategies = {
  /**
   * Skip operation strategy
   */
  skip: (name = 'skip') => ({
    name,
    qosLevel: QoSLevel.MINIMAL,
    fallbackFn: async () => ({ skipped: true }),
    qualityMetric: 0,
    estimatedLatencyMs: 0,
  }),

  /**
   * Cache-based strategy
   */
  cache: (cacheFn, name = 'cache') => ({
    name,
    qosLevel: QoSLevel.MEDIUM,
    fallbackFn: cacheFn,
    qualityMetric: 70,
    estimatedLatencyMs: 10,
  }),

  /**
   * Simple/degraded quality strategy
   */
  simple: (simpleFn, name = 'simple') => ({
    name,
    qosLevel: QoSLevel.LOW,
    fallbackFn: simpleFn,
    qualityMetric: 50,
    estimatedLatencyMs: 100,
  }),

  /**
   * Default value strategy
   */
  defaultValue: (value, name = 'default') => ({
    name,
    qosLevel: QoSLevel.MINIMAL,
    fallbackFn: async () => value,
    qualityMetric: 20,
    estimatedLatencyMs: 0,
  }),
};
