/**
 * @file Real-time SHACL Validator for streaming updates
 * @module streaming/real-time-validator
 *
 * @description
 * Provides incremental SHACL validation on stream updates with efficient
 * delta validation, violation detection, and integration with hooks.
 */

import { EventEmitter } from 'events';
import { Store } from 'n3';
import { z } from 'zod';
import { trace, SpanStatusCode } from '@opentelemetry/api';
import { validateShacl } from '../validate.mjs';
import { createObservabilityManager } from '../observability.mjs';

const tracer = trace.getTracer('unrdf-streaming');

/**
 * Validation mode
 * @enum {string}
 */
export const ValidationMode = {
  FULL: 'full',
  INCREMENTAL: 'incremental',
  DELTA: 'delta'
};

/**
 * Validator configuration schema
 */
const ValidatorConfigSchema = z.object({
  mode: z.enum([ValidationMode.FULL, ValidationMode.INCREMENTAL, ValidationMode.DELTA]).default(ValidationMode.DELTA),
  shapes: z.any(),
  strict: z.boolean().default(false),
  enableCaching: z.boolean().default(true),
  cacheSize: z.number().min(1).default(100),
  debounceMs: z.number().min(0).default(100),
  observability: z.object({
    serviceName: z.string().default('unrdf-real-time-validator'),
    enableTracing: z.boolean().default(true),
    enableMetrics: z.boolean().default(true)
  }).optional()
});

/**
 * Validation result schema
 */
const ValidationResultSchema = z.object({
  id: z.string(),
  timestamp: z.number(),
  mode: z.enum([ValidationMode.FULL, ValidationMode.INCREMENTAL, ValidationMode.DELTA]),
  conforms: z.boolean(),
  violations: z.array(z.any()),
  warnings: z.array(z.any()),
  deltaHash: z.string().optional(),
  duration: z.number()
});

/**
 * Real-time SHACL Validator
 * Validates RDF streams with incremental SHACL validation
 */
export class RealTimeValidator extends EventEmitter {
  /**
   * Create a new real-time validator
   * @param {Object} config - Validator configuration
   */
  constructor(config = {}) {
    super();
    this.config = ValidatorConfigSchema.parse(config);
    this.shapesStore = this._initializeShapes(this.config.shapes);
    this.validationCache = new Map();
    this.debounceTimer = null;
    this.pendingValidations = [];
    this.isValidating = false;

    // Observability
    this.observability = createObservabilityManager(
      this.config.observability || {}
    );

    // Metrics
    this.metrics = {
      validationsPerformed: 0,
      violationsDetected: 0,
      cacheHits: 0,
      cacheMisses: 0,
      errorCount: 0,
      validationLatency: []
    };

    // Initialize observability
    this.observability.initialize().catch(err => {
      console.warn('[RealTimeValidator] Failed to initialize observability:', err.message);
    });
  }

  /**
   * Initialize SHACL shapes
   * @private
   */
  _initializeShapes(shapes) {
    if (!shapes) {
      throw new Error('SHACL shapes are required');
    }

    if (typeof shapes === 'string') {
      // Parse Turtle string
      const { Parser } = require('n3');
      return new Store(new Parser().parse(shapes));
    } else if (shapes instanceof Store) {
      return shapes;
    } else {
      throw new TypeError('Shapes must be a Store or Turtle string');
    }
  }

  /**
   * Validate a delta change
   * @param {Object} delta - Transaction delta
   * @param {Store} [store] - Current store state
   * @returns {Promise<Object>} Validation result
   */
  async validateDelta(delta, store) {
    return tracer.startActiveSpan('real-time-validator.validate-delta', async (span) => {
      const startTime = Date.now();

      try {
        span.setAttributes({
          'validation.mode': this.config.mode,
          'delta.additions': delta.additions?.length || 0,
          'delta.removals': delta.removals?.length || 0
        });

        // Generate delta hash for caching
        const deltaHash = this._hashDelta(delta);

        // Check cache if enabled
        if (this.config.enableCaching) {
          const cached = this.validationCache.get(deltaHash);
          if (cached) {
            this.metrics.cacheHits++;
            span.setAttribute('validation.cache_hit', true);
            span.setStatus({ code: SpanStatusCode.OK });
            span.end();
            return cached;
          }
          this.metrics.cacheMisses++;
        }

        let result;

        switch (this.config.mode) {
          case ValidationMode.FULL:
            result = await this._validateFull(store);
            break;
          case ValidationMode.INCREMENTAL:
            result = await this._validateIncremental(delta, store);
            break;
          case ValidationMode.DELTA:
            result = await this._validateDeltaOnly(delta);
            break;
          default:
            throw new Error(`Unknown validation mode: ${this.config.mode}`);
        }

        const validationResult = ValidationResultSchema.parse({
          id: `validation-${Date.now()}-${Math.random().toString(36).substr(2, 9)}`,
          timestamp: Date.now(),
          mode: this.config.mode,
          conforms: result.conforms,
          violations: result.results?.filter(r => r.severity === 'http://www.w3.org/ns/shacl#Violation') || [],
          warnings: result.results?.filter(r => r.severity === 'http://www.w3.org/ns/shacl#Warning') || [],
          deltaHash,
          duration: Date.now() - startTime
        });

        // Cache result
        if (this.config.enableCaching) {
          this._addToCache(deltaHash, validationResult);
        }

        // Update metrics
        this.metrics.validationsPerformed++;
        this.metrics.violationsDetected += validationResult.violations.length;
        this.metrics.validationLatency.push(validationResult.duration);
        if (this.metrics.validationLatency.length > 1000) {
          this.metrics.validationLatency.shift();
        }

        // Emit events
        if (!validationResult.conforms) {
          this.emit('violation', validationResult);
        }
        this.emit('validated', validationResult);

        span.setAttributes({
          'validation.conforms': validationResult.conforms,
          'validation.violations': validationResult.violations.length,
          'validation.duration_ms': validationResult.duration
        });

        span.setStatus({ code: SpanStatusCode.OK });
        span.end();

        return validationResult;
      } catch (error) {
        span.recordException(error);
        span.setStatus({
          code: SpanStatusCode.ERROR,
          message: error.message
        });
        span.end();
        this.metrics.errorCount++;
        this.observability.recordError(error, { context: 'validate-delta' });
        throw error;
      }
    });
  }

  /**
   * Validate full store
   * @private
   */
  async _validateFull(store) {
    if (!store) {
      throw new Error('Store is required for full validation');
    }

    return validateShacl(store, this.shapesStore, {
      strict: this.config.strict,
      includeDetails: true
    });
  }

  /**
   * Validate incrementally (affected nodes only)
   * @private
   */
  async _validateIncremental(delta, store) {
    if (!store) {
      throw new Error('Store is required for incremental validation');
    }

    // Extract affected subjects from delta
    const affectedSubjects = new Set();

    for (const quad of [...(delta.additions || []), ...(delta.removals || [])]) {
      affectedSubjects.add(quad.subject.value);
    }

    // Create a filtered store with only affected subgraphs
    const filteredStore = new Store();

    for (const subject of affectedSubjects) {
      const quads = store.getQuads(subject, null, null, null);
      for (const quad of quads) {
        filteredStore.addQuad(quad);
      }
    }

    // Validate filtered store
    return validateShacl(filteredStore, this.shapesStore, {
      strict: this.config.strict,
      includeDetails: true
    });
  }

  /**
   * Validate delta changes only
   * @private
   */
  async _validateDeltaOnly(delta) {
    // Create temporary store with delta changes
    const tempStore = new Store();

    for (const quad of delta.additions || []) {
      tempStore.addQuad(quad);
    }

    // Validate additions
    return validateShacl(tempStore, this.shapesStore, {
      strict: this.config.strict,
      includeDetails: true
    });
  }

  /**
   * Hash a delta for caching
   * @private
   */
  _hashDelta(delta) {
    const additions = (delta.additions || []).map(q =>
      `${q.subject.value}|${q.predicate.value}|${q.object.value}`
    ).sort().join(',');

    const removals = (delta.removals || []).map(q =>
      `${q.subject.value}|${q.predicate.value}|${q.object.value}`
    ).sort().join(',');

    return `${additions}::${removals}`;
  }

  /**
   * Add validation result to cache
   * @private
   */
  _addToCache(hash, result) {
    this.validationCache.set(hash, result);

    // Trim cache if it exceeds size
    if (this.validationCache.size > this.config.cacheSize) {
      const firstKey = this.validationCache.keys().next().value;
      this.validationCache.delete(firstKey);
    }
  }

  /**
   * Validate with debouncing
   * @param {Object} delta - Transaction delta
   * @param {Store} [store] - Current store state
   * @returns {Promise<Object>} Validation result
   */
  async validateDebounced(delta, store) {
    return new Promise((resolve, reject) => {
      // Add to pending validations
      this.pendingValidations.push({ delta, store, resolve, reject });

      // Clear existing timer
      if (this.debounceTimer) {
        clearTimeout(this.debounceTimer);
      }

      // Set new timer
      this.debounceTimer = setTimeout(async () => {
        await this._processPendingValidations();
      }, this.config.debounceMs);
    });
  }

  /**
   * Process pending validations
   * @private
   */
  async _processPendingValidations() {
    if (this.isValidating || this.pendingValidations.length === 0) {
      return;
    }

    this.isValidating = true;

    const pending = [...this.pendingValidations];
    this.pendingValidations = [];

    try {
      // Process all pending validations
      for (const { delta, store, resolve, reject } of pending) {
        try {
          const result = await this.validateDelta(delta, store);
          resolve(result);
        } catch (error) {
          reject(error);
        }
      }
    } finally {
      this.isValidating = false;
    }
  }

  /**
   * Create a validation hook for transactions
   * @param {Object} [options] - Hook options
   * @returns {Object} Transaction hook
   */
  createValidationHook(options = {}) {
    return {
      id: 'real-time-validator',
      mode: 'pre',
      condition: async (store, delta) => {
        try {
          const result = await this.validateDelta(delta, store);
          return result.conforms;
        } catch (error) {
          console.error('[RealTimeValidator] Validation hook failed:', error.message);
          if (this.config.strict || options.strict) {
            throw error;
          }
          return true;
        }
      },
      effect: 'veto'
    };
  }

  /**
   * Clear validation cache
   */
  clearCache() {
    this.validationCache.clear();
  }

  /**
   * Get performance metrics
   * @returns {Object} Metrics
   */
  getMetrics() {
    const latencies = this.metrics.validationLatency.slice(-100);
    const avgLatency = latencies.length > 0
      ? latencies.reduce((sum, l) => sum + l, 0) / latencies.length
      : 0;

    const p95Latency = latencies.length > 0
      ? latencies.sort((a, b) => a - b)[Math.floor(latencies.length * 0.95)]
      : 0;

    return {
      ...this.metrics,
      cacheSize: this.validationCache.size,
      avgLatency,
      p95Latency,
      cacheHitRate: this.metrics.cacheHits > 0
        ? this.metrics.cacheHits / (this.metrics.cacheHits + this.metrics.cacheMisses)
        : 0
    };
  }

  /**
   * Cleanup resources
   * @returns {Promise<void>}
   */
  async cleanup() {
    if (this.debounceTimer) {
      clearTimeout(this.debounceTimer);
    }

    this.clearCache();
    this.removeAllListeners();
    await this.observability.shutdown();
  }
}

/**
 * Create a real-time validator instance
 * @param {Object} config - Validator configuration
 * @returns {RealTimeValidator} Real-time validator
 */
export function createRealTimeValidator(config = {}) {
  return new RealTimeValidator(config);
}
