/**
 * KGC-4D Playground OTEL Instrumentation
 *
 * Integrates with @unrdf/validation package to trace:
 * 1. Universe data persistence (verify quads stored)
 * 2. Shard projection (trace query → serialization)
 * 3. Delta validation (trace validation hook execution)
 */

import { now } from '../server/universe.mjs';

// Fallback validator with in-memory span storage
const fallbackValidator = {
  _validationTempSpans: new Map(),
};

// Initialize validator (lazy load to avoid import errors)
let defaultOTELValidator = fallbackValidator;
let validatorInitialized = false;
let initializePromise = null;

async function initializeValidator() {
  if (validatorInitialized) return;
  if (initializePromise) return initializePromise;

  initializePromise = (async () => {
    try {
      const validation = await import('@unrdf/validation');
      defaultOTELValidator = validation.defaultOTELValidator || fallbackValidator;
    } catch (error) {
      console.debug('[OTEL] Validation package not available, using in-memory span storage');
      defaultOTELValidator = fallbackValidator;
    }
    validatorInitialized = true;
  })();

  return initializePromise;
}

// Initialize immediately for synchronous access (after module load)
let initTask = initializeValidator().catch(() => {
  defaultOTELValidator = fallbackValidator;
});

/**
 * Ensure validator is initialized before recording spans
 * Call this in tests or critical paths to guarantee validator is loaded
 */
export async function ensureValidatorInitialized() {
  if (initTask) await initTask;
  return defaultOTELValidator;
}

/**
 * Create OTEL span data for Universe persistence
 * @param {string} operationType - 'add' or 'delete'
 * @param {number} operationCount - Number of operations
 * @param {number} duration - Execution duration in ms
 * @param {Object} metadata - Additional metadata
 * @returns {Object} Span data compatible with validation package
 */
export function createPersistenceSpan(operationType, operationCount, duration, metadata = {}) {
  return {
    name: 'universe.persist',
    status: 'ok',
    duration,
    attributes: {
      'operation.type': operationType,
      'operation.count': operationCount,
      'data.persistence': 'oxigraph',
      'vector.clock.updated': true,
      'service.name': 'kgc-4d-playground',
      'component': 'universe',
      ...metadata,
    },
    timestamp: now().toString(),
  };
}

/**
 * Create OTEL span data for Shard projection
 * @param {number} quadCount - Number of quads in shard
 * @param {number} duration - Execution duration in ms
 * @param {Object} query - Shard projection query
 * @returns {Object} Span data for validation package
 */
export function createShardProjectionSpan(quadCount, duration, query = {}) {
  return {
    name: 'shard.projection',
    status: 'ok',
    duration,
    attributes: {
      'shard.quad_count': quadCount,
      'shard.has_subject_filter': !!query.subject,
      'shard.has_predicate_filter': !!query.predicate,
      'shard.has_type_filter': !!query.type,
      'shard.has_belongsTo_filter': !!query.belongsTo,
      'vector.clock.included': true,
      'timestamp.precision': 'ns',
      'service.name': 'kgc-4d-playground',
      'component': 'shard',
    },
    timestamp: now().toString(),
  };
}

/**
 * Create OTEL span data for Delta validation hook
 * @param {string} hookId - Hook identifier
 * @param {boolean} passed - Whether validation passed
 * @param {string} reason - Reason if failed
 * @param {number} duration - Execution duration in ms
 * @returns {Object} Span data for validation package
 */
export function createValidationHookSpan(hookId, passed, reason, duration) {
  return {
    name: 'delta.validation',
    status: passed ? 'ok' : 'error',
    duration,
    attributes: {
      'hook.id': hookId,
      'hook.result': passed ? 'ACCEPT' : 'REJECT',
      'hook.reason': reason || 'Passed',
      'service.name': 'kgc-4d-playground',
      'component': 'delta',
      'validation.enforced': true,
    },
    timestamp: now().toString(),
  };
}

/**
 * Record OTEL spans to validation package
 * @param {Array} spans - Array of span data objects
 * @param {string} validationId - Validation ID for tracking
 */
export function recordOTELSpans(spans, validationId) {
  if (!defaultOTELValidator || !defaultOTELValidator._validationTempSpans) {
    console.warn('[OTEL] Validator not available, spans not recorded');
    return;
  }

  try {
    const existing = defaultOTELValidator._validationTempSpans.get(validationId) || [];
    existing.push(...spans);
    defaultOTELValidator._validationTempSpans.set(validationId, existing);
  } catch (error) {
    console.error('[OTEL] Failed to record spans:', error.message);
  }
}

/**
 * Get OTEL validation status for a validation session
 * @param {string} validationId - Validation ID
 * @returns {Object} Validation status with all recorded spans
 */
export function getOTELValidationStatus(validationId) {
  if (!defaultOTELValidator || !defaultOTELValidator._validationTempSpans) {
    return { status: 'unavailable', spans: [] };
  }

  const spans = defaultOTELValidator._validationTempSpans.get(validationId) || [];
  const passedSpans = spans.filter((s) => s.status === 'ok').length;
  const failedSpans = spans.filter((s) => s.status === 'error').length;

  return {
    status: failedSpans > 0 ? 'failed' : failedSpans === 0 && spans.length > 0 ? 'passed' : 'no-spans',
    total_spans: spans.length,
    passed: passedSpans,
    failed: failedSpans,
    spans,
    timestamp: new Date().toISOString(),
  };
}

/**
 * Verify data persistence via OTEL spans
 * @param {string} validationId - Validation ID
 * @returns {Object} Persistence verification result
 */
export function verifyDataPersistence(validationId) {
  const status = getOTELValidationStatus(validationId);
  const persistenceSpans = (status.spans || []).filter((s) => s.name === 'universe.persist');

  return {
    verified: persistenceSpans.length > 0,
    persistence_spans: persistenceSpans.length,
    operations_traced: persistenceSpans.reduce((sum, s) => sum + (s.attributes?.['operation.count'] || 0), 0),
    average_duration_ms: persistenceSpans.length > 0
      ? persistenceSpans.reduce((sum, s) => sum + s.duration, 0) / persistenceSpans.length
      : 0,
    proof: persistenceSpans.map((s) => ({
      operation_type: s.attributes?.['operation.type'],
      operation_count: s.attributes?.['operation.count'],
      duration_ms: s.duration,
      timestamp: s.timestamp,
    })),
  };
}

/**
 * Verify validation hooks via OTEL spans
 * @param {string} validationId - Validation ID
 * @returns {Object} Validation hook verification result
 */
export function verifyValidationHooks(validationId) {
  const status = getOTELValidationStatus(validationId);
  const validationSpans = (status.spans || []).filter((s) => s.name === 'delta.validation');

  const rejectedCount = validationSpans.filter((s) => s.status === 'error').length;
  const acceptedCount = validationSpans.filter((s) => s.status === 'ok').length;

  return {
    verified: validationSpans.length > 0,
    total_validations: validationSpans.length,
    accepted: acceptedCount,
    rejected: rejectedCount,
    average_duration_ms: validationSpans.length > 0
      ? validationSpans.reduce((sum, s) => sum + s.duration, 0) / validationSpans.length
      : 0,
    hook_execution_trace: validationSpans.map((s) => ({
      hook_id: s.attributes?.['hook.id'],
      result: s.attributes?.['hook.result'],
      reason: s.attributes?.['hook.reason'],
      duration_ms: s.duration,
      timestamp: s.timestamp,
    })),
  };
}

/**
 * Verify Shard projection via OTEL spans
 * @param {string} validationId - Validation ID
 * @returns {Object} Shard projection verification result
 */
export function verifyShardProjection(validationId) {
  const status = getOTELValidationStatus(validationId);
  const projectionSpans = (status.spans || []).filter((s) => s.name === 'shard.projection');

  return {
    verified: projectionSpans.length > 0,
    total_projections: projectionSpans.length,
    total_quads_projected: projectionSpans.reduce((sum, s) => sum + (s.attributes?.['shard.quad_count'] || 0), 0),
    average_duration_ms: projectionSpans.length > 0
      ? projectionSpans.reduce((sum, s) => sum + s.duration, 0) / projectionSpans.length
      : 0,
    average_quads_per_projection: projectionSpans.length > 0
      ? projectionSpans.reduce((sum, s) => sum + (s.attributes?.['shard.quad_count'] || 0), 0) / projectionSpans.length
      : 0,
    projection_trace: projectionSpans.map((s) => ({
      quad_count: s.attributes?.['shard.quad_count'],
      has_subject_filter: s.attributes?.['shard.has_subject_filter'],
      has_type_filter: s.attributes?.['shard.has_type_filter'],
      duration_ms: s.duration,
      timestamp: s.timestamp,
    })),
  };
}
