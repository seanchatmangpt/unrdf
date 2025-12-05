/**
 * KGC-4D Delta Instrumented for OTEL
 *
 * Wraps delta.mjs functions with OTEL span recording for validation hook tracing
 */

import { submitDelta as submitDeltaBase } from '../server/delta.mjs';
import { createValidationHookSpan, recordOTELSpans } from './instrumentation.mjs';

let instrumentationId = null;

/**
 * Set the validation ID for span recording
 * @param {string} id - Validation ID
 */
export function setInstrumentationId(id) {
  instrumentationId = id;
}

/**
 * Validation hook configurations with OTEL span factories
 */
const INSTRUMENTED_HOOKS = {
  'validate-budget': {
    name: 'Budget Validator',
    predicate: 'http://kgc.io/ontology/budget',
    validate: (value) => {
      const budget = parseInt(value, 10);
      if (isNaN(budget)) return { valid: false, reason: 'Budget must be a number' };
      if (budget < 0) return { valid: false, reason: 'Budget cannot be negative' };
      if (budget > 100000) return { valid: false, reason: 'Budget cannot exceed $100,000' };
      return { valid: true };
    },
  },
  'validate-status': {
    name: 'Status Validator',
    predicate: 'http://kgc.io/ontology/status',
    validate: (value) => {
      const allowed = ['active', 'paused', 'completed', 'cancelled'];
      if (!allowed.includes(value)) {
        return { valid: false, reason: `Status must be one of: ${allowed.join(', ')}` };
      }
      return { valid: true };
    },
  },
  'validate-name': {
    name: 'Name Validator',
    predicate: 'http://kgc.io/ontology/name',
    validate: (value) => {
      if (!value || value.trim().length === 0) {
        return { valid: false, reason: 'Name cannot be empty' };
      }
      if (value.length > 100) {
        return { valid: false, reason: 'Name cannot exceed 100 characters' };
      }
      return { valid: true };
    },
  },
  'validate-title': {
    name: 'Title Validator',
    predicate: 'http://kgc.io/ontology/title',
    validate: (value) => {
      if (!value || value.trim().length === 0) {
        return { valid: false, reason: 'Title cannot be empty' };
      }
      return { valid: true };
    },
  },
};

/**
 * Run validation hooks with OTEL span recording
 * @param {Object} operation - Delta operation
 * @returns {Object} Validation result with OTEL spans
 */
export function runValidationHooksInstrumented(operation) {
  const predicate = operation.predicate.value || operation.predicate;
  const spans = [];

  // Check each hook
  for (const [hookId, hookConfig] of Object.entries(INSTRUMENTED_HOOKS)) {
    if (hookConfig.predicate === predicate) {
      const startTime = Date.now();
      const result = hookConfig.validate(operation.object.value);
      const duration = Date.now() - startTime;

      // Create OTEL validation span
      const span = createValidationHookSpan(
        hookId,
        result.valid,
        result.reason || 'Passed',
        duration
      );

      spans.push(span);

      if (!result.valid) {
        // Record spans if instrumentation active
        if (instrumentationId) {
          recordOTELSpans(spans, instrumentationId);
        }
        return { valid: false, reason: result.reason, spans };
      }
    }
  }

  // Record all acceptance spans
  if (instrumentationId) {
    recordOTELSpans(spans, instrumentationId);
  }

  return { valid: true, spans };
}

/**
 * Wrapped submitDelta with comprehensive OTEL instrumentation
 * @param {Object} delta - Delta to submit
 * @returns {Promise<Object>} ACK or REJECT with OTEL trace
 */
export async function submitDeltaInstrumented(delta) {
  const startTime = Date.now();

  // Run validation with instrumentation
  const operations = delta.operations || [];
  for (const op of operations) {
    const validation = runValidationHooksInstrumented(op);
    if (!validation.valid) {
      const duration = Date.now() - startTime;

      return {
        status: 'REJECT',
        reason: validation.reason,
        operation: op,
        duration_ms: duration,
        t_ns: Date.now().toString(),
        otel_spans: validation.spans,
        instrumentation_id: instrumentationId,
      };
    }
  }

  // Call original submitDelta for persistence
  const result = await submitDeltaBase(delta);

  const duration = Date.now() - startTime;

  return {
    ...result,
    duration_ms: duration,
    instrumentation_id: instrumentationId,
    validation_trace: {
      operations_count: operations.length,
      all_validations_passed: true,
      average_validation_ms: duration / Math.max(operations.length, 1),
    },
  };
}

/**
 * Get validation hook configuration for inspection
 * @returns {Object} Hook configurations
 */
export function getValidationHookConfig() {
  return INSTRUMENTED_HOOKS;
}
