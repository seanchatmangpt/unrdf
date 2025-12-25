/**
 * @file YAWL Engine Hooks - Policy pack integration and KGC-4D logging
 * @module @unrdf/yawl/engine-hooks
 *
 * @description
 * Hook and policy functionality:
 * - Policy pack registration and execution
 * - KGC-4D event logging
 * - Circuit breaker state management
 * - Pre/post task hooks
 */

import { now, toISO } from '@unrdf/kgc-4d';
import {
  createWorkflowReceipt,
  appendWorkflowEvent,
} from './events/yawl-events.mjs';
import { ENGINE_EVENTS } from './engine-constants.mjs';

// =============================================================================
// Policy Pack Integration
// =============================================================================

/**
 * Register a policy pack for a workflow
 * @param {Object} engine - Engine instance
 * @param {string} workflowId - Workflow ID
 * @param {Object} policyPack - Policy pack from createYAWLPolicyPack
 * @returns {void}
 */
export function registerPolicyPack(engine, workflowId, policyPack) {
  if (!engine.workflows.has(workflowId)) {
    throw new Error(`Workflow ${workflowId} not found`);
  }
  engine._policyPacks.set(workflowId, policyPack);
}

/**
 * Get policy pack for a workflow
 * @param {Object} engine - Engine instance
 * @param {string} workflowId - Workflow ID
 * @returns {Object|undefined} Policy pack if registered
 */
export function getPolicyPack(engine, workflowId) {
  return engine._policyPacks.get(workflowId);
}

// =============================================================================
// KGC-4D Integration
// =============================================================================

/**
 * Log a case event to KGC-4D
 * @param {Object} engine - Engine instance
 * @param {string} eventType - Event type
 * @param {Object} payload - Event payload
 */
export async function logCaseEvent(engine, eventType, payload) {
  try {
    const receipt = await createWorkflowReceipt({
      beforeState: { empty: true },
      afterState: payload,
      decision: { action: eventType, ...payload },
      justification: { reasoning: `Case event: ${eventType}` },
    });

    await appendWorkflowEvent(engine.store, eventType, {
      ...payload,
      timestamp: toISO(now()),
      receipt,
    });
  } catch (error) {
    console.error(`Failed to log case event ${eventType}:`, error);
  }
}

/**
 * Log a task event to KGC-4D
 * @param {Object} engine - Engine instance
 * @param {string} eventType - Event type
 * @param {Object} payload - Event payload
 * @param {string} [caseId] - Case ID for context
 */
export async function logTaskEvent(engine, eventType, payload, caseId) {
  try {
    const receipt = await createWorkflowReceipt({
      beforeState: payload.beforeState || { workItemId: payload.workItemId },
      afterState: payload,
      decision: { action: eventType, ...payload },
      justification: { reasoning: `Task event: ${eventType}` },
    });

    await appendWorkflowEvent(
      engine.store,
      eventType,
      {
        ...payload,
        receipt,
      },
      { caseId }
    );
  } catch (error) {
    console.error(`Failed to log task event ${eventType}:`, error);
  }
}

// =============================================================================
// Circuit Breaker
// =============================================================================

/**
 * Check if circuit breaker is open
 * @param {Object} engine - Engine instance
 * @param {string} key - Circuit breaker key
 * @returns {boolean}
 */
export function isCircuitOpen(engine, key) {
  const breaker = engine._circuitBreakers.get(key);
  if (!breaker) return false;

  if (breaker.state === 'open') {
    // Check if reset timeout has passed
    const elapsed = Number(now() - breaker.openedAt) / 1_000_000;
    if (elapsed >= engine.circuitBreakerResetTimeout) {
      breaker.state = 'half-open';
      return false;
    }
    return true;
  }

  return false;
}

/**
 * Record a circuit breaker failure
 * @param {Object} engine - Engine instance
 * @param {string} key - Circuit breaker key
 */
export function recordCircuitFailure(engine, key) {
  let breaker = engine._circuitBreakers.get(key);
  if (!breaker) {
    breaker = { failures: 0, state: 'closed', openedAt: null };
    engine._circuitBreakers.set(key, breaker);
  }

  breaker.failures++;

  if (breaker.failures >= engine.circuitBreakerThreshold) {
    breaker.state = 'open';
    breaker.openedAt = now();
    engine._stats.circuitBreakerTrips++;

    engine.emit(ENGINE_EVENTS.CIRCUIT_BREAKER_OPEN, {
      key,
      failures: breaker.failures,
    });
  }
}

/**
 * Reset circuit breaker on success
 * @param {Object} engine - Engine instance
 * @param {string} key - Circuit breaker key
 */
export function resetCircuitBreaker(engine, key) {
  const breaker = engine._circuitBreakers.get(key);
  if (breaker) {
    const wasOpen = breaker.state !== 'closed';
    breaker.failures = 0;
    breaker.state = 'closed';
    breaker.openedAt = null;

    if (wasOpen) {
      engine.emit(ENGINE_EVENTS.CIRCUIT_BREAKER_CLOSE, { key });
    }
  }
}
