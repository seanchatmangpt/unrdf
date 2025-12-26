/**
 * YAWL Receipt Generation & Workflow Functions
 * Cryptographic receipts and high-level workflow operations
 *
 * @module yawl-events-receipts
 * @description
 * Receipt generation with BLAKE3 hashing and high-level workflow functions
 * for creating cases, enabling tasks, and managing work items.
 */

import { blake3 } from 'hash-wasm';
import { now, toISO } from '@unrdf/kgc-4d';
import {
  ReceiptSchema,
  generateUUID,
  serializeCaseState,
  appendWorkflowEvent,
} from './yawl-events-core.mjs';

// ============================================================================
// Receipt Generation
// ============================================================================

/**
 * Create a workflow receipt with cryptographic proofs
 *
 * Generates:
 * - Before state hash (from parent task completion)
 * - After state hash (after task enabled/completed)
 * - BLAKE3 hash of decision (task enabled or not)
 * - Justification: which hook validated, what condition checked
 * - Git ref from snapshot
 *
 * @param {Object} options - Receipt options
 * @param {Object} options.beforeState - State before the transition
 * @param {Object} options.afterState - State after the transition
 * @param {Object} options.decision - Decision data (event payload)
 * @param {Object} [options.justification] - Justification details
 * @param {string} [options.gitRef] - Git reference from snapshot
 * @returns {Promise<Object>} Receipt with all cryptographic proofs
 *
 * @example
 * const receipt = await createWorkflowReceipt({
 *   beforeState: { workItems: [] },
 *   afterState: { workItems: [{ id: 'w1', state: 'enabled' }] },
 *   decision: { taskEnabled: true, taskId: 'Approval' },
 *   justification: { hookValidated: 'pre-enable-hook', conditionChecked: 'has_manager' },
 * });
 */
export async function createWorkflowReceipt(options) {
  const {
    beforeState,
    afterState,
    decision,
    justification = {},
    gitRef = null,
  } = options;

  // Serialize states deterministically
  const beforeSerialized = serializeCaseState(beforeState);
  const afterSerialized = serializeCaseState(afterState);
  const decisionSerialized = serializeCaseState(decision);

  // Calculate BLAKE3 hashes
  const [beforeHash, afterHash, decisionHash] = await Promise.all([
    blake3(beforeSerialized),
    blake3(afterSerialized),
    blake3(decisionSerialized),
  ]);

  // Get current timestamp
  const t_ns = now();

  // Build receipt
  const receipt = {
    beforeHash,
    afterHash,
    hash: decisionHash,
    justification: {
      hookValidated: justification.hookValidated || null,
      conditionChecked: justification.conditionChecked || null,
      sparqlQuery: justification.sparqlQuery || null,
      reasoning: justification.reasoning || null,
    },
    gitRef,
    t_ns: t_ns.toString(),
    timestamp_iso: toISO(t_ns),
  };

  // Validate receipt against schema
  const validated = ReceiptSchema.parse(receipt);

  return validated;
}

/**
 * Verify a workflow receipt by checking hash chain
 *
 * @param {Object} receipt - Receipt to verify
 * @param {Object} beforeState - State that should produce beforeHash
 * @param {Object} afterState - State that should produce afterHash
 * @param {Object} decision - Decision that should produce hash
 * @returns {Promise<Object>} Verification result
 */
export async function verifyWorkflowReceipt(receipt, beforeState, afterState, decision) {
  try {
    // Recalculate hashes
    const beforeSerialized = serializeCaseState(beforeState);
    const afterSerialized = serializeCaseState(afterState);
    const decisionSerialized = serializeCaseState(decision);

    const [beforeHash, afterHash, decisionHash] = await Promise.all([
      blake3(beforeSerialized),
      blake3(afterSerialized),
      blake3(decisionSerialized),
    ]);

    // Verify all hashes match
    const valid =
      beforeHash === receipt.beforeHash &&
      afterHash === receipt.afterHash &&
      decisionHash === receipt.hash;

    return {
      valid,
      verified: {
        beforeHash: beforeHash === receipt.beforeHash,
        afterHash: afterHash === receipt.afterHash,
        decisionHash: decisionHash === receipt.hash,
      },
      receipt,
    };
  } catch (error) {
    return {
      valid: false,
      error: error.message,
      receipt,
    };
  }
}

// ============================================================================
// High-Level Workflow Functions
// ============================================================================

/**
 * Create a new workflow case
 *
 * @param {import('@unrdf/kgc-4d').KGCStore} store - KGC-4D store instance
 * @param {string} specId - Workflow specification ID
 * @param {Object} [options={}] - Additional options
 * @returns {Promise<Object>} Created case with ID and receipt
 *
 * @example
 * const case1 = await createCase(store, 'approval-workflow');
 * console.log(case1.caseId);
 * console.log(case1.receipt);
 */
export async function createCase(store, specId, options = {}) {
  const caseId = generateUUID();
  const timestamp = toISO(now());

  // Create receipt for case creation
  const receipt = await createWorkflowReceipt({
    beforeState: { empty: true },
    afterState: { caseId, specId, state: 'active' },
    decision: { action: 'create_case', specId },
    justification: {
      reasoning: 'New workflow case instantiated',
    },
    gitRef: options.gitRef,
  });

  // Append event
  const eventReceipt = await appendWorkflowEvent(store, 'CASE_CREATED', {
    caseId,
    specId,
    timestamp,
    receipt,
  }, options);

  return {
    caseId,
    specId,
    state: 'active',
    createdAt: timestamp,
    receipt,
    eventReceipt,
  };
}

/**
 * Enable a task for a case (creates work item)
 *
 * @param {import('@unrdf/kgc-4d').KGCStore} store - KGC-4D store instance
 * @param {string} caseId - Case UUID
 * @param {string} taskId - Task identifier
 * @param {Object} [options={}] - Additional options
 * @returns {Promise<Object>} Work item with ID and receipt
 */
export async function enableTask(store, caseId, taskId, options = {}) {
  const workItemId = generateUUID();
  const enabledAt = toISO(now());

  // Create receipt
  const receipt = await createWorkflowReceipt({
    beforeState: options.beforeState || { caseId },
    afterState: { caseId, workItemId, taskId, state: 'enabled' },
    decision: { action: 'enable_task', taskId, caseId },
    justification: options.justification || {
      reasoning: `Task ${taskId} enabled`,
    },
    gitRef: options.gitRef,
  });

  // Append event
  const eventReceipt = await appendWorkflowEvent(store, 'TASK_ENABLED', {
    taskId,
    caseId,
    workItemId,
    enabledAt,
    receipt,
  }, options);

  return {
    workItemId,
    taskId,
    caseId,
    state: 'enabled',
    enabledAt,
    receipt,
    eventReceipt,
  };
}

/**
 * Start a work item (begin task execution)
 *
 * @param {import('@unrdf/kgc-4d').KGCStore} store - KGC-4D store instance
 * @param {string} workItemId - Work item UUID
 * @param {string} caseId - Case UUID (for event context)
 * @param {Object} [options={}] - Additional options
 * @returns {Promise<Object>} Updated work item with receipt
 */
export async function startWorkItem(store, workItemId, caseId, options = {}) {
  const startedAt = toISO(now());

  // Create receipt
  const receipt = await createWorkflowReceipt({
    beforeState: options.beforeState || { workItemId, state: 'enabled' },
    afterState: { workItemId, state: 'started' },
    decision: { action: 'start_work_item', workItemId },
    justification: options.justification || {
      reasoning: `Work item ${workItemId} started`,
    },
    gitRef: options.gitRef,
  });

  // Append event
  const eventReceipt = await appendWorkflowEvent(store, 'TASK_STARTED', {
    workItemId,
    startedAt,
    receipt,
  }, { ...options, caseId });

  return {
    workItemId,
    state: 'started',
    startedAt,
    receipt,
    eventReceipt,
  };
}

/**
 * Complete a work item with result
 *
 * @param {import('@unrdf/kgc-4d').KGCStore} store - KGC-4D store instance
 * @param {string} workItemId - Work item UUID
 * @param {string} caseId - Case UUID (for event context)
 * @param {*} result - Task result data
 * @param {Object} [options={}] - Additional options
 * @returns {Promise<Object>} Completed work item with receipt
 */
export async function completeWorkItem(store, workItemId, caseId, result, options = {}) {
  const completedAt = toISO(now());

  // Create receipt
  const receipt = await createWorkflowReceipt({
    beforeState: options.beforeState || { workItemId, state: 'started' },
    afterState: { workItemId, state: 'completed', result },
    decision: { action: 'complete_work_item', workItemId, result },
    justification: options.justification || {
      reasoning: `Work item ${workItemId} completed`,
    },
    gitRef: options.gitRef,
  });

  // Append event
  const eventReceipt = await appendWorkflowEvent(store, 'TASK_COMPLETED', {
    workItemId,
    completedAt,
    result,
    receipt,
  }, { ...options, caseId });

  return {
    workItemId,
    state: 'completed',
    completedAt,
    result,
    receipt,
    eventReceipt,
  };
}

/**
 * Record a control flow evaluation
 *
 * @param {import('@unrdf/kgc-4d').KGCStore} store - KGC-4D store instance
 * @param {string} caseId - Case UUID
 * @param {string} taskId - Task being evaluated
 * @param {boolean} result - Evaluation result
 * @param {string} sparqlQuery - SPARQL query used for evaluation
 * @param {Object} [options={}] - Additional options
 * @returns {Promise<Object>} Control flow decision with receipt
 */
export async function recordControlFlowEvaluation(store, caseId, taskId, result, sparqlQuery, options = {}) {
  const timestamp = toISO(now());

  // Create receipt
  const receipt = await createWorkflowReceipt({
    beforeState: options.beforeState || { caseId, taskId },
    afterState: { caseId, taskId, evaluated: true, result },
    decision: { action: 'evaluate_control_flow', taskId, result, sparqlQuery },
    justification: {
      sparqlQuery,
      conditionChecked: options.conditionChecked || taskId,
      reasoning: `Control flow for ${taskId}: ${result ? 'enabled' : 'blocked'}`,
    },
    gitRef: options.gitRef,
  });

  // Append event
  const eventReceipt = await appendWorkflowEvent(store, 'CONTROL_FLOW_EVALUATED', {
    caseId,
    taskId,
    result,
    timestamp,
    sparqlQuery,
    receipt,
  }, options);

  return {
    caseId,
    taskId,
    result,
    timestamp,
    sparqlQuery,
    receipt,
    eventReceipt,
  };
}
