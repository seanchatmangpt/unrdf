/**
 * @file Deterministic Replay - Rebuild workflow state from receipts
 * @module @unrdf/yawl-durable/replay
 *
 * Core innovation: YAWL receipts form an immutable event history.
 * We can replay this history to reconstruct exact workflow state
 * without re-executing activities.
 *
 * This is the foundation of durable execution - workflows can be
 * paused, restarted, or moved between machines while maintaining
 * exactly-once semantics.
 */

import { blake3 } from 'hash-wasm';

// =============================================================================
// Receipt Verification
// =============================================================================

/**
 * Verify receipt chain integrity
 *
 * @param {Array} receipts - Receipt chain
 * @returns {Promise<Object>} Verification result
 */
export async function verifyReceiptChain(receipts) {
  if (!receipts || receipts.length === 0) {
    return { valid: false, error: 'No receipts provided' };
  }

  // Verify first receipt is genesis (no previous)
  if (receipts[0].previousReceiptHash !== null) {
    return {
      valid: false,
      error: 'First receipt must have null previousReceiptHash (genesis)',
    };
  }

  // Verify chain integrity
  for (let i = 1; i < receipts.length; i++) {
    const prev = receipts[i - 1];
    const curr = receipts[i];

    if (curr.previousReceiptHash !== prev.receiptHash) {
      return {
        valid: false,
        error: `Chain broken at receipt ${i}: expected ${prev.receiptHash}, got ${curr.previousReceiptHash}`,
        receiptIndex: i,
      };
    }
  }

  return {
    valid: true,
    receiptCount: receipts.length,
    genesisHash: receipts[0].receiptHash,
    latestHash: receipts[receipts.length - 1].receiptHash,
  };
}

// =============================================================================
// State Reconstruction
// =============================================================================

/**
 * Replay receipts to reconstruct workflow state
 *
 * This is deterministic - given the same receipt chain, we always
 * reconstruct the exact same state. This enables:
 * - Time-travel debugging (replay to any point)
 * - Crash recovery (rebuild state after restart)
 * - Workflow migration (move execution between machines)
 * - Audit trails (prove exact execution history)
 *
 * @param {Array} receipts - Receipt chain
 * @returns {Promise<Object>} Reconstructed workflow state
 *
 * @example
 * const state = await replayFromReceipts(receipts);
 * console.log(state.events); // All workflow events
 * console.log(state.completedTasks); // Completed activities
 * console.log(state.data); // Current workflow data
 */
export async function replayFromReceipts(receipts) {
  // Verify chain first
  const verification = await verifyReceiptChain(receipts);
  if (!verification.valid) {
    throw new Error(`Invalid receipt chain: ${verification.error}`);
  }

  const state = {
    caseId: receipts[0].caseId,
    events: [],
    completedTasks: new Set(),
    activeTasks: new Set(),
    cancelledTasks: new Set(),
    failedTasks: new Set(),
    data: {},
    timeline: [],
  };

  // Replay each receipt
  for (const receipt of receipts) {
    const event = {
      receiptId: receipt.id,
      eventType: receipt.eventType,
      taskId: receipt.taskId,
      workItemId: receipt.workItemId,
      timestamp: receipt.timestamp_iso,
      payload: receipt.payload,
    };

    state.events.push(event);

    // Update state based on event type
    switch (receipt.eventType) {
      case 'CASE_CREATED':
        state.timeline.push({ type: 'CREATED', timestamp: receipt.timestamp_iso });
        break;

      case 'TASK_ENABLED':
        state.activeTasks.add(receipt.taskId);
        state.timeline.push({
          type: 'TASK_ENABLED',
          taskId: receipt.taskId,
          timestamp: receipt.timestamp_iso,
        });
        break;

      case 'TASK_STARTED':
        state.timeline.push({
          type: 'TASK_STARTED',
          taskId: receipt.taskId,
          timestamp: receipt.timestamp_iso,
        });
        break;

      case 'TASK_COMPLETED':
        state.completedTasks.add(receipt.taskId);
        state.activeTasks.delete(receipt.taskId);

        // Merge task output into workflow data
        if (receipt.payload.context?.output) {
          state.data = { ...state.data, ...receipt.payload.context.output };
        }

        state.timeline.push({
          type: 'TASK_COMPLETED',
          taskId: receipt.taskId,
          timestamp: receipt.timestamp_iso,
          output: receipt.payload.context?.output,
        });
        break;

      case 'TASK_CANCELLED':
        state.cancelledTasks.add(receipt.taskId);
        state.activeTasks.delete(receipt.taskId);
        state.timeline.push({
          type: 'TASK_CANCELLED',
          taskId: receipt.taskId,
          timestamp: receipt.timestamp_iso,
        });
        break;

      case 'TASK_FAILED':
        state.failedTasks.add(receipt.taskId);
        state.activeTasks.delete(receipt.taskId);
        state.timeline.push({
          type: 'TASK_FAILED',
          taskId: receipt.taskId,
          timestamp: receipt.timestamp_iso,
        });
        break;

      case 'TASK_TIMEOUT':
        state.failedTasks.add(receipt.taskId);
        state.activeTasks.delete(receipt.taskId);
        state.timeline.push({
          type: 'TASK_TIMEOUT',
          taskId: receipt.taskId,
          timestamp: receipt.timestamp_iso,
        });
        break;
    }
  }

  return {
    ...state,
    completedTasks: Array.from(state.completedTasks),
    activeTasks: Array.from(state.activeTasks),
    cancelledTasks: Array.from(state.cancelledTasks),
    failedTasks: Array.from(state.failedTasks),
    receiptCount: receipts.length,
    genesisHash: receipts[0].receiptHash,
    latestHash: receipts[receipts.length - 1].receiptHash,
  };
}

/**
 * Replay receipts up to a specific point in time
 *
 * This enables time-travel debugging - reconstruct workflow state
 * as it existed at any point in execution history.
 *
 * @param {Array} receipts - Full receipt chain
 * @param {bigint|string} targetTimestamp - Target timestamp (nanoseconds or ISO string)
 * @returns {Promise<Object>} State at target time
 */
export async function replayToTimestamp(receipts, targetTimestamp) {
  const target = typeof targetTimestamp === 'string'
    ? BigInt(new Date(targetTimestamp).getTime()) * 1000000n
    : targetTimestamp;

  // Filter receipts up to target time
  const filteredReceipts = receipts.filter(r => r.t_ns <= target);

  if (filteredReceipts.length === 0) {
    throw new Error(`No receipts found before timestamp ${targetTimestamp}`);
  }

  const state = await replayFromReceipts(filteredReceipts);

  return {
    ...state,
    targetTimestamp: targetTimestamp.toString(),
    receiptCount: filteredReceipts.length,
    totalReceipts: receipts.length,
  };
}

/**
 * Compute state hash at any point in receipt chain
 *
 * @param {Array} receipts - Receipts up to target point
 * @returns {Promise<string>} State hash
 */
export async function computeStateHash(receipts) {
  const state = await replayFromReceipts(receipts);

  const stateData = {
    completedTasks: state.completedTasks.sort(),
    activeTasks: state.activeTasks.sort(),
    data: state.data,
    receiptCount: state.receiptCount,
    latestHash: state.latestHash,
  };

  return blake3(JSON.stringify(stateData));
}
