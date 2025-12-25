/**
 * @file YAWL Engine Coordination - Events, time-travel, and resources
 * @module @unrdf/yawl/engine-coordination
 *
 * @description
 * Coordination functionality:
 * - Event subscription and emission
 * - Time-travel and checkpoints
 * - Resource management
 * - Snapshot automation
 */

import { freezeUniverse, now, toISO } from '@unrdf/kgc-4d';
import {
  getWorkflowAuditTrail,
  reconstructCase as kgcReconstructCase,
} from './events/yawl-events.mjs';
import { ENGINE_EVENTS } from './engine-constants.mjs';

// =============================================================================
// Event Subscription
// =============================================================================

/**
 * Subscribe to engine events
 * @param {Object} engine - Engine instance
 * @param {string} eventType - Event type from ENGINE_EVENTS
 * @param {Function} handler - Event handler function
 * @returns {Function} Unsubscribe function
 *
 * @example
 * ```javascript
 * const unsubscribe = on(engine, 'task:completed', (event) => {
 *   console.log('Task completed:', event.taskId, event.output);
 * });
 *
 * // Later...
 * unsubscribe();
 * ```
 */
export function on(engine, eventType, handler) {
  if (typeof handler !== 'function') {
    throw new TypeError('Handler must be a function');
  }

  if (!engine._eventHandlers.has(eventType)) {
    engine._eventHandlers.set(eventType, new Set());
  }

  engine._eventHandlers.get(eventType).add(handler);

  // Return unsubscribe function
  return () => {
    const handlers = engine._eventHandlers.get(eventType);
    if (handlers) {
      handlers.delete(handler);
    }
  };
}

/**
 * Emit an event to all subscribers
 * @param {Object} engine - Engine instance
 * @param {string} eventType - Event type
 * @param {Object} data - Event data
 */
export function emit(engine, eventType, data) {
  const handlers = engine._eventHandlers.get(eventType);
  if (!handlers) return;

  const event = {
    type: eventType,
    timestamp: now().toString(),
    timestampISO: toISO(now()),
    ...data,
  };

  for (const handler of handlers) {
    try {
      handler(event);
    } catch (error) {
      // Log error but don't throw to avoid disrupting other handlers
      console.error(`Error in event handler for ${eventType}:`, error);
    }
  }
}

/**
 * Remove all handlers for an event type
 * @param {Object} engine - Engine instance
 * @param {string} eventType - Event type
 */
export function off(engine, eventType) {
  engine._eventHandlers.delete(eventType);
}

// =============================================================================
// Time-Travel
// =============================================================================

/**
 * Create a checkpoint for time-travel
 * @param {Object} engine - Engine instance
 * @param {string} [label] - Optional checkpoint label
 * @returns {Promise<{timestamp: bigint, hash: string}>}
 */
export async function checkpoint(engine, label) {
  if (!engine.git) {
    throw new Error('Git backbone required for checkpoints');
  }

  const freezeResult = await freezeUniverse(engine.store, engine.git);

  // Store checkpoint with case states
  const caseStates = new Map();
  for (const [caseId, yawlCase] of engine.cases) {
    caseStates.set(caseId, yawlCase.toJSON());
  }

  engine.checkpoints.set(BigInt(freezeResult.t_ns), {
    label,
    hash: freezeResult.universe_hash,
    gitRef: freezeResult.git_ref,
    caseStates: Object.fromEntries(caseStates),
    events: [...engine.events],
  });

  engine._stats.checkpointsCreated++;

  engine.emit(ENGINE_EVENTS.CHECKPOINT_CREATED, {
    timestamp: BigInt(freezeResult.t_ns).toString(),
    hash: freezeResult.universe_hash,
    label,
  });

  return {
    timestamp: BigInt(freezeResult.t_ns),
    hash: freezeResult.universe_hash,
  };
}

/**
 * Replay a case to a specific point in time
 * @param {Object} engine - Engine instance
 * @param {string} caseId - Case ID
 * @param {bigint} targetTime - Target timestamp (nanoseconds)
 * @returns {Promise<{state: Object, events: Array, verified: boolean}>}
 */
export async function replayCase(engine, caseId, targetTime) {
  // If Git backbone available, use KGC-4D reconstruction
  if (engine.git) {
    try {
      return await kgcReconstructCase(engine.store, engine.git, caseId, targetTime);
    } catch {
      // Fall back to checkpoint-based reconstruction
    }
  }

  // Find the checkpoint before or at timestamp
  let targetCheckpoint = null;
  let targetCheckpointTime = 0n;

  for (const [checkpointTime, checkpoint] of engine.checkpoints) {
    if (checkpointTime <= targetTime && checkpointTime > targetCheckpointTime) {
      targetCheckpointTime = checkpointTime;
      targetCheckpoint = checkpoint;
    }
  }

  if (!targetCheckpoint) {
    throw new Error(`No checkpoint found before ${targetTime}`);
  }

  // Get case state from checkpoint
  const caseState = targetCheckpoint.caseStates[caseId];
  if (!caseState) {
    throw new Error(`Case ${caseId} not found in checkpoint`);
  }

  // Filter events to only those before timestamp
  const eventsBeforeTimestamp = targetCheckpoint.events.filter(
    e => BigInt(e.timestamp) <= targetTime
  );

  // Verify receipt chain
  const yawlCase = engine.cases.get(caseId);
  let verified = true;

  if (yawlCase && yawlCase.receipts.length > 0) {
    for (let i = 0; i < yawlCase.receipts.length; i++) {
      const receipt = yawlCase.receipts[i];
      const previous = i > 0 ? yawlCase.receipts[i - 1] : null;

      if (BigInt(receipt.timestamp) > targetTime) break;

      const chainResult = await receipt.verifyChain(previous);
      if (!chainResult.valid) {
        verified = false;
        break;
      }
    }
  }

  return {
    state: caseState,
    events: eventsBeforeTimestamp.filter(e => e.caseId === caseId),
    verified,
    reconstructedAt: toISO(targetTime),
  };
}

/**
 * Get complete event history for a case
 * @param {Object} engine - Engine instance
 * @param {string} caseId - Case ID
 * @returns {Promise<Object>} Audit trail with events and receipts
 */
export async function getCaseHistory(engine, caseId) {
  // If KGC-4D event log enabled, get from store
  if (engine.enableEventLog) {
    try {
      return await getWorkflowAuditTrail(engine.store, caseId);
    } catch {
      // Fall back to in-memory events
    }
  }

  // Get from in-memory event log
  const caseEvents = engine.events.filter(e => e.caseId === caseId);

  // Get receipts from case if available
  const yawlCase = engine.cases.get(caseId);
  const receipts = yawlCase
    ? yawlCase.receipts.map(r => r.toJSON())
    : [];

  return {
    caseId,
    events: caseEvents,
    receipts,
    eventCount: caseEvents.length,
    exportedAt: toISO(now()),
  };
}

/**
 * Replay workflow to a specific receipt
 * @param {Object} engine - Engine instance
 * @param {string} caseId - Case ID
 * @param {string} receiptId - Target receipt ID
 * @returns {Promise<{state: Object, verified: boolean}>}
 */
export async function replayToReceipt(engine, caseId, receiptId) {
  const yawlCase = engine.cases.get(caseId);
  if (!yawlCase) {
    throw new Error(`Case ${caseId} not found`);
  }

  // Find target receipt
  const targetReceiptIndex = yawlCase.receipts.findIndex(r => r.id === receiptId);
  if (targetReceiptIndex === -1) {
    throw new Error(`Receipt ${receiptId} not found`);
  }

  const targetReceipt = yawlCase.receipts[targetReceiptIndex];

  // Verify receipt chain up to target
  let verified = true;
  for (let i = 0; i <= targetReceiptIndex; i++) {
    const receipt = yawlCase.receipts[i];
    const previous = i > 0 ? yawlCase.receipts[i - 1] : null;

    const chainResult = await receipt.verifyChain(previous);
    if (!chainResult.valid) {
      verified = false;
      break;
    }
  }

  return {
    state: {
      afterHash: targetReceipt.afterHash,
      timestamp: targetReceipt.timestamp,
      action: targetReceipt.action,
      taskId: targetReceipt.taskId,
    },
    verified,
  };
}

// =============================================================================
// Resource Management
// =============================================================================

/**
 * Add a resource to the pool
 * @param {Object} engine - Engine instance
 * @param {Object} resourceData - Resource data
 * @returns {Object} Added resource
 */
export function addResource(engine, resourceData) {
  return engine.resourcePool.addResource(resourceData);
}

/**
 * Remove a resource from the pool
 * @param {Object} engine - Engine instance
 * @param {string} resourceId - Resource ID
 * @returns {boolean} True if removed
 */
export function removeResource(engine, resourceId) {
  return engine.resourcePool.removeResource(resourceId);
}

/**
 * Get resource pool statistics
 * @param {Object} engine - Engine instance
 * @returns {Object} Resource stats
 */
export function getResourceStats(engine) {
  return engine.resourcePool.getStats();
}

// =============================================================================
// Snapshot Timer
// =============================================================================

/**
 * Start automatic snapshot timer
 * @param {Object} engine - Engine instance
 */
export function startSnapshotTimer(engine) {
  if (engine._snapshotTimer) {
    clearInterval(engine._snapshotTimer);
  }

  engine._snapshotTimer = setInterval(async () => {
    try {
      await checkpoint(engine, 'auto');
    } catch (error) {
      console.error('Auto checkpoint failed:', error);
    }
  }, engine.snapshotInterval);
}

/**
 * Stop automatic snapshot timer
 * @param {Object} engine - Engine instance
 */
export function stopSnapshotTimer(engine) {
  if (engine._snapshotTimer) {
    clearInterval(engine._snapshotTimer);
    engine._snapshotTimer = null;
  }
}

/**
 * Shutdown engine gracefully
 * @param {Object} engine - Engine instance
 */
export function shutdown(engine) {
  stopSnapshotTimer(engine);
  engine._eventHandlers.clear();
}

// =============================================================================
// Serialization
// =============================================================================

/**
 * Serialize engine state
 * @param {Object} engine - Engine instance
 * @returns {Object} JSON-serializable state
 */
export function serializeEngine(engine) {
  return {
    nodeId: engine.nodeId,
    workflows: [...engine.workflows.values()].map(w => w.toJSON()),
    cases: [...engine.cases.values()].map(c => c.toJSON()),
    resourcePool: engine.resourcePool.toJSON(),
    events: engine.events,
    stats: engine._stats,
  };
}
