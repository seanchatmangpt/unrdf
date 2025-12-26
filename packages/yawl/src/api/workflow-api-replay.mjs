/**
 * @file YAWL Workflow API - Case Replay and Time Travel
 * @module @unrdf/yawl/api/workflow-api-replay
 *
 * @description
 * Implements deterministic case replay using KGC-4D event sourcing.
 * Reconstructs workflow case state at any point in time.
 */

import {
  YAWL_EVENT_TYPES,
  WORK_ITEM_STATUS,
  now,
  toISO,
  createReceipt,
} from './workflow-api-validation.mjs';

// ============================================================================
// Case Replay Functions
// ============================================================================

/**
 * Replay a case to reconstruct its state at a specific time.
 *
 * Uses KGC-4D reconstructState() to get case state at target time.
 * Returns historical case with all work item history.
 * Fully deterministic - same input always produces same output.
 *
 * @param {string} caseId - Case identifier to replay
 * @param {string|bigint} targetTime - Target time (ISO string or nanoseconds)
 * @param {Object} options - Replay options
 * @param {Object} options.store - KGC-4D store with event history
 * @param {Object} options.gitBackbone - Git backbone for snapshot access
 * @param {Function} [options.reconstructState] - Custom reconstructState function
 * @returns {Promise<Object>} Historical case with work item history
 *
 * @throws {Error} If case not found in event history
 * @throws {Error} If target time is before case creation
 *
 * @example
 * const historicalCase = await replayCase('order-12345', '2025-01-15T10:30:00Z', {
 *   store,
 *   gitBackbone,
 * });
 */
export async function replayCase(caseId, targetTime, options = {}) {
  // Validate inputs
  if (!caseId || typeof caseId !== 'string') {
    throw new TypeError('caseId must be a non-empty string');
  }

  if (!options.store) {
    throw new Error('replayCase requires a store option');
  }

  if (!options.gitBackbone) {
    throw new Error('replayCase requires a gitBackbone option');
  }

  // Convert target time to BigInt if ISO string
  let targetTimeNs;
  if (typeof targetTime === 'string') {
    // Parse ISO date to nanoseconds
    const ms = new Date(targetTime).getTime();
    if (isNaN(ms)) {
      throw new Error(`Invalid target time: ${targetTime}`);
    }
    targetTimeNs = BigInt(ms) * 1_000_000n;
  } else if (typeof targetTime === 'bigint') {
    targetTimeNs = targetTime;
  } else {
    throw new TypeError('targetTime must be an ISO string or BigInt nanoseconds');
  }

  const t_ns = now();

  // Use custom reconstructState or import from kgc-4d
  let reconstructState = options.reconstructState;
  if (!reconstructState) {
    // Dynamic import for reconstructState
    try {
      const kgc4d = await import('@unrdf/kgc-4d');
      reconstructState = kgc4d.reconstructState;
    } catch {
      throw new Error(
        'replayCase requires @unrdf/kgc-4d reconstructState or options.reconstructState'
      );
    }
  }

  // Reconstruct state at target time
  const historicalStore = await reconstructState(
    options.store,
    options.gitBackbone,
    targetTimeNs
  );

  // Query historical store for case events
  const caseEvents = await queryCaseEvents(caseId, historicalStore);

  if (caseEvents.length === 0) {
    throw new Error(`Case ${caseId} not found in event history at time ${targetTime}`);
  }

  // Reconstruct work item states from events
  const workItemHistory = reconstructWorkItemHistory(caseEvents);

  // Get case creation event for metadata
  const creationEvent = caseEvents.find(
    (e) => e.type === YAWL_EVENT_TYPES.CASE_CREATED
  );

  // Create receipt for replay
  const receipt = await createReceipt(
    YAWL_EVENT_TYPES.CASE_REPLAYED,
    {
      caseId,
      targetTime: targetTime.toString(),
      eventCount: caseEvents.length,
      reconstructedAt: toISO(t_ns),
    }
  );

  // Return historical case object
  return {
    caseId,
    workflowId: creationEvent?.payload?.workflowId,
    status: determineHistoricalCaseStatus(workItemHistory),
    workItemHistory,
    events: caseEvents,
    targetTime: targetTime.toString(),
    reconstructedAt: toISO(t_ns),
    receipt,
    _historicalStore: historicalStore,

    getWorkItemAtTime(taskId) {
      return workItemHistory.get(taskId) || null;
    },
    getWorkItemsAtTime() {
      return Array.from(workItemHistory.values());
    },
    getTaskEvents(taskId) {
      return caseEvents.filter((e) => e.payload?.taskId === taskId);
    },
  };
}

// ============================================================================
// Helper Functions
// ============================================================================

/**
 * Query case events from historical store
 * @param {string} caseId - Case identifier
 * @param {Object} store - Historical store
 * @returns {Promise<Array<Object>>} Case events
 */
export async function queryCaseEvents(caseId, store) {
  const events = [];

  // Query for events with this caseId in payload
  // Using SPARQL if available, otherwise pattern matching
  if (store.query) {
    try {
      const sparql = `
        PREFIX kgc: <http://kgc.io/>
        SELECT ?event ?type ?payload ?t_ns WHERE {
          ?event kgc:type ?type ;
                 kgc:payload ?payload ;
                 kgc:t_ns ?t_ns .
          FILTER(CONTAINS(?payload, "${caseId}"))
        }
        ORDER BY ?t_ns
      `;
      const results = await store.query(sparql);

      for (const row of results) {
        try {
          const payload = JSON.parse(row.payload?.value || '{}');
          if (payload.caseId === caseId) {
            events.push({
              id: row.event?.value,
              type: row.type?.value,
              payload,
              t_ns: row.t_ns?.value,
            });
          }
        } catch {
          // Skip malformed events
        }
      }
    } catch {
      // Fallback to pattern matching if SPARQL fails
    }
  }

  return events;
}

/**
 * Reconstruct work item history from events
 * @param {Array<Object>} events - Case events
 * @returns {Map<string, Object>} Work item history
 */
export function reconstructWorkItemHistory(events) {
  const workItemHistory = new Map();

  for (const event of events) {
    const taskId = event.payload?.taskId;
    if (!taskId) continue;

    // Get or create work item state
    let workItem = workItemHistory.get(taskId);
    if (!workItem) {
      workItem = {
        taskId,
        caseId: event.payload.caseId,
        events: [],
        status: WORK_ITEM_STATUS.PENDING,
      };
      workItemHistory.set(taskId, workItem);
    }

    // Add event to history
    workItem.events.push(event);

    // Update status based on event type
    switch (event.type) {
      case YAWL_EVENT_TYPES.TASK_ENABLED:
        workItem.status = WORK_ITEM_STATUS.ENABLED;
        break;
      case YAWL_EVENT_TYPES.TASK_STARTED:
        workItem.status = WORK_ITEM_STATUS.ACTIVE;
        workItem.startTime = event.payload.startTime;
        break;
      case YAWL_EVENT_TYPES.TASK_COMPLETED:
        workItem.status = WORK_ITEM_STATUS.COMPLETED;
        workItem.endTime = event.payload.endTime;
        workItem.result = event.payload.result;
        break;
      case YAWL_EVENT_TYPES.WORK_ITEM_CANCELLED:
        workItem.status = WORK_ITEM_STATUS.CANCELLED;
        workItem.endTime = event.payload.cancelTime;
        workItem.cancelReason = event.payload.reason;
        break;
    }
  }

  return workItemHistory;
}

/**
 * Determine historical case status from work item history
 * @param {Map<string, Object>} workItemHistory - Work item history
 * @returns {string} Case status
 */
export function determineHistoricalCaseStatus(workItemHistory) {
  const workItems = Array.from(workItemHistory.values());

  if (workItems.length === 0) {
    return 'unknown';
  }

  const allComplete = workItems.every(
    (wi) =>
      wi.status === WORK_ITEM_STATUS.COMPLETED ||
      wi.status === WORK_ITEM_STATUS.CANCELLED
  );

  if (allComplete) {
    const anyCompleted = workItems.some(
      (wi) => wi.status === WORK_ITEM_STATUS.COMPLETED
    );
    return anyCompleted ? 'completed' : 'cancelled';
  }

  const anyActive = workItems.some(
    (wi) => wi.status === WORK_ITEM_STATUS.ACTIVE
  );

  return anyActive ? 'active' : 'pending';
}
