/**
 * @file YAWL Realtime Server - Socket.io integration for collaborative workflows
 * @module @unrdf/yawl-realtime/server
 *
 * @description
 * Real-time collaboration server that:
 * - Broadcasts YAWL engine events to connected clients
 * - Handles concurrent task claims with optimistic locking
 * - Syncs workflow state using CRDT-inspired merge strategies
 * - Uses YAWL receipts for conflict detection
 */

import { Server } from 'socket.io';
import { z } from 'zod';
import { ENGINE_EVENTS } from '@unrdf/yawl';

// =============================================================================
// Schemas
// =============================================================================

const TaskClaimSchema = z.object({
  caseId: z.string(),
  workItemId: z.string(),
  userId: z.string(),
  timestamp: z.number(),
  expectedReceiptHash: z.string().optional(),
});

const TaskReleaseSchema = z.object({
  caseId: z.string(),
  workItemId: z.string(),
  userId: z.string(),
});

const TaskCompleteSchema = z.object({
  caseId: z.string(),
  workItemId: z.string(),
  userId: z.string(),
  output: z.record(z.any()),
  receiptHash: z.string().optional(),
});

// =============================================================================
// Optimistic Lock Manager
// =============================================================================

/**
 * Manages optimistic locks for concurrent task execution
 * Uses Lamport timestamps for conflict resolution
 */
class OptimisticLockManager {
  constructor() {
    /** @type {Map<string, {userId: string, timestamp: number, receiptHash: string}>} */
    this.locks = new Map();
    /** @type {Map<string, number>} Lamport clock per case */
    this.lamportClocks = new Map();
  }

  /**
   * Get next Lamport timestamp for a case
   * @param {string} caseId - Case ID
   * @returns {number} Next timestamp
   */
  getNextTimestamp(caseId) {
    const current = this.lamportClocks.get(caseId) || 0;
    const next = current + 1;
    this.lamportClocks.set(caseId, next);
    return next;
  }

  /**
   * Update Lamport clock based on received timestamp
   * @param {string} caseId - Case ID
   * @param {number} receivedTimestamp - Timestamp from claim request
   */
  updateClock(caseId, receivedTimestamp) {
    const current = this.lamportClocks.get(caseId) || 0;
    const next = Math.max(current, receivedTimestamp) + 1;
    this.lamportClocks.set(caseId, next);
  }

  /**
   * Attempt to acquire lock on a work item
   * @param {string} workItemId - Work item ID
   * @param {string} caseId - Case ID
   * @param {string} userId - User attempting to claim
   * @param {number} timestamp - Lamport timestamp of claim
   * @param {string} [receiptHash] - Expected receipt hash for conflict detection
   * @returns {{success: boolean, lock?: Object, conflict?: Object}}
   */
  acquire(workItemId, caseId, userId, timestamp, receiptHash) {
    // Update Lamport clock
    this.updateClock(caseId, timestamp);

    const existing = this.locks.get(workItemId);

    if (!existing) {
      // No lock exists, grant it
      const lock = { userId, timestamp, receiptHash, caseId };
      this.locks.set(workItemId, lock);
      return { success: true, lock };
    }

    // Lock exists - check for conflicts
    if (existing.userId === userId) {
      // Same user, update timestamp
      existing.timestamp = timestamp;
      return { success: true, lock: existing };
    }

    // Different user - use Lamport timestamp for conflict resolution
    if (timestamp > existing.timestamp) {
      // Newer claim wins (last-write-wins CRDT semantics)
      const lock = { userId, timestamp, receiptHash, caseId };
      this.locks.set(workItemId, lock);
      return {
        success: true,
        lock,
        conflict: {
          previousOwner: existing.userId,
          previousTimestamp: existing.timestamp,
          resolution: 'newer_claim_wins',
        },
      };
    }

    // Existing lock is newer or equal, deny claim
    return {
      success: false,
      conflict: {
        currentOwner: existing.userId,
        currentTimestamp: existing.timestamp,
        requestTimestamp: timestamp,
        resolution: 'existing_lock_held',
      },
    };
  }

  /**
   * Release a lock
   * @param {string} workItemId - Work item ID
   * @param {string} userId - User releasing the lock
   * @returns {{success: boolean, error?: string}}
   */
  release(workItemId, userId) {
    const lock = this.locks.get(workItemId);

    if (!lock) {
      return { success: true }; // Already released
    }

    if (lock.userId !== userId) {
      return {
        success: false,
        error: `Lock held by ${lock.userId}, cannot release by ${userId}`,
      };
    }

    this.locks.delete(workItemId);
    return { success: true };
  }

  /**
   * Get current lock holder
   * @param {string} workItemId - Work item ID
   * @returns {Object|null} Lock info or null
   */
  getLock(workItemId) {
    return this.locks.get(workItemId) || null;
  }

  /**
   * Get all locks for a case
   * @param {string} caseId - Case ID
   * @returns {Array<{workItemId: string, lock: Object}>}
   */
  getLocksForCase(caseId) {
    const result = [];
    for (const [workItemId, lock] of this.locks.entries()) {
      if (lock.caseId === caseId) {
        result.push({ workItemId, lock });
      }
    }
    return result;
  }

  /**
   * Clear all locks (for testing)
   */
  clear() {
    this.locks.clear();
    this.lamportClocks.clear();
  }
}

// =============================================================================
// State Synchronization Manager
// =============================================================================

/**
 * Manages state synchronization using CRDT-inspired merge strategies
 */
class StateSyncManager {
  constructor() {
    /** @type {Map<string, Object>} Latest known state per case */
    this.caseStates = new Map();
    /** @type {Map<string, Array>} Receipt chains per case */
    this.receiptChains = new Map();
  }

  /**
   * Update case state with CRDT merge semantics
   * @param {string} caseId - Case ID
   * @param {Object} update - State update
   * @param {string} receiptHash - Receipt hash for verification
   * @returns {{merged: Object, conflicts: Array}}
   */
  mergeState(caseId, update, receiptHash) {
    const current = this.caseStates.get(caseId) || { data: {}, workItems: {} };
    const conflicts = [];

    // Last-Write-Wins for scalar values
    const mergedData = { ...current.data };
    for (const [key, value] of Object.entries(update.data || {})) {
      if (current.data[key] !== undefined && current.data[key] !== value) {
        conflicts.push({
          type: 'data_overwrite',
          key,
          oldValue: current.data[key],
          newValue: value,
          resolution: 'last_write_wins',
        });
      }
      mergedData[key] = value;
    }

    // Set union for work items (add-wins semantics)
    const mergedWorkItems = { ...current.workItems, ...(update.workItems || {}) };

    const merged = {
      data: mergedData,
      workItems: mergedWorkItems,
      lastUpdated: Date.now(),
      receiptHash,
    };

    this.caseStates.set(caseId, merged);

    // Track receipt in chain
    const chain = this.receiptChains.get(caseId) || [];
    chain.push({ hash: receiptHash, timestamp: Date.now() });
    this.receiptChains.set(caseId, chain);

    return { merged, conflicts };
  }

  /**
   * Get current state for a case
   * @param {string} caseId - Case ID
   * @returns {Object|null}
   */
  getState(caseId) {
    return this.caseStates.get(caseId) || null;
  }

  /**
   * Verify receipt chain continuity
   * @param {string} caseId - Case ID
   * @param {string} expectedHash - Expected current hash
   * @returns {{valid: boolean, error?: string}}
   */
  verifyReceiptChain(caseId, expectedHash) {
    const state = this.caseStates.get(caseId);
    if (!state) {
      return { valid: true }; // No state yet, accept any
    }

    if (state.receiptHash === expectedHash) {
      return { valid: true };
    }

    return {
      valid: false,
      error: `Receipt hash mismatch: expected ${expectedHash}, current ${state.receiptHash}`,
      currentHash: state.receiptHash,
    };
  }
}

// =============================================================================
// YAWL Realtime Server
// =============================================================================

/**
 * Real-time collaboration server for YAWL workflows
 *
 * @example
 * ```javascript
 * import { createWorkflowEngine } from '@unrdf/yawl';
 * import { YAWLRealtimeServer } from '@unrdf/yawl-realtime/server';
 *
 * const engine = createWorkflowEngine();
 * const server = new YAWLRealtimeServer(engine, { port: 3000 });
 *
 * server.start();
 * ```
 */
export class YAWLRealtimeServer {
  /**
   * @param {import('@unrdf/yawl').WorkflowEngine} engine - YAWL engine instance
   * @param {Object} options - Server options
   * @param {number} [options.port=3000] - Port to listen on
   * @param {Object} [options.corsOptions] - CORS configuration
   */
  constructor(engine, options = {}) {
    this.engine = engine;
    this.port = options.port || 3000;

    // Create Socket.io server
    this.io = new Server(this.port, {
      cors: options.corsOptions || {
        origin: '*',
        methods: ['GET', 'POST'],
      },
    });

    // Initialize managers
    this.lockManager = new OptimisticLockManager();
    this.syncManager = new StateSyncManager();

    // Track connected clients
    /** @type {Map<string, {userId: string, socket: Object}>} */
    this.clients = new Map();

    this._setupEngineEventBroadcasting();
    this._setupSocketHandlers();
  }

  /**
   * Setup broadcasting of YAWL engine events to all clients
   * @private
   */
  _setupEngineEventBroadcasting() {
    // Broadcast all engine events to connected clients
    const eventTypes = Object.values(ENGINE_EVENTS);

    for (const eventType of eventTypes) {
      this.engine.on(eventType, (event) => {
        this.io.emit('yawl:event', {
          type: eventType,
          ...event,
        });

        // Update state for case-related events
        if (event.caseId && event.type) {
          const caseInstance = this.engine.cases.get(event.caseId);
          if (caseInstance) {
            const latestReceipt = caseInstance.receipts[caseInstance.receipts.length - 1];
            this.syncManager.mergeState(
              event.caseId,
              {
                data: caseInstance.data,
                workItems: Object.fromEntries(caseInstance.workItems),
              },
              latestReceipt?.hash || 'initial'
            );
          }
        }
      });
    }
  }

  /**
   * Setup Socket.io connection and message handlers
   * @private
   */
  _setupSocketHandlers() {
    this.io.on('connection', (socket) => {
      console.log(`[YAWLRealtimeServer] Client connected: ${socket.id}`);

      // Handle user identification
      socket.on('identify', (data) => {
        const userId = data.userId || socket.id;
        this.clients.set(socket.id, { userId, socket });
        socket.userId = userId;

        socket.emit('identified', { userId, socketId: socket.id });
        console.log(`[YAWLRealtimeServer] User identified: ${userId}`);
      });

      // Handle task claim requests (optimistic locking)
      socket.on('task:claim', async (data) => {
        try {
          const claim = TaskClaimSchema.parse(data);
          const userId = socket.userId || socket.id;

          // Attempt to acquire lock
          const result = this.lockManager.acquire(
            claim.workItemId,
            claim.caseId,
            userId,
            claim.timestamp,
            claim.expectedReceiptHash
          );

          if (result.success) {
            // Lock acquired, start the task
            const taskResult = await this.engine.startTask(
              claim.caseId,
              claim.workItemId,
              { actor: userId }
            );

            socket.emit('task:claimed', {
              success: true,
              workItemId: claim.workItemId,
              lock: result.lock,
              task: taskResult.task,
              receipt: taskResult.receipt,
              conflict: result.conflict,
            });

            // Broadcast to other clients
            socket.broadcast.emit('task:locked', {
              caseId: claim.caseId,
              workItemId: claim.workItemId,
              userId,
              timestamp: result.lock.timestamp,
            });

            console.log(`[YAWLRealtimeServer] Task claimed: ${claim.workItemId} by ${userId}`);
          } else {
            // Lock denied
            socket.emit('task:claimed', {
              success: false,
              workItemId: claim.workItemId,
              conflict: result.conflict,
            });

            console.log(`[YAWLRealtimeServer] Task claim denied: ${claim.workItemId} by ${userId}`);
          }
        } catch (error) {
          socket.emit('error', {
            type: 'task:claim',
            message: error.message,
            stack: error.stack,
          });
          console.error('[YAWLRealtimeServer] Task claim error:', error);
        }
      });

      // Handle task completion
      socket.on('task:complete', async (data) => {
        try {
          const completion = TaskCompleteSchema.parse(data);
          const userId = socket.userId || socket.id;

          // Verify lock ownership
          const lock = this.lockManager.getLock(completion.workItemId);
          if (!lock || lock.userId !== userId) {
            socket.emit('task:completed', {
              success: false,
              error: 'Task not locked by this user',
            });
            return;
          }

          // Complete the task
          const result = await this.engine.completeTask(
            completion.caseId,
            completion.workItemId,
            completion.output,
            userId
          );

          // Release lock
          this.lockManager.release(completion.workItemId, userId);

          socket.emit('task:completed', {
            success: true,
            workItemId: completion.workItemId,
            task: result.task,
            receipt: result.receipt,
            downstreamEnabled: result.downstreamEnabled,
          });

          // Broadcast to other clients
          socket.broadcast.emit('task:unlocked', {
            caseId: completion.caseId,
            workItemId: completion.workItemId,
            userId,
          });

          console.log(`[YAWLRealtimeServer] Task completed: ${completion.workItemId} by ${userId}`);
        } catch (error) {
          socket.emit('error', {
            type: 'task:complete',
            message: error.message,
            stack: error.stack,
          });
          console.error('[YAWLRealtimeServer] Task completion error:', error);
        }
      });

      // Handle task release (unlock without completing)
      socket.on('task:release', (data) => {
        try {
          const release = TaskReleaseSchema.parse(data);
          const userId = socket.userId || socket.id;

          const result = this.lockManager.release(release.workItemId, userId);

          socket.emit('task:released', {
            success: result.success,
            workItemId: release.workItemId,
            error: result.error,
          });

          if (result.success) {
            socket.broadcast.emit('task:unlocked', {
              caseId: release.caseId,
              workItemId: release.workItemId,
              userId,
            });
          }
        } catch (error) {
          socket.emit('error', {
            type: 'task:release',
            message: error.message,
          });
        }
      });

      // Handle state sync requests
      socket.on('state:sync', (data) => {
        const { caseId } = data;
        const state = this.syncManager.getState(caseId);
        const locks = this.lockManager.getLocksForCase(caseId);

        socket.emit('state:synced', {
          caseId,
          state,
          locks,
        });
      });

      // Handle disconnection
      socket.on('disconnect', () => {
        console.log(`[YAWLRealtimeServer] Client disconnected: ${socket.id}`);
        this.clients.delete(socket.id);

        // Release all locks held by this user
        // (In production, you might want to keep locks for a grace period)
        // For now, we'll just log them
        for (const [workItemId, lock] of this.lockManager.locks.entries()) {
          if (lock.userId === socket.userId) {
            console.log(`[YAWLRealtimeServer] Orphaned lock: ${workItemId} by ${socket.userId}`);
          }
        }
      });
    });
  }

  /**
   * Start the server
   * @returns {Promise<void>}
   */
  async start() {
    console.log(`[YAWLRealtimeServer] Server started on port ${this.port}`);
    return this;
  }

  /**
   * Stop the server
   * @returns {Promise<void>}
   */
  async stop() {
    return new Promise((resolve) => {
      this.io.close(() => {
        console.log('[YAWLRealtimeServer] Server stopped');
        resolve();
      });
    });
  }

  /**
   * Get server statistics
   * @returns {Object} Server stats
   */
  getStats() {
    return {
      connectedClients: this.clients.size,
      activeLocks: this.lockManager.locks.size,
      casesTracked: this.syncManager.caseStates.size,
      clients: Array.from(this.clients.values()).map(c => ({
        userId: c.userId,
        socketId: c.socket.id,
      })),
    };
  }
}

// =============================================================================
// Exports
// =============================================================================

export {
  OptimisticLockManager,
  StateSyncManager,
};

export default YAWLRealtimeServer;
