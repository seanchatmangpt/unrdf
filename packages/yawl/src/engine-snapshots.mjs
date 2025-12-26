/**
 * @file YAWL Engine Snapshots - Time-travel, snapshots, and state persistence
 * @module @unrdf/yawl/engine-snapshots
 */

import { freezeUniverse, now, toISO } from '@unrdf/kgc-4d';
import {
  getWorkflowAuditTrail,
  reconstructCase as kgcReconstructCase,
} from './events/yawl-events.mjs';
import { ENGINE_EVENTS } from './engine-core.mjs';

/**
 * Mixin that adds snapshot and time-travel capabilities
 *
 * Provides:
 * - Checkpoint creation
 * - Case replay to specific timestamps
 * - Receipt-based replay
 * - Case history retrieval
 * - Automatic snapshot timer
 *
 * @param {class} Base - Base class to extend
 * @returns {class} Extended class with snapshot capabilities
 */
export function withSnapshots(Base) {
  return class EngineSnapshots extends Base {
    // =========================================================================
    // Checkpoint Creation
    // =========================================================================

    /**
     * Create a checkpoint for time-travel
     * @param {string} [label] - Optional checkpoint label
     * @returns {Promise<{timestamp: bigint, hash: string}>}
     */
    async checkpoint(label) {
      if (!this.git) {
        throw new Error('Git backbone required for checkpoints');
      }

      const freezeResult = await freezeUniverse(this.store, this.git);

      // Store checkpoint with case states
      const caseStates = new Map();
      for (const [caseId, yawlCase] of this.cases) {
        caseStates.set(caseId, yawlCase.toJSON());
      }

      this.checkpoints.set(BigInt(freezeResult.t_ns), {
        label,
        hash: freezeResult.universe_hash,
        gitRef: freezeResult.git_ref,
        caseStates: Object.fromEntries(caseStates),
        events: [...this.events],
      });

      this._stats.checkpointsCreated++;

      this.emit(ENGINE_EVENTS.CHECKPOINT_CREATED, {
        timestamp: BigInt(freezeResult.t_ns).toString(),
        hash: freezeResult.universe_hash,
        label,
      });

      return {
        timestamp: BigInt(freezeResult.t_ns),
        hash: freezeResult.universe_hash,
      };
    }

    // =========================================================================
    // Time-Travel Replay
    // =========================================================================

    /**
     * Replay a case to a specific point in time
     * @param {string} caseId - Case ID
     * @param {bigint} targetTime - Target timestamp (nanoseconds)
     * @returns {Promise<{state: Object, events: Array, verified: boolean}>}
     */
    async replayCase(caseId, targetTime) {
      // If Git backbone available, use KGC-4D reconstruction
      if (this.git) {
        try {
          return await kgcReconstructCase(this.store, this.git, caseId, targetTime);
        } catch {
          // Fall back to checkpoint-based reconstruction
        }
      }

      // Find the checkpoint before or at timestamp
      let targetCheckpoint = null;
      let targetCheckpointTime = 0n;

      for (const [checkpointTime, checkpoint] of this.checkpoints) {
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
      const yawlCase = this.cases.get(caseId);
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
     * @param {string} caseId - Case ID
     * @returns {Promise<Object>} Audit trail with events and receipts
     */
    async getCaseHistory(caseId) {
      // If KGC-4D event log enabled, get from store
      if (this.enableEventLog) {
        try {
          return await getWorkflowAuditTrail(this.store, caseId);
        } catch {
          // Fall back to in-memory events
        }
      }

      // Get from in-memory event log
      const caseEvents = this.events.filter(e => e.caseId === caseId);

      // Get receipts from case if available
      const yawlCase = this.cases.get(caseId);
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
     * @param {string} caseId - Case ID
     * @param {string} receiptId - Target receipt ID
     * @returns {Promise<{state: Object, verified: boolean}>}
     */
    async replayToReceipt(caseId, receiptId) {
      const yawlCase = this.cases.get(caseId);
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

    // =========================================================================
    // Snapshot Timer
    // =========================================================================

    /**
     * Start automatic snapshot timer
     * @protected
     */
    _startSnapshotTimer() {
      if (this._snapshotTimer) {
        clearInterval(this._snapshotTimer);
      }

      this._snapshotTimer = setInterval(async () => {
        try {
          await this.checkpoint('auto');
        } catch (error) {
          console.error('Auto checkpoint failed:', error);
        }
      }, this.snapshotInterval);
    }

    /**
     * Stop automatic snapshot timer
     * @protected
     */
    _stopSnapshotTimer() {
      if (this._snapshotTimer) {
        clearInterval(this._snapshotTimer);
        this._snapshotTimer = null;
      }
    }

    // =========================================================================
    // Constructor Extension
    // =========================================================================

    /**
     * Initialize snapshot timer if enabled
     * @protected
     */
    constructor(...args) {
      super(...args);

      // Initialize snapshot timer if enabled
      if (this.enableSnapshots && this.git) {
        this._startSnapshotTimer();
      }
    }
  };
}
