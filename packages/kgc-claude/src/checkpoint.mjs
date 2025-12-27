/**
 * Universal Checkpoint - Surface-agnostic freeze/thaw with receipts
 *
 * Provides checkpoints as receipts + snapshots:
 * freeze: O_t → ⟨snapshot, receipt_t, hash(μ(O_t))⟩
 *
 * Any automation step becomes: freeze → propose Δ → admit/deny → freeze
 * This is portable across any surface that can emit tool traces.
 *
 * @module @unrdf/kgc-claude/checkpoint
 */

import { blake3 } from 'hash-wasm';
import { z } from 'zod';
import { dataFactory } from '@unrdf/oxigraph';
import { now, toISO, freezeUniverse, reconstructState, VectorClock } from '@unrdf/kgc-4d';
import { GRAPHS, PREDICATES } from './constants.mjs';

/**
 * Checkpoint receipt schema
 */
export const CheckpointReceiptSchema = z.object({
  id: z.string().uuid(),
  t_ns: z.bigint(),
  timestamp_iso: z.string(),
  snapshotHash: z.string(),
  gitRef: z.string().optional(),
  universeSize: z.number(),
  runCapsuleIds: z.array(z.string()).default([]),
  vectorClock: z.any().optional(),
  previousCheckpointHash: z.string().nullable(),
  checkpointHash: z.string(),
});

/**
 * @typedef {z.infer<typeof CheckpointReceiptSchema>} CheckpointReceipt
 */

/**
 * Generate UUID v4
 * @returns {string}
 */
function generateUUID() {
  if (typeof crypto !== 'undefined' && crypto.randomUUID) {
    return crypto.randomUUID();
  }
  return 'xxxxxxxx-xxxx-4xxx-yxxx-xxxxxxxxxxxx'.replace(/[xy]/g, (c) => {
    const r = (Math.random() * 16) | 0;
    const v = c === 'x' ? r : (r & 0x3) | 0x8;
    return v.toString(16);
  });
}

/**
 * Checkpoint history for chaining
 * @type {Map<string, CheckpointReceipt>}
 */
const checkpointHistory = new Map();

/**
 * Get the latest checkpoint hash
 * @returns {string|null}
 */
function getLatestCheckpointHash() {
  let latest = null;
  let latestTime = 0n;
  for (const cp of checkpointHistory.values()) {
    if (cp.t_ns > latestTime) {
      latestTime = cp.t_ns;
      latest = cp.checkpointHash;
    }
  }
  return latest;
}

/**
 * Create a universal checkpoint
 *
 * @param {Object} store - KGCStore instance
 * @param {Object} gitBackbone - GitBackbone instance
 * @param {Object} [options]
 * @param {string[]} [options.runCapsuleIds] - Run capsule IDs to include
 * @param {Object} [options.vectorClock] - Vector clock for causality
 * @returns {Promise<CheckpointReceipt>}
 *
 * @example
 * const checkpoint = await freeze(store, git);
 * // ... do work ...
 * const restored = await thaw(store, git, checkpoint.id);
 */
export async function freeze(store, gitBackbone, options = {}) {
  // 1. Freeze universe using KGC-4D
  const freezeResult = await freezeUniverse(store, gitBackbone);

  const id = generateUUID();
  const t_ns = now();

  // 2. Get checkpoint chain hash
  const previousCheckpointHash = getLatestCheckpointHash();

  // 3. Compute checkpoint hash (chains to previous)
  const hashContent = {
    id,
    t_ns: t_ns.toString(),
    snapshotHash: freezeResult.universe_hash,
    gitRef: freezeResult.git_ref,
    runCapsuleIds: options.runCapsuleIds || [],
    previousCheckpointHash,
  };
  const checkpointHash = await blake3(JSON.stringify(hashContent));

  // 4. Count universe size
  const universeGraph = dataFactory.namedNode(GRAPHS.UNIVERSE);
  const universeQuads = [...store.match(null, null, null, universeGraph)];

  const receipt = CheckpointReceiptSchema.parse({
    id,
    t_ns,
    timestamp_iso: toISO(t_ns),
    snapshotHash: freezeResult.universe_hash,
    gitRef: freezeResult.git_ref,
    universeSize: universeQuads.length,
    runCapsuleIds: options.runCapsuleIds || [],
    vectorClock: options.vectorClock,
    previousCheckpointHash,
    checkpointHash,
  });

  // 5. Store in history
  checkpointHistory.set(id, receipt);

  return receipt;
}

/**
 * Restore state from a checkpoint
 *
 * @param {Object} store - KGCStore instance
 * @param {Object} gitBackbone - GitBackbone instance
 * @param {string} checkpointId - Checkpoint ID to restore
 * @returns {Promise<Object>} Restored store instance
 */
export async function thaw(store, gitBackbone, checkpointId) {
  const checkpoint = checkpointHistory.get(checkpointId);
  if (!checkpoint) {
    throw new Error(`Checkpoint not found: ${checkpointId}`);
  }

  // Reconstruct state at checkpoint time
  return reconstructState(store, gitBackbone, checkpoint.t_ns);
}

/**
 * Verify a checkpoint receipt
 *
 * @param {CheckpointReceipt} receipt - Checkpoint to verify
 * @param {Object} gitBackbone - GitBackbone instance
 * @returns {Promise<{ valid: boolean, reason?: string }>}
 */
export async function verifyCheckpoint(receipt, gitBackbone) {
  try {
    // 1. Load snapshot from git
    const nquads = await gitBackbone.readSnapshot(receipt.gitRef);

    // 2. Recompute hash
    const recomputedHash = await blake3(nquads);

    // 3. Compare
    if (recomputedHash !== receipt.snapshotHash) {
      return {
        valid: false,
        reason: `Snapshot hash mismatch: expected ${receipt.snapshotHash}, got ${recomputedHash}`,
      };
    }

    // 4. Verify chain if previous exists
    if (receipt.previousCheckpointHash) {
      const previous = [...checkpointHistory.values()].find(
        (cp) => cp.checkpointHash === receipt.previousCheckpointHash,
      );
      if (!previous) {
        return {
          valid: false,
          reason: `Previous checkpoint not found: ${receipt.previousCheckpointHash}`,
        };
      }
    }

    return { valid: true };
  } catch (error) {
    return { valid: false, reason: `Verification failed: ${error.message}` };
  }
}

/**
 * Create a checkpoint-protected execution context
 *
 * @param {Object} store - KGCStore instance
 * @param {Object} gitBackbone - GitBackbone instance
 * @param {Function} operation - Async function to execute
 * @returns {Promise<{ result: any, checkpoint: CheckpointReceipt }>}
 *
 * @example
 * const { result, checkpoint } = await withCheckpoint(store, git, async (ctx) => {
 *   // Do work that can be rolled back
 *   return someResult;
 * });
 */
export async function withCheckpoint(store, gitBackbone, operation) {
  // 1. Create checkpoint before operation
  const beforeCheckpoint = await freeze(store, gitBackbone);

  try {
    // 2. Execute operation
    const result = await operation({ checkpoint: beforeCheckpoint });

    // 3. Create checkpoint after operation
    const afterCheckpoint = await freeze(store, gitBackbone, {
      runCapsuleIds: result.runCapsuleIds || [],
    });

    return { result, checkpoint: afterCheckpoint };
  } catch (error) {
    // 4. Rollback on failure
    await thaw(store, gitBackbone, beforeCheckpoint.id);
    throw error;
  }
}

/**
 * Get checkpoint history
 * @returns {CheckpointReceipt[]}
 */
export function getCheckpointHistory() {
  return [...checkpointHistory.values()].sort((a, b) => {
    if (a.t_ns < b.t_ns) return -1;
    if (a.t_ns > b.t_ns) return 1;
    return 0;
  });
}

/**
 * Clear checkpoint history (for testing)
 */
export function clearCheckpointHistory() {
  checkpointHistory.clear();
}

/**
 * Session continuity: reconstruct state from receipts + event log
 *
 * Sessions are derived, not stored:
 * O_{t+k} = reconstruct(O_t, Δ_{t:t+k})
 *
 * @param {Object} store - KGCStore instance
 * @param {Object} gitBackbone - GitBackbone instance
 * @param {bigint} targetTime - Target time to reconstruct
 * @returns {Promise<Object>} Reconstructed store
 */
export async function reconstructSession(store, gitBackbone, targetTime) {
  return reconstructState(store, gitBackbone, targetTime);
}

/**
 * Calculate session drift from ideal state
 *
 * @param {Object} actualStore - Actual current store
 * @param {Object} expectedStore - Expected store from reconstruction
 * @returns {number} Drift score (0 = perfect, higher = more drift)
 */
export function calculateDrift(actualStore, expectedStore) {
  const actualGraph = dataFactory.namedNode(GRAPHS.UNIVERSE);
  const actualQuads = new Set([...actualStore.match(null, null, null, actualGraph)].map(quadToString));
  const expectedQuads = new Set([...expectedStore.match(null, null, null, actualGraph)].map(quadToString));

  // Calculate symmetric difference
  let missing = 0;
  let extra = 0;

  for (const q of expectedQuads) {
    if (!actualQuads.has(q)) missing++;
  }
  for (const q of actualQuads) {
    if (!expectedQuads.has(q)) extra++;
  }

  // Drift = (missing + extra) / expected size
  return expectedQuads.size === 0 ? 0 : (missing + extra) / expectedQuads.size;
}

/**
 * Convert quad to string for comparison
 * @param {Object} quad
 * @returns {string}
 */
function quadToString(quad) {
  return `${quad.subject.value}|${quad.predicate.value}|${quad.object.value}|${quad.graph.value}`;
}
