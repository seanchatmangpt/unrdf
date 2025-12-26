/**
 * Snapshot Integration - Optional KGC-4D freeze/restore support
 * @module agent-8/freeze
 */

/**
 * Create snapshot of current store state
 *
 * Integrates with @unrdf/kgc-4d to create Git-backed snapshots of the store.
 * Requires KGC-4D GitBackbone instance and enableSnapshots=true on store.
 *
 * @param {AtomicStore} store - Store to snapshot
 * @param {string} label - Human-readable label for the snapshot
 * @param {Object} gitBackbone - GitBackbone from @unrdf/kgc-4d
 * @returns {Promise<Object>} Freeze receipt from KGC-4D
 * @throws {Error} If snapshots not enabled or freeze fails
 *
 * @example
 * import { snapshotStore } from './freeze.mjs';
 * import { GitBackbone } from '@unrdf/kgc-4d/git';
 *
 * const git = new GitBackbone('/tmp/store-snapshots');
 * const receipt = await snapshotStore(store, 'v1.0', git);
 * console.log(receipt.git_ref); // Git commit hash
 */
export async function snapshotStore(store, label, gitBackbone) {
  if (!store || !store.snapshotsEnabled) {
    throw new Error('Snapshots not enabled on this store');
  }

  if (!label || typeof label !== 'string') {
    throw new TypeError('Label must be a non-empty string');
  }

  if (!gitBackbone || typeof gitBackbone !== 'object') {
    throw new TypeError('GitBackbone instance required');
  }

  try {
    // Dynamic import of KGC-4D freeze functionality
    const { freezeUniverse } = await import('@unrdf/kgc-4d/freeze');

    // Extract all quads from store
    const quads = [...store.match()];

    // Create freeze receipt
    const freezeReceipt = await freezeUniverse(quads, {
      label,
      gitBackbone,
      timestamp: new Date().toISOString()
    });

    return freezeReceipt;
  } catch (error) {
    throw new Error(`Snapshot creation failed: ${error.message}`);
  }
}

/**
 * Restore store from snapshot
 *
 * Clears the target store and restores it from a KGC-4D snapshot.
 * WARNING: This operation clears all existing data in the store.
 *
 * @param {AtomicStore} store - Target store (will be cleared)
 * @param {string} gitRef - Git reference from freeze receipt
 * @param {Object} gitBackbone - GitBackbone instance
 * @returns {Promise<void>}
 * @throws {Error} If restore fails
 *
 * @example
 * await restoreSnapshot(store, 'abc123...', git);
 * console.log(store.size()); // Restored quad count
 */
export async function restoreSnapshot(store, gitRef, gitBackbone) {
  if (!store || !store.snapshotsEnabled) {
    throw new Error('Snapshots not enabled on this store');
  }

  if (!gitRef || typeof gitRef !== 'string') {
    throw new TypeError('Git reference must be a non-empty string');
  }

  if (!gitBackbone || typeof gitBackbone !== 'object') {
    throw new TypeError('GitBackbone instance required');
  }

  try {
    // Dynamic import of KGC-4D reconstruction
    const { reconstructState } = await import('@unrdf/kgc-4d/freeze');

    // Reconstruct state from Git reference
    const quads = await reconstructState(gitRef, gitBackbone);

    // Clear existing store
    store.clear();

    // Add reconstructed quads
    for (const quad of quads) {
      store.add(quad);
    }

  } catch (error) {
    throw new Error(`Snapshot restore failed: ${error.message}`);
  }
}

/**
 * Check if snapshots are enabled
 *
 * @param {AtomicStore} store - Store to check
 * @returns {boolean} True if snapshots are enabled
 *
 * @example
 * if (snapshotsEnabled(store)) {
 *   await snapshotStore(store, 'backup', git);
 * }
 */
export function snapshotsEnabled(store) {
  return store && store.snapshotsEnabled === true;
}

/**
 * List available snapshots
 *
 * Returns list of available snapshots from GitBackbone.
 * Requires KGC-4D integration.
 *
 * @param {Object} gitBackbone - GitBackbone instance
 * @returns {Promise<Array>} Array of snapshot metadata
 * @throws {Error} If listing fails
 *
 * @example
 * const snapshots = await listSnapshots(git);
 * console.log(snapshots); // [{ ref: 'abc...', label: 'v1.0', timestamp: '...' }, ...]
 */
export async function listSnapshots(gitBackbone) {
  if (!gitBackbone || typeof gitBackbone !== 'object') {
    throw new TypeError('GitBackbone instance required');
  }

  try {
    // This is a placeholder - actual implementation depends on KGC-4D API
    // For now, return empty array
    return [];
  } catch (error) {
    throw new Error(`Snapshot listing failed: ${error.message}`);
  }
}
