/**
 * Time Travel - Advanced Checkpointing with Named Points and Selective Rollback
 *
 * Hyper-advanced time-travel capabilities combining:
 * - KGC-4D snapshot reconstruction
 * - Named checkpoints with labels
 * - Checkpoint diffing and comparison
 * - Selective rollback to specific points
 * - Branch-aware checkpointing
 *
 * @module @unrdf/kgc-claude/capabilities/time-travel
 */

import { blake3 } from 'hash-wasm';
import { dataFactory } from '@unrdf/oxigraph';
import { now, toISO, freezeUniverse, reconstructState } from '@unrdf/kgc-4d';
import { z } from 'zod';

// =============================================================================
// Schemas
// =============================================================================

/**
 * Named checkpoint schema
 */
export const NamedCheckpointSchema = z.object({
  id: z.string().uuid(),
  label: z.string(),
  description: z.string().optional(),
  t_ns: z.bigint(),
  timestamp_iso: z.string(),
  snapshotHash: z.string(),
  gitRef: z.string(),
  universeSize: z.number(),
  branch: z.string().default('main'),
  tags: z.array(z.string()).default([]),
  metadata: z.record(z.any()).optional(),
  previousCheckpointId: z.string().nullable(),
  checkpointHash: z.string(),
});

/**
 * Checkpoint diff schema
 */
export const CheckpointDiffSchema = z.object({
  fromCheckpoint: z.string(),
  toCheckpoint: z.string(),
  addedQuads: z.number(),
  removedQuads: z.number(),
  unchangedQuads: z.number(),
  timeDeltaNs: z.bigint(),
  additions: z.array(z.any()).optional(),
  deletions: z.array(z.any()).optional(),
  diffHash: z.string(),
});

// =============================================================================
// Time Travel Manager
// =============================================================================

/**
 * TimeTravelManager - Advanced checkpointing with named points
 */
export class TimeTravelManager {
  /**
   * @param {Object} options
   * @param {Object} options.store - KGCStore instance
   * @param {Object} options.gitBackbone - GitBackbone instance
   * @param {number} [options.maxCheckpoints] - Maximum checkpoints to retain
   */
  constructor(options = {}) {
    const { store, gitBackbone, maxCheckpoints = 1000 } = options;

    if (!store) throw new Error('store is required');
    if (!gitBackbone) throw new Error('gitBackbone is required');

    /** @type {Object} */
    this.store = store;

    /** @type {Object} */
    this.gitBackbone = gitBackbone;

    /** @type {Map<string, Object>} Named checkpoints by ID */
    this.checkpoints = new Map();

    /** @type {Map<string, string>} Label to checkpoint ID mapping */
    this.labelIndex = new Map();

    /** @type {Map<string, Set<string>>} Branch to checkpoint IDs mapping */
    this.branchIndex = new Map();

    /** @type {Map<string, Set<string>>} Tag to checkpoint IDs mapping */
    this.tagIndex = new Map();

    /** @type {number} */
    this.maxCheckpoints = maxCheckpoints;

    /** @type {string} */
    this.currentBranch = 'main';
  }

  /**
   * Create a named checkpoint
   *
   * @param {string} label - Checkpoint label (must be unique per branch)
   * @param {Object} [options]
   * @param {string} [options.description] - Checkpoint description
   * @param {string[]} [options.tags] - Tags for categorization
   * @param {Object} [options.metadata] - Additional metadata
   * @param {string} [options.branch] - Branch name (default: current)
   * @returns {Promise<Object>} Named checkpoint receipt
   */
  async createCheckpoint(label, options = {}) {
    const {
      description = '',
      tags = [],
      metadata = {},
      branch = this.currentBranch,
    } = options;

    // Check for duplicate label on same branch
    const existingId = this.labelIndex.get(`${branch}:${label}`);
    if (existingId) {
      throw new Error(`Checkpoint label "${label}" already exists on branch "${branch}"`);
    }

    // Freeze universe
    const freezeResult = await freezeUniverse(this.store, this.gitBackbone);

    const id = this._generateUUID();
    const t_ns = now();

    // Get previous checkpoint on this branch
    const branchCheckpoints = this.branchIndex.get(branch) || new Set();
    const previousCheckpointId = this._getLatestCheckpointId(branchCheckpoints);

    // Compute checkpoint hash (chains to previous)
    const hashContent = {
      id,
      label,
      branch,
      t_ns: t_ns.toString(),
      snapshotHash: freezeResult.universe_hash,
      previousCheckpointId,
    };
    const checkpointHash = await blake3(JSON.stringify(hashContent));

    // Count universe size
    const universeGraph = dataFactory.namedNode('http://kgc.io/Universe');
    const universeQuads = [...this.store.match(null, null, null, universeGraph)];

    const checkpoint = NamedCheckpointSchema.parse({
      id,
      label,
      description,
      t_ns,
      timestamp_iso: toISO(t_ns),
      snapshotHash: freezeResult.universe_hash,
      gitRef: freezeResult.git_ref,
      universeSize: universeQuads.length,
      branch,
      tags,
      metadata,
      previousCheckpointId,
      checkpointHash,
    });

    // Store checkpoint
    this.checkpoints.set(id, checkpoint);

    // Index by label
    this.labelIndex.set(`${branch}:${label}`, id);

    // Index by branch
    if (!this.branchIndex.has(branch)) {
      this.branchIndex.set(branch, new Set());
    }
    this.branchIndex.get(branch).add(id);

    // Index by tags
    for (const tag of tags) {
      if (!this.tagIndex.has(tag)) {
        this.tagIndex.set(tag, new Set());
      }
      this.tagIndex.get(tag).add(id);
    }

    // Enforce max checkpoints (LRU eviction per branch)
    this._enforceMaxCheckpoints(branch);

    return checkpoint;
  }

  /**
   * Restore to a named checkpoint
   *
   * @param {string} label - Checkpoint label
   * @param {Object} [options]
   * @param {string} [options.branch] - Branch name (default: current)
   * @returns {Promise<Object>} Restored store instance
   */
  async restoreToCheckpoint(label, options = {}) {
    const { branch = this.currentBranch } = options;

    const checkpointId = this.labelIndex.get(`${branch}:${label}`);
    if (!checkpointId) {
      throw new Error(`Checkpoint "${label}" not found on branch "${branch}"`);
    }

    const checkpoint = this.checkpoints.get(checkpointId);
    return reconstructState(this.store, this.gitBackbone, checkpoint.t_ns);
  }

  /**
   * Restore to specific time (nanosecond precision)
   *
   * @param {bigint} targetTime - Target time in nanoseconds
   * @returns {Promise<Object>} Restored store instance
   */
  async restoreToTime(targetTime) {
    return reconstructState(this.store, this.gitBackbone, targetTime);
  }

  /**
   * Compare two checkpoints and compute diff
   *
   * @param {string} fromLabel - Source checkpoint label
   * @param {string} toLabel - Target checkpoint label
   * @param {Object} [options]
   * @param {string} [options.branch] - Branch name (default: current)
   * @param {boolean} [options.includeDetails] - Include quad details
   * @returns {Promise<Object>} Checkpoint diff
   */
  async diffCheckpoints(fromLabel, toLabel, options = {}) {
    const { branch = this.currentBranch, includeDetails = false } = options;

    const fromId = this.labelIndex.get(`${branch}:${fromLabel}`);
    const toId = this.labelIndex.get(`${branch}:${toLabel}`);

    if (!fromId) throw new Error(`Checkpoint "${fromLabel}" not found`);
    if (!toId) throw new Error(`Checkpoint "${toLabel}" not found`);

    const fromCheckpoint = this.checkpoints.get(fromId);
    const toCheckpoint = this.checkpoints.get(toId);

    // Load both snapshots
    const fromSnapshot = await this.gitBackbone.readSnapshot(fromCheckpoint.gitRef);
    const toSnapshot = await this.gitBackbone.readSnapshot(toCheckpoint.gitRef);

    // Parse N-Quads
    const fromQuads = this._parseNQuads(fromSnapshot);
    const toQuads = this._parseNQuads(toSnapshot);

    // Compute set differences
    const fromSet = new Set(fromQuads.map(this._quadToString));
    const toSet = new Set(toQuads.map(this._quadToString));

    const additions = [];
    const deletions = [];

    for (const quad of toQuads) {
      const quadStr = this._quadToString(quad);
      if (!fromSet.has(quadStr)) {
        additions.push(includeDetails ? quad : quadStr);
      }
    }

    for (const quad of fromQuads) {
      const quadStr = this._quadToString(quad);
      if (!toSet.has(quadStr)) {
        deletions.push(includeDetails ? quad : quadStr);
      }
    }

    const unchangedQuads = toQuads.length - additions.length;
    const timeDeltaNs = toCheckpoint.t_ns - fromCheckpoint.t_ns;

    // Compute diff hash
    const diffContent = {
      fromCheckpoint: fromCheckpoint.id,
      toCheckpoint: toCheckpoint.id,
      addedQuads: additions.length,
      removedQuads: deletions.length,
      timeDeltaNs: timeDeltaNs.toString(),
    };
    const diffHash = await blake3(JSON.stringify(diffContent));

    return CheckpointDiffSchema.parse({
      fromCheckpoint: fromCheckpoint.id,
      toCheckpoint: toCheckpoint.id,
      addedQuads: additions.length,
      removedQuads: deletions.length,
      unchangedQuads,
      timeDeltaNs,
      additions: includeDetails ? additions : undefined,
      deletions: includeDetails ? deletions : undefined,
      diffHash,
    });
  }

  /**
   * List checkpoints by criteria
   *
   * @param {Object} [options]
   * @param {string} [options.branch] - Filter by branch
   * @param {string} [options.tag] - Filter by tag
   * @param {bigint} [options.after] - Filter by time (after)
   * @param {bigint} [options.before] - Filter by time (before)
   * @returns {Object[]} Matching checkpoints
   */
  listCheckpoints(options = {}) {
    const { branch, tag, after, before } = options;

    let checkpoints = Array.from(this.checkpoints.values());

    if (branch) {
      const branchIds = this.branchIndex.get(branch) || new Set();
      checkpoints = checkpoints.filter(cp => branchIds.has(cp.id));
    }

    if (tag) {
      const tagIds = this.tagIndex.get(tag) || new Set();
      checkpoints = checkpoints.filter(cp => tagIds.has(cp.id));
    }

    if (after !== undefined) {
      checkpoints = checkpoints.filter(cp => cp.t_ns > after);
    }

    if (before !== undefined) {
      checkpoints = checkpoints.filter(cp => cp.t_ns < before);
    }

    // Sort by time (newest first)
    return checkpoints.sort((a, b) => {
      if (a.t_ns > b.t_ns) return -1;
      if (a.t_ns < b.t_ns) return 1;
      return 0;
    });
  }

  /**
   * Switch to a different branch
   *
   * @param {string} branchName - Branch name
   * @param {Object} [options]
   * @param {boolean} [options.create] - Create branch if doesn't exist
   * @returns {string} Current branch name
   */
  switchBranch(branchName, options = {}) {
    const { create = false } = options;

    if (!this.branchIndex.has(branchName) && !create) {
      throw new Error(`Branch "${branchName}" does not exist. Use create: true to create it.`);
    }

    this.currentBranch = branchName;
    return this.currentBranch;
  }

  /**
   * Get checkpoint by label
   *
   * @param {string} label - Checkpoint label
   * @param {Object} [options]
   * @param {string} [options.branch] - Branch name (default: current)
   * @returns {Object|undefined} Checkpoint or undefined
   */
  getCheckpoint(label, options = {}) {
    const { branch = this.currentBranch } = options;
    const checkpointId = this.labelIndex.get(`${branch}:${label}`);
    return checkpointId ? this.checkpoints.get(checkpointId) : undefined;
  }

  /**
   * Delete checkpoint by label
   *
   * @param {string} label - Checkpoint label
   * @param {Object} [options]
   * @param {string} [options.branch] - Branch name (default: current)
   * @returns {boolean} True if deleted
   */
  deleteCheckpoint(label, options = {}) {
    const { branch = this.currentBranch } = options;
    const checkpointId = this.labelIndex.get(`${branch}:${label}`);

    if (!checkpointId) return false;

    const checkpoint = this.checkpoints.get(checkpointId);

    // Remove from all indexes
    this.checkpoints.delete(checkpointId);
    this.labelIndex.delete(`${branch}:${label}`);

    const branchSet = this.branchIndex.get(branch);
    if (branchSet) branchSet.delete(checkpointId);

    for (const tag of checkpoint.tags) {
      const tagSet = this.tagIndex.get(tag);
      if (tagSet) tagSet.delete(checkpointId);
    }

    return true;
  }

  /**
   * Export checkpoint history for persistence
   *
   * @returns {Object} Serializable checkpoint data
   */
  export() {
    return {
      checkpoints: Array.from(this.checkpoints.entries()).map(([id, cp]) => ({
        ...cp,
        t_ns: cp.t_ns.toString(),
      })),
      currentBranch: this.currentBranch,
    };
  }

  /**
   * Import checkpoint history from persistence
   *
   * @param {Object} data - Exported checkpoint data
   */
  import(data) {
    this.checkpoints.clear();
    this.labelIndex.clear();
    this.branchIndex.clear();
    this.tagIndex.clear();

    for (const cp of data.checkpoints) {
      const checkpoint = {
        ...cp,
        t_ns: BigInt(cp.t_ns),
      };

      this.checkpoints.set(checkpoint.id, checkpoint);
      this.labelIndex.set(`${checkpoint.branch}:${checkpoint.label}`, checkpoint.id);

      if (!this.branchIndex.has(checkpoint.branch)) {
        this.branchIndex.set(checkpoint.branch, new Set());
      }
      this.branchIndex.get(checkpoint.branch).add(checkpoint.id);

      for (const tag of checkpoint.tags) {
        if (!this.tagIndex.has(tag)) {
          this.tagIndex.set(tag, new Set());
        }
        this.tagIndex.get(tag).add(checkpoint.id);
      }
    }

    this.currentBranch = data.currentBranch || 'main';
  }

  // =============================================================================
  // Private Methods
  // =============================================================================

  /**
   * Generate UUID
   * @returns {string}
   * @private
   */
  _generateUUID() {
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
   * Get latest checkpoint ID from set
   * @param {Set<string>} checkpointIds
   * @returns {string|null}
   * @private
   */
  _getLatestCheckpointId(checkpointIds) {
    let latestId = null;
    let latestTime = 0n;

    for (const id of checkpointIds) {
      const cp = this.checkpoints.get(id);
      if (cp && cp.t_ns > latestTime) {
        latestTime = cp.t_ns;
        latestId = cp.id;
      }
    }

    return latestId;
  }

  /**
   * Enforce max checkpoints per branch (LRU eviction)
   * @param {string} branch
   * @private
   */
  _enforceMaxCheckpoints(branch) {
    const branchSet = this.branchIndex.get(branch);
    if (!branchSet || branchSet.size <= this.maxCheckpoints) return;

    // Sort by time (oldest first)
    const checkpoints = Array.from(branchSet)
      .map(id => this.checkpoints.get(id))
      .filter(Boolean)
      .sort((a, b) => {
        if (a.t_ns < b.t_ns) return -1;
        if (a.t_ns > b.t_ns) return 1;
        return 0;
      });

    // Remove oldest
    const toRemove = checkpoints.length - this.maxCheckpoints;
    for (let i = 0; i < toRemove; i++) {
      const cp = checkpoints[i];
      this.deleteCheckpoint(cp.label, { branch });
    }
  }

  /**
   * Parse N-Quads string to quad objects
   * @param {string} nquads
   * @returns {Object[]}
   * @private
   */
  _parseNQuads(nquads) {
    const quads = [];
    const lines = nquads.split('\n').filter(l => l.trim());

    for (const line of lines) {
      const match = line.match(/^(<[^>]+>|_:[^\s]+)\s+(<[^>]+>)\s+(.+?)\s+(<[^>]+>)\s+\.$/);
      if (match) {
        quads.push({
          subject: match[1],
          predicate: match[2],
          object: match[3],
          graph: match[4],
        });
      }
    }

    return quads;
  }

  /**
   * Convert quad to string for comparison
   * @param {Object} quad
   * @returns {string}
   * @private
   */
  _quadToString(quad) {
    return `${quad.subject}|${quad.predicate}|${quad.object}|${quad.graph}`;
  }
}

// =============================================================================
// Factory Functions
// =============================================================================

/**
 * Create a time travel manager
 *
 * @param {Object} options
 * @returns {TimeTravelManager}
 */
export function createTimeTravelManager(options) {
  return new TimeTravelManager(options);
}
