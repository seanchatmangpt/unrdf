/**
 * Execution Branches - Parallel Universe Exploration with Fork/Merge
 *
 * Enables branching execution where multiple alternative states can be explored
 * in parallel, compared, and selectively merged. Inspired by Git branching but
 * for runtime execution state.
 *
 * @module @unrdf/kgc-claude/capabilities/execution-branches
 */

import { blake3 } from 'hash-wasm';
import { dataFactory } from '@unrdf/oxigraph';
import { now, toISO, freezeUniverse, reconstructState } from '@unrdf/kgc-4d';
import { z } from 'zod';

// =============================================================================
// Schemas
// =============================================================================

/**
 * Execution branch schema
 */
export const ExecutionBranchSchema = z.object({
  id: z.string().uuid(),
  name: z.string(),
  description: z.string().optional(),
  parentBranch: z.string().nullable(),
  forkPoint: z.object({
    checkpointId: z.string().uuid(),
    t_ns: z.bigint(),
    timestamp_iso: z.string(),
    snapshotHash: z.string(),
  }),
  head: z.object({
    checkpointId: z.string().uuid(),
    t_ns: z.bigint(),
    timestamp_iso: z.string(),
    snapshotHash: z.string(),
  }),
  status: z.enum(['active', 'merged', 'abandoned']),
  createdAt: z.string(),
  mergedAt: z.string().nullable(),
  metadata: z.record(z.any()).optional(),
});

/**
 * Merge result schema
 */
export const MergeResultSchema = z.object({
  success: z.boolean(),
  targetBranch: z.string(),
  sourceBranch: z.string(),
  strategy: z.enum(['fast-forward', 'three-way', 'ours', 'theirs', 'manual']),
  conflicts: z.array(z.object({
    subject: z.string(),
    predicate: z.string(),
    conflictType: z.enum(['add-add', 'modify-modify', 'delete-modify']),
    ours: z.any(),
    theirs: z.any(),
  })).default([]),
  resolvedConflicts: z.number(),
  mergeCheckpointId: z.string().uuid().optional(),
  timestamp: z.string(),
});

// =============================================================================
// Execution Branch Manager
// =============================================================================

/**
 * ExecutionBranchManager - Manage parallel execution universes
 */
export class ExecutionBranchManager {
  /**
   * @param {Object} options
   * @param {Object} options.store - KGCStore instance
   * @param {Object} options.gitBackbone - GitBackbone instance
   * @param {Object} [options.timeTravelManager] - Time travel manager (optional)
   */
  constructor(options = {}) {
    const { store, gitBackbone, timeTravelManager } = options;

    if (!store) throw new Error('store is required');
    if (!gitBackbone) throw new Error('gitBackbone is required');

    /** @type {Object} */
    this.store = store;

    /** @type {Object} */
    this.gitBackbone = gitBackbone;

    /** @type {Object|null} */
    this.timeTravelManager = timeTravelManager || null;

    /** @type {Map<string, Object>} Branches by name */
    this.branches = new Map();

    /** @type {string} */
    this.currentBranch = 'main';

    // Create main branch
    this._initializeMainBranch();
  }

  /**
   * Fork current branch creating a parallel execution universe
   *
   * @param {string} branchName - New branch name
   * @param {Object} [options]
   * @param {string} [options.description] - Branch description
   * @param {Object} [options.metadata] - Additional metadata
   * @returns {Promise<Object>} New branch
   */
  async forkBranch(branchName, options = {}) {
    const { description = '', metadata = {} } = options;

    if (this.branches.has(branchName)) {
      throw new Error(`Branch "${branchName}" already exists`);
    }

    const parentBranch = this.branches.get(this.currentBranch);
    if (!parentBranch) {
      throw new Error(`Current branch "${this.currentBranch}" not found`);
    }

    // Create checkpoint at fork point
    const freezeResult = await freezeUniverse(this.store, this.gitBackbone);
    const forkCheckpointId = this._generateUUID();
    const t_ns = now();

    const forkPoint = {
      checkpointId: forkCheckpointId,
      t_ns,
      timestamp_iso: toISO(t_ns),
      snapshotHash: freezeResult.universe_hash,
    };

    const branch = ExecutionBranchSchema.parse({
      id: this._generateUUID(),
      name: branchName,
      description,
      parentBranch: this.currentBranch,
      forkPoint,
      head: { ...forkPoint }, // Initially, head = fork point
      status: 'active',
      createdAt: new Date().toISOString(),
      mergedAt: null,
      metadata,
    });

    this.branches.set(branchName, branch);

    return branch;
  }

  /**
   * Switch to a different branch
   *
   * @param {string} branchName - Branch to switch to
   * @returns {Promise<Object>} Switched branch
   */
  async switchBranch(branchName) {
    const branch = this.branches.get(branchName);
    if (!branch) {
      throw new Error(`Branch "${branchName}" not found`);
    }

    if (branch.status !== 'active') {
      throw new Error(`Cannot switch to ${branch.status} branch "${branchName}"`);
    }

    // Restore to branch head
    await reconstructState(this.store, this.gitBackbone, branch.head.t_ns);

    this.currentBranch = branchName;

    return branch;
  }

  /**
   * Advance current branch head (create new checkpoint)
   *
   * @param {Object} [options]
   * @param {string} [options.label] - Checkpoint label
   * @param {Object} [options.metadata] - Checkpoint metadata
   * @returns {Promise<Object>} Updated branch
   */
  async advanceBranch(options = {}) {
    const branch = this.branches.get(this.currentBranch);
    if (!branch) {
      throw new Error(`Current branch "${this.currentBranch}" not found`);
    }

    if (branch.status !== 'active') {
      throw new Error(`Cannot advance ${branch.status} branch`);
    }

    // Create checkpoint at new head
    const freezeResult = await freezeUniverse(this.store, this.gitBackbone);
    const checkpointId = this._generateUUID();
    const t_ns = now();

    branch.head = {
      checkpointId,
      t_ns,
      timestamp_iso: toISO(t_ns),
      snapshotHash: freezeResult.universe_hash,
    };

    return branch;
  }

  /**
   * Merge source branch into target branch
   *
   * @param {string} sourceBranch - Branch to merge from
   * @param {string} targetBranch - Branch to merge into
   * @param {Object} [options]
   * @param {string} [options.strategy] - Merge strategy
   * @param {Function} [options.conflictResolver] - Custom conflict resolver
   * @returns {Promise<Object>} Merge result
   */
  async mergeBranches(sourceBranch, targetBranch, options = {}) {
    const {
      strategy = 'three-way',
      conflictResolver = null,
    } = options;

    const source = this.branches.get(sourceBranch);
    const target = this.branches.get(targetBranch);

    if (!source) throw new Error(`Source branch "${sourceBranch}" not found`);
    if (!target) throw new Error(`Target branch "${targetBranch}" not found`);

    if (source.status !== 'active') {
      throw new Error(`Cannot merge ${source.status} branch "${sourceBranch}"`);
    }

    // Detect if fast-forward is possible
    const isFastForward = source.forkPoint.t_ns === target.head.t_ns;

    if (isFastForward && strategy !== 'manual') {
      // Fast-forward merge: just update target head
      target.head = { ...source.head };

      source.status = 'merged';
      source.mergedAt = new Date().toISOString();

      return MergeResultSchema.parse({
        success: true,
        targetBranch,
        sourceBranch,
        strategy: 'fast-forward',
        conflicts: [],
        resolvedConflicts: 0,
        mergeCheckpointId: source.head.checkpointId,
        timestamp: new Date().toISOString(),
      });
    }

    // Three-way merge
    const baseSnapshot = await this.gitBackbone.readSnapshot(source.forkPoint.snapshotHash);
    const sourceSnapshot = await this.gitBackbone.readSnapshot(source.head.snapshotHash);
    const targetSnapshot = await this.gitBackbone.readSnapshot(target.head.snapshotHash);

    // Parse snapshots
    const baseQuads = this._parseNQuads(baseSnapshot);
    const sourceQuads = this._parseNQuads(sourceSnapshot);
    const targetQuads = this._parseNQuads(targetSnapshot);

    // Create quad sets
    const baseSet = new Set(baseQuads.map(this._quadToString));
    const sourceSet = new Set(sourceQuads.map(this._quadToString));
    const targetSet = new Set(targetQuads.map(this._quadToString));

    // Detect conflicts
    const conflicts = [];

    // Add-add conflicts: both added same subject/predicate with different objects
    const sourceAdded = this._diffQuads(sourceQuads, baseQuads);
    const targetAdded = this._diffQuads(targetQuads, baseQuads);

    const addAddConflicts = this._detectAddAddConflicts(sourceAdded, targetAdded);
    conflicts.push(...addAddConflicts);

    // Modify-modify conflicts: both modified same quad
    const sourceModified = this._getModifiedQuads(baseQuads, sourceQuads);
    const targetModified = this._getModifiedQuads(baseQuads, targetQuads);

    const modifyModifyConflicts = this._detectModifyModifyConflicts(
      sourceModified,
      targetModified
    );
    conflicts.push(...modifyModifyConflicts);

    // Delete-modify conflicts: one deleted, other modified
    const sourceDeleted = this._diffQuads(baseQuads, sourceQuads);
    const targetDeleted = this._diffQuads(baseQuads, targetQuads);

    const deleteModifyConflicts = this._detectDeleteModifyConflicts(
      sourceDeleted,
      targetDeleted,
      sourceModified,
      targetModified
    );
    conflicts.push(...deleteModifyConflicts);

    // Resolve conflicts
    let resolvedConflicts = 0;
    const resolvedQuads = [];

    if (conflicts.length > 0) {
      if (strategy === 'ours') {
        // Keep target version
        resolvedQuads.push(...targetQuads);
        resolvedConflicts = conflicts.length;
      } else if (strategy === 'theirs') {
        // Keep source version
        resolvedQuads.push(...sourceQuads);
        resolvedConflicts = conflicts.length;
      } else if (conflictResolver) {
        // Custom conflict resolution
        for (const conflict of conflicts) {
          const resolution = await conflictResolver(conflict);
          if (resolution) {
            resolvedQuads.push(resolution);
            resolvedConflicts++;
          }
        }
      } else {
        // Manual resolution required
        return MergeResultSchema.parse({
          success: false,
          targetBranch,
          sourceBranch,
          strategy: 'manual',
          conflicts,
          resolvedConflicts: 0,
          timestamp: new Date().toISOString(),
        });
      }
    } else {
      // No conflicts: merge changes
      const mergedQuads = this._mergeQuads(baseQuads, sourceQuads, targetQuads);
      resolvedQuads.push(...mergedQuads);
    }

    // Create merged state
    const mergedStore = await this._createStoreFromQuads(resolvedQuads);

    // Freeze merged state
    const freezeResult = await freezeUniverse(mergedStore, this.gitBackbone);
    const mergeCheckpointId = this._generateUUID();
    const t_ns = now();

    // Update target head
    target.head = {
      checkpointId: mergeCheckpointId,
      t_ns,
      timestamp_iso: toISO(t_ns),
      snapshotHash: freezeResult.universe_hash,
    };

    // Mark source as merged
    source.status = 'merged';
    source.mergedAt = new Date().toISOString();

    return MergeResultSchema.parse({
      success: true,
      targetBranch,
      sourceBranch,
      strategy,
      conflicts,
      resolvedConflicts,
      mergeCheckpointId,
      timestamp: new Date().toISOString(),
    });
  }

  /**
   * Compare two branches
   *
   * @param {string} branch1 - First branch
   * @param {string} branch2 - Second branch
   * @returns {Promise<Object>} Branch comparison
   */
  async compareBranches(branch1, branch2) {
    const b1 = this.branches.get(branch1);
    const b2 = this.branches.get(branch2);

    if (!b1) throw new Error(`Branch "${branch1}" not found`);
    if (!b2) throw new Error(`Branch "${branch2}" not found`);

    const snapshot1 = await this.gitBackbone.readSnapshot(b1.head.snapshotHash);
    const snapshot2 = await this.gitBackbone.readSnapshot(b2.head.snapshotHash);

    const quads1 = this._parseNQuads(snapshot1);
    const quads2 = this._parseNQuads(snapshot2);

    const added = this._diffQuads(quads2, quads1);
    const removed = this._diffQuads(quads1, quads2);

    return {
      branch1,
      branch2,
      divergedAt: this._findCommonAncestor(b1, b2),
      ahead: added.length,
      behind: removed.length,
      identical: added.length === 0 && removed.length === 0,
    };
  }

  /**
   * List all branches
   *
   * @param {Object} [options]
   * @param {string} [options.status] - Filter by status
   * @returns {Object[]} Branches
   */
  listBranches(options = {}) {
    const { status } = options;

    let branches = Array.from(this.branches.values());

    if (status) {
      branches = branches.filter(b => b.status === status);
    }

    return branches;
  }

  /**
   * Delete a branch
   *
   * @param {string} branchName - Branch to delete
   * @param {Object} [options]
   * @param {boolean} [options.force] - Force delete even if not merged
   * @returns {boolean} True if deleted
   */
  deleteBranch(branchName, options = {}) {
    const { force = false } = options;

    if (branchName === 'main') {
      throw new Error('Cannot delete main branch');
    }

    if (branchName === this.currentBranch) {
      throw new Error('Cannot delete current branch. Switch to another branch first.');
    }

    const branch = this.branches.get(branchName);
    if (!branch) return false;

    if (branch.status === 'active' && !force) {
      throw new Error(`Branch "${branchName}" is not merged. Use force: true to delete.`);
    }

    this.branches.delete(branchName);
    return true;
  }

  // =============================================================================
  // Private Methods
  // =============================================================================

  /**
   * Initialize main branch
   * @private
   */
  async _initializeMainBranch() {
    const id = this._generateUUID();
    const t_ns = now();

    const mainBranch = ExecutionBranchSchema.parse({
      id,
      name: 'main',
      description: 'Main execution branch',
      parentBranch: null,
      forkPoint: {
        checkpointId: id,
        t_ns,
        timestamp_iso: toISO(t_ns),
        snapshotHash: '',
      },
      head: {
        checkpointId: id,
        t_ns,
        timestamp_iso: toISO(t_ns),
        snapshotHash: '',
      },
      status: 'active',
      createdAt: new Date().toISOString(),
      mergedAt: null,
      metadata: {},
    });

    this.branches.set('main', mainBranch);
  }

  /**
   * Generate UUID
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
   * Parse N-Quads
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
   * Quad to string
   * @private
   */
  _quadToString(quad) {
    return `${quad.subject}|${quad.predicate}|${quad.object}|${quad.graph}`;
  }

  /**
   * Diff quads
   * @private
   */
  _diffQuads(quads1, quads2) {
    const set2 = new Set(quads2.map(this._quadToString.bind(this)));
    return quads1.filter(q => !set2.has(this._quadToString(q)));
  }

  /**
   * Get modified quads
   * @private
   */
  _getModifiedQuads(baseQuads, currentQuads) {
    const baseMap = new Map();
    for (const quad of baseQuads) {
      const key = `${quad.subject}|${quad.predicate}`;
      baseMap.set(key, quad);
    }

    const modified = [];
    for (const quad of currentQuads) {
      const key = `${quad.subject}|${quad.predicate}`;
      const baseQuad = baseMap.get(key);
      if (baseQuad && baseQuad.object !== quad.object) {
        modified.push({ base: baseQuad, current: quad });
      }
    }

    return modified;
  }

  /**
   * Detect add-add conflicts
   * @private
   */
  _detectAddAddConflicts(sourceAdded, targetAdded) {
    const conflicts = [];
    const targetMap = new Map();

    for (const quad of targetAdded) {
      const key = `${quad.subject}|${quad.predicate}`;
      targetMap.set(key, quad);
    }

    for (const quad of sourceAdded) {
      const key = `${quad.subject}|${quad.predicate}`;
      const targetQuad = targetMap.get(key);
      if (targetQuad && targetQuad.object !== quad.object) {
        conflicts.push({
          subject: quad.subject,
          predicate: quad.predicate,
          conflictType: 'add-add',
          ours: targetQuad,
          theirs: quad,
        });
      }
    }

    return conflicts;
  }

  /**
   * Detect modify-modify conflicts
   * @private
   */
  _detectModifyModifyConflicts(sourceModified, targetModified) {
    const conflicts = [];
    const targetMap = new Map();

    for (const mod of targetModified) {
      const key = `${mod.current.subject}|${mod.current.predicate}`;
      targetMap.set(key, mod);
    }

    for (const mod of sourceModified) {
      const key = `${mod.current.subject}|${mod.current.predicate}`;
      const targetMod = targetMap.get(key);
      if (targetMod && targetMod.current.object !== mod.current.object) {
        conflicts.push({
          subject: mod.current.subject,
          predicate: mod.current.predicate,
          conflictType: 'modify-modify',
          ours: targetMod.current,
          theirs: mod.current,
        });
      }
    }

    return conflicts;
  }

  /**
   * Detect delete-modify conflicts
   * @private
   */
  _detectDeleteModifyConflicts(sourceDeleted, targetDeleted, sourceModified, targetModified) {
    const conflicts = [];

    // Source deleted, target modified
    for (const quad of sourceDeleted) {
      const key = `${quad.subject}|${quad.predicate}`;
      const targetMod = targetModified.find(m =>
        `${m.current.subject}|${m.current.predicate}` === key
      );
      if (targetMod) {
        conflicts.push({
          subject: quad.subject,
          predicate: quad.predicate,
          conflictType: 'delete-modify',
          ours: targetMod.current,
          theirs: null,
        });
      }
    }

    // Target deleted, source modified
    for (const quad of targetDeleted) {
      const key = `${quad.subject}|${quad.predicate}`;
      const sourceMod = sourceModified.find(m =>
        `${m.current.subject}|${m.current.predicate}` === key
      );
      if (sourceMod) {
        conflicts.push({
          subject: quad.subject,
          predicate: quad.predicate,
          conflictType: 'delete-modify',
          ours: null,
          theirs: sourceMod.current,
        });
      }
    }

    return conflicts;
  }

  /**
   * Merge quads (no conflicts)
   * @private
   */
  _mergeQuads(baseQuads, sourceQuads, targetQuads) {
    const baseSet = new Set(baseQuads.map(this._quadToString.bind(this)));
    const sourceSet = new Set(sourceQuads.map(this._quadToString.bind(this)));
    const targetSet = new Set(targetQuads.map(this._quadToString.bind(this)));

    const merged = new Map();

    // Start with base
    for (const quad of baseQuads) {
      const key = this._quadToString(quad);
      merged.set(key, quad);
    }

    // Apply source changes
    for (const quad of sourceQuads) {
      const key = this._quadToString(quad);
      if (!baseSet.has(key)) {
        merged.set(key, quad); // Added in source
      }
    }

    for (const quad of baseQuads) {
      const key = this._quadToString(quad);
      if (!sourceSet.has(key)) {
        merged.delete(key); // Deleted in source
      }
    }

    // Apply target changes
    for (const quad of targetQuads) {
      const key = this._quadToString(quad);
      if (!baseSet.has(key)) {
        merged.set(key, quad); // Added in target
      }
    }

    for (const quad of baseQuads) {
      const key = this._quadToString(quad);
      if (!targetSet.has(key)) {
        merged.delete(key); // Deleted in target
      }
    }

    return Array.from(merged.values());
  }

  /**
   * Create store from quads
   * @private
   */
  async _createStoreFromQuads(quads) {
    const StoreClass = this.store.constructor;
    const newStore = new StoreClass();

    const nquads = quads.map(q => `${q.subject} ${q.predicate} ${q.object} ${q.graph} .`).join('\n');

    await newStore.load(nquads, {
      format: 'application/n-quads',
      graph: 'http://kgc.io/Universe',
    });

    return newStore;
  }

  /**
   * Find common ancestor
   * @private
   */
  _findCommonAncestor(branch1, branch2) {
    // Simplified: return fork point if same parent
    if (branch1.parentBranch === branch2.parentBranch) {
      return branch1.forkPoint.timestamp_iso;
    }
    return null;
  }
}

// =============================================================================
// Factory Functions
// =============================================================================

/**
 * Create execution branch manager
 *
 * @param {Object} options
 * @returns {ExecutionBranchManager}
 */
export function createExecutionBranchManager(options) {
  return new ExecutionBranchManager(options);
}
