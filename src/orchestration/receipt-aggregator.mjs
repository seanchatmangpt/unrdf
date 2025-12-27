/**
 * @fileoverview Receipt Aggregator - Combine receipts into workflow receipts
 *
 * **Purpose**: Aggregate per-package receipts into comprehensive workflow receipts:
 * - Package-level receipts: individual admission/test/build/validation receipts
 * - Stage-level receipts: all packages for a given stage
 * - Workflow-level receipt: overall decision with full audit trail
 *
 * **Properties**:
 * - Merkle tree aggregation for efficient verification
 * - Hierarchical receipt structure
 * - Full provenance tracking
 * - Rollback trail included
 *
 * @module orchestration/receipt-aggregator
 */

import { z } from 'zod';
import { blake3 } from 'hash-wasm';

/**
 * Package receipt schema
 */
export const PackageReceiptSchema = z.object({
  packageName: z.string(),
  stage: z.string(),
  decision: z.enum(['allow', 'deny']),
  timestamp: z.string().datetime(),
  duration: z.number(),
  hash: z.string(),
  details: z.any().optional()
});

/**
 * Stage receipt schema
 */
export const StageReceiptSchema = z.object({
  stage: z.string(),
  decision: z.enum(['allow', 'deny']),
  timestamp: z.string().datetime(),
  duration: z.number(),
  packageReceipts: z.array(PackageReceiptSchema),
  merkleRoot: z.string(),
  stats: z.object({
    total: z.number(),
    passed: z.number(),
    failed: z.number()
  })
});

/**
 * Workflow receipt schema
 */
export const WorkflowReceiptSchema = z.object({
  workflowId: z.string().uuid(),
  decision: z.enum(['allow', 'deny']),
  timestamp: z.string().datetime(),
  duration: z.number(),
  changedPackages: z.array(z.string()),
  affectedPackages: z.array(z.string()),
  executionOrder: z.array(z.string()),
  stageReceipts: z.array(StageReceiptSchema),
  merkleRoot: z.string(),
  rollbackTrail: z.array(z.object({
    stage: z.string(),
    checkpoint: z.string(),
    state: z.any().optional()
  })).optional(),
  metadata: z.record(z.any()).optional()
});

/**
 * Receipt Aggregator - Combines individual receipts into workflow receipts
 *
 * @class ReceiptAggregator
 *
 * @example
 * const aggregator = new ReceiptAggregator('workflow-123');
 *
 * // Add package receipts for each stage
 * aggregator.addPackageReceipt('admission', '@unrdf/core', 'allow', { ... });
 * aggregator.addPackageReceipt('admission', '@unrdf/utils', 'allow', { ... });
 *
 * // Finalize stage
 * const stageReceipt = await aggregator.finalizeStage('admission');
 *
 * // Finalize workflow
 * const workflowReceipt = await aggregator.finalizeWorkflow({
 *   changedPackages: ['@unrdf/core'],
 *   affectedPackages: ['@unrdf/core', '@unrdf/utils']
 * });
 */
export class ReceiptAggregator {
  /**
   * Create a new receipt aggregator
   *
   * @param {string} [workflowId] - Workflow identifier
   */
  constructor(workflowId = null) {
    /** @type {string} Workflow ID */
    this.workflowId = workflowId || crypto.randomUUID();

    /** @type {Date} Start time */
    this.startTime = new Date();

    /** @type {Map<string, Object[]>} Package receipts by stage */
    this.packageReceiptsByStage = new Map();

    /** @type {Object[]} Finalized stage receipts */
    this.stageReceipts = [];

    /** @type {Object[]} Rollback trail */
    this.rollbackTrail = [];

    /** @type {Object} Metadata */
    this.metadata = {};
  }

  /**
   * Compute hash of data
   *
   * @param {any} data - Data to hash
   * @returns {Promise<string>} BLAKE3 hash
   * @private
   */
  async _hash(data) {
    const json = JSON.stringify(data, Object.keys(data || {}).sort());
    return blake3(json);
  }

  /**
   * Compute merkle root of hashes
   *
   * @param {string[]} hashes - Array of hashes
   * @returns {Promise<string>} Merkle root
   * @private
   */
  async _computeMerkleRoot(hashes) {
    if (hashes.length === 0) {
      return await this._hash({ empty: true });
    }

    if (hashes.length === 1) {
      return hashes[0];
    }

    // Build merkle tree
    let level = [...hashes];

    while (level.length > 1) {
      const nextLevel = [];

      for (let i = 0; i < level.length; i += 2) {
        const left = level[i];
        const right = level[i + 1] || left; // Duplicate if odd
        const combined = await this._hash({ left, right });
        nextLevel.push(combined);
      }

      level = nextLevel;
    }

    return level[0];
  }

  /**
   * Add a package receipt for a stage
   *
   * @param {string} stage - Stage name
   * @param {string} packageName - Package name
   * @param {'allow'|'deny'} decision - Receipt decision
   * @param {Object} [details] - Additional details
   * @returns {Promise<Object>} Package receipt
   */
  async addPackageReceipt(stage, packageName, decision, details = {}) {
    const timestamp = new Date();
    const duration = details.duration || 0;

    const receipt = {
      packageName,
      stage,
      decision,
      timestamp: timestamp.toISOString(),
      duration,
      hash: await this._hash({ packageName, stage, decision, timestamp: timestamp.toISOString() }),
      details
    };

    // Validate
    PackageReceiptSchema.parse(receipt);

    // Store
    if (!this.packageReceiptsByStage.has(stage)) {
      this.packageReceiptsByStage.set(stage, []);
    }
    this.packageReceiptsByStage.get(stage).push(receipt);

    return receipt;
  }

  /**
   * Finalize a stage and create stage receipt
   *
   * @param {string} stage - Stage name
   * @returns {Promise<Object>} Stage receipt
   */
  async finalizeStage(stage) {
    const packageReceipts = this.packageReceiptsByStage.get(stage) || [];

    if (packageReceipts.length === 0) {
      throw new Error(`No package receipts for stage: ${stage}`);
    }

    const timestamp = new Date();
    const hashes = packageReceipts.map(r => r.hash);
    const merkleRoot = await this._computeMerkleRoot(hashes);

    const passed = packageReceipts.filter(r => r.decision === 'allow').length;
    const failed = packageReceipts.filter(r => r.decision === 'deny').length;

    // Stage passes only if all packages pass
    const decision = failed === 0 ? 'allow' : 'deny';

    const totalDuration = packageReceipts.reduce((sum, r) => sum + r.duration, 0);

    const stageReceipt = {
      stage,
      decision,
      timestamp: timestamp.toISOString(),
      duration: totalDuration,
      packageReceipts,
      merkleRoot,
      stats: {
        total: packageReceipts.length,
        passed,
        failed
      }
    };

    // Validate
    StageReceiptSchema.parse(stageReceipt);

    // Store
    this.stageReceipts.push(stageReceipt);

    return stageReceipt;
  }

  /**
   * Add rollback trail entry
   *
   * @param {string} stage - Stage name
   * @param {string} checkpoint - Checkpoint ID
   * @param {any} [state] - Restored state
   */
  addRollbackTrail(stage, checkpoint, state = null) {
    this.rollbackTrail.push({
      stage,
      checkpoint,
      state
    });
  }

  /**
   * Set workflow metadata
   *
   * @param {Object} metadata - Metadata object
   */
  setMetadata(metadata) {
    this.metadata = { ...this.metadata, ...metadata };
  }

  /**
   * Finalize workflow and create workflow receipt
   *
   * @param {Object} options - Workflow options
   * @param {string[]} options.changedPackages - Packages that were changed
   * @param {string[]} options.affectedPackages - All affected packages
   * @param {string[]} [options.executionOrder] - Execution order
   * @returns {Promise<Object>} Workflow receipt
   */
  async finalizeWorkflow(options) {
    const endTime = new Date();
    const duration = endTime - this.startTime;

    // Compute stage merkle root
    const stageHashes = [];
    for (const stage of this.stageReceipts) {
      stageHashes.push(stage.merkleRoot);
    }
    const merkleRoot = await this._computeMerkleRoot(stageHashes);

    // Workflow passes only if all stages pass
    const failed = this.stageReceipts.filter(s => s.decision === 'deny').length;
    const decision = failed === 0 ? 'allow' : 'deny';

    const workflowReceipt = {
      workflowId: this.workflowId,
      decision,
      timestamp: endTime.toISOString(),
      duration,
      changedPackages: options.changedPackages,
      affectedPackages: options.affectedPackages,
      executionOrder: options.executionOrder || options.affectedPackages,
      stageReceipts: this.stageReceipts,
      merkleRoot,
      rollbackTrail: this.rollbackTrail.length > 0 ? this.rollbackTrail : undefined,
      metadata: Object.keys(this.metadata).length > 0 ? this.metadata : undefined
    };

    // Validate
    WorkflowReceiptSchema.parse(workflowReceipt);

    return workflowReceipt;
  }

  /**
   * Get current stage receipts
   *
   * @returns {Object[]}
   */
  getStageReceipts() {
    return [...this.stageReceipts];
  }

  /**
   * Get package receipts for a stage
   *
   * @param {string} stage - Stage name
   * @returns {Object[]}
   */
  getPackageReceipts(stage) {
    return [...(this.packageReceiptsByStage.get(stage) || [])];
  }

  /**
   * Get summary of current state
   *
   * @returns {Object}
   */
  getSummary() {
    const stageStats = this.stageReceipts.map(s => ({
      stage: s.stage,
      decision: s.decision,
      packageCount: s.stats.total
    }));

    return {
      workflowId: this.workflowId,
      startTime: this.startTime.toISOString(),
      stagesCompleted: this.stageReceipts.length,
      stageStats,
      hasFailures: this.stageReceipts.some(s => s.decision === 'deny'),
      rollbackTrailLength: this.rollbackTrail.length
    };
  }

  /**
   * Verify a workflow receipt
   *
   * @param {Object} receipt - Workflow receipt to verify
   * @returns {Promise<Object>} Verification result
   */
  async verify(receipt) {
    const errors = [];

    // Verify stage merkle roots
    for (const stageReceipt of receipt.stageReceipts) {
      const hashes = stageReceipt.packageReceipts.map(r => r.hash);
      const computedRoot = await this._computeMerkleRoot(hashes);

      if (computedRoot !== stageReceipt.merkleRoot) {
        errors.push({
          type: 'stage_merkle_mismatch',
          stage: stageReceipt.stage,
          expected: stageReceipt.merkleRoot,
          computed: computedRoot
        });
      }
    }

    // Verify workflow merkle root
    const stageHashes = receipt.stageReceipts.map(s => s.merkleRoot);
    const computedWorkflowRoot = await this._computeMerkleRoot(stageHashes);

    if (computedWorkflowRoot !== receipt.merkleRoot) {
      errors.push({
        type: 'workflow_merkle_mismatch',
        expected: receipt.merkleRoot,
        computed: computedWorkflowRoot
      });
    }

    // Verify decision consistency
    const hasFailedStage = receipt.stageReceipts.some(s => s.decision === 'deny');
    if (hasFailedStage && receipt.decision === 'allow') {
      errors.push({
        type: 'decision_inconsistency',
        message: 'Workflow allowed despite failed stages'
      });
    }

    return {
      valid: errors.length === 0,
      errors,
      verifiedAt: new Date().toISOString()
    };
  }

  /**
   * Export receipt to JSON-LD format
   *
   * @param {Object} receipt - Workflow receipt
   * @returns {Object} JSON-LD representation
   */
  toJSONLD(receipt) {
    return {
      '@context': {
        unrdf: 'https://unrdf.org/vocab#',
        prov: 'http://www.w3.org/ns/prov#',
        xsd: 'http://www.w3.org/2001/XMLSchema#'
      },
      '@type': 'unrdf:WorkflowReceipt',
      '@id': `urn:workflow:${receipt.workflowId}`,
      'unrdf:decision': receipt.decision,
      'prov:generatedAtTime': {
        '@type': 'xsd:dateTime',
        '@value': receipt.timestamp
      },
      'unrdf:duration': receipt.duration,
      'unrdf:changedPackages': receipt.changedPackages,
      'unrdf:affectedPackages': receipt.affectedPackages,
      'unrdf:merkleRoot': receipt.merkleRoot,
      'unrdf:stages': receipt.stageReceipts.map(s => ({
        '@type': 'unrdf:StageReceipt',
        'unrdf:stage': s.stage,
        'unrdf:decision': s.decision,
        'unrdf:merkleRoot': s.merkleRoot,
        'unrdf:packageCount': s.stats.total
      }))
    };
  }

  /**
   * Create receipt aggregator for a workflow
   *
   * @param {string} [workflowId] - Workflow ID
   * @returns {ReceiptAggregator}
   */
  static create(workflowId = null) {
    return new ReceiptAggregator(workflowId);
  }
}

/**
 * Create a receipt aggregator
 *
 * @param {string} [workflowId] - Workflow ID
 * @returns {ReceiptAggregator}
 */
export function createReceiptAggregator(workflowId = null) {
  return new ReceiptAggregator(workflowId);
}
