/**
 * Multiple Instance Tracker - WP12 Instance State Management
 *
 * Tracks state of multiple task instances spawned for WP12 pattern.
 * No synchronization - instances complete independently.
 *
 * @module @unrdf/yawl/multiple-instance/instance-tracker
 */

import { z } from 'zod';
import { now } from '@unrdf/kgc-4d';

// =============================================================================
// Zod Schemas
// =============================================================================

/**
 * Instance status enumeration
 * @readonly
 * @enum {string}
 */
export const InstanceStatus = Object.freeze({
  SPAWNED: 'spawned',
  ENABLED: 'enabled',
  ACTIVE: 'active',
  COMPLETED: 'completed',
  FAILED: 'failed',
  CANCELLED: 'cancelled',
});

/**
 * MI metadata schema for task instances
 */
export const MultiInstanceMetadataSchema = z.object({
  /** Parent task ID */
  parentTaskId: z.string().min(1),
  /** Instance index (0-based) */
  instanceIndex: z.number().int().nonnegative(),
  /** Total instance count */
  totalInstances: z.number().int().positive(),
  /** Spawn timestamp */
  spawnedAt: z.bigint(),
  /** Instance-specific input data */
  instanceInputData: z.record(z.unknown()).optional(),
});

/**
 * Instance record schema
 */
export const InstanceRecordSchema = z.object({
  /** Instance task ID */
  instanceId: z.string().min(1),
  /** Current status */
  status: z.enum(['spawned', 'enabled', 'active', 'completed', 'failed', 'cancelled']),
  /** MI metadata */
  metadata: MultiInstanceMetadataSchema,
  /** Created timestamp */
  createdAt: z.bigint(),
  /** Updated timestamp */
  updatedAt: z.bigint(),
  /** Completion timestamp (if terminal) */
  completedAt: z.bigint().optional(),
  /** Output data (if completed) */
  outputData: z.record(z.unknown()).optional(),
  /** Error info (if failed) */
  error: z.string().optional(),
});

/**
 * Aggregate status schema
 */
export const AggregateStatusSchema = z.object({
  /** Parent task ID */
  parentTaskId: z.string().min(1),
  /** Total instances */
  totalInstances: z.number().int().nonnegative(),
  /** Status breakdown */
  statusCounts: z.object({
    spawned: z.number().int().nonnegative(),
    enabled: z.number().int().nonnegative(),
    active: z.number().int().nonnegative(),
    completed: z.number().int().nonnegative(),
    failed: z.number().int().nonnegative(),
    cancelled: z.number().int().nonnegative(),
  }),
  /** All instances completed? */
  allCompleted: z.boolean(),
  /** Any instances failed? */
  anyFailed: z.boolean(),
  /** First instance start time */
  firstSpawnedAt: z.bigint().optional(),
  /** Last instance completion time */
  lastCompletedAt: z.bigint().optional(),
  /** Instance IDs by status */
  instances: z.object({
    spawned: z.array(z.string()),
    enabled: z.array(z.string()),
    active: z.array(z.string()),
    completed: z.array(z.string()),
    failed: z.array(z.string()),
    cancelled: z.array(z.string()),
  }),
});

// =============================================================================
// MultiInstanceTracker Class
// =============================================================================

/**
 * Tracks multiple instance state without synchronization
 *
 * WP12 Pattern: Instances run independently, no barrier required
 */
export class MultiInstanceTracker {
  /**
   * Create a new MultiInstanceTracker
   */
  constructor() {
    /** @type {Map<string, InstanceRecord>} instanceId -> record */
    this.instances = new Map();

    /** @type {Map<string, Set<string>>} parentTaskId -> Set<instanceId> */
    this.parentToInstances = new Map();

    /** @type {Map<string, string>} instanceId -> parentTaskId */
    this.instanceToParent = new Map();
  }

  // ===========================================================================
  // Instance Registration
  // ===========================================================================

  /**
   * Register a new MI instance
   *
   * @param {Object} options - Instance options
   * @param {string} options.instanceId - Instance task ID
   * @param {string} options.parentTaskId - Parent task ID
   * @param {number} options.instanceIndex - Instance index (0-based)
   * @param {number} options.totalInstances - Total instance count
   * @param {Object} [options.instanceInputData] - Instance-specific input
   * @returns {InstanceRecord} Created instance record
   */
  registerInstance(options) {
    const metadata = {
      parentTaskId: options.parentTaskId,
      instanceIndex: options.instanceIndex,
      totalInstances: options.totalInstances,
      spawnedAt: now(),
      instanceInputData: options.instanceInputData,
    };

    const record = {
      instanceId: options.instanceId,
      status: InstanceStatus.SPAWNED,
      metadata,
      createdAt: now(),
      updatedAt: now(),
    };

    const validated = record;

    // Store record
    this.instances.set(options.instanceId, validated);

    // Map parent to instance
    if (!this.parentToInstances.has(options.parentTaskId)) {
      this.parentToInstances.set(options.parentTaskId, new Set());
    }
    this.parentToInstances.get(options.parentTaskId).add(options.instanceId);

    // Map instance to parent
    this.instanceToParent.set(options.instanceId, options.parentTaskId);

    return validated;
  }

  // ===========================================================================
  // Instance Updates
  // ===========================================================================

  /**
   * Update instance status
   *
   * @param {string} instanceId - Instance ID
   * @param {string} status - New status
   * @param {Object} [data] - Additional data
   * @param {Object} [data.outputData] - Output data (for completed)
   * @param {string} [data.error] - Error message (for failed)
   */
  updateInstanceStatus(instanceId, status, data = {}) {
    const record = this.instances.get(instanceId);
    if (!record) {
      throw new Error(`Instance not found: ${instanceId}`);
    }

    // Validate status
    if (!Object.values(InstanceStatus).includes(status)) {
      throw new Error(`Invalid status: ${status}`);
    }

    record.status = status;
    record.updatedAt = now();

    // Set completion time for terminal states
    if ([InstanceStatus.COMPLETED, InstanceStatus.FAILED, InstanceStatus.CANCELLED].includes(status)) {
      record.completedAt = now();
    }

    // Add output data if completed
    if (status === InstanceStatus.COMPLETED && data.outputData) {
      record.outputData = data.outputData;
    }

    // Add error if failed
    if (status === InstanceStatus.FAILED && data.error) {
      record.error = data.error;
    }
  }

  /**
   * Get instance record
   *
   * @param {string} instanceId - Instance ID
   * @returns {InstanceRecord|undefined}
   */
  getInstance(instanceId) {
    return this.instances.get(instanceId);
  }

  /**
   * Get parent task ID for instance
   *
   * @param {string} instanceId - Instance ID
   * @returns {string|undefined}
   */
  getParentTaskId(instanceId) {
    return this.instanceToParent.get(instanceId);
  }

  // ===========================================================================
  // Aggregate Queries
  // ===========================================================================

  /**
   * Get aggregate status for all instances of a parent task
   *
   * @param {string} parentTaskId - Parent task ID
   * @returns {AggregateStatus}
   */
  getAggregateStatus(parentTaskId) {
    const instanceIds = this.parentToInstances.get(parentTaskId);
    if (!instanceIds || instanceIds.size === 0) {
      return AggregateStatusSchema.parse({
        parentTaskId,
        totalInstances: 0,
        statusCounts: {
          spawned: 0,
          enabled: 0,
          active: 0,
          completed: 0,
          failed: 0,
          cancelled: 0,
        },
        allCompleted: true,
        anyFailed: false,
        instances: {
          spawned: [],
          enabled: [],
          active: [],
          completed: [],
          failed: [],
          cancelled: [],
        },
      });
    }

    const counts = {
      spawned: 0,
      enabled: 0,
      active: 0,
      completed: 0,
      failed: 0,
      cancelled: 0,
    };

    const byStatus = {
      spawned: [],
      enabled: [],
      active: [],
      completed: [],
      failed: [],
      cancelled: [],
    };

    let firstSpawnedAt;
    let lastCompletedAt;

    for (const instanceId of instanceIds) {
      const record = this.instances.get(instanceId);
      if (!record) continue;

      const status = record.status;
      counts[status]++;
      byStatus[status].push(instanceId);

      // Track timing
      if (!firstSpawnedAt || record.metadata.spawnedAt < firstSpawnedAt) {
        firstSpawnedAt = record.metadata.spawnedAt;
      }

      if (record.completedAt) {
        if (!lastCompletedAt || record.completedAt > lastCompletedAt) {
          lastCompletedAt = record.completedAt;
        }
      }
    }

    const totalInstances = instanceIds.size;
    const terminalCount = counts.completed + counts.failed + counts.cancelled;
    const allCompleted = terminalCount === totalInstances;
    const anyFailed = counts.failed > 0;

    return {
      parentTaskId,
      totalInstances,
      statusCounts: counts,
      allCompleted,
      anyFailed,
      firstSpawnedAt,
      lastCompletedAt,
      instances: byStatus,
    };
  }

  /**
   * Get all instances for a parent task
   *
   * @param {string} parentTaskId - Parent task ID
   * @returns {InstanceRecord[]}
   */
  getInstancesByParent(parentTaskId) {
    const instanceIds = this.parentToInstances.get(parentTaskId);
    if (!instanceIds) return [];

    return Array.from(instanceIds)
      .map(id => this.instances.get(id))
      .filter(Boolean);
  }

  /**
   * Get instances by status
   *
   * @param {string} parentTaskId - Parent task ID
   * @param {string} status - Status to filter by
   * @returns {InstanceRecord[]}
   */
  getInstancesByStatus(parentTaskId, status) {
    const instances = this.getInstancesByParent(parentTaskId);
    return instances.filter(inst => inst.status === status);
  }

  /**
   * Check if all instances are completed (any terminal state)
   *
   * @param {string} parentTaskId - Parent task ID
   * @returns {boolean}
   */
  areAllInstancesComplete(parentTaskId) {
    const aggregate = this.getAggregateStatus(parentTaskId);
    return aggregate.allCompleted;
  }

  /**
   * Check if any instances failed
   *
   * @param {string} parentTaskId - Parent task ID
   * @returns {boolean}
   */
  hasFailedInstances(parentTaskId) {
    const aggregate = this.getAggregateStatus(parentTaskId);
    return aggregate.anyFailed;
  }

  /**
   * Get completion percentage
   *
   * @param {string} parentTaskId - Parent task ID
   * @returns {number} Percentage (0-100)
   */
  getCompletionPercentage(parentTaskId) {
    const aggregate = this.getAggregateStatus(parentTaskId);
    if (aggregate.totalInstances === 0) return 100;

    const completed = aggregate.statusCounts.completed +
                     aggregate.statusCounts.failed +
                     aggregate.statusCounts.cancelled;

    return (completed / aggregate.totalInstances) * 100;
  }

  // ===========================================================================
  // Cleanup
  // ===========================================================================

  /**
   * Remove all instances for a parent task
   *
   * @param {string} parentTaskId - Parent task ID
   */
  removeInstancesByParent(parentTaskId) {
    const instanceIds = this.parentToInstances.get(parentTaskId);
    if (!instanceIds) return;

    for (const instanceId of instanceIds) {
      this.instances.delete(instanceId);
      this.instanceToParent.delete(instanceId);
    }

    this.parentToInstances.delete(parentTaskId);
  }

  /**
   * Clear all instances
   */
  clear() {
    this.instances.clear();
    this.parentToInstances.clear();
    this.instanceToParent.clear();
  }

  // ===========================================================================
  // Statistics
  // ===========================================================================

  /**
   * Get tracker statistics
   *
   * @returns {Object}
   */
  getStats() {
    return {
      totalInstances: this.instances.size,
      totalParents: this.parentToInstances.size,
      instancesPerParent: this.parentToInstances.size > 0
        ? this.instances.size / this.parentToInstances.size
        : 0,
    };
  }
}

// =============================================================================
// Global Instance Tracker (Singleton)
// =============================================================================

/** Global instance tracker for WP12 */
export const globalInstanceTracker = new MultiInstanceTracker();

// =============================================================================
// Exports
// =============================================================================

export default MultiInstanceTracker;
