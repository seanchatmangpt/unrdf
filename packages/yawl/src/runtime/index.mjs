/**
 * YAWL Runtime - Minimal MI Infrastructure
 *
 * Exports minimal infrastructure for Multiple Instance pattern execution.
 * Built by COPYING proven patterns from WP12-14 implementations.
 *
 * @module @unrdf/yawl/runtime
 */

// Task Spawner exports
export {
  spawnInstances,
  enableInstance,
  startInstance,
  completeInstance,
  failInstance,
  cancelInstance,
  InstanceStatus,
  SpawnOptionsSchema,
  InstanceRecordSchema,
  SpawnResultSchema,
} from './task-spawner.mjs';

// Instance Pool exports
export {
  InstancePool,
  globalInstancePool,
  executeInstances,
  executeInstancesSequential,
  waitForCompletion,
  getCompletionPercentage,
  AggregateStatusSchema,
} from './instance-pool.mjs';
