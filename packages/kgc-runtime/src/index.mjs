/**
 * KGC Runtime - Main Entry Point
 * Exports admission gate, receipt chain system, work item executor, and merge functionality
 */

export { AdmissionGate } from './admission-gate.mjs';
export { WorkItemExecutor, WORK_ITEM_STATES } from './work-item.mjs';
export {
  shardMerge,
  mergeCapsules,
  ConflictDetector,
  ConflictResolver,
} from './merge.mjs';
