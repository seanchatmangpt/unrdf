/**
 * KGC Runtime - Main Entry Point
 * Exports admission gate, receipt chain system, work item executor, merge functionality,
 * custom validators, and enhanced bounds enforcement
 */

export { AdmissionGate } from './admission-gate.mjs';
export { WorkItemExecutor, WORK_ITEM_STATES } from './work-item.mjs';
export {
  shardMerge,
  mergeCapsules,
  ConflictDetector,
  ConflictResolver,
} from './merge.mjs';
export { EnhancedBoundsChecker } from './enhanced-bounds.mjs';
export {
  validateReceiptChainIntegrity,
  validateTemporalConsistency,
  validateArtifactHash,
  validateDependencyDAG,
  validateAsyncPolicy,
  validateTimeRange,
  ReceiptChainSchema,
  TemporallyOrderedSchema,
  ArtifactSchema,
  WorkItemDependencySchema,
  RunCapsuleTimeRangeSchema,
  createAsyncPolicySchema,
  detectCycle,
  combineValidators,
  createValidationResult,
} from './validators.mjs';
export {
  generateReceipt,
  verifyReceiptHash,
  verifyReceiptChain,
  ReceiptStore,
} from './receipt.mjs';
export {
  RunCapsule,
  storeCapsule,
  replayCapsule,
  listCapsules,
} from './capsule.mjs';
export {
  PluginManager,
  PLUGIN_STATES,
  createPluginManager,
} from './plugin-manager.mjs';
export {
  PluginIsolation,
  createPluginIsolation,
  createPublicAPI,
} from './plugin-isolation.mjs';
export {
  APIVersionManager,
  CURRENT_API_VERSION,
  API_STATUS,
  getVersionManager,
  isPluginCompatible,
  validatePluginVersion,
} from './api-version.mjs';
