/**
 * @file Zod schemas for knowledge hook validation - Main export file
 * @module schemas
 *
 * @description
 * Comprehensive Zod schemas for validating all knowledge hook components
 * including hook definitions, conditions, events, and execution results.
 */

// Re-export hook schemas
export {
  HookMetaSchema,
  FileRefSchema,
  SparqlAskConditionSchema,
  SparqlSelectConditionSchema,
  ShaclConditionSchema,
  DeltaConditionSchema,
  ThresholdConditionSchema,
  CountConditionSchema,
  WindowConditionSchema,
  ConditionSchema,
  DeterminismSchema,
  ReceiptSchema,
  HookContextSchema,
  HookEventSchema,
  HookResultSchema,
  HookChannelSchema,
  KnowledgeHookSchema,
} from './hook-schemas.mjs';

// Re-export config schemas
export {
  ObservabilityConfigSchema,
  PerformanceMetricsSchema,
  ManagerConfigSchema,
  FileResolverConfigSchema,
  ConditionEvaluatorConfigSchema,
  HookExecutorConfigSchema,
} from './config-schemas.mjs';

// Re-export transaction schemas
export {
  TransactionDeltaSchema,
  TransactionReceiptSchema,
  QuadSchema,
  DeltaSchema,
  TransactionHookSchema,
  TransactionHookResultSchema,
  HashSchema,
  TransactionReceiptSchemaNew,
  TransactionOptionsSchema,
  ManagerOptionsSchema,
} from './transaction-schemas.mjs';

// Re-export advanced schemas
export {
  QueryPlanSchema,
  IndexSchema,
  DeltaAwareContextSchema,
  AgentProposalSchema,
  ResolutionStrategySchema,
  ResolutionResultSchema,
  SandboxConfigSchema,
  SandboxContextSchema,
  SandboxResultSchema,
  LockchainEntrySchema,
  LockchainConfigSchema,
  PolicyPackMetaSchema,
  PolicyPackConfigSchema,
  PolicyPackManifestSchema,
} from './advanced-schemas.mjs';

// Re-export validation functions
export {
  validateKnowledgeHook,
  validateHookEvent,
  validateCondition,
  validateManagerConfig,
  validateTransactionDelta,
  createKnowledgeHook,
  createHookEvent,
  createCondition,
} from './validation-functions.mjs';
