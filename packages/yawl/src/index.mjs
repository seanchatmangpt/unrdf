/**
 * @unrdf/yawl - YAWL Workflow Engine
 * Implementation of Van der Aalst's workflow patterns with KGC-4D integration
 *
 * @module @unrdf/yawl
 */

// =============================================================================
// YAWL RDF ONTOLOGY - Namespace definitions, classes, and properties
// =============================================================================
export {
  // Namespaces
  YAWL,
  YAWL_CASE,
  YAWL_TASK,
  YAWL_WORK,
  RDF,
  RDFS,
  XSD,
  FOAF,
  NAMESPACES,
  YAWL_PREFIXES,
  SPARQL_PREFIXES,

  // Classes
  WorkflowCase,
  WorkflowSpec,
  Task,
  WorkItem,
  Condition,
  Flow,
  AtomicTask,
  CompositeTask,
  MultipleInstanceTask,
  AutomatedTask,
  ManualTask,

  // Case status values
  Case_Active,
  Case_Completed,
  Case_Suspended,
  Case_Cancelled,
  Case_Failed,

  // Work item status values
  WorkItem_Enabled,
  WorkItem_Fired,
  WorkItem_Allocated,
  WorkItem_Started,
  WorkItem_Completed,
  WorkItem_Suspended,
  WorkItem_Cancelled,
  WorkItem_Failed,

  // Split/Join behaviors
  XOR_Split,
  AND_Split,
  OR_Split,
  XOR_Join,
  AND_Join,
  OR_Join,

  // Properties
  specId,
  status,
  createdAt,
  updatedAt,
  completedAt,
  workItems,
  caseData,
  parentCase,
  taskName,
  taskId,
  kind,
  joinsTo,
  joinsFrom,
  splitBehavior,
  joinBehavior,
  decomposes,
  inputCondition,
  outputCondition,
  hasTasks,
  taskRef,
  caseRef,
  owner,
  startedBy,
  completedBy,
  workItemData,
  timerExpiry,
  flowCondition,
  flowPriority,
  isDefaultFlow,
  sourceTask,
  targetTask,
  cancellationSet,

  // RDF vocabulary terms
  rdfType,
  rdfsLabel,
  rdfsComment,

  // URI factories
  caseUri,
  taskUri,
  workItemUri,
  specUri,
  caseGraph,
  flowUri,
  conditionUri,

  // Literal factories
  dateTimeLiteral,
  integerLiteral,
  booleanLiteral,
  stringLiteral,

  // Query utilities
  SPARQL_QUERIES,
  bindQuery,

  // Metadata
  ONTOLOGY_VERSION,
  ONTOLOGY_DESCRIPTION,
} from './ontology/yawl-ontology.mjs';

// =============================================================================
// YAWL RDF STORE - Store operations and SPARQL queries
// =============================================================================
export {
  // Store creation
  createYawlStore,

  // Case operations
  addCase,
  getCase,
  updateCaseStatus,

  // Work item operations
  addWorkItem,
  queryWorkItems,
  queryEnabledTasks,
  updateWorkItemStatus,

  // Workflow specification operations
  addWorkflowSpec,
  addTask,

  // SPARQL query execution
  executeSparqlSelect,
  executeSparqlAsk,
  sparqlQueryWorkItems,
  sparqlQueryEnabledTasks,
  sparqlCheckAndJoinSatisfied,
  sparqlFindNextTasks,

  // Utility functions
  getStoreStats,
  clearCase,
  exportCaseAsTurtle,
} from './store/yawl-store.mjs';

// =============================================================================
// YAWL WORKFLOW ENGINE
// =============================================================================
export {
  // Primary exports
  WorkflowEngine,
  createWorkflowEngine,
  ENGINE_EVENTS,
  HealthStatus,
  // Engine constants
  YAWL_NS as ENGINE_NS,
  YAWL_GRAPHS,
  // Legacy alias
  YawlEngine,
} from './engine.mjs';
export {
  // Workflow class
  Workflow,
  YawlWorkflow, // Legacy alias
  // Factory function
  createWorkflow,
  // RDF integration
  workflowToRDF,
  workflowFromRDF,
  // Schemas (aliased to avoid conflict with workflow-api exports)
  WorkflowSpecSchema as WorkflowDefinitionSchema,
  TaskDefSchema,
  FlowDefSchema,
} from './workflow.mjs';
export { YawlCase } from './case.mjs';
export {
  // Task classes
  YawlTask,
  TaskInstance,
  TaskDefinition,
  // Task status
  TaskStatus,
  VALID_TRANSITIONS,
  // Zod schemas
  TaskDefinitionSchema,
  TaskInstanceSchema,
  TransitionReceiptSchema,
  // Factory functions
  createTaskDefinition,
  createTaskInstance,
  buildTransitionReceipt,
} from './task.mjs';
export {
  // Main Receipt API
  generateReceipt,
  verifyReceipt,
  verifyChainLink,
  ProofChain,
  RECEIPT_EVENT_TYPES,
  ReceiptSchema as CryptoReceiptSchema,
  // Legacy Receipt API
  YawlReceipt,
  buildReceipt,
} from './receipt.mjs';
export { YawlResourcePool } from './resource.mjs';
export {
  // Pattern builders
  sequence,
  parallelSplit,
  synchronization,
  exclusiveChoice,
  simpleMerge,
  multiChoice,
  structuredSyncMerge,
  arbitraryCycle,
  deferredChoice,
  // Split/Join types
  SPLIT_TYPE,
  JOIN_TYPE,
  // Pattern registry and application
  PATTERNS,
  applyPattern,
  // Pattern validation
  validatePattern,
  validatePatternContext,
  validateCardinality,
  validateSplitJoinMatch,
  detectCycles,
  validateNoCycles as validateNoFlowCycles,
  // Pattern utilities
  isSplitPattern,
  isJoinPattern,
  getSplitTypeForPattern,
  getJoinTypeForPattern,
  getPatternByWPNumber,
  getAllPatterns,
  createPatternBuilder,
  // Pattern schemas
  SplitTypeSchema as PatternSplitTypeSchema,
  JoinTypeSchema as PatternJoinTypeSchema,
  TaskDefSchema as PatternTaskDefSchema,
  FlowDefSchema as PatternFlowDefSchema,
  PatternContextSchema,
  PatternResultSchema,
} from './patterns.mjs';

// Event sourcing with KGC-4D time-travel
export {
  // Event types and constants (aliased to avoid conflict with workflow-api)
  YAWL_EVENT_TYPES as EVENTS_YAWL_EVENT_TYPES,
  YAWL_NS as EVENTS_YAWL_NS,
  YAWL_PREDICATES,

  // Core event functions
  appendWorkflowEvent,
  reconstructCase,
  createWorkflowReceipt,
  verifyWorkflowReceipt,
  getWorkflowAuditTrail,

  // High-level workflow functions (aliased to avoid conflict with workflow-api)
  createCase as createEventCase,
  enableTask as enableEventTask,
  startWorkItem,
  completeWorkItem,
  recordControlFlowEvaluation,
} from './events/yawl-events.mjs';

// YAWL-Hooks Integration Layer
export {
  // Policy Pack Builder (main entry point)
  createYAWLPolicyPack,

  // Hook Creators
  createTaskEnablementHook,
  createTaskEnablementValidator,
  createTaskCompletionHook,
  createTaskCompletionRouter,
  createResourceAllocationHook,
  createResourceAllocationValidator,
  createCancellationHook,
  createTimeoutHook,
  createCancellationHandler,

  // SPARQL Query Generators
  generateEnablementQuery,
  generatePredicateQuery,
  generateResourceCapacityQuery,

  // Zod Schemas for hooks
  YAWLWorkflowSchema as YAWLHooksWorkflowSchema,
  YAWLTaskSchema as YAWLHooksTaskSchema,
  ControlFlowSchema,
  ResourceConstraintSchema,
  HookReceiptSchema,
} from './hooks/yawl-hooks.mjs';

// YAWL Resource Allocation Semantics (Policy Packs + SPARQL Eligibility)
export {
  // Core classes
  YawlResourceManager,
  ResourcePool,

  // Factory functions
  createResourceManager,
  createParticipant,
  createTool,
  createRole,
  createPolicyPack,

  // Resource types
  ResourceType,

  // Schemas
  ResourceSchema,
  WorkItemSchema,
  PolicyPackSchema,
  TimeWindowSchema,
  AllocationReceiptSchema,

  // SPARQL helpers
  RESOURCE_SPARQL_PREFIXES,
  createRoleMembershipQuery,
  createCapabilityQuery,

  // Namespace constants (prefixed to avoid collision with events YAWL_NS)
  YAWL_NS as YAWL_RESOURCE_NS,
  FOAF_NS,
  RDF_NS,
  XSD_NS,
  TIME_NS,
} from './resources/index.mjs';

// YAWL Core Type System (JSDoc + Zod)
export {
  // Status transition maps
  CASE_STATUS_TRANSITIONS,
  WORK_ITEM_STATUS_TRANSITIONS,

  // Type guards
  isCaseStatus,
  isWorkItemStatus,
  isTaskKind,
  isResourceType,
  isValidCaseTransition,
  isValidWorkItemTransition,

  // Constants
  CASE_STATUSES,
  WORK_ITEM_STATUSES,
  TASK_KINDS,
  SPLIT_TYPES,
  JOIN_TYPES,
  RESOURCE_TYPES,
  RECEIPT_DECISIONS,
} from './types/yawl-types.mjs';

export {
  // Enumeration schemas
  CaseStatusSchema,
  WorkItemStatusSchema,
  TaskKindSchema,
  SplitTypeSchema,
  JoinTypeSchema,
  ResourceTypeSchema,
  ReceiptDecisionSchema,

  // Supporting schemas
  TaskTimerSchema,
  MultipleInstanceConfigSchema,
  WorkItemTimerSchema,
  ReceiptStateSchema,
  DataVariableSchema,
  ConditionSchema,

  // Core entity schemas
  CaseSchema,
  TaskSchema,
  WorkItemSchema as WorkItemCoreSchema,
  ControlFlowSchema as ControlFlowCoreSchema,
  ResourceSchema as ResourceCoreSchema,
  ReceiptSchema as TypeReceiptSchema,
  YawlNetSpecSchema,

  // Transition schemas
  CaseTransitionSchema,
  WorkItemTransitionSchema,
  ResourceEligibilitySchema,
  NoCyclesSchema,

  // Validation functions
  validateCase,
  validateTask,
  validateWorkItem,
  validateControlFlow,
  validateResource,
  validateReceipt,
  validateCaseTransition,
  validateWorkItemTransition,
  validateResourceEligibility,
  validateYawlNetSpec,
  validateNoCycles,

  // Factory functions
  createCase as createCaseEntity,
  createWorkItem as createWorkItemEntity,
  createResource as createResourceEntity,
} from './types/yawl-schemas.mjs';

// =============================================================================
// YAWL PUBLIC WORKFLOW API - High-level workflow interface
// =============================================================================
export {
  // Core Workflow API functions (aliased to avoid conflict with workflow.mjs createWorkflow)
  createWorkflow as createWorkflowAPI,
  createCase as createWorkflowCase,
  enableTask as enableWorkflowTask,
  startTask,
  completeTask,
  cancelWorkItem,
  replayCase,

  // Workflow API Constants (aliased versions for namespacing)
  YAWL_NS as WORKFLOW_API_NS,
  YAWL_EVENT_TYPES as WORKFLOW_API_EVENT_TYPES,
  WORK_ITEM_STATUS,
  CONTROL_FLOW_PATTERNS,

  // Workflow API Schemas (aliased versions to avoid conflicts)
  TaskSchema as WorkflowTaskSchema,
  ControlFlowSchema as WorkflowControlFlowSchema,
  ResourceSchema as WorkflowResourceSchema,
  WorkflowSpecSchema,
  WorkflowOptionsSchema,
  CaseOptionsSchema,
  WorkItemSchema as WorkflowWorkItemSchema,
  EnableTaskOptionsSchema,
  ReceiptSchema as WorkflowReceiptSchema,
} from './api/index.mjs';

// Re-export workflow-api WITHOUT aliases for backward compatibility and test compatibility
// These are the primary public API exports that tests expect
export {
  createWorkflow,
  createCase,
  enableTask,
  YAWL_NS,
  YAWL_EVENT_TYPES,
  TaskSchema,
} from './api/index.mjs';

// =============================================================================
// NITRO INTEGRATIONS - Scheduler and Monitor
// =============================================================================
export {
  // Scheduler
  NitroScheduler,
  createScheduler,
  // Monitor
  NitroMonitor,
  createMonitor,
  // Schemas
  CronScheduleSchema,
  DelayedExecutionSchema,
  ScheduleConfigSchema,
  TaskMetricsSchema,
  ResourceMetricsSchema,
  HealthStatusSchema,
  AlertConfigSchema,
  PerformanceMetricsSchema,
  // Validation functions
  validateScheduleConfig,
  validatePerformanceMetrics,
  validateAlertConfig,
} from './integrations/index.mjs';

// =============================================================================
// MULTIPLE INSTANCE PATTERNS - WP13-WP15
// =============================================================================
export {
  // Synchronization Barrier (AND-join)
  SyncBarrier,
  createSyncBarrier,
  BarrierConfigSchema,
  InstanceCompletionSchema,
  BarrierResultSchema,
} from './multiple-instance/sync-barrier.mjs';

export {
  // WP13: Multiple Instances with Design-Time Knowledge
  spawnInstancesDesignTime,
  createMultipleInstanceTask,
  estimateCompletionTime,
  MultipleInstanceTaskSchema,
  SpawnOptionsSchema,
  InstanceResultSchema,
  SpawnResultSchema,
} from './multiple-instance/wp13-design-time.mjs';
