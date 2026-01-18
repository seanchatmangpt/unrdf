# API Alignment Report

**Generated:** 2025-12-25T23:33:08.606Z
**Checker:** validation/api-alignment-check.mjs

## Methodology

This report follows **Adversarial PM** principles from CLAUDE.md:
- ✅ RAN actual export extraction from index.mjs files
- ✅ RAN documentation parsing from API-REFERENCE.md
- ✅ COMPARED with evidence (not assumptions)
- ✅ MEASURED coverage metrics (not claims)

## Executive Summary

**Overall Critical API Coverage:** 36/43 (83.7%)

| Package | Total Exports | Documented | Coverage | Critical APIs | Critical Documented | Critical Coverage |
|---------|---------------|------------|----------|---------------|---------------------|-------------------|
| @unrdf/core | 76 | 13 | 17.1% | 14 | 13 | 92.9% |
| @unrdf/kgc-4d | 64 | 4 | 6.3% | 9 | 4 | 44.4% |
| @unrdf/hooks | 62 | 12 | 19.4% | 10 | 9 | 90.0% |
| @unrdf/yawl | 309 | 10 | 3.2% | 10 | 10 | 100.0% |

## Detailed Findings by Package

### @unrdf/core

**Total Exports:** 76
**Documented:** 13 (17.1%)

#### ❌ CRITICAL - Missing from Documentation (80/20 violation)

These APIs are in the top 20% (used 80% of the time) but NOT documented:

- `createStore`

#### ✅ Critical APIs Documented

- `COMMON_PREFIXES`
- `DCTERMS`
- `FOAF`
- `OWL`
- `RDF`
- `RDFS`
- `XSD`
- `executeAskSync`
- `executeConstructSync`
- `executeSelectSync`
- `literal`
- `namedNode`
- `quad`

#### ⚠️ Documented but NOT Exported

These are in docs but do not exist in actual exports (possible errors):

- `createOxiStore`
- `readFileSync`

#### ℹ️ Other Undocumented Exports (62 total)

These are exported but not in the critical 80/20 list. This is OK if they are advanced/internal APIs:

<details>
<summary>Click to expand full list</summary>

- ``
- `CircuitBreaker`
- `ConfigError`
- `DebugLogger`
- `ERROR_CODES`
- `NetworkError`
- `ParserError`
- `PerformanceTracker`
- `QuadSchema`
- `QueryError`
- `QueryOptionsSchema`
- `RateLimiter`
- `SKOS`
- `StoreError`
- `StoreSchema`
- `TimeoutError`
- `UnrdfError`
- `UnrdfStore`
- `ValidationError`
- `addQuad`
- `assertError`
- `blankNode`
- `bulkOperation`
- `canonicalize`
- `countQuads`
- `createBlankNode`
- `createDebugger`
- `createError`
- `createLiteral`
- `createNamedNode`
- `createQuad`
- `createTerms`
- `createUnrdfStore`
- `createVariable`
- `defaultGraph`
- `dumpDebugSnapshot`
- `executeAsk`
- `executeConstruct`
- `executeQuery`
- `executeQuerySync`
- `executeSelect`
- `fallback`
- `formatBytes`
- `getQuads`
- `getSystemInfo`
- `isIsomorphic`
- `iterateQuads`
- `perfTracker`
- `prepareQuery`
- `prepareQuerySync`
- `removeQuad`
- `retry`
- `sortQuads`
- `toNTriples`
- `trace`
- `traceMethod`
- `validateQuad`
- `validateStore`
- `variable`
- `withRecovery`
- `withTimeout`
- `wrapError`

</details>

### @unrdf/kgc-4d

**Total Exports:** 64
**Documented:** 4 (6.3%)

#### ❌ CRITICAL - Missing from Documentation (80/20 violation)

These APIs are in the top 20% (used 80% of the time) but NOT documented:

- `EVENT_TYPES`
- `GRAPHS`
- `fromISO`
- `now`
- `toISO`

#### ✅ Critical APIs Documented

- `KGCStore`
- `freezeUniverse`
- `reconstructState`
- `verifyReceipt`

#### ℹ️ Other Undocumented Exports (55 total)

These are exported but not in the critical 80/20 list. This is OK if they are advanced/internal APIs:

<details>
<summary>Click to expand full list</summary>

- ``
- `// Constants
  D_BROWSER`
- `// Coordinate generation
  coordsForEvent`
- `// Distance and similarity
  cosineSimilarity`
- `// Guards and validation
  guardDimension`
- `// Visualization and projection
  projectPCA`
- `D_DEFAULT`
- `D_HEAVY`
- `D_LIGHT`
- `D_MEDIUM`
- `D_NODE_MAX`
- `DeltaSyncActions`
- `DeltaSyncState`
- `EVENT_TYPE_ENCODING`
- `GitBackbone`
- `HookRegistry`
- `LATENCY_BUDGET_MS`
- `N_BROWSER_MAX`
- `ONTOLOGY_AXES`
- `ONTOLOGY_DIM`
- `OPS_BUDGET`
- `PREDICATES`
- `SSEClient`
- `VectorClock`
- `addNanoseconds`
- `batchCoordsForEvents`
- `calculateCentroid`
- `calculateMemoryFootprint`
- `calculateOpsPerQuery`
- `clusterProjection`
- `cosineDistance`
- `createDeltaSyncReducer`
- `createUniverseContext`
- `createVisualizationData`
- `dotProduct`
- `duration`
- `euclideanDistance`
- `euclideanDistanceSquared`
- `findKNearest`
- `findWithinThreshold`
- `guardAll`
- `guardCoordinates`
- `guardEntityCount`
- `guardLatency`
- `guardMemory`
- `guardedOperation`
- `hasClockJumpDetected`
- `manhattanDistance`
- `normalize`
- `normalizeProjection`
- `pairwiseDistances`
- `projectRandom`
- `resetClockJumpDetection`
- `suggestDimension`
- `validateDimension`

</details>

### @unrdf/hooks

**Total Exports:** 62
**Documented:** 12 (19.4%)

#### ❌ CRITICAL - Missing from Documentation (80/20 violation)

These APIs are in the top 20% (used 80% of the time) but NOT documented:

- `unregisterHook`

#### ✅ Critical APIs Documented

- `defineHook`
- `executeHook`
- `executeHookChain`
- `normalizeLanguageTag`
- `registerHook`
- `rejectBlankNodes`
- `trimLiterals`
- `validatePredicateIRI`
- `validateSubjectIRI`

#### ⚠️ Documented but NOT Exported

These are in docs but do not exist in actual exports (possible errors):

- `executeBatch`
- `parseInt`

#### ℹ️ Other Undocumented Exports (49 total)

These are exported but not in the critical 80/20 list. This is OK if they are advanced/internal APIs:

<details>
<summary>Click to expand full list</summary>

- ``
- `// Batch API (high-performance bulk operations)
  executeBatch`
- `// Cache management
  clearHookCaches`
- `// Pooled variants (zero-allocation transforms)
  normalizeLanguageTagPooled`
- `// Validation-only execution
  validateOnly`
- `ChainResultSchema`
- `HookConfigSchema`
- `HookRegistrySchema`
- `HookResultSchema`
- `HookScheduler`
- `HookSchema`
- `HookTriggerSchema`
- `KnowledgeHookManager`
- `QuadPool`
- `QualityGateSchema`
- `QualityMetricsCollector`
- `SPCDataPointSchema`
- `ScheduleConfigSchema`
- `builtinHooks`
- `clearCompiledChainCache`
- `clearHooks`
- `compileHookChain`
- `compileValidationOnlyChain`
- `createHookRegistry`
- `createHookScheduler`
- `createQualityHooks`
- `executeHooksByTrigger`
- `getChainKey`
- `getCompilerStats`
- `getHook`
- `getHookMetadata`
- `getHooksByTrigger`
- `getRegistryStats`
- `hasHook`
- `hasTransformation`
- `hasValidation`
- `isJitAvailable`
- `isPooledQuad`
- `isValidHook`
- `listHooks`
- `normalizeNamespace`
- `standardValidation`
- `transformBatch`
- `trimLiteralsPooled`
- `validateBatch`
- `validateIRIFormat`
- `validateLanguageTag`
- `validateObjectLiteral`
- `wouldPassHooks`

</details>

### @unrdf/yawl

**Total Exports:** 309
**Documented:** 10 (3.2%)

#### ✅ Critical APIs Documented

- `cancelWorkItem`
- `completeTask`
- `createCase`
- `createWorkflow`
- `enableTask`
- `exclusiveChoice`
- `parallelSplit`
- `replayCase`
- `startTask`
- `synchronization`

#### ⚠️ Documented but NOT Exported

These are in docs but do not exist in actual exports (possible errors):

- `KGCStore`
- `createOxiStore`
- `createPooledTransform`
- `defineHook`
- `executeBatch`
- `executeHook`
- `executeHookChain`
- `executeSelect`
- `executeSelectSync`
- `freezeUniverse`
- `reconstructState`

#### ℹ️ Other Undocumented Exports (299 total)

These are exported but not in the critical 80/20 list. This is OK if they are advanced/internal APIs:

<details>
<summary>Click to expand full list</summary>

- ``
- `// Case operations
  addCase`
- `// Case status values
  Case_Active`
- `// Classes
  WorkflowCase`
- `// Constants
  CASE_STATUSES`
- `// Core classes
  YawlResourceManager`
- `// Core entity schemas
  CaseSchema`
- `// Core event functions
  appendWorkflowEvent`
- `// Enumeration schemas
  CaseStatusSchema`
- `// Factory functions
  createResourceManager`
- `// Factory functions
  createTaskDefinition`
- `// Hook Creators
  createTaskEnablementHook`
- `// Legacy Receipt API
  YawlReceipt`
- `// Legacy alias
  // Factory function
  createWorkflow`
- `// Legacy alias
  YawlEngine`
- `// Literal factories
  dateTimeLiteral`
- `// Main Receipt API
  generateReceipt`
- `// Metadata
  ONTOLOGY_VERSION`
- `// Namespaces
  YAWL`
- `// Pattern builders
  sequence`
- `// Pattern registry and application
  PATTERNS`
- `// Pattern utilities
  isSplitPattern`
- `// Pattern validation
  validatePattern`
- `// Policy Pack Builder (main entry point)
  createYAWLPolicyPack`
- `// Primary exports
  WorkflowEngine`
- `// Properties
  specId`
- `// Query utilities
  SPARQL_QUERIES`
- `// RDF integration
  workflowToRDF`
- `// RDF vocabulary terms
  rdfType`
- `// Resource types
  ResourceType`
- `// SPARQL Query Generators
  generateEnablementQuery`
- `// SPARQL helpers
  RESOURCE_SPARQL_PREFIXES`
- `// SPARQL query execution
  executeSparqlSelect`
- `// Schemas
  ResourceSchema`
- `// Split/Join behaviors
  XOR_Split`
- `// Split/Join types
  SPLIT_TYPE`
- `// Status transition maps
  CASE_STATUS_TRANSITIONS`
- `// Store creation
  createYawlStore`
- `// Supporting schemas
  TaskTimerSchema`
- `// Task classes
  YawlTask`
- `// Task status
  TaskStatus`
- `// Transition schemas
  CaseTransitionSchema`
- `// Type guards
  isCaseStatus`
- `// URI factories
  caseUri`
- `// Utility functions
  getStoreStats`
- `// Validation functions
  validateCase`
- `// Work item operations
  addWorkItem`
- `// Work item status values
  WorkItem_Enabled`
- `// Workflow class
  Workflow`
- `// Workflow specification operations
  addWorkflowSpec`
- `// Zod schemas
  TaskDefinitionSchema`
- `AND_Join`
- `AND_Split`
- `AllocationReceiptSchema`
- `AtomicTask`
- `AutomatedTask`
- `CONTROL_FLOW_PATTERNS`
- `CaseOptionsSchema`
- `Case_Cancelled`
- `Case_Completed`
- `Case_Failed`
- `Case_Suspended`
- `CompositeTask`
- `Condition`
- `ConditionSchema`
- `ControlFlowCoreSchema`
- `ControlFlowSchema`
- `CryptoReceiptSchema`
- `DataVariableSchema`
- `ENGINE_EVENTS`
- `ENGINE_NS`
- `EVENTS_YAWL_EVENT_TYPES`
- `EVENTS_YAWL_NS`
- `EnableTaskOptionsSchema`
- `FOAF`
- `FOAF_NS`
- `Flow`
- `FlowDefSchema`
- `HealthStatus`
- `HookReceiptSchema`
- `JOIN_TYPE`
- `JOIN_TYPES`
- `JoinTypeSchema`
- `ManualTask`
- `MultipleInstanceConfigSchema`
- `MultipleInstanceTask`
- `NAMESPACES`
- `NoCyclesSchema`
- `ONTOLOGY_DESCRIPTION`
- `OR_Join`
- `OR_Split`
- `PatternContextSchema`
- `PatternFlowDefSchema`
- `PatternJoinTypeSchema`
- `PatternResultSchema`
- `PatternSplitTypeSchema`
- `PatternTaskDefSchema`
- `PolicyPackSchema`
- `ProofChain`
- `RDF`
- `RDFS`
- `RDF_NS`
- `RECEIPT_DECISIONS`
- `RECEIPT_EVENT_TYPES`
- `RESOURCE_TYPES`
- `ReceiptDecisionSchema`
- `ReceiptStateSchema`
- `ResourceConstraintSchema`
- `ResourceCoreSchema`
- `ResourceEligibilitySchema`
- `ResourcePool`
- `ResourceTypeSchema`
- `SPARQL_PREFIXES`
- `SPLIT_TYPES`
- `SplitTypeSchema`
- `TASK_KINDS`
- `TIME_NS`
- `Task`
- `TaskDefSchema`
- `TaskDefinition`
- `TaskInstance`
- `TaskInstanceSchema`
- `TaskKindSchema`
- `TaskSchema`
- `TimeWindowSchema`
- `TransitionReceiptSchema`
- `TypeReceiptSchema`
- `VALID_TRANSITIONS`
- `WORKFLOW_API_EVENT_TYPES`
- `WORKFLOW_API_NS`
- `WORK_ITEM_STATUS`
- `WORK_ITEM_STATUSES`
- `WORK_ITEM_STATUS_TRANSITIONS`
- `WorkItem`
- `WorkItemCoreSchema`
- `WorkItemSchema`
- `WorkItemStatusSchema`
- `WorkItemTimerSchema`
- `WorkItemTransitionSchema`
- `WorkItem_Allocated`
- `WorkItem_Cancelled`
- `WorkItem_Completed`
- `WorkItem_Failed`
- `WorkItem_Fired`
- `WorkItem_Started`
- `WorkItem_Suspended`
- `WorkflowControlFlowSchema`
- `WorkflowDefinitionSchema`
- `WorkflowOptionsSchema`
- `WorkflowReceiptSchema`
- `WorkflowResourceSchema`
- `WorkflowSpec`
- `WorkflowSpecSchema`
- `WorkflowTaskSchema`
- `WorkflowWorkItemSchema`
- `XOR_Join`
- `XSD`
- `XSD_NS`
- `YAWLHooksTaskSchema`
- `YAWLHooksWorkflowSchema`
- `YAWL_CASE`
- `YAWL_EVENT_TYPES`
- `YAWL_GRAPHS`
- `YAWL_NS`
- `YAWL_PREDICATES`
- `YAWL_PREFIXES`
- `YAWL_RESOURCE_NS`
- `YAWL_TASK`
- `YAWL_WORK`
- `YawlCase`
- `YawlNetSpecSchema`
- `YawlResourcePool`
- `YawlWorkflow`
- `addTask`
- `applyPattern`
- `arbitraryCycle`
- `bindQuery`
- `booleanLiteral`
- `buildReceipt`
- `buildTransitionReceipt`
- `cancellationSet`
- `caseData`
- `caseGraph`
- `caseRef`
- `clearCase`
- `completeWorkItem`
- `completedAt`
- `completedBy`
- `conditionUri`
- `createCancellationHandler`
- `createCancellationHook`
- `createCapabilityQuery`
- `createCaseEntity`
- `createEventCase`
- `createParticipant`
- `createPatternBuilder`
- `createPolicyPack`
- `createResourceAllocationHook`
- `createResourceAllocationValidator`
- `createResourceEntity`
- `createRole`
- `createRoleMembershipQuery`
- `createTaskCompletionHook`
- `createTaskCompletionRouter`
- `createTaskEnablementValidator`
- `createTaskInstance`
- `createTimeoutHook`
- `createTool`
- `createWorkItemEntity`
- `createWorkflowAPI`
- `createWorkflowCase`
- `createWorkflowEngine`
- `createWorkflowReceipt`
- `createdAt`
- `decomposes`
- `deferredChoice`
- `detectCycles`
- `enableEventTask`
- `enableWorkflowTask`
- `executeSparqlAsk`
- `exportCaseAsTurtle`
- `flowCondition`
- `flowPriority`
- `flowUri`
- `generatePredicateQuery`
- `generateResourceCapacityQuery`
- `getAllPatterns`
- `getCase`
- `getJoinTypeForPattern`
- `getPatternByWPNumber`
- `getSplitTypeForPattern`
- `getWorkflowAuditTrail`
- `hasTasks`
- `inputCondition`
- `integerLiteral`
- `isDefaultFlow`
- `isJoinPattern`
- `isResourceType`
- `isTaskKind`
- `isValidCaseTransition`
- `isValidWorkItemTransition`
- `isWorkItemStatus`
- `joinBehavior`
- `joinsFrom`
- `joinsTo`
- `kind`
- `multiChoice`
- `outputCondition`
- `owner`
- `parentCase`
- `queryEnabledTasks`
- `queryWorkItems`
- `rdfsComment`
- `rdfsLabel`
- `reconstructCase`
- `recordControlFlowEvaluation`
- `simpleMerge`
- `sourceTask`
- `sparqlCheckAndJoinSatisfied`
- `sparqlFindNextTasks`
- `sparqlQueryEnabledTasks`
- `sparqlQueryWorkItems`
- `specUri`
- `splitBehavior`
- `startWorkItem`
- `startedBy`
- `status`
- `stringLiteral`
- `structuredSyncMerge`
- `targetTask`
- `taskId`
- `taskName`
- `taskRef`
- `taskUri`
- `timerExpiry`
- `updateCaseStatus`
- `updateWorkItemStatus`
- `updatedAt`
- `validateCardinality`
- `validateCaseTransition`
- `validateControlFlow`
- `validateNoCycles`
- `validateNoFlowCycles`
- `validatePatternContext`
- `validateReceipt`
- `validateResource`
- `validateResourceEligibility`
- `validateSplitJoinMatch`
- `validateTask`
- `validateWorkItem`
- `validateWorkItemTransition`
- `validateYawlNetSpec`
- `verifyChainLink`
- `verifyReceipt`
- `verifyWorkflowReceipt`
- `workItemData`
- `workItemUri`
- `workItems`
- `workflowFromRDF`

</details>

## Recommendations

### @unrdf/core

Add documentation for these critical APIs:

1. `createStore` - Add usage example, parameters, return type, and use case

### @unrdf/kgc-4d

Add documentation for these critical APIs:

1. `EVENT_TYPES` - Add usage example, parameters, return type, and use case
1. `GRAPHS` - Add usage example, parameters, return type, and use case
1. `fromISO` - Add usage example, parameters, return type, and use case
1. `now` - Add usage example, parameters, return type, and use case
1. `toISO` - Add usage example, parameters, return type, and use case

### @unrdf/hooks

Add documentation for these critical APIs:

1. `unregisterHook` - Add usage example, parameters, return type, and use case

### Overall Assessment: ⚠️ GOOD

Most critical APIs are documented, but some gaps exist. Prioritize documenting the missing critical APIs listed above.

---

**Evidence-based validation complete.** See validation/api-alignment-check.mjs for checker implementation.
