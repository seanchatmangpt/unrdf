# YAWL File Splitting Plan

## Overview
Split 5 oversized files (8,374 total lines) into 29 smaller modules (<500 lines each).

## File Splitting Breakdown

### 1. yawl-patterns.test.mjs → 7 test files (1,740 → ~248 lines each)

**packages/yawl/test/patterns/pattern-basic.test.mjs** (Lines 1-501, ~300 lines)
- Test utilities (lines 1-103)
- Van der Aalst WP1-WP7 tests (lines 109-501)
- Imports: `../src/index.mjs`, `../../src/engine.mjs`

**packages/yawl/test/patterns/pattern-controlflow.test.mjs** (Lines 507-664, ~160 lines)
- Control flow tests (cycles, nested conditionals, deferred choice)
- Imports: `../src/index.mjs`, `../../src/engine.mjs`

**packages/yawl/test/patterns/pattern-resources.test.mjs** (Lines 670-827, ~160 lines)
- Resource allocation and pool tests
- Imports: `../src/index.mjs`, `../../src/engine.mjs`, `../../src/resource.mjs`

**packages/yawl/test/patterns/pattern-cancellation.test.mjs** (Lines 833-999, ~170 lines)
- Cancellation tests (single, region, timeout, circuit breaker)
- Imports: `../src/index.mjs`, `../../src/engine.mjs`

**packages/yawl/test/patterns/pattern-timetravel.test.mjs** (Lines 1005-1178, ~175 lines)
- Time-travel and replay tests
- Imports: `../src/index.mjs`, `../../src/engine.mjs`

**packages/yawl/test/patterns/pattern-receipts.test.mjs** (Lines 1184-1320, ~140 lines)
- Receipt verification tests
- Imports: `../src/index.mjs`, `../../src/engine.mjs`

**packages/yawl/test/patterns/pattern-integration.test.mjs** (Lines 1326-1740, ~415 lines)
- Integration tests + WP8-WP20
- Imports: `../src/index.mjs`, `../../src/engine.mjs`

**Strategy**: Create test/patterns/ directory, move test file there, create index that runs all

---

### 2. workflow-api.mjs → 4 modules (1,709 → ~427 lines each)

**packages/yawl/src/api/workflow-api-core.mjs** (~450 lines)
- Lines 1-213: Namespace constants, Zod schemas
- Lines 218-300: Utility functions (generateId, now, createHash, createReceipt)
- Lines 306-477: createWorkflow function
- Lines 479-637: createCase function
- Exports: schemas, createWorkflow, createCase, constants

**packages/yawl/src/api/workflow-api-tasks.mjs** (~380 lines)
- Lines 639-750: enableTask function
- Lines 752-850: startTask function
- Lines 852-941: completeTask function
- Lines 943-1061: cancelWorkItem function
- Exports: enableTask, startTask, completeTask, cancelWorkItem
- Imports: ./workflow-api-core.mjs (schemas, utilities)

**packages/yawl/src/api/workflow-api-replay.mjs** (~240 lines)
- Lines 1063-1203: replayCase function
- Lines 1544-1677: Helper functions (queryCaseEvents, reconstructWorkItemHistory, determineHistoricalCaseStatus)
- Exports: replayCase
- Imports: ./workflow-api-core.mjs (constants, utilities)

**packages/yawl/src/api/workflow-api-helpers.mjs** (~440 lines)
- Lines 1210-1277: buildControlFlowGraph, findInitialTasks
- Lines 1280-1415: createWorkflowRDF, createCaseRDF
- Lines 1418-1542: evaluateControlFlowAndEnable, evaluateCondition, checkAllPredecessorsComplete
- Exports: helper functions
- Imports: ./workflow-api-core.mjs (constants)

**Index file**: packages/yawl/src/api/workflow-api.mjs (barrel export)

---

### 3. workflow.mjs → 4 modules (1,703 → ~426 lines each)

**packages/yawl/src/workflow/workflow-core.mjs** (~490 lines)
- Lines 1-73: Imports
- Lines 78-161: Zod schemas
- Lines 175-339: Workflow class constructor + initialization
- Lines 344-522: Query methods
- Exports: Workflow class, schemas

**packages/yawl/src/workflow/workflow-validation.mjs** (~380 lines)
- Lines 527-902: All validation methods
- Exports: validation functions
- Imports: ./workflow-core.mjs (Workflow, schemas), ../patterns.mjs

**packages/yawl/src/workflow/workflow-controlflow.mjs** (~200 lines)
- Lines 907-1059: evaluateDownstream, canEnable methods
- Exports: control flow evaluation functions
- Imports: ./workflow-core.mjs, ../patterns.mjs

**packages/yawl/src/workflow/workflow-rdf.mjs** (~430 lines)
- Lines 1064-1174: Serialization (mutation methods for compatibility)
- Lines 1242-1295: Factory function
- Lines 1299-1667: workflowToRDF, workflowFromRDF
- Exports: createWorkflow, workflowToRDF, workflowFromRDF
- Imports: ./workflow-core.mjs, ./workflow-validation.mjs, ../ontology/yawl-ontology.mjs

**Index file**: packages/yawl/src/workflow.mjs (re-export Workflow, createWorkflow, etc.)

---

### 4. engine.mjs → 4 modules (1,653 → ~413 lines each)

**packages/yawl/src/engine/engine-core.mjs** (~450 lines)
- Lines 1-115: Imports, constants, config schema
- Lines 120-255: WorkflowEngine constructor
- Lines 260-336: Workflow management
- Lines 341-361: Policy pack integration
- Lines 366-500: Case management
- Exports: WorkflowEngine class (partial), constants

**packages/yawl/src/engine/engine-execution.mjs** (~395 lines)
- Lines 505-877: Task execution (enableTask, startTask, completeTask, cancelTask, cancelRegion)
- Lines 882-976: Circuit breaker and timeout
- Exports: task execution methods (to be added to WorkflowEngine.prototype)
- Imports: ./engine-core.mjs, ../task.mjs, ../receipt.mjs

**packages/yawl/src/engine/engine-timetravel.mjs** (~290 lines)
- Lines 981-1167: Time-travel methods (checkpoint, replayCase, getCaseHistory, replayToReceipt)
- Lines 1172-1197: Resource management
- Exports: time-travel methods
- Imports: ./engine-core.mjs, ./events/yawl-events.mjs

**packages/yawl/src/engine/engine-system.mjs** (~420 lines)
- Lines 1202-1270: Event subscription
- Lines 1275-1351: Health & statistics
- Lines 1356-1551: Internal methods (event log, KGC-4D, circuit breaker, snapshot timer)
- Lines 1556-1579: Serialization and shutdown
- Lines 1584-1644: Factory function
- Exports: createWorkflowEngine, system methods
- Imports: ./engine-core.mjs, ./engine-execution.mjs, ./engine-timetravel.mjs

**Index file**: packages/yawl/src/engine.mjs (re-export WorkflowEngine, createWorkflowEngine, etc.)

---

### 5. yawl-resources.mjs → 4 modules (1,569 → ~392 lines each)

**packages/yawl/src/resources/resources-core.mjs** (~320 lines)
- Lines 1-64: Namespace definitions
- Lines 69-148: Zod schemas
- Lines 155-207: YawlResourceManager constructor
- Lines 212-320: Policy pack management
- Lines 1184-1203: Store access methods
- Exports: YawlResourceManager class (partial), schemas, constants

**packages/yawl/src/resources/resources-allocation.mjs** (~410 lines)
- Lines 365-455: allocateResource method
- Lines 456-605: Allocation helpers (deallocate, capacity check, eligibility)
- Lines 699-809: Resource eligibility and querying
- Exports: allocation methods (to be added to YawlResourceManager.prototype)
- Imports: ./resources-core.mjs

**packages/yawl/src/resources/resources-pools.mjs** (~330 lines)
- Lines 814-977: Availability/calendar methods
- Lines 982-1086: Resource pool creation/management
- Lines 1089-1179: Capacity tracking
- Lines 1208-1359: ResourcePool class
- Exports: pool-related methods, ResourcePool class
- Imports: ./resources-core.mjs

**packages/yawl/src/resources/resources-helpers.mjs** (~210 lines)
- Lines 1364-1499: Factory functions (createResourceManager, createParticipant, createTool, createRole, createPolicyPack)
- Lines 1503-1569: SPARQL query helpers
- Exports: factory functions, SPARQL helpers
- Imports: ./resources-core.mjs

**Index file**: packages/yawl/src/resources/yawl-resources.mjs (re-export all)

---

## Execution Strategy

1. **Phase 1**: Split test file (no dependencies)
   - Create `test/patterns/` directory
   - Create 7 test files
   - Update test suite runner if needed

2. **Phase 2**: Split workflow-api.mjs
   - Create `src/api/` directory structure
   - Create 4 modules + index
   - Update imports in dependent files

3. **Phase 3**: Split workflow.mjs
   - Create `src/workflow/` directory
   - Create 4 modules + index
   - Update imports in dependent files

4. **Phase 4**: Split engine.mjs
   - Create `src/engine/` directory
   - Create 4 modules + index
   - Update imports in dependent files

5. **Phase 5**: Split yawl-resources.mjs
   - Create `src/resources/` directory
   - Create 4 modules + index
   - Move existing to index location

6. **Phase 6**: Update all dependent imports
   - Find all files importing the old paths
   - Update to new module paths

7. **Phase 7**: Verification
   - Run `timeout 10s pnpm test --filter @unrdf/yawl`
   - Check `wc -l packages/yawl/**/*.mjs` for line counts
   - Verify all files <500 lines

## Import Dependency Map

- **workflow-api.mjs** ← imported by: engine.mjs, test files
- **workflow.mjs** ← imported by: engine.mjs, workflow-api.mjs, test files
- **engine.mjs** ← imported by: test files, potentially index.mjs
- **yawl-resources.mjs** ← imported by: engine.mjs, test files
- **yawl-patterns.test.mjs** ← no dependencies (it only imports)

## Success Criteria

- All 29 new files created
- All files <500 lines
- Zero test failures
- Zero import errors
- All exports maintained
- 100% backward compatibility via index barrel exports
