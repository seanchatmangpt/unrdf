# Detailed Refactoring Plan - Top 10 Files

**Status**: READY (waiting for 100% test pass rate)
**Date**: 2025-12-25

---

## File 1: otel-span-builder.mjs (1278 lines → 4 modules)

**Path**: `/home/user/unrdf/packages/validation/src/otel-span-builder.mjs`

### Current Structure Analysis
```bash
# Analyzed via grep
Export count: 14 execution functions + 1 utility function
- createSpanData() - utility
- executeKnowledgeEngine()
- executeCLIParse()
- executeCLIQuery()
- executeCLIValidate()
- executeCLIHook()
- executeTransactionManager()
- executeKnowledgeEngineCore()
- executeKnowledgeHooksAPI()
- executePolicyPacks()
- executeLockchainIntegrity()
- executeBrowserCompatibility()
- executeAtomVMBridge()
- executeAtomVMRuntime()
- executeAtomVMErlang()
```

### Split Strategy (Semantic Grouping)

#### Module 1: `otel-span-builder-core.mjs` (~350 lines)
**Purpose**: Core knowledge engine and CLI execution
```javascript
// Exports:
export { createSpanData } from './otel-span-builder-utils.mjs';
export { executeKnowledgeEngine };
export { executeKnowledgeEngineCore };
export { executeCLIParse };
export { executeCLIQuery };
export { executeCLIValidate };
export { executeCLIHook };
```

#### Module 2: `otel-span-builder-transactions.mjs` (~300 lines)
**Purpose**: Transaction, hook, and policy execution
```javascript
// Exports:
export { executeTransactionManager };
export { executeKnowledgeHooksAPI };
export { executePolicyPacks };
export { executeLockchainIntegrity };
```

#### Module 3: `otel-span-builder-runtime.mjs` (~600 lines)
**Purpose**: AtomVM and browser runtime execution
```javascript
// Exports:
export { executeBrowserCompatibility };
export { executeAtomVMBridge };
export { executeAtomVMRuntime };
export { executeAtomVMErlang };
```

#### Module 4: `otel-span-builder.mjs` (barrel export, ~28 lines)
**Purpose**: Re-export all functions for backward compatibility
```javascript
/**
 * @file OpenTelemetry Span Builder
 * @module validation/otel-span-builder
 */

export { createSpanData } from './otel-span-builder-core.mjs';
export {
  executeKnowledgeEngine,
  executeKnowledgeEngineCore,
  executeCLIParse,
  executeCLIQuery,
  executeCLIValidate,
  executeCLIHook
} from './otel-span-builder-core.mjs';

export {
  executeTransactionManager,
  executeKnowledgeHooksAPI,
  executePolicyPacks,
  executeLockchainIntegrity
} from './otel-span-builder-transactions.mjs';

export {
  executeBrowserCompatibility,
  executeAtomVMBridge,
  executeAtomVMRuntime,
  executeAtomVMErlang
} from './otel-span-builder-runtime.mjs';
```

### Dependent Files (Need Import Updates)
```bash
# Find dependents:
grep -r "from.*otel-span-builder" packages/*/src packages/*/test --include="*.mjs"

# Expected dependents:
- packages/validation/src/otel-validator-core.mjs
- packages/validation/test/*.test.mjs
```

### Execution Checklist
- [ ] Read full otel-span-builder.mjs
- [ ] Create otel-span-builder-core.mjs (functions 1-7)
- [ ] Create otel-span-builder-transactions.mjs (functions 8-11)
- [ ] Create otel-span-builder-runtime.mjs (functions 12-14)
- [ ] Update otel-span-builder.mjs to barrel export
- [ ] Find all imports: `grep -r "otel-span-builder"`
- [ ] Update imports (if needed, or rely on barrel export)
- [ ] Run: `timeout 5s pnpm -C packages/validation test:fast`
- [ ] Verify: All tests pass, 0 functional changes
- [ ] Check line counts: All files <500 lines

---

## File 2: yawl-schemas.mjs (1091 lines → 4 modules)

**Path**: `/home/user/unrdf/packages/yawl/src/types/yawl-schemas.mjs`

### Split Strategy (Domain Grouping)

#### Module 1: `yawl-schemas-workflow.mjs` (~350 lines)
**Purpose**: Workflow and workflow specification schemas
- WorkflowSchema
- WorkflowSpecificationSchema
- WorkflowMetadataSchema

#### Module 2: `yawl-schemas-task.mjs` (~350 lines)
**Purpose**: Task, condition, and composite task schemas
- TaskSchema
- CompositeTaskSchema
- AtomicTaskSchema
- ConditionSchema

#### Module 3: `yawl-schemas-execution.mjs` (~350 lines)
**Purpose**: Execution context, state, and event schemas
- ExecutionContextSchema
- WorkflowStateSchema
- WorkflowEventSchema
- WorkflowTransitionSchema

#### Module 4: `yawl-schemas.mjs` (barrel export, ~41 lines)
```javascript
export * from './yawl-schemas-workflow.mjs';
export * from './yawl-schemas-task.mjs';
export * from './yawl-schemas-execution.mjs';
```

### Dependent Files
```bash
# Find dependents:
grep -r "from.*yawl-schemas" packages/*/src --include="*.mjs"
# Expected: yawl-hooks.mjs, yawl-store.mjs, yawl-ontology.mjs
```

---

## File 3: yawl-hooks.mjs (1073 lines → 4 modules)

**Path**: `/home/user/unrdf/packages/yawl/src/hooks/yawl-hooks.mjs`

### Split Strategy (Lifecycle Phases)

#### Module 1: `yawl-hooks-lifecycle.mjs` (~350 lines)
**Purpose**: Workflow lifecycle hooks
- beforeWorkflowStart
- afterWorkflowStart
- beforeWorkflowComplete
- afterWorkflowComplete

#### Module 2: `yawl-hooks-execution.mjs` (~350 lines)
**Purpose**: Task execution hooks
- beforeTaskExecute
- afterTaskExecute
- onTaskError
- onTaskRetry

#### Module 3: `yawl-hooks-validation.mjs` (~350 lines)
**Purpose**: Validation and transformation hooks
- beforeValidation
- afterValidation
- beforeTransformation
- afterTransformation

#### Module 4: `yawl-hooks.mjs` (barrel export, ~23 lines)
```javascript
export * from './yawl-hooks-lifecycle.mjs';
export * from './yawl-hooks-execution.mjs';
export * from './yawl-hooks-validation.mjs';
```

---

## File 4: schemas.mjs (1063 lines → 4 modules)

**Path**: `/home/user/unrdf/packages/knowledge-engine/src/schemas.mjs`

### Split Strategy (Domain Model)

#### Module 1: `schemas-graph.mjs` (~350 lines)
**Purpose**: Graph, triple, and quad schemas
- GraphSchema
- TripleSchema
- QuadSchema
- NamedNodeSchema

#### Module 2: `schemas-query.mjs` (~350 lines)
**Purpose**: SPARQL query and result schemas
- QuerySchema
- QueryResultSchema
- BindingSchema
- SolutionSchema

#### Module 3: `schemas-transaction.mjs` (~350 lines)
**Purpose**: Transaction and operation schemas
- TransactionSchema
- OperationSchema
- CommitSchema
- RollbackSchema

#### Module 4: `schemas.mjs` (barrel export, ~13 lines)
```javascript
export * from './schemas-graph.mjs';
export * from './schemas-query.mjs';
export * from './schemas-transaction.mjs';
```

---

## File 5: query-optimizer.mjs (1051 lines → 4 modules)

**Path**: `/home/user/unrdf/packages/knowledge-engine/src/query-optimizer.mjs`

### Split Strategy (Optimizer Phases)

#### Module 1: `query-optimizer-rules.mjs` (~350 lines)
**Purpose**: Optimization rule definitions
- filterPushdownRule
- joinReorderingRule
- constantFoldingRule
- redundantPatternEliminationRule

#### Module 2: `query-optimizer-transforms.mjs` (~350 lines)
**Purpose**: Query transformation functions
- transformFilterPushdown
- transformJoinReordering
- transformConstantFolding
- transformPatternElimination

#### Module 3: `query-optimizer-cost.mjs` (~350 lines)
**Purpose**: Cost estimation and statistics
- estimateCost
- estimateCardinality
- collectStatistics
- selectJoinOrder

#### Module 4: `query-optimizer.mjs` (barrel export + main class, ~1 line)
```javascript
export * from './query-optimizer-rules.mjs';
export * from './query-optimizer-transforms.mjs';
export * from './query-optimizer-cost.mjs';
```

---

## Files 6-10: Summary Strategy

### File 6: otel-validator-core.mjs (1004 lines)
**Split**: validator-core, validator-features, validator-reporters (3 modules)

### File 7: domain-infer.mjs (966 lines)
**Split**: domain-infer-rules, domain-infer-engine, domain-infer-utils (3 modules)

### File 8: initialize.mjs (957 lines)
**Split**: initialize-core, initialize-plugins, initialize-config (3 modules)

### File 9: knowledge-substrate-core.mjs (927 lines)
**Split**: substrate-store, substrate-query, substrate-hooks (3 modules)

### File 10: browser.mjs (910 lines)
**Split**: browser-core, browser-polyfills, browser-compat (3 modules)

---

## Universal Refactoring Process

### For Each File:
1. **Analyze** (5 min)
   ```bash
   wc -l <file>
   grep -n "^export" <file> | wc -l
   grep -n "^import" <file>
   ```

2. **Plan Split** (10 min)
   - Identify logical boundaries (functions, classes, domains)
   - Design 3-4 modules with clear responsibilities
   - Target: Each module <500 lines
   - Design barrel export strategy

3. **Execute Split** (30 min per file)
   ```bash
   # Read original
   Read <file>

   # Create new modules
   Write <module-1.mjs>
   Write <module-2.mjs>
   Write <module-3.mjs>

   # Update barrel
   Edit <original-file.mjs>

   # Find dependents
   grep -r "from.*<file-name>" packages/*/src packages/*/test

   # Update imports (if needed)
   # Usually not needed if barrel export maintains API
   ```

4. **Validate** (5 min)
   ```bash
   # Run tests
   timeout 5s pnpm -C packages/<package> test:fast

   # Check line counts
   wc -l packages/<package>/src/<module>*.mjs

   # Verify exports
   node -e "import('./packages/<package>/src/<file>.mjs').then(m => console.log(Object.keys(m)))"
   ```

5. **Verify** (2 min)
   - [ ] All tests passing (100%)
   - [ ] All modules <500 lines
   - [ ] Barrel export maintains API
   - [ ] No functional changes
   - [ ] Imports still work

---

## Batch Execution Strategy (80/20)

### Phase 1: Critical Files (Files 1-5)
**Impact**: 5 files → 20 new modules, ~5000 lines refactored
**Time**: 2-3 hours
**Outcome**: Eliminate all critical violations (>1000 lines)

### Phase 2: Major Files (Files 6-10)
**Impact**: 5 files → 15 new modules, ~4700 lines refactored
**Time**: 2-3 hours
**Outcome**: Eliminate all major violations (900-1000 lines)

### Phase 3: Moderate Files (Top 20 of 700-900 lines)
**Impact**: 20 files → 40 new modules, ~16,000 lines refactored
**Time**: 4-6 hours (if needed)
**Outcome**: Reduce violations from 83 → ~50 files

---

## Success Metrics

### After Phase 1:
- Files >1000 lines: 5 → 0 (100% reduction)
- Total violations: 83 → ~78 (6% reduction)
- Critical risk eliminated

### After Phase 2:
- Files >900 lines: 10 → 0 (100% reduction)
- Total violations: 83 → ~73 (12% reduction)
- Major risk eliminated

### After Phase 3 (if pursued):
- Files >700 lines: 30 → ~10 (67% reduction)
- Total violations: 83 → ~50 (40% reduction)
- Moderate risk minimized

---

## Execution Command (When Tests at 100%)

```bash
# Start refactoring (example for File 1)
cd /home/user/unrdf

# 1. Analyze
wc -l packages/validation/src/otel-span-builder.mjs
grep -n "^export async function" packages/validation/src/otel-span-builder.mjs

# 2. Read and split (see detailed plans above)
# 3. Test after EACH split
timeout 5s pnpm -C packages/validation test:fast

# 4. Verify line counts
wc -l packages/validation/src/otel-span-builder*.mjs

# 5. Final validation
timeout 30s npm run test:fast  # Must be 100%
grep "Pass Rate" E2E-TEST-REPORT.md  # Update report
```

---

**Status**: Plans ready, waiting for prerequisite (100% test pass rate)
**Next Action**: Fix test failures → Run execution command
