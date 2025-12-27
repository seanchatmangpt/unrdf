# Multi-Package Workflows

This document describes how to propose, execute, and manage multi-package atomic changes using the UNRDF orchestration system.

## Table of Contents

1. [Overview](#overview)
2. [Core Concepts](#core-concepts)
3. [Workflow Stages](#workflow-stages)
4. [Proposing Multi-Package Changes](#proposing-multi-package-changes)
5. [Execution Flow](#execution-flow)
6. [Rollback Procedures](#rollback-procedures)
7. [Receipt System](#receipt-system)
8. [Examples](#examples)
9. [Best Practices](#best-practices)
10. [Troubleshooting](#troubleshooting)

---

## Overview

The orchestration system coordinates changes across multiple packages atomically. This means:

- **All succeed or all fail**: If any package fails admission, testing, building, or validation, ALL changes are rolled back.
- **Dependency-aware execution**: Packages are processed in topological order based on their dependency graph.
- **Complete audit trail**: Every operation generates cryptographic receipts for verification.
- **Automatic recovery**: Failed workflows automatically restore previous state.

### When to Use Multi-Package Workflows

Use multi-package workflows when:

- Breaking changes in a core package affect downstream consumers
- New features span multiple packages that must be released together
- Dependency upgrades require consistent version pinning
- API changes require synchronized updates across the monorepo

---

## Core Concepts

### Dependency Resolution

The system builds a dependency graph and computes:

- **Closure**: All packages affected by a change (direct + transitive dependents)
- **Topological Order**: Safe execution order respecting dependencies
- **Execution Levels**: Groups of packages that can run in parallel

```javascript
import { createDependencyResolver } from '@unrdf/orchestration';

const resolver = createDependencyResolver();
resolver.addPackages({
  '@unrdf/core': { dependencies: [] },
  '@unrdf/utils': { dependencies: ['@unrdf/core'] },
  '@unrdf/cli': { dependencies: ['@unrdf/core', '@unrdf/utils'] }
});

const result = resolver.resolve(['@unrdf/core']);
// result.order = ['@unrdf/core', '@unrdf/utils', '@unrdf/cli']
// result.closure = ['@unrdf/core', '@unrdf/utils', '@unrdf/cli']
```

### Atomic Execution

All changes within a workflow are treated as a single transaction:

1. **Begin Transaction**: Checkpoint current state
2. **Execute Stages**: Run admission, testing, building, etc.
3. **Commit or Rollback**: Apply all changes or restore checkpoints

### Receipts

Every operation generates a receipt containing:

- Package name and stage
- Decision (allow/deny)
- Timestamp and duration
- Cryptographic hash for verification
- Merkle root for aggregation

---

## Workflow Stages

Each workflow proceeds through these stages in order:

### Stage 1: Dependency Analysis

**Purpose**: Detect cycles, build execution order, compute closure.

**Inputs**: Changed packages, package dependency graph

**Outputs**: Topological order, affected packages, execution levels

**Failure Condition**: Circular dependencies detected

### Stage 2: Admission (GOS Gate)

**Purpose**: Check all packages against Governed Ontology Substrate rules.

**Checks**:
- Forbidden operations (hard blocks)
- Invariant preservation
- Partition policies
- Namespace protection

**Failure Condition**: Any package violates admission rules

### Stage 3: Testing

**Purpose**: Run test suites for all affected packages.

**Execution**: Can run in parallel for independent packages

**Metrics Collected**:
- Pass/fail counts
- Test coverage
- Duration

**Failure Condition**: Any test failures

### Stage 4: Building

**Purpose**: Compile and build artifacts for all packages.

**Execution**: Can run in parallel for independent packages

**Outputs**: Build artifacts, bundle sizes, type definitions

**Failure Condition**: Build errors

### Stage 5: Validation

**Purpose**: Check production readiness.

**Checks**:
- Security scanning
- Performance benchmarks
- API compatibility
- Documentation completeness

**Failure Condition**: Validation score below threshold

### Stage 6: Integration

**Purpose**: Cross-package integration tests.

**Execution**: Runs after all packages are built

**Scope**: Tests interactions between changed packages

**Failure Condition**: Integration test failures

### Stage 7: Commit

**Purpose**: Atomically apply all changes to Universe O.

**Actions**:
- Apply RDF changes to partitions
- Generate final workflow receipt
- Update version registry

**Failure Condition**: None (previous stages prevent failures here)

---

## Proposing Multi-Package Changes

### Basic Workflow Configuration

```javascript
import { executeWorkflow } from '@unrdf/orchestration';

const result = await executeWorkflow({
  changedPackages: ['@unrdf/core'],
  packages: {
    '@unrdf/core': { dependencies: [] },
    '@unrdf/utils': { dependencies: ['@unrdf/core'] },
    '@unrdf/cli': { dependencies: ['@unrdf/core', '@unrdf/utils'] }
  },
  options: {
    parallel: true,      // Run independent stages in parallel
    dryRun: false,       // Actually apply changes
    skipStages: [],      // Stages to skip (not recommended)
    timeouts: {
      testing: 120000,   // 2 minutes
      building: 60000    // 1 minute
    }
  }
});
```

### Impact Analysis Before Execution

Always analyze impact before executing:

```javascript
import { analyzeWorkflowImpact } from '@unrdf/orchestration';

const impact = analyzeWorkflowImpact({
  changedPackages: ['@unrdf/core'],
  packages: { ... }
});

console.log('Risk Level:', impact.recommendation.riskLevel);
console.log('Affected Packages:', impact.affectedPackages.length);
console.log('Estimated Duration:', impact.recommendation.estimatedDuration);
```

### Using the Orchestrator Class

For more control, use the orchestrator directly:

```javascript
import { createWorkflowOrchestrator } from '@unrdf/orchestration';

const orchestrator = createWorkflowOrchestrator({
  admissionEngine: myEngine,
  testRunner: myTestRunner,
  builder: myBuilder,
  validator: myValidator
});

// Execute with custom components
const result = await orchestrator.execute(config);

// Check statistics
console.log(orchestrator.getStats());
```

---

## Execution Flow

### Successful Execution

```
[Start Workflow]
        |
        v
[1. Dependency Analysis] --> Success
        |
        v
[2. Admission] --> All packages: ALLOW
        |
        v
[3. Testing] --> All packages: PASS
        |
        v
[4. Building] --> All packages: SUCCESS
        |
        v
[5. Validation] --> All packages: VALID
        |
        v
[6. Integration] --> SUCCESS
        |
        v
[7. Commit] --> Applied atomically
        |
        v
[Workflow Receipt: ALLOW]
```

### Failed Execution with Rollback

```
[Start Workflow]
        |
        v
[1. Dependency Analysis] --> Success
        |
        v
[2. Admission] --> All packages: ALLOW
        |
        v
[3. Testing] --> Package X: FAIL
        |
        v
[ROLLBACK TRIGGERED]
        |
        +---> Restore Package X checkpoint
        +---> Restore all previous checkpoints
        |
        v
[Workflow Receipt: DENY]
```

---

## Rollback Procedures

### Automatic Rollback

Rollback happens automatically when any stage fails:

1. **Checkpoint Restoration**: State restored from before the failed operation
2. **Operation Reversal**: Custom rollback handlers execute in reverse order
3. **Transaction Cleanup**: All checkpoints and operations cleaned up

### Manual Rollback (Emergency)

For emergency situations, the rollback manager can be used directly:

```javascript
import { createRollbackManager, withRollback } from '@unrdf/orchestration';

const manager = createRollbackManager();

// Using withRollback helper
try {
  await withRollback(manager, ['pkg1', 'pkg2'], async (rm) => {
    await rm.checkpoint('pkg1', 'stage1', currentState);
    // ... make changes ...
    if (somethingWrong) throw new Error('Failed');
  });
} catch (error) {
  // Automatic rollback already executed
  console.log('Changes rolled back');
}
```

### Rollback Trail

The workflow receipt includes a rollback trail showing:

- Which stages were rolled back
- Which checkpoints were restored
- Final state after rollback

---

## Receipt System

### Package Receipt

Generated for each package at each stage:

```json
{
  "packageName": "@unrdf/core",
  "stage": "testing",
  "decision": "allow",
  "timestamp": "2024-12-26T10:30:00.000Z",
  "duration": 1500,
  "hash": "abc123..."
}
```

### Stage Receipt

Aggregates all package receipts for a stage:

```json
{
  "stage": "testing",
  "decision": "allow",
  "packageReceipts": [...],
  "merkleRoot": "def456...",
  "stats": {
    "total": 5,
    "passed": 5,
    "failed": 0
  }
}
```

### Workflow Receipt

Final receipt for the entire workflow:

```json
{
  "workflowId": "uuid",
  "decision": "allow",
  "changedPackages": ["@unrdf/core"],
  "affectedPackages": ["@unrdf/core", "@unrdf/utils", "@unrdf/cli"],
  "executionOrder": [...],
  "stageReceipts": [...],
  "merkleRoot": "ghi789...",
  "duration": 45000
}
```

### Receipt Verification

```javascript
import { createReceiptAggregator } from '@unrdf/orchestration';

const aggregator = createReceiptAggregator();
const verification = await aggregator.verify(workflowReceipt);

if (verification.valid) {
  console.log('Receipt verified successfully');
} else {
  console.log('Verification errors:', verification.errors);
}
```

---

## Examples

### Example 1: Core Breaking Change

A breaking change to `@unrdf/core` affecting 10+ downstream packages.

```javascript
import { runCoreBreakingChange } from '@unrdf/orchestration/example-workflows';

const result = await runCoreBreakingChange();

// Affected: core, types, utils, validation, oxigraph, streaming, cli, kgc-4d, api, web, integration-tests
```

### Example 2: Feature Spanning 3 Packages

A new feature requiring changes to `core`, `streaming`, and `cli`.

```javascript
import { runFeatureSpanningPackages } from '@unrdf/orchestration/example-workflows';

const result = await runFeatureSpanningPackages();

// All 3 packages updated atomically or none
```

### Example 3: Dependency Upgrade

Upgrading `zod` from v3 to v4 across all packages using it.

```javascript
import { runDependencyUpgrade } from '@unrdf/orchestration/example-workflows';

const result = await runDependencyUpgrade();

// All packages upgraded to same version atomically
```

---

## Best Practices

### 1. Always Analyze Impact First

```javascript
const impact = analyzeWorkflowImpact(config);
if (impact.recommendation.riskLevel === 'critical') {
  console.log('Consider breaking into smaller changes');
}
```

### 2. Run Dry Runs

```javascript
const dryRunResult = await orchestrator.dryRun(config);
if (dryRunResult.decision === 'deny') {
  console.log('Would fail at:', dryRunResult.stages.find(s => s.status === 'failed'));
}
```

### 3. Keep Changes Small

- Prefer multiple small workflows over one large workflow
- Break breaking changes into backward-compatible phases
- Coordinate with downstream package maintainers

### 4. Monitor Execution

```javascript
const result = await orchestrator.execute(config);
console.log('Stats:', orchestrator.getStats());
console.log('Duration:', result.duration, 'ms');
```

### 5. Preserve Receipts

Store workflow receipts for:
- Audit compliance
- Rollback verification
- Change history

---

## Troubleshooting

### Circular Dependency Error

**Problem**: `Cycle detected: pkg-a -> pkg-b -> pkg-c -> pkg-a`

**Solution**: Refactor to remove circular dependencies. Consider:
- Extract shared code to a new package
- Use dependency injection
- Restructure package boundaries

### Admission Denied

**Problem**: Package failed GOS admission

**Solution**: Check the decision details:
```javascript
if (result.decision === 'deny') {
  const failedStage = result.stages.find(s => s.status === 'failed');
  console.log('Failed stage:', failedStage.name);
  console.log('Failed packages:', failedStage.packages.filter(p => p.status !== 'completed'));
}
```

### Test Failures

**Problem**: Tests fail for dependent packages

**Solution**:
1. Run tests locally first: `pnpm test --filter=@unrdf/affected-pkg`
2. Check if changes require test updates
3. Ensure mocks are updated for new APIs

### Rollback Incomplete

**Problem**: Rollback reported errors

**Solution**: Check rollback result:
```javascript
if (!result.rollbackResult.success) {
  console.log('Rollback errors:', result.rollbackResult.errors);
  // Manual intervention may be required
}
```

### Performance Issues

**Problem**: Workflow takes too long

**Solution**:
1. Enable parallel execution: `options.parallel = true`
2. Reduce affected packages by splitting changes
3. Optimize slow test suites
4. Increase timeouts only if justified

---

## Architecture Reference

```
/src/orchestration/
  ├── dependency-resolver.mjs   # Topological sorting, cycle detection
  ├── stage-executor.mjs        # Stage execution with timeout/retry
  ├── rollback-manager.mjs      # Checkpoint and transaction management
  ├── receipt-aggregator.mjs    # Receipt generation and merkle trees
  ├── workflow-orchestrator.mjs # Main coordination engine
  ├── index.mjs                 # Public API exports
  └── example-workflows/        # Runnable example scenarios
      ├── core-breaking-change.mjs
      ├── feature-spanning-packages.mjs
      ├── dependency-upgrade.mjs
      └── index.mjs
```

---

## API Reference

See the inline JSDoc documentation in each module for detailed API reference:

- `createDependencyResolver()` - Dependency graph operations
- `createStageExecutor()` - Stage execution
- `createRollbackManager()` - Transaction management
- `createReceiptAggregator()` - Receipt generation
- `createWorkflowOrchestrator()` - Main orchestration
- `executeWorkflow()` - Convenience function
- `analyzeWorkflowImpact()` - Impact analysis

---

*Last updated: December 2024*
*Module version: 1.0.0*
