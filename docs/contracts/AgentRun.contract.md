# AgentRun Contract

## Purpose

Defines the execution contract for individual agent work items with bounded autonomy and deterministic outcomes.

## Input: WorkItem

```typescript
interface WorkItem {
  id: string; // Unique work item ID
  agent_id: string; // Target agent (agent-0 through agent-9)
  description: string; // What to implement
  dependencies: string[]; // Work item IDs that must complete first

  // Constraints
  constraints: {
    max_files: number; // Maximum files to modify (default: 10)
    max_tool_ops: number; // Maximum tool operations (default: 50)
    max_delta_size: number; // Maximum state changes (default: 100)
    timeout_ms: number; // Execution timeout (default: 300000 = 5min)
  };

  // Budget
  budget: {
    time_estimate_ms: number; // Expected completion time
    complexity: 'low' | 'medium' | 'high';
  };

  // Context
  input_files?: string[]; // Required input files
  output_files?: string[]; // Expected output files
  test_command?: string; // Verification command
}
```

## Output: RunResult

```typescript
interface RunResult {
  // Status
  status: 'success' | 'failure' | 'denied' | 'timeout';

  // Receipt
  receipt_id: string; // UUID of generated receipt
  receipt_path: string; // Path to RECEIPT.json

  // Artifacts
  modified_files: string[]; // Absolute paths to modified files
  created_files: string[]; // Absolute paths to created files
  deleted_files: string[]; // Absolute paths to deleted files

  // Hashes
  output_hash: string; // BLAKE3 of final state
  artifact_hashes: Record<string, string>; // path -> BLAKE3

  // Metrics
  metrics: {
    execution_time_ms: number;
    tool_ops_count: number;
    delta_size: number;
    files_touched: number;
    tests_passed?: number;
    tests_failed?: number;
  };

  // Verification
  test_output?: string; // Test command output
  proof_script?: string; // Path to replay script

  // Errors
  error?: string;
  denial_reason?: string;
}
```

## Atomicity Guarantee

An agent run is **atomic**: either fully succeeds or fully fails.

```typescript
function executeWorkItem(workItem: WorkItem): RunResult {
  // 1. Create checkpoint before run
  const beforeSnapshot = await freeze(store, git);

  try {
    // 2. Execute work item with autonomy guard
    const runCapsule = await executeWithGuard(workItem);

    // 3. Check admission rules
    const admitted = checkAdmission(runCapsule, {
      maxFiles: workItem.constraints.max_files,
      maxToolOps: workItem.constraints.max_tool_ops,
      maxDeltaSize: workItem.constraints.max_delta_size,
    });

    if (!admitted.ok) {
      // Rollback to before state
      await thaw(store, git, beforeSnapshot.id);
      return { status: 'denied', denial_reason: admitted.reason };
    }

    // 4. Run tests if specified
    if (workItem.test_command) {
      const testResult = await runTests(workItem.test_command);
      if (!testResult.passed) {
        await thaw(store, git, beforeSnapshot.id);
        return { status: 'failure', error: 'Tests failed' };
      }
    }

    // 5. Create after checkpoint and generate receipt
    const afterSnapshot = await freeze(store, git);
    const receipt = await generateReceipt({
      beforeHash: beforeSnapshot.hash,
      afterHash: afterSnapshot.hash,
      runCapsule,
      testResult,
    });

    return {
      status: 'success',
      receipt_id: receipt.id,
      output_hash: afterSnapshot.hash,
      // ... other fields
    };
  } catch (error) {
    // Rollback on any error
    await thaw(store, git, beforeSnapshot.id);
    return { status: 'failure', error: error.message };
  }
}
```

## Admission Rules

A run is **admitted** if it satisfies all constraints:

```typescript
function checkAdmission(
  capsule: RunCapsule,
  constraints: Constraints
): { ok: boolean; reason?: string } {
  // Rule 1: File limit
  const filesTouched = new Set(capsule.artifacts.filter(a => a.path).map(a => a.path));
  if (filesTouched.size > constraints.maxFiles) {
    return {
      ok: false,
      reason: `Exceeded max files: ${filesTouched.size} > ${constraints.maxFiles}`,
    };
  }

  // Rule 2: Tool operation limit
  if (capsule.toolTrace.length > constraints.maxToolOps) {
    return {
      ok: false,
      reason: `Exceeded max tool ops: ${capsule.toolTrace.length} > ${constraints.maxToolOps}`,
    };
  }

  // Rule 3: Delta size limit
  const deltaSize =
    capsule.deltaO.length +
    capsule.deltaPi.length +
    capsule.deltaLambda.length +
    capsule.deltaQ.length;
  if (deltaSize > constraints.maxDeltaSize) {
    return {
      ok: false,
      reason: `Exceeded max delta size: ${deltaSize} > ${constraints.maxDeltaSize}`,
    };
  }

  // Rule 4: No duplicate hashes (idempotency)
  // Checked against global history

  return { ok: true };
}
```

## Idempotent Replay

Every successful run produces a replay script:

```bash
#!/usr/bin/env bash
# replay-agent-1.sh
# Deterministic replay of Agent 1 work item

set -euo pipefail

# 1. Restore input state
git checkout <before_commit>

# 2. Re-run agent with same inputs
node run-agent.mjs --agent=1 --work-item=<work_item_id>

# 3. Verify output hash
ACTUAL_HASH=$(hash-output.mjs)
EXPECTED_HASH="<after_hash>"

if [ "$ACTUAL_HASH" != "$EXPECTED_HASH" ]; then
  echo "ERROR: Hash mismatch"
  exit 1
fi

echo "✅ Replay successful: hash verified"
```

## Dependency Resolution

Work items form a DAG (Directed Acyclic Graph):

```typescript
function resolveDependencies(workItems: WorkItem[]): WorkItem[][] {
  // Topological sort
  const graph = buildDependencyGraph(workItems);
  const levels = topologicalSort(graph);

  // Execute level-by-level
  // Level N can run in parallel
  return levels;
}
```

Example:

```
Level 0: [agent-1, agent-2, agent-3]  // No dependencies, parallel
Level 1: [agent-4, agent-5]            // Depend on Level 0
Level 2: [agent-0]                     // Reconciler, depends on all
```

## Testing Contract

```javascript
// Test 1: Atomicity on failure
const beforeHash = getStateHash();
const result = await executeWorkItem(failingWorkItem);
assert(result.status === 'failure');
assert(getStateHash() === beforeHash); // State unchanged

// Test 2: Admission enforcement
const result = await executeWorkItem({
  ...workItem,
  constraints: { max_files: 1 },
});
// If agent touches 2+ files:
assert(result.status === 'denied');
assert(result.denial_reason.includes('Exceeded max files'));

// Test 3: Replay determinism
const result1 = await executeWorkItem(workItem);
execSync('./replay-script.sh');
const result2 = getStateHash();
assert(result1.output_hash === result2);
```

## Error Handling

| Error Type           | Behavior          | Recovery                     |
| -------------------- | ----------------- | ---------------------------- |
| Constraint violation | Deny, rollback    | None (work item failed)      |
| Test failure         | Failure, rollback | Fix and retry                |
| Timeout              | Failure, rollback | Increase timeout or simplify |
| Tool error           | Failure, rollback | Fix tool input               |
| Dependency missing   | Block until ready | Wait for dependencies        |

## Usage Example

```javascript
const workItem = {
  id: 'work-item-1',
  agent_id: 'agent-1',
  description: 'Implement KnowledgeStore contract',
  dependencies: [],
  constraints: {
    max_files: 5,
    max_tool_ops: 20,
    max_delta_size: 50,
    timeout_ms: 60000,
  },
  budget: {
    time_estimate_ms: 30000,
    complexity: 'medium',
  },
  output_files: ['/docs/contracts/KnowledgeStore.contract.md'],
  test_command: 'node validate-contract.mjs KnowledgeStore',
};

const result = await executeWorkItem(workItem);

if (result.status === 'success') {
  console.log(`✅ Work item complete: ${result.receipt_id}`);
  console.log(`Output hash: ${result.output_hash}`);
} else {
  console.error(`❌ Work item failed: ${result.error || result.denial_reason}`);
}
```
