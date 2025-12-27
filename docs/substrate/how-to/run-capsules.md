# How to Create and Persist Run Capsules

Run capsules are first-class objects that capture every Claude execution with deterministic hashing and admission control.

## Problem

You need to record Claude's operations in a way that is:

- **Deterministic**: Same inputs → same hash
- **Auditable**: Complete tool trace and artifacts
- **Admissible**: Checked against invariants before persistence

## Solution

Use the run capsule builder pattern with admission checks.

## Step-by-Step

### 1. Create a Run Capsule Builder

```javascript
import { createRunCapsule } from '@unrdf/kgc-claude';

const run = createRunCapsule({
  parentRunId: 'optional-parent-uuid', // For nested runs
  previousRunHash: 'optional-chain-hash', // For hash chaining
  vectorClock: vectorClockInstance, // For causality tracking
});

console.log('Run ID:', run.id);
console.log('Timestamp:', run.t_ns);
```

### 2. Record Tool Calls

```javascript
// Add a tool call
const callId = run.addToolCall({
  name: 'Read',
  input: { file: 'src/config.mjs' },
});

// Later, complete it with output
run.completeToolCall(callId, {
  output: 'export const config = { ... };',
});

// Or mark as failed
run.completeToolCall(callId, {
  error: 'File not found',
});
```

### 3. Track Artifacts

```javascript
// File created
run.addArtifact({
  type: 'create',
  path: 'src/new-module.mjs',
  contentHash: 'sha256-abc123...',
  metadata: { lines: 42, size: 1024 },
});

// File edited
run.addArtifact({
  type: 'edit',
  path: 'package.json',
  contentHash: 'sha256-def456...',
});

// File deleted
run.addArtifact({
  type: 'delete',
  path: 'deprecated.mjs',
});

// Command execution
run.addArtifact({
  type: 'command',
  metadata: { command: 'npm test', exitCode: 0 },
});
```

### 4. Record State Changes (Deltas)

The four delta types represent changes to different aspects of the system:

```javascript
// ΔO: Changes to ontology/universe state
run.addDeltaO({
  type: 'add',
  target: 'http://kgc.io/entity/user-123',
  after: { name: 'Alice', role: 'developer' },
});

run.addDeltaO({
  type: 'modify',
  target: 'http://kgc.io/entity/config',
  before: { debug: false },
  after: { debug: true },
});

run.addDeltaO({
  type: 'delete',
  target: 'http://kgc.io/entity/temp-data',
  before: { tempValue: 42 },
});

// ΔΠ: Changes to projections (views)
run.addDeltaPi({
  type: 'add',
  target: 'projection:cli-dashboard',
  after: { columns: ['name', 'status'] },
});

// ΔΛ: Changes to laws/constraints
run.addDeltaLambda({
  type: 'add',
  target: 'law:max-file-size',
  after: { limit: 10485760 }, // 10MB
});

// ΔQ: Changes to invariants
run.addDeltaQ({
  type: 'add',
  target: 'invariant:unique-ids',
  after: { enforce: true },
});
```

### 5. Check Admission Before Sealing

```javascript
import { checkAdmission } from '@unrdf/kgc-claude';

// Seal the run capsule
const capsule = await run.seal();

// Check admission rules
const admissionCheck = checkAdmission(capsule, {
  history: runHistory, // Set of previously admitted hashes
  preserveQ: capsule => {
    // Custom invariant checking
    // Return true if all invariants preserved
    return capsule.deltaQ.every(delta => {
      // Validate each invariant change
      return validateInvariant(delta);
    });
  },
});

if (!admissionCheck.admitted) {
  console.error('Admission denied:', admissionCheck.reason);
  // Handle rejection (don't persist)
} else {
  console.log('Admission granted');
  // Proceed to persist
}
```

### 6. Persist to Store

```javascript
import { persistRunCapsule } from '@unrdf/kgc-claude';

// Only persist if admitted
if (admissionCheck.admitted) {
  const { receipt } = await persistRunCapsule(store, capsule);

  console.log('Run persisted!');
  console.log('Event type:', receipt.type);
  console.log('Receipt hash:', receipt.hash);

  // Add to history to prevent duplicates
  runHistory.add(capsule.runHash);
}
```

### 7. Replay from Store

```javascript
import { replayRunCapsule } from '@unrdf/kgc-claude';

const replayedRun = await replayRunCapsule(store, capsule.id);

if (replayedRun) {
  console.log('Replayed run:', replayedRun.id);
  console.log('Status:', replayedRun.status);
  console.log('Tool trace:', replayedRun.toolTrace);
  console.log('Artifacts:', replayedRun.artifacts);
}
```

## Advanced Patterns

### Chaining Runs

```javascript
let previousHash = null;

for (const task of tasks) {
  const run = createRunCapsule({ previousRunHash: previousHash });

  // Execute task
  const callId = run.addToolCall({ name: task.tool, input: task.input });
  run.completeToolCall(callId, { output: task.result });

  const capsule = await run.seal();
  await persistRunCapsule(store, capsule);

  previousHash = capsule.runHash;
}

console.log('Run chain completed');
```

### Nested Runs (Parent-Child)

```javascript
// Parent run
const parentRun = createRunCapsule();
const parentCapsule = await parentRun.seal();

// Child run
const childRun = createRunCapsule({ parentRunId: parentCapsule.id });
childRun.addToolCall({ name: 'Subtask', input: {} });
const childCapsule = await childRun.seal();

console.log('Parent-child relationship established');
```

### Conditional Admission

```javascript
const capsule = await run.seal();

const admissionCheck = checkAdmission(capsule, {
  history: runHistory,
  preserveQ: capsule => {
    // Only admit if delta size is reasonable
    if (capsule.deltaO.length > 100) return false;

    // Only admit if no critical files touched
    const criticalFiles = ['package.json', 'tsconfig.json'];
    const touchedCritical = capsule.artifacts.some(a => criticalFiles.includes(a.path));
    if (touchedCritical) return false;

    return true;
  },
});
```

### Denial Handling

```javascript
if (!admissionCheck.admitted) {
  // Mark run as denied
  run.deny(admissionCheck.reason);

  // Seal with denial status
  const deniedCapsule = await run.seal();

  console.log('Denied capsule:', deniedCapsule.status); // 'denied'
  console.log('Denial reason:', deniedCapsule.denialReason);

  // Optionally persist denied runs for audit
  // await persistRunCapsule(store, deniedCapsule);
}
```

## Best Practices

1. **Always seal before persisting**: Unsealed runs don't have hashes
2. **Check admission**: Don't persist runs that violate invariants
3. **Track all artifacts**: Complete audit trail requires all file touches
4. **Complete tool calls**: Incomplete calls make replay unreliable
5. **Use hash chains**: Link runs for causality tracking
6. **Validate before sealing**: Check constraints before finalizing
7. **Handle denials**: Log denied runs for security audits

## Common Issues

**Issue**: Different hashes for same operations

- **Cause**: Non-deterministic data (timestamps, random IDs in input)
- **Fix**: Use normalized inputs, avoid embedding `Date.now()` in tool calls

**Issue**: Admission always fails

- **Cause**: Overly strict `preserveQ` function
- **Fix**: Debug the invariant check, ensure reasonable constraints

**Issue**: Cannot replay run

- **Cause**: Missing tool trace or incomplete data
- **Fix**: Ensure all tool calls are completed before sealing

**Issue**: Hash chain breaks

- **Cause**: Missing or incorrect previousRunHash
- **Fix**: Track previous hashes carefully, verify chain continuity

## See Also

- [API Reference: Run Capsule](../reference.md#run-capsule)
- [Explanation: Why Deterministic Hashing](../explanation.md#deterministic-hashing)
- [Tutorial: Step 2-4](../tutorial.md#step-2-create-your-first-run-capsule)
