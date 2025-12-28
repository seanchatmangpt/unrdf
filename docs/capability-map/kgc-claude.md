# @unrdf/kgc-claude Capability Map

**Version**: 5.0.0
**Status**: Production Ready
**Runtime**: Node.js ≥18.0.0
**Last Updated**: 2025-12-28

---

## Overview

KGC-Claude Substrate - Deterministic run objects, universal checkpoints, bounded autonomy, and multi-agent concurrency for Claude integration. Turns Claude into a replaceable actuator with correctness, continuity, and scale moved into the substrate.

**Key Capabilities**:
- **Deterministic Run Objects**: Every Claude run as a first-class capsule with normalized tool traces
- **Universal Checkpointing**: Portable freeze/thaw across CLI/IDE/MCP surfaces
- **Bounded Autonomy**: Explicit budget/capacity enforcement with denial receipts
- **Multi-Agent Concurrency**: Shard deltas and merge deterministically with conflict resolution
- **Async Workflow Primitives**: WorkItem nodes with completion receipts
- **10-Agent Swarm Calculus**: SwarmOrchestrator, Poka-Yoke guards, observable I/O

**Package Exports**:
```javascript
import {
  createRunCapsule,
  freeze,
  createAutonomyGuard,
  mergeDeltas,
  SwarmOrchestrator,
  createSubstrate
} from '@unrdf/kgc-claude';
```

**Dependencies**:
- Required: `@unrdf/core` (workspace), `@unrdf/oxigraph` (workspace), `@unrdf/kgc-4d` (workspace), `@unrdf/yawl` (workspace), `@unrdf/hooks` (workspace), `hash-wasm` (^4.12.0), `zod` (^3.24.3)
- Optional: None

**Evidence**:
- Test Coverage: Not specified
- Test Files: vitest-based test suite
- OTEL Validation: Not specified
- Example Files: Multi-agent demos, swarm orchestration

---

## Capability Atoms

### Core Capabilities (Tier 1 - Substrate Primitives)

| Atom | Type | Runtime | Evidence | Compositions |
|------|------|---------|----------|--------------|
| `createRunCapsule()` | Function | Node | [src/run-capsule.mjs](file:///home/user/unrdf/packages/kgc-claude/src/run-capsule.mjs) | C1 |
| `freeze()` | Function | Node | [src/checkpoint.mjs](file:///home/user/unrdf/packages/kgc-claude/src/checkpoint.mjs) | C2 |
| `thaw()` | Function | Node | [src/checkpoint.mjs](file:///home/user/unrdf/packages/kgc-claude/src/checkpoint.mjs) | C3 |
| `createAutonomyGuard()` | Function | Node | [src/autonomy-guard.mjs](file:///home/user/unrdf/packages/kgc-claude/src/autonomy-guard.mjs) | C4 |
| `mergeDeltas()` | Function | Node | [src/shard-merge.mjs](file:///home/user/unrdf/packages/kgc-claude/src/shard-merge.mjs) | C5 |
| `enqueueWorkItem()` | Function | Node | [src/async-workflow.mjs](file:///home/user/unrdf/packages/kgc-claude/src/async-workflow.mjs) | C6 |
| `project()` | Function | Node | [src/projection.mjs](file:///home/user/unrdf/packages/kgc-claude/src/projection.mjs) | C7 |

**Verification**:
```bash
timeout 5s pnpm --filter @unrdf/kgc-claude test
```

### Advanced Capabilities (Tier 2 - 10-Agent Swarm)

| Atom | Type | Runtime | Evidence | Compositions |
|------|------|---------|----------|--------------|
| `SwarmOrchestrator` | Class | Node | [src/swarm-orchestrator.mjs](file:///home/user/unrdf/packages/kgc-claude/src/swarm-orchestrator.mjs) | C8 |
| `PokaYokeGuard` | Class | Node | [src/poka-yoke-guards.mjs](file:///home/user/unrdf/packages/kgc-claude/src/poka-yoke-guards.mjs) | C9 |
| `ObservableIO` | Class | Node | [src/observable-io.mjs](file:///home/user/unrdf/packages/kgc-claude/src/observable-io.mjs) | C10 |
| `InfoScheduler` | Class | Node | [src/info-scheduler.mjs](file:///home/user/unrdf/packages/kgc-claude/src/info-scheduler.mjs) | C11 |
| `DriftDetector` | Class | Node | [src/drift-detector.mjs](file:///home/user/unrdf/packages/kgc-claude/src/drift-detector.mjs) | C12 |
| `BudgetEnforcer` | Class | Node | [src/budget-enforcer.mjs](file:///home/user/unrdf/packages/kgc-claude/src/budget-enforcer.mjs) | C13 |
| `AgentHarness` | Class | Node | [src/agent-harness.mjs](file:///home/user/unrdf/packages/kgc-claude/src/agent-harness.mjs) | C14 |
| `ReceiptCompositor` | Class | Node | [src/receipt-compositor.mjs](file:///home/user/unrdf/packages/kgc-claude/src/receipt-compositor.mjs) | C15 |

### Experimental Capabilities (Tier 3 - Hyper-Advanced Delegation)

| Atom | Type | Runtime | Evidence | Compositions |
|------|------|---------|----------|--------------|
| `HierarchicalDelegator` | Class | Node | [src/hierarchical-delegation.mjs](file:///home/user/unrdf/packages/kgc-claude/src/hierarchical-delegation.mjs) | C16 |
| `AgentSwarmPatterns` | Class | Node | [src/agent-swarm-patterns.mjs](file:///home/user/unrdf/packages/kgc-claude/src/agent-swarm-patterns.mjs) | C17 |
| `DelegationOptimizer` | Class | Node | [src/delegation-optimizer.mjs](file:///home/user/unrdf/packages/kgc-claude/src/delegation-optimizer.mjs) | C18 |

---

## Composition Patterns

**C1**: **Deterministic Run** - Create capsule → Add tool calls → Seal
```javascript
import { createRunCapsule, persistRunCapsule } from '@unrdf/kgc-claude';

const run = createRunCapsule({ sessionId: 'session-1' });
run.addToolCall({ name: 'Read', input: { file: 'foo.txt' }, output: 'content' });
const capsule = await run.seal();
await persistRunCapsule(store, capsule);
```

**C2**: **Universal Checkpoint** - Freeze state → Git commit → Receipt
```javascript
import { freeze } from '@unrdf/kgc-claude';

const receipt = await freeze(store, gitBackbone, { message: 'Checkpoint 1' });
// receipt = { checkpointId, hash, commitSha, timestamp }
```

**C3**: **Restore from Checkpoint** - Thaw state → Reconstruct
```javascript
import { thaw } from '@unrdf/kgc-claude';

const state = await thaw(store, gitBackbone, checkpointId);
// state = RDF graph at checkpoint
```

**C4**: **Bounded Autonomy** - Create guard → Check budget → Deny/allow
```javascript
import { createAutonomyGuard, withGuard } from '@unrdf/kgc-claude';

const guard = createAutonomyGuard({
  maxTokens: 10000,
  maxToolCalls: 50,
  maxDuration: 300000 // 5 minutes
});

const result = await withGuard(guard, async () => {
  // Guarded operation
  return await claudeRun();
});
```

**C5**: **Multi-Agent Merge** - Shard deltas → Merge → Apply
```javascript
import { createShard, addDelta, mergeDeltas } from '@unrdf/kgc-claude';

const shard1 = createShard({ agentId: 'agent-1', scope: 'task-A' });
addDelta(shard1, delta1);

const shard2 = createShard({ agentId: 'agent-2', scope: 'task-B' });
addDelta(shard2, delta2);

const merged = await mergeDeltas([shard1, shard2]); // Deterministic merge
```

**C6**: **Async Workflow** - Enqueue → Assign → Execute → Complete
```javascript
import { enqueueWorkItem, assignWorkItem, completeWorkItem } from '@unrdf/kgc-claude';

const workItemId = await enqueueWorkItem({
  task: 'process-data',
  payload: { dataId: 'data-123' }
});

await assignWorkItem(workItemId, 'executor-1');
const result = await executeTask(workItemId);
await completeWorkItem(workItemId, { result });
```

**C7**: **Surface Projection** - Project state → UI/CLI view
```javascript
import { project, registerProjection } from '@unrdf/kgc-claude';

registerProjection('ui', (state) => ({
  tasks: state.workItems.filter(w => w.status === 'pending'),
  progress: state.completedCount / state.totalCount
}));

const uiView = project(state, 'ui');
```

**C8**: **10-Agent Swarm** - Create swarm → Dispatch probes → Aggregate
```javascript
import { SwarmOrchestrator, createSwarm10 } from '@unrdf/kgc-claude';

const swarm = createSwarm10({
  agents: [
    { id: 'planner', role: 'planning' },
    { id: 'coder', role: 'implementation' },
    // ... 8 more agents
  ]
});

const results = await swarm.dispatch({ type: 'feature-request', spec });
```

**C9**: **Poka-Yoke Guards** - Deny-by-construction → Hard stops
```javascript
import { PokaYokeGuard, ForbiddenOps } from '@unrdf/kgc-claude';

const guard = new PokaYokeGuard({
  forbidden: [ForbiddenOps.FILE_DELETE, ForbiddenOps.NETWORK_REQUEST]
});

await guard.check(operation); // Throws if forbidden
```

**C10**: **Observable I/O** - Trace all I/O → No side effects
```javascript
import { ObservableIO, createObservableIO } from '@unrdf/kgc-claude';

const io = createObservableIO();
const result = await io.controlledInput({ source: 'file', path: 'data.json' });
const trace = io.getTrace(); // All I/O operations logged
```

**C11**: **Info Scheduler** - Information-theoretic task ordering
```javascript
import { InfoScheduler, createScheduler } from '@unrdf/kgc-claude';

const scheduler = createScheduler();
scheduler.addProbe({ id: 'probe-1', entropy: 0.8, cost: 100 });
const next = scheduler.selectNext(); // Highest info gain per cost
```

**C12**: **Drift Detection** - Detect convergence/divergence
```javascript
import { DriftDetector, createDriftDetector } from '@unrdf/kgc-claude';

const detector = createDriftDetector();
detector.snapshot(artifact, 'v1');
detector.snapshot(artifact, 'v2');
const drift = detector.measure('v1', 'v2'); // Hamming distance
```

**C13**: **Budget Enforcement** - Hard resource limits
```javascript
import { BudgetEnforcer, createBudget } from '@unrdf/kgc-claude';

const budget = createBudget({ tokens: 10000, memory: 1024 });
await budget.allocate({ tokens: 500 }); // Throws if exceeds
```

**C14**: **Agent Harness** - Per-agent lifecycle
```javascript
import { AgentHarness, createAgent } from '@unrdf/kgc-claude';

const agent = createAgent({
  id: 'agent-1',
  role: 'coder',
  budget: { tokens: 5000 }
});

await agent.execute(task);
const receipt = agent.getReceipt();
```

**C15**: **Receipt Compositor** - Merkle tree of receipts
```javascript
import { ReceiptCompositor, createCompositor } from '@unrdf/kgc-claude';

const compositor = createCompositor();
compositor.add(receipt1);
compositor.add(receipt2);
const composite = compositor.finalize(); // Merkle root
```

**C16**: **Hierarchical Delegation** - Multi-level agent hierarchy
```javascript
import { HierarchicalDelegator, create3LevelHierarchy } from '@unrdf/kgc-claude';

const hierarchy = create3LevelHierarchy({
  L1: ['orchestrator'],
  L2: ['planner', 'coordinator'],
  L3: ['coder', 'tester', 'reviewer']
});

const result = await hierarchy.delegate(task);
```

**C17**: **Swarm Patterns** - Fan-out/fan-in, pipeline, consensus
```javascript
import { AgentSwarmPatterns, executeFanOutFanIn } from '@unrdf/kgc-claude';

const results = await executeFanOutFanIn({
  task: 'analyze-code',
  agents: [agent1, agent2, agent3],
  aggregation: 'vote'
});
```

**C18**: **Delegation Optimizer** - Smart task routing
```javascript
import { DelegationOptimizer, createDelegationOptimizer } from '@unrdf/kgc-claude';

const optimizer = createDelegationOptimizer({
  agents: [/* agents with capabilities */],
  strategy: 'cost-based'
});

const assignment = optimizer.assign(task);
```

---

## Performance Model

**Theoretical Performance**:

Based on multi-agent architecture:
- Time Complexity: O(a) for swarm dispatch (a=agent count)
- Space Complexity: O(a·s) for shards (a=agents, s=shard size)
- Scalability: Limited by agent coordination overhead

**Empirical Benchmarks**:

Not available in performance-analysis.md. Package is experimental.

**Performance Characteristics**:
- Deterministic merge: O(n) where n = delta count
- Receipt composition: O(log n) Merkle tree
- Budget enforcement: O(1) check
- Poka-Yoke guards: O(1) deny

---

## Runtime Compatibility Matrix

| Capability | Node.js | Browser | BEAM/WASM | Notes |
|------------|---------|---------|-----------|-------|
| Run Capsules | ✅ ≥18.0 | ❌ Not supported | ⏳ Planned | Requires filesystem |
| Checkpointing | ✅ ≥18.0 | ❌ Not supported | ❌ Not supported | Requires Git |
| Multi-Agent Merge | ✅ ≥18.0 | ✅ ES2020+ | ✅ Supported | Universal |
| Swarm Orchestration | ✅ ≥18.0 | ⚠️ Partial | ⏳ Planned | Limited in browser |
| Budget Enforcement | ✅ ≥18.0 | ✅ ES2020+ | ✅ Supported | Universal |

**Legend**:
- ✅ Fully supported
- ⏳ Planned/In progress
- ❌ Not supported
- ⚠️ Partial support

---

## Evidence & Verification

### Source Code References

All capability atoms are traceable to source:
- Core Substrate: [src/run-capsule.mjs](file:///home/user/unrdf/packages/kgc-claude/src/run-capsule.mjs), [src/checkpoint.mjs](file:///home/user/unrdf/packages/kgc-claude/src/checkpoint.mjs)
- Swarm Calculus: [src/swarm-orchestrator.mjs](file:///home/user/unrdf/packages/kgc-claude/src/swarm-orchestrator.mjs)

### Verification Commands

**Quick Verification** (< 5 seconds):
```bash
timeout 5s pnpm --filter @unrdf/kgc-claude test
```

---

## Cross-References

### Related Packages
- **@unrdf/kgc-4d**: Event sourcing backend
- **@unrdf/yawl**: Workflow primitives
- **@unrdf/hooks**: Policy integration

---

**Document Metadata**:
- **Template Version**: 1.0.0
- **Generated**: 2025-12-28
- **Maintainer**: @unrdf/core-team
- **Last Review**: 2025-12-28
- **Next Review**: 2026-03-28
