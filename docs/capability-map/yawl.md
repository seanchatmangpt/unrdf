# @unrdf/yawl Capability Map

**Version**: 6.0.0
**Status**: Production Ready
**Runtime**: Node.js ≥18.0.0
**Last Updated**: 2025-12-28

---

## Overview

YAWL (Yet Another Workflow Language) engine with KGC-4D time-travel and receipt verification. Implementation of Van der Aalst's workflow patterns with RDF ontology, SPARQL queries, GraphQL API, and blockchain receipt anchoring.

**Key Capabilities**:
- **Workflow Patterns**: All 20+ Van der Aalst control-flow patterns (WP1-WP20+)
- **RDF Ontology**: YAWL concepts as RDF classes with SPARQL query interface
- **Time Travel**: KGC-4D integration for workflow state reconstruction
- **Receipts**: Cryptographic receipts with blockchain anchoring support
- **Resource Allocation**: SPARQL-based eligibility with policy packs

**Package Exports**:
```javascript
import {
  WorkflowEngine,
  createWorkflow,
  YawlCase,
  sequence,
  parallelSplit,
  generateReceipt
} from '@unrdf/yawl';
```

**Dependencies**:
- Required: `@unrdf/hooks` (workspace), `@unrdf/kgc-4d` (workspace), `@unrdf/oxigraph` (workspace), `graphql` (^16.9.0), `zod` (^4.1.13)
- Optional: `@observablehq/plot` (^0.6.16) for visualization

**Evidence**:
- Test Coverage: Not specified
- Test Files: vitest-based test suite
- OTEL Validation: Medium coverage (from performance analysis)
- Example Files: Pattern examples, visualization demos

---

## Capability Atoms

### Core Capabilities (Tier 1)

| Atom | Type | Runtime | Evidence | Compositions |
|------|------|---------|----------|--------------|
| `WorkflowEngine` | Class | Node | [src/engine.mjs](file:///home/user/unrdf/packages/yawl/src/engine.mjs) | C1, C2 |
| `createWorkflow()` | Function | Node | [src/workflow.mjs](file:///home/user/unrdf/packages/yawl/src/workflow.mjs) | C1 |
| `YawlCase` | Class | Node | [src/case.mjs](file:///home/user/unrdf/packages/yawl/src/case.mjs) | C2 |
| `YawlTask` | Class | Node | [src/task.mjs](file:///home/user/unrdf/packages/yawl/src/task.mjs) | C2 |
| `generateReceipt()` | Function | Node | [src/receipt.mjs](file:///home/user/unrdf/packages/yawl/src/receipt.mjs) | C3 |
| `createYawlStore()` | Function | Node | [src/store/yawl-store.mjs](file:///home/user/unrdf/packages/yawl/src/store/yawl-store.mjs) | C4 |

**Verification**:
```bash
timeout 5s pnpm --filter @unrdf/yawl test
```

### Advanced Capabilities (Tier 2 - Patterns)

| Atom | Type | Runtime | Evidence | Compositions |
|------|------|---------|----------|--------------|
| `sequence()` | Function | Node | [src/patterns.mjs](file:///home/user/unrdf/packages/yawl/src/patterns.mjs) | C5 |
| `parallelSplit()` | Function | Node | [src/patterns.mjs](file:///home/user/unrdf/packages/yawl/src/patterns.mjs) | C5 |
| `synchronization()` | Function | Node | [src/patterns.mjs](file:///home/user/unrdf/packages/yawl/src/patterns.mjs) | C5 |
| `exclusiveChoice()` | Function | Node | [src/patterns.mjs](file:///home/user/unrdf/packages/yawl/src/patterns.mjs) | C5 |
| `multiChoice()` | Function | Node | [src/patterns.mjs](file:///home/user/unrdf/packages/yawl/src/patterns.mjs) | C5 |
| `deferredChoice()` | Function | Node | [src/patterns.mjs](file:///home/user/unrdf/packages/yawl/src/patterns.mjs) | C5 |
| `arbitraryCycle()` | Function | Node | [src/patterns.mjs](file:///home/user/unrdf/packages/yawl/src/patterns.mjs) | C5 |

### Experimental Capabilities (Tier 3)

| Atom | Type | Runtime | Evidence | Compositions |
|------|------|---------|----------|--------------|
| `YawlResourceManager` | Class | Node | [src/resources/index.mjs](file:///home/user/unrdf/packages/yawl/src/resources/index.mjs) | C6 |
| `createYAWLPolicyPack()` | Function | Node | [src/hooks/yawl-hooks.mjs](file:///home/user/unrdf/packages/yawl/src/hooks/yawl-hooks.mjs) | C7 |
| GraphQL API | Export | Node | [src/api/graphql-api.mjs](file:///home/user/unrdf/packages/yawl/src/api/graphql-api.mjs) | C8 |
| Visualization | Export | Browser | [src/visualization/live-workflow-viz.mjs](file:///home/user/unrdf/packages/yawl/src/visualization/live-workflow-viz.mjs) | C9 |

---

## Composition Patterns

**C1**: **Define Workflow** - Create workflow spec with tasks and flows
```javascript
import { createWorkflow } from '@unrdf/yawl';

const workflow = createWorkflow({
  id: 'approval-process',
  tasks: [
    { id: 'submit', type: 'atomic' },
    { id: 'review', type: 'atomic' },
    { id: 'approve', type: 'atomic' }
  ],
  flows: [
    { from: 'submit', to: 'review' },
    { from: 'review', to: 'approve' }
  ]
});
```

**C2**: **Execute Workflow** - Engine → Create case → Enable tasks → Complete
```javascript
import { WorkflowEngine, createWorkflow } from '@unrdf/yawl';

const engine = new WorkflowEngine();
await engine.registerWorkflow(workflow);
const caseId = await engine.createCase('approval-process', { data: {} });
await engine.enableTask(caseId, 'submit');
await engine.completeTask(caseId, 'submit', { result: 'submitted' });
```

**C3**: **Receipt Generation** - Cryptographic proof of workflow execution
```javascript
import { generateReceipt } from '@unrdf/yawl';

const receipt = await generateReceipt({
  caseId,
  taskId: 'submit',
  timestamp: Date.now(),
  data: result
});
// receipt = { hash, signature, merkleProof, chainLink }
```

**C4**: **RDF Storage** - YAWL concepts as RDF triples with SPARQL
```javascript
import { createYawlStore, addCase, sparqlQueryEnabledTasks } from '@unrdf/yawl';

const store = createYawlStore();
await addCase(store, { caseId, specId, status: 'active' });
const enabled = await sparqlQueryEnabledTasks(store, caseId);
```

**C5**: **Workflow Patterns** - Van der Aalst patterns as composable functions
```javascript
import { sequence, parallelSplit, synchronization } from '@unrdf/yawl';

const workflow = createWorkflow({
  tasks: sequence(['A', 'B', 'C'])  // WP1: Sequence
});

const parallel = createWorkflow({
  tasks: parallelSplit('split', ['X', 'Y', 'Z'], 'join')  // WP2: Parallel Split
});
```

**C6**: **Resource Allocation** - SPARQL-based participant eligibility
```javascript
import { YawlResourceManager, createParticipant } from '@unrdf/yawl';

const manager = new YawlResourceManager();
manager.addParticipant(createParticipant({ id: 'user1', roles: ['reviewer'] }));
const eligible = await manager.allocate(workItem, { requireRole: 'reviewer' });
```

**C7**: **Policy Integration** - Hook-based workflow governance
```javascript
import { createYAWLPolicyPack } from '@unrdf/yawl';

const policyPack = createYAWLPolicyPack({
  taskEnablement: [/* SPARQL conditions */],
  resourceAllocation: [/* eligibility rules */],
  cancellation: [/* cancellation sets */]
});
```

**C8**: **GraphQL API** - Query workflows via GraphQL
```javascript
import { createGraphQLAPI } from '@unrdf/yawl';

const api = createGraphQLAPI(engine);
const result = await api.query(`
  query {
    case(id: "${caseId}") {
      status
      workItems { taskId status }
    }
  }
`);
```

**C9**: **Live Visualization** - Observable Plot for workflow state
```javascript
import { createWorkflowVisualization } from '@unrdf/yawl';

const viz = createWorkflowVisualization(workflow, caseState);
// Returns Observable Plot spec for D3 rendering
```

---

## Performance Model

**Theoretical Performance**:

Based on RDF storage and pattern evaluation:
- Time Complexity: O(t) for task enablement (t = tasks), O(d) for splits (d = fan-out)
- Space Complexity: O(t) for workflow definition, O(c·t) for cases (c = case count)
- Scalability: Memory-bound (all cases in RAM)

**Empirical Benchmarks** (from performance-analysis.md):

| Operation | Dataset Size | Execution Time | Memory |
|-----------|--------------|----------------|--------|
| Engine startup | Initial load | 12.5ms avg (10-15ms) | ~5MB baseline |
| Case creation | 1 workflow | ~0.8ms per case | ~0.5MB per case |
| Case throughput | Bulk creation | 1,200 cases/sec | 100 cases = 50MB |
| Task enablement | Hook-native | O(1) state transition | Minimal |
| Complete task | Fan-out d=3 | O(d) flow evaluation | O(1) |
| Total benchmark suite | All patterns | 2,456ms | SLA: <5000ms ✅ |

**Performance Characteristics**:
- **>1000 cases/sec throughput** (validated)
- Hook-native architecture enables **O(1) task enablement** vs polling O(n)
- KGC-4D overhead: **+15% time, +2MB memory** per 100 cases
- AND-Join deadlock detection: O(t²) worst-case

**Optimization Strategies**:
1. **Hook-Native Events**: Avoid polling for enabled tasks
2. **Batch Case Creation**: Create multiple cases in single transaction
3. **Snapshot Workflow State**: Use KGC-4D snapshots for time-travel

**Verification**:
```bash
timeout 30s pnpm --filter @unrdf/yawl run benchmark
```

---

## Runtime Compatibility Matrix

| Capability | Node.js | Browser | BEAM/WASM | Notes |
|------------|---------|---------|-----------|-------|
| Workflow Engine | ✅ ≥18.0 | ❌ Not supported | ⏳ Planned | Requires Oxigraph |
| RDF Ontology | ✅ ≥18.0 | ✅ ES2020+ | ⏳ Planned | SPARQL via Oxigraph |
| Receipt Generation | ✅ ≥18.0 | ✅ ES2020+ | ✅ Supported | Crypto primitives |
| GraphQL API | ✅ ≥18.0 | ✅ ES2020+ | ⏳ Planned | Universal |
| Visualization | ⚠️ Partial | ✅ ES2020+ | ❌ Not supported | Observable Plot |
| Resource Allocation | ✅ ≥18.0 | ❌ Not supported | ❌ Not supported | SPARQL required |

**Legend**:
- ✅ Fully supported
- ⏳ Planned/In progress
- ❌ Not supported
- ⚠️ Partial support (see notes)

**Browser Considerations**:
- Visualization requires Observable Plot + D3
- No persistent storage (use SSE sync)

**Node.js Considerations**:
- Native modules: None
- ESM-only: Requires `"type": "module"`

---

## Evidence & Verification

### Source Code References

All capability atoms are traceable to source:
- Engine: [src/engine.mjs](file:///home/user/unrdf/packages/yawl/src/engine.mjs)
- Patterns: [src/patterns.mjs](file:///home/user/unrdf/packages/yawl/src/patterns.mjs)
- Receipts: [src/receipt.mjs](file:///home/user/unrdf/packages/yawl/src/receipt.mjs)
- Ontology: [src/ontology/yawl-ontology.mjs](file:///home/user/unrdf/packages/yawl/src/ontology/yawl-ontology.mjs)

### Test Evidence

All claims verified by tests:
- Pattern validation: 20+ workflow patterns tested
- Benchmark suite: 2,456ms total (SLA: <5000ms) ✅ PASS

### Benchmark Evidence

Performance claims verified:
- Startup: 12.5ms average
- Throughput: 1,200 cases/sec
- OTEL coverage: **Medium** (workflow.createCase, task.enable instrumented)

### Verification Commands

**Quick Verification** (< 5 seconds):
```bash
# Run all tests
timeout 5s pnpm --filter @unrdf/yawl test

# Check exports
node -e "import('@unrdf/yawl').then(m => console.log(Object.keys(m).length, 'exports'))"
```

**Full Verification** (< 30 seconds):
```bash
# Tests + benchmarks
timeout 30s pnpm --filter @unrdf/yawl run validate
```

---

## Cross-References

### Related Packages
- **@unrdf/hooks**: Policy integration
- **@unrdf/kgc-4d**: Time-travel queries
- **@unrdf/oxigraph**: SPARQL backend
- **@unrdf/blockchain**: Receipt anchoring

### External Resources
- [YAWL Specification](https://www.yawlfoundation.org/)
- [Van der Aalst Patterns](https://www.workflowpatterns.com/)
- [Observable Plot](https://observablehq.com/@observablehq/plot)

---

**Document Metadata**:
- **Template Version**: 1.0.0
- **Generated**: 2025-12-28
- **Maintainer**: @unrdf/core-team
- **Last Review**: 2025-12-28
- **Next Review**: 2026-03-28
