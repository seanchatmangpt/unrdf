# @unrdf/yawl Documentation

`@unrdf/yawl` is a workflow orchestration engine implementing Van der Aalst's YAWL (Yet Another Workflow Language) pattern language. It provides structured control flow, cryptographic audit receipts, and integration with the UNRDF knowledge graph platform.

---

## What you can do with YAWL

- Define workflows as directed graphs of tasks connected by typed control flows
- Choose from 20 standard workflow patterns (sequence, AND-split, XOR-split, OR-split, and their join counterparts)
- Run workflow cases (instances) through the `WorkflowEngine`
- Receive a cryptographic BLAKE3 receipt for every task transition
- Replay any case from its event log (time-travel via KGC-4D)
- Attach resource pools and role-based allocation to tasks
- Wire YAWL patterns into the `@unrdf/hooks` policy system

---

## Documentation sections

| Section                                | For whom      | Purpose                                                       |
| -------------------------------------- | ------------- | ------------------------------------------------------------- |
| [Tutorials](./tutorials/README.md)     | Beginners     | Learn by building — step-by-step guided examples              |
| [How-To Guides](./how-to/README.md)    | Practitioners | Solve specific problems with concise recipes                  |
| [Reference](./reference/README.md)     | All           | Precise API specification for schemas, classes, and functions |
| [Explanation](./explanation/README.md) | Everyone      | Conceptual background on why YAWL works the way it does       |

---

## Quick start

```bash
pnpm add @unrdf/yawl
```

```javascript
import { WorkflowEngine, createWorkflow, sequence } from '@unrdf/yawl';

// 1. Define a workflow
const workflow = createWorkflow({
  id: 'hello-yawl',
  name: 'Hello YAWL',
  tasks: [
    { id: 'start', name: 'Start' },
    { id: 'work', name: 'Do Work' },
    { id: 'end', name: 'End' },
  ],
  flows: [sequence('start', 'work'), sequence('work', 'end')],
});

// 2. Run a case through the engine
const engine = new WorkflowEngine({ nodeId: 'demo' });
engine.registerWorkflow(workflow);

const { case: yawlCase } = await engine.createCase('hello-yawl');
const [workItem] = yawlCase.getEnabledWorkItems();
await engine.startTask(yawlCase.id, workItem.id);
await engine.completeTask(yawlCase.id, workItem.id, { result: 'done' });
```

---

## See also

- [`@unrdf/daemon`](../daemon/README.md) — background scheduler that drives YAWL workflows
- [`@unrdf/hooks`](../hooks/) — policy hooks that YAWL integrates with via `createYAWLPolicyPack`
- [`@unrdf/kgc-4d`](../kgc-4d/) — time-travel event store used by YAWL for audit trails
