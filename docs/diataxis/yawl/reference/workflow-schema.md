# Reference: Workflow Schema

Authoritative specification for workflow definition structures and the `WorkflowEngine` configuration. All schemas are implemented with Zod and validated at runtime.

---

## `WorkflowSpecSchema`

Used by the high-level `createWorkflow()` API (`@unrdf/yawl/api`).

```typescript
WorkflowSpecSchema = z.object({
  id: z.string().min(1).max(255), // required
  name: z.string().min(1).max(255), // required
  version: z
    .string()
    .regex(/^\d+\.\d+\.\d+$/)
    .optional(),
  description: z.string().max(5000).optional(),
  tasks: z.array(TaskSchema).min(1), // at least 1 task required
  controlFlow: z.array(ControlFlowSchema).optional(),
  resources: z.array(ResourceSchema).optional(),
  inputVariables: z.array(z.string()).optional(),
  outputVariables: z.array(z.string()).optional(),
  cancellationRegions: z.record(z.string(), z.array(z.string())).optional(),
});
```

### `TaskSchema` (inside `WorkflowSpecSchema`)

```typescript
TaskSchema = z.object({
  id: z.string().min(1).max(255), // required, unique per workflow
  name: z.string().min(1).max(255), // required
  kind: z.enum(['atomic', 'composite', 'multiple-instance']).default('atomic'),
  description: z.string().max(2000).optional(),
  inputVariables: z.array(z.string()).optional(),
  outputVariables: z.array(z.string()).optional(),
  preConditions: z.array(z.string()).optional(), // documentary strings
  postConditions: z.array(z.string()).optional(), // documentary strings
  timeout: z.number().positive().optional(), // milliseconds
  priority: z.number().int().min(0).max(100).default(50),
  resourcePattern: z.string().optional(),
  cancellationRegion: z.string().optional(),
});
```

### `ControlFlowSchema` (inside `WorkflowSpecSchema`)

```typescript
ControlFlowSchema = z.object({
  id: z.string().min(1).max(255), // required, unique
  type: z.enum([
    // required
    'sequence',
    'and-split',
    'and-join',
    'xor-split',
    'xor-join',
    'or-split',
    'or-join',
    'deferred-choice',
    'cancellation-region',
  ]),
  from: z.string().min(1), // source task ID
  to: z.union([z.string(), z.array(z.string())]), // target task ID(s)
  condition: z.string().optional(), // condition expression (string)
  weight: z.number().min(0).max(1).optional(),
});
```

### `ResourceSchema` (inside `WorkflowSpecSchema`)

```typescript
ResourceSchema = z.object({
  id: z.string().min(1).max(255),
  name: z.string().min(1).max(255),
  type: z.enum(['role', 'participant', 'position', 'capability', 'org-group']),
  qualifications: z.array(z.string()).optional(),
  capabilities: z.array(z.string()).optional(),
  constraints: z.record(z.string(), z.any()).optional(),
});
```

---

## `createWorkflow(spec, options)` — high-level API

Located in `@unrdf/yawl/api` (or re-exported from `@unrdf/yawl`).

```typescript
async function createWorkflow(
  spec: WorkflowSpec,
  options?: WorkflowOptions
): Promise<WorkflowObject>;
```

### `WorkflowOptionsSchema`

```typescript
WorkflowOptionsSchema = z
  .object({
    store: z.any().optional(), // KGC-4D store — triggers RDF representation
    gitBackbone: z.any().optional(),
    hookRegistry: z.any().optional(),
    policyPacks: z.array(z.any()).optional(),
    validateSpec: z.boolean().default(true), // run Zod validation on spec
    createRDF: z.boolean().default(true), // write quads to store if store provided
  })
  .optional();
```

### Returned `WorkflowObject` shape

| Property / Method        | Type                                             | Description                                        |
| ------------------------ | ------------------------------------------------ | -------------------------------------------------- |
| `id`                     | `string`                                         | Workflow ID from spec                              |
| `name`                   | `string`                                         | Human-readable name                                |
| `version`                | `string`                                         | Semver string                                      |
| `receipt`                | `Receipt`                                        | Cryptographic receipt for `WORKFLOW_CREATED` event |
| `getTask(id)`            | `Task \| null`                                   | O(1) task lookup                                   |
| `isInitialTask(id)`      | `boolean`                                        | True if task has no incoming flows                 |
| `getDownstreamTasks(id)` | `Task[]`                                         | Direct successors in the control-flow graph        |
| `getUpstreamTasks(id)`   | `Task[]`                                         | Direct predecessors                                |
| `getEnabledWorkItems()`  | Called on the **case** object, not the workflow. |                                                    |

---

## `createWorkflow(spec, options)` — lower-level class form

Located in `@unrdf/yawl` (re-exported from `./workflow.mjs`).

```typescript
function createWorkflow(
  spec: WorkflowSpec,
  options?: { validate?: boolean; strict?: boolean }
): Workflow;
```

| Option     | Default | Effect                                                            |
| ---------- | ------- | ----------------------------------------------------------------- |
| `validate` | `true`  | Run full validation after construction                            |
| `strict`   | `false` | When `true`, throw on validation errors instead of attaching them |

The returned `Workflow` instance has:

| Method                             | Returns                       | Description                                |
| ---------------------------------- | ----------------------------- | ------------------------------------------ |
| `validate()`                       | `{ valid, errors, warnings }` | Run all validation checks                  |
| `isValid()`                        | `boolean`                     | Quick validity check                       |
| `getTasks()`                       | `TaskDef[]`                   | All task definitions                       |
| `evaluateDownstream(taskId, data)` | `TaskDef[]`                   | Evaluate flows from task with context data |
| `canEnable(taskId, caseState)`     | `boolean`                     | Check if join conditions are met           |

---

## `WorkflowEngine` configuration

`EngineConfigSchema`:

```typescript
EngineConfigSchema = z.object({
  nodeId: z.string().optional(),
  gitPath: z.string().optional(),
  enableEventLog: z.boolean().default(true),
  enableSnapshots: z.boolean().default(true),
  snapshotInterval: z.number().positive().default(60000), // ms
  circuitBreakerThreshold: z.number().int().positive().default(5),
  circuitBreakerResetTimeout: z.number().int().positive().default(30000), // ms
  maxConcurrentCases: z.number().int().positive().default(1000),
  defaultTaskTimeout: z.number().int().positive().default(30000), // ms
  store: z.any().optional(),
  hookAdapter: z.any().optional(),
  kgcAdapter: z.any().optional(),
  supervisorAdapter: z.any().optional(),
});
```

### Engine methods

| Method                                              | Signature                                                                          | Returns                                  |
| --------------------------------------------------- | ---------------------------------------------------------------------------------- | ---------------------------------------- |
| `registerWorkflow(workflow)`                        | `(Workflow) => Workflow`                                                           | Registered workflow                      |
| `createCase(workflowId, data?, options?)`           | `async (string, object?, object?) => { case, receipt }`                            | Case and start receipt                   |
| `enableTask(caseId, taskId, actor?)`                | `async (string, string, string?) => { task, receipt }`                             | Work item and receipt                    |
| `startTask(caseId, workItemId, options?)`           | `async (string, string, object?) => { task, receipt, resource? }`                  | Work item, receipt, optional resource    |
| `completeTask(caseId, workItemId, output?, actor?)` | `async (string, string, object?, string?) => { task, receipt, downstreamEnabled }` | Work item, receipt, enabled successors   |
| `failTask(caseId, workItemId, error)`               | `async (string, string, Error) => void`                                            | —                                        |
| `cancelCase(caseId, workItemId)`                    | `async (string, string) => void`                                                   | —                                        |
| `healthCheck()`                                     | `() => HealthReport`                                                               | `{ status, components, errors, uptime }` |
| `getActiveCases()`                                  | `() => YawlCase[]`                                                                 | All active case objects                  |
| `createSnapshot(caseId)`                            | `async (string) => Snapshot`                                                       | Snapshot record                          |
| `restoreFromSnapshot(caseId)`                       | `async (string) => YawlCase`                                                       | Restored case                            |

### `HealthStatus` constants

```javascript
HealthStatus.HEALTHY; // 'healthy'
HealthStatus.DEGRADED; // 'degraded'
HealthStatus.UNHEALTHY; // 'unhealthy'
```

---

## Named graphs

```javascript
YAWL_GRAPHS = {
  WORKFLOWS: 'http://yawl.io/workflows',
  CASES: 'http://yawl.io/cases',
  WORKITEMS: 'http://yawl.io/workitems',
  EVENTS: 'http://yawl.io/events',
};
```

---

## `workflowToRDF(workflow, store, options?)` / `workflowFromRDF(store, workflowId, options?)`

Serializes and deserializes a `Workflow` instance to/from an RDF store.

```typescript
// Serialize
function workflowToRDF(
  workflow: Workflow,
  store: OxigraphStore,
  options?: { graph?: string }
): { specUri: NamedNode; quadCount: number };

// Deserialize
async function workflowFromRDF(
  store: OxigraphStore,
  workflowId: string,
  options?: { graph?: string }
): Promise<Workflow | null>;
```

---

## See also

- [Task API Reference](./task-api.md)
- [XOR Split/Join Reference](./xor-split-join.md)
- [Tutorial: Your First Workflow](../tutorials/01-first-workflow.md)
