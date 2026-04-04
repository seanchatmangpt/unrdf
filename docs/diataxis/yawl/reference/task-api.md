# Reference: Task API

Precise specification for task definitions, work item lifecycle, status transitions, and the engine event catalogue.

---

## `TaskDefinitionSchema`

From `@unrdf/yawl/schemas` or `@unrdf/yawl`:

```typescript
TaskDefinitionSchema = z.object({
  id: z.string().min(1), // required
  name: z.string().optional(),
  kind: z
    .enum(['AtomicTask', 'CompositeTask', 'MultipleInstanceTask', 'EmptyTask'])
    .default('AtomicTask'),
  inputConditions: z.array(z.string()).default([]), // documentary strings
  outputConditions: z.array(z.string()).default([]), // documentary strings
  splitType: z.enum(['sequence', 'and', 'xor', 'or']).default('sequence'),
  joinType: z.enum(['sequence', 'and', 'xor', 'or']).default('sequence'),
  timeout: z.number().positive().optional(), // ms
  requiredRoles: z.array(z.string()).default([]),
  resourcePattern: z.string().optional(),
  cancellationRegion: z.string().optional(),
  cancellationSet: z.array(z.string()).default([]),
  preCondition: z.function().optional(), // (inputData) => boolean | throws
  postCondition: z.function().optional(), // (outputData) => boolean | throws
  decomposesTo: z.string().optional(), // sub-workflow ID for CompositeTask
});
```

---

## `TaskStatus` enum

```javascript
TaskStatus = {
  DISABLED: 'disabled', // defined but not yet enabled
  ENABLED: 'enabled', // ready to start
  ACTIVE: 'active', // currently executing
  COMPLETED: 'completed', // terminal — succeeded
  CANCELLED: 'cancelled', // terminal — cancelled
  FAILED: 'failed', // terminal — errored
  TIMEOUT: 'timeout', // terminal — timed out
};
```

### `VALID_TRANSITIONS`

Which status can transition to which:

```javascript
VALID_TRANSITIONS = {
  disabled: ['enabled', 'cancelled'],
  enabled: ['disabled', 'active', 'cancelled', 'timeout'],
  active: ['completed', 'cancelled', 'failed', 'timeout'],
  completed: [], // terminal
  cancelled: [], // terminal
  failed: [], // terminal
  timeout: [], // terminal
};
```

---

## `TaskInstanceSchema`

Runtime shape of a work item (task instance):

```typescript
TaskInstanceSchema = z.object({
  id: z.string().min(1), // UUID assigned at case start
  taskDefId: z.string().min(1), // references the task definition ID
  caseId: z.string().min(1),
  name: z.string().optional(),
  status: z
    .enum(['disabled', 'enabled', 'active', 'completed', 'cancelled', 'failed', 'timeout'])
    .default('disabled'),
  inputData: z.record(z.unknown()).default({}),
  outputData: z.record(z.unknown()).default({}),
  // ... timestamps added at runtime
});
```

---

## `TaskDefinition` class

Constructed by `createTaskDefinition(spec)`:

| Property        | Type                    | Description                                 |
| --------------- | ----------------------- | ------------------------------------------- |
| `id`            | `string`                | Task identifier                             |
| `kind`          | `string`                | `'AtomicTask'` \| `'CompositeTask'` \| etc. |
| `splitType`     | `string`                | Outgoing flow behaviour                     |
| `joinType`      | `string`                | Incoming flow behaviour                     |
| `preCondition`  | `Function \| undefined` | Pre-start validator                         |
| `postCondition` | `Function \| undefined` | Post-complete validator                     |

Methods added by `extendTaskDefinition`:

| Method                        | Returns   | Description                                  |
| ----------------------------- | --------- | -------------------------------------------- |
| `validatePreCondition(data)`  | `boolean` | Run `preCondition(data)`, throws on failure  |
| `validatePostCondition(data)` | `boolean` | Run `postCondition(data)`, throws on failure |

---

## `TaskInstance` class

Constructed by `createTaskInstance(taskDef, caseId, options?)`:

| Property           | Type                      | Description                      |
| ------------------ | ------------------------- | -------------------------------- |
| `id`               | `string`                  | UUID work item ID                |
| `taskDefId`        | `string`                  | Source definition ID             |
| `caseId`           | `string`                  | Owning case ID                   |
| `status`           | `TaskStatus`              | Current status                   |
| `enabledAt`        | `bigint \| null`          | KGC-4D nanosecond timestamp      |
| `startedAt`        | `bigint \| null`          | KGC-4D nanosecond timestamp      |
| `completedAt`      | `bigint \| null`          | KGC-4D nanosecond timestamp      |
| `inputData`        | `Record<string, unknown>` | Data at task start               |
| `outputData`       | `Record<string, unknown>` | Data produced on completion      |
| `assignedResource` | `string \| undefined`     | Allocated resource ID            |
| `receipts`         | `Receipt[]`               | Receipt chain for this work item |

Methods:

| Method                          | Returns   | Description                         |
| ------------------------------- | --------- | ----------------------------------- |
| `enable(actor?)`                | `Receipt` | Transition `disabled → enabled`     |
| `start(resourceId?, actor?)`    | `Receipt` | Transition `enabled → active`       |
| `complete(output?, actor?)`     | `Receipt` | Transition `active → completed`     |
| `cancel(actor?)`                | `Receipt` | Transition to `cancelled`           |
| `fail(error, actor?)`           | `Receipt` | Transition to `failed`              |
| `timeout()`                     | `Receipt` | Transition to `timeout`             |
| `validateTransition(newStatus)` | `boolean` | Check `VALID_TRANSITIONS`           |
| `computeStateHash()`            | `string`  | BLAKE3 of current state             |
| `verifyReceiptChain()`          | `boolean` | Verify all receipts chain correctly |

---

## `TransitionReceiptSchema`

Shape of a receipt produced by a task transition:

```typescript
TransitionReceiptSchema = z.object({
  id:                  z.string().uuid(),
  eventType:           z.enum(['TASK_ENABLED', 'TASK_STARTED', 'TASK_COMPLETED',
                                'TASK_CANCELLED', 'TASK_FAILED', 'TASK_TIMEOUT', ...]),
  t_ns:                z.bigint(),              // nanosecond timestamp
  timestamp_iso:       z.string(),             // ISO 8601
  caseId:              z.string().min(1),
  taskId:              z.string().min(1),
  workItemId:          z.string().optional(),
  previousReceiptHash: z.string().length(64).nullable(),  // BLAKE3 hex
  payloadHash:         z.string().length(64),             // BLAKE3 hex
  receiptHash:         z.string().length(64),             // BLAKE3 hex
  payload: z.object({
    decision:      z.string(),
    justification: z.object({...}).optional(),
    actor:         z.string().optional(),
    context:       z.any().optional(),
  }),
  kgcEventId: z.string().optional(),
  gitRef:     z.string().optional(),
  vectorClock: z.object({...}).optional(),
})
```

---

## `RECEIPT_EVENT_TYPES`

```javascript
RECEIPT_EVENT_TYPES = {
  CASE_CREATED: 'CASE_CREATED',
  TASK_ENABLED: 'TASK_ENABLED',
  TASK_STARTED: 'TASK_STARTED',
  TASK_COMPLETED: 'TASK_COMPLETED',
  TASK_CANCELLED: 'TASK_CANCELLED',
  TASK_FAILED: 'TASK_FAILED',
  TASK_TIMEOUT: 'TASK_TIMEOUT',
  WORK_ITEM_CREATED: 'WORK_ITEM_CREATED',
  CONTROL_FLOW_EVALUATED: 'CONTROL_FLOW_EVALUATED',
  RESOURCE_ALLOCATED: 'RESOURCE_ALLOCATED',
  RESOURCE_RELEASED: 'RESOURCE_RELEASED',
};
```

---

## `ENGINE_EVENTS`

All events emitted by `WorkflowEngine`. Subscribe with `engine.on(EVENT, handler)`:

```javascript
ENGINE_EVENTS = {
  TASK_ENABLED: 'task:enabled',
  TASK_STARTED: 'task:started',
  TASK_COMPLETED: 'task:completed',
  TASK_CANCELLED: 'task:cancelled',
  TASK_FAILED: 'task:failed',
  TASK_TIMEOUT: 'task:timeout',
  CASE_CREATED: 'case:created',
  CASE_STARTED: 'case:started',
  CASE_COMPLETED: 'case:completed',
  CASE_FAILED: 'case:failed',
  CASE_CANCELLED: 'case:cancelled',
  CASE_SUSPENDED: 'case:suspended',
  CASE_RESUMED: 'case:resumed',
  WORKFLOW_REGISTERED: 'workflow:registered',
  CIRCUIT_BREAKER_OPEN: 'circuit:open',
  CIRCUIT_BREAKER_CLOSE: 'circuit:close',
  CHECKPOINT_CREATED: 'checkpoint:created',
  RESOURCE_ALLOCATED: 'resource:allocated',
  RESOURCE_RELEASED: 'resource:released',
  OBSERVER_ERROR: 'observer:error',
  TIMER_REGISTERED: 'timer:registered',
  TIMER_CANCELLED: 'timer:cancelled',
  TIMER_EXPIRED: 'timer:expired',
  BULK_LAUNCH: 'bulk:launch',
  BULK_CANCEL: 'bulk:cancel',
};
```

### Event payloads

| Event                | Payload fields                                              |
| -------------------- | ----------------------------------------------------------- |
| `case:created`       | `{ caseId, workflowId, data }`                              |
| `case:started`       | `{ caseId, workflowId, taskId, workItemId }`                |
| `case:completed`     | `{ caseId, workflowId }`                                    |
| `task:enabled`       | `{ caseId, taskId, workItemId, actor? }`                    |
| `task:started`       | `{ caseId, workItemId, resourceId?, actor? }`               |
| `task:completed`     | `{ caseId, workItemId, output, actor?, downstreamEnabled }` |
| `task:failed`        | `{ caseId, workItemId, error }`                             |
| `resource:allocated` | `{ caseId, workItemId, resourceId, role? }`                 |
| `resource:released`  | `{ caseId, workItemId, resourceId }`                        |
| `circuit:open`       | `{ taskId, workflowId, failures }`                          |
| `checkpoint:created` | `{ caseId, checkpointId }`                                  |

---

## Receipt functions

| Function                             | Signature                                        | Description                   |
| ------------------------------------ | ------------------------------------------------ | ----------------------------- |
| `generateReceipt(event)`             | `async (ReceiptEvent) => Receipt`                | Generate a new BLAKE3 receipt |
| `verifyReceipt(receipt)`             | `async (Receipt) => VerificationResult`          | Verify hashes are correct     |
| `verifyChainLink(current, previous)` | `async (Receipt, Receipt) => VerificationResult` | Verify chain link             |

`VerificationResult` shape:

```typescript
{
  valid:  boolean,
  error?: string,
  checks?: {
    payloadHashValid:  boolean,
    chainHashValid:    boolean,
    timestampValid:    boolean,
  }
}
```

---

## Type guards

From `@unrdf/yawl/types`:

```javascript
isCaseStatus(value); // boolean
isWorkItemStatus(value); // boolean
isTaskKind(value); // boolean
isResourceType(value); // boolean
isValidCaseTransition(from, to); // boolean
isValidWorkItemTransition(from, to); // boolean
```

---

## See also

- [Workflow Schema Reference](./workflow-schema.md)
- [XOR Split/Join Reference](./xor-split-join.md)
- [Explanation: Task Contracts and Receipts](../explanation/02-task-contracts-and-receipts.md)
