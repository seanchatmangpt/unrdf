# @unrdf/yawl

![Version](https://img.shields.io/badge/version-5.0.0-blue) ![Production Ready](https://img.shields.io/badge/production-ready-green)

**YAWL (Yet Another Workflow Language) - Enterprise Workflow Engine with Time-Travel and Cryptographic Receipts**

A production-ready implementation of Van der Aalst's YAWL workflow patterns with KGC-4D event sourcing, providing verifiable workflow execution with cryptographic receipts and time-travel capabilities.

## Features

- **20 YAWL Workflow Patterns**: Complete implementation of Van der Aalst's control flow patterns (WP1-WP20)
- **Hook-Native Architecture**: Policy enforcement and validation with declarative SPARQL rules
- **Event Sourcing**: KGC-4D time-travel with cryptographic receipts for every state transition
- **RDF-Native**: All workflow state stored as RDF triples with SPARQL queries
- **Cryptographic Receipts**: BLAKE3 hash chains for tamper-proof audit trails
- **Resource Management**: Role-based allocation with capability constraints
- **Cancellation Regions**: Advanced error handling with transactional rollback
- **Time-Travel & Replay**: Reconstruct workflow state at any point in time
- **Type-Safe**: Full JSDoc types with Zod runtime validation

## Installation

```bash
pnpm add @unrdf/yawl @unrdf/oxigraph
```

## Quick Start

Create and execute a simple sequential workflow in under 2 minutes:

```javascript
import { createWorkflow, createCase, enableTask, startTask, completeTask } from '@unrdf/yawl';
import { createStore } from '@unrdf/oxigraph';

// 1. Create RDF store for workflow state
const store = createStore();

// 2. Define workflow with 3 sequential tasks
const workflowReceipt = await createWorkflow(store, {
  id: 'doc-approval',
  name: 'Document Approval Workflow',
  tasks: [
    { id: 'draft', name: 'Create Draft', kind: 'atomic' },
    { id: 'review', name: 'Review Document', kind: 'atomic' },
    { id: 'publish', name: 'Publish Document', kind: 'atomic' },
  ],
  flow: [
    { from: 'draft', to: 'review' },
    { from: 'review', to: 'publish' },
  ],
});

console.log('Workflow created:', workflowReceipt.workflow_id);

// 3. Start a workflow case (instance)
const caseReceipt = await createCase(store, {
  workflowId: workflowReceipt.workflow_id,
  caseId: 'case-001',
});

console.log('Case started:', caseReceipt.case_id);

// 4. Execute tasks
const enableReceipt = await enableTask(store, {
  caseId: caseReceipt.case_id,
  taskId: 'draft',
});

const startReceipt = await startTask(store, {
  caseId: caseReceipt.case_id,
  workItemId: enableReceipt.work_item_id,
});

const completeReceipt = await completeTask(store, {
  caseId: caseReceipt.case_id,
  workItemId: startReceipt.work_item_id,
  outputData: { documentContent: 'Final draft content...' },
});

console.log('Task completed:', completeReceipt.receipt_id);
console.log('Next tasks enabled:', completeReceipt.enabled_tasks);
```

**Output:**

```
Workflow created: doc-approval
Case started: case-001
Task completed: receipt-abc123
Next tasks enabled: ['review']
```

## Architecture

### Modular Structure

The package is organized into focused modules:

- **`api/`** - Workflow API (creation, execution, queries, cancellation, time-travel)
- **`engine/`** - YAWL engine subsystems (core, events, hooks, health, snapshots, queries)
- **`workflow/`** - Workflow class (core, validation, patterns, RDF)
- **`case/`** - Case management (core, lifecycle, RDF serialization)
- **`task/`** - Task execution (lifecycle, state transitions)
- **`receipt.mjs`** - Cryptographic receipts (BLAKE3 hash chains)
- **`resources/`** - Resource management (participants, tools, roles, capacity)
- **`hooks/`** - Policy enforcement hooks (validation, authorization)
- **`events/`** - Event sourcing (KGC-4D integration)
- **`cancellation/`** - Cancellation handling (regions, rollback)

### Workflow Patterns

Supports all 20 Van der Aalst patterns:

- **WP1**: Sequence
- **WP2**: Parallel Split (AND-split)
- **WP3**: Synchronization (AND-join)
- **WP4**: Exclusive Choice (XOR-split)
- **WP5**: Simple Merge (XOR-join)
- **WP6**: Multi-Choice (OR-split)
- **WP7**: Structured Synchronizing Merge (OR-join)
- **WP8-WP20**: Advanced patterns (cancellation, iteration, state-based routing, milestones, etc.)

See [YAWL Patterns Documentation](../../docs/guides/yawl-patterns.md) for detailed examples.

---

## Core API

### Workflow Lifecycle

```javascript
// Create workflow definition
const { workflow_id } = await createWorkflow(store, {
  id: 'my-workflow',
  name: 'My Workflow',
  tasks: [{ id: 'task1', name: 'Task 1', kind: 'atomic' }],
  flow: [],
});

// Start case (workflow instance)
const { case_id } = await createCase(store, {
  workflowId: workflow_id,
  caseId: 'case-123',
  initialData: { key: 'value' },
});
```

### Task Execution

```javascript
// Enable task (make available for execution)
const { work_item_id } = await enableTask(store, {
  caseId: case_id,
  taskId: 'task1',
});

// Start work item (claim task)
const startReceipt = await startTask(store, {
  caseId: case_id,
  workItemId: work_item_id,
  actor: 'user@example.com',
});

// Complete work item (finish task)
const completeReceipt = await completeTask(store, {
  caseId: case_id,
  workItemId: work_item_id,
  outputData: { result: 'success' },
});
```

### Time Travel & Replay

```javascript
import { replayCase } from '@unrdf/yawl';

// Replay case from event log
const caseState = await replayCase(store, {
  caseId: case_id,
  asOfTime: '2024-12-25T10:30:00Z', // Optional: replay to specific time
});

console.log('Case status:', caseState.status);
console.log('Active work items:', caseState.active_work_items);
console.log('Completed tasks:', caseState.completed_tasks);
```

## Common Workflow Patterns

### Pattern 1: Sequential Workflow

```javascript
const workflow = {
  id: 'sequential',
  name: 'Sequential Approval',
  tasks: [
    { id: 'submit', name: 'Submit', kind: 'atomic' },
    { id: 'review', name: 'Review', kind: 'atomic' },
    { id: 'approve', name: 'Approve', kind: 'atomic' },
  ],
  flow: [
    { from: 'submit', to: 'review' },
    { from: 'review', to: 'approve' },
  ],
};
```

**Flow:** `Submit → Review → Approve`

### Pattern 2: Parallel Workflow (AND-split/AND-join)

```javascript
const workflow = {
  id: 'parallel-approval',
  name: 'Parallel Multi-Reviewer Approval',
  tasks: [
    { id: 'submit', name: 'Submit Document', kind: 'atomic' },
    { id: 'legal-review', name: 'Legal Review', kind: 'atomic' },
    { id: 'tech-review', name: 'Technical Review', kind: 'atomic' },
    { id: 'finance-review', name: 'Finance Review', kind: 'atomic' },
    { id: 'finalize', name: 'Finalize', kind: 'atomic' },
  ],
  flow: [
    // AND-split: one task → multiple parallel tasks
    { from: 'submit', to: 'legal-review', splitType: 'AND' },
    { from: 'submit', to: 'tech-review', splitType: 'AND' },
    { from: 'submit', to: 'finance-review', splitType: 'AND' },

    // AND-join: wait for all to complete
    { from: 'legal-review', to: 'finalize', joinType: 'AND' },
    { from: 'tech-review', to: 'finalize', joinType: 'AND' },
    { from: 'finance-review', to: 'finalize', joinType: 'AND' },
  ],
};
```

**Flow:**

```
                → Legal Review   →
Submit Document → Tech Review    → Finalize
                → Finance Review →
```

### Pattern 3: Exclusive Choice (XOR-split)

```javascript
const workflow = {
  id: 'conditional-workflow',
  name: 'Conditional Processing',
  tasks: [
    { id: 'check', name: 'Check Amount', kind: 'atomic' },
    { id: 'auto-approve', name: 'Auto Approve', kind: 'atomic' },
    { id: 'manual-review', name: 'Manual Review', kind: 'atomic' },
    { id: 'done', name: 'Done', kind: 'atomic' },
  ],
  flow: [
    // XOR-split: choose one path based on condition
    {
      from: 'check',
      to: 'auto-approve',
      splitType: 'XOR',
      condition: { amount_lt: 1000 },
    },
    {
      from: 'check',
      to: 'manual-review',
      splitType: 'XOR',
      condition: { amount_gte: 1000 },
    },

    // XOR-join: merge paths
    { from: 'auto-approve', to: 'done', joinType: 'XOR' },
    { from: 'manual-review', to: 'done', joinType: 'XOR' },
  ],
};
```

**Flow:**

```
           → Auto Approve (if amount < 1000) →
Check Amount                                    Done
           → Manual Review (if amount ≥ 1000) →
```

### Pattern 4: OR-split (Multi-Choice)

```javascript
const workflow = {
  id: 'notification-workflow',
  name: 'Multi-Channel Notification',
  tasks: [
    { id: 'event', name: 'Event Triggered', kind: 'atomic' },
    { id: 'email', name: 'Send Email', kind: 'atomic' },
    { id: 'sms', name: 'Send SMS', kind: 'atomic' },
    { id: 'push', name: 'Send Push Notification', kind: 'atomic' },
    { id: 'complete', name: 'Complete', kind: 'atomic' },
  ],
  flow: [
    // OR-split: enable multiple tasks based on data
    { from: 'event', to: 'email', splitType: 'OR', condition: { email_enabled: true } },
    { from: 'event', to: 'sms', splitType: 'OR', condition: { sms_enabled: true } },
    { from: 'event', to: 'push', splitType: 'OR', condition: { push_enabled: true } },

    // OR-join: wait for all enabled paths
    { from: 'email', to: 'complete', joinType: 'OR' },
    { from: 'sms', to: 'complete', joinType: 'OR' },
    { from: 'push', to: 'complete', joinType: 'OR' },
  ],
};
```

### Pattern 5: Cancellation Regions

```javascript
import { createWorkflow, createCase, enableTask, cancelWorkItem } from '@unrdf/yawl';

const workflow = {
  id: 'order-processing',
  name: 'Order Processing with Cancellation',
  tasks: [
    { id: 'place-order', name: 'Place Order', kind: 'atomic' },
    { id: 'payment', name: 'Process Payment', kind: 'atomic' },
    { id: 'ship', name: 'Ship Order', kind: 'atomic' },
    { id: 'cancel', name: 'Cancel Order', kind: 'atomic' },
  ],
  flow: [
    { from: 'place-order', to: 'payment' },
    { from: 'payment', to: 'ship' },
    // Cancellation can happen anytime before shipping
    { from: 'place-order', to: 'cancel' },
  ],
  cancellationRegions: [
    {
      id: 'order-region',
      tasks: ['payment', 'ship'],
      cancelTrigger: 'cancel',
    },
  ],
};

// Cancel an order
await cancelWorkItem(store, {
  caseId: case_id,
  workItemId: payment_work_item_id,
  reason: 'Customer requested cancellation',
});
```

## Error Handling

All YAWL API functions return cryptographic receipts with detailed error information:

```javascript
import { completeTask } from '@unrdf/yawl';

try {
  const receipt = await completeTask(store, {
    caseId: 'case-123',
    workItemId: 'wi-456',
    outputData: { result: 'success' },
  });

  if (receipt.decision === 'ACCEPT') {
    console.log('Task completed successfully');
    console.log('Enabled next tasks:', receipt.enabled_tasks);
  } else if (receipt.decision === 'REJECT') {
    console.error('Task completion rejected:', receipt.justification);
  }
} catch (error) {
  if (error.code === 'INVALID_STATE_TRANSITION') {
    console.error('Cannot complete task in current state:', error.message);
    console.error('Current status:', error.details.current_status);
    console.error('Required status:', error.details.required_status);
  } else if (error.code === 'VALIDATION_ERROR') {
    console.error('Invalid input data:', error.details.validation_errors);
  } else {
    console.error('Unexpected error:', error);
  }
}
```

## Examples

See runnable examples in the [examples directory](../../examples/yawl/):

- **[01-simple-sequential.mjs](../../examples/yawl/01-simple-sequential.mjs)** - Simple 3-task sequential workflow (2 min)
- **[02-parallel-approval.mjs](../../examples/yawl/02-parallel-approval.mjs)** - Parallel review workflow with AND-split/join
- **[03-conditional-routing.mjs](../../examples/yawl/03-conditional-routing.mjs)** - XOR-split with conditional routing
- **[04-cancellation-regions.mjs](../../examples/yawl/04-cancellation-regions.mjs)** - Error handling with cancellation
- **[05-time-travel.mjs](../../examples/yawl/05-time-travel.mjs)** - Event replay and time-travel queries

## Advanced Features

### Cryptographic Receipts

Every state transition returns a cryptographic receipt with BLAKE3/SHA-256 hash chain:

```javascript
const receipt = await completeTask(store, {
  caseId: 'case-123',
  workItemId: 'wi-456',
});

console.log({
  receipt_id: receipt.receipt_id, // Unique receipt ID
  event_id: receipt.event_id, // Event that triggered transition
  hash: receipt.hash, // BLAKE3/SHA-256 hash
  previous_hash: receipt.previous_hash, // Previous receipt hash (chain)
  timestamp: receipt.timestamp, // ISO 8601 timestamp
  decision: receipt.decision, // ACCEPT | REJECT
  justification: receipt.justification, // Human-readable explanation
  enabled_tasks: receipt.enabled_tasks, // Next tasks enabled
  state: receipt.state, // Complete state snapshot
});
```

### SPARQL Queries

Query workflow state directly with SPARQL:

```javascript
import { executeSparqlSelect } from '@unrdf/yawl';

const results = await executeSparqlSelect(
  store,
  `
  PREFIX yawl: <http://yawl.sourceforge.net/ontology/>

  SELECT ?task ?status ?actor ?startedAt
  WHERE {
    ?workItem yawl:caseRef <urn:yawl:case:case-123> ;
              yawl:taskRef ?task ;
              yawl:status ?status ;
              yawl:startedBy ?actor ;
              yawl:startedAt ?startedAt .
  }
  ORDER BY ?startedAt
`
);

results.forEach(row => {
  console.log(`${row.task}: ${row.status} by ${row.actor} at ${row.startedAt}`);
});
```

### Resource Allocation

Assign tasks to roles with capability constraints:

```javascript
import { createResourceManager, createRole, createParticipant } from '@unrdf/yawl';

const resourceMgr = createResourceManager(store);

// Define roles
await resourceMgr.addRole({
  id: 'reviewer',
  name: 'Document Reviewer',
  capabilities: ['document:review', 'document:approve'],
});

// Assign users to roles
await resourceMgr.addParticipant({
  id: 'alice',
  name: 'Alice Smith',
  email: 'alice@example.com',
  roles: ['reviewer'],
});

// Create workflow with resource constraints
const workflow = {
  id: 'doc-workflow',
  tasks: [
    {
      id: 'review',
      name: 'Review Document',
      kind: 'atomic',
      resourceConstraints: {
        role: 'reviewer',
        requiredCapabilities: ['document:review'],
      },
    },
  ],
  flow: [],
};
```

## API Reference

### Core Functions

| Function                         | Description                    | Returns                                      |
| -------------------------------- | ------------------------------ | -------------------------------------------- |
| `createWorkflow(store, spec)`    | Create workflow definition     | `{ workflow_id, receipt_id, hash }`          |
| `createCase(store, options)`     | Start workflow instance        | `{ case_id, receipt_id, enabled_tasks }`     |
| `enableTask(store, options)`     | Enable task for execution      | `{ work_item_id, receipt_id }`               |
| `startTask(store, options)`      | Claim and start task           | `{ receipt_id, started_at }`                 |
| `completeTask(store, options)`   | Complete task with output data | `{ receipt_id, enabled_tasks, case_status }` |
| `cancelWorkItem(store, options)` | Cancel in-progress task        | `{ receipt_id, cancelled_tasks }`            |
| `replayCase(store, options)`     | Replay case from events        | `{ status, work_items, history }`            |

### Constants

```javascript
import {
  WORK_ITEM_STATUS, // enabled, fired, started, completed, cancelled
  YAWL_EVENT_TYPES, // workflow.created, case.created, task.completed
  CONTROL_FLOW_PATTERNS, // XOR, AND, OR
} from '@unrdf/yawl';
```

### Schemas (Zod)

```javascript
import {
  WorkflowSpecSchema, // Validate workflow definitions
  TaskSchema, // Validate task definitions
  ControlFlowSchema, // Validate control flow
  ReceiptSchema, // Validate receipts
} from '@unrdf/yawl';

// Validate workflow before creation
const validatedSpec = WorkflowSpecSchema.parse(myWorkflowSpec);
```

## Use Cases

### 1. Document Approval Workflows

Multi-stage approval with parallel reviewers and conditional routing based on document type, size, or content.

### 2. E-commerce Order Processing

Order placement → Payment → Inventory → Shipping → Delivery with cancellation support at each stage.

### 3. DevOps CI/CD Pipelines

Code commit → Build → Test → Security Scan → Deploy with parallel test suites and conditional deployment.

### 4. Healthcare Patient Care Pathways

Patient admission → Diagnosis → Treatment Plan → Procedure → Recovery with dynamic routing based on patient condition.

### 5. Financial Transaction Processing

Transaction submitted → Fraud detection → Compliance check → Settlement with multi-level approvals for large amounts.

## Performance

Benchmarked on Apple M1 Pro (8 cores):

- **Workflow creation**: ~5ms per workflow
- **Case start**: ~3ms per case
- **Task completion**: ~2ms per task
- **Event replay**: ~50ms for 1000 events
- **SPARQL query**: ~1-10ms (simple queries)

Store 100,000+ workflow events with <100MB memory footprint.

## Daemon Integration

YAWL integrates with `@unrdf/daemon` for advanced workflow automation including scheduled case creation, task timeout enforcement, automatic retries, and parallel task distribution.

### Features

- **Scheduled Case Creation** - Create workflow cases on cron schedules or intervals
- **Task Timeout Enforcement** - Automatically cancel tasks exceeding time limits
- **Automatic Retry** - Retry failed tasks with exponential backoff
- **Deferred Choice** - Wait for external events before proceeding
- **Parallel Distribution** - Distribute AND-split tasks across daemon nodes
- **Health Monitoring** - Real-time metrics and health checks

### Quick Example

```javascript
import { Daemon } from '@unrdf/daemon';
import { YawlDaemonBridge } from '@unrdf/daemon/integrations/yawl';
import { createWorkflow } from '@unrdf/yawl';
import { createStore } from '@unrdf/oxigraph';

// Create daemon and YAWL engine
const daemon = new Daemon({ daemonId: 'yawl-daemon' });
const store = createStore();

// Mock YAWL engine (in production, use real engine)
const yawlEngine = {
  createCase: async ({ workflowId, caseId }) => ({ caseId, workflowId }),
  enableTask: async ({ caseId, taskId }) => ({ caseId, taskId }),
  on: (event, handler) => () => {},
};

// Create bridge
const bridge = new YawlDaemonBridge(daemon, yawlEngine, {
  daemonNodeId: 'node-1',
  maxConcurrentCases: 100,
  enableAutoRetry: true,
  enableTimeoutTracking: true,
});

await daemon.start();
await bridge.start();

// Schedule daily case creation at 2 AM
await bridge.scheduleRecurringCase(
  'approval-workflow',
  '0 2 * * *',
  { caseIdPrefix: 'daily', priority: 5 }
);

// Watch task for timeout
await bridge.watchTaskTimeout('case-001', 'review-task', 60000);

// Get bridge statistics
const stats = bridge.getStats();
console.log('Active timeouts:', stats.activeTimeouts);
console.log('Active retries:', stats.activeRetries);
```

### CLI Management

Control daemon operations via CLI:

```bash
# Start daemon (programmatically)
# See tutorial for full daemon setup

# List scheduled operations
unrdf daemon list

# Execute operation immediately
unrdf daemon run backup-graphs

# Schedule trigger
unrdf daemon schedule sync-federation cron --payload '{"schedule":"0 * * * *"}'

# View daemon status
unrdf daemon status --include-metrics

# View operation logs
unrdf daemon logs --follow --filter "yawl"

# Show configuration
unrdf daemon config

# Show cluster status
unrdf daemon cluster
```

### Documentation

- **[Tutorial: YAWL Daemon Setup](../../docs/diataxis/tutorials/yawl-daemon-setup.md)** - Complete setup guide (15-20 min)
- **[How-to: Daemon Management](../../docs/diataxis/how-to/yawl-daemon-management.md)** - Common tasks (5-10 min)
- **[Reference: Daemon API](../../docs/diataxis/reference/yawl-daemon-api.md)** - Complete API reference
- **[Example: Daemon Workflow](../../examples/yawl-daemon-workflow.mjs)** - Working example

## Dependencies

- `@unrdf/oxigraph` - RDF triple store (required)
- `@unrdf/kgc-4d` - Event sourcing with time-travel
- `@unrdf/hooks` - Policy enforcement
- `@unrdf/daemon` - Background task scheduler (optional, for daemon integration)
- `zod` - Runtime validation
- `hash-wasm` - BLAKE3/SHA-256 hashing

## When to Use @unrdf/yawl

**Use YAWL for:**

- ✅ Complex workflows with parallel/conditional routing
- ✅ Audit requirements (cryptographic receipts)
- ✅ Time-travel / replay capabilities needed
- ✅ RDF-native applications
- ✅ Policy-driven workflows

**Don't use YAWL for:**

- ❌ Simple linear task lists (use array)
- ❌ Real-time event processing (use @unrdf/streaming)
- ❌ State machines with loops (use finite automaton)
- ❌ Performance-critical paths (<1ms latency)

## Documentation

- **[Quick Start Guide](../../docs/guides/yawl-quickstart.md)** - Get started in 10 minutes
- **[YAWL Patterns](../../docs/guides/yawl-patterns.md)** - All 20 control flow patterns
- **[Event Sourcing](../../docs/guides/yawl-event-sourcing.md)** - Time-travel and receipts
- **[Resource Management](../../docs/guides/yawl-resources.md)** - Role-based allocation
- **[API Reference](https://unrdf.dev/docs/yawl)** - Full API documentation
- **[MIGRATION.md](./MIGRATION.md)** - Migration guide and architecture details

## Development

```bash
# Install dependencies
pnpm install

# Run tests
pnpm test

# Run tests with coverage
pnpm test:coverage

# Watch mode
pnpm test:watch

# Run linter
pnpm lint

# Run full validation
pnpm validate
```

## Testing

```bash
# Run tests
pnpm test

# Watch mode
pnpm test:watch

# Coverage report
pnpm test:coverage
```

## Contributing

See [MIGRATION.md](./MIGRATION.md) for architecture details and contribution guidelines.

## License

MIT

---

**Questions?** See [UNRDF Documentation](https://github.com/unrdf/unrdf) or open an issue.
