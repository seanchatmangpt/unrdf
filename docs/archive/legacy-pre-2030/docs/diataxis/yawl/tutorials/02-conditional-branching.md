# Tutorial: Conditional Branching

In this tutorial you will build an expense approval workflow that routes to an **approve** path or a **reject** path depending on the submitted amount. You will use the XOR-split pattern (exclusive choice) and see how YAWL evaluates conditions at runtime.

**Time**: 20–30 minutes
**Prerequisite**: Complete [Tutorial 01](./01-first-workflow.md) first.

---

## The Workflow You Will Build

```
submit-expense
      │
      ▼  (XOR-split)
    review ──── amount < 1000 ──▶ approve ──┐
             └── default    ──▶ reject  ──┤
                                            ▼
                                         notify
```

Four tasks. One decision point. One convergence point.

---

## Part 1: The XOR-Split Pattern

An **XOR-split** is a task with multiple outgoing flows where exactly one branch is taken. You declare it by:

1. Setting `splitType: 'xor'` on the source task
2. Adding `condition` functions to the flows (all but one, which serves as the default)

```javascript
import { WorkflowEngine, createWorkflow, sequence } from '@unrdf/yawl';

const expenseApproval = createWorkflow({
  id: 'expense-approval',
  name: 'Expense Approval',
  version: '1.0.0',

  tasks: [
    { id: 'submit', name: 'Submit Expense' },

    // The decision point — splitType marks it as XOR
    { id: 'review', name: 'Manager Review', splitType: 'xor' },

    { id: 'approve', name: 'Approve Expense' },
    { id: 'reject', name: 'Reject Expense' },
    { id: 'notify', name: 'Notify Employee' },
  ],

  flows: [
    // Before the split: plain sequence
    sequence('submit', 'review'),

    // Conditional branch: fires when amount < 1000
    {
      from: 'review',
      to: 'approve',
      condition: ctx => ctx.amount < 1000,
      priority: 1, // evaluated first
    },

    // Default branch: fires when no condition matched
    {
      from: 'review',
      to: 'reject',
      priority: 0, // lower priority = default
    },

    // After the split: both paths converge
    sequence('approve', 'notify'),
    sequence('reject', 'notify'),
  ],
});

const engine = new WorkflowEngine({ nodeId: 'expense-demo' });
engine.registerWorkflow(expenseApproval);

console.log('Workflow registered:', expenseApproval.id);
```

---

## Part 2: Run the Approval Path

Create a case with an amount below 1000 and watch it route to `approve`:

```javascript
// (continuing from Part 1 setup)

const { case: case1 } = await engine.createCase('expense-approval', {
  amount: 500,
  submittedBy: 'alice',
  description: 'Team lunch',
});

// --- submit ---
let [workItem] = case1.getEnabledWorkItems();
await engine.startTask(case1.id, workItem.id);
await engine.completeTask(case1.id, workItem.id, { submitted: true });

// --- review (XOR-split decision happens here) ---
[workItem] = case1.getEnabledWorkItems();
console.log('At review task:', workItem.taskDefId ?? workItem.id);
await engine.startTask(case1.id, workItem.id);

const { downstreamEnabled } = await engine.completeTask(case1.id, workItem.id, {
  verdict: 'review-complete',
});

// The engine evaluated the conditions against the case data
console.log(
  'Branch taken:',
  downstreamEnabled.map(d => d.taskId)
);
// Expected: [ 'approve' ]  (because amount=500 < 1000)

// --- approve ---
[workItem] = case1.getEnabledWorkItems();
await engine.startTask(case1.id, workItem.id);
await engine.completeTask(case1.id, workItem.id, { approvedAt: new Date().toISOString() });

// --- notify ---
[workItem] = case1.getEnabledWorkItems();
await engine.startTask(case1.id, workItem.id);
await engine.completeTask(case1.id, workItem.id, { notified: true });

console.log('Case status:', case1.status); // completed
```

---

## Part 3: Run the Rejection Path

Run the same workflow with `amount: 2500`:

```javascript
const { case: case2 } = await engine.createCase('expense-approval', {
  amount: 2500,
  submittedBy: 'bob',
  description: 'Conference trip',
});

// submit
let [wi] = case2.getEnabledWorkItems();
await engine.startTask(case2.id, wi.id);
await engine.completeTask(case2.id, wi.id, {});

// review — XOR evaluates conditions:
//   condition(ctx => ctx.amount < 1000) → false  (2500 >= 1000)
//   no other condition → default branch fires
wi = case2.getEnabledWorkItems()[0];
await engine.startTask(case2.id, wi.id);
const { downstreamEnabled } = await engine.completeTask(case2.id, wi.id, {});

console.log(
  'Branch taken:',
  downstreamEnabled.map(d => d.taskId)
);
// Expected: [ 'reject' ]

// reject → notify
for (const _ of [1, 2]) {
  const [item] = case2.getEnabledWorkItems();
  await engine.startTask(case2.id, item.id);
  await engine.completeTask(case2.id, item.id, {});
}

console.log('Rejection case status:', case2.status); // completed
```

---

## Part 4: Use the `exclusiveChoice` Builder

YAWL ships pattern builders so you don't have to write raw flow objects. The `exclusiveChoice()` builder sets `splitType: 'xor'` on the source task and returns a typed `PatternResult`:

```javascript
import { WorkflowEngine, createWorkflow, sequence, exclusiveChoice } from '@unrdf/yawl';

const workflow = createWorkflow({
  id: 'approval-v2',
  name: 'Approval v2',
  tasks: [
    { id: 'submit' },
    { id: 'review' },
    { id: 'approve' },
    { id: 'reject' },
    { id: 'notify' },
  ],
  flows: [
    sequence('submit', 'review'),

    // exclusiveChoice returns { pattern, sourceTask, flows }
    // Spread .flows into the workflow definition
    ...exclusiveChoice('review', [
      { taskId: 'approve', condition: ctx => ctx.amount < 1000, priority: 1 },
      { taskId: 'reject' },
    ]).flows,

    sequence('approve', 'notify'),
    sequence('reject', 'notify'),
  ],
});

console.log('Built with builder:', workflow.id);
```

The builder enforces at least 2 branches and validates task IDs at creation time rather than at runtime.

---

## Part 5: Inspect the Receipt Chain

Every task transition produces a chained BLAKE3 receipt. Let's verify the chain across two steps:

```javascript
import { verifyChainLink } from '@unrdf/yawl';

// (continuing from Part 2 example)
// The engine accumulates receipts on the case
const receipts = case1.receipts;

console.log('Total receipts:', receipts.length);

// Verify each link in the chain
for (let i = 1; i < receipts.length; i++) {
  const link = verifyChainLink(receipts[i], receipts[i - 1]);
  console.log(`Link ${i}: valid=${link.valid}`);
}

// Each receipt has a BLAKE3 hash and points to the previous one
const last = receipts.at(-1);
console.log('Last event type:', last.eventType);
console.log('Receipt hash (first 16):', last.receiptHash?.slice(0, 16));
console.log('Previous hash (first 16):', last.previousReceiptHash?.slice(0, 16));
```

The chain ensures no receipt can be inserted or removed without breaking the BLAKE3 hashes.

---

## What You Learned

You can now build branching workflows:

1. **Declare XOR** — set `splitType: 'xor'` on the decision task
2. **Attach conditions** — functions on flows receive the case context object
3. **Set priority** — higher priority flows are evaluated first; the last flow with no condition is the default
4. **Use builders** — `exclusiveChoice(sourceId, branches)` is the type-safe shorthand
5. **Verify receipts** — `verifyChainLink(receipt, previous)` checks the BLAKE3 chain

## Next Steps

- [How-To: Orchestrate with Daemon](../how-to/03-orchestrate-with-daemon.md) — drive YAWL workflows from a scheduled daemon
- [Reference: XOR Split/Join](../reference/xor-split-join.md) — full semantics and edge cases
- [Explanation: Task Contracts and Receipts](../explanation/02-task-contracts-and-receipts.md) — understand how receipts are generated
