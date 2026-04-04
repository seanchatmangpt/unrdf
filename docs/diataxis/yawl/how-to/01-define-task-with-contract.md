# How-To: Define a Task with a Contract

A task **contract** specifies what data a task expects as input and what it must produce as output. YAWL validates both at runtime and includes the validation outcome in the task receipt.

This guide shows you how to attach pre-conditions and post-conditions to a task using Zod schemas.

---

## Use the `TaskDefinitionSchema` fields

The `TaskDefinitionSchema` accepts `preCondition` and `postCondition` functions:

```javascript
import { createWorkflow, WorkflowEngine, sequence } from '@unrdf/yawl';
import { z } from 'zod';

// Define schemas for input and output
const PaymentInput = z.object({
  orderId: z.string().min(1),
  amount: z.number().positive(),
  currency: z.string().length(3),
});

const PaymentOutput = z.object({
  charged: z.boolean(),
  transactionId: z.string(),
  chargedAt: z.string().datetime(),
});

const workflow = createWorkflow({
  id: 'payment-workflow',
  name: 'Payment Processing',
  tasks: [
    { id: 'start', name: 'Start' },
    {
      id: 'charge',
      name: 'Charge Card',

      // Pre-condition: called before the task starts
      // Return true to allow, false (or throw) to block
      preCondition: inputData => {
        const result = PaymentInput.safeParse(inputData);
        if (!result.success) {
          throw new Error(`Invalid input: ${result.error.message}`);
        }
        return true;
      },

      // Post-condition: called after the task completes
      // Validates the output data produced by the worker
      postCondition: outputData => {
        const result = PaymentOutput.safeParse(outputData);
        if (!result.success) {
          throw new Error(`Invalid output: ${result.error.message}`);
        }
        return true;
      },
    },
    { id: 'end', name: 'End' },
  ],
  flows: [sequence('start', 'charge'), sequence('charge', 'end')],
});
```

---

## Pass input data when starting a task

Input data is the `initialData` you provide to `createCase`, and any per-task data you pass as options. The pre-condition receives the merged data available at task start time:

```javascript
const engine = new WorkflowEngine({ nodeId: 'payment-node' });
engine.registerWorkflow(workflow);

// Case data becomes the context for conditions
const { case: paymentCase } = await engine.createCase('payment-workflow', {
  orderId: 'ORD-555',
  amount: 99.99,
  currency: 'USD',
});

// Skip 'start', go straight to 'charge'
let [wi] = paymentCase.getEnabledWorkItems();
await engine.startTask(paymentCase.id, wi.id); // start task
await engine.completeTask(paymentCase.id, wi.id, {});

// Charge task is now enabled
[wi] = paymentCase.getEnabledWorkItems();
await engine.startTask(paymentCase.id, wi.id);

// completeTask with output that satisfies PaymentOutput
await engine.completeTask(paymentCase.id, wi.id, {
  charged: true,
  transactionId: 'TXN-9988',
  chargedAt: new Date().toISOString(),
});
```

---

## Handle a failing pre-condition

If the pre-condition throws, `startTask` rejects and the work item stays in `enabled`:

```javascript
const { case: badCase } = await engine.createCase('payment-workflow', {
  orderId: '', // invalid — empty string
  amount: -1, // invalid — negative
  currency: 'USDX', // invalid — not 3 chars
});

let [wi] = badCase.getEnabledWorkItems();
await engine.startTask(badCase.id, wi.id); // skip start task

[wi] = badCase.getEnabledWorkItems();
await engine.startTask(badCase.id, wi.id); // start charge

try {
  await engine.completeTask(badCase.id, wi.id, {
    charged: false,
    // transactionId missing — fails PaymentOutput
  });
} catch (err) {
  console.error('Post-condition failed:', err.message);
  // Work item reverts to 'active', not completed
}
```

---

## Use `inputConditions` and `outputConditions` (array form)

For simpler string-based conditions you can use the `inputConditions` / `outputConditions` arrays on `TaskDefinitionSchema`. These are string labels recorded in the receipt but not evaluated by default — they serve as documented contract obligations:

```javascript
{
  id: 'validate-order',
  name: 'Validate Order',
  inputConditions:  ['order.id is non-empty', 'order.amount > 0'],
  outputConditions: ['validation.passed is boolean'],
}
```

The strings appear verbatim in the receipt's `payload.justification.conditionChecked` field.

---

## What the receipt records

After a task with conditions completes, the receipt payload includes:

```json
{
  "eventType": "TASK_COMPLETED",
  "caseId": "...",
  "taskId": "charge",
  "payload": {
    "decision": "COMPLETE",
    "justification": {
      "conditionChecked": "preCondition + postCondition",
      "reasoning": "Both conditions passed"
    },
    "actor": "system"
  },
  "payloadHash": "...",
  "receiptHash": "..."
}
```

---

## See also

- [Reference: Task API](../reference/task-api.md) — full `TaskDefinitionSchema` fields
- [Explanation: Task Contracts and Receipts](../explanation/02-task-contracts-and-receipts.md) — why contracts matter for audit trails
