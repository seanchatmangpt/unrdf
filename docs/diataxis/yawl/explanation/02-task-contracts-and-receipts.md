# Explanation: Task Contracts and Receipts

This document explains two related mechanisms in `@unrdf/yawl`:

1. **Task contracts** — the agreement between the workflow definition and the code that executes each task
2. **Receipts** — the cryptographic proof chain that records every state transition

Read this to understand why these mechanisms exist, not how to use them (see [How-To: Define a Task with a Contract](../how-to/01-define-task-with-contract.md) for that).

---

## What is a task contract?

A workflow definition specifies the _structure_ of work: which tasks exist, how they connect, what order they execute in. But a workflow definition does not specify what data a task needs or what data it produces. That is the contract.

A **task contract** is the interface between the workflow orchestrator and the code that executes a task:

```
workflow definition      contract                  worker code
────────────────────     ─────────────────────     ──────────────────────
task 'charge-card'  →→   input: { orderId,    →→   async chargeCard(input) {
                              amount,               // validate input
                              currency }            // call payment gateway
                         output: { charged,        // return output
                              transactionId,      }
                              chargedAt }
```

The contract lives in the task definition via `preCondition` and `postCondition` functions. When the engine starts a task, it passes the case data to `preCondition`. When the worker provides output, the engine passes it to `postCondition`. Both must return `true` (or not throw) for the transition to proceed.

### Why bother?

Without contracts, a task's input/output expectations live only in the worker's code — invisible to the workflow engine, undocumented in the workflow definition, and unverified at runtime. Contracts make implicit expectations explicit and shift validation to the workflow layer where it can be recorded in receipts.

### The trade-off

Strict contract enforcement adds latency (Zod parse on every transition) and creates coupling between the workflow definition and the data schema. In practice this is usually the right trade-off for auditable business processes, but for high-throughput batch workflows you might relax or skip post-conditions.

---

## What is a receipt?

A **receipt** is an immutable, tamper-evident record of a single state transition. Every call to `startTask`, `completeTask`, `failTask`, etc. produces a receipt.

The receipt contains:

| Field                   | Meaning                                                            |
| ----------------------- | ------------------------------------------------------------------ |
| `id`                    | UUID uniquely identifying this receipt                             |
| `eventType`             | `TASK_STARTED`, `TASK_COMPLETED`, etc.                             |
| `t_ns`                  | Nanosecond timestamp (from KGC-4D)                                 |
| `timestamp_iso`         | ISO 8601 string                                                    |
| `caseId`                | Which workflow case                                                |
| `taskId`                | Which task definition                                              |
| `workItemId`            | Which specific work item                                           |
| `payload.decision`      | The decision made (`ENABLE`, `START`, `COMPLETE`, etc.)            |
| `payload.justification` | Why the decision was made (SPARQL query, condition checked, actor) |
| `payload.actor`         | Who made the decision                                              |
| `payloadHash`           | BLAKE3 hash of the serialised payload                              |
| `receiptHash`           | BLAKE3 hash of this receipt including `previousReceiptHash`        |
| `previousReceiptHash`   | Hash of the prior receipt in the chain, or `null` for first        |

The last two fields form the chain.

---

## The BLAKE3 chain

Each receipt's `receiptHash` is computed over:

```
receiptHash = BLAKE3(
  id +
  eventType +
  t_ns.toString() +
  caseId +
  taskId +
  previousReceiptHash (or 'genesis') +
  payloadHash
)
```

Because `receiptHash` includes `previousReceiptHash`, you cannot:

- Insert a receipt into the middle of the chain (its `previousReceiptHash` would be wrong)
- Remove a receipt from the chain (the successor's `previousReceiptHash` would not match)
- Alter any field in a receipt without invalidating its own `receiptHash` and all successors

This creates an **append-only log** with the same integrity properties as a blockchain, but without the distributed consensus overhead. It is suitable for single-node audit trails and for UNRDF's multi-party workflows where receipts are compared across nodes.

---

## Why BLAKE3?

BLAKE3 is used throughout `@unrdf/yawl` (via `hash-wasm`) for several reasons:

- **Speed**: BLAKE3 is roughly 10x faster than SHA-256 on modern hardware
- **Security**: Provides 256-bit security strength with no known attacks
- **Streaming**: Supports incremental hashing for large payloads
- **Determinism**: Given the same inputs, always produces the same 64-character hex string

The `BLAKE3_HEX_LENGTH` constant (`64`) is used in schema validation to verify receipt hashes are the correct length.

---

## Justification: the reasoning layer

The `payload.justification` field records _why_ a decision was made, not just _what_ the decision was. This is particularly important for XOR and OR-split decisions:

```json
{
  "justification": {
    "sparqlQuery": "ASK { ... }",
    "queryResult": true,
    "conditionChecked": "amount < 1000",
    "reasoning": "Flow condition evaluated to true",
    "approvedBy": "manager@example.com"
  }
}
```

When the YAWL-Hooks integration (`createYAWLPolicyPack`) is used, the SPARQL queries run against the RDF store to determine eligibility. The query text and result are recorded in justification. This means an auditor can reproduce the exact decision by re-running the query against a snapshot of the store at the recorded timestamp.

---

## The `ProofChain` class

For cases that need explicit chain management, `ProofChain` provides a convenience wrapper:

```javascript
import { ProofChain } from '@unrdf/yawl/receipt';

const chain = new ProofChain();

const r1 = await chain.append({
  eventType: 'TASK_STARTED',
  caseId: 'case-1',
  taskId: 'charge-card',
  payload: { decision: 'START', actor: 'system' },
});

const r2 = await chain.append({
  eventType: 'TASK_COMPLETED',
  caseId: 'case-1',
  taskId: 'charge-card',
  payload: { decision: 'COMPLETE', actor: 'payment-service' },
});

const isValid = await chain.verify();
console.log('Chain valid:', isValid); // true
```

`ProofChain` keeps the previous hash in memory and passes it automatically to each `generateReceipt()` call.

---

## Where receipts are stored

Receipts are stored in two places:

1. **In-memory on the `YawlCase`**: `yawlCase.receipts` — an array of all receipts for the case, ordered by creation time. This is the primary access path during active execution.

2. **In the KGC-4D event log**: When `enableEventLog: true` (the default), each receipt is appended to the KGC-4D store as a typed event. This persists the chain across process restarts and enables `replayCase()`.

On restart, the engine can reconstruct a case by replaying its events from the KGC-4D log, re-verifying each receipt hash as it goes.

---

## What receipts are not

Receipts are **not** a substitute for application-level access control. They prove that a transition happened in a particular order, with particular data, attributed to a particular actor. They do not prevent an actor from making a fraudulent claim — they make such claims detectable after the fact.

For pre-transition enforcement, use `preCondition` functions or the YAWL-Hooks policy pack (`createYAWLPolicyPack`) which evaluates SPARQL-based eligibility before enabling tasks.

---

## See also

- [How-To: Define a Task with a Contract](../how-to/01-define-task-with-contract.md)
- [Reference: Task API — TransitionReceiptSchema](../reference/task-api.md#transitionreceiptschema)
- [Explanation: YAWL Pattern Language](./01-yawl-pattern-language.md)
