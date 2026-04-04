# Explanation: The YAWL Pattern Language

This document explains what YAWL is, where the pattern language comes from, and how `@unrdf/yawl` implements it. Read this to understand the design, not to learn how to use it (see the [Tutorials](../tutorials/README.md) for that).

---

## What is YAWL?

YAWL stands for **Yet Another Workflow Language**. It is a formal workflow language designed by Wil van der Aalst and colleagues to be _expressively complete_ — meaning any workflow pattern that can be described can be expressed in YAWL without workarounds.

This is in contrast to many commercial BPM systems (including early BPMN versions) that could not represent certain combinations of split and join semantics without hacks. Van der Aalst and ter Hofstede identified 20 workflow patterns in 2003 and used them as a benchmark. YAWL was designed to pass all 20.

`@unrdf/yawl` implements those patterns in JavaScript, with RDF backing and cryptographic receipts layered on top.

---

## The token-flow model

YAWL is based on **Petri nets**, though you do not need to know Petri net theory to use it. The intuition is:

- A workflow specification is a directed graph of tasks and conditions
- When a case is created, a **token** is placed at the start condition
- The token flows through tasks as they execute
- When a task completes, it may produce multiple tokens (split) or consume multiple tokens (join)
- The case ends when all tokens are consumed at the end condition

In `@unrdf/yawl` this is implemented as work item state transitions rather than explicit token objects, but the semantics are the same.

---

## The four kinds of split

A **split** is a task with multiple outgoing flows. There are four kinds:

### Sequence (no split — WP1)

One outgoing flow. Token passes to exactly one successor. This is the default when `splitType` is not specified.

```
A ──→ B ──→ C
```

### AND-split (WP2: Parallel Split)

All outgoing flows fire simultaneously. A token is produced on every branch. Use when the downstream tasks are independent and can run concurrently.

```
A ──→ B
A ──→ C    (both B and C are enabled simultaneously)
```

In code: set `splitType: 'and'` on task A, or use `parallelSplit('A', ['B', 'C'])`.

### XOR-split (WP4: Exclusive Choice)

Exactly one outgoing flow fires based on condition evaluation. Conditions are evaluated in priority order. If no condition matches, the default flow (lowest priority, no condition) fires.

```
A ──condition1──→ B    (fires if condition1 is true)
A ──default─────→ C    (fires otherwise)
```

In code: set `splitType: 'xor'` on task A and attach `condition` functions to flows.

### OR-split (WP6: Multi-Choice)

One or more outgoing flows may fire. Each flow's condition is evaluated independently. At least one must fire. This is the most flexible split — use it when downstream tasks are partially dependent on data.

---

## The four kinds of join

A **join** is a task with multiple incoming flows. There are four kinds:

### Sequence (no join)

One incoming flow. Token passes straight through.

### AND-join (WP3: Synchronization)

The task waits until tokens arrive on **all** incoming flows before firing. This is the correct counterpart to AND-split — it re-synchronizes parallel branches.

```
B ──→ D    (D waits for both B and C)
C ──→ D
```

In code: set `joinType: 'and'` on task D, or use `synchronization(['B', 'C'], 'D')`.

### XOR-join (WP5: Simple Merge)

The task fires whenever a token arrives on **any** incoming flow. It does not wait. This is the correct counterpart to XOR-split — because only one branch was activated, only one token ever arrives.

**Important**: XOR-join with an AND-split is incorrect — multiple tokens would arrive and each would trigger the downstream task independently. Always match split and join types.

### OR-join (WP7: Structured Synchronizing Merge)

Waits for all **activated** incoming branches to complete. Unlike AND-join, it does not wait for branches that were never activated. This is the correct counterpart to OR-split.

---

## Why patterns must be matched

The most common mistake in workflow design is mismatching split and join types:

| Split                    | Correct join                 | Incorrect join                      |
| ------------------------ | ---------------------------- | ----------------------------------- |
| AND-split (all branches) | AND-join (wait for all)      | XOR-join (fires multiple times)     |
| XOR-split (one branch)   | XOR-join (passes through)    | AND-join (deadlock — waits forever) |
| OR-split (some branches) | OR-join (wait for activated) | AND-join (may deadlock)             |

`@unrdf/yawl` validates split/join consistency in `validateSplitJoinMatch(workflow)` and will report mismatches.

---

## Cancellation regions (WP19, WP20)

A **cancellation region** is a named set of tasks. When any task in the region completes, all other tasks in the region that are active or enabled are automatically cancelled.

```javascript
{
  tasks: [
    { id: 'approach-a', cancellationRegion: 'choice' },
    { id: 'approach-b', cancellationRegion: 'choice' },
  ],
  cancellationRegions: {
    'choice': ['approach-a', 'approach-b'],
  }
}
```

If `approach-a` completes first, `approach-b` is immediately cancelled. This implements WP16 (Deferred Choice) — the first path that actually starts determines which branch wins.

---

## How `@unrdf/yawl` extends the base model

The UNRDF implementation adds three layers on top of the YAWL model:

### 1. RDF backing

Every case, work item, and task definition has an RDF URI (`YAWL_CASE`, `YAWL_TASK`, `YAWL_WORK` namespaces). Workflow state can be persisted to an Oxigraph store and queried with SPARQL. This enables:

- SPARQL-driven eligibility queries for resource allocation
- Cross-case analytics
- Integration with the wider UNRDF knowledge graph

### 2. KGC-4D event sourcing

Every state transition is appended as an immutable event to the KGC-4D time-travel store. This means:

- Any case can be replayed from scratch using `replayCase()`
- Checkpoints allow point-in-time restores
- The full audit trail is queryable

### 3. BLAKE3 cryptographic receipts

Every task transition generates a `Receipt` — a data structure containing:

- A BLAKE3 hash of the transition payload
- A BLAKE3 hash chaining to the previous receipt
- The actor, timestamp (nanosecond precision), and justification

This creates an append-only, tamper-evident log per case. See [Task Contracts and Receipts](./02-task-contracts-and-receipts.md) for details.

---

## Patterns implemented

`@unrdf/yawl` implements the following patterns from Van der Aalst's catalogue:

| WP   | Name                                             | Status |
| ---- | ------------------------------------------------ | ------ |
| WP1  | Sequence                                         | Full   |
| WP2  | Parallel Split (AND-split)                       | Full   |
| WP3  | Synchronization (AND-join)                       | Full   |
| WP4  | Exclusive Choice (XOR-split)                     | Full   |
| WP5  | Simple Merge (XOR-join)                          | Full   |
| WP6  | Multi-Choice (OR-split)                          | Full   |
| WP7  | Structured Synchronizing Merge (OR-join)         | Full   |
| WP8  | Multi-Merge                                      | Full   |
| WP9  | Structured Discriminator                         | Full   |
| WP10 | Arbitrary Cycle                                  | Full   |
| WP11 | Implicit Termination                             | Full   |
| WP13 | Multiple Instances with Design-Time Knowledge    | Full   |
| WP14 | Multiple Instances without Design-Time Knowledge | Full   |
| WP15 | Multiple Instances with Runtime Knowledge        | Full   |
| WP16 | Deferred Choice                                  | Full   |
| WP19 | Cancel Task                                      | Full   |
| WP20 | Cancel Case                                      | Full   |

---

## See also

- [Reference: XOR Split/Join](../reference/xor-split-join.md) — full builder API
- [Tutorial: Conditional Branching](../tutorials/02-conditional-branching.md) — XOR-split in practice
- [Explanation: Task Contracts and Receipts](./02-task-contracts-and-receipts.md)
