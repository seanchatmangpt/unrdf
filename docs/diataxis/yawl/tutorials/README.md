# YAWL Tutorials

Tutorials are learning-oriented. You will build something real in each one, picking up the core concepts along the way.

**Start here if you have never used `@unrdf/yawl` before.**

---

## Tutorial list

| Tutorial                                                    | What you build                                               | Concepts covered                                             |
| ----------------------------------------------------------- | ------------------------------------------------------------ | ------------------------------------------------------------ |
| [01 — Your First Workflow](./01-first-workflow.md)          | A sequential three-task workflow run through the engine      | `WorkflowEngine`, `createWorkflow`, task lifecycle, receipts |
| [02 — Conditional Branching](./02-conditional-branching.md) | An expense approval process that routes to approve or reject | XOR-split, XOR-join, conditional flows, `exclusiveChoice`    |

---

## Prerequisites

- Node.js >= 18
- `pnpm add @unrdf/yawl` (or the workspace dep `@unrdf/yawl: 26.4.4`)
- Familiarity with async/await JavaScript

---

## What you will know after these tutorials

After completing both tutorials you will be able to:

- Define workflows with tasks and typed control flow
- Instantiate cases and drive them through the engine lifecycle
- Read and verify cryptographic receipts for each transition
- Apply XOR conditional branching to real decision problems

From there the [How-To Guides](../how-to/README.md) show you how to solve specific production problems.
