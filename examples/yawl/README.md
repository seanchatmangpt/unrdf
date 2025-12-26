# YAWL Examples

This directory contains runnable examples demonstrating YAWL workflow patterns and features.

## Quick Start

All examples are standalone and runnable with Node.js:

```bash
node examples/yawl/01-simple-sequential.mjs
```

## Examples

### 1. Simple Sequential Workflow

**File:** `01-simple-sequential.mjs`
**Time:** 2 minutes
**Pattern:** Sequential execution (WP-1)

Learn the basics: create a workflow, start a case, execute tasks in order.

**Flow:**

```
Create Draft → Review Document → Publish Document
```

### 2. Parallel Approval Workflow

**File:** `02-parallel-approval.mjs`
**Time:** 3 minutes
**Pattern:** AND-split (WP-2) + AND-join (WP-3)

Multiple reviewers approve in parallel. Workflow continues only after ALL reviews complete.

**Flow:**

```
                → Legal Review   →
Submit Document → Tech Review    → Finalize
                → Finance Review →
```

### 3. Conditional Routing

**File:** `03-conditional-routing.mjs`
**Time:** 3 minutes
**Pattern:** XOR-split (WP-4) + XOR-join (WP-5)

Route workflow based on data conditions. Small amounts auto-approve, large amounts require manual review.

**Flow:**

```
           → Auto Approve (if amount < $1000) →
Check Amount                                     Done
           → Manual Review (if amount ≥ $1000) →
```

### 4. Cancellation Regions

**File:** `04-cancellation-regions.mjs`
**Time:** 4 minutes
**Pattern:** Cancel Region (WP-19)

Handle errors and cancellations with transactional rollback. E-commerce order that can be cancelled before shipping.

**Flow:**

```
Place Order → Process Payment → Ship Order
                   ↓
              Cancel Order → Refund
```

### 5. Time Travel and Event Replay

**File:** `05-time-travel.mjs`
**Time:** 3 minutes
**Feature:** Event sourcing, cryptographic receipts

Query workflow state at any point in time. Reconstruct audit trails with cryptographic verification.

**Demonstrates:**

- Event sourcing with KGC-4D
- Replaying cases from event log
- Time-travel queries
- Receipt chain verification

## Running Examples

Each example is self-contained and includes detailed console output:

```bash
# Run a specific example
node examples/yawl/01-simple-sequential.mjs

# Expected output:
# ================================================================================
# YAWL Example 1: Simple Sequential Workflow
# ================================================================================
#
# Step 1: Creating RDF store...
# ✅ Store created
# ...
```

## Learning Path

Recommended order for learning YAWL:

1. **01-simple-sequential.mjs** - Start here! Learn the basics
2. **02-parallel-approval.mjs** - Understand parallel execution
3. **03-conditional-routing.mjs** - Learn conditional branching
4. **04-cancellation-regions.mjs** - Handle errors and cancellations
5. **05-time-travel.mjs** - Explore event sourcing and auditing

## Key Concepts

### Workflow Patterns

YAWL implements 20 control flow patterns from Van der Aalst's research:

- **WP-1**: Sequence - Tasks execute one after another
- **WP-2**: Parallel Split (AND-split) - Enable multiple tasks simultaneously
- **WP-3**: Synchronization (AND-join) - Wait for all paths to complete
- **WP-4**: Exclusive Choice (XOR-split) - Choose exactly one path
- **WP-5**: Simple Merge (XOR-join) - Merge exclusive paths
- **WP-6**: Multi-Choice (OR-split) - Enable subset of paths based on conditions
- **WP-7**: Structured Synchronizing Merge (OR-join) - Wait for enabled paths
- **WP-19**: Cancel Region - Cancel in-progress tasks

### Task Lifecycle

Every task goes through these states:

```
enabled → fired → started → completed
                      ↓
                  cancelled
```

### Cryptographic Receipts

Every state transition returns a receipt with:

- Unique receipt ID
- SHA-256 hash
- Previous receipt hash (chain)
- Timestamp
- Actor
- Decision (ACCEPT/REJECT)
- Justification

## Common Patterns

### Sequential Tasks

```javascript
flow: [
  { from: 'task1', to: 'task2' },
  { from: 'task2', to: 'task3' },
];
```

### Parallel Execution (AND-split/join)

```javascript
flow: [
  { from: 'start', to: 'parallel1', splitType: 'AND' },
  { from: 'start', to: 'parallel2', splitType: 'AND' },
  { from: 'parallel1', to: 'end', joinType: 'AND' },
  { from: 'parallel2', to: 'end', joinType: 'AND' },
];
```

### Conditional Branching (XOR-split/join)

```javascript
flow: [
  { from: 'check', to: 'path1', splitType: 'XOR', condition: { x_lt: 100 } },
  { from: 'check', to: 'path2', splitType: 'XOR', condition: { x_gte: 100 } },
  { from: 'path1', to: 'merge', joinType: 'XOR' },
  { from: 'path2', to: 'merge', joinType: 'XOR' },
];
```

## API Quick Reference

```javascript
import {
  createWorkflow, // Define workflow
  createCase, // Start instance
  enableTask, // Make task available
  startTask, // Claim task
  completeTask, // Finish task
  cancelWorkItem, // Cancel task
  replayCase, // Time-travel query
} from '@unrdf/yawl';

import { createStore } from '@unrdf/oxigraph';
```

## Next Steps

- Read the [YAWL Package README](../../packages/yawl/README.md) for full API documentation
- Explore [Use Case Guides](../../docs/guides/) for production scenarios
- Review [YAWL Patterns Guide](../../docs/guides/yawl-patterns.md) for all 20 patterns

## Questions?

See the main [UNRDF Documentation](https://github.com/unrdf/unrdf) or open an issue.
