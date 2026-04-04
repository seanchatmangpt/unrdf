# YAWL How-To Guides

How-to guides are task-oriented. Each guide answers a specific question of the form "how do I...?".

**Assume you already know the basics.** If you are new to YAWL, start with the [Tutorials](../tutorials/README.md) instead.

---

## Guides

| Guide                                                                   | What it solves                                                    |
| ----------------------------------------------------------------------- | ----------------------------------------------------------------- |
| [01 — Define a Task with a Contract](./01-define-task-with-contract.md) | Attach Zod input/output schemas and pre/post conditions to a task |
| [02 — Handle Task Failure](./02-handle-task-failure.md)                 | Fail, retry, cancel, and use circuit breakers on tasks            |
| [03 — Orchestrate with Daemon](./03-orchestrate-with-daemon.md)         | Drive YAWL workflows from a `@unrdf/daemon` scheduled operation   |

---

## Common patterns at a glance

```javascript
// Cancel a work item
await engine.cancelCase(caseId, workItemId);

// Check engine health
const health = engine.healthCheck();
console.log(health.status); // 'healthy' | 'degraded' | 'unhealthy'

// Query active cases
const activeCases = engine.getActiveCases();

// Snapshot for time-travel replay
await engine.createSnapshot(caseId);
```
