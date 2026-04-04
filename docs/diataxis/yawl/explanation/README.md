# YAWL Explanation

Explanation is understanding-oriented. These documents discuss the concepts behind YAWL — why it works the way it does, and how the pieces fit together.

**Read these when you want to understand, not when you want to do.**

---

## Explanations

| Document                                                                | What it explains                                                                                            |
| ----------------------------------------------------------------------- | ----------------------------------------------------------------------------------------------------------- |
| [01 — The YAWL Pattern Language](./01-yawl-pattern-language.md)         | What workflow patterns are, why Van der Aalst's taxonomy matters, and how YAWL implements them              |
| [02 — Task Contracts and Receipts](./02-task-contracts-and-receipts.md) | How pre/post-conditions define task contracts, and why BLAKE3 receipts provide a tamper-evident audit trail |

---

## Background reading

- Van der Aalst, W.M.P. et al. (2003). "Workflow Patterns." [workflowpatterns.com](https://www.workflowpatterns.com/)
- YAWL: A Petri-net-based workflow language supporting all workflow patterns
- KGC-4D: UNRDF's time-travel event store that YAWL uses for receipt chaining and case replay
