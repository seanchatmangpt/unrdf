# Explanation: @unrdf/daemon

Explanation documents are **understanding-oriented**. They describe why the daemon is designed the way it is, how the layers fit together, and what trade-offs were made. Read these when you want to reason about the system, not just use it.

## Available Explanations

| Document                                           | Covers                                                                                                               |
| -------------------------------------------------- | -------------------------------------------------------------------------------------------------------------------- |
| [Daemon Architecture](./01-daemon-architecture.md) | 5-layer design, MCP server layer, OTel instrumentation layer, security layer, clustering model, key design decisions |

## When to Read Explanations

- Before making architectural decisions that extend the daemon
- When debugging unexpected behavior and needing to understand the event flow
- When evaluating whether the daemon fits a use case (see the "When to Use" section)
- When understanding why operations use EventEmitter rather than pure Promise chains

## See Also

- [Tutorial: Your First Daemon](../tutorials/01-first-daemon.md) — hands-on learning
- [How-To Guides](../how-to/) — task-oriented solutions
- [API Reference](../reference/) — exact signatures and types
