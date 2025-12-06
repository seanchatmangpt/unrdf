# Design Decisions: @unrdf/streaming

Rationale behind design choices.

---

## Design Trade-offs

Key decisions made during design, with rationale:

## Decision 1: Performance vs Simplicity

Prioritized performance because real-world use cases demand it.

## Decision 2: Features vs Maintainability

Focused on core features to maintain simplicity and reliability.

## Decision 3: Flexibility vs Defaults

Chose sensible defaults with full configuration for advanced users.

## Why Async?

Asynchronous operations scale better and don't block the event loop.

## Why Configuration?

Central configuration enables testing and production customization.

## Why Error Details?

Comprehensive error information helps debugging and recovery.

## Alternatives Considered

- Synchronous API: Rejected for scalability
- Plugin system: Rejected for complexity
- Multiple libs: Rejected for fragmentation

## Future Evolution

Design can evolve through new options, functions, and optimizations.

## See Also

- Architecture for system design
- Concepts for fundamentals
