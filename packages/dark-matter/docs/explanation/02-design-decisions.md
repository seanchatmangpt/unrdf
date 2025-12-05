# Design Decisions: @unrdf/dark-matter

Rationale behind design choices.

---

## Key Trade-offs

### Trade-off 1: Performance vs Simplicity

**Decision:** Prioritize performance for real-world use cases.

**Rationale:** Users need efficiency, added complexity is justified.

**Impact:** Slightly more complex but much better performance.

### Trade-off 2: Features vs Maintainability

**Decision:** Focus on core features, keep system maintainable.

**Rationale:** Simple, well-maintained code is better than complex broken code.

**Impact:** Core features work reliably, extensions available.

### Trade-off 3: Flexibility vs Defaults

**Decision:** Smart defaults with full configuration.

**Rationale:** 80% of users get good defaults, 20% can tune everything.

**Impact:** Works great out of box, expert users can optimize.

---

## Why This Approach

### Asynchronous API

Why async-first design:
- Modern JavaScript is async
- System resources need non-blocking I/O
- Better scalability and responsiveness

### Configuration System

Why central configuration:
- Different use cases need different tuning
- Enables testing with different settings
- Production customization without code changes

### Error Handling

Why comprehensive errors:
- Distributed systems are unreliable
- Good error messages help debugging
- Recovery strategies need detail

---

## Alternatives Rejected

### Synchronous API
- **Rejected because**: Doesn't scale to real-world use

### Plugin System
- **Rejected because**: Adds complexity with minimal benefit

### Multiple Libraries
- **Rejected because**: Fragmentation worse than one good library

---

## Future Evolution

The design can evolve through:
- New configuration options
- Additional functions
- Performance optimizations
- Better error messages

---

## See Also

- [01-architecture.md](01-architecture.md) - System design
- [03-concepts.md](03-concepts.md) - Core concepts
