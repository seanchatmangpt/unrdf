# KGC-4D Reference Documentation

**Information-oriented: Precise API documentation and schemas**

Complete technical reference for all KGC-4D classes, functions, constants, and schemas.

---

## Reference Catalog

### Core APIs

1. **[KGCStore API](./kgc-store-api.md)** - Extended RDF store with 4D capabilities
2. **[GitBackbone API](./git-backbone-api.md)** - Git-backed snapshot storage
3. **[Time API](./time-api.md)** - Nanosecond timestamp functions
4. **[VectorClock API](./vector-clock-api.md)** - Causal ordering and merging

### Schemas & Constants

5. **[Receipt Schema](./receipt-schema.md)** - Freeze receipt structure
6. **[Events & Predicates](./events-predicates.md)** - Event types and RDF predicates
7. **[Named Graphs](./named-graphs.md)** - Universe, EventLog, System graphs

### Advanced

8. **[HDIT System](./hdit-system.md)** - Hyperdimensional coordinate functions

---

## Quick Lookup

**Looking for...**

- **Append events** → [KGCStore API](./kgc-store-api.md#appendEvent)
- **Freeze universe** → [KGCStore API](./kgc-store-api.md) + [Receipt Schema](./receipt-schema.md)
- **Time functions** → [Time API](./time-api.md)
- **Git operations** → [GitBackbone API](./git-backbone-api.md)
- **Event types** → [Events & Predicates](./events-predicates.md)
- **Graph URIs** → [Named Graphs](./named-graphs.md)
- **Vector clocks** → [VectorClock API](./vector-clock-api.md)
- **HDIT coordinates** → [HDIT System](./hdit-system.md)

---

## Reference Format

Each reference doc includes:

- **Function/Class signature** with TypeScript-style types
- **Parameters** with types and descriptions
- **Return values** with types
- **Examples** (minimal, see Tutorials for complete examples)
- **Source code location** (file:line)
- **Related functions** in same module

---

## Evidence Base

All references point to:
- Source: `/home/user/unrdf/packages/kgc-4d/src/`
- Tests: `/home/user/unrdf/packages/kgc-4d/test/`
- Exports: `/home/user/unrdf/packages/kgc-4d/src/index.mjs`

---

**Navigate:** [Main Diataxis](../README.md) | [Tutorials](../tutorials/) | [How-To](../how-to/) | [Explanation](../explanation/)
