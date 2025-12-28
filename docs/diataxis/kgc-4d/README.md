# KGC-4D Documentation (Diataxis Framework)

**4-Dimensional Knowledge Graph Engine with Nanosecond Precision**

Navigate by your learning goal:

---

## Quick Start Paths

### New to KGC-4D? Start with Tutorials
**Goal:** Learn by doing, hands-on practice with working examples

Start here if you're new to 4D knowledge graphs and want to build working examples step-by-step.

**[Go to Tutorials](./tutorials/)** | **Start with:** [Getting Started with Nanosecond Time](./tutorials/01-nanosecond-timestamps.md)

---

### Solving a Specific Problem? Go to How-To Guides  
**Goal:** Solve specific tasks you're facing

Use these when you know what you want to accomplish but need implementation details.

**[Go to How-To Guides](./how-to/)** | **Popular:** [Freeze and Verify Snapshots](./how-to/01-freeze-and-verify.md)

---

### Looking Up API Details? See Reference
**Goal:** Find exact syntax, parameters, schemas

Complete API documentation for all KGC-4D classes, functions, and schemas.

**[Go to Reference](./reference/)** | **Most viewed:** [KGCStore API](./reference/kgc-store-api.md)

---

### Understanding Architecture? Read Explanation
**Goal:** Deep dive into design rationale and principles

Conceptual articles explaining the "why" behind KGC-4D's architecture.

**[Go to Explanation](./explanation/)** | **Start with:** [Why Nanosecond Precision](./explanation/01-why-nanosecond-precision.md)

---

## Interactive Decision Tree

```
What do you want to do?
    │
    ├─ Learn KGC-4D from scratch → TUTORIALS
    │   ├─ Understand nanosecond timestamps → Tutorial 01
    │   ├─ Create and freeze a universe → Tutorial 02
    │   ├─ Time travel to past states → Tutorial 03
    │   ├─ Query event logs → Tutorial 04
    │   └─ Use vector clocks → Tutorial 05
    │
    ├─ Solve a specific problem → HOW-TO
    │   ├─ How do I freeze snapshots? → How-To 01
    │   ├─ How do I implement time travel? → How-To 02
    │   ├─ How do I query event history? → How-To 03
    │   ├─ How do I use vector clocks? → How-To 04
    │   ├─ How do I optimize performance? → How-To 05
    │   └─ How do I work with HDIT? → How-To 06
    │
    ├─ Look up exact syntax → REFERENCE
    │   ├─ KGCStore methods → Reference: KGCStore API
    │   ├─ GitBackbone methods → Reference: GitBackbone API
    │   ├─ Time functions → Reference: Time API
    │   ├─ Receipt schema → Reference: Receipt Schema
    │   ├─ Event types → Reference: Events & Predicates
    │   ├─ Vector clock API → Reference: VectorClock API
    │   ├─ HDIT coordinates → Reference: HDIT System
    │   └─ Named graphs → Reference: Named Graphs
    │
    └─ Understand architecture → EXPLANATION
        ├─ Why nanosecond precision? → Explanation 01
        ├─ How does time travel work? → Explanation 02
        ├─ What's the zero-info invariant? → Explanation 03
        ├─ Why Git backing? → Explanation 04
        ├─ Why BLAKE3 hashing? → Explanation 05
        └─ Monotonic clock guarantees? → Explanation 06
```

---

## Learning Paths by Use Case

### Path 1: Quick Prototype (30 minutes)
**For developers who need a proof-of-concept fast**

1. [Tutorial 01: Nanosecond Timestamps](./tutorials/01-nanosecond-timestamps.md) - 5 min
2. [Tutorial 02: Create and Freeze Universe](./tutorials/02-create-freeze-universe.md) - 10 min
3. [How-To 01: Freeze and Verify](./how-to/01-freeze-and-verify.md) - 10 min
4. [Reference: KGCStore API](./reference/kgc-store-api.md) - 5 min
5. **Result:** Working 4D knowledge graph with snapshots

---

### Path 2: Audit & Compliance (1 hour)
**For teams needing immutable audit trails**

1. [Tutorial 02: Create and Freeze Universe](./tutorials/02-create-freeze-universe.md) - 10 min
2. [Tutorial 04: Query Event Logs](./tutorials/04-query-event-logs.md) - 15 min
3. [How-To 01: Freeze and Verify](./how-to/01-freeze-and-verify.md) - 10 min
4. [Explanation 03: Zero-Information Invariant](./explanation/03-zero-info-invariant.md) - 15 min
5. [Reference: Receipt Schema](./reference/receipt-schema.md) - 10 min
6. **Result:** Production-ready audit trail with cryptographic verification

---

### Path 3: Time Travel & Rollback (1.5 hours)
**For systems requiring state rollback and history replay**

1. [Tutorial 03: Time Travel to Past States](./tutorials/03-time-travel.md) - 20 min
2. [How-To 02: Implement Time Travel](./how-to/02-implement-time-travel.md) - 20 min
3. [Explanation 02: How Time Travel Works](./explanation/02-how-time-travel-works.md) - 25 min
4. [Reference: Time API](./reference/time-api.md) - 15 min
5. [How-To 05: Optimize Performance](./how-to/05-optimize-performance.md) - 10 min
6. **Result:** Production time-travel system with optimized replay

---

### Path 4: Distributed Systems (2 hours)
**For architects building causally-consistent distributed graphs**

1. [Tutorial 05: Use Vector Clocks](./tutorials/05-vector-clocks.md) - 20 min
2. [How-To 04: Vector Clocks for Distribution](./how-to/04-vector-clocks.md) - 30 min
3. [Explanation 06: Monotonic Clock Guarantees](./explanation/06-monotonic-clocks.md) - 25 min
4. [Reference: VectorClock API](./reference/vector-clock-api.md) - 20 min
5. [Explanation 04: Why Git Backing](./explanation/04-why-git-backing.md) - 25 min
6. **Result:** Distributed 4D graph with causal consistency

---

## Evidence-Based Documentation

Every document in this Diataxis structure:

- **References proven capabilities** from /home/user/unrdf/packages/kgc-4d/src/
- **Links to working examples** in /home/user/unrdf/packages/kgc-4d/examples/
- **Points to test evidence** in /home/user/unrdf/packages/kgc-4d/test/
- **Provides exact code locations** (file:line) for implementation

---

## Documentation Quality Standards

| Category | Focus | Tone | Success Metric |
|----------|-------|------|----------------|
| **Tutorials** | Learning-oriented | Encouraging | Reader completes successfully |
| **How-To** | Problem-oriented | Direct | Reader solves their problem |
| **Reference** | Information-oriented | Precise | Reader finds exact answer |
| **Explanation** | Understanding-oriented | Analytical | Reader gains deep insight |

---

## Status

| Category | Files | Status |
|----------|-------|--------|
| Tutorials | 5 | Skeleton structure complete |
| How-To | 6 | Skeleton structure complete |
| Reference | 8 | Skeleton structure complete |
| Explanation | 6 | Skeleton structure complete |

**Total:** 25 documentation files

**Created:** 2025-12-27  
**Evidence Base:** kgc-4d v5.0.1

---

## Quick Links

- [KGC-4D Package README](/home/user/unrdf/packages/kgc-4d/README.md)
- [Main UNRDF Diataxis](/home/user/unrdf/docs/diataxis/README.md)
- [Architecture Requirements (ARD)](/home/user/unrdf/packages/kgc-4d/docs/ARD.md)
- [API Reference](/home/user/unrdf/packages/kgc-4d/docs/API.md)

---

**Navigate:** [Tutorials](./tutorials/) | [How-To](./how-to/) | [Reference](./reference/) | [Explanation](./explanation/)
