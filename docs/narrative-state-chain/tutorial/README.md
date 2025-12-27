# Tutorials: Learn by Doing

**Hands-on tutorials for narrative state chain.**

## Available Tutorials

### 1. [Your First Scene](01-hello-world.md) (10 min)
**Status:** Complete  
Create a universe, propose a delta, verify receipt.

**You'll learn:**
- Universe (Σ) creation
- Delta (Δ) operations
- Guard (H) basics
- Receipt (R) verification

**Evidence:** Working code with assertions

---

### 2. Freeze & Time Travel (15 min)
**Status:** Skeleton (to be completed)  
Snapshot universe state and reconstruct history.

**You'll learn:**
- `freezeUniverse()` for snapshots
- Hash-addressed identity
- `reconstructState()` for time travel
- Git backbone integration

**Evidence:** [packages/kgc-4d/test/4d-time-travel-validation.test.mjs](/home/user/unrdf/packages/kgc-4d/test/4d-time-travel-validation.test.mjs)

---

## Learning Path

1. Start with **01: Your First Scene** — Core concepts
2. Then **02: Freeze & Time Travel** — State management
3. Move to [How-To Guides](../how-to/README.md) — Solve specific problems

---

## Prerequisites

All tutorials assume:
- Node.js 18+ installed
- UNRDF packages installed (`npm install`)
- Basic RDF understanding (triples: subject, predicate, object)

---

## Running Tutorial Code

```bash
# Run tutorial code directly
node docs/narrative-state-chain/tutorial/01-hello-world.mjs

# Or copy to your project
cp docs/narrative-state-chain/tutorial/01-hello-world.mjs my-project/
```

---

**Next:** [How-To Guides](../how-to/README.md)
