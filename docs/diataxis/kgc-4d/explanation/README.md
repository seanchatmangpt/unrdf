# KGC-4D Explanation Documentation

**Understanding-oriented: Deep dives into architecture and design rationale**

These articles explain the "why" behind KGC-4D's design decisions and architectural principles.

---

## Explanation Catalog

### Core Principles

1. **[Why Nanosecond Precision Matters](./01-why-nanosecond-precision.md)**  
   Why BigInt nanoseconds instead of Date.now() milliseconds

2. **[How Time Travel Reconstruction Works](./02-how-time-travel-works.md)**  
   Algorithm: snapshot + delta replay for efficient state reconstruction

3. **[The Zero-Information Invariant Principle](./03-zero-info-invariant.md)**  
   Why the universe is fully reconstructible from EventLog + Git

### Architecture Rationale

4. **[Architecture Rationale: Why Git Backing?](./04-why-git-backing.md)**  
   Why isomorphic-git for snapshots instead of database blobs

5. **[Why BLAKE3 Hashing?](./05-why-blake3.md)**  
   Hash algorithm choice: BLAKE3 vs SHA-256 vs alternatives

6. **[Monotonic Clock Guarantees](./06-monotonic-clocks.md)**  
   How KGC-4D ensures time never goes backward

---

## By Topic

**Time & Causality:**
- [Why Nanosecond Precision](./01-why-nanosecond-precision.md)
- [Monotonic Clock Guarantees](./06-monotonic-clocks.md)

**State Management:**
- [How Time Travel Works](./02-how-time-travel-works.md)
- [Zero-Information Invariant](./03-zero-info-invariant.md)

**Architecture:**
- [Why Git Backing](./04-why-git-backing.md)
- [Why BLAKE3](./05-why-blake3.md)

---

## Explanation Format

Each article includes:

- **Problem/Question** - What design challenge?
- **Context** - What constraints?
- **Decision** - What was chosen?
- **Rationale** - Why this choice?
- **Tradeoffs** - What was sacrificed?
- **Alternatives** - What was rejected and why?
- **Evidence** - Empirical data or proofs

---

## Evidence Base

Explanations reference:
- Source code: `/home/user/unrdf/packages/kgc-4d/src/`
- ARD (Architecture Requirements): `/home/user/unrdf/packages/kgc-4d/docs/ARD.md`
- Benchmarks: `/home/user/unrdf/packages/kgc-4d/test/benchmarks/`

---

**Navigate:** [Main Diataxis](../README.md) | [Tutorials](../tutorials/) | [How-To](../how-to/) | [Reference](../reference/)
