# Upgrade Plan: atomvm & kgc-4d to Production (L4/L5)

**Role:** Explore Operator (Gemini)
**Objective:** Decompose the technical requirements for upgrading `atomvm` and `kgc-4d` from Research (L2) to Production (L4/L5) quality.

## 1. Upgrading @unrdf/atomvm (L2 -> L4)
To reach L4, `atomvm` must transition from an experimental VM into a hardened, lawful `POWL8` execution substrate.

### Requirements:
- **Constitutional Compiler:** Integrate `AtomVM` with the `SpecKit` build pipeline. Every bytecode compiled by `atomvm` must be derived from a validated `.specify/*.ttl` graph.
- **Lawful Concurrency:** Implement hard invariants for concurrency control. `atomvm` must natively enforce the `POWL8` synchronization barriers.
- **Auditability:** Every process execution in `atomvm` must produce a cryptographically signed `PROV-O` receipt, proving it was a lawful transition.
- **Hardened Runtime:** Remove all unsafe `unsafe-` prefixed internal opcodes from the public execution grammar.

## 2. Upgrading @unrdf/kgc-4d (L2 -> L5)
To reach L5, `kgc-4d` must move from "4D research" to a standard, hardened KGC (Knowledge Graph Control) substrate.

### Requirements:
- **Semantic Closure:** The "4D" (3D + Time) state tracking must be fully mapped to `OWL-Time` and `PROV-O`.
- **SHACL Admissibility:** Define a rigid `SHACL` shape library for 4D graph objects, ensuring that any state mutation (spatial or temporal) conforms to the system's physics constraints.
- **Manufacturing Closure:** Integrate with `OSTAR`. `kgc-4d` graph snapshots must be manufacture-ready—meaning they can be reconstructed from scratch using the receipts log.
- **Performance:** Benchmark 4D graph traversals against the DfLSS Belt. It must satisfy the sub-millisecond query latency requirement.

## Strategy for Claude Code (Exploit Operator)

1. **Phase 1 (Spec):** Create a dedicated `.specify/` folder for `atomvm` and `kgc-4d`, capturing their production requirements as RDF graphs.
2. **Phase 2 (Instrumentation):** Add OpenTelemetry (`otel`) and `PROV-O` receipt generation to the core runtime loops of both packages.
3. **Phase 3 (Validation):** Create a benchmark test suite that asserts the new performance and safety metrics defined in the DfLSS Belt.
4. **Phase 4 (Refactor):** Port the experimental Python/JS logic into native-aligned code paths (or AtomVM bytecode, if applicable) that support the MCPP control alphabet.

---
*Status:* **Upgrades initiated.** The Explore Operator has set the production invariants. The Exploit Operator can now commence technical implementation.