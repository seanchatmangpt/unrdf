# Feature Requirements for Production Upgrades (@unrdf/atomvm & @unrdf/kgc-4d)

**Date:** 2026-04-24
**Objective:** Define the specific functional and non-functional feature requirements to bridge the gap from Research (L2) to Production (L4/L5).

---

## 1. @unrdf/atomvm (L2 -> L4 Hardening)

To reach L4, `atomvm` must transition from an experimental VM into a production-grade, lawful execution substrate.

### Feature Requirements:
1. **Constitutional Bytecode Loader:**
   - A loader that refuses to execute any bytecode blob not accompanied by a valid cryptographic `SpecKit` receipt (linking it to a validated `.specify/*.ttl` graph).
2. **POWL8 Hardware-Abstraction Layer (HAL):**
   - Native implementation of `POWL8` concurrency primitives (`Seq`, `Choice`, `Loop`, `Sync`). These must be mapped to low-level VM opcodes to achieve "programming-language speed."
3. **Deterministic Receipt Service:**
   - A runtime-level module that emits `PROV-O` event streams for every process lifecycle event (`spawn`, `terminate`, `suspend`).
4. **Safety Sandbox (Constraint Enforcement):**
   - A kernel-level sandbox that dynamically strips "unsafe" opcodes (e.g., raw memory access, non-deterministic syscalls) based on the current ODRL policy loaded in the runtime context.

---

## 2. @unrdf/kgc-4d (L2 -> L5 Hardening)

To reach L5, `kgc-4d` must move from an experimental spatial-temporal graph to the core production-hardened KGC substrate.

### Feature Requirements:
1. **Temporal-Spatial Indexing:**
   - Implement native `OWL-Time` and 3D-Coordinate (R-Tree) indices in the KGC runtime.
   - *Requirement:* Sub-millisecond lookup for state at any $(x, y, z, t)$.
2. **SHACL-4D Validation Engine:**
   - A lightweight, JIT-compiled SHACL engine that runs at the graph insertion boundary. Every `addTriple` call must be checked against 4D physics invariants (e.g., "An object cannot be in two places at the same time").
3. **Graph Time-Travel (Manufacturing Closure):**
   - The ability to "rehydrate" the graph state to any point in the `PROV-O` receipt log. This makes the system "manufacture-ready"—reconstruction from receipts must be deterministic.
4. **Policy-Bound Mutation:**
   - Every mutation activity must carry an `odrl:Policy` object. If the current graph state conflicts with the mutation policy, the KGC engine must roll back the transaction automatically.

---

## 3. Cross-Cutting Requirements (The MCPP Surface)

1. **Deterministic Telemetry (OpenTelemetry):**
   - Both packages must export OTEL metrics conforming to the DfLSS Belt. If performance metrics drift outside the target band, the MCPP daemon must raise a "Constitutional Violation" event.
2. **Standardized Configuration (`mcpp.toml`):**
   - Removal of all hardcoded configuration. Both packages must support dynamic injection of policy, role, and boundary rules via the `mcpp.toml` semantic actuator.
3. **Receipt-Based Audit Trail:**
   - Any state-changing operation (in either VM execution or Graph mutation) must produce a hash-linked receipt, enabling full forensic auditability.

---

*Status:* **Feature Specification Complete.** The Exploit operator can now translate these functional requirements into `p-plan:Step` modules in the `.specify/` graphs.