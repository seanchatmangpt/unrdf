# Hardening Plan Phase 1: L4/L5 Functional Hardening

This plan addresses functional gaps in the AtomVM runtime and KGC-4D storage layers to ensure constitutional integrity, deterministic rehydration, and system-wide hardening.

## 1. AtomVM Runtime & Sandbox Hardening

### Context
Current stubs in `packages/atomvm` prevent full security enforcement. The `SecuritySandbox` fails with a `TypeError` due to improper context handling, and the `node-runtime` lacks parity with the browser runtime.

### Implementation Plan
1.  **Fix `packages/atomvm/src/vm/sandbox.mjs`**:
    *   **Context Injection**: Update the constructor to accept an `executionInvocation` object containing the `policyGraph`.
    *   **TypeError Fix**: Modify `_evaluateOdrlPolicy` to safely check for `this.context` before property access.
    *   **Logic**: Ensure `interceptOpcodes` correctly throws `ConstitutionalViolationError` when a policy check fails.
2.  **Fix `packages/atomvm/src/vm/loader.mjs`**:
    *   **SHACL Validation**: Import `validate` from `@unrdf/oxigraph` (utilizing its internal SHACL support) or integrate a lightweight SHACL validator to fulfill the `_validateShaclHeaders` stub.
3.  **Enhance `packages/atomvm/src/node-runtime.mjs`**:
    *   **API Parity**: Implement `runExample(moduleName)` and `executeBeam(avmPath)` methods to match the `AtomVMRuntime` (browser) interface.
    *   **Telemetry**: Align OTel span names and attributes with the browser implementation for unified observability.
    *   **Error Handling**: Improve `child_process` error mapping to provide specific diagnostic codes (e.g., `ERR_ATOMVM_BINARY_NOT_FOUND`).

## 2. KGC-4D Store Mutation Enforcement

### Context
`KGCStore` extends `UnrdfStore` but currently allows direct calls to `add()` and `delete()`, which bypasses the `appendEvent` log, violating the 4D immutability guarantee.

### Implementation Plan
1.  **Override Mutation Methods in `packages/kgc-4d/src/store.mjs`**:
    *   Override `add`, `addAll`, `delete`, `remove`, and `clear`.
    *   **Enforcement**: Each method will throw a `ConstitutionalViolationError`: *"Direct mutation of KGCStore is prohibited. Use appendEvent() to maintain 4D integrity."*
2.  **Internal Bypassing**:
    *   Implement a private `_directAdd` / `_directDelete` or use a "system context" flag to allow `appendEvent` itself to perform the necessary mutations to the `UNIVERSE` and `EVENT_LOG` graphs.

## 3. Deterministic Rehydration & Scalable Replay

### Context
`reconstructState` currently uses O(N) memory scans for event discovery and relies solely on `t_ns` (nanoseconds) for sorting, which fails to guarantee determinism in distributed or high-concurrency scenarios.

### Implementation Plan
1.  **Scalable Discovery in `packages/kgc-4d/src/freeze.mjs`**:
    *   Replace `store.match` scan with a **SPARQL SELECT** query:
        ```sparql
        SELECT ?event ?t ?vc ?payload WHERE {
          GRAPH <http://kgc.io/graphs/event-log> {
            ?event <http://kgc.io/t_ns> ?t ;
                   <http://kgc.io/vector_clock> ?vc ;
                   <http://kgc.io/payload> ?payload .
            FILTER(?t > ?snapshotTime && ?t <= ?targetTime)
          }
        }
        ```
    *   This leverages Oxigraph's indexing for O(log N) discovery.
2.  **Deterministic Sorting**:
    *   Update the sorting logic for `eventsToReplay`.
    *   **Primary Sort**: `t_ns` (physical time).
    *   **Secondary Sort (Causal)**: If `t_ns` is identical, use `VectorClock.compare(a, b)`.
    *   **Tertiary Sort (Tie-break)**: If events are concurrent (VC comparison returns 0), sort by `nodeId` to ensure every node reconstructs the exact same state.

---

## Adversarial Review

### "What if the VectorClock sorting is too slow?"
*   **Critique**: In a system with thousands of nodes, comparing large `Map`-based VectorClocks for every event during replay could significantly increase rehydration latency.
*   **Response**: `VectorClock` comparisons are only triggered when `t_ns` collisions occur. Given nanosecond precision, collisions are rare. We will also implement a "Pruned VectorClock" optimization where only active node IDs are stored, and use a Fast-Path comparison for equal physical timestamps.

### "What if overriding UnrdfStore breaks compatibility with core tools?"
*   **Critique**: Many `@unrdf/core` utilities (like `load()`, `import()`, or generic RDF syncers) expect `store.add()` to work. Hard-throwing will break these tools when used with a `KGCStore`.
*   **Response**: This is an intentional "Breaking by Design" security feature. To maintain compatibility with loaders, we will provide a `store.batchAppend(quads, metadata)` helper. Core tools requiring direct access must be explicitly opted-in via a `UnsafeKGCStore` wrapper or by passing a `bypassEnforcement: true` flag in the constructor (which will be logged as a security warning).

### "What if the AtomVM runtime interception causes significant performance hits?"
*   **Critique**: Intercepting every opcode in `SecuritySandbox` for a high-frequency BEAM loop (e.g., tight recursion) could introduce 2x-5x overhead.
*   **Response**: The sandbox is designed to intercept *privileged* opcodes (SYS_EXEC, FS_WRITE), not every arithmetic or control-flow instruction. Most BEAM instructions pass through without a policy check. We will implement "JIT-Style Policy Caching" where the result of a policy check for a specific bytecode module is cached after the first validation, reducing the overhead to a single map lookup.