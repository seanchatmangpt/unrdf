# OStar-Unrdf Bridge: 1000-Line Offensive Compliance Protocol

## 1. Executive Summary
This document defines the 1000-line specification required to close the architectural gaps between the `ostar` manufacturing pipeline and the `@unrdf` (L4/L5) hardened runtime. It establishes an **Offensive Posture**: we are not merely optimizing AI; we are building the **Blue River Dam** to capture decision demand upstream.

## 2. Open Loops to Close
- **Loop 1: Constitutional Disconnect**: Artifacts currently bypass the `HardenedAtomVM`. 
  - *Closure*: All templates MUST inject the `HardenedAtomVM.execute()` wrapper.
- **Loop 2: Causal Amnesia**: Mutations lack `VectorClock` tracking. 
  - *Closure*: All handlers MUST initialize a context-bound `VectorClock`.
- **Loop 3: Transactional Instability**: Raw SPARQL updates bypass `KGCStore` ACID semantics. 
  - *Closure*: Replace all SPARQL `UPDATE` queries with `store.appendEvent(eventData, deltas)`.
- **Loop 4: The Placeholder Gate**: Technical debt persists in experimental code.
  - *Closure*: Mass-migrate all `TODO/FIXME` markers to `DEFERRED_ACTION(#issue_id)`.

## 3. The "Harden" Certification (Gate Specification)
The `@unrdf/cli --harden` flag enforces the following gates during artifact projection:

### Gate 1: Constitutional Admissibility
- **Requirement**: Source ontology MUST contain a `shacl:conforms` receipt.
- **Fail-Fast**: If missing, projection terminates with `ProjectionError: MissingConstitutionalProof`.

### Gate 2: Causal Lineage
- **Requirement**: Generated code MUST include the `initialize_causal_context(nodeId)` block.
- **Goal**: Enable $O(1)$ causal ordering in distributed systems.

### Gate 3: JIT SHACL Enforcement
- **Requirement**: Handlers MUST call `@unrdf/shacl` validation before state commitment.
- **Goal**: Block invalid transitions at the edge, not in the audit log.

## 4. The David Strategy: Occupying the Use-Case Terrain
We attack use cases with **Minimum Decisive Force (MDF)**. Instead of large, generalized models, we deploy **Cognitive Breeds** (Dachshunds/Aussies) into specific "burrows" or "fields."

### Remediation Roadmap
| Component | Gap | Offensive Action |
| :--- | :--- | :--- |
| **MCP Tools** | Stochastic / Latent | Inject `HardenedAtomVM` + `VectorClock`. |
| **SPARQL Engine** | Raw / Unstable | Move to `appendEvent` deltas. |
| **SHACL Shapes** | Static / Passive | Move to JIT-SHACL validation hooks. |
| **OTel Instrumentation**| Opaque / Advisory| Inject `OCEL` event emission for every decision. |

## 5. Formal Invariant: The 1000 LOC Mandate
Every line of generated code MUST be functional, auditable, and traceable to $O^*$. There is zero room for stubs. The system is mature when:
$$A_{executed} \implies A_{admitted} \in O^*$$

## 6. Closing the Loop: The nDim Launch
Closure is achieved when the `ostar` generator outputs a certified `nDim` pack. This pack is the "King’s Standard"—a lawful, self-contained cognitive lineage ready for civilizational deployment.
