# Exploration: POWL8 as the Process ISA

**Date:** 2026-04-24
**Role:** Explore Operator (Gemini)
**Objective:** Decompose POWL8 into an executable control alphabet and surface candidate architectural shapes for the Doctor, Wizard, and Telco archetypes.

## POWL8: The Executable Control Alphabet

Traditional workflows view process as metadata. UniverseOS requires process to be expressed as a native ISA. POWL8 provides the minimal set of lawful ISA operations.

### Proposed Instruction Set Candidates
1. **Sequence (`powl8:Sequence`)**: Linear progression of lawful steps.
2. **Choice (`powl8:Choice`)**: Branching logic gated by O* acceptance predicates.
3. **Loop (`powl8:Loop`)**: Iterative progression bounded by a termination proof (receipt).
4. **Partial-Order / Parallel (`powl8:PartialOrder`)**: Concurrent execution of non-conflicting steps (the native concurrency model of AtomVM).
5. **Synchronization (`powl8:Sync`)**: Barrier operations awaiting multiple cryptographic receipts before allowing the graph to proceed.

## Mapping the "MySpace Tom" Triad to POWL8

The Doctor, Wizard, and Telco are the minimal viable universe capabilities (the baseline for avoiding the cold-start problem). How do they manifest in the POWL8 ISA?

### 1. Doctor (Truth / Validation / Admissibility)
- **Role:** Evaluates constraints and yields diagnostic truth.
- **POWL8 Shape:** Primarily acts as the gating logic within `powl8:Choice` or the termination condition for `powl8:Loop`.
- **Candidate Implementation:** A Doctor agent executes a SHACL validation (`sh:conforms`) and returns a boolean or an EARL report.

### 2. Wizard (Transformation / Capability)
- **Role:** Mutates state lawfully.
- **POWL8 Shape:** The execution payload within a `powl8:Sequence` or `powl8:PartialOrder` step.
- **Candidate Implementation:** Takes an `O*` input, performs a task (e.g., Code generation, OSTAR manufacturing), and emits `ΔO`.

### 3. Telco (Connectivity / Routing / Infrastructure)
- **Role:** Orchestrates the flow of data between nodes.
- **POWL8 Shape:** The wiring that connects nodes—specifically managing `powl8:Sync` barriers and `powl8:PartialOrder` distributions.
- **Candidate Implementation:** A message broker or RDF routing layer that moves the receipt of a Wizard's action to the next Doctor's queue.

## Risks & Alternative Architectures

- **Risk of Granularity:** If POWL8 is too granular, defining a complex system (like DTeam AutoML) might require millions of instructions, causing the execution engine (AtomVM) to thrash.
- **Alternative (Macro/Micro Split):** Strictly enforce the boundary between `POWL64` (ABI execution frame/planning) and `POWL8` (ISA execution micro-ops). `POWL64` represents the "Plan", and the MuStar runtime issues `POWL8` instructions just-in-time for execution, identical to how a CPU decodes complex instructions into micro-ops.