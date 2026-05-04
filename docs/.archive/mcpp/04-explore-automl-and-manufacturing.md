# Exploration: DTeam AutoML & OSTAR-MuStar Manufacturing

**Date:** 2026-04-24
**Role:** Explore Operator (Gemini)
**Objective:** Surface candidate structures for transitioning legacy AI wrappers and AutoML components into the lawful OSTAR/MuStar manufacturing execution layer.

## Legacy AI & AutoML vs. DTeam AutoML

The old AI pattern (hardcoded LangChain wrappers, direct API calls) violates the MCPP premise of a single, lawful execution surface.

### Concept: DTeam (Digital Team)
A Digital Team acts, whereas a Digital Twin only mirrors state. DTeam AutoML must not be "AI automation" but an ontology-bound operating organization.

### Candidate Structure
- **Planner Agent:** Generates candidate geometries (`POWL64` branches).
- **Process Miner:** Evaluates historical receipts to optimize the geometries.
- **RDF Hook Engine:** Injects execution policies and side-effects.
- **Simulator (Unibit AutoML):** Tests the geometry against expected distributions.
- **Executor:** The `MuStar` runtime running the `POWL8` ISA.
- **Auditor:** The `Accept(ΔO)` gate validator.

## OSTAR and MuStar: Closure and Runtime

### OSTAR (Manufacturing Closure)
- **Role:** The RDF-first manufacturing substrate.
- **Exploration:** It shouldn't just compile code; it must compile the structural bounds. OSTAR consumes the `O*` ontology and emits the deterministic artifact (the compiled binaries, the Docker images, the Terraform plans) plus a cryptographically signed receipt.

### MuStar (Semantic-to-Kinetic Lowering)
- **Role:** The runtime semantic bridge.
- **Exploration:** It lowers high-level `POWL64` intent into the executable `POWL8` ISA.
- **Risk:** If MuStar lowers intent into an alien runtime form (like raw Python scripts outside of AtomVM), the semantics are lost. MuStar must lower strictly into NPOWL8/POWL8 micro-ops so the execution remains under the MCPP control grammar.

## Candidate Architecture for the CLI Structure
The `mcpp` CLI (`clap-noun-verb`) is the single invocation grammar.
- **Risk of Unbounded Surface:** Command lines naturally trend toward entropy (`--force`, `--ignore-checks`).
- **Exploration:** The CLI should not execute commands; it should construct `ΔO_candidate` payloads and submit them to the MCPP daemon for validation and `POWL8` execution. A user typing `mcpp deploy app` is actually invoking a `Wizard` capability that must pass a `Doctor` diagnostic gate before the `Telco` routes it to production.