# Exploration: mcpp.toml & The Invocation Grammar

**Date:** 2026-04-24
**Role:** Explore Operator (Gemini)
**Objective:** Decompose the `mcpp.toml` configuration and the `clap-noun-verb` CLI structure into a secure, semantic control surface.

## The CLI as a Semantic Actuator

The God Box identifies `mcpp.toml` and `CLI structure`. The CLI must not be a random collection of scripts. It is the entry point for human and agent intent to enter UniverseOS.

### Candidate: The `clap-noun-verb` Grammar
- **Concept:** Commands are structured strictly as `<Subject> <Action> <Target>`.
  - *Example:* `mcpp generate spec`
  - *Example:* `mcpp validate ontology`
- **Mapping to Ontology:**
  - *Noun (Subject/Target):* Maps to an `rdfs:Class` or `dcat:Resource` (e.g., `Spec`, `Ontology`).
  - *Verb (Action):* Maps to an `as:Activity` type or a `powl8:Sequence` trigger (e.g., `as:Create`, `mcpp:Validate`).
- **Exploration:** The CLI uses `clap` (in Rust) or similar to parse the grammar, but instead of executing local functions, it constructs an RDF command payload (`ΔO_candidate`) and submits it to the `Doctor` for validation.

### Candidate: `mcpp.toml`
- **Role:** The local workspace projection of the MCPP identity.
- **Exploration:** Instead of duplicating semantic truth, `mcpp.toml` acts as a local pointer.
  - It defines the location of the local triplestore or `.specify/` graphs.
  - It lists the active `Doctor`, `Wizard`, and `Telco` endpoints.
  - It specifies the local application profile bounds (e.g., which SHACL shapes to load).
- *Analogy:* `mcpp.toml` is to MCPP what `.git/config` is to Git—a local context locator for a global protocol.

## Surfaced Risks

1. **The "Unbounded Command Surface" Risk:**
   - *Risk:* As mentioned in the Spec Kit constitution, CLIs tend to accrue flag bloat (`--force`, `--skip-checks`, `--dry-run`), bypassing the lawful ontology.
   - *Mitigation:* The `mcpp.toml` explicitly forbids arbitrary flags. All configuration variations must be expressed as properties of the `ΔO_candidate` graph, not as CLI hacks. If a user needs a "dry run," they invoke an `as:TentativeAccept` activity, which the system processes lawfully without committing.
2. **Latency of Invocation:**
   - *Risk:* If every CLI command requires a full RDF graph construction and validation cycle, the developer experience (DX) will degrade.
   - *Mitigation:* Fast-path validation using local WASM-compiled (AtomVM) `Doctor` probes for immediate terminal feedback, followed by asynchronous cryptographic settlement.