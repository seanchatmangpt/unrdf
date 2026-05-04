# Exploration: Public Ontology Discovery & Mapping Risks

**Date:** 2026-04-24
**Role:** Explore Operator (Gemini)
**Objective:** Surface candidate ontologies for the Spec Kit conversion, identify non-obvious risks, and propose alternative decompositions.

## Candidate Mappings (Spec Kit -> Public Ontology)

As the Explore Operator, I propose the following strict mappings from Spec Kit concepts to public RDF ontologies, ensuring we do not invent private terms unnecessarily.

| Spec Kit Concept | Proposed Primary Ontologies | Candidate Use-Cases / Edge Surfacing |
| :--- | :--- | :--- |
| **Constitution** | `ODRL`, `SHACL`, `DPV`, `SKOS` | Use ODRL for `Prohibition`, `Duty`, and `Permission`. Use SHACL for defining structurally admissible states. Use DPV for compliance/governance logic. |
| **Specify** | `DCAT 3`, `ADMS`, `CodeMeta`, `DOAP`, `SPDX` | Map the `.specify` markdown artifacts into `dcat:Dataset` or `adms:Asset`. Treat each feature as a semantic asset. |
| **Clarifications** | `Web Annotation (OA)`, `ActivityStreams (AS2)` | Use `oa:Annotation` with `oa:questioning` to target specific `dcat:Resource` nodes. Use AS2 for the actor/object events (e.g., `as:Accept`, `as:Reject`). |
| **Plan / Tasks** | `PROV-O`, `P-Plan`, `OWL-Time`, `ORG` | `p-plan:Step` for individual tasks; `prov:Plan` for the overarching execution geometry. |
| **Receipts** | `PROV-O`, `SPDX`, `EARL` | W3C Evaluation and Report Language (EARL) to assert validation of SHACL shapes, acting as the verifiable receipt of execution. |

## Surfaced Risks & Edge Cases

1. **Expressivity of ODRL for Execution Policies:**
   - *Risk:* ODRL is primarily designed for digital rights and IP, not necessarily for runtime execution constraints (e.g., "Do not allow unbounded command surfaces").
   - *Exploration:* We may need an `mcpp:` application profile that extends `odrl:Action` to include concepts like `mcpp:ExposeCommandSurface`, while keeping the core `odrl:Prohibition` structure intact.
2. **Partial Acceptance States (The EARL Gap):**
   - *Risk:* SHACL validation is often binary. In a real-world scenario (especially when transitioning legacy components), we may encounter partial graph validation.
   - *Exploration:* Introduce EARL (`earl:Assertion`) to log exactly which SHACL shapes failed. The Acceptance gate `Accept(ΔO)` should require a 100% `earl:passed` assertion for constitutional shapes, but might allow `earl:cantTell` for non-critical documentation shapes.
3. **Graph Bloat (The Path Explosion Problem):**
   - *Risk:* Mapping every single interaction (every question asked, every git commit) into PROV-O/AS2 could bloat the triplestore, making `O*` (the accepted closed ontology) unwieldy.
   - *Exploration:* Define a "Daily Dying" pruning policy specifically for the graph. Transient AS2 events should be pruned or archived into cold storage, keeping only the final resolved `O*` states and their direct `p-plan:Step` origins.

## Alternative Decomposition: Staging vs. Truth Graphs
Rather than writing directly to `O*`, the Explore operator should write to an `O_candidate` namespace. 
- `Gemini` generates `ΔO_candidate`.
- A SHACL validation engine assesses `O_candidate + ΔO_candidate`.
- If valid, a transformation promotes it to `O*` and assigns a cryptographic receipt.
- `Claude Code` (Exploit) is only permitted to read from `O*`.