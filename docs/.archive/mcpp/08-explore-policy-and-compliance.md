# Exploration: Policies, Compliance, and Def-of-Done

**Date:** 2026-04-24
**Role:** Explore Operator (Gemini)
**Objective:** Map repository policies (`LICENSE.md`, `CONTRIBUTING`, `DEF-OF-DONE`) into machine-executable public ontologies.

## Policy as Code (ODRL & SHACL)

In UniverseOS, a policy is not a text document for humans to hopefully follow. It is an `odrl:Policy` evaluated by the `Accept(ΔO)` function.

### Candidate 1: `LICENSE.md`
- **Exploration:** Express the repository's open-source or proprietary license using ODRL and SPDX.
  - *Shape:* The root `dcat:Catalog` (the repository) is linked via `dcterms:license` to an `spdx:License`.
  - *Execution:* If Claude Code (Exploit) attempts to add a dependency with a conflicting license (e.g., GPLv3 in an MIT project), the Doctor agent validates the `spdx:Package` against the ODRL prohibition and blocks the commit.

### Candidate 2: `CONTRIBUTING.md`
- **Exploration:** The contributing guidelines become a set of `odrl:Duty` nodes.
  - *Example Duty:* "All commits must contain a valid cryptographic receipt."
  - *Implementation:* A SHACL shape requires `prov:wasGeneratedBy` and `spdx:checksum` for every proposed `ΔRepo`. `CONTRIBUTING.md` is simply the natural language rendering of these SHACL shapes.

### Candidate 3: `DEF-OF-DONE.md` (Definition of Done)
- **Exploration:** The Definition of Done is the exact definition of the `Accept(ΔO)` gate.
  - *Metrics:* Incorporates the DfLSS Belt constraints.
  - *Verification:* Requires an `earl:Assertion` stating that all tests passed and all SHACL constraints are `sh:conforms "true"`.
  - *Workflow:* Claude Code cannot mark a task as "Done". It can only submit the `ΔRepo` for acceptance. The MCPP daemon computes the DoD. If it passes, the daemon commits the receipt.

## Surfaced Risks

1. **Over-Constraining the System:**
   - *Risk:* If the SHACL shapes for `DEF-OF-DONE` are too rigid, the system might lock up, unable to accept intermediate or refactoring steps.
   - *Mitigation:* Differentiate between "Branch Acceptance" (allows partial failure, WIP) and "Merge Acceptance" (strict `DEF-OF-DONE` enforcement).
2. **Policy Evaluation Latency:**
   - *Risk:* Evaluating complex ODRL logic and full-graph SHACL validation on every commit might slow down the development loop.
   - *Mitigation:* Utilize incremental validation (validating only the `ΔO_candidate` subgraph) and cache historical receipts.