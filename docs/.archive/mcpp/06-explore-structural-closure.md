# Exploration: Structural Closure & Repository Topology

**Date:** 2026-04-24
**Role:** Explore Operator (Gemini)
**Objective:** Surface candidate strategies for organizing the monorepo, mapping physical directory structures to semantic assets within UniverseOS.

## The Semantic-to-Physical Gap

Currently, the repository is a standard monorepo (e.g., `packages/`, `apps/`, `docs/`). However, under MCPP, the physical structure must reflect the semantic closure (`O*`). A repository is not just a collection of files; it is a serialized `dcat:Catalog`.

### Candidate 1: Topological Alignment to NPOWL8 / POWL8
- **`src/` vs `capabilities/`**: Instead of generic code folders, organize by the `Doctor`, `Wizard`, and `Telco` archetypes.
  - `capabilities/doctor/`: Validation and diagnostic shapes.
  - `capabilities/wizard/`: Transformational micro-ops and code generation templates.
  - `capabilities/telco/`: Routing, messaging, and API gateways.
- **`OSTAR` Manufacturing Output**: The `dist/` or `build/` folders become `receipts/` and `artifacts/`. Every compiled binary is accompanied by an `spdx:Package` and `prov:Entity` receipt.

### Candidate 2: The "Organized" and "Create Structure" Directives
The God Box mandates structural hygiene (`Organized`, `repo`, `create structure`).
- **Exploration:** The physical layout must be deterministic. If an asset exists in the `O*` graph as a `dcat:Dataset`, there must be a canonical path algorithm that maps its IRI to a file path (e.g., `urn:mcpp:capability:wizard-codegen` maps to `packages/capabilities/wizard-codegen/`).
- **Daily Dying Application:** Any folder or file that cannot be deterministically mapped back to an active `dcat:Resource` in the Spec Kit graph is automatically flagged for "Daily Dying" (pruning).

## Surfaced Risks

1. **Monorepo Tooling Friction:**
   - *Risk:* Existing tools (pnpm, Turborepo) expect `packages/*`. Renaming or reshaping them too radically might break the ecosystem.
   - *Mitigation:* Retain the `packages/` prefix but enforce semantic grouping inside it (e.g., `packages/mcpp-doctor-*/`). The topological map is defined in `mcpp.toml` or `workspace.yaml`.
2. **The "Empty Node" Cold Start:**
   - *Risk:* Reorganizing the repo might break historical references.
   - *Mitigation:* Generate JSON-LD contexts (`@context`) that map the new semantic locations, ensuring backward compatibility for legacy queries during the migration.