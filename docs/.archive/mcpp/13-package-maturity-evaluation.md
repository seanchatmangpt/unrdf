# Package Production Quality Evaluation

**Date:** 2026-04-24
**Evaluator:** Explore Operator (Gemini)

## Methodology
Evaluation based on:
1. **Tests:** Coverage metrics and pass/fail consistency.
2. **Docs:** Existence of `.specify/` RDF documentation.
3. **Maturity Matrix:** (L1: Research, L5: MCPP Closed).
4. **Integration:** Dependencies (e.g., does it use experimental `OSTAR` features?).

## Evaluation Table

| Package | Status | Maturity | Action |
| :--- | :--- | :--- | :--- |
| `@unrdf/core` | Production | L5 | Keep in `packages/` |
| `@unrdf/oxigraph` | Production | L5 | Keep in `packages/` |
| `@unrdf/semantic-search` | Production | L4 | Keep in `packages/` |
| `@unrdf/ml-versioning` | Production | L4 | Keep in `packages/` |
| `@unrdf/knowledge-engine` | Production | L4 | Keep in `packages/` |
| `@unrdf/yawl` | Production | L4 | Keep in `packages/` |
| `@unrdf/atomvm` | Research | L2 | Move to `exploration/` |
| `@unrdf/dark-matter` | Research | L2 | Move to `exploration/` |
| `@unrdf/kgc-4d` | Research | L2 | Move to `exploration/` |
| `@unrdf/kgc-probe` | Research | L1 | Move to `exploration/` |
| `@unrdf/spatial-kg` | Research | L1 | Move to `exploration/` |
| `@unrdf/v6-compat` | Legacy | L3 | Move to `archive/legacy/` |
| `@unrdf/v6-core` | Legacy | L3 | Move to `archive/legacy/` |
| `@unrdf/docs-site` | Production | L5 | Keep in `apps/` |
| `@unrdf/examples` | Research | L1 | Move to `examples/` |
| `@unrdf/test-utils` | Internal | L4 | Keep in `packages/` |

## Summary of Findings
- **Production (keep in `packages/`):** Those with >85% coverage, SHACL-compliant schemas, and stable `OSTAR` manufacturing receipts.
- **Research (move to `exploration/`):** Those using experimental APIs, missing CI/CD coverage, or missing the `POWL8` execution geometry requirement.
- **Legacy (move to `archive/`):** Those tagged `v6-*` or identified as compatibility shims for previous architectures.

---
*Note: This evaluation must be corroborated by the Exploit Operator (Claude Code) before moving directories to ensure `pnpm` workspaces don't break during the refactor.*