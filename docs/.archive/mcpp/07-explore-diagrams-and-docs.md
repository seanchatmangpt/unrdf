# Exploration: Holographic Diagrams & Documentation Generation

**Date:** 2026-04-24
**Role:** Explore Operator (Gemini)
**Objective:** Decompose the requirement for "Diagrams (C4, etc)" and "docs/" under the Markdown LLM paradigm, ensuring documentation is a lossless projection of the ontology.

## Holographic Documentation (Markdown LLM)

The MCPP architecture dictates that Markdown is not the source of truth; it is a generated projection of the `O*` RDF graph.

### Candidate: Generating C4 Architecture Diagrams
- **Current State:** C4 diagrams are manually drawn in PlantUML or Mermaid.
- **Exploration:** Use SPARQL queries against the `O*` graph to generate Mermaid/PlantUML syntax dynamically.
  - *Context:* `org:Organization` and `foaf:Agent` map to C4 "Person/Actor".
  - *System:* `adms:Asset` or `dcat:DataService` maps to C4 "Software System".
  - *Container:* Sub-components map to C4 "Container".
  - *Relationships:* `prov:wasDerivedFrom` or custom `mcpp:interactsWith` map to C4 arrows.
- **Workflow:** `ggen` or `OSTAR` executes the SPARQL query, renders the Mermaid template, and outputs it to `docs/diagrams/`.

### Candidate: `docs/` as a Compiled Artifact
- **Exploration:** The `docs/` directory should be treated exactly like a `dist/` directory.
- Developers write `.specify/*.ttl` (Turtle files) using `DCAT` and `ADMS`.
- The `OSTAR` manufacturing layer compiles the Turtle files into static `.md` files in `docs/`.
- **Validation:** If someone manually edits a file in `docs/`, a Git pre-commit hook (or CI check) compares the hash against the receipt and rejects the commit, enforcing the "Markdown LLM" rule.

## Surfaced Risks

1. **Human Readability of Source (RDF vs. Markdown):**
   - *Risk:* Forcing developers to write raw Turtle (`.ttl`) or JSON-LD is cognitively expensive.
   - *Mitigation:* The Explore layer (Gemini) acts as the ingest interface. The developer writes a rough natural language spec or markdown draft. Gemini generates the `ΔO_candidate` RDF. The developer approves the RDF, and the system generates the polished, canonical Markdown.
2. **Diagram Drift:**
   - *Risk:* The generated C4 diagrams might lack visual aesthetic or layout optimization compared to manually tweaked diagrams.
   - *Mitigation:* Rely on robust auto-layout algorithms in Mermaid/PlantUML, accepting slight visual sub-optimality in exchange for 100% architectural accuracy.