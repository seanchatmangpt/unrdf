# Project Gemini Instructions

This file contains team-shared architecture, conventions, and workflows for the UnRDF project.

## Foundational Principles
- **Vision 2030:** We adhere to the core principles outlined in [VISION-2030-BEST-PRACTICES.md](VISION-2030-BEST-PRACTICES.md).

## Development Workflows
- **Ontology First:** Always update or validate against the relevant ontologies before code changes.
- **Semantic CI/CD:** Ensure all changes pass semantic validation in addition to traditional tests.
- **Agent Collaboration:** Maintain agent-readable metadata and clear intent in all documentation.

## Technical Standards
- **Tech Stack:** Node.js (TypeScript), RDF, SPARQL, SHACL.
- **Backends:** Oxigraph (Rust-based) for persistence, Memory for testing.
- **Testing:** 100% test coverage is the goal for core packages.
