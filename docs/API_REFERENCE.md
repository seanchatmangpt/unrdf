# UNRDF API Reference

See the individual package READMEs and source code for authoritative API documentation.

## Core Packages

### `@unrdf/core`

- `createKnowledgeSubstrateCore(options?)` — Initialize the RDF platform
- `KnowledgeSubstrateCore` — Main class, exposes `parseRdf`, `query`, `construct`, `validate`

### `@unrdf/hooks`

- `executeHooksByTrigger(trigger, quad)` — Run registered hooks, returns `ChainResult`
- `ChainResult` — `{ valid: boolean, errors: string[], quads: Quad[] }`

### `@unrdf/federation`

- `createFederationNode(config)` — Create a distributed federation node
- `quorumVote(proposal, peers)` — M-of-N voting

### Admission Engine (`src/admission/admission-engine.mjs`)

- `DecisionResultSchema` — Zod schema for admission decisions
- `AdmissionConfigSchema` — Zod schema for engine configuration
- `AdmissionEngine` — Class that evaluates capsules against admission rules
- `createAdmissionEngine(config?)` — Factory function returning an `AdmissionEngine`
- `wouldAdmit(capsule, config?)` — Stateless helper: returns `true` if capsule passes admission

## Full Documentation

See [PACKAGES.md](PACKAGES.md) for detailed per-package documentation.
