# Autonomic Hyper-Intelligence Swarm Developer Guide

This guide outlines best practices for building **autonomic hyper-intelligence swarms**—distributed agents that self-govern, reason over knowledge graphs, and collaborate in real time.

## 1. Architecture Overview
- **Agents:** Autonomous processes implementing behaviors (e.g. data ingestors, validators, inferencers).  
- **Core Engine:** Manages agent lifecycle, event dispatch, and state storage.  
- **Knowledge Base:** RDF store (N3.Store) + policy/rule layers.  
- **Lockchain:** Cryptographic ledger for provenance and audit.  
- **CLI & API:** Interfaces for orchestration and integration.  
- **Observability:** Telemetry (OpenTelemetry), logs, metrics.

## 2. Environment & Dependencies
- Node.js >=18, ESM (.mjs) only.  
- pnpm workspaces or uv CLI.  
- vm2 for sandboxed agent execution.  
- @comunica/query-sparql, rdf-validate-shacl, eyereasoner, rdf-canonize, jsonld.  
- OpenTelemetry: @opentelemetry/sdk-node, api, auto-instrumentations-node.  
- Testcontainers for integration tests.

## 3. Code Structure & Patterns
```text
swarm/              # root
├── src/
│   ├── agents/     # agent modules (one per concern)
│   ├── composables/ # parse, query, validate, reason, canon
│   ├── core/       # Swarm engine, registry, scheduler
│   ├── lockchain/  # crypto-ledger adapter
│   ├── cli.mjs     # CLI commands
│   └── index.mjs   # main facade
├── test/           # unit, BDD, integration
└── docs/           # JSDoc, developer docs
```

### Module Guidelines
- **Single Responsibility:** one concept per composable.  
- **JSDoc:** describe inputs, outputs, and behavior.  
- **Telemetry:** wrap public functions with `traced()`/`tracedSync()`.  
- **Validation:** use Zod for config and API payloads.

## 4. Agent Design
- Implement `createAgent(context)` returning event handlers.  
- Register agents via `agents/index.mjs` import convention.  
- Use async/await, avoid shared mutable state.  
- Sandbox untrusted code in vm2 with resource limits.

## 5. Data Flow & Coordination
- **Event Bus:** publish-subscribe for quad insertions, queries, validation results.  
- **Message Contracts:** JSON or RDF JSON-LD with schema validation.  
- **Backpressure:** limit concurrent agent invocations to prevent overload.

## 6. Policy & Reasoning Integration
- Load SHACL shapes via rdf-validate-shacl.  
- Run reasoning with eyereasoner, TTL rules.  
- Canonicalize graphs with rdf-canonize for isomorphism checks.

## 7. Observability & Tracing
- Initialize OTEL SDK in `telemetry/otel-setup.mjs`.  
- Use `traced(name, fn)` decorators on all composables.  
- Instrument CLI in `cli.mjs` with spans for each command.  
- Expose metrics via Prometheus exporter and logs in JSON format.

## 8. Testing Strategy
- **Unit Tests:** test composables with Vitest + citty-test-utils.  
- **Performance Tests:** benchmark large-graph operations (<500ms parse, <100ms query).  
- **Telemetry Tests:** in-memory OTEL exporter to verify spans.  
- **Integration:** Testcontainers spin up Redis/postgres/lockchain; run CLI end-to-end.  
- **Chaos Tests:** simulate failures/timeouts in agent sandboxes.

## 9. CI/CD & Release
- **Lint & Format:** ESLint + Prettier on commit.  
- **Precommit Hook:** run `pnpm lint && pnpm test`.  
- **CI Pipeline:** uv tasks for `test`, `test:e2e`, `docs`, `bump`, `publish`.  
- **Versioning:** Semantic via `uv bump`, auto-changelog.  
- **Containers:** Docker builds tagged by version; Helm charts for Kubernetes.

## 10. Definition of Done
**Chapter Level:** duration, objectives, examples, exercises, visuals, review by SME + ID + accessibility, CI green.  
**Module Level:** JSDoc, traced, test coverage ≥90%, performance, telemetry, integration check.

---

*This guide extends the core UnRDF patterns to any autonomic hyper-intelligence swarm framework.*