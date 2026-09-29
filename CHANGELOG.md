# Changelog

Release versions are CalVer (`YY.M.D`). The workspace is version-agnostic: every
`package.json` carries a placeholder, and the release workflow stamps the tag version
(`make version-bump VERSION=<tag>`) before building and publishing.

The release workflow extracts the section headed `## [<version>]` from this file for the
GitHub Release body, so keep a following `## [` heading below each release.

## [26.9.28] - 2026-09-28

Follows 26.9.25. Adds the AtomVM continuum, ships the semantic-parts package, and repairs
a workspace-wide version sweep that had corrupted literals, imports and tests.

### Added

- **AtomVM continuum** (`@unrdf/atomvm`, `src/continuum/`): a cloud / fog / edge / browser
  tier topology (`tiers.mjs`), an HTTP `TierNode` with a `/health` endpoint that boots the
  real AtomVM runtime and proves liveness with an Erlang tier witness before accepting
  requests (`tier-node.mjs`), a browser tier client (`browser-client.mjs`) and receipt-chain
  validation across tiers (`receipt-chain.mjs`).
- AtomVM tooling: `avm-packer.mjs` and `scripts/pack-beam.mjs` to package BEAM files,
  `scripts/build-tier-fixtures.mjs`, the `atomvm-wasm` CLI, and example BEAM programs.
- Continuum e2e tests for cloud, fog, edge and browser tiers, and the
  `atomvm-continuum.yml` CI workflow.
- `@unrdf/semantic-parts`: deterministic inverted index with ranked alternatives, semantic
  delta and exact signature classes, marketplace RDF projection, and a transport-neutral
  CodeGraph release reader protocol; gated by an Interchangeable Parts CI workflow that
  actually runs the falsifiers.
- `@unrdf/project-engine` / `@unrdf/cli`: ported init pipeline.
- ESLint flat config (`eslint.config.mjs`).

### Fixed

- Repaired damage from the version-agnostic sweep: restored corrupted literals (IPs, hosts,
  OpenAPI `3.0.0`, data files), stale `[VERSION]` placeholders, and mangled spec URLs.
- `@unrdf/cli`: crash on startup (undeclared dependencies, `js-yaml`); the `--version`
  output test is now CI-proof.
- `@unrdf/knowledge-engine`: `query.mjs` used an unimported `Store` on the CONSTRUCT path;
  `getPerformanceMetrics()` called an undefined `getRuntimeConfig()` and now falls back to
  the current cache size.
- `@unrdf/kgc-4d` / `@unrdf/kgc-substrate`: unified universe graph IRIs so snapshots and
  time-travel replay work (replay had been a no-op).
- `@unrdf/consensus`: transport start now awaits `listen` and survives refused peers;
  `verifyProof` return value.
- `@unrdf/ai-ml-innovations`: secure-aggregation masks now cancel; RDP accountant fixed.
- `@unrdf/blockchain`: `calculateGasSavings` no longer truncates the percentage.
- `@unrdf/codegen`, `@unrdf/caching`, `@unrdf/kgc-probe`, `@unrdf/chatman-equation`,
  `@unrdf/diataxis-kit`, `@unrdf/kgc-cli`, `@unrdf/project-engine`, `@unrdf/test-utils`,
  `@unrdf/serverless`, `@unrdf/federation`, `@unrdf/rdf-graphql`: Zod v4 schema fixes,
  broken imports and previously never-run test suites.
- CI: Node 20 compatibility for `node --test` package scripts (routed through
  `scripts/run-node-tests.mjs`), Chromium install in the package matrix, replacement of
  shut-down action versions, and a working `release.yml` for the version-agnostic monorepo.
- Wall-clock test assertions made robust (best-of-N, bounds unchanged).

### Removed

- Husky hooks.
- Legacy workflow benchmark files (`workflow-e2e-bench.mjs`, `task-activation-bench.mjs`,
  `engine-performance.mjs`, `workflow-performance.mjs`).

### Known issues

Carried over from the base branch; not regressions in this release.

- Tests needing the `open-ontologies` binary fail without it: `@unrdf/hooks` (1),
  `@unrdf/daemon` (5 files), `@unrdf/chatman-equation`. The gitlink has no `.gitmodules`
  entry, so CI cannot build it.
- `@unrdf/kgn` and `kgc-4d-playground` builds fail.
- `@unrdf/decision-fabric` (jest) rejects the `-- --coverage` suffix.
- Entry-point breakage in `@unrdf-examples/knowledge-rag`, `@unrdf/codegen` and
  `@unrdf/collab`.
- 68 files fail `prettier --check`; `CLI Sync Command Tests` fails on a missing example
  ontology; the sidecar OTEL job finds no test files.

## [26.9.25]

See the `v26.9.25` production closure (#110) and #112/#113.
