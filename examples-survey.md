# unrdf as an industry-examples substrate — survey (2026-10-21)

Survey only, no code changes. Repo: `/Users/sac/unrdf` (pnpm monorepo, ESM, Node >= 20,
~59 packages under `packages/`). Scope: third substrate candidate alongside `~/ggen` and
`~/ggen-marketplace` for the industry `examples/` task.

## 1. What unrdf is

`/Users/sac/unrdf/README.md`: "JavaScript platform for building intelligent knowledge
graph applications using semantic web standards (RDF, SPARQL, SHACL)". Not a Rust
toolkit — the Rust layer is confined to `@unrdf/oxigraph` (wraps the `oxigraph` npm
crate, `/Users/sac/unrdf/packages/oxigraph/src/index.mjs:2`) and the Open Ontologies
sidecar binary. Everything else is pure JS/TS (`.mjs`, vitest).

Key packages (from `/Users/sac/unrdf/package.json` workspaces + `docs/PACKAGES.md`):
`@unrdf/core` (store/SPARQL/SHACL), `@unrdf/knowledge-engine` (rules, inference,
canonicalization), `@unrdf/oxigraph` (Rust-backed store), `@unrdf/cli`, `@unrdf/otel`,
`@unrdf/wasm4pm` (WASM process mining), `@unrdf/react`/browser shims
(`BROWSER-COMPATIBILITY-IMPLEMENTATION.md`, `examples/browser-vanilla.html`,
`examples/browser-react.jsx`, `examples/browser-vue.vue`).

## 2. W3C spec coverage

| Spec | Status | Evidence |
|---|---|---|
| RDF 1.1 parse/serialize | ALIVE | `n3` + `@rdfjs/*` in `packages/core/package.json:~72-85` (n3 ^1.26.0, rdf-ext, jsonld ^9) |
| Turtle / N-Triples / N-Quads / JSON-LD | ALIVE | `packages/core/src/validation/shacl-validator.mjs` (turtle parse), `packages/knowledge-engine/src/canonicalize.mjs` (N-Quads writer) |
| RDF-star (RDF 1.2 quoted triples) | ALIVE | `packages/core/test/rdf-star.test.mjs` — `RDFStarFactory`, `quotedTriple`, `isQuotedTriple`, `AnnotationBuilder`; `executeSPARQLStar` in `packages/oxigraph/src/index.mjs:36` |
| SPARQL 1.1/1.2 (incl. SPARQL-star) | ALIVE | `packages/core/src/sparql/` (executor-sync, semantic-executor), Comunica `@comunica/query-sparql` in `packages/knowledge-engine/package.json`, `executeSPARQLStar` via oxigraph |
| SHACL | ALIVE | `rdf-validate-shacl ^0.6.5` in core + knowledge-engine; `packages/core/src/validation/shacl-validator.mjs` (zod-typed reports, `validateGraph`) |
| RDFC-1.0 canonicalization | PARTIAL — ships **URDNA2015** via `rdf-canonize ^5.0.0` (`packages/knowledge-engine/src/canonicalize.mjs`, default `algorithm='URDNA2015'`); rdf-canonize 5.x also supports `RDFC-1.0` by option, but unrdf pins URDNA2015 as default. JSON-LD canonicalization available indirectly via `jsonld` ^9. | `packages/knowledge-engine/src/canonicalize.mjs` |
| OWL/RL / EIF reasoning | PARTIAL | `eyereasoner ^18.23.0` in knowledge-engine deps |

## 2b. CLI / API surface

CLI: `/Users/sac/unrdf/packages/cli/src/cli/main.mjs`, commands in
`packages/cli/src/cli/commands/`: `query.mjs`, `validate.mjs`, `convert.mjs`,
`graph.mjs`, `context.mjs`, `hooks.mjs`, `mcp.mjs`, `sync.mjs` (ggen-style sync),
`init.mjs`, `doctor/`, `pack.mjs`, `publish.mjs`, `template.mjs`, `atomvm.mjs`,
`daemon.mjs`, `telco.mjs`. ggen-parity is explicit: the root `package.json` `mcp:sync`
script drives `packages/cli/src/cli/main.mjs sync --config packages/daemon/src/mcp/.unrdf.toml`
(plus `mcp:sync:dry`), and `ggen.toml` sits at the repo root.

ggen.toml at repo root (`/Users/sac/unrdf/ggen.toml`: project `unrdf-package-reconstitution`
v26.8.1, authority pinned to `seanchatmangpt/ggen-legacy@70e599a5`, zero-unreceipted-actuation
law block) — **unrdf is already manufactured/verified from ggen authority**, so a
ggen-pack "industry examples" lane can target unrdf with the same manifest discipline.

## 3. Minimal working example (from its own examples/tests)

From `/Users/sc/unrdf/examples/01-hello-rdf.mjs` (also README quick-start):

```javascript
import { createStore, namedNode, literal, FOAF, RDF } from '@unrdf/core';

const store = createStore();
store.addQuad(namedNode('http://example.org/Alice'), RDF.type, FOAF.Person);
store.addQuad(namedNode('http://example.org/Alice'), FOAF.name, literal('Alice Smith'));
```

SHACL, from `packages/core/test/validation.test.mjs:99-106` (`validateGraph(data, shapes, {maxErrors})`),
validator at `packages/core/src/validation/shacl-validator.mjs` (wraps `rdf-validate-shacl`,
zod `ValidationReportSchema` with `conforms/results/details`):

```javascript
const report = await validateGraph(data, shapesTTL, { maxErrors: 5 });
// report.conforms, report.results[i].path/message/focusNode/severity
```

Canonicalization (`packages/knowledge-engine/src/canonicalize.mjs`):

```javascript
import { canonicalize } from '@unrdf/knowledge-engine/canonicalize';
const canonical = await canonicalize(store); // URDNA2015 canonical N-Quads, 30s timeout
```

SPARQL via oxigraph backend: `packages/oxigraph/src/index.mjs` exports
`query`, `update`, `executeSPARQLStar`; sync executor at
`packages/core/src/sparql/executor-sync.mjs`; Comunica-based federated/streaming at
`packages/federation`, `packages/streaming`.

## 4. Interop with ggen packs / smalt graphlaw.wasm

- **ggen**: direct. Root `ggen.toml` (authority `ggen-legacy@70e599a5`,
  `[[generation.rules]]` from package.json → `authority/unrdf/package-graph.json`),
  `mcp:sync` CLI command (packages/cli/.../commands/sync.mjs), `.artifacts/ggen-unrdf/`
  replay outputs. Same RDF/SHACL vocabulary family as ggen packs — a TTL ontology
  parsed by unrdf is byte-compatible with what ggen projects from.
- **smalt graphlaw.wasm**: **no direct boundary found** — zero grep hits for `graphlaw`
  or `smalt` across package manifests. But the boundary is cheap: unrdf's
  `@unrdf/oxigraph` wraps the same Rust `oxigraph` crate family graphlaw's kernel
  targets; a JS↔wasm bridge would be a thin adapter mapping `rdf.validate-shacl`
  reports into the graphlaw admission verdict shape. unrdf's own wasm surface
  (`packages/wasm4pm`, PoWL v2 process mining) proves the repo already hosts JS↔WASM
  boundaries — just not this one.
- **purrdf/oxrdf** (Rust crate names): not referenced. The analog is the `oxigraph`
  npm binding (`packages/oxigraph`), i.e. Oxigraph's Rust core behind an npm API, not
  oxrdf's Data-model trait.

## 5. What industry examples/ could use unrdf for that ggen alone can't

1. **Browser-side RDF validation**: `BROWSER-COMPATIBILITY-IMPLEMENTATION.md` +
   `packages/react` + shims (`src/browser/` — IndexedDB virtual FS, BrowserWorker,
   lockchain-writer-browser). ggen is a Rust CLI; it cannot ship SHACL-in-browser.
   Sovereign Console (`smalt workspace/`) bans egress — unrdf's browser build could
   validate TTL shapes client-side inside the Next.js app with zero network.
   `examples/browser-vanilla.html` is the minimal witness.
2. **RDFC-1.0 / URDNA2015 canonical hashes for receipts**: `canonicalize()` gives
   canonical N-Quads suitable for exact-subject digests — a JS-side receipt anchor
   complementary to smalt's DETS/ledger chain. Note the default is URDNA2015, not
   RDFC-1.0; switch via `canonicalize(store, { algorithm: 'RDFC-1.0' })`.
3. **OTEL / event mapping**: `@unrdf/otel` (`packages/otel/package.json`) — weaver
   registry-generated semconv (`src/generated/attributes.mjs`), OCEL export
   (`./ocel`), conformance courts (`./conformance`), collector configs in `otel/`
   (grafana/loki/alloy configs in `otel/`). ggen has no telemetry surface; unrdf can
   emit smalt-style OCEL event logs from agent runs (direct analog of smalt's
   `F` primitive, `lib/xaas/telemetry/ocel_ndjson.ex`).
4. **RDF-star provenance annotations**: `AnnotationBuilder`/`createProvenance` from
   `packages/core/test/rdf-star.test.mjs` — attach confidence/temporal annotations to
   claims in an examples graph, which ggen's static projection can't express.
5. **MCP server exposure of a KG**: `packages/cli/.../commands/mcp.mjs` + daemon MCP
   generate — expose an industry example graph directly to agents without writing a
   server.
6. **Process mining over example event logs**: `@unrdf/wasm4pm` (PoWL v2, WASM
   accelerator) — discover/validate processes from the OCEL logs of (3).

## 6. Verdict

ALIVE as a JS-side complement, PARTIAL as a primary substrate. Strongest unique value
for industry examples/: browser SHACL validation + canonicalization receipts + OTEL/OCEL
mapping — none of which ggen (Rust CLI) or ggen-marketplace (packs) provide. ggen.toml
proves ggen authority already targets this repo, so the ggen-pack lane and an unrdf lane
can share one ontology. No graphlaw.wasm bridge exists; building one is a thin adapter,
not a port. Canonicalization default is URDNA2015 — pin `RDFC-1.0` explicitly if the
W3C spec identity matters.
