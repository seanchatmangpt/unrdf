# UNRDF — Claude Code Configuration

## UNRDF v26.4.23 - RDF Knowledge Graph Platform

**REQUIRED READING:**

- **Root instructions**: See `../CLAUDE.md` (Constitutional Law, Evidence Standards, Git Safety, Dependencies)

> **Status**: Research Prototype | **Language**: JavaScript ESM (.mjs), Zod validation
> **Package Manager**: pnpm (required) | **Test Framework**: Vitest 4.1.x

| Fact           | Value                                                  |
| -------------- | ------------------------------------------------------ |
| Version        | workspace 26.4.23                                       |
| Packages       | 20 (YAWL/KGC moved to separate repos)                  |
| Node.js / pnpm | >=18.0.0 / >=7.0.0 (.tool-versions: 24.11.1 / 10.33.0) |

---

## Critical Rules

- **FIX FORWARD ONLY** — NEVER `git reset --hard`; fix in place, add commits
- **ESM only** (`.mjs`), **Zod schemas** for public APIs, **max 500 lines** per file
- **Verify, don't assume**: run code, show output, read full results. No claim without evidence.

---

## Commands

```bash
pnpm install && pnpm build && pnpm test:fast   # First-time bootstrap
pnpm test:fast                               # Pre-push suite (1249 tests, ~15s)
pnpm --filter @unrdf/cli test                # Single package
pnpm --filter @unrdf/core exec vitest run test/foo.test.mjs  # Single file
pnpm lint && pnpm build                     # Lint + build
pnpm mcp:sync                               # Sync MCP definitions (pre-commit)
```

---

## Architecture

```
Application Layer (CLI, APIs, Browser, MCP)
        ↓
O* Innovations (Federation, Marketplace, Streaming)
        ↓
Knowledge Substrate (Hooks, Transactions, Validation)
        ↓
RDF Core (SPARQL, SHACL, Storage)
        ↓
Backends (Memory, Oxigraph, Remote)
```

**Core packages**: `@unrdf/core`, `@unrdf/oxigraph`, `@unrdf/hooks`, `@unrdf/v6-core`
**Extended**: `@unrdf/daemon`, `@unrdf/streaming`, `@unrdf/federation`, `@unrdf/cli`, `@unrdf/otel`

### O\* Innovations

- **Innovation 4** — Federation Quorum: M-of-N voting, BLAKE3 receipt chaining → `packages/federation/`
- **Innovation 5** — Hooks Marketplace: SPARQL CONSTRUCT, N3 forward-chaining, SHACL soft-fail → `packages/hooks/`
- **Innovation 6** — Streaming Admission: Delta receipts with chaining, I/O BLAKE3 hashes → `packages/streaming/`

### MCP & Self-Play Autonomics (v26.4.23+)

- 15 tools for ontology operations (`onto_validate`, `onto_query`, `onto_reason`, etc.)
- Key files: `packages/daemon/src/mcp/`, `docs/MCP_INTEGRATION.md`, `packages/daemon/GROQ-INTEGRATION.md`
- Quick-start: `pnpm --filter @unrdf/daemon start` then `node packages/cli/src/cli/main.mjs mcp:list`

---

## Gotchas

### Zod v4

- `.args()` / `.returns()` removed → use bare `z.function()`
- `parse()` returns plain object (prototype methods lost) → validate-only pattern
- `eslint-env` comments ignored → use `/* global window, document */`

### Hooks Package

- **N3 quad spread broken**: `cloneQuad(quad, {...overrides})` instead of `{...quad}`
- **`executeHooksByTrigger` returns `ChainResult`**: use `result.valid`, not `result[0].valid`
- **Infinite loops hang vitest**: `while(true)` blocks event loop; use async hooks

### Template Renderer

- **Empty if-block fallthrough**: `if (cond) { /* handled */ } else if ...` with empty body matching → ALL else branches skip, silently no-op. Remove the block.
- **Auto-quoting of template syntax**: Values containing `{{ }}` in YAML frontmatter are automatically quoted before parsing (js-yaml 3.x compatibility). Example: `to: {{ output_dir }}/file.py` → `to: "{{ output_dir }}/file.py"`. Works in `renderTemplate()` and `batchRender()`.
- **Preprocessing limitations**: Simple regex-based approach handles key-value pairs, not complex nested YAML. Test against actual template patterns, not edge cases.
- **Regex vs string anchor**: `before`/`after` support regex (`/MARKER.*/`). Heuristic: `/\/[gimsuy]*$/` (must END with `/`/flags). `startsWith('/')` falsely matches `// MARKER`.
- **Dual mode systems**: `getOperationMode()` (write/inject/before/after/append/prepend/lineAt) vs `frontmatter.mode` (overwrite/append/skip_existing/prepend) are separate. Check both `opMode.mode` AND `result.mode`.
- **force from frontmatter**: `effectiveForce = options.force || frontmatter.force`; all skip checks must use `effectiveForce`

### OTel Weaver

- Binary: `~/.cargo/bin/weaver` v0.22.1+; commands: `registry check`, `registry generate`, `registry live-check`
- Template path: `templates/{registry_basename}/{target}/`; iterate `ctx.groups` NOT `ctx`
- `@unrdf/otel` is generated — don't edit `packages/otel/src/generated/` directly

### Test Utils (`@unrdf/test-utils`)

- **Import pattern**: relative imports (pnpm workspace linking unreliable)
- **Quote stripping in tests**: `extractFrontmatter()` utility strips quotes from values. When testing templates with quoted `{{ }}` values, assertions must account for quote removal.
- **pnpm install blocker**: full `pnpm install` may fail → use `pnpm install --filter @unrdf/X`
- **Pre-existing failures**: `federation/test/metrics.test.mjs` (21 failures) — do NOT fix unless asked

### Integration Tests

- open-ontologies: skips if `~/.local/bin/open-ontologies` missing
- Groq: skips if `GROQ_API_KEY` not set
- Timing: relax μs-level assertions to 50–200ms; use 10x multipliers for CI
- OTEL init warning in daemon tests is non-blocking (`__vite_ssr_import_1__.default.default is not a function`)

---

## Troubleshooting

| Problem              | Fix                                                          |
| -------------------- | ------------------------------------------------------------ |
| `pnpm install` fails | `pnpm install --filter @unrdf/core`                          |
| Tests timeout/hang   | `timeout 5s pnpm test:fast`; grep `while(true)` for blockers |
| Port in use          | `lsof -ti:8090 \| xargs kill -9`                             |
| MCP tools not found  | `pnpm --filter @unrdf/daemon build && pnpm mcp:sync`         |
| Vitest watch stale   | `rm -rf node_modules/.vite && pnpm test:watch`               |

---

## Git Workflow

- Branch from `main`, conventional commits: `type(scope): description`
- Never force-push to `main`; always pull before push
- Pre-commit: `pnpm mcp:sync && pnpm lint && pnpm test:fast`

## Code Generation (unrdf) Gotchas

- **Nunjucks filter syntax**: Use parentheses `filter(arg)` not colons `filter: arg`. Wrong: `| join: ', '`. Right: `| join(', ')`.
- **Template output paths**: Rule `outputPath` takes precedence over template `to:` field via `context.outputPath || frontmatter.to`.
- **Dry-run file checks**: Wrap `fs.access()` with `if (!dryRun)` to allow CI validation without requiring files to exist on runner.
- **SPARQL-to-template variables**: SPARQL results produce clean key names (no `?` prefix) but both `system` and `?system` available in templates. Must match rule SPARQL SELECT bindings exactly (case-sensitive).

## Key Docs

| Document                              | Purpose                                      |
| ------------------------------------- | -------------------------------------------- |
| `package.json`                        | Authoritative version, dependencies, scripts |
| `docs/MCP_INTEGRATION.md`             | MCP protocol and tools guide                 |
| `packages/daemon/GROQ-INTEGRATION.md` | Groq LLM integration                         |
| `docs/GETTING_STARTED.md`             | Installation and first example               |

---

## Personal

- **Mark** (~6yr Dachshund) - Noise-sensitive, guards workspace
