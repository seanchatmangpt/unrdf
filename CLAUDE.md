# Claude Code Configuration - UNRDF v26.4.3

> **UNRDF**: Research-grade RDF Knowledge Graph Platform
> **Version**: 26.4.3 (from package.json)
> **Status**: Research Prototype - Architecturally complete, not production-validated
> **Language**: JavaScript ESM (.mjs), Zod runtime validation
> **Package Manager**: pnpm (required - see package.json engines)

---

## Quick Facts (Verified)

| Fact                | Source                                      |
| ------------------- | ------------------------------------------- |
| Version             | `cat package.json` → `"version": "26.4.3"`  |
| Packages            | README.md: 20-package monorepo              |
| Core Packages       | @unrdf/core, @unrdf/oxigraph, @unrdf/hooks  |
| Test Pass Rate      | 100% (from README.md consolidation results) |
| Testing Framework   | Vitest 4.0.16                               |
| Node.js Requirement | >=18.0.0                                    |

---

## Critical Rules

### FIX FORWARD ONLY

- NEVER `git reset --hard` or destructive git operations
- Fix issues in place → debug → apply targeted fixes
- Commits are immutable; solve problems by adding commits
- Exception: `git revert` (creates new commit) is allowed if absolutely necessary

### Code Standards

- **Format**: ESM only (`.mjs` extension)
- **Validation**: Zod schemas for all public APIs
- **File Size**: Max 500 lines per file
- **Test Coverage**: Minimum 80% (line/branch/function)

### Verify, Don't Assume

- Run ALL commands before claiming success: `timeout 5s pnpm test`
- Read actual output — don't assume test pass
- Use authoritative sources: package.json, README.md, actual CLI output
- Measure performance with OTEL validation: `node validation/run-all.mjs comprehensive`

---

## Commands (Verified from package.json)

```bash
# Testing
pnpm test                    # All tests (includes coverage, slow ~2min)
pnpm test:fast               # Fast pre-push suite
pnpm test:coverage           # With coverage reports
pnpm exec vitest run         # Fast test run without coverage (use for iteration)
pnpm exec vitest run <file>  # Run single test file

# Quality
pnpm lint              # ESLint check
pnpm lint:fix          # Auto-fix lint issues
pnpm format            # Prettier format
pnpm quality           # Quality report

# Building
pnpm build             # Build all packages
pnpm clean             # Clean artifacts
pnpm dev               # Start dev servers

# Benchmarks
pnpm bench             # Run benchmarks
pnpm bench:baseline    # Baseline comparison
pnpm profile:cpu       # CPU profiling
```

---

## Package Tiers (20 Total)

### Core (3 packages - production ready)

- **`@unrdf/core`** - RDF storage, SPARQL, SHACL validation
- **`@unrdf/oxigraph`** - Rust-based persistent backend
- **`@unrdf/hooks`** - Autonomous behavior framework

### Extended (4 packages)

- **`@unrdf/daemon`** - Background orchestrator with security
- **`@unrdf/streaming`** - Large graph streaming & sync
- **`@unrdf/federation`** - Distributed query execution
- **`@unrdf/cli`** - Command-line interface

### Optional/Alpha (6 packages)

- @unrdf/browser, @unrdf/react, @unrdf/composables
- @unrdf/dark-matter, @unrdf/project-engine, @unrdf/knowledge-engine (alpha)

### Internal (7+ packages)

- Test utilities, validation, domain schemas, documentation

See [README.md Production Packages section](README.md#production-packages) for current status.

---

## Known Limitations (From README.md)

1. **Research Prototype Status** — Not production-validated
2. **@unrdf/knowledge-engine** — Removed (47% codebase, 0% usage, broken imports)
   - Use @unrdf/core equivalents instead
   - Recoverable from git history if needed
3. **Performance** — Not optimized for production scale (see LIMITATIONS section in README)
4. **Security** — Security audit complete (Dec 2025); see SECURITY-REPORT-ADVERSARIAL-FRAMEWORKS.md

---

## Development Workflow

### Before Committing

```bash
timeout 5s pnpm test:fast    # Pre-push tests
timeout 30s pnpm lint        # Linting check
timeout 60s pnpm build       # Build verification
```

### Git Workflow

- Branch from `main`
- Conventional commits: `type(scope): description`
- Always pull before push
- Never force-push to `main`

### Session Quality Checklist

- [ ] Did I RUN commands, not just read code?
- [ ] Did I read FULL output (not stop at first pass)?
- [ ] Can I prove it works? (show output, not "should work")
- [ ] Are claims verifiable? (use authoritative sources)

---

## Architecture Layers

```
Application Layer (CLI, APIs, Browser)
        ↓
Knowledge Substrate (Hooks, Transactions, Validation)
        ↓
RDF Core (SPARQL, SHACL, Storage)
        ↓
Backends (Memory, Oxigraph, Remote)
```

See [ARCHITECTURE.md](docs/ARCHITECTURE.md) for detailed design.

---

## Key Documentation

| Document                      | Purpose                                      |
| ----------------------------- | -------------------------------------------- |
| **package.json**              | Authoritative version, dependencies, scripts |
| **README.md**                 | Project status, use cases, examples          |
| **ARCHITECTURE.md**           | System design and layers                     |
| **docs/GETTING_STARTED.md**   | Installation and first example               |
| **docs/LOCAL-DEVELOPMENT.md** | Dev environment setup                        |

---

## Hooks Package Gotchas

- **N3 quad spread is broken**: `{...quad}` does NOT copy `subject`/`predicate`/`object`/`graph` from N3 DataFactory quads (prototype getters, not own properties). Always use explicit: `{ subject: quad.subject, predicate: quad.predicate, object: {...}, graph: quad.graph }`.
- **Infinite loops hang vitest**: `while(true)` in hook functions blocks the Node.js event loop — `setTimeout` timeouts cannot interrupt synchronous code. Use async hooks with `await` for timeout testing.
- **Performance thresholds are flaky under full-suite load**: tight μs-level assertions (e.g. `toBeLessThan(2)`) pass in isolation but fail when all 39 test files run concurrently due to system load. Relax thresholds to 50–200ms for CI stability.
- **`executeHooksByTrigger` returns `ChainResult`** (`{ valid, quad, results }`) not an array. Do not use `result[0].valid` — use `result.valid`.

---

## Personal Notes

- **Mark** (~6yr Dachshund) - Noise-sensitive, guards workspace
