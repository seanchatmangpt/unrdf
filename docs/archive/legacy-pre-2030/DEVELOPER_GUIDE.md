# UNRDF Developer Guide

This guide synthesizes best practices for developing, testing, and extending the **unrdf** framework. It draws on patterns in the codebase, existing tests, and enterprise conventions.

## Table of Contents
1. [Prerequisites & Setup](#prerequisites--setup)
2. [Project Structure](#project-structure)
3. [Coding Conventions](#coding-conventions)
4. [Composables Pattern](#composables-pattern)
5. [OpenTelemetry Instrumentation](#opentelemetry-instrumentation)
6. [Testing Strategy](#testing-strategy)
7. [Command-Line Interface](#command-line-interface)
8. [Documentation & API Reference](#documentation--api-reference)
9. [Build, Versioning & Release](#build-versioning--release)
10. [Contributing & PRs](#contributing--prs)

---

## 1. Prerequisites & Setup
- Node.js >= 18.x, pnpm (or npm with uv)
- Java 11+ for Testcontainers
- Docker running for integration tests
- `uv` CLI installed globally (for build, test, bump, docs)

Clone and install:
```bash
git clone https://github.com/unrdf/unrdf.git
cd unrdf
pnpm install
```

Run tests:
```bash
uv run test       # unit & BDD specs
uv run test:e2e   # end-to-end tests (Testcontainers)
```

---

## 2. Project Structure
```text
unrdf/
├── src/                  # ESM source modules
│   ├── index.mjs         # main entrypoint (composables exports)
│   ├── composables/      # single-concern API modules
│   ├── knowledge-engine/ # reasoning & persistence logic
│   ├── cli.mjs           # CLI using commander
│   └── utils/            # shared helpers
├── test/                 # Vitest + BDD + integration specs
├── docs/                 # JSDoc configuration
├── examples/             # usage examples and demos
├── package.json          # scripts, dependencies, uv config
└── README.md
```

---

## 3. Coding Conventions
- **ESM** `.mjs` modules only; no TypeScript.
- **JSDoc** on every public function and exported symbol.
- **Zod** for runtime validation in API layers.
- **traced/tracedSync** decorator for OTEL spans (see `/telemetry/tracer.mjs`).
- **Linting**: `eslint --ext .mjs src/ test/ examples/`
- **Formatting**: `prettier --write src/ test/ examples/`

---

## 4. Composables Pattern
Each concern (parsing, querying, validation, reasoning, canonicalization) is a standalone module exposing minimal surface:
```js
/** parse Turtle */
export function useTurtle({input}) { return store }
/** query via Comunica */
export function useQuery(store, sparql) { return results }
```

When adding a new composable:
1. Create `src/composables/yourConcern.mjs`.  
2. Export a single function or `useYourConcern()` factory.  
3. Add JSDoc, types, examples.  
4. Instrument with `traced('yourConcern', fn)`.

---

## 5. OpenTelemetry Instrumentation
Use `/telemetry/start.js` to init SDK and `/telemetry/tracer.js` to wrap spans:
```js
import { traced } from '../telemetry/tracer.mjs'
export const parseTurtle = traced('parseTurtle', ttl => { ... })
```
Ensure all async and sync entrypoints are traced.

---

## 6. Testing Strategy
- **Unit & BDD**: Vitest + `citty-test-utils` for Given–When–Then style.  
- **Performance**: `performance.now()` benchmarks in tests.  
- **Telemetry**: in-memory span exporter tests for `traced`/`tracedSync`.  
- **Integration**: Testcontainers for clean-room verification of CLI and services.

Run all tests:
```bash
pnpm run test
pnpm run test:e2e
```

---

## 7. Command-Line Interface
CLI built with `commander` in `src/cli.mjs`. Best practices:
- Expose subcommands (`render`, `validate`).
- Use `traced('cli.command', ...)` to trace commands.
- Exit with proper codes and clean shutdown (e.g. OTEL shutdown).

---

## 8. Documentation & API Reference
- Generate JSDoc:
  ```bash
  uv docs  # runs jsdoc -c jsdoc.conf.json
  ```
- Host `docs/` on GitHub Pages or internal portal.

---

## 9. Build, Versioning & Release
- Use `uv bump [patch|minor|major]` to update version and changelog.  
- `uv prepublishOnly` ensures build and tests pass.  
- Publish to npm: `npm publish` (release scripts are in package.json).

---

## 10. Contributing & PRs
- Follow `conventional commits` via `commitizen`.  
- Run `pnpm lint`, `pnpm test` before PR.  
- Target `main` with PRs; include screenshots, diagrams.  
- All changes must include tests and JSDoc updates.

---

### Definition of Done Summary
**Chapter DoD:** Structure, content depth, examples, exercises, review, accessibility, branding, CI pass.  
**Sub-Chapter DoD:** Duration, Objective, Learning Objectives, Overview, Key Steps, Example, Exercise, Summary, metadata.