# Repository Guidelines

## Project Structure & Module Organization
- `src/` contains runtime modules: `context/` for store wiring, `composables/` for hook helpers, `knowledge-engine/` + `knowledge-engine/` for orchestration, and `cli-v2/` as the active entrypoint; treat `dist/` as ephemeral output.
- Tests sit in `test/` (unit/integration) and `knowledge-engine/test/` (gRPC). Shared fixtures live in `test-data/`, with reusable configs in `env/`.
- Supporting assets reside in `docs/`, `examples/`, `policy-packs/`, while deployment automation is under `k8s/` and `terraform/`.

## Build, Test, and Development Commands
- `pnpm install` — resolve dependencies; stick with PNPM.
- `pnpm dev` — run the CLI locally with live reload.
- `pnpm build` — execute `build.config.mjs` and emit bundles to `dist/`.
- `pnpm test` / `pnpm test:watch` — Vitest with coverage; watch for feature work.
- `pnpm test:e2e` plus `pnpm testcontainers:start|stop` — exercise Terraform/K8s paths.
- `pnpm lint`, `pnpm format:check`, and `pnpm docs` — enforce style and refresh generated docs.

## Coding Style & Naming Conventions
- Pure ESM `.mjs`; 2-space indent, single quotes, trailing commas on multiline literals.
- Guard exports with JSDoc and Zod schemas located beside implementations.
- Filenames use kebab-case; exports use camelCase for functions and PascalCase for types.
- Prefer automated fixes from ESLint/Prettier; justify any rule suppression inline.

## Testing Guidelines
- `pnpm test --coverage` must maintain ≥80% branches/functions/lines/statements as configured in `vitest.config.mjs`.
- Run `pnpm e2e:setup`, `pnpm e2e:run`, and `pnpm e2e:cleanup` around full-stack trials to release containers.
- Annotate new fixtures in `test-data/` with provenance notes.

## Commit & Pull Request Guidelines
- Use imperative commit subjects that name the surface (`knowledge-engine: harden retry flow`); keep bodies focused on rationale and risk.
- PRs should outline change, motivation, and validation (`pnpm lint && pnpm test`), plus logs or screenshots for CLI-facing updates.
- Rebase before requesting review and tag the module owner you affected; reference policy packs or docs when relevant.
- CI runs `pnpm ci:test`; align your local workflow to avoid failures.

## Security & Configuration Tips
- Do not commit secrets; rely on the redacted samples in `env/` and the Vault workflow in `docker-compose.vault.yml`.
- Update `policy-packs/` and `custom-conventions.yaml` together when adding governance metrics.
- Match telemetry attribute names to the custom conventions so dashboards stay consistent.
