# Code generation setup (UNRDF)

UNRDF uses **`unrdf sync`** with a **`unrdf.toml`** configuration file at the project root (see `@unrdf/cli`).

Optional repository metadata for ontology outputs lives in **`codegen.toml`** (output under `codegen/generated/` when used).

## Package documentation regeneration

From the monorepo root:

```bash
pnpm run unrdf:discover
pnpm run unrdf:generate
```

See also `docs/diataxis/reference/sync-config.md` and `src/generated/README.md`.
