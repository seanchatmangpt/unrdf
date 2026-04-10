# Build & Quality Commands — UNRDF

## Testing

```bash
pnpm test:fast                                           # Pre-push suite (1249 tests, ~15s)
pnpm --filter @unrdf/core test                            # Single package
pnpm --filter @unrdf/core exec vitest run test/foo.test.mjs   # Single file
pnpm test:watch                                        # Watch mode
pnpm test:coverage                                      # Coverage reports
```

## Lint & Build

```bash
pnpm lint                       # Check all packages
pnpm lint:fix                   # Auto-fix issues
pnpm --filter @unrdf/core lint   # Single package
pnpm build                      # Build all
pnpm clean                      # Clean artifacts
pnpm format                     # Prettier format all
pnpm format:check               # Check formatting
```

## Pre-commit Gate

```bash
pnpm mcp:sync && pnpm lint && pnpm test:fast
```

## Install & Filter

Full `pnpm install` may fail on unrelated packages. Always use:
```bash
pnpm install --filter @unrdf/X
```
