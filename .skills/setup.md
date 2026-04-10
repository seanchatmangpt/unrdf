# Skill: First-Time Setup

Bootstrap the UNRDF workspace for development.

## Steps

1. Install dependencies and build:
   ```bash
   pnpm install && pnpm build && pnpm test:fast
   ```
   Expected: ~3-5 minutes on first run.

2. Verify: `pnpm test:fast` should show 1249+ passing tests.

## Common Failure

If `pnpm install` fails on unrelated packages, install specific packages:
```bash
pnpm install --filter @unrdf/core
```
