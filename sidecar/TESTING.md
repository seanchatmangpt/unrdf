## Sidecar Integration Testing

Run Nuxt server and execute integration tests as a two-step CI job.

### Local

```bash
pnpm --filter @unrdf/sidecar dev &
node sidecar/scripts/run-integration-tests.mjs
```

### CI (single step)

```bash
node sidecar/scripts/run-integration-tests.mjs
```

Notes:
- The script waits for http://localhost:3000 before running tests.
- Uses PNPM exclusively.


