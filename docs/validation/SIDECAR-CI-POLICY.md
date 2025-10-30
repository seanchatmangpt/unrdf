## Sidecar CI Policy

- Keep sidecar integration tests in a dedicated CI job.
- Start Nuxt server and then run tests via:

```bash
node sidecar/scripts/run-integration-tests.mjs
```

- Mark the job required only when PRs change `sidecar/**` or carry label `sidecar-required`.
- For local runs, see `sidecar/TESTING.md`.


