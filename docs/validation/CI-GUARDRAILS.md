## CI Guardrails (Null-Success Prevention)

Recommended pipeline order:
- Lint → Unit (stable) → OTEL Validation → Sidecar Integration (optional) → Security Audit.

Hard-fail conditions:
- Unit tests: total tests == 0.
- OTEL validation: no spans or throughput == 0; missing `coverage/otel-report.json`.
- Artifacts missing or older than the commit.

Sidecar policy:
- Run `node sidecar/scripts/run-integration-tests.mjs` in a separate job.
- Make it required only when PR is labeled `sidecar-required`.


