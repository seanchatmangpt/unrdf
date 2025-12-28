# @unrdf/observability API Reference

**Version**: 1.0.0
**Maturity**: stable
**Main Export**: ./src/index.mjs

---

## Description

Innovative Prometheus/Grafana observability dashboard for UNRDF distributed workflows

---

## Installation

```bash
pnpm add @unrdf/observability
```

---

## Exports

**Module Exports**:
- `src/index.mjs`
- `src/metrics/workflow-metrics.mjs`
- `src/exporters/grafana-exporter.mjs`
- `src/alerts/alert-manager.mjs`
- `{`

---

## Dependencies

- `prom-client`
- `@opentelemetry/api`
- `@opentelemetry/exporter-prometheus`
- `@opentelemetry/sdk-metrics`
- `express`
- `zod`

---

## Keywords

`prometheus` · `grafana` · `metrics` · `observability` · `monitoring` · `alerting` · `workflow` · `distributed-systems`

---

## Maturity Signals

| Signal | Status |
|--------|--------|
| Has Tests | ❌ No |
| Has Examples | ✅ Yes |
| Has README | ✅ Yes |
| Has ChangeLog | ❌ No |

---

## Package Role

General utility functions

---

## Resources

- **Source**: [`packages/observability`](../../packages/observability)
- **Tests**: [`packages/observability/test`](../../packages/observability/test)
- **Examples**: [`packages/observability/examples`](../../packages/observability/examples)

- **Full Capability Map**: *Coming soon*

---

**Last Updated**: 2025-12-28
**Generated from**: capability-map.json
