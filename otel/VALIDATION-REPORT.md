# UNRDF OTEL Stack -- Validation Report

**Date:** 2026-04-04
**Scope:** 80/20 Framework -- All Tiers
**Weaver Version:** 0.22.1

## Summary

| Component             | Status              | Notes                                                                            |
| --------------------- | ------------------- | -------------------------------------------------------------------------------- |
| OTEL Collector Config | **PASS**            | All 3 pipelines correctly wired (traces->Tempo, metrics->Prometheus, logs->Loki) |
| Custom Conventions    | **PASS**            | 7 groups, 7 metrics, no OTel reserved prefix conflicts                           |
| Dashboard JSON        | **PASS**            | 3 dashboards parse correctly, all reference `${DS_PROMETHEUS}`                   |
| AI SDK Telemetry      | **PASS** (warnings) | GenAI attributes correct; span names deviate from spec, missing `total_tokens`   |
| Weaver Validation     | **PASS**            | `weaver registry check` passed with no policy violations                         |
| SLO Burn Rate Alerts  | **PASS**            | 3 rules in `slo_burn_rate` group with correct burn rate math                     |
| Loki Ruler            | **PASS**            | 2 rules covering error rate and service-down detection                           |
| Alertmanager          | **PASS**            | Default + critical routing, inhibit rules, webhook to Grafana                    |

## Detailed Results

### 1. OTEL Collector Config (`otel-collector-config.yaml`)

**Status: PASS**

| Item       | Value                                                                                                       |
| ---------- | ----------------------------------------------------------------------------------------------------------- |
| Receivers  | `otlp` (gRPC+HTTP), `jaeger` (gRPC+thrift), `prometheus` (scrape)                                           |
| Processors | `batch` (5s/1024), `transform/logs` (trace_id/span_id -> Loki attrs), `resource` (service.name/version/env) |
| Exporters  | `otlp/tempo` (queue=5000), `prometheus` (resource_to_telemetry), `loki`, `debug`                            |
| Extensions | `health_check` on `:13133`                                                                                  |
| Pipelines  | 3 (traces, metrics, logs)                                                                                   |

Pipeline wiring verification:

- **traces**: `otlp, jaeger` -> `batch, resource` -> `otlp/tempo, debug` -- correct
- **metrics**: `otlp, prometheus` -> `batch` -> `prometheus` -- correct
- **logs**: `otlp` -> `batch, transform/logs` -> `loki, debug` -- correct

All receiver/processor/exporter references resolve to defined components.

### 2. Custom Conventions (`custom-conventions.yaml`)

**Status: PASS**

| Item               | Value                                                                                                                            |
| ------------------ | -------------------------------------------------------------------------------------------------------------------------------- |
| Attribute Groups   | 7 (`knowledge_hook`, `policy_pack`, `rdf_graph`, `effect_sandbox`, `crypto_provenance`, `kgc_transaction`, `grpc_sidecar`)       |
| Metric Definitions | 7 (4 histograms, 3 counters)                                                                                                     |
| Prefix Conflicts   | None detected against OTel reserved prefix list                                                                                  |
| Structure          | All attributes have `type`, `brief`, `requirement_level`                                                                         |
| Enum Types         | 6 attributes use member enums (hook_type, hook_result, validation_result, operation_type, execution_mode, circuit_breaker_state) |

Prefixes used: `knowledge_hook`, `policy_pack`, `rdf`, `effect`, `crypto`, `transaction`, `sidecar`

None of these conflict with OTel reserved prefixes (`telemetry`, `otel`, `http`, `rpc`, `db`, `messaging`, `faas`, `net`, `peer`, `service`, `cloud`, `container`, `k8s`, `gen_ai`, `system`, etc.).

### 3. Dashboard JSON Files (`grafana-provisioning/dashboards/`)

**Status: PASS** (minor warnings)

| File                    | Panels | Datasource         | Status     |
| ----------------------- | ------ | ------------------ | ---------- |
| `service-graph.json`    | 6      | `${DS_PROMETHEUS}` | Valid JSON |
| `trace-to-metrics.json` | 10     | `${DS_PROMETHEUS}` | Valid JSON |
| `unrdf-otel.json`       | 9      | `${DS_PROMETHEUS}` | Valid JSON |

All dashboards reference `${DS_PROMETHEUS}` for datasource variables.

**Warnings (non-blocking):**

- All 3 dashboards are missing a `uid` field. Grafana will auto-generate one, but explicit UIDs are recommended for provisioning stability and cross-environment consistency.

### 4. AI SDK Telemetry (`openllmetry/instrumentation.mjs`)

**Status: PASS** (warnings)

**Compliant GenAI attributes used:**

| Attribute                        | Spec Level  | Present |
| -------------------------------- | ----------- | ------- |
| `gen_ai.system`                  | REQUIRED    | Yes     |
| `gen_ai.request.model`           | REQUIRED    | Yes     |
| `gen_ai.response.model`          | OPTIONAL    | Yes     |
| `gen_ai.response.finish_reasons` | OPTIONAL    | Yes     |
| `gen_ai.usage.input_tokens`      | RECOMMENDED | Yes     |
| `gen_ai.usage.output_tokens`     | RECOMMENDED | Yes     |

**Warnings:**

1. **Span name convention**: Current format is `llm.<operation>`. The GenAI semantic convention spec requires `gen_ai.<operation>` (e.g., `gen_ai.chat`, `gen_ai.embed`, `gen_ai.score`). This affects Tempo service graph grouping.
2. **Missing `gen_ai.usage.total_tokens`**: RECOMMENDED attribute since semconv 1.26.0. Currently only `input_tokens` and `output_tokens` are set. Total tokens should be computed as `input + output`.
3. **Non-standard `ai.*` attributes**: `ai.operationId`, `ai.toolCall.name`, `ai.toolCall.args`, `ai.toolCall.result` are Vercel AI SDK-specific and not part of the OTel GenAI spec. These are acceptable for SDK integration but should be documented as vendor-specific.

### 5. Weaver Validation (`otel/registry/`)

**Status: PASS**

```
Weaver Registry Check
Checking registry `otel/registry`
No registry manifest found: otel/registry/manifest.yaml
No `after_resolution` policy violation
```

Weaver 0.22.1 validated the 8 registry files in `otel/registry/`:

- `cli.yaml`, `crypto.yaml`, `knowledge_hook.yaml`, `mcp.yaml`, `metrics.yaml`, `policy.yaml`, `rdf.yaml`, `transaction.yaml`

No policy violations detected. The missing `manifest.yaml` is informational (weaver operates without it).

### 6. SLO Burn Rate Alerts (`alert-rules.yml` -- `slo_burn_rate` group)

**Status: PASS**

| Alert                | Severity | SLO Target | Window | Burn Rate Multiplier |
| -------------------- | -------- | ---------- | ------ | -------------------- |
| `SLOBurnRate5xx`     | critical | 99.9%      | 1h     | 14.4x                |
| `SLOBurnRate5xxSlow` | warning  | 99%        | 5m     | 6x                   |
| `SLOBurnRateLatency` | warning  | 99%        | 5m     | 6x                   |

Total alert rules across all groups: 10 (5 infrastructure + 2 Tempo + 3 SLO)

All rules have proper `for` thresholds, severity labels, and summary annotations.

### 7. Loki Ruler (`loki-rules/rate-limit-errors.yaml`)

**Status: PASS**

| Alert           | Severity | Detection                                       |
| --------------- | -------- | ----------------------------------------------- |
| `HighErrorRate` | warning  | >10% error log rate in unrdf containers over 5m |
| `ServiceDown`   | critical | Zero logs from unrdf container over 10m         |

LogQL expressions use proper `{container=~".*unrdf.*"}` label matchers with pipeline-level filtering.

### 8. Alertmanager (`alertmanager.yml`)

**Status: PASS**

| Item              | Value                                                      |
| ----------------- | ---------------------------------------------------------- |
| Default receiver  | `default` (webhook to Grafana)                             |
| Critical receiver | `critical` (webhook to Grafana, 0s group_wait, 5m repeat)  |
| Grouping          | `['alertname', 'service']`                                 |
| Inhibit rules     | 1 (critical suppresses warning for same alertname+service) |
| Resolve timeout   | 5m                                                         |

Both receivers route to `http://grafana:3000/api/alertmanager/grafana/receiver`. Route and inhibit rule references all resolve to defined receivers.

## Warnings

1. **Dashboard UIDs missing** -- All 3 Grafana dashboards lack explicit `uid` fields. Grafana auto-generates UIDs, but explicit UIDs prevent drift across reprovisions. Add a `uid` field to each dashboard JSON (e.g., `"uid": "service-graph"`, `"uid": "trace-to-metrics"`, `"uid": "unrdf-otel"`).

2. **Span name convention in AI SDK** -- `instrumentation.mjs` uses `llm.<operation>` for span names instead of the GenAI spec's `gen_ai.<operation>`. This will cause Tempo to group these spans under an `llm` service rather than alongside other GenAI-compliant traces.

3. **Missing `gen_ai.usage.total_tokens`** -- The `withLLMSpan` function sets `input_tokens` and `output_tokens` individually but does not compute `total_tokens` as recommended by the spec.

4. **Weaver manifest missing** -- `otel/registry/manifest.yaml` does not exist. Weaver works without it, but a manifest enables dependency resolution and version tracking for the custom registry.

5. **`rdf` prefix ambiguity** -- The `rdf` prefix in custom conventions is valid for OTel but overlaps with the W3C RDF namespace URI. Consider `rdf_graph` or `kgc_rdf` to avoid confusion in mixed RDF/OTel contexts.

6. **Alertmanager single point of failure** -- Both `default` and `critical` receivers send to the same Grafana webhook. If Grafana is down, no alerts are delivered. Consider adding a secondary receiver (email, Slack, PagerDuty) for the critical route.

## Recommendations

**Priority 1 -- Fix for spec compliance:**

- Rename span names in `instrumentation.mjs` from `llm.<operation>` to `gen_ai.<operation>`
- Add `gen_ai.usage.total_tokens` computation in `withLLMSpan`

**Priority 2 -- Stability improvements:**

- Add explicit `uid` fields to all dashboard JSON files
- Create `otel/registry/manifest.yaml` for weaver dependency tracking

**Priority 3 -- Operational hardening:**

- Add a secondary Alertmanager receiver (Slack/PagerDuty) for critical alerts
- Consider renaming `rdf` prefix to `kgc_rdf` to disambiguate from W3C namespace
