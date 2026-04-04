# Weaver Validation Report -- UNRDF OTEL Instrumentation

**Date:** 2026-04-04
**Scope:** Daemon package + OTEL stack (collector, Tempo, Prometheus, Loki, Grafana dashboards)
**Validator:** Manual Weaver-compatible analysis (OTel Weaver CLI package `@opentelemetry/weaver` does not exist on npm; the `weaver` binary on PATH is an unrelated process manager)

---

## Summary

| #   | Validation Step                            | Status | Issues                                                                                                   |
| --- | ------------------------------------------ | ------ | -------------------------------------------------------------------------------------------------------- |
| 1   | Weaver CLI availability                    | WARN   | `@opentelemetry/weaver` package not found on npm; manual validation performed instead                    |
| 2   | Custom conventions YAML structure          | PASS   | Well-formed; 7 groups, 7 metrics; proper attribute types and prefixes                                    |
| 3   | AI SDK `experimental_telemetry` attributes | WARN   | 2 issues: deprecated `gen_ai.system` vs `gen_ai.provider.name`; missing required `gen_ai.operation.name` |
| 4   | Cross-prefix conflict check                | PASS   | No conflicts between custom `mcp.tool.*`, `knowledge_hook.*`, `rdf.*`, etc. and official conventions     |
| 5   | OTEL Collector config                      | PASS   | Valid syntax; correct pipeline wiring                                                                    |
| 6   | Loki + Ruler config                        | PASS   | Valid config; syntactically correct LogQL alert rules                                                    |
| 7   | Dashboard JSON validation                  | PASS   | All 3 dashboards valid JSON; correct datasource refs and metric names                                    |
| 8   | OTEL SDK initialization                    | WARN   | Deprecated `SemanticResourceAttributes`; duplicate `traceExporter` + `spanProcessor` in NodeSDK config   |
| 9   | MCP tool instrumentation wiring            | PASS   | 39 tools instrumented via `withMcpSpan`                                                                  |
| 10  | OTEL context propagation                   | PASS   | Correct W3C traceparent parsing/formatting; proper context injection/extraction                          |

**Overall:** 6 PASS, 4 WARN, 0 FAIL

---

## Step 1: Weaver CLI Availability

**Status: WARN**

The `@opentelemetry/weaver` package does not exist on the npm registry (verified via `npm info` returning 404). The `weaver` binary found at `/Users/sac/.cargo/bin/weaver` is an unrelated Rust-based process manager (`weaver --help` shows task/process management commands, not OTel validation).

**Impact:** All validation performed manually against the OTel Semantic Conventions registry spec (fetched from `https://github.com/open-telemetry/semantic-conventions`).

**Remediation:** None required for validation accuracy. The OTel Semantic Conventions are published as YAML/Markdown in the GitHub repo, enabling manual validation. If automated Weaver validation becomes available in the future, the `custom-conventions.yaml` is already in a compatible format.

---

## Step 2: Custom Conventions YAML Validation

**File:** `/Users/sac/unrdf/otel/custom-conventions.yaml`
**Status: PASS**

### Structure Validation

| Field                         | Present        | Valid                                                     |
| ----------------------------- | -------------- | --------------------------------------------------------- |
| `groups` (top-level)          | Yes            | Well-formed array of 7 groups                             |
| `metrics` (top-level)         | Yes            | Well-formed array of 7 metrics                            |
| Group `id`                    | All 7 groups   | Unique, snake_case                                        |
| Group `prefix`                | All 7 groups   | Unique, snake_case                                        |
| Group `brief`                 | All 7 groups   | Present                                                   |
| Attribute `id`                | All attributes | Unique within groups                                      |
| Attribute `type`              | All attributes | Valid: `string`, `int`, `boolean`, or enum with `members` |
| Attribute `brief`             | All attributes | Present                                                   |
| Attribute `requirement_level` | All attributes | Valid values: `required`, `recommended`, `optional`       |
| Metric `name`                 | All 7 metrics  | Unique, snake_case                                        |
| Metric `instrument`           | All 7 metrics  | Valid: `histogram` or `counter`                           |
| Metric `unit`                 | All 7 metrics  | Present                                                   |
| Metric `attributes`           | All 7 metrics  | Valid prefixed attribute references                       |

### Groups Summary

1. `knowledge_hook` (prefix: `knowledge_hook`) -- 6 attributes
2. `policy_pack` (prefix: `policy_pack`) -- 6 attributes
3. `rdf_graph` (prefix: `rdf`) -- 5 attributes
4. `effect_sandbox` (prefix: `effect`) -- 4 attributes
5. `crypto_provenance` (prefix: `crypto`) -- 4 attributes
6. `kgc_transaction` (prefix: `transaction`) -- 5 attributes
7. `grpc_sidecar` (prefix: `sidecar`) -- 4 attributes

### Metrics Summary

1. `knowledge_hook_duration` (histogram, ms)
2. `policy_pack_violations` (counter, {violation})
3. `rdf_quad_operations` (counter, {quad})
4. `effect_sandbox_executions` (counter, {execution})
5. `crypto_signature_verifications` (counter, {verification})
6. `transaction_latency` (histogram, ms)
7. `sidecar_requests_total` (counter, {request})

### Minor Observation

Enum types use inline `type: { members: [...] }` rather than `type: enum` with a separate `enum:` block. This is the newer OTel semantic convention registry format and is valid for Weaver-compatible conventions.

---

## Step 3: AI SDK `experimental_telemetry` Attribute Validation

**Status: WARN (2 issues)**

### Call Sites Verified

| File                                                   | Line | `experimental_telemetry` Present | Function ID                |
| ------------------------------------------------------ | ---- | -------------------------------- | -------------------------- |
| `packages/daemon/src/autonomous-agent.mjs`             | 52   | Yes                              | `autonomous-agent.reason`  |
| `packages/daemon/src/autonomous-refinement-engine.mjs` | 428  | Yes                              | `refinement-engine.decide` |

Both call sites correctly use `isEnabled: true` and include `metadata` with `functionId` and domain-specific context.

### Issue 3a: Deprecated `gen_ai.system` Attribute

**Severity:** Low (AI SDK internal, not custom code)

The Vercel AI SDK `experimental_telemetry` emits the attribute `gen_ai.system` internally (this is controlled by the AI SDK itself, not by UNRDF code). Per the [latest OTel GenAI semantic conventions](https://github.com/open-telemetry/semantic-conventions/blob/main/docs/gen-ai/gen-ai-spans.md), `gen_ai.system` has been **renamed** to `gen_ai.provider.name`.

- **Old (v1.36.0):** `gen_ai.system` = `openai`, `anthropic`, etc.
- **New (current):** `gen_ai.provider.name` = `openai`, `anthropic`, `groq`, etc.

The OTel spec explicitly documents a transition plan: instrumentations should default to v1.36.0 behavior unless `OTEL_SEMCONV_STABILITY_OPT_IN=gen_ai_latest_experimental` is set. The AI SDK follows this pattern, so this is expected behavior but should be noted for dashboard queries that filter on `gen_ai.system` vs `gen_ai.provider.name`.

**Remediation:** Set the environment variable `OTEL_SEMCONV_STABILITY_OPT_IN=gen_ai_latest_experimental` when running the daemon to get the newer attribute names. Update any dashboard filters accordingly.

### Issue 3b: Missing `gen_ai.operation.name` Attribute

**Severity:** Medium

The current OTel GenAI spec requires `gen_ai.operation.name` (values: `chat`, `text_completion`, `embeddings`, `generate_content`, etc.). The AI SDK's `experimental_telemetry` should emit this automatically, but there is no explicit verification or supplementary attribute being set in the UNRDF call sites.

The `functionId` in the metadata (e.g., `autonomous-agent.reason`) is UNRDF-specific and does not map to the OTel `gen_ai.operation.name` convention.

**Remediation:** After enabling `gen_ai_latest_experimental`, verify that the AI SDK emits `gen_ai.operation.name=chat` on the generated spans. If not, consider adding a span processor to set it.

### AI SDK Attribute Compatibility Matrix

| AI SDK Attribute                 | OTel Spec Attribute              | Status                      |
| -------------------------------- | -------------------------------- | --------------------------- |
| `gen_ai.system`                  | `gen_ai.provider.name` (renamed) | WARN: deprecated            |
| `gen_ai.request.model`           | `gen_ai.request.model`           | PASS: matches               |
| `gen_ai.request.temperature`     | `gen_ai.request.temperature`     | PASS: matches               |
| `gen_ai.response.model`          | `gen_ai.response.model`          | PASS: matches               |
| `gen_ai.usage.input_tokens`      | `gen_ai.usage.input_tokens`      | PASS: matches               |
| `gen_ai.usage.output_tokens`     | `gen_ai.usage.output_tokens`     | PASS: matches               |
| `gen_ai.response.finish_reasons` | `gen_ai.response.finish_reasons` | PASS: matches               |
| `ai.operationId`                 | (none -- SDK-specific)           | OK: custom prefix           |
| `ai.toolCall.name`               | `gen_ai.tool.name`               | WARN: use OTel attr instead |
| `ai.toolCall.args`               | `gen_ai.tool.call.arguments`     | WARN: use OTel attr instead |
| `ai.toolCall.result`             | `gen_ai.tool.call.result`        | WARN: use OTel attr instead |

---

## Step 4: Cross-Prefix Conflict Check (Manual Weaver Validation)

**Status: PASS**

### Custom Prefixes in Use

| Prefix             | Source                  | Conflict? |
| ------------------ | ----------------------- | --------- |
| `knowledge_hook.*` | custom-conventions.yaml | None      |
| `policy_pack.*`    | custom-conventions.yaml | None      |
| `rdf.*`            | custom-conventions.yaml | None      |
| `effect.*`         | custom-conventions.yaml | None      |
| `crypto.*`         | custom-conventions.yaml | None      |
| `transaction.*`    | custom-conventions.yaml | None      |
| `sidecar.*`        | custom-conventions.yaml | None      |
| `mcp.tool.*`       | MCP instrumentation     | None      |
| `mcp.server.*`     | MCP instrumentation     | None      |
| `ai.*`             | AI SDK internal         | None      |

### Against OTel Reserved Prefixes

The following prefixes are reserved in the OTel semantic conventions: `telemetry.*`, `aws.*`, `azure.*`, `gcp.*`, `system.*`, `process.*`, `container.*`, `host.*`, `os.*`, `service.*`, `web.*`, `http.*`, `rpc.*`, `db.*`, `messaging.*`, `faas.*`, `deployment.*`, `k8s.*`, `azure.*`, `consumer.*`, `producer.*`, `gen_ai.*`, `disk.*`, `network.*`, `peer.*`, `server.*`, `client.*`, `url.*`, `user_agent.*`, `browser.*`, `thread.*`, `code.*`, `exception.*`, `log.*`, `otel.*`, `vcs.*`, `cloudevents.*`, `feature_flag.*`, `android.*`, `ios.*`.

**Finding:** None of the UNRDF custom prefixes (`mcp.*`, `knowledge_hook.*`, `rdf.*`, `effect.*`, `crypto.*`, `policy_pack.*`, `sidecar.*`, `transaction.*`) conflict with any OTel reserved prefix.

### `gen_ai.*` Namespace

The `gen_ai.*` namespace is reserved for GenAI operations per the latest spec. The UNRDF custom conventions do not define any `gen_ai.*` attributes. The AI SDK's `experimental_telemetry` emits into this namespace directly (not controllable by UNRDF code), which is correct.

---

## Step 5: OTEL Collector Config Validation

**File:** `/Users/sac/unrdf/otel/otel-collector-config.yaml`
**Status: PASS**

### Receivers

| Receiver     | Protocol    | Endpoint        | Status |
| ------------ | ----------- | --------------- | ------ |
| `otlp`       | gRPC        | `0.0.0.0:4317`  | Valid  |
| `otlp`       | HTTP        | `0.0.0.0:4318`  | Valid  |
| `jaeger`     | gRPC        | `0.0.0.0:14250` | Valid  |
| `jaeger`     | thrift_http | `0.0.0.0:14268` | Valid  |
| `prometheus` | scrape      | self + tempo    | Valid  |

### Processors

| Processor        | Config                                | Status |
| ---------------- | ------------------------------------- | ------ |
| `batch`          | timeout=5s, size=1024, max=2048       | Valid  |
| `transform/logs` | `traceStatements` with `context: log` | Valid  |
| `resource`       | service.name, version, environment    | Valid  |

**Note on `transform/logs`:** Uses `traceStatements` (not `statements`) with `context: log`, which is correct for the OTel Collector transform processor when correlating log records with traces. The statements copy `trace_id` and `span_id` to `loki.trace_id` and `loki.span_id` for Loki log correlation. This is the recommended pattern for Tempo+Loki trace-log correlation.

### Exporters

| Exporter     | Endpoint                            | Status |
| ------------ | ----------------------------------- | ------ |
| `otlp/tempo` | `tempo:4319` (TLS insecure)         | Valid  |
| `prometheus` | `0.0.0.0:8889`                      | Valid  |
| `loki`       | `http://loki:3100/loki/api/v1/push` | Valid  |
| `debug`      | stdout                              | Valid  |

### Pipeline Wiring

| Pipeline  | Receivers            | Processors                | Exporters             | Valid? |
| --------- | -------------------- | ------------------------- | --------------------- | ------ |
| `traces`  | `[otlp, jaeger]`     | `[batch, resource]`       | `[otlp/tempo, debug]` | Yes    |
| `metrics` | `[otlp, prometheus]` | `[batch]`                 | `[prometheus]`        | Yes    |
| `logs`    | `[otlp]`             | `[batch, transform/logs]` | `[loki, debug]`       | Yes    |

**Note:** The `resource` processor is only applied to traces, not metrics or logs. This means metrics and logs exported to Prometheus/Loki will not have the `service.name=unrdf-daemon` label injected by the collector. This is fine if the SDK already sets these (which it does via `otel-sdk.mjs`), but could be a gap for external systems that don't set resource attributes.

### Extensions

- `health_check` on `0.0.0.0:13133` -- valid

---

## Step 6: Loki + Ruler Config Validation

### Loki Config

**File:** `/Users/sac/unrdf/otel/loki.yaml`
**Status: PASS**

| Section          | Key Fields                                                                                          | Status |
| ---------------- | --------------------------------------------------------------------------------------------------- | ------ |
| `server`         | `http_listen_port: 3100`, `grpc_listen_port: 9096`                                                  | Valid  |
| `common.storage` | TSDB with filesystem backend                                                                        | Valid  |
| `schema_config`  | `store: tsdb`, `schema: v13`, `period: 24h`                                                         | Valid  |
| `ruler`          | `enable_alertmanager_v2: true`, `alertmanager_url: http://alertmanager:9093`, `storage.type: local` | Valid  |

### Alert Rules

**File:** `/Users/sac/unrdf/otel/loki-rules/rate-limit-errors.yaml`
**Status: PASS**

#### Rule 1: `HighErrorRate`

```logql
sum(rate({container=~".*unrdf.*"} |= "error" [5m])) by (container)
/ sum(rate({container=~".*unrdf.*"} [5m])) by (container)
> 0.1
```

- **LogQL syntax:** Valid. Uses log pipeline (`|= "error"`), rate aggregation, and division for ratio calculation.
- **Logic:** Error rate > 10% for 5 minutes, fires after 5m sustained.
- **Severity:** `warning` -- appropriate.
- **Potential issue:** Division by zero if the container produces no logs. Loki returns `0` for empty ranges, so this will not error but the result will be `NaN`. Grafana handles this gracefully (displays "N/A"), but it means the alert will not fire for a container that is completely silent. This is actually correct behavior -- the `ServiceDown` alert covers that case.

#### Rule 2: `ServiceDown`

```logql
sum(rate({container=~".*unrdf.*"} [10m])) by (container) == 0
```

- **LogQL syntax:** Valid. Uses `rate()` with 10m window and equality check.
- **Logic:** No logs from container in 10 minutes.
- **Severity:** `critical` -- appropriate.
- **For duration:** 10m -- good, avoids false positives from brief pauses.

---

## Step 7: Dashboard JSON Validation

### service-graph.json

**File:** `/Users/sac/unrdf/otel/grafana-provisioning/dashboards/service-graph.json`
**Status: PASS**

- **JSON validity:** Valid
- **Datasource references:** All use `${DS_PROMETHEUS}` variable with `type: "prometheus"` -- correct
- **Metric names:** Uses Tempo's `metrics_generator` output:
  - `traces_service_graph_request_total` -- correct for Tempo service graph
  - `traces_service_graph_request_failed_total` -- correct for Tempo service graph
  - `traces_service_graph_request_total_bucket` -- correct for histogram-based latency quantiles
- **Panel types:** `nodeGraph`, `stat`, `timeseries` -- all valid
- **Template variables:** `DS_PROMETHEUS` (datasource), `service` (label_values from `traces_service_graph_request_total`) -- valid

### trace-to-metrics.json

**File:** `/Users/sac/unrdf/otel/grafana-provisioning/dashboards/trace-to-metrics.json`
**Status: PASS**

- **JSON validity:** Valid
- **Datasource references:** All use `${DS_PROMETHEUS}` -- correct
- **Metric names:** Uses Tempo's `spanmetrics` connector output:
  - `traces_spanmetrics_latency_count` -- correct
  - `traces_spanmetrics_latency_bucket` -- correct for histogram quantiles
  - `traces_spanmetrics_latency_sum` -- referenced implicitly via `histogram_quantile`
- **Template variables:** `DS_PROMETHEUS`, `service` (from `traces_spanmetrics_latency_count`), `operation` (from `span_name` label) -- valid
- **Panel types:** `stat`, `timeseries` -- all valid

### unrdf-otel.json

**File:** `/Users/sac/unrdf/otel/grafana-provisioning/dashboards/unrdf-otel.json`
**Status: PASS**

- **JSON validity:** Valid
- **Datasource references:** All use `${DS_PROMETHEUS}` -- correct
- **Metric names:** Uses OTel Collector internal metrics:
  - `otelcol_exporter_sent_spans` -- correct
  - `otelcol_exporter_send_failed_spans` -- correct
  - `otelcol_exporter_queue_size` -- correct
  - `otelcol_processor_batch_batch_send_size_bytes` -- correct
- **MCP-specific panels:** Filters on `span_name=~"mcp\\.tool\\..*"` -- correct regex escaping
- **Template variables:** `DS_PROMETHEUS` -- valid
- **Panel types:** `timeseries`, `stat`, `gauge`, `bargauge` -- all valid

### dashboards.yml

**File:** `/Users/sac/unrdf/otel/grafana-provisioning/dashboards/dashboards.yml`
**Status: PASS**

Valid Grafana provisioning config pointing to `/etc/grafana/provisioning/dashboards`.

---

## Step 8: OTEL SDK Initialization Validation

**File:** `/Users/sac/unrdf/packages/daemon/src/integrations/otel-sdk.mjs`
**Status: WARN (2 issues)**

### Issue 8a: Deprecated `SemanticResourceAttributes` Import

```javascript
import { SemanticResourceAttributes } from '@opentelemetry/semantic-conventions';
```

**Severity:** Medium

The `SemanticResourceAttributes` enum from `@opentelemetry/semantic-conventions` is deprecated in modern OTel SDK versions. The recommended approach is to use the `ATTR_*` constants from `@opentelemetry/semantic-conventions` (e.g., `ATTR_SERVICE_NAME`) or use string literals directly (`'service.name'`).

**Remediation:** Replace with:

```javascript
import {
  ATTR_SERVICE_NAME,
  ATTR_SERVICE_VERSION,
  ATTR_DEPLOYMENT_ENVIRONMENT,
} from '@opentelemetry/semantic-conventions';
```

### Issue 8b: Duplicate Exporter Configuration in NodeSDK

```javascript
sdk = new NodeSDK({
  resource,
  traceExporter, // <-- passed here
  spanProcessor, // <-- AND passed here
  autoDetectResources: false,
});
```

**Severity:** Medium

Both `traceExporter` and `spanProcessor` are passed to `NodeSDK`. The `spanProcessor` wraps the same `traceExporter` (via `new BatchSpanProcessor(traceExporter, ...)`). When `NodeSDK` receives both, it may create a second `BatchSpanProcessor` wrapping the same exporter internally, resulting in **double processing** -- each span being exported twice, effectively doubling export volume.

The `traceExporter` parameter in `NodeSDK` is a convenience that auto-wraps in a `BatchSpanProcessor`. When you also provide `spanProcessor`, you should not provide `traceExporter`.

**Remediation:** Remove the `traceExporter` parameter, keeping only `spanProcessor`:

```javascript
sdk = new NodeSDK({
  resource,
  spanProcessor,
  autoDetectResources: false,
});
```

---

## Step 9: MCP Tool Instrumentation Validation

**File:** `/Users/sac/unrdf/packages/daemon/src/mcp/otel-instrumentation.mjs`
**Status: PASS**

### Attributes Emitted

| Attribute              | Type    | Notes                           |
| ---------------------- | ------- | ------------------------------- |
| `mcp.tool.name`        | string  | Tool name (e.g., `graph_query`) |
| `mcp.tool.args`        | string  | JSON-serialized arguments       |
| `mcp.server.name`      | string  | Hardcoded `unrdf-daemon-mcp`    |
| `mcp.tool.success`     | boolean | Set to `true` on success        |
| `mcp.tool.result_size` | number  | JSON-serialized result length   |

### Span Naming

Spans are named `mcp.tool.<toolName>` (e.g., `mcp.tool.graph_query`). This follows the OTel convention for hierarchical span names with dot-separated namespace.

### Wiring Coverage

39 MCP tools in `index.mjs` are wrapped with `withMcpSpan`. Verified via grep: every tool registration uses `return withMcpSpan('<tool_name>', fn)(args)`.

### Gap: Error Attributes

The `withMcpSpan` wrapper only sets `mcp.tool.success: true`. If the handler throws an error, the `withSpan` utility in `otel-tracer.mjs` catches the error and records it on the span via `span.recordException(error)`, but the `mcp.tool.success` attribute is never set to `false` because the success-setting code runs after the handler returns successfully.

**Remediation:** Consider adding a `span.setAttributes({ 'mcp.tool.success': false })` in the catch block of `withMcpSpan`, or rely on the OTel `SpanStatusCode.ERROR` (set by `withSpan`) which Grafana can filter on.

---

## Step 10: OTEL Context Propagation Validation

**File:** `/Users/sac/unrdf/packages/daemon/src/integrations/otel-context.mjs`
**Status: PASS**

### W3C Traceparent Parsing

- Validates version `00` (only supported version)
- Validates 32-char hex trace ID (not all zeros)
- Validates 16-char hex span ID (not all zeros)
- Validates 2-char hex trace flags
- Returns structured context object

### W3C Traceparent Formatting

- Produces correct `00-{traceId}-{spanId}-{flags}` format
- Defaults flags to `01` (sampled)

### Context Injection/Extraction

- Uses OTel's `propagation.inject()` and `propagation.extract()` -- correct approach
- Creates carrier objects for propagation -- follows OTel API

### Log Enrichment

- `enrichLogWithTraceContext()` adds `trace_id`, `span_id`, `trace_flags` to log entries
- These match the attribute names used in the collector's `transform/logs` processor (`trace_id`, `span_id`)

### Unused gRPC Metadata Constants

`GRPC_TRACE_METADATA` constants (`x-trace-id`, `x-span-id`, etc.) are defined but not used anywhere in the codebase. These would be for gRPC metadata propagation, which is a valid pattern but not yet wired up.

---

## Remediation Priority

| Priority | Issue                                              | File                       | Fix                                                            |
| -------- | -------------------------------------------------- | -------------------------- | -------------------------------------------------------------- |
| P1       | Duplicate exporter in NodeSDK                      | `otel-sdk.mjs:55-60`       | Remove `traceExporter` param, keep `spanProcessor`             |
| P2       | Deprecated `SemanticResourceAttributes`            | `otel-sdk.mjs:11`          | Switch to `ATTR_*` constants                                   |
| P2       | Missing `mcp.tool.success: false` on error         | `otel-instrumentation.mjs` | Add error attribute in catch path                              |
| P3       | Enable `gen_ai_latest_experimental`                | Environment                | Set `OTEL_SEMCONV_STABILITY_OPT_IN=gen_ai_latest_experimental` |
| P3       | Dashboard metric name alignment after GenAI opt-in | Dashboard JSONs            | Update filters from `gen_ai.system` to `gen_ai.provider.name`  |
| P4       | Unused `GRPC_TRACE_METADATA` constants             | `otel-context.mjs`         | Wire up or remove dead code                                    |
