# How to Enable OTel Tracing

**Goal**: Enable OpenTelemetry distributed tracing for the daemon and its 36 MCP tools so spans are exported to an OTLP collector.

---

## Set the required environment variables

```bash
# Enable OTEL (default: true)
export OTEL_ENABLED=true

# OTLP gRPC endpoint (default: localhost:4317)
export OTEL_EXPORTER_OTLP_ENDPOINT=localhost:4317

# Service identity
export OTEL_SERVICE_NAME=unrdf-daemon
export OTEL_SERVICE_VERSION=26.4.4

# Then start the daemon
node src/daemon.mjs
```

When `OTEL_ENABLED=false`, all instrumentation is skipped and no SDK is initialized — safe for local development without a collector.

---

## Understand what gets traced

Every MCP tool call is wrapped with `withMcpSpan()` from `src/mcp/otel-instrumentation.mjs`. This creates a span named `mcp.tool.<toolName>` and sets the following attributes:

| Attribute              | Value                          | Set when        |
| ---------------------- | ------------------------------ | --------------- |
| `mcp.tool.name`        | Tool name (e.g. `graph_query`) | Always          |
| `mcp.tool.args`        | JSON-stringified arguments     | Always          |
| `mcp.server.name`      | `unrdf-daemon-mcp`             | Always          |
| `mcp.tool.success`     | `true` or `false`              | On completion   |
| `mcp.tool.result_size` | Byte length of JSON result     | On success only |

Example span for a SPARQL query:

```
Span: mcp.tool.graph_query
  mcp.tool.name       = "graph_query"
  mcp.tool.args       = "{\"query\":\"SELECT * WHERE { ?s ?p ?o }\"}"
  mcp.server.name     = "unrdf-daemon-mcp"
  mcp.tool.success    = true
  mcp.tool.result_size = 1842
```

---

## Run a local collector for development

Use the OpenTelemetry Collector with a simple config to print spans to stdout:

```yaml
# otel-collector-config.yaml
receivers:
  otlp:
    protocols:
      grpc:
        endpoint: 0.0.0.0:4317

exporters:
  logging:
    loglevel: debug

service:
  pipelines:
    traces:
      receivers: [otlp]
      exporters: [logging]
```

```bash
docker run --rm -p 4317:4317 \
  -v $(pwd)/otel-collector-config.yaml:/etc/otel-collector-config.yaml \
  otel/opentelemetry-collector:latest \
  --config /etc/otel-collector-config.yaml
```

---

## Configure sampling

For high-traffic deployments, reduce sampling to avoid overhead:

```bash
# Sample 10% of traces
export OTEL_TRACES_SAMPLER=traceidratio
export OTEL_TRACES_SAMPLER_ARG=0.1

# Sample only when parent span is sampled (recommended in production)
export OTEL_TRACES_SAMPLER=parentbased_traceidratio
export OTEL_TRACES_SAMPLER_ARG=1.0
```

---

## Tune batch export performance

Control how spans are batched before export:

```bash
# Maximum spans to queue before forced export (default: 1000)
export OTEL_BATCH_MAX_QUEUE_SIZE=10000

# Delay between scheduled exports in ms
export OTEL_BATCH_SCHEDULE_DELAY_MILLIS=500
```

---

## Verify tracing is active

Check that spans appear in your collector. For a quick sanity check, call any MCP tool and look for spans with `mcp.tool.name` set:

```bash
# From your OTEL backend or collector logs, look for:
# Span: mcp.tool.daemon_status
#   mcp.tool.name    = "daemon_status"
#   mcp.tool.success = true
```

If no spans appear:

1. Confirm `OTEL_ENABLED=true` (not `"false"` as a string)
2. Confirm the collector is reachable: `nc -z localhost 4317`
3. Check daemon startup logs for `[otel] SDK initialized`

---

## See also

- [Environment Variables Reference](../reference/environment-variables.md) — full list of OTEL env vars
- [MCP Tools Reference](../reference/mcp-tools.md) — all 36 instrumented tools
- [Daemon Architecture](../explanation/01-daemon-architecture.md) — OTel layer in the stack
