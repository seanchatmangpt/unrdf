# Tutorial: Exploring Grafana Dashboards

**What you'll learn**: How to use the Grafana instance to explore metrics, traces, and logs.

**Prerequisites**: Completed [01-first-cluster.md](01-first-cluster.md)

---

## Access Grafana

Open http://localhost:3001 and log in with `admin` / `admin`.

## Configured datasources

The chart provisions four datasources automatically:

| Datasource | Purpose | Connection |
|-----------|---------|------------|
| **Prometheus** | Metrics | `unrdf-observability-prometheus:9090` |
| **Tempo** | Distributed traces | `unrdf-observability-tempo:3200` |
| **Loki** | Logs | `unrdf-observability-loki:3100` |
| **Pyroscope** | Continuous profiling | `unrdf-observability-pyroscope:4040` |

Navigate to **Configuration → Data Sources** to verify all four show green checkmarks.

## Explore traces

1. **Explore** → select **Tempo** from the dropdown
2. Click **Search Traces** (or run a query with `{}`
3. Click on any trace to see the span waterfall
4. Use **Trace to Logs** to jump from a span to its corresponding log lines in Loki
5. Use **Trace to Metrics** to see related metrics from Prometheus

## Explore metrics

1. **Explore** → select **Prometheus**
2. Try these queries:

```
# Rate of HTTP requests to HotROD
rate(http_requests_total{job="example-app"}[5m])

# UP status of all monitored services
up

# Node CPU usage (from node-exporter)
100 - (avg by (instance) (rate(node_cpu_seconds_total{mode="idle"}[5m])) * 100)

# Tempo ingest rate
rate(tempo_distributor_traces_received_total[5m])

# OTel Collector own metrics
otelcol_receiver_accepted_spans
```

## Explore logs

1. **Explore** → select **Loki**
2. Try these queries:

```
# All logs from the example app
{app_kubernetes_io_component="example-app"}

# Error-level logs across all services
{namespace="unrdf-observability"} |= "error" | json

# Logs with trace ID (correlate with Tempo)
{app_kubernetes_io_component="example-app"} | json | trace_id != ""
```

## Cross-linking

Grafana supports linking between datasources:
- From a **Tempo trace**, click **Logs** to see logs for that trace
- From a **Prometheus metric**, click **Explore** in Tempo to find related traces
- From **Loki logs**, click the `traceId` field to jump to the trace

This correlation works because the OTel Collector injects trace IDs into logs automatically.

## Alerting

Prometheus is configured with alert rules (defined in the `configmap-prometheus.yaml` ConfigMap). Alerts route to the Alertmanager at http://localhost:9093.

The default Alertmanager config uses a `null` receiver (alerts are discarded). To route alerts to Slack or PagerDuty, update the Alertmanager ConfigMap — see [how-to/configure-alerting.md](../how-to/configure-alerting.md).

---

**Back to**: [01-first-cluster.md](01-first-cluster.md)
