# Explanation: Architecture Overview

How the UNRDF observability stack is structured and why each component exists.

---

## The data pipeline

The stack implements the three pillars of observability (metrics, logs, traces) plus continuous profiling, connected through an OpenTelemetry Collector:

```
                    ┌─────────────────────┐
                    │   Your Application  │
                    └──────────┬──────────┘
                               │ OTLP (gRPC/HTTP)
                               ▼
                    ┌─────────────────────┐
                    │   OTel Collector    │  ← central telemetry pipeline
                    └──┬──────┬──────┬────┘
                       │      │      │
          traces       │      │      │  logs        metrics
                       ▼      │      ▼               ▼
                ┌──────────┐ │ ┌──────────┐   ┌──────────────┐
                │  Tempo   │ │ │   Loki   │   │  Prometheus  │
                └────┬─────┘ │ └────┬─────┘   └──────┬───────┘
                     │       │      │                │
              S3 backend     │   Loki rules     Alert rules
                     │       │      │                │
                     ▼       │      ▼                ▼
                ┌──────────┐ │ ┌──────────┐   ┌──────────────┐
                │  MinIO   │ │ │  Alert-  │   │ Alert-       │
                │  (S3)    │ │ │  manager │   │ manager      │
                └──────────┘ │ └──────────┘   └──────────────┘
                             │
                     host logs
                             │
                             ▼
                     ┌──────────┐
                     │ Promtail │
                     └──────────┘

  Separate: ┌──────────┐    ┌────────────┐    ┌────────────┐
            │   Grafana │◄───│  Pyroscope │    │    Node    │
            │ (unifies) │    │ (profiling)│    │  Exporter  │
            └──────────┘    └────────────┘    └────────────┘
```

## Why OTel Collector?

The OTel Collector acts as a **telemetry gateway**. Rather than having every application connect directly to Tempo, Loki, and Prometheus, all data flows through one pipeline. This gives us:

- **Protocol translation**: Accept OTLP from apps, export to Prometheus format, Loki push API, etc.
- **Processing**: Batch requests, filter spans, enrich attributes, add `log_statements` processor
- **Buffering**: Absorb burst traffic without overwhelming backends
- **Self-instrumentation**: The collector exposes its own metrics on `:8889`, which Prometheus scrapes

## Why MinIO for Tempo storage?

Tempo requires an object store for distributed trace storage. In production this would be AWS S3, GCS, or Azure Blob. MinIO provides an S3-compatible API for local development so Tempo works the same way in kind as it would in production.

The MinIO setup Job (`job-minio-setup.yaml`) creates a `tempo-traces` bucket on first install using `mc` (MinIO Client).

## Why Promtail?

Promtail is the log agent for Loki. In a Docker Compose environment, it reads from `/var/lib/docker/containers`. In Kubernetes, it uses the `kubernetes_sd_configs` with `role: pod` to discover pod logs automatically.

The Promtail config uses JSON pipeline stages to extract `trace_id` and `level` fields from log lines, enabling trace-log correlation in Grafana.

## Why Node Exporter?

Node Exporter exposes host-level metrics (CPU, memory, disk, network) as Prometheus scrape targets. In kind, it uses `hostNetwork: true` and `hostPath` mounts on `/proc` and `/sys` to access the kind node's (Docker container's) metrics.

## Why Pyroscope?

Pyroscope provides continuous profiling — CPU, memory, and goroutine profiles collected automatically over time. Unlike one-off profiling, continuous profiling lets you correlate performance changes with deployments, load changes, or code changes. Grafana acquired Pyroscope and integrated it natively.

## Component responsibilities

| Component | Role | Data stored |
|-----------|------|-------------|
| **OTel Collector** | Telemetry pipeline | Ephemeral (buffer only) |
| **Prometheus** | Metrics storage and querying | Time-series (TSDB, PVC) |
| **Tempo** | Distributed trace storage | Traces (object store via MinIO) |
| **Loki** | Log aggregation and querying | Log chunks (TSDB, PVC) |
| **Grafana** | Visualization and correlation | Dashboards (PVC) |
| **Alertmanager** | Alert routing and deduplication | State (PVC) |
| **Promtail** | Log collection from pods | Ephemeral |
| **Node Exporter** | Host metrics | Ephemeral |
| **MinIO** | S3-compatible object storage | Objects (PVC) |
| **Pyroscope** | Continuous profiling | Profiles (PVC) |
| **HotROD** | Example app (generates traces) | None |
| **UNRDF App** | The application being monitored | None (disabled by default) |
