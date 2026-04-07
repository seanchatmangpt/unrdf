# Reference: ConfigMaps and Secrets

All ConfigMaps and Secrets created by the chart, their contents, and which services consume them.

---

## ConfigMaps

### configmap-prometheus.yaml

**Name**: `unrdf-observability-prometheus`
**Mounted by**: `deployment-prometheus.yaml`
**Data keys**:

| Key | Description |
|-----|-------------|
| `prometheus.yml` | Prometheus configuration with scrape targets, alerting, remote_write |
| `alert-rules.yml` | Prometheus alert rules |

**Scrape targets configured**:
- `unrdf-observability-otel-collector:8889` (OTel Collector own metrics)
- `unrdf-observability-prometheus:9090` (self-scrape)
- `unrdf-observability-node-exporter:9100`
- `unrdf-observability-tempo:3200` (Tempo metrics)
- `unrdf-observability-minio:9000` (MinIO metrics)
- `unrdf-observability-alertmanager:9093`
- `unrdf-observability-grafana:3000`
- `unrdf-observability-loki:3100`
- `unrdf-observability-pyroscope:4040`
- `unrdf-daemon:9464` (UNRDF daemon — expected down when app is disabled)

### configmap-otel-collector.yaml

**Name**: `unrdf-observability-otel-collector`
**Mounted by**: `deployment-otel-collector.yaml` (subPath mount)
**Data keys**:

| Key | Description |
|-----|-------------|
| `otel-collector-config.yaml` | Full OTel Collector pipeline config |

**Pipeline**:
- **Receivers**: OTLP (gRPC :4317, HTTP :4318), Jaeger (14268), Prometheus (:8889)
- **Processors**: batch, memory_limiter, log_statements (attribute extraction)
- **Exporters**: OTLP→Tempo, Prometheus, Loki

### configmap-tempo.yaml

**Name**: `unrdf-observability-tempo`
**Mounted by**: `deployment-tempo.yaml`
**Data keys**:

| Key | Description |
|-----|-------------|
| `tempo.yaml` | Tempo configuration |

**Key config**: S3 backend pointing to MinIO (`unrdf-observability-minio:9000`), bucket `tempo-traces`. Metrics generator remote_write to Prometheus.

### configmap-loki.yaml

**Name**: `unrdf-observability-loki`
**Mounted by**: `deployment-loki.yaml`
**Data keys**:

| Key | Description |
|-----|-------------|
| `loki.yaml` | Loki configuration |

**Key config**: TSDB storage engine, filesystem object store, ruler with alertmanager URL.

### configmap-promtail.yaml

**Name**: `unrdf-observability-promtail`
**Mounted by**: `deployment-promtail.yaml` (subPath mount)
**Data keys**:

| Key | Description |
|-----|-------------|
| `promtail.yaml` | Promtail configuration |

**Key config**: `kubernetes_sd_configs` with `role: pod`, filters by `app.kubernetes.io/instance` label. JSON pipeline for `trace_id` and `level` extraction.

### configmap-alertmanager.yaml

**Name**: `unrdf-observability-alertmanager`
**Mounted by**: `deployment-alertmanager.yaml` (subPath mount as `alertmanager.yml`)
**Data keys**:

| Key | Description |
|-----|-------------|
| `alertmanager.yml` | Alertmanager routing and receiver config |

**Default**: `null` receiver (dev mode). Route and inhibit rules defined but no external notifications.

### configmap-grafana-datasources.yaml

**Name**: `unrdf-observability-grafana-datasources`
**Mounted by**: `deployment-grafana.yaml` at `/etc/grafana/provisioning/datasources`
**Data keys**:

| Key | Description |
|-----|-------------|
| `datasources.yaml` | Grafana datasource provisioning |

**Datasources**: Prometheus, Tempo, Loki, Pyroscope — all using K8s internal service names.

### configmap-grafana-dashboards.yaml

**Name**: `unrdf-observability-grafana-dashboards`
**Mounted by**: `deployment-grafana.yaml` at `/etc/grafana/provisioning/dashboards`
**Data keys**:

| Key | Description |
|-----|-------------|
| `dashboards.yaml` | Dashboard provider config (file-based) |

### configmap-loki-rules.yaml

**Name**: `unrdf-observability-loki-rules`
**Mounted by**: `deployment-loki.yaml`
**Data keys**:

| Key | Description |
|-----|-------------|
| `loki-rules.yaml` | Loki alerting rules |

**Default rule**: `rate-limit-errors` — alerts when error log rate exceeds threshold.

---

## Secrets

### secret-minio.yaml

**Name**: `unrdf-observability-minio`
**Type**: Opaque
**Keys**:

| Key | Source |
|-----|--------|
| `MINIO_ROOT_USER` | `values.yaml: minio.rootUser` |
| `MINIO_ROOT_PASSWORD` | `values.yaml: minio.rootPassword` |

**Consumed by**: `deployment-minio.yaml`, `job-minio-setup.yaml`

### secret-grafana.yaml

**Name**: `unrdf-observability-grafana`
**Type**: Opaque
**Keys**:

| Key | Source |
|-----|--------|
| `GF_SECURITY_ADMIN_PASSWORD` | `values.yaml: grafana.adminPassword` |

**Consumed by**: `deployment-grafana.yaml`

---

## PVCs

All PVCs use `storageClassName: standard` (kind default).

| PVC Name | Size | Mounted by |
|----------|------|------------|
| `unrdf-observability-prometheus-data` | 10Gi | deployment-prometheus |
| `unrdf-observability-grafana-data` | 5Gi | deployment-grafana |
| `unrdf-observability-tempo-data` | 10Gi | deployment-tempo |
| `unrdf-observability-minio-data` | 10Gi | deployment-minio |
| `unrdf-observability-loki-data` | 10Gi | deployment-loki |
| `unrdf-observability-pyroscope-data` | 5Gi | deployment-pyroscope |
| `unrdf-observability-alertmanager-data` | 1Gi | deployment-alertmanager |

---

## RBAC

### serviceaccount-promtail.yaml

| Resource | Name | Purpose |
|----------|------|---------|
| ServiceAccount | `unrdf-observability-promtail` | Identity for Promtail pods |
| ClusterRole | `unrdf-observability-promtail` | Read access to pods, nodes, services, endpoints, ingresses |
| ClusterRoleBinding | `unrdf-observability-promtail` | Bind ClusterRole to ServiceAccount |

**Permissions**: `get`, `list`, `watch` on `nodes`, `nodes/proxy`, `services`, `endpoints`, `pods`, `ingresses`.
