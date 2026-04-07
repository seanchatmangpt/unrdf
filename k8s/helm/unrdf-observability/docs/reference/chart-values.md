# Reference: Helm Chart Values

Complete reference for all configurable values in `values.yaml`.

---

## Global

| Key | Type | Default | Description |
|-----|------|---------|-------------|
| `namespace` | string | `unrdf-observability` | Kubernetes namespace for all resources |

## Component: unrdf

| Key | Type | Default | Description |
|-----|------|---------|-------------|
| `unrdf.enabled` | bool | `false` | Deploy the UNRDF application |
| `unrdf.imageRepo` | string | `unrdf` | Container image repository |
| `unrdf.imageTag` | string | `6.0.0-rc.1` | Container image tag |
| `unrdf.replicas` | int | `1` | Number of replicas |
| `unrdf.resources.requests.cpu` | string | `1` | CPU request |
| `unrdf.resources.requests.memory` | string | `2Gi` | Memory request |
| `unrdf.resources.limits.cpu` | string | `2` | CPU limit |
| `unrdf.resources.limits.memory` | string | `4Gi` | Memory limit |
| `unrdf.env` | list | `[]` | Extra environment variables |

## Component: prometheus

| Key | Type | Default | Description |
|-----|------|---------|-------------|
| `prometheus.imageRepo` | string | `prom/prometheus` | Container image |
| `prometheus.imageTag` | string | `v2.48.0` | Image tag |
| `prometheus.replicas` | int | `1` | Replicas |
| `prometheus.retention` | string | `30d` | TSDB retention period |
| `prometheus.resources.requests.cpu` | string | `250m` | CPU request |
| `prometheus.resources.requests.memory` | string | `512Mi` | Memory request |
| `prometheus.resources.limits.cpu` | string | `1` | CPU limit |
| `prometheus.resources.limits.memory` | string | `1Gi` | Memory limit |

## Component: grafana

| Key | Type | Default | Description |
|-----|------|---------|-------------|
| `grafana.imageRepo` | string | `grafana/grafana` | Container image |
| `grafana.imageTag` | string | `10.2.2` | Image tag |
| `grafana.replicas` | int | `1` | Replicas |
| `grafana.adminPassword` | string | `admin` | Admin password |
| `grafana.resources.requests.cpu` | string | `100m` | CPU request |
| `grafana.resources.requests.memory` | string | `256Mi` | Memory request |
| `grafana.resources.limits.cpu` | string | `500m` | CPU limit |
| `grafana.resources.limits.memory` | string | `512Mi` | Memory limit |

## Component: otelCollector

| Key | Type | Default | Description |
|-----|------|---------|-------------|
| `otelCollector.imageRepo` | string | `otel/opentelemetry-collector-contrib` | Container image |
| `otelCollector.imageTag` | string | `0.91.0` | Image tag |
| `otelCollector.replicas` | int | `1` | Replicas |
| `otelCollector.resources.requests.cpu` | string | `100m` | CPU request |
| `otelCollector.resources.requests.memory` | string | `256Mi` | Memory request |
| `otelCollector.resources.limits.cpu` | string | `1` | CPU limit |
| `otelCollector.resources.limits.memory` | string | `1Gi` | Memory limit |

## Component: alertmanager

| Key | Type | Default | Description |
|-----|------|---------|-------------|
| `alertmanager.imageRepo` | string | `prom/alertmanager` | Container image |
| `alertmanager.imageTag` | string | `v0.26.0` | Image tag |
| `alertmanager.replicas` | int | `1` | Replicas |
| `alertmanager.resources.requests.cpu` | string | `50m` | CPU request |
| `alertmanager.resources.requests.memory` | string | `128Mi` | Memory request |
| `alertmanager.resources.limits.cpu` | string | `200m` | CPU limit |
| `alertmanager.resources.limits.memory` | string | `256Mi` | Memory limit |

## Component: nodeExporter

| Key | Type | Default | Description |
|-----|------|---------|-------------|
| `nodeExporter.imageRepo` | string | `prom/node-exporter` | Container image |
| `nodeExporter.imageTag` | string | `v1.7.0` | Image tag |
| `nodeExporter.resources.requests.cpu` | string | `50m` | CPU request |
| `nodeExporter.resources.requests.memory` | string | `64Mi` | Memory request |
| `nodeExporter.resources.limits.cpu` | string | `200m` | CPU limit |
| `nodeExporter.resources.limits.memory` | string | `128Mi` | Memory limit |

## Component: tempo

| Key | Type | Default | Description |
|-----|------|---------|-------------|
| `tempo.imageRepo` | string | `grafana/tempo` | Container image |
| `tempo.imageTag` | string | `2.7.1` | Image tag |
| `tempo.replicas` | int | `1` | Replicas |
| `tempo.resources.requests.cpu` | string | `100m` | CPU request |
| `tempo.resources.requests.memory` | string | `256Mi` | Memory request |
| `tempo.resources.limits.cpu` | string | `1` | CPU limit |
| `tempo.resources.limits.memory` | string | `2Gi` | Memory limit |

## Component: minio

| Key | Type | Default | Description |
|-----|------|---------|-------------|
| `minio.imageRepo` | string | `minio/minio` | Container image |
| `minio.imageTag` | string | `RELEASE.2024-11-07T00-52-20Z` | Image tag |
| `minio.replicas` | int | `1` | Replicas |
| `minio.rootUser` | string | `minioadmin` | Root username |
| `minio.rootPassword` | string | `minioadmin` | Root password |
| `minio.resources.requests.cpu` | string | `100m` | CPU request |
| `minio.resources.requests.memory` | string | `256Mi` | Memory request |
| `minio.resources.limits.cpu` | string | `1` | CPU limit |
| `minio.resources.limits.memory` | string | `1Gi` | Memory limit |

## Component: exampleApp

| Key | Type | Default | Description |
|-----|------|---------|-------------|
| `exampleApp.imageRepo` | string | `jaegertracing/example-hotrod` | Container image |
| `exampleApp.imageTag` | string | `1.62.0` | Image tag |
| `exampleApp.replicas` | int | `1` | Replicas |
| `exampleApp.resources.requests.cpu` | string | `50m` | CPU request |
| `exampleApp.resources.requests.memory` | string | `64Mi` | Memory request |
| `exampleApp.resources.limits.cpu` | string | `200m` | CPU limit |
| `exampleApp.resources.limits.memory` | string | `256Mi` | Memory limit |

## Component: pyroscope

| Key | Type | Default | Description |
|-----|------|---------|-------------|
| `pyroscope.imageRepo` | string | `grafana/pyroscope` | Container image |
| `pyroscope.imageTag` | string | `1.10.1` | Image tag |
| `pyroscope.replicas` | int | `1` | Replicas |
| `pyroscope.resources.requests.cpu` | string | `100m` | CPU request |
| `pyroscope.resources.requests.memory` | string | `256Mi` | Memory request |
| `pyroscope.resources.limits.cpu` | string | `1` | CPU limit |
| `pyroscope.resources.limits.memory` | string | `1Gi` | Memory limit |

## Component: loki

| Key | Type | Default | Description |
|-----|------|---------|-------------|
| `loki.imageRepo` | string | `grafana/loki` | Container image |
| `loki.imageTag` | string | `2.9.3` | Image tag |
| `loki.replicas` | int | `1` | Replicas |
| `loki.resources.requests.cpu` | string | `100m` | CPU request |
| `loki.resources.requests.memory` | string | `256Mi` | Memory request |
| `loki.resources.limits.cpu` | string | `1` | CPU limit |
| `loki.resources.limits.memory` | string | `1Gi` | Memory limit |

## Component: promtail

| Key | Type | Default | Description |
|-----|------|---------|-------------|
| `promtail.imageRepo` | string | `grafana/promtail` | Container image |
| `promtail.imageTag` | string | `2.9.3` | Image tag |
| `promtail.replicas` | int | `1` | Replicas |
| `promtail.resources.requests.cpu` | string | `50m` | CPU request |
| `promtail.resources.requests.memory` | string | `64Mi` | Memory request |
| `promtail.resources.limits.cpu` | string | `200m` | CPU limit |
| `promtail.resources.limits.memory` | string | `128Mi` | Memory limit |

## Storage

| Key | Type | Default | Description |
|-----|------|---------|-------------|
| `storage.prometheus` | string | `10Gi` | Prometheus TSDB |
| `storage.grafana` | string | `5Gi` | Grafana data |
| `storage.tempo` | string | `10Gi` | Tempo traces |
| `storage.minio` | string | `10Gi` | MinIO objects |
| `storage.loki` | string | `10Gi` | Loki chunks |
| `storage.pyroscope` | string | `5Gi` | Pyroscope profiles |
| `storage.alertmanager` | string | `1Gi` | Alertmanager state |
