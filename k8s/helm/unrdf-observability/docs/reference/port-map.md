# Reference: Port Map

All service ports — kind node ports, container ports, and host access.

---

## Host access ports

These are the ports available on `localhost` after `make k8s-create`:

| Service | Host Port | URL | Description |
|---------|-----------|-----|-------------|
| UNRDF API | 3000 | http://localhost:3000 | UNRDF application (disabled by default) |
| Grafana | 3001 | http://localhost:3001 | Grafana dashboards |
| Prometheus | 9091 | http://localhost:9091 | Prometheus UI |
| OTLP gRPC | 4317 | `localhost:4317` | OTel Collector (gRPC) |
| OTLP HTTP | 4318 | `localhost:4318` | OTel Collector (HTTP) |
| Tempo API | 3200 | http://localhost:3200 | Tempo trace API |
| Pyroscope | 4040 | http://localhost:4040 | Pyroscope profiling |
| HotROD | 8082 | http://localhost:8082 | Example trace-generating app |
| Node Exporter | 9100 | http://localhost:9100 | Host metrics |
| Alertmanager | 9093 | http://localhost:9093 | Alertmanager UI |
| Loki | 3100 | http://localhost:3100 | Loki log API |
| MinIO S3 API | 9000 | http://localhost:9000 | MinIO object storage |
| MinIO Console | 9001 | http://localhost:9001 | MinIO web console |

## Kind node port mappings

Inside the kind cluster, services use different ports (container ports). The kind config maps host ports to container ports via `extraPortMappings`:

| Service | Host Port | Container Port |
|---------|-----------|----------------|
| UNRDF API | 3000 | 30000 |
| Grafana | 3001 | 30001 |
| Prometheus | 9091 | 30002 |
| OTLP gRPC | 4317 | 30437 |
| OTLP HTTP | 4318 | 30438 |
| Tempo API | 3200 | 30200 |
| Pyroscope | 4040 | 30040 |
| HotROD | 8082 | 30082 |
| Node Exporter | 9100 | 30100 |
| Alertmanager | 9093 | 30093 |
| Loki | 3100 | 30100 → 30110 |
| MinIO S3 API | 9000 | 30090 |
| MinIO Console | 9001 | 30091 |

## In-cluster service ports (K8s DNS)

Inside the cluster, use the K8s Service name and port:

| Service | K8s Service Name | Port(s) | Protocol |
|---------|-----------------|---------|----------|
| Prometheus | `unrdf-observability-prometheus` | 9090 | TCP |
| Grafana | `unrdf-observability-grafana` | 3000 | TCP |
| OTel Collector | `unrdf-observability-otel-collector` | 4317 (gRPC), 4318 (HTTP), 8888 (metrics), 13133 (health) | TCP |
| Tempo | `unrdf-observability-tempo` | 3200 (HTTP), 4319 (OTLP gRPC), 4320 (OTLP HTTP) | TCP |
| Loki | `unrdf-observability-loki` | 3100 (HTTP), 9096 (gRPC) | TCP |
| MinIO | `unrdf-observability-minio` | 9000 (API), 9001 (Console) | TCP |
| Alertmanager | `unrdf-observability-alertmanager` | 9093 | TCP |
| Node Exporter | `unrdf-observability-node-exporter` | 9100 | TCP |
| Promtail | `unrdf-observability-promtail` | 9080 | TCP |
| Pyroscope | `unrdf-observability-pyroscope` | 4040 | TCP |
| Example App | `unrdf-observability-example-app` | 8080 | TCP |
| UNRDF App | `unrdf-observability-unrdf` | 3000 (API), 9090 (metrics), 8080 (health) | TCP |

Full K8s DNS FQDN: `<service-name>.unrdf-observability.svc.cluster.local`

## Service types

| Service | Type | Exposed externally |
|---------|------|-------------------|
| prometheus | NodePort (30002) | Yes (localhost:9091) |
| grafana | NodePort (30001) | Yes (localhost:3001) |
| tempo | NodePort (30200) | Yes (localhost:3200) |
| loki | NodePort (30110) | Yes (localhost:3100) |
| alertmanager | NodePort (30093) | Yes (localhost:9093) |
| minio | NodePort (30090, 30091) | Yes (localhost:9000, 9001) |
| node-exporter | NodePort (30100) | Yes (localhost:9100) |
| example-app | NodePort (30082) | Yes (localhost:8082) |
| pyroscope | NodePort (30040) | Yes (localhost:4040) |
| unrdf | NodePort (30000) | Yes (localhost:3000) |
| otel-collector | ClusterIP | No (in-cluster only) |
| promtail | ClusterIP | No (in-cluster only) |
