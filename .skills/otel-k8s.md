# Skill: OTEL & Kubernetes Deployment

Deploy the full UNRDF observability stack via Helm on a local Kind cluster.

## Quick Deploy

```bash
make k8s-create && make k8s-up
```

## Stack Components

| Service | Port | Purpose |
|---------|------|---------|
| Prometheus | 9091 | Metrics |
| Grafana | 3001 | Dashboards |
| Tempo | 13200 | Traces |
| Loki | 13100 | Logs |
| Pyroscope | 14040 | Profiling |

## Endpoints

- OTLP gRPC: `localhost:14317`
- OTLP HTTP: `localhost:14318`

## Key Files

- Helm chart: `k8s/helm/unrdf-observability/`
- Kind config: `k8s/kind-config.yaml`
- Deployment guide: `playground/OTEL-K8S-DEPLOYMENT-GUIDE.md`
