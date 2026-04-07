# UNRDF Observability Stack — Kubernetes Documentation

Complete documentation for the `unrdf-observability` Helm chart deployed on `kind`.

## Quick start

```bash
make k8s-create    # create kind cluster
make k8s-up        # deploy all services
make k8s-ports     # print access URLs
```

## Diataxis structure

This documentation follows the [diataxis](https://diataxis.fr/) framework.

---

### Tutorials — Learning-oriented

Guided lessons for first-time users. Follow them in order.

| Tutorial | What you'll learn |
|----------|------------------|
| [01-first-cluster](tutorials/01-first-cluster.md) | Create a kind cluster, deploy the stack, verify services |
| [02-sending-traces](tutorials/02-sending-traces.md) | Send your application's traces to the stack |
| [03-dashboards](tutorials/03-dashboards.md) | Use Grafana to explore metrics, traces, and logs |

### How-to guides — Goal-oriented

Practical guides for specific tasks. Pick what you need.

| Guide | Task |
|-------|------|
| [custom-values](how-to/custom-values.md) | Override defaults with values files and --set flags |
| [manage-secrets](how-to/manage-secrets.md) | Rotate credentials, use external secret stores |
| [upgrade-rollback](how-to/upgrade-rollback.md) | Upgrade chart versions, rollback failed changes |
| [configure-alerting](how-to/configure-alerting.md) | Set up Prometheus alert rules and Alertmanager routes |
| [debug-pods](how-to/debug-pods.md) | Diagnose pod crashes, connectivity issues, failed scrapes |
| [scale-components](how-to/scale-components.md) | Adjust resource limits, replica counts, storage sizes |

### Reference — Information-oriented

Technical details for lookup.

| Reference | Contents |
|-----------|----------|
| [chart-values](reference/chart-values.md) | Every configurable value in values.yaml |
| [port-map](reference/port-map.md) | Host ports, cluster ports, in-cluster DNS names |
| [configmaps](reference/configmaps.md) | All ConfigMaps, Secrets, PVCs, and RBAC resources |
| [security-hardening](reference/security-hardening.md) | Current posture, gaps, hardening checklist |
| [makefile-targets](reference/makefile-targets.md) | All k8s-* Make targets |

### Explanation — Understanding-oriented

Conceptual background for why things work the way they do.

| Explanation | Topic |
|-------------|-------|
| [architecture](explanation/architecture.md) | Data pipeline, component responsibilities, why each exists |
| [service-discovery](explanation/service-discovery.md) | Docker DNS to K8s DNS, Helm tpl(), template escaping |
| [design-decisions](explanation/design-decisions.md) | Why Helm, kind, NodePort, PVC, MinIO, and other choices |

---

## Stack components

| Component | Purpose | Image |
|-----------|---------|-------|
| Prometheus | Metrics storage | `prom/prometheus:v2.48.0` |
| Grafana | Visualization | `grafana/grafana:10.2.2` |
| Tempo | Trace storage | `grafana/tempo:2.7.1` |
| Loki | Log aggregation | `grafana/loki:2.9.3` |
| Promtail | Log collection | `grafana/promtail:2.9.3` |
| OTel Collector | Telemetry pipeline | `otel/opentelemetry-collector-contrib:0.91.0` |
| Alertmanager | Alert routing | `prom/alertmanager:v0.26.0` |
| MinIO | S3-compatible storage | `minio/minio:RELEASE.2024-11-07T00-52-20Z` |
| Pyroscope | Continuous profiling | `grafana/pyroscope:1.10.1` |
| Node Exporter | Host metrics | `prom/node-exporter:v1.7.0` |
| HotROD | Example trace app | `jaegertracing/example-hotrod:1.62.0` |
| UNRDF | Application (optional) | `unrdf:6.0.0-rc.1` |
