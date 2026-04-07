# Explanation: Design Decisions

Why the chart is structured the way it is, and what alternatives were considered.

---

## Why Helm?

Alternatives considered:
- **Raw YAML manifests**: Would work, but no parameterization — every environment would need separate files
- **Kustomize**: Good for overlays but doesn't handle ConfigMap template rendering (service names need to be substituted)
- **Helm**: Provides values-based configuration, template rendering with `tpl()`, lifecycle hooks (Jobs), and `--reuse-values` for upgrades

Helm was chosen because the ConfigMaps contain service names that change based on the release name and namespace. Only Helm's `tpl()` function can render these at install time.

## Why one monolithic chart instead of per-component charts?

Alternatives considered:
- **Separate charts per component**: More modular, but requires managing dependencies (Helmfile, Argo CD ApplicationSets)
- **kube-prometheus-stack**: Covers Prometheus/Grafana/Alertmanager but not Tempo/Loki/Pyroscope/OTel Collector
- **Grafana Loki Stack**: Covers Loki/Promtail but not Tempo/OTel Collector

The monolithic chart keeps the dev workflow simple — one `helm install` deploys everything. For production, you'd likely use the upstream charts for individual components and a composition tool like Helmfile.

## Why kind for local development?

Alternatives considered:
- **minikube**: Heavier, slower startup, more moving parts
- **k3d**: Good alternative, but kind is the de facto standard for CI
- **Docker Compose**: Already existed, but the goal was K8s-native deployment

kind was chosen because:
1. It's just Docker containers — no VM overhead
2. It matches CI environments (GitHub Actions, GitLab CI both use kind)
3. `extraPortMappings` give direct host access without port-forward
4. The user already has it installed (v0.30.0)

## Why NodePort instead of Ingress?

kind doesn't ship with an Ingress controller by default. Adding an Ingress controller (nginx-ingress, traefik) would add complexity to the dev workflow.

NodePort with `extraPortMappings` gives the same result (localhost access) with fewer moving parts. For production, you'd replace NodePort with Ingress or use a cloud load balancer.

## Why PVC instead of emptyDir?

`emptyDir` volumes are ephemeral — data is lost when pods restart. For a monitoring stack, losing all Prometheus metrics or Tempo traces on pod restart defeats the purpose.

PVCs with `storageClassName: standard` (kind's default) use the host's filesystem, giving persistence across pod restarts.

Limitation: kind's `standard` storage class doesn't support volume expansion. Changing PVC size requires recreation.

## Why the UNRDF app is disabled by default

The UNRDF application needs to be built as a Docker image before it can run in K8s. The image doesn't exist in a public registry. Rather than requiring users to build the image before their first `helm install`, the app defaults to `enabled: false`.

Users can enable it after loading the image:
```bash
docker build -t unrdf:6.0.0-rc.1 .
kind load docker-image unrdf:6.0.0-rc.1 --name unrdf
helm upgrade --set unrdf.enabled=true ...
```

## Why MinIO instead of Tempo's local storage

Tempo supports local filesystem storage (`storage: filesystem`), but:
1. Production uses S3/GCS — local filesystem doesn't exercise the same code path
2. MinIO gives us an S3-compatible API for testing
3. The MinIO setup Job demonstrates K8s Helm hooks for initialization

## Why Promtail uses livenessProbe instead of readinessProbe

In kind, containers use containerd (not Docker). Promtail looks for logs at `/var/lib/docker/containers`, which doesn't exist in kind. This causes the readiness probe to fail, blocking the pod from receiving traffic.

Since Promtail is a log collector (it doesn't receive traffic from other services), readiness doesn't matter. A liveness probe with a long `failureThreshold` ensures the pod isn't restarted repeatedly while it searches for logs.

## Why OTel Collector exports metrics on :8889 instead of :8888

The OTel Collector has a built-in metrics server that binds to `:8888` by default (its own operational metrics). If the prometheus exporter also tries to bind `:8888`, there's a port conflict.

Moving the prometheus exporter to `:8889` avoids this. Prometheus is configured to scrape `:8889`.
