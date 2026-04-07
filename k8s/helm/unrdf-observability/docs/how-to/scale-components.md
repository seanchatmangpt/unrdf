# How to: Scale Components

Adjust resource allocation and replica counts for different workloads.

---

## Change replica count

```bash
# Scale Prometheus to 3 replicas (note: needs shared storage for TSDB)
helm upgrade unrdf-observability k8s/helm/unrdf-observability \
  --namespace unrdf-observability \
  --reuse-values \
  --set prometheus.replicas=3

# Scale down to 0 (disable a component)
helm upgrade unrdf-observability k8s/helm/unrdf-observability \
  --namespace unrdf-observability \
  --reuse-values \
  --set exampleApp.replicas=0
```

**Note**: Most components in this chart are stateful (PVC-backed). Scaling beyond 1 replica requires shared storage or a StatefulSet, which this chart doesn't currently use. For production horizontal scaling, consider using the upstream Helm charts for individual components (prometheus-community/kube-prometheus-stack, grafana/loki-stack, etc.).

## Adjust resource requests/limits

```bash
# Give Prometheus more memory
helm upgrade unrdf-observability k8s/helm/unrdf-observability \
  --namespace unrdf-observability \
  --reuse-values \
  --set prometheus.resources.requests.cpu=500m \
  --set prometheus.resources.requests.memory=2Gi \
  --set prometheus.resources.limits.cpu=4 \
  --set prometheus.resources.limits.memory=8Gi
```

### Resource guidance by workload

| Workload | Component | CPU req | CPU lim | Mem req | Mem lim |
|----------|-----------|---------|---------|---------|---------|
| Dev laptop | all | defaults (values.yaml) | defaults | defaults | defaults |
| Team shared | prometheus | 500m | 2 | 2Gi | 8Gi |
| Team shared | tempo | 250m | 2 | 512Mi | 4Gi |
| Team shared | loki | 250m | 2 | 512Mi | 4Gi |
| Production | prometheus | 1 | 4 | 4Gi | 16Gi |
| Production | tempo | 1 | 4 | 2Gi | 16Gi |

## Adjust storage sizes

```bash
helm upgrade unrdf-observability k8s/helm/unrdf-observability \
  --namespace unrdf-observability \
  --reuse-values \
  --set storage.prometheus=100Gi \
  --set storage.tempo=50Gi \
  --set storage.loki=50Gi
```

**Important**: K8s PVCs cannot be resized in-place with the `standard` storage class used by kind. To change storage size:
1. Uninstall the release
2. Delete the PVCs
3. Reinstall with new storage values

```bash
helm uninstall unrdf-observability -n unrdf-observability
kubectl -n unrdf-observability delete pvc --all
helm install unrdf-observability k8s/helm/unrdf-observability \
  --namespace unrdf-observability --create-namespace
```

## Change retention periods

```bash
# Prometheus: shorter retention (default 30d)
helm upgrade unrdf-observability k8s/helm/unrdf-observability \
  --namespace unrdf-observability \
  --reuse-values \
  --set prometheus.retention=7d

# Then restart Prometheus to pick up the new arg
kubectl -n unrdf-observability rollout restart deployment/unrdf-observability-prometheus
```

## Disable components you don't need

For lighter local dev, disable unnecessary components:

```bash
helm upgrade unrdf-observability k8s/helm/unrdf-observability \
  --namespace unrdf-observability \
  --reuse-values \
  --set pyroscope.replicas=0 \
  --set exampleApp.replicas=0 \
  --set nodeExporter.resources.requests.cpu=10m
```
