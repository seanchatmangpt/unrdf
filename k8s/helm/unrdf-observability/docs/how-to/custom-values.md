# How to: Override Default Values

Customize the Helm chart for your environment using a values file or `--set` flags.

---

## Using a values file

Create `values-dev.yaml`:

```yaml
namespace: unrdf-observability

prometheus:
  imageTag: v2.54.0
  retention: 7d
  resources:
    requests:
      cpu: 500m
      memory: 1Gi
    limits:
      cpu: '2'
      memory: 4Gi

grafana:
  adminPassword: s3cret

minio:
  rootUser: my-admin
  rootPassword: my-password-12345678

storage:
  prometheus: 50Gi
  tempo: 50Gi
```

Deploy:

```bash
helm upgrade --install unrdf-observability k8s/helm/unrdf-observability \
  --namespace unrdf-observability \
  --create-namespace \
  -f k8s/helm/unrdf-observability/values.yaml \
  -f values-dev.yaml
```

Values files are merged left-to-right. The last file wins on conflicts.

## Using --set flags

For quick one-offs:

```bash
helm upgrade --install unrdf-observability k8s/helm/unrdf-observability \
  --namespace unrdf-observability \
  --set grafana.adminPassword=newpass \
  --set prometheus.retention=7d \
  --set storage.prometheus=50Gi
```

## Enable the UNRDF app

```bash
helm upgrade --install unrdf-observability k8s/helm/unrdf-observability \
  --namespace unrdf-observability \
  --set unrdf.enabled=true \
  --set unrdf.imageTag=6.0.0-rc.1
```

## Disable the example app

```bash
# The example app doesn't have an enabled flag yet. To remove it:
helm upgrade --install unrdf-observability k8s/helm/unrdf-observability \
  --namespace unrdf-observability \
  --set exampleApp.replicas=0
```

## Common overrides

| Goal | Values override |
|------|----------------|
| More Prometheus storage | `--set storage.prometheus=100Gi` |
| Shorter retention | `--set prometheus.retention=7d` |
| Stronger Grafana password | `--set grafana.adminPassword=<pass>` |
| Change MinIO credentials | `--set minio.rootUser=<user> --set minio.rootPassword=<pass>` |
| Pin image version | `--set tempo.imageTag=v2.7.1` |
| More replicas | `--set prometheus.replicas=3` |

## See current values

```bash
helm get values unrdf-observability -n unrdf-observability
```

## See all effective values (merged defaults + overrides)

```bash
helm get values unrdf-observability -n unrdf-observability --all
```
