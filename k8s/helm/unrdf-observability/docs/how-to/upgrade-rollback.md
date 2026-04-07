# How to: Upgrade and Rollback

Manage Helm release versions with safe upgrade and rollback procedures.

---

## Check current release

```bash
helm list -n unrdf-observability
helm history unrdf-observability -n unrdf-observability
```

## Upgrade the chart

### Regular upgrade

```bash
helm upgrade unrdf-observability k8s/helm/unrdf-observability \
  --namespace unrdf-observability \
  --reuse-values
```

`--reuse-values` preserves your existing values. Without it, Helm resets to the chart defaults.

### Upgrade with new values

```bash
helm upgrade unrdf-observability k8s/helm/unrdf-observability \
  --namespace unrdf-observability \
  -f values.yaml \
  -f values-prod.yaml
```

### Upgrade image versions

```bash
helm upgrade unrdf-observability k8s/helm/unrdf-observability \
  --namespace unrdf-observability \
  --reuse-values \
  --set prometheus.imageTag=v2.54.0 \
  --set tempo.imageTag=v2.7.1 \
  --set loki.imageTag=2.9.6
```

## Upgrade chart version

If the chart itself is updated (new `Chart.yaml` version):

```bash
helm upgrade unrdf-observability k8s/helm/unrdf-observability \
  --namespace unrdf-observability \
  --reuse-values \
  --force   # needed if chart hooks changed
```

## Dry-run before applying

```bash
helm upgrade unrdf-observability k8s/helm/unrdf-observability \
  --namespace unrdf-observability \
  --reuse-values \
  --dry-run
```

## Diff before applying

Requires the `helm-diff` plugin:

```bash
helm plugin install https://github.com/databus23/helm-diff

helm diff upgrade unrdf-observability k8s/helm/unrdf-observability \
  --namespace unrdf-observability \
  --reuse-values
```

## Rollback

```bash
# List history
helm history unrdf-observability -n unrdf-observability

# Rollback to previous revision
helm rollback unrdf-observability -n unrdf-observability

# Rollback to specific revision
helm rollback unrdf-observability 2 -n unrdf-observability
```

## Troubleshooting failed upgrades

```bash
# Describe the release (shows hooks, values, etc.)
helm get all unrdf-observability -n unrdf-observability

# Check for pending hooks
helm get hooks unrdf-observability -n unrdf-observability

# If a hook Job is stuck, delete it and retry
kubectl -n unrdf-observability delete job unrdf-observability-minio-setup
helm upgrade unrdf-observability k8s/helm/unrdf-observability \
  --namespace unrdf-observability --reuse-values
```
