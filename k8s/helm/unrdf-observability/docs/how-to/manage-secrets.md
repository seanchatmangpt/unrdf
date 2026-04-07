# How to: Manage Secrets Securely

The chart stores MinIO and Grafana credentials as K8s Secrets. This guide covers rotating and externalizing them.

---

## Current secret handling

Two secrets are created by the chart:

| Secret | Keys | Source |
|--------|------|--------|
| `unrdf-observability-minio` | `MINIO_ROOT_USER`, `MINIO_ROOT_PASSWORD` | `values.yaml` |
| `unrdf-observability-grafana` | `GF_SECURITY_ADMIN_PASSWORD` | `values.yaml` |

These are created as opaque K8s Secrets with base64-encoded values from your `values.yaml`.

## Rotate MinIO credentials

```bash
# 1. Generate new credentials
NEW_USER=$(openssl rand -hex 8)
NEW_PASS=$(openssl rand -hex 16)

# 2. Upgrade with new values
helm upgrade unrdf-observability k8s/helm/unrdf-observability \
  --namespace unrdf-observability \
  --set minio.rootUser="$NEW_USER" \
  --set minio.rootPassword="$NEW_PASS" \
  --reuse-values

# 3. Restart MinIO to pick up new credentials
kubectl -n unrdf-observability rollout restart deployment/unrdf-observability-minio

# 4. Restart Tempo (it connects to MinIO)
kubectl -n unrdf-observability rollout restart deployment/unrdf-observability-tempo

# 5. Re-run the bucket setup job
kubectl -n unrdf-observability delete job unrdf-observability-minio-setup
helm upgrade unrdf-observability k8s/helm/unrdf-observability \
  --namespace unrdf-observability --reuse-values
```

## Rotate Grafana password

```bash
helm upgrade unrdf-observability k8s/helm/unrdf-observability \
  --namespace unrdf-observability \
  --set grafana.adminPassword="new-password" \
  --reuse-values

kubectl -n unrdf-observability rollout restart deployment/unrdf-observability-grafana
```

## Use an external secret store

For production, use a secret management tool instead of values.yaml:

### Option A: Sealed Secrets

```bash
# Install Sealed Secrets controller
helm install sealed-secrets sealed-secrets/sealed-secrets \
  --namespace kube-system

# Seal your secret
echo -n "s3cret" | kubectl create secret generic grafana-password \
  --dry-run=client --from-file=password=/dev/stdin -o yaml | \
  kubeseal -o yaml > sealed-grafana-secret.yaml

# Add to your chart as a file in templates/
```

### Option B: Vault Agent Injector

If you use HashiCorp Vault, annotate your deployments:

```yaml
spec:
  template:
    metadata:
      annotations:
        vault.hashicorp.com/agent-inject: "true"
        vault.hashicorp.com/role: "unrdf"
        vault.hashicorp.com/secret-volume-path: "/etc/secrets"
        vault.hashicorp.com/agent-inject-secret-config: "secret/data/unrdf/minio"
```

### Option C: Kubernetes External Secrets Operator

```yaml
apiVersion: external-secrets.io/v1beta1
kind: ExternalSecret
metadata:
  name: minio-creds
  namespace: unrdf-observability
spec:
  refreshInterval: 1h
  secretStoreRef:
    name: vault
    kind: ClusterSecretStore
  target:
    name: unrdf-observability-minio
  data:
    - secretKey: MINIO_ROOT_USER
      remoteRef:
        key: secret/data/unrdf/minio
        property: username
    - secretKey: MINIO_ROOT_PASSWORD
      remoteRef:
        key: secret/data/unrdf/minio
        property: password
```

## Never commit secrets to git

Add to `.gitignore`:

```
values-prod.yaml
values-*.local.yaml
```
