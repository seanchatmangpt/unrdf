# Reference: Security Hardening Checklist

Current security posture and recommended hardening for the Helm chart. This document covers what's implemented, what's not, and how to close gaps.

---

## Current Posture (as-deployed)

### Implemented

| Control | Status | Where |
|---------|--------|-------|
| Resource limits and requests | All 12 components | `values.yaml` → deployments |
| Read-only ConfigMap mounts | All config volumes | `deployment-*.yaml` `readOnly: true` |
| MinIO credentials in K8s Secret | Yes | `secret-minio.yaml` |
| Grafana password in K8s Secret | Yes | `secret-grafana.yaml` |
| Secret consumed via `secretKeyRef` | MinIO, Grafana | deployment-minio.yaml, deployment-grafana.yaml |
| Promtail uses dedicated ServiceAccount | Yes | `serviceaccount-promtail.yaml` |
| Promtail RBAC (least privilege) | Yes | ClusterRole: get/list/watch only |
| Helm hook job cleanup | Yes | `hook-delete-policy: hook-succeeded` |
| Readiness probes | Prometheus, Grafana, Tempo, Loki, MinIO, Pyroscope, OTEL Collector, HotROD, UNRDF | deployment-*.yaml |
| Liveness probes | Promtail (long timeout) | deployment-promtail.yaml |
| Container images pinned by tag | All 12 | values.yaml |

### Not Implemented (gaps)

| Control | Risk | Priority |
|---------|------|----------|
| `securityContext` (runAsNonRoot, readOnlyRootFilesystem) | Containers run as default user, writable rootfs | HIGH |
| `runAsNonRoot: true` | Most images run as root by default | HIGH |
| `readOnlyRootFilesystem: true` | Containers can write to rootfs | HIGH |
| `allowPrivilegeEscalation: false` | Privilege escalation possible | HIGH |
| `drop capabilities (ALL)` | Unnecessary Linux capabilities retained | HIGH |
| `seccompProfile: runtime/default` | No seccomp restriction | MEDIUM |
| NetworkPolicy | All pods can talk to all pods | MEDIUM |
| Pod security labels | No PSP/SSA labels enforced | MEDIUM |
| `hostNetwork` on node-exporter | Required for host metrics, but broadens attack surface | LOW (necessary) |
| `hostPID` on node-exporter | Can see all host processes | LOW (necessary) |
| `hostPath` mounts | node-exporter, promtail mount host filesystems | LOW (necessary) |
| Image digest pinning | Tags can float; use digests for reproducibility | LOW |
| `imagePullPolicy: Always` (prod) | kind uses `IfNotPresent` default; prod should pin | LOW |
| ServiceAccount per pod | Only Promtail has a dedicated SA; rest use `default` | MEDIUM |
| Pod anti-affinity | No spread/restriction on pod placement | LOW |

---

## Hardening Recommendations

### 1. Add securityContext to all deployments

Add to `values.yaml` and reference in templates:

```yaml
# values.yaml
podSecurityContext:
  runAsNonRoot: true
  runAsUser: 1000
  runAsGroup: 3000
  fsGroup: 3000

securityContext:
  allowPrivilegeEscalation: false
  readOnlyRootFilesystem: true
  capabilities:
    drop:
      - ALL
  seccompProfile:
    type: RuntimeDefault
```

Apply in each deployment template:

```yaml
spec:
  securityContext:
    {{- toYaml .Values.podSecurityContext | nindent 4 }}
  containers:
    - name: {{ $component }}
      securityContext:
        {{- toYaml .Values.securityContext | nindent 8 }}
```

**Exceptions**: Node-exporter requires `hostNetwork` and `hostPID` (cannot use `runAsNonRoot`). MinIO needs writable filesystem at `/data`.

### 2. NetworkPolicy

Add a default-deny NetworkPolicy and explicit allow rules:

```yaml
# Default deny all ingress/egress
apiVersion: networking.k8s.io/v1
kind: NetworkPolicy
metadata:
  name: default-deny
  namespace: unrdf-observability
spec:
  podSelector: {}
  policyTypes:
    - Ingress
    - Egress
---
# Allow Prometheus to scrape targets
apiVersion: networking.k8s.io/v1
kind: NetworkPolicy
metadata:
  name: prometheus-scrape
spec:
  podSelector:
    matchLabels:
      app.kubernetes.io/component: prometheus
  ingress:
    - from:
        - podSelector:
            matchLabels:
              app.kubernetes.io/component: prometheus
      ports:
        - port: 9090
```

### 3. Dedicated ServiceAccounts

Create a ServiceAccount for each component instead of using `default`:

```yaml
{{- range $component := list "prometheus" "grafana" "tempo" "loki" "otel-collector" "minio" "alertmanager" }}
apiVersion: v1
kind: ServiceAccount
metadata:
  name: {{ printf "%s-%s" (include "unrdf-observability.fullname" .) $component }}
  namespace: {{ .Values.namespace }}
  labels:
    {{- include "unrdf-observability.componentLabels" . | nindent 4 }}
---
{{- end }}
```

Then reference in each deployment:

```yaml
spec:
  serviceAccountName: {{ printf "%s-%s" (include "unrdf-observability.fullname" .) $component }}
```

### 4. Pod Security Standards labels

Add to the namespace (or per-pod):

```yaml
# Enforce restricted profile
apiVersion: v1
kind: Namespace
metadata:
  name: unrdf-observability
  labels:
    pod-security.kubernetes.io/enforce: restricted
    pod-security.kubernetes.io/audit: restricted
    pod-security.kubernetes.io/warn: restricted
```

**Note**: This will block node-exporter (requires privileged). Use a separate namespace for node-exporter if enforcing restricted profile.

### 5. Pin images by digest

```bash
# Get digest for each image
docker pull prom/prometheus:v2.48.0
docker inspect --format='{{index .RepoDigests 0}}' prom/prometheus:v2.48.0
# → prom/prometheus@sha256:abc123...

# Update values.yaml
prometheus:
  imageRepo: prom/prometheus
  imageDigest: sha256:abc123...
```

### 6. Externalize secrets

See [how-to/manage-secrets.md](../how-to/manage-secrets.md) for Sealed Secrets, Vault, or External Secrets Operator integration.

---

## Hardening tiers

| Tier | Scope | Controls |
|------|-------|----------|
| **Dev** (current) | Local kind cluster | Resource limits, Secrets, basic RBAC |
| **Staging** | Shared kind/real cluster | + securityContext, NetworkPolicy, per-pod SA |
| **Production** | Cloud K8s | + PSS labels, digest pinning, external secrets, pod anti-affinity |
