# Explanation: Service Discovery — Docker Compose to Kubernetes

How hostnames and service addresses change when moving from Docker Compose to Kubernetes.

---

## The problem

Docker Compose uses container names as DNS hostnames. When two services are on the same Docker network, they can reach each other by name:

```yaml
# docker-compose.yml
prometheus:
  image: prom/prometheus
  # reachable at "prometheus:9090" from other containers

alertmanager:
  image: prom/alertmanager
  # reachable at "alertmanager:9093"
```

In Prometheus config:
```yaml
scrape_configs:
  - job_name: 'alertmanager'
    static_configs:
      - targets: ['alertmanager:9093']
```

This works because Docker's embedded DNS resolves `alertmanager` to the container's IP.

## Kubernetes DNS

Kubernetes has its own DNS system. Every Service gets a DNS record:

```
<service-name>.<namespace>.svc.cluster.local
```

So `alertmanager` becomes:
```
unrdf-observability-alertmanager.unrdf-observability.svc.cluster.local
```

Fortunately, K8s DNS supports **short names** within the same namespace. If Prometheus and Alertmanager are both in `unrdf-observability`, you can use:
```
unrdf-observability-alertmanager
```

## How the Helm chart handles this

Every ConfigMap that references another service uses the Helm `tpl()` function to render K8s service names at install time.

For example, in `configmap-prometheus.yaml`:

```yaml
data:
  prometheus.yml: |
    scrape_configs:
      - job_name: 'alertmanager'
        static_configs:
          - targets:
            - '{{ printf "%s-alertmanager.%s.svc.cluster.local:9093"
                (include "unrdf-observability.fullname" .)
                .Release.Namespace }}'
```

This renders to:
```
- 'unrdf-observability-alertmanager.unrdf-observability.svc.cluster.local:9093'
```

### Helm helper templates

The chart provides helpers in `_helpers.tpl` to keep this DRY:

| Helper | Output | Example |
|--------|--------|---------|
| `unrdf-observability.fullname` | `unrdf-observability` | Release name (truncated to 63 chars) |
| `unrdf-observability.service` | `unrdf-observability-<component>` | K8s Service name |
| `unrdf-observability.hostname` | `unrdf-observability-<component>.<ns>.svc.cluster.local` | Full FQDN |

Usage in ConfigMaps:
```yaml
{{ printf "%s-alertmanager.%s.svc.cluster.local:9093"
   (include "unrdf-observability.fullname" .)
   .Release.Namespace }}
```

## Escaping Prometheus template variables

A subtlety: Prometheus configs use `{{ }}` for their own template variables (in alert annotations, etc.). Helm also uses `{{ }}`. Without escaping, Helm tries to parse Prometheus templates.

The chart uses the escape pattern:
```yaml
# Helm renders {{ "{{" }} as literal {{
summary: "High error rate on {{ "{{" }} $labels.job {{ "}}" }}"
# Renders to:
summary: "High error rate on {{ $labels.job }}"
```

This pattern is used in:
- `configmap-prometheus.yaml` (alert annotations)
- `configmap-loki-rules.yaml` (alert annotations)
- `configmap-grafana-datasources.yaml` (JSON data)

## Service types

Not all services need external access:

- **NodePort**: Exposed outside the cluster via kind `extraPortMappings` (Grafana, Prometheus, Tempo, etc.)
- **ClusterIP**: Internal only (OTel Collector, Promtail) — no external access needed
