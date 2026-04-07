# Tutorial: Sending Your Application's Traces

**What you'll learn**: How to configure an application to send OpenTelemetry traces to the in-cluster collector.

**Prerequisites**: Completed [01-first-cluster.md](01-first-cluster.md), cluster running

---

## How it works

The OTel Collector is the central pipeline. All telemetry (traces, metrics, logs) flows through it:

```
Your App ──OTLP──▶ OTEL Collector ──▶ Tempo (traces)
                                   ──▶ Prometheus (metrics)
                                   ──▶ Loki (logs)
```

The collector is exposed inside the cluster at:
- **gRPC**: `unrdf-observability-otel-collector.unrdf-observability:4317`
- **HTTP**: `unrdf-observability-otel-collector.unrdf-observability:4318`

From outside the cluster (host machine):
- **gRPC**: `localhost:4317`
- **HTTP**: `localhost:4318`

## Option A: In-cluster application

If your app runs as a Deployment in the same namespace, use the K8s DNS name:

```yaml
apiVersion: apps/v1
kind: Deployment
metadata:
  name: my-app
  namespace: unrdf-observability
spec:
  template:
    spec:
      containers:
        - name: my-app
          env:
            - name: OTEL_EXPORTER_OTLP_ENDPOINT
              value: "http://unrdf-observability-otel-collector:4318"
            - name: OTEL_SERVICE_NAME
              value: "my-app"
```

## Option B: Host application (outside K8s)

Use the kind port mapping. OTLP HTTP is mapped to localhost:4318:

```bash
OTEL_EXPORTER_OTLP_ENDPOINT=http://localhost:4318 \
OTEL_SERVICE_NAME=my-app \
  node my-app.mjs
```

## Option C: Enable the UNRDF app

The Helm chart includes an UNRDF deployment (disabled by default). To enable it:

1. Build and load the image into kind:
```bash
docker build -t unrdf:6.0.0-rc.1 .
kind load docker-image unrdf:6.0.0-rc.1 --name unrdf
```

2. Upgrade the release with the app enabled:
```bash
helm upgrade unrdf-observability k8s/helm/unrdf-observability \
  --namespace unrdf-observability \
  --set unrdf.enabled=true
```

## Verify traces arrive

1. Run your application and generate some requests
2. Open Grafana at http://localhost:3001
3. Go to **Explore** → **Tempo** → **Search Traces**
4. Filter by `service.name = "my-app"`

You should see your traces with spans, attributes, and timing.

---

**Next tutorial**: [03-dashboards.md](03-dashboards.md) — Using pre-built Grafana dashboards.
