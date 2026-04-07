# Tutorial: Create Your First kind Cluster

**What you'll learn**: How to create a local Kubernetes cluster using `kind` and deploy the UNRDF observability stack.

**Prerequisites**: Docker running, `kind` v0.30.0+, `kubectl` v1.35.0+, `helm` v3.19.0+

---

## Step 1: Create the cluster

```bash
make k8s-create
```

This runs `kind create cluster` using the config at `k8s/kind-config.yaml`. The config maps 13 ports from your host into the cluster node so you can access services without `kubectl port-forward`.

Verify the cluster is running:

```bash
kubectl cluster-info
# Kubernetes control plane is running at https://127.0.0.1:xxxxx
```

## Step 2: Deploy the stack

```bash
make k8s-up
```

This runs `helm upgrade --install` which creates the `unrdf-observability` namespace (if needed) and deploys all 11 services plus a MinIO setup job.

## Step 3: Watch pods come up

```bash
make k8s-status
# or watch live:
kubectl -n unrdf-observability get pods -w
```

Expected output — all pods `Running` or `Completed` (the minio-setup Job):

```
NAME                                              READY   STATUS      RESTARTS   AGE
unrdf-observability-alertmanager-xxxxx            1/1     Running     0          60s
unrdf-observability-example-app-xxxxx             1/1     Running     0          60s
unrdf-observability-grafana-xxxxx                 1/1     Running     0          60s
unrdf-observability-loki-xxxxx                    1/1     Running     0          60s
unrdf-observability-minio-xxxxx                   1/1     Running     0          60s
unrdf-observability-minio-setup-xxxxx             0/1     Completed   0          60s
unrdf-observability-node-exporter-xxxxx           1/1     Running     0          60s
unrdf-observability-otel-collector-xxxxx          1/1     Running     0          60s
unrdf-observability-prometheus-xxxxx              1/1     Running     0          60s
unrdf-observability-promtail-xxxxx                1/1     Running     0          60s
unrdf-observability-pyroscope-xxxxx               1/1     Running     0          60s
unrdf-observability-tempo-xxxxx                   1/1     Running     0          60s
```

## Step 4: Verify services

Open these URLs in your browser:

| Service       | URL                         |
|---------------|-----------------------------|
| Grafana       | http://localhost:3001       |
| Prometheus    | http://localhost:9091       |
| MinIO Console | http://localhost:9001       |
| HotROD        | http://localhost:8082       |

Grafana login: `admin` / `admin`

## Step 5: Generate trace traffic

Click around the HotROD app at http://localhost:8082 a few times. This sends OpenTelemetry traces through the collector to Tempo.

Then in Grafana:
1. Go to **Explore** → select **Tempo** datasource
2. Click **Search Traces**
3. You should see traces from HotROD with service names like `frontend`, `driver`, `customer`

## Step 6: Tear down

```bash
make k8s-down      # remove Helm release (keeps cluster)
make k8s-destroy   # delete the kind cluster entirely
```

---

**Next tutorial**: [02-first-trace.md](02-first-trace.md) — Sending your own traces to the stack.
