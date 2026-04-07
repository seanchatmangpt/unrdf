# Reference: Makefile Targets

All `k8s-*` targets in the root Makefile.

---

## Targets

### `make k8s-create`

Create a kind cluster named `unrdf` using `k8s/kind-config.yaml`.

```bash
kind create cluster --config k8s/kind-config.yaml --name unrdf
```

Prerequisites: Docker running, `kind` installed.

### `make k8s-up`

Deploy (or upgrade) the Helm release into the cluster.

```bash
helm upgrade --install unrdf-observability k8s/helm/unrdf-observability \
  --namespace unrdf-observability --create-namespace \
  -f k8s/helm/unrdf-observability/values.yaml
```

Creates the namespace if it doesn't exist.

### `make k8s-down`

Remove the Helm release. Keeps the cluster and PVCs.

```bash
helm uninstall unrdf-observability --namespace unrdf-observability
```

### `make k8s-destroy`

Delete the kind cluster entirely. Removes all data.

```bash
kind delete cluster --name unrdf
```

### `make k8s-status`

Show pod status in the namespace.

```bash
kubectl -n unrdf-observability get pods
```

### `make k8s-logs`

Tail logs from all containers in the namespace.

```bash
kubectl -n unrdf-observability logs -f --all-containers --max-log-requests=20
```

### `make k8s-ports`

Print the port mapping table.

```
UNRDF API:     http://localhost:3000
Grafana:       http://localhost:3001
Prometheus:    http://localhost:9091
Tempo API:     http://localhost:13200
Loki:          http://localhost:13100
Pyroscope:     http://localhost:14040
HotROD:        http://localhost:8082
Node Exporter: http://localhost:9100
Alertmanager:  http://localhost:19093
MinIO API:     http://localhost:19000
MinIO Console: http://localhost:19001
OTLP gRPC:     localhost:14317
OTLP HTTP:     localhost:14318
```

---

## Common workflows

```bash
# Fresh start
make k8s-create && make k8s-up

# Redeploy after chart changes
make k8s-down && make k8s-up

# Check health
make k8s-status
make k8s-ports

# Clean everything
make k8s-down && make k8s-destroy
```
