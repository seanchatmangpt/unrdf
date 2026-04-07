# How to: Debug Pods and Services

Diagnose common issues with the observability stack running in kind.

---

## Pod not starting

```bash
# Check pod events and status
kubectl -n unrdf-observability describe pod <pod-name>

# Common causes:
# - ImagePullBackOff → image doesn't exist or can't be pulled
# - CrashLoopBackOff → container exits immediately (check logs)
# - Pending → resource limits too high for kind node

# Check resource availability on kind node
kubectl top nodes
```

## Container crashes

```bash
# Get previous container logs (before restart)
kubectl -n unrdf-observability logs <pod-name> --previous

# Live logs
kubectl -n unrdf-observability logs -f <pod-name>
```

## All pods logs at once

```bash
make k8s-logs

# or filter by component
kubectl -n unrdf-observability logs -f deployment/unrdf-observability-prometheus
kubectl -n unrdf-observability logs -f deployment/unrdf-observability-tempo
```

## Service connectivity

```bash
# Test DNS resolution from inside a pod
kubectl -n unrdf-observability exec deployment/unrdf-observability-prometheus -- \
  nslookup unrdf-observability-tempo

# Test HTTP connectivity
kubectl -n unrdf-observability exec deployment/unrdf-observability-prometheus -- \
  wget -qO- http://unrdf-observability-tempo:3200/ready

# Check service endpoints (shows which pods are behind each service)
kubectl -n unrdf-observability get endpoints
```

## Prometheus targets down

```bash
# Check which targets are up/down
curl -s http://localhost:9091/api/v1/targets | \
  jq '.data.activeTargets[] | select(.health != "up") | {job: .labels.job, health: .health, error: .lastErrorScrape}'

# Common fix: restart the pod that's being scraped
kubectl -n unrdf-observability rollout restart deployment/<target-deployment>
```

## Tempo not receiving traces

```bash
# Check Tempo health
curl -s http://localhost:3200/status | jq

# Check Tempo metrics (ingest rate)
curl -s http://localhost:3200/metrics | grep tempo_distributor_traces_received

# Verify OTel Collector is running and connected
kubectl -n unrdf-observability logs deployment/unrdf-observability-otel-collector | tail -20

# Check the collector's own metrics
curl -s http://localhost:8888/metrics | grep otelcol_receiver_accepted_spans
```

## Loki not ingesting logs

```bash
# Check Loki health
curl -s http://localhost:3100/ready

# Query Loki directly
curl -s "http://localhost:3100/loki/api/v1/query_range?query={namespace=\"unrdf-observability\"}&limit=10" | jq

# Check Promtail logs
kubectl -n unrdf-observability logs deployment/unrdf-observability-promtail | tail -20

# Common issue: Promtail can't find logs in kind (containerd paths)
# This is expected — promtail uses livenessProbe (not readinessProbe)
```

## MinIO storage issues

```bash
# Check MinIO health
curl -s http://localhost:9000/minio/health/live

# Verify bucket exists
kubectl -n unrdf-observability exec deployment/unrdf-observability-minio -- \
  mc alias set local http://localhost:9000 $MINIO_ROOT_USER $MINIO_ROOT_PASSWORD && \
  mc ls local/

# Check the setup job ran successfully
kubectl -n unrdf-observability get job unrdf-observability-minio-setup
```

## Port not accessible from host

```bash
# Verify kind port mappings
docker port unrdf-control-plane

# Check if something on the host is already using the port
lsof -i :3001  # Grafana
lsof -i :9091  # Prometheus
```

## Helm release issues

```bash
# Full release dump
helm get all unrdf-observability -n unrdf-observability

# Check for failed hooks
helm get hooks unrdf-observability -n unrdf-observability

# Delete stuck hook jobs
kubectl -n unrdf-observability delete job <stuck-job-name>

# Nuclear option: uninstall and reinstall
helm uninstall unrdf-observability -n unrdf-observability
helm install unrdf-observability k8s/helm/unrdf-observability \
  --namespace unrdf-observability --create-namespace
```
