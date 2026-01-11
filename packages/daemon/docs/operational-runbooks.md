# Operational Runbooks: @unrdf/daemon

Step-by-step procedures for operating @unrdf/daemon in production. Covers day-1 setup, day-2 operations, common failure scenarios, performance tuning, and upgrade/rollback procedures.

**Audience**: SREs, operations engineers, on-call engineers
**Time to read**: 30-40 minutes
**Updated**: January 2026

---

## Table of Contents

1. [Day-1 Setup Checklist](#day-1-setup-checklist)
2. [Day-2 Operations](#day-2-operations)
3. [Monitoring & Alerting](#monitoring--alerting)
4. [Common Failure Scenarios](#common-failure-scenarios)
5. [Performance Tuning](#performance-tuning)
6. [Version Upgrades](#version-upgrades)
7. [Rollback Procedures](#rollback-procedures)
8. [Operations Quick Reference](#operations-quick-reference)

---

## Day-1 Setup Checklist

### Pre-Deployment Verification (1 hour)

**Step 1: Verify infrastructure** (15 min)
```bash
# Check Kubernetes cluster is healthy
kubectl get nodes -o wide
# Expected: All nodes Ready, versions consistent

# Verify storage classes available
kubectl get storageclasses
# Expected: fast-ssd present with status "active"

# Test DNS resolution
kubectl run -it --rm debug --image=busybox --restart=Never -- \
  nslookup kubernetes.default
# Expected: DNS works
```

**Step 2: Verify container image** (10 min)
```bash
# Pull image
docker pull unrdf-daemon:1.0.0

# Run image test
docker run --rm unrdf-daemon:1.0.0 \
  node -e "console.log('Image OK')"

# Check image size
docker images unrdf-daemon | awk 'NR==2 {print $7}'
# Expected: < 150 MB
```

**Step 3: Prepare configuration** (15 min)
```bash
# Create namespace
kubectl create namespace unrdf-daemons

# Create secrets
kubectl create secret tls daemon-tls \
  --cert=daemon-cert.pem \
  --key=daemon-key.pem \
  -n unrdf-daemons

# Create ConfigMap
kubectl create configmap daemon-config \
  --from-file=daemon-config.json \
  -n unrdf-daemons
```

**Step 4: Pre-flight checks** (20 min)
```bash
# Validate all YAML manifests
for file in *.yaml; do
  kubectl apply -f $file --dry-run=client -o yaml > /dev/null
  [ $? -eq 0 ] && echo "✓ $file valid" || echo "✗ $file invalid"
done

# Check resource availability
kubectl describe nodes | grep -A 10 "Allocated resources" | head -20

# Verify network policies (if in use)
kubectl get networkpolicies --all-namespaces
```

### Initial Deployment (1.5 hours)

**Step 1: Deploy infrastructure** (20 min)
```bash
# Deploy RBAC
kubectl apply -f 02-rbac.yaml
sleep 10
kubectl get sa -n unrdf-daemons

# Deploy storage
kubectl apply -f storage-classes.yaml
sleep 5
kubectl get storageclasses

# Deploy ConfigMap and Secrets
kubectl apply -f 03-config.yaml 04-secrets.yaml
sleep 5
kubectl describe configmap daemon-config -n unrdf-daemons
```

**Step 2: Deploy daemon cluster** (30 min)
```bash
# Deploy service first (so DNS is ready)
kubectl apply -f 06-service.yaml
sleep 10

# Deploy StatefulSet
kubectl apply -f 05-deployment.yaml
sleep 5
kubectl get statefulset -n unrdf-daemons

# Watch pod startup
kubectl get pods -n unrdf-daemons -w
# Expected: All 3 pods reach Running state (5-10 min)
```

**Step 3: Verify cluster health** (30 min)
```bash
# Wait for all pods ready
kubectl wait --for=condition=ready pod \
  -l app=unrdf-daemon -n unrdf-daemons \
  --timeout=300s

# Check each pod individually
for pod in unrdf-daemon-{0,1,2}; do
  echo "=== Checking $pod ==="
  kubectl get pod $pod -n unrdf-daemons -o wide
  kubectl logs -n unrdf-daemons $pod | tail -20
done

# Test health endpoints
kubectl port-forward -n unrdf-daemons svc/unrdf-daemon 8080:8080 &
sleep 5

curl http://localhost:8080/health/startup
# Expected: { "status": "ready" }

curl http://localhost:8080/health/cluster
# Expected: { "clusterSize": 3, "leader": "unrdf-daemon-0", "replicated": true }

kill %1  # Kill port-forward
```

**Step 4: Initialize operations** (30 min)
```bash
# Port-forward to daemon
kubectl port-forward -n unrdf-daemons svc/unrdf-daemon 8080:8080 &

# Test basic operation execution
curl -X POST http://localhost:8080/operations \
  -H "Content-Type: application/json" \
  -d '{
    "id": "test-op",
    "name": "Test Operation",
    "handler": "console.log(\"success\")"
  }'

# Execute operation
curl -X POST http://localhost:8080/execute/test-op

# Verify operation completed
curl http://localhost:8080/operations/test-op/history | jq '.[-1]'
# Expected: { status: "success", duration: <milliseconds> }

# Check metrics updated
curl http://localhost:8080/metrics | jq '{totalOperations, successRate}'

kill %1  # Kill port-forward
```

### Post-Deployment Validation (30 min)

**Step 1: Security verification**
```bash
# Verify TLS is configured
kubectl exec -n unrdf-daemons unrdf-daemon-0 -- \
  curl -I https://localhost:8443 2>/dev/null | head -1
# Expected: HTTP/2 200 or similar

# Verify RBAC is enforced
kubectl auth can-i get pods --as=system:serviceaccount:unrdf-daemons:daemon-sa
# Expected: yes

# Check network policies
kubectl get networkpolicies -n unrdf-daemons
```

**Step 2: Persistence verification**
```bash
# Check PVC status
kubectl get pvc -n unrdf-daemons
# Expected: All bound, size 50Gi

# Verify data persisted
kubectl exec -n unrdf-daemons unrdf-daemon-0 -- \
  ls -lah /data/raft/
# Expected: Raft state files present

# Test PVC reclaim
kubectl delete pod unrdf-daemon-0 -n unrdf-daemons
sleep 30
kubectl get pod unrdf-daemon-0 -n unrdf-daemons
# Expected: Pod restarted, data intact
```

**Step 3: Monitoring setup**
```bash
# Verify Prometheus scraping
curl http://localhost:9090/api/v1/targets | \
  jq '.data.activeTargets[] | select(.labels.job=="daemon")' | head -20

# Check dashboard imports
kubectl exec -n monitoring prometheus-0 -- \
  curl -I http://unrdf-daemon:8080/metrics
# Expected: HTTP 200
```

**Acceptance criteria**:
- [ ] All 3 pods Running and Ready
- [ ] Cluster health endpoint reports 3 nodes
- [ ] Test operation executed and logged
- [ ] Raft replication confirmed
- [ ] TLS communication established
- [ ] Monitoring metrics flowing
- [ ] PVC persistence verified

---

## Day-2 Operations

### Daily Standup Checks (5 min)

**Every morning, verify health**:
```bash
#!/bin/bash
# daily-standup.sh

NAMESPACE="unrdf-daemons"

echo "=== Pod Status ==="
kubectl get pods -n $NAMESPACE -o wide

echo -e "\n=== Resource Usage ==="
kubectl top pods -n $NAMESPACE

echo -e "\n=== Cluster Health ==="
kubectl exec -n $NAMESPACE unrdf-daemon-0 -- \
  curl -s http://localhost:8080/health/cluster | jq .

echo -e "\n=== Recent Errors ==="
kubectl logs -n $NAMESPACE unrdf-daemon-0 \
  --since=24h | grep ERROR | tail -5

echo -e "\n=== Alerting Status ==="
kubectl exec -n monitoring alertmanager-0 -- \
  curl -s http://localhost:9093/api/v1/alerts | \
  jq '.data[] | select(.labels.service=="daemon")' | head -20
```

### Weekly Maintenance (30 min)

**Every Monday, 2 AM**:

```bash
#!/bin/bash
# weekly-maintenance.sh

echo "=== Weekly Maintenance Report ==="
DATE=$(date +%Y-%m-%d)

# 1. Backup verification
echo -e "\n1. Backup Status:"
aws s3 ls s3://unrdf-daemon-backups/manifests/ --recursive | tail -1

# 2. Log rotation
echo -e "\n2. Log Sizes:"
for pod in unrdf-daemon-{0,1,2}; do
  SIZE=$(kubectl exec -n unrdf-daemons $pod -- \
    du -sh /var/log/daemon/ 2>/dev/null || echo "N/A")
  echo "$pod: $SIZE"
done

# 3. Certificate expiry
echo -e "\n3. Certificate Status:"
kubectl get certificate -n unrdf-daemons -o wide

# 4. Dependency updates check
echo -e "\n4. Outdated Packages:"
kubectl exec -n unrdf-daemons unrdf-daemon-0 -- \
  npm outdated

# 5. Performance trend
echo -e "\n5. Operation Success Rate (7 days):"
curl http://localhost:8080/metrics/7d | jq '.successRate'
```

### Monthly Review (2 hours)

**First business day of each month**:

```bash
#!/bin/bash
# monthly-review.sh

echo "=== Monthly Operations Review ==="
MONTH=$(date +%B)

# Compile metrics
kubectl exec -n unrdf-daemons unrdf-daemon-0 -- \
  curl -s http://localhost:8080/metrics/month | jq '{
    totalOperations,
    successRate,
    avgDuration,
    p95Duration,
    errorCount,
    clusterHealth: {
      replicationLag,
      failoverCount,
      uptime
    }
  }' > /tmp/metrics-$MONTH.json

# Analyze SLO compliance
echo -e "\n=== SLO Compliance ==="
cat > /tmp/slo-check.jq <<'EOF'
{
  availability: (.clusterHealth.uptime / 730 * 100),
  latency: (.p95Duration <= 100),
  errorRate: (.errorCount / .totalOperations * 100)
}
EOF

jq -f /tmp/slo-check.jq /tmp/metrics-$MONTH.json

# Generate report
echo "Report saved to /tmp/metrics-$MONTH.json"
```

---

## Monitoring & Alerting

### Key Metrics to Monitor

| Metric | Target | Warning | Critical |
|--------|--------|---------|----------|
| Pod Ready | 100% | < 95% | < 90% |
| Cluster Health | 3 nodes | 2 nodes | < 2 nodes |
| Success Rate | ≥ 99% | ≥ 98% | < 98% |
| P95 Latency | < 100ms | < 200ms | > 500ms |
| Error Count | 0/hour | 1-5/hour | > 5/hour |
| Replication Lag | < 10ms | < 50ms | > 100ms |
| Disk Usage | < 60% | < 80% | > 90% |
| Memory Usage | < 60% | < 80% | > 85% |

### Prometheus Configuration

```yaml
# prometheus-rules.yaml
apiVersion: monitoring.coreos.com/v1
kind: PrometheusRule
metadata:
  name: daemon-rules
  namespace: monitoring
spec:
  groups:
  - name: daemon.rules
    interval: 30s
    rules:
    # Alert: Cluster unhealthy
    - alert: DaemonClusterUnhealthy
      expr: count(up{job="daemon"}) < 3
      for: 2m
      annotations:
        summary: "Daemon cluster has fewer than 3 nodes"

    # Alert: High error rate
    - alert: DaemonHighErrorRate
      expr: |
        rate(daemon_operations_failed_total[5m]) > 0.01
      for: 5m
      annotations:
        summary: "Daemon error rate exceeds 1%"

    # Alert: High latency
    - alert: DaemonHighLatency
      expr: |
        histogram_quantile(0.95, daemon_operation_duration_ms) > 100
      for: 5m
      annotations:
        summary: "Daemon P95 latency exceeds 100ms"

    # Alert: Replication lag
    - alert: DaemonReplicationLag
      expr: |
        daemon_raft_replication_lag_ms > 100
      for: 1m
      annotations:
        summary: "Raft replication lag exceeds 100ms"

    # Alert: Disk usage
    - alert: DaemonDiskUsageHigh
      expr: |
        (kubelet_volume_stats_used_bytes / kubelet_volume_stats_capacity_bytes) > 0.9
      for: 5m
      annotations:
        summary: "Daemon disk usage exceeds 90%"
```

### Alert Response Playbooks

**Alert: DaemonClusterUnhealthy**
```bash
#!/bin/bash
# Handle cluster health alert

# 1. Check pod status
kubectl get pods -n unrdf-daemons -o wide

# 2. Get logs from down pods
for pod in $(kubectl get pods -n unrdf-daemons -o name | grep -v Running); do
  echo "=== Logs from $pod ==="
  kubectl logs -n unrdf-daemons $pod --previous
done

# 3. Check Raft state
kubectl exec -n unrdf-daemons unrdf-daemon-0 -- \
  curl -s http://localhost:8080/health/cluster | jq .

# 4. If node is dead, trigger recovery
# (See Common Failure Scenarios below)
```

**Alert: DaemonHighErrorRate**
```bash
#!/bin/bash
# Handle high error rate alert

# 1. Identify error pattern
kubectl logs -n unrdf-daemons unrdf-daemon-0 -f | grep ERROR | head -20

# 2. Check operation queue
curl http://unrdf-daemon-0:8080/queue

# 3. Check dependencies (database, external APIs)
kubectl exec -n unrdf-daemons unrdf-daemon-0 -- \
  curl -I http://database:5432

# 4. If transient, monitor. If persistent, trigger incident.
```

---

## Common Failure Scenarios

### Scenario 1: Single Pod Crash

**Symptoms**: One pod missing, 2 of 3 healthy

**Resolution**:
```bash
# 1. Check logs for crash reason
kubectl logs -n unrdf-daemons unrdf-daemon-0 --previous | tail -50

# 2. Identify failure type
kubectl describe pod -n unrdf-daemons unrdf-daemon-0

# 3. Restart pod
kubectl delete pod -n unrdf-daemons unrdf-daemon-0
# StatefulSet will recreate it
sleep 30

# 4. Verify recovery
kubectl logs -n unrdf-daemons unrdf-daemon-0 | head -20
# Expected: Normal startup logs, cluster rejoin

# 5. Verify cluster health restored
kubectl exec -n unrdf-daemons unrdf-daemon-0 -- \
  curl -s http://localhost:8080/health/cluster | jq .clusterSize
# Expected: 3
```

### Scenario 2: Leader Failure (Leader Election)

**Symptoms**: Cluster is online, operations slow, leader missing

**Resolution**:
```bash
# 1. Identify new leader
kubectl exec -n unrdf-daemons unrdf-daemon-0 -- \
  curl -s http://localhost:8080/health/cluster | jq .leader

# 2. Check election logs
for pod in unrdf-daemon-{0,1,2}; do
  echo "=== Logs from $pod ==="
  kubectl logs -n unrdf-daemons $pod | grep -i "election\|leader" | tail -5
done

# 3. Verify new leader is operational
NEW_LEADER=$(kubectl exec -n unrdf-daemons unrdf-daemon-0 -- \
  curl -s http://localhost:8080/health/cluster | jq -r .leader)

kubectl exec -n unrdf-daemons $NEW_LEADER -- \
  curl -s http://localhost:8080/health/live | jq .

# 4. Wait for replication to catch up
sleep 30
curl http://localhost:8080/health/cluster | jq '.replicationLag'
# Expected: < 50ms
```

### Scenario 3: Disk Full on Raft Storage

**Symptoms**: Operations failing, "no space left on device" errors

**Resolution**:
```bash
# 1. Check disk usage
kubectl exec -n unrdf-daemons unrdf-daemon-0 -- \
  df -h /data/raft/

# 2. Check what's consuming space
kubectl exec -n unrdf-daemons unrdf-daemon-0 -- \
  du -sh /data/raft/* | sort -rh

# 3. If Raft log is huge, trigger snapshot
kubectl exec -n unrdf-daemons unrdf-daemon-0 -- \
  curl -X POST http://localhost:8080/admin/snapshot

# 4. Expand PVC
kubectl patch pvc raft-data-unrdf-daemon-0 -n unrdf-daemons \
  -p '{"spec":{"resources":{"requests":{"storage":"100Gi"}}}}'

# 5. Expand filesystem (if needed)
kubectl exec -n unrdf-daemons unrdf-daemon-0 -- \
  resize2fs /dev/xvdf
```

### Scenario 4: Memory Leak (OOMKilled)

**Symptoms**: Pod crashes with "OOMKilled", operation cache grows unbounded

**Resolution**:
```bash
# 1. Check memory trend
kubectl top pods -n unrdf-daemons --containers

# 2. Check cache size
kubectl exec -n unrdf-daemons unrdf-daemon-0 -- \
  curl -s http://localhost:8080/metrics | jq .cache

# 3. Investigate operation handler leaks
# Look for operations that don't release resources
grep -r "addEventListener\|on(" src/operations/ | \
  grep -v "removeEventListener\|off("

# 4. Increase memory limit temporarily
kubectl set resources statefulset unrdf-daemon \
  -n unrdf-daemons \
  --limits=memory=4Gi

# 5. Schedule restart at low-traffic time
kubectl rollout restart statefulset/unrdf-daemon -n unrdf-daemons

# 6. Reduce cache size if needed
kubectl set env statefulset unrdf-daemon \
  -n unrdf-daemons \
  CACHE_MAX_SIZE=500
```

### Scenario 5: Network Partition (Split Brain)

**Symptoms**: Two clusters operating independently (split brain in Raft)

**Resolution**:
```bash
# 1. Identify partition
for pod in unrdf-daemon-{0,1,2}; do
  echo "=== Status of $pod ==="
  kubectl exec -n unrdf-daemons $pod -- \
    curl -s http://localhost:8080/health/cluster | jq '.leader'
done

# 2. If multiple leaders reported, split brain detected
echo "ALERT: Split brain detected!"

# 3. Identify majority partition
# (In 3-node cluster, whichever has 2+ nodes is correct)

# 4. Isolate minority partition
# Apply NetworkPolicy to block the minority
kubectl apply -f - <<EOF
apiVersion: networking.k8s.io/v1
kind: NetworkPolicy
metadata:
  name: isolate-minority
  namespace: unrdf-daemons
spec:
  podSelector:
    matchExpressions:
    - key: pod-name
      operator: In
      values:
      - unrdf-daemon-2  # Isolate this pod
  policyTypes:
  - Ingress
  - Egress
EOF

# 5. Wait for leader to stabilize (Raft will cut off minority)
sleep 30

# 6. Resolve network issue
# (Fix network, restore connectivity)

# 7. Remove isolation policy
kubectl delete networkpolicy isolate-minority -n unrdf-daemons

# 8. Verify cluster reunification
sleep 60
curl http://localhost:8080/health/cluster | jq .clusterSize
# Expected: 3
```

---

## Performance Tuning

### Baseline Performance

**Measure before optimization**:
```bash
# Get current metrics
curl http://unrdf-daemon-0:8080/metrics | jq '{
  totalOperations,
  avgDuration,
  p95Duration,
  p99Duration,
  successRate,
  activeCount
}'

# Save as baseline
curl http://unrdf-daemon-0:8080/metrics > /tmp/baseline-$(date +%s).json
```

### Tuning Parameters

#### 1. Concurrency Tuning

```javascript
// Test different concurrency levels
const concurrencyLevels = [5, 10, 20, 50];

for (const concurrency of concurrencyLevels) {
  const daemon = new Daemon({ maxConcurrent: concurrency });
  await daemon.start();

  // Run 1000 operations
  const start = Date.now();
  for (let i = 0; i < 1000; i++) {
    daemon.execute('test-op');
  }

  const duration = Date.now() - start;
  const throughput = 1000 / (duration / 1000);

  console.log(`Concurrency: ${concurrency}, Throughput: ${throughput} ops/sec`);
  await daemon.stop();
}
```

**Recommended values**:
- CPU-bound operations: concurrency = CPU cores
- I/O-bound operations: concurrency = CPU cores × 2-4
- Mixed workload: concurrency = CPU cores × 1.5

#### 2. Cache Size Tuning

```bash
# Monitor cache hit rate
curl http://unrdf-daemon-0:8080/metrics | jq '{
  cacheSize,
  cacheHits,
  cacheMisses,
  cacheHitRate
}'

# Adjust cache size based on hit rate
# Goal: > 70% hit rate

# Too small cache (< 50% hit rate):
kubectl set env statefulset unrdf-daemon \
  -n unrdf-daemons \
  CACHE_MAX_SIZE=2000  # Increase 2x

# Too large cache (memory issues):
kubectl set env statefulset unrdf-daemon \
  -n unrdf-daemons \
  CACHE_MAX_SIZE=500   # Decrease 50%
```

#### 3. Raft Tuning

```yaml
# Adjust Raft parameters
apiVersion: v1
kind: ConfigMap
metadata:
  name: daemon-config
  namespace: unrdf-daemons
data:
  daemon-config.json: |
    {
      "raft": {
        "heartbeatIntervalMs": 150,
        "electionTimeoutMs": 1500,
        "snapshotIntervalOps": 100000
      }
    }
```

**Parameter tuning**:
- High-latency network: Increase heartbeat/election timeouts (300ms/3000ms)
- Low-latency network: Decrease timeouts (100ms/1000ms)
- Frequent snapshots: Reduce operations between snapshots (50000)
- Memory constrained: Increase snapshot interval (500000)

### Performance Optimization Checklist

- [ ] Monitor P95 latency (target: < 100ms)
- [ ] Achieve > 99% success rate
- [ ] Keep replication lag < 50ms
- [ ] Maintain > 70% cache hit rate
- [ ] CPU usage < 70% during peak
- [ ] Memory usage stable (no growth)

---

## Version Upgrades

### Pre-Upgrade Checklist

```bash
#!/bin/bash
# pre-upgrade-checks.sh

echo "=== Pre-Upgrade Verification ==="

# 1. Backup current state
./backup-daemon.sh
echo "✓ Backup created"

# 2. Test new version locally
docker run -it --rm \
  -e NODE_ENV=test \
  unrdf-daemon:1.1.0 \
  npm test
echo "✓ Tests pass on new version"

# 3. Check breaking changes
echo "Breaking changes from $(git describe --tags):1.1.0:"
git log $(git describe --tags)..1.1.0 --grep="BREAKING" --oneline

# 4. Verify cluster is healthy before upgrade
kubectl exec -n unrdf-daemons unrdf-daemon-0 -- \
  curl -s http://localhost:8080/health/cluster | jq .
echo "✓ Cluster healthy"

# 5. Drain in-flight operations (optional, for critical upgrades)
# kubectl exec -n unrdf-daemons unrdf-daemon-0 -- \
#   curl -X POST http://localhost:8080/admin/drain
# sleep 60  # Wait for queue to empty
```

### Blue-Green Upgrade Strategy

**Safest approach for production**:

```bash
#!/bin/bash
# blue-green-upgrade.sh

echo "=== Blue-Green Upgrade ==="

# 1. Deploy new version alongside old
kubectl set image statefulset/unrdf-daemon \
  daemon=unrdf-daemon:1.1.0 \
  -n unrdf-daemons \
  --record

# Watch new pods start
kubectl get pods -n unrdf-daemons -w

# 2. Let new pods start and join cluster (5-10 min)
sleep 300

# 3. Verify cluster still has 3 nodes
NODE_COUNT=$(kubectl exec -n unrdf-daemons unrdf-daemon-0 -- \
  curl -s http://localhost:8080/health/cluster | jq .clusterSize)
[ "$NODE_COUNT" = "3" ] && echo "✓ Cluster size OK" || exit 1

# 4. Run smoke tests
for i in {1..10}; do
  curl -X POST http://unrdf-daemon-0:8080/execute/test-op || exit 1
done
echo "✓ Smoke tests pass"

# 5. Monitor metrics
for i in {1..5}; do
  SUCCESS_RATE=$(curl -s http://unrdf-daemon-0:8080/metrics | jq .successRate)
  echo "Success rate check $i: $SUCCESS_RATE%"
  [ "$SUCCESS_RATE" -gt "98" ] || exit 1
  sleep 30
done

echo "✓ Upgrade successful"
```

### Rolling Update (Faster)

For non-breaking changes:

```bash
# Update StatefulSet
kubectl patch statefulset unrdf-daemon \
  -n unrdf-daemons \
  -p '{"spec":{"template":{"spec":{"containers":[{"name":"daemon","image":"unrdf-daemon:1.1.0"}]}}}}'

# Watch rollout
kubectl rollout status statefulset/unrdf-daemon \
  -n unrdf-daemons \
  --timeout=10m

# Verify
kubectl get statefulset -n unrdf-daemons
```

---

## Rollback Procedures

### Quick Rollback (< 5 minutes)

If new version has critical issues:

```bash
#!/bin/bash
# rollback.sh

echo "=== Rolling back to previous version ==="

PREVIOUS_VERSION=$(kubectl rollout history statefulset/unrdf-daemon \
  -n unrdf-daemons | tail -2 | head -1 | awk '{print $1}')

# Trigger rollback
kubectl rollout undo statefulset/unrdf-daemon \
  -n unrdf-daemons \
  --to-revision=$PREVIOUS_VERSION

# Monitor rollback
kubectl rollout status statefulset/unrdf-daemon \
  -n unrdf-daemons \
  --timeout=5m

# Verify
kubectl get pods -n unrdf-daemons
curl http://unrdf-daemon-0:8080/health/cluster
```

### Manual Rollback (If automatic fails)

```bash
# 1. Edit StatefulSet directly
kubectl edit statefulset unrdf-daemon -n unrdf-daemons
# Change image back to previous version

# 2. Delete all pods to force restart
kubectl delete pods -l app=unrdf-daemon -n unrdf-daemons

# 3. Wait for new pods
sleep 60
kubectl get pods -n unrdf-daemons

# 4. Verify cluster
curl http://unrdf-daemon-0:8080/health/cluster
```

### Data Recovery After Rollback

If rollback corrupts Raft state:

```bash
# 1. Restore from backup
./recover-daemon.sh

# 2. Verify cluster startup
kubectl logs -n unrdf-daemons unrdf-daemon-0 | grep "cluster started"

# 3. Verify data integrity
curl http://unrdf-daemon-0:8080/metrics | jq .totalOperations
# Should match backup manifest
```

---

## Operations Quick Reference

### Essential Commands

| Task | Command |
|------|---------|
| Check cluster status | `kubectl exec -n unrdf-daemons unrdf-daemon-0 -- curl -s http://localhost:8080/health/cluster \| jq .` |
| View recent logs | `kubectl logs -n unrdf-daemons unrdf-daemon-0 -f` |
| Get metrics | `kubectl exec -n unrdf-daemons unrdf-daemon-0 -- curl -s http://localhost:8080/metrics \| jq .` |
| Execute operation | `kubectl exec -n unrdf-daemons unrdf-daemon-0 -- curl -X POST http://localhost:8080/execute/OP_ID` |
| Scale cluster | `kubectl scale statefulset unrdf-daemon -n unrdf-daemons --replicas=5` |
| Upgrade version | `kubectl set image statefulset/unrdf-daemon daemon=unrdf-daemon:VERSION -n unrdf-daemons` |
| Rollback | `kubectl rollout undo statefulset/unrdf-daemon -n unrdf-daemons` |
| Restart cluster | `kubectl delete pods -l app=unrdf-daemon -n unrdf-daemons` |

### Alert Response Escalation

```
Page (P1): Cluster down or split brain
  → Immediate: Check pod status, failover to backup cluster

Page (P2): High error rate or high latency
  → 10 min: Investigate logs, check dependencies

Email (P3): Resource warnings or performance degradation
  → 24 hours: Review metrics, plan optimization
```

---

## Summary

### Operational Excellence Checklist

- [ ] Daily health checks running automatically
- [ ] Weekly backups verified restorable
- [ ] Monthly DR drill scheduled
- [ ] All alerts understood and tested
- [ ] Runbooks documented for your team
- [ ] On-call engineer trained on procedures
- [ ] Incident response playbook ready
- [ ] Performance baselines established

### Related Guides

- **[Production Deployment](./production-deployment.md)** - Infrastructure setup
- **[Security Hardening](./security-hardening.md)** - Security best practices
- **[API Reference](./reference.md)** - Configuration reference

### Emergency Contacts

- **On-Call**: #unrdf-oncall Slack
- **Escalation**: @SRE-leads
- **Page**: PagerDuty: unrdf-daemon-alerts
