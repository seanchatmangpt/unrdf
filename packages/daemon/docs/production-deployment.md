# Production Deployment Guide: @unrdf/daemon

Complete guide for deploying and operating @unrdf/daemon in production environments. Covers infrastructure requirements, Kubernetes deployments, Docker optimization, Raft clustering, SSL/TLS, and disaster recovery.

**Audience**: DevOps engineers, platform engineers, SREs
**Time**: 45-60 minutes to read and implement
**Updated**: January 2026

---

## Table of Contents

1. [Infrastructure Requirements](#infrastructure-requirements)
2. [Docker Image Build & Optimization](#docker-image-build--optimization)
3. [Kubernetes Deployment](#kubernetes-deployment)
4. [Raft Cluster Configuration](#raft-cluster-configuration)
5. [SSL/TLS & Network Security](#ssltls--network-security)
6. [Storage & Persistence](#storage--persistence)
7. [Backup & Disaster Recovery](#backup--disaster-recovery)
8. [Capacity Planning](#capacity-planning)
9. [Troubleshooting Deployment](#troubleshooting-deployment)

---

## Infrastructure Requirements

### Minimum Requirements

**Single-Node Daemon** (Development/Testing):
- CPU: 2 cores (x86_64 or ARM64)
- RAM: 512 MB minimum, 1 GB recommended
- Storage: 1 GB for operation logs and receipts
- Network: Standard TCP/IP
- Node.js: 18.0.0+, 20 LTS recommended

**3-Node Cluster** (High Availability):
- Per Node: 4 CPU cores, 2 GB RAM, 10 GB storage
- Network: < 100ms latency between nodes
- Storage: Dedicated disk for Raft state (3-5 IOPS per operation)

**Production Scale** (Multi-region, 100+ operations/sec):
- Per Node: 8+ CPU cores, 4+ GB RAM, 50+ GB storage
- Network: < 50ms latency (same DC), < 500ms (cross-DC)
- Storage: 10,000+ IOPS, separate volumes for logs/state

### CPU Profile

**Typical daemon operations consume**:
- Scheduling/trigger evaluation: ~5-10ms per operation
- Receipt generation (cryptographic): ~20-50ms per operation
- Raft replication: ~10-30ms per consensus round
- Event processing: ~2-5ms per event

**Headroom calculation**:
```
Required CPU cores = (Operations/sec × 50ms) / 1000 + 0.5 (headroom)
Example: 100 ops/sec = (100 × 50) / 1000 + 0.5 = 5.5 cores
```

### Memory Requirements

**LRU Cache baseline**: ~1KB per operation in cache (1000 operations = 1 MB)

**Typical breakdown** (per daemon instance):
- Node.js runtime: 40-60 MB
- Daemon core structures: 10-20 MB
- Operation cache (1000 entries): 1 MB
- Raft state machine: 5-10 MB
- Event listeners: 5-10 MB
- **Total per daemon**: ~80-120 MB baseline

**Growth factors**:
- Each additional 1000 cached operations: +1 MB
- Each additional trigger pattern: +100 KB
- Each active listener: +50 KB

**Formula**:
```
RAM = 120 MB (baseline) + (cached_ops / 1000) + listeners × 0.05 MB
```

### Storage Requirements

**Operation State** (persistent):
- Raft log: 1-5 KB per operation
- Receipt storage: 2-3 KB per receipt (including proofs)
- Snapshot: ~20% of log size, taken every 100K entries

**Example**:
```
100K operations × 4 KB per entry = 400 MB log
Plus snapshots and overhead: ~500 MB total
```

**Retention Policy** (recommended):
- Hot receipts (last 30 days): SSD
- Warm receipts (30-90 days): HDD or object storage
- Cold receipts (>90 days): Archive storage

---

## Docker Image Build & Optimization

### Multi-stage Dockerfile

```dockerfile
# Stage 1: Build
FROM node:20-alpine AS builder

WORKDIR /build

# Install pnpm
RUN npm install -g pnpm@8

# Copy package files
COPY pnpm-workspace.yaml pnpm-lock.yaml ./
COPY packages/daemon ./packages/daemon

# Install dependencies and build
RUN pnpm install --frozen-lockfile
RUN pnpm -C packages/daemon build || true

# Stage 2: Runtime
FROM node:20-alpine

WORKDIR /app

# Install dumb-init for proper signal handling
RUN apk add --no-cache dumb-init

# Install pnpm in runtime
RUN npm install -g pnpm@8

# Copy built artifacts from builder
COPY --from=builder /build/packages/daemon ./

# Install production dependencies only
RUN pnpm install --prod --frozen-lockfile

# Create non-root user
RUN addgroup -g 1000 daemon && \
    adduser -D -u 1000 -G daemon daemon && \
    chown -R daemon:daemon /app

USER daemon

# Health check
HEALTHCHECK --interval=30s --timeout=5s --start-period=10s --retries=3 \
  CMD node -e "require('http').get('http://localhost:8080/health', (r) => process.exit(r.statusCode === 200 ? 0 : 1))"

# Use dumb-init to handle signals properly
ENTRYPOINT ["/sbin/dumb-init", "--"]

# Default command
CMD ["node", "src/daemon.mjs"]

# Metadata
LABEL org.unrdf.component="daemon" \
      org.unrdf.version="1.0.0" \
      org.opencontainers.image.description="UNRDF background daemon for scheduled tasks and event-driven operations"
```

### Image Optimization Checklist

**Size Optimization**:
- Alpine Linux base: ~5 MB (vs 150 MB+ for full Linux)
- Single-stage build artifact pruning: Remove dev dependencies
- Layer caching: Leverage Docker layer caching for faster builds

**Build command**:
```bash
docker build \
  --target runtime \
  --build-arg NODE_ENV=production \
  -t unrdf-daemon:1.0.0 \
  -t unrdf-daemon:latest \
  -f Dockerfile .
```

**Resulting image size**: 120-150 MB (vs 400+ MB without optimization)

### Image Verification

```bash
# Inspect image layers
docker history unrdf-daemon:latest

# Scan for vulnerabilities (requires Trivy)
trivy image unrdf-daemon:latest

# Verify image works
docker run --rm unrdf-daemon:latest \
  node -e "console.log('Image OK')"
```

---

## Kubernetes Deployment

### Namespace & RBAC Setup

```yaml
# 01-namespace.yaml
apiVersion: v1
kind: Namespace
metadata:
  name: unrdf-daemons
  labels:
    app.kubernetes.io/name: unrdf
    app.kubernetes.io/component: daemon

---
# 02-rbac.yaml
apiVersion: v1
kind: ServiceAccount
metadata:
  name: daemon-sa
  namespace: unrdf-daemons

---
apiVersion: rbac.authorization.k8s.io/v1
kind: ClusterRole
metadata:
  name: daemon-reader
rules:
- apiGroups: [""]
  resources: ["configmaps", "secrets"]
  verbs: ["get", "list", "watch"]
- apiGroups: [""]
  resources: ["pods"]
  verbs: ["get", "list"]

---
apiVersion: rbac.authorization.k8s.io/v1
kind: ClusterRoleBinding
metadata:
  name: daemon-reader-binding
roleRef:
  apiGroup: rbac.authorization.k8s.io
  kind: ClusterRole
  name: daemon-reader
subjects:
- kind: ServiceAccount
  name: daemon-sa
  namespace: unrdf-daemons
```

### ConfigMap & Secrets

```yaml
# 03-config.yaml
apiVersion: v1
kind: ConfigMap
metadata:
  name: daemon-config
  namespace: unrdf-daemons
data:
  daemon-config.json: |
    {
      "maxConcurrent": 10,
      "healthCheckIntervalMs": 30000,
      "metricsRetentionMs": 3600000,
      "logLevel": "info",
      "raft": {
        "enabled": true,
        "minClusterSize": 3,
        "heartbeatIntervalMs": 150,
        "electionTimeoutMs": 1500
      }
    }

---
# 04-secrets.yaml
apiVersion: v1
kind: Secret
metadata:
  name: daemon-secrets
  namespace: unrdf-daemons
type: Opaque
stringData:
  tls-cert: |
    -----BEGIN CERTIFICATE-----
    [base64-encoded-cert]
    -----END CERTIFICATE-----
  tls-key: |
    -----BEGIN PRIVATE KEY-----
    [base64-encoded-key]
    -----END PRIVATE KEY-----
  api-auth-token: "production-auth-token-here"
```

### StatefulSet Deployment

```yaml
# 05-deployment.yaml
apiVersion: apps/v1
kind: StatefulSet
metadata:
  name: unrdf-daemon
  namespace: unrdf-daemons
  labels:
    app: unrdf-daemon
    version: "1.0.0"
spec:
  serviceName: unrdf-daemon
  replicas: 3

  selector:
    matchLabels:
      app: unrdf-daemon

  template:
    metadata:
      labels:
        app: unrdf-daemon
        version: "1.0.0"
      annotations:
        prometheus.io/scrape: "true"
        prometheus.io/port: "8080"
        prometheus.io/path: "/metrics"

    spec:
      serviceAccountName: daemon-sa

      # Pod disruption budget for HA
      affinity:
        podAntiAffinity:
          preferredDuringSchedulingIgnoredDuringExecution:
          - weight: 100
            podAffinityTerm:
              labelSelector:
                matchExpressions:
                - key: app
                  operator: In
                  values:
                  - unrdf-daemon
              topologyKey: kubernetes.io/hostname

      # Init container for Raft state setup
      initContainers:
      - name: raft-init
        image: busybox:1.35
        command: ['sh', '-c', 'mkdir -p /data/raft && chmod 0700 /data/raft']
        volumeMounts:
        - name: raft-data
          mountPath: /data

      # Main daemon container
      containers:
      - name: daemon
        image: unrdf-daemon:1.0.0
        imagePullPolicy: IfNotPresent

        ports:
        - name: http
          containerPort: 8080
          protocol: TCP
        - name: raft
          containerPort: 8081
          protocol: TCP

        env:
        - name: NODE_ENV
          value: "production"
        - name: DAEMON_ID
          valueFrom:
            fieldRef:
              fieldPath: metadata.name
        - name: NODE_ID
          valueFrom:
            fieldRef:
              fieldPath: status.podIP
        - name: CLUSTER_ID
          value: "production-cluster-1"
        - name: POD_NAME
          valueFrom:
            fieldRef:
              fieldPath: metadata.name
        - name: POD_NAMESPACE
          valueFrom:
            fieldRef:
              fieldPath: metadata.namespace
        - name: LOG_LEVEL
          value: "info"

        # Configuration from ConfigMap
        volumeMounts:
        - name: config
          mountPath: /etc/daemon
          readOnly: true

        # TLS secrets
        - name: tls
          mountPath: /etc/daemon/tls
          readOnly: true

        # Persistent Raft state
        - name: raft-data
          mountPath: /data/raft

        # Resource limits
        resources:
          requests:
            cpu: "2"
            memory: "1Gi"
          limits:
            cpu: "4"
            memory: "2Gi"

        # Readiness probe (daemon is ready to accept connections)
        readinessProbe:
          httpGet:
            path: /health/ready
            port: http
            scheme: HTTP
          initialDelaySeconds: 10
          periodSeconds: 10
          timeoutSeconds: 5
          successThreshold: 1
          failureThreshold: 3

        # Liveness probe (daemon is still running)
        livenessProbe:
          httpGet:
            path: /health/live
            port: http
            scheme: HTTP
          initialDelaySeconds: 30
          periodSeconds: 30
          timeoutSeconds: 5
          successThreshold: 1
          failureThreshold: 3

        # Startup probe (wait for daemon to become ready)
        startupProbe:
          httpGet:
            path: /health/startup
            port: http
            scheme: HTTP
          initialDelaySeconds: 0
          periodSeconds: 5
          timeoutSeconds: 5
          successThreshold: 1
          failureThreshold: 24  # 2 minutes total

        # Graceful shutdown
        lifecycle:
          preStop:
            exec:
              command: ["/bin/sh", "-c", "sleep 15 && curl -X POST http://localhost:8080/shutdown || true"]

        # Security context
        securityContext:
          runAsNonRoot: true
          runAsUser: 1000
          allowPrivilegeEscalation: false
          readOnlyRootFilesystem: true
          capabilities:
            drop:
            - ALL
            add:
            - NET_BIND_SERVICE

      # Volumes
      volumes:
      - name: config
        configMap:
          name: daemon-config
      - name: tls
        secret:
          secretName: daemon-secrets
          defaultMode: 0400

      # DNS policy for pod-to-pod communication
      dnsPolicy: ClusterFirst
      restartPolicy: Always
      terminationGracePeriodSeconds: 30

  # Persistent volume for Raft state
  volumeClaimTemplates:
  - metadata:
      name: raft-data
    spec:
      accessModes: ["ReadWriteOnce"]
      storageClassName: fast-ssd
      resources:
        requests:
          storage: 50Gi

---
# 06-service.yaml
apiVersion: v1
kind: Service
metadata:
  name: unrdf-daemon
  namespace: unrdf-daemons
  labels:
    app: unrdf-daemon
spec:
  clusterIP: None  # Headless service for StatefulSet
  selector:
    app: unrdf-daemon
  ports:
  - name: http
    port: 8080
    targetPort: 8080
  - name: raft
    port: 8081
    targetPort: 8081
  publishNotReadyAddresses: true  # Allow discovery before ready

---
# 07-ingress.yaml
apiVersion: networking.k8s.io/v1
kind: Ingress
metadata:
  name: daemon-ingress
  namespace: unrdf-daemons
  annotations:
    cert-manager.io/cluster-issuer: "letsencrypt-prod"
    nginx.ingress.kubernetes.io/ssl-redirect: "true"
spec:
  ingressClassName: nginx
  tls:
  - hosts:
    - daemon.example.com
    secretName: daemon-tls-cert
  rules:
  - host: daemon.example.com
    http:
      paths:
      - path: /
        pathType: Prefix
        backend:
          service:
            name: unrdf-daemon
            port:
              number: 8080

---
# 08-pdb.yaml (Pod Disruption Budget)
apiVersion: policy/v1
kind: PodDisruptionBudget
metadata:
  name: daemon-pdb
  namespace: unrdf-daemons
spec:
  minAvailable: 2
  selector:
    matchLabels:
      app: unrdf-daemon
```

### Deployment Verification

```bash
# Deploy to cluster
kubectl apply -f 01-namespace.yaml
kubectl apply -f 02-rbac.yaml
kubectl apply -f 03-config.yaml
kubectl apply -f 04-secrets.yaml
kubectl apply -f 05-deployment.yaml
kubectl apply -f 06-service.yaml
kubectl apply -f 07-ingress.yaml
kubectl apply -f 08-pdb.yaml

# Verify deployment
kubectl get statefulset -n unrdf-daemons
kubectl get pods -n unrdf-daemons
kubectl logs -n unrdf-daemons unrdf-daemon-0

# Check pod readiness
kubectl get pods -n unrdf-daemons -o wide

# Port forward for testing
kubectl port-forward -n unrdf-daemons svc/unrdf-daemon 8080:8080

# Test health endpoint
curl http://localhost:8080/health
```

---

## Raft Cluster Configuration

### Cluster Bootstrap

**Initial cluster formation**:
```bash
# On node-0 (becomes initial leader)
export CLUSTER_NODES="node-0:8081,node-1:8081,node-2:8081"
export NODE_ID="node-0"
node src/daemon.mjs --raft-enabled --cluster-nodes=$CLUSTER_NODES --node-id=$NODE_ID

# On node-1
export CLUSTER_NODES="node-0:8081,node-1:8081,node-2:8081"
export NODE_ID="node-1"
node src/daemon.mjs --raft-enabled --cluster-nodes=$CLUSTER_NODES --node-id=$NODE_ID

# On node-2
export CLUSTER_NODES="node-0:8081,node-1:8081,node-2:8081"
export NODE_ID="node-2"
node src/daemon.mjs --raft-enabled --cluster-nodes=$CLUSTER_NODES --node-id=$NODE_ID
```

### Configuration Parameters

| Parameter | Default | Recommended | Notes |
|-----------|---------|-------------|-------|
| `raftEnabled` | false | true | Enable Raft consensus |
| `heartbeatIntervalMs` | 150 | 150 | Time between leader heartbeats |
| `electionTimeoutMs` | 1500 | 1500 | Timeout before new election |
| `minClusterSize` | 1 | 3 | Minimum nodes for consensus |
| `snapshotIntervalOps` | 100000 | 100000 | Operations before snapshot |
| `maxLogSize` | 1000000 | 1000000 | Max Raft log entries in memory |

### Health Monitoring

```javascript
// Get Raft cluster status
const health = daemon.getHealth();
console.log({
  isLeader: health.isLeader,
  clusterSize: health.clusterSize,
  replicationLag: health.replicationLag,  // in ms
  lastHeartbeat: health.lastHeartbeat,
  commitIndex: health.commitIndex,
});

// Monitor replication
daemon.on('raft:replication-complete', (event) => {
  console.log(`Replicated to ${event.nodeCount}/${event.clusterSize} nodes`);
});
```

---

## SSL/TLS & Network Security

### Certificate Generation

```bash
# Generate self-signed certificate for testing
openssl req -x509 -newkey rsa:4096 -keyout daemon-key.pem -out daemon-cert.pem -days 365 -nodes \
  -subj "/CN=unrdf-daemon.example.com"

# Production: Use Let's Encrypt
certbot certonly --standalone -d daemon.example.com

# Store in Kubernetes secret
kubectl create secret tls daemon-tls \
  --cert=daemon-cert.pem \
  --key=daemon-key.pem \
  -n unrdf-daemons
```

### Daemon Configuration

```javascript
import fs from 'fs';

const daemonConfig = {
  // ... other config
  tls: {
    enabled: true,
    cert: fs.readFileSync('/etc/daemon/tls/daemon-cert.pem'),
    key: fs.readFileSync('/etc/daemon/tls/daemon-key.pem'),
    // Mutual TLS (mTLS)
    ca: fs.readFileSync('/etc/daemon/tls/ca-cert.pem'),
    requestCert: true,
    rejectUnauthorized: true,
  },
};
```

### Network Policies

```yaml
# network-policy.yaml
apiVersion: networking.k8s.io/v1
kind: NetworkPolicy
metadata:
  name: daemon-network-policy
  namespace: unrdf-daemons
spec:
  podSelector:
    matchLabels:
      app: unrdf-daemon

  # Deny all ingress by default
  policyTypes:
  - Ingress
  - Egress

  ingress:
  # Allow from other daemon pods (Raft clustering)
  - from:
    - podSelector:
        matchLabels:
          app: unrdf-daemon
    ports:
    - protocol: TCP
      port: 8080  # HTTP
    - protocol: TCP
      port: 8081  # Raft

  # Allow from monitoring
  - from:
    - namespaceSelector:
        matchLabels:
          name: monitoring
    ports:
    - protocol: TCP
      port: 8080

  egress:
  # Allow DNS
  - to:
    - namespaceSelector: {}
      podSelector:
        matchLabels:
          k8s-app: kube-dns
    ports:
    - protocol: UDP
      port: 53

  # Allow outbound to database/storage
  - to:
    - namespaceSelector:
        matchLabels:
          name: data-layer
    ports:
    - protocol: TCP
      port: 5432

  # Allow to other daemon pods
  - to:
    - podSelector:
        matchLabels:
          app: unrdf-daemon
    ports:
    - protocol: TCP
      port: 8080
    - protocol: TCP
      port: 8081
```

---

## Storage & Persistence

### Persistent Volume Setup

**Requirements**:
- Type: SSD for Raft state (write-heavy operations)
- IOPS: ≥1000 (3 nodes × 300+ IOPS each)
- Replication: At least 2x (RAID-1 or cloud equivalent)
- Backup: Daily snapshots

**StorageClass configuration**:
```yaml
apiVersion: storage.k8s.io/v1
kind: StorageClass
metadata:
  name: fast-ssd
provisioner: kubernetes.io/aws-ebs  # or your storage provisioner
parameters:
  type: gp3
  iops: "3000"
  throughput: "125"
  encrypted: "true"
reclaimPolicy: Retain
allowVolumeExpansion: true
```

### Raft State Backup

```bash
#!/bin/bash
# backup-raft-state.sh

BACKUP_DIR="/backups/raft"
DAEMON_POD="unrdf-daemon-0"
NAMESPACE="unrdf-daemons"

mkdir -p $BACKUP_DIR

# Take backup (requires daemon shutdown or read-only mode)
kubectl exec -n $NAMESPACE $DAEMON_POD -- \
  tar czf - /data/raft | \
  gzip > $BACKUP_DIR/raft-backup-$(date +%Y%m%d-%H%M%S).tar.gz

# Verify backup
gzip -t $BACKUP_DIR/*.tar.gz && echo "Backups verified OK"

# Cleanup old backups (keep 30 days)
find $BACKUP_DIR -name "*.tar.gz" -mtime +30 -delete
```

---

## Backup & Disaster Recovery

### Backup Strategy

**RPO (Recovery Point Objective)**: 15 minutes
**RTO (Recovery Time Objective)**: 30 minutes

**Three-tier backup**:

1. **Continuous Replication** (< 1 second):
   - Raft log replicated to all 3 cluster nodes
   - Loss tolerance: 1 node failure

2. **Hourly Snapshots** (15-minute RPO):
   - Full Raft state snapshots to S3 or equivalent
   - Lifecycle: 30 days hot, 1 year archive

3. **Cross-Region Backup** (hourly):
   - Asynchronous replication to secondary region
   - Enables multi-region failover

### Backup Implementation

```bash
#!/bin/bash
# backup-daemon.sh - Run hourly via cron

export AWS_REGION=us-east-1
BACKUP_BUCKET="s3://unrdf-daemon-backups"
DAEMON_NAMESPACE="unrdf-daemons"
DATE=$(date +%Y-%m-%d/%H-%M-%S)

# Backup each Raft volume
for POD in unrdf-daemon-{0,1,2}; do
  kubectl exec -n $DAEMON_NAMESPACE $POD -- \
    tar czf - /data/raft | \
    aws s3 cp - "$BACKUP_BUCKET/$POD/$DATE/raft.tar.gz"
done

# Upload metadata
cat > /tmp/backup-manifest.json <<EOF
{
  "timestamp": "$(date -u +%Y-%m-%dT%H:%M:%SZ)",
  "cluster": "production-cluster-1",
  "nodes": ["unrdf-daemon-0", "unrdf-daemon-1", "unrdf-daemon-2"],
  "status": "completed"
}
EOF

aws s3 cp /tmp/backup-manifest.json \
  "$BACKUP_BUCKET/manifests/$DATE/manifest.json"
```

### Disaster Recovery Procedure

**Scenario**: Complete cluster loss (all 3 nodes fail simultaneously)

**Recovery steps** (30-minute RTO):

1. **Restore from latest snapshot** (5 min):
```bash
# Download latest backup
aws s3 ls s3://unrdf-daemon-backups/manifests/ --recursive | tail -1
aws s3 cp s3://unrdf-daemon-backups/unrdf-daemon-0/2026-01-10/raft.tar.gz - | \
  tar xzf - -C /tmp

# Restore Raft state
ls /tmp/data/raft/
cp -r /tmp/data/raft/* /data/raft/
```

2. **Initialize new cluster from state** (10 min):
```bash
# Boot new daemon pods with restored state
kubectl apply -f new-cluster-manifests.yaml

# Monitor recovery
kubectl logs -n unrdf-daemons unrdf-daemon-0 -f
```

3. **Verify cluster integrity** (5 min):
```bash
# Check all nodes operational
for NODE in {0,1,2}; do
  curl http://unrdf-daemon-$NODE:8080/health/cluster
done

# Verify operation count matches backup
BACKUP_OP_COUNT=$(jq .operationCount /tmp/manifest.json)
CURRENT_OP_COUNT=$(curl http://unrdf-daemon-0:8080/metrics | grep operation_count)
```

4. **Replay any lost operations** (10 min):
```bash
# If recovery point is 15 min old, replay recent operations
curl -X POST http://api.example.com/daemon/replay \
  -d '{"since": "'$(date -d '-15 min' -Iseconds)'"}' \
  -H "Content-Type: application/json"
```

### Testing Disaster Recovery

**Monthly DR drill**:
```bash
#!/bin/bash
# dr-drill.sh - Execute monthly

# 1. Full cluster backup
./backup-daemon.sh

# 2. Destroy cluster
kubectl delete statefulset unrdf-daemon -n unrdf-daemons

# 3. Recover from backup
./recover-daemon.sh

# 4. Verify all operations recovered
EXPECTED=50000
ACTUAL=$(curl http://unrdf-daemon-0:8080/metrics | jq .totalOperations)
if [ "$EXPECTED" == "$ACTUAL" ]; then
  echo "DR drill PASSED"
  exit 0
else
  echo "DR drill FAILED: expected $EXPECTED, got $ACTUAL"
  exit 1
fi
```

---

## Capacity Planning

### Growth Projections

**Estimate resources** for your use case:

```
Q1 2026: 100 operations/day → 5 ops/hour
  - Daemon instances: 1
  - Storage: 1 GB
  - CPU/RAM: < 0.5 CPU, 256 MB

Q2 2026: 1,000 operations/day → 50 ops/hour
  - Daemon instances: 2-3 (HA)
  - Storage: 10 GB
  - CPU/RAM: 1 CPU, 1 GB per instance

Q4 2026: 100,000 operations/day → 4,000+ ops/hour
  - Daemon instances: 5-7 (sharded)
  - Storage: 500+ GB
  - CPU/RAM: 4 CPU, 2-4 GB per instance
```

### Horizontal Scaling

When single 3-node cluster hits limits:

**Option 1: Shard by operation type**:
```yaml
# Deploy separate daemon cluster per operation family
unrdf-daemon-email:          # Email operations
  replicas: 3
  resource:
    cpu: 2
    memory: 1Gi

unrdf-daemon-data:           # Data processing
  replicas: 5
  resource:
    cpu: 4
    memory: 2Gi

unrdf-daemon-sync:           # Synchronization
  replicas: 3
  resource:
    cpu: 2
    memory: 1Gi
```

**Option 2: Geographic sharding**:
```yaml
# Deploy daemon clusters per region
unrdf-daemon-us-east:
  region: us-east-1
  replicas: 3

unrdf-daemon-eu-west:
  region: eu-west-1
  replicas: 3

unrdf-daemon-ap-southeast:
  region: ap-southeast-1
  replicas: 3
```

---

## Troubleshooting Deployment

### Common Issues

**Issue: Pods stuck in Pending**
```bash
# Check resource availability
kubectl describe node | grep -A5 "Allocated resources"

# Check PVC binding
kubectl get pvc -n unrdf-daemons

# Solution: Add nodes or increase resource requests
```

**Issue: High Raft replication latency**
```bash
# Check network latency
kubectl exec -n unrdf-daemons unrdf-daemon-0 -- \
  curl http://unrdf-daemon-1:8081/ping

# Check pod distribution
kubectl get pods -n unrdf-daemons -o wide

# Solution: Use pod anti-affinity, fix network issues
```

**Issue: OOMKilled (out of memory)**
```bash
# Check actual memory usage
kubectl top pods -n unrdf-daemons

# Check operation cache size
curl http://unrdf-daemon-0:8080/metrics | jq .cache

# Solution: Increase memory requests/limits or reduce cache size
```

### Debug Commands

```bash
# Full daemon logs with context
kubectl logs -n unrdf-daemons unrdf-daemon-0 --timestamps=true -f

# Metrics endpoint
curl http://unrdf-daemon-0:8080/metrics | jq .

# Health checks
curl http://unrdf-daemon-0:8080/health/live
curl http://unrdf-daemon-0:8080/health/ready
curl http://unrdf-daemon-0:8080/health/cluster

# Recent operations (last 100)
curl http://unrdf-daemon-0:8080/operations?limit=100 | jq .

# Cluster state
kubectl exec -n unrdf-daemons unrdf-daemon-0 -- \
  node -e "
    const d = require('@unrdf/daemon');
    d.getClusterStatus().then(c => console.log(JSON.stringify(c, null, 2)));
  "
```

---

## Summary & Next Steps

### Deployment Checklist

- [ ] Infrastructure provisioned (CPU, RAM, storage)
- [ ] Docker image built and tested
- [ ] Kubernetes manifests configured
- [ ] TLS certificates generated and deployed
- [ ] Backup/DR procedures tested
- [ ] Network policies configured
- [ ] Monitoring and alerting set up
- [ ] First cluster deployment successful
- [ ] All 3 nodes healthy and replicating
- [ ] Operations executing successfully

### Related Guides

- **[Operational Runbooks](./operational-runbooks.md)** - Day-2 operations
- **[Security Hardening](./security-hardening.md)** - Security best practices
- **[API Reference](./reference.md)** - Daemon configuration options
- **[How-To Guides](./how-to.md)** - Common tasks

### Support & Resources

- GitHub Issues: https://github.com/unrdf/unrdf/issues
- Documentation: https://unrdf.org/docs
- Community Slack: https://unrdf.slack.com
