# Docker Deployment

This guide explains how to deploy Knowd using Docker and container orchestration platforms.

## Docker Basics

### Building Docker Images

**Multi-stage build:**
```dockerfile
# Dockerfile
FROM golang:1.22-alpine AS builder

# Install build dependencies
RUN apk add --no-cache git ca-certificates tzdata

WORKDIR /app
COPY go.mod go.sum ./
RUN go mod download

# Copy source code
COPY . .

# Build the binary
RUN CGO_ENABLED=0 GOOS=linux go build -a -installsuffix cgo -o knowd ./cmd/knowd

# Final stage
FROM alpine:latest

# Install runtime dependencies
RUN apk --no-cache add ca-certificates tzdata

# Create knowd user
RUN addgroup -g 1000 knowd && \
    adduser -D -s /bin/sh -u 1000 -G knowd knowd

# Copy binary
COPY --from=builder /app/knowd /usr/local/bin/knowd

# Set ownership
RUN chown knowd:knowd /usr/local/bin/knowd

# Switch to non-root user
USER knowd

# Expose port
EXPOSE 8090

# Health check
HEALTHCHECK --interval=30s --timeout=10s --start-period=5s --retries=3 \
  CMD wget --no-verbose --tries=1 --spider http://localhost:8090/healthz || exit 1

# Run the application
CMD ["knowd", "-addr=:8090"]
```

**Build and run:**
```bash
# Build image
docker build -t knowd:latest .

# Run container
docker run -d \
  --name knowd \
  -p 8090:8090 \
  -v knowd-data:/var/lib/knowd \
  knowd:latest
```

### Docker Compose

**Single node deployment:**
```yaml
# docker-compose.yml
version: '3.8'
services:
  knowd:
    build: .
    ports:
      - "8090:8090"
    environment:
      - KNOWD_ADDR=:8090
      - KNOWD_DATA_DIR=/var/lib/knowd
      - KNOWD_STORE=disk
      - KNOWD_LOG_LEVEL=info
    volumes:
      - knowd-data:/var/lib/knowd
      - ./config:/etc/knowd:ro
    restart: unless-stopped
    healthcheck:
      test: ["CMD", "wget", "--no-verbose", "--tries=1", "--spider", "http://localhost:8090/healthz"]
      interval: 30s
      timeout: 10s
      retries: 3

volumes:
  knowd-data:
```

**Clustered deployment:**
```yaml
# docker-compose.cluster.yml
version: '3.8'
services:
  knowd-leader:
    image: knowd:latest
    command: knowd -cluster-mode=leader -peer-addrs=knowd-follower1:8091,knowd-follower2:8092
    ports:
      - "8090:8090"
    environment:
      - KNOWD_MTLS_CERT=/etc/knowd/certs/leader.crt
      - KNOWD_MTLS_KEY=/etc/knowd/certs/leader.key
      - KNOWD_MTLS_CA=/etc/knowd/certs/ca.crt
    volumes:
      - knowd-leader-data:/var/lib/knowd
      - ./certs:/etc/knowd/certs:ro
    networks:
      - knowd-cluster

  knowd-follower1:
    image: knowd:latest
    command: knowd -cluster-mode=follower -peer-addrs=knowd-leader:8090
    environment:
      - KNOWD_MTLS_CERT=/etc/knowd/certs/follower1.crt
      - KNOWD_MTLS_KEY=/etc/knowd/certs/follower1.key
      - KNOWD_MTLS_CA=/etc/knowd/certs/ca.crt
    volumes:
      - knowd-follower1-data:/var/lib/knowd
      - ./certs:/etc/knowd/certs:ro
    networks:
      - knowd-cluster

  knowd-follower2:
    image: knowd:latest
    command: knowd -cluster-mode=follower -peer-addrs=knowd-leader:8090
    environment:
      - KNOWD_MTLS_CERT=/etc/knowd/certs/follower2.crt
      - KNOWD_MTLS_KEY=/etc/knowd/certs/follower2.key
      - KNOWD_MTLS_CA=/etc/knowd/certs/ca.crt
    volumes:
      - knowd-follower2-data:/var/lib/knowd
      - ./certs:/etc/knowd/certs:ro
    networks:
      - knowd-cluster

volumes:
  knowd-leader-data:
  knowd-follower1-data:
  knowd-follower2-data:

networks:
  knowd-cluster:
    driver: bridge
```

## Kubernetes Deployment

### Namespace and RBAC

**Namespace creation:**
```bash
kubectl create namespace knowd
kubectl config set-context --current --namespace=knowd
```

**RBAC configuration:**
```yaml
# rbac.yaml
apiVersion: v1
kind: ServiceAccount
metadata:
  name: knowd
  namespace: knowd

---
apiVersion: rbac.authorization.k8s.io/v1
kind: ClusterRole
metadata:
  name: knowd
rules:
- apiGroups: [""]
  resources: ["pods", "services", "endpoints"]
  verbs: ["get", "list", "watch"]

---
apiVersion: rbac.authorization.k8s.io/v1
kind: ClusterRoleBinding
metadata:
  name: knowd
roleRef:
  apiGroup: rbac.authorization.k8s.io
  kind: ClusterRole
  name: knowd
subjects:
- kind: ServiceAccount
  name: knowd
  namespace: knowd
```

### StatefulSet Deployment

**Leader node:**
```yaml
# leader-statefulset.yaml
apiVersion: apps/v1
kind: StatefulSet
metadata:
  name: knowd-leader
  namespace: knowd
spec:
  serviceName: knowd-leader
  replicas: 1
  selector:
    matchLabels:
      app: knowd
      role: leader
  template:
    metadata:
      labels:
        app: knowd
        role: leader
    spec:
      serviceAccountName: knowd
      containers:
      - name: knowd
        image: knowd:latest
        ports:
        - containerPort: 8090
          name: http
        - containerPort: 8091
          name: grpc
        env:
        - name: KNOWD_CLUSTER_MODE
          value: "leader"
        - name: KNOWD_PEER_ADDRS
          value: "knowd-follower-0.knowd-follower:8090"
        - name: KNOWD_MTLS_CERT
          value: "/etc/knowd/certs/tls.crt"
        - name: KNOWD_MTLS_KEY
          value: "/etc/knowd/certs/tls.key"
        - name: KNOWD_MTLS_CA
          value: "/etc/knowd/certs/ca.crt"
        volumeMounts:
        - name: data
          mountPath: /var/lib/knowd
        - name: certs
          mountPath: /etc/knowd/certs
          readOnly: true
      volumes:
      - name: certs
        secret:
          secretName: knowd-tls
  volumeClaimTemplates:
  - metadata:
      name: data
    spec:
      accessModes: ["ReadWriteOnce"]
      resources:
        requests:
          storage: 100Gi
```

**Follower nodes:**
```yaml
# follower-statefulset.yaml
apiVersion: apps/v1
kind: StatefulSet
metadata:
  name: knowd-follower
  namespace: knowd
spec:
  serviceName: knowd-follower
  replicas: 2
  selector:
    matchLabels:
      app: knowd
      role: follower
  template:
    metadata:
      labels:
        app: knowd
        role: follower
    spec:
      serviceAccountName: knowd
      containers:
      - name: knowd
        image: knowd:latest
        ports:
        - containerPort: 8090
          name: http
        env:
        - name: KNOWD_CLUSTER_MODE
          value: "follower"
        - name: KNOWD_PEER_ADDRS
          value: "knowd-leader-0.knowd-leader:8090"
        - name: KNOWD_MTLS_CERT
          value: "/etc/knowd/certs/tls.crt"
        - name: KNOWD_MTLS_KEY
          value: "/etc/knowd/certs/tls.key"
        - name: KNOWD_MTLS_CA
          value: "/etc/knowd/certs/ca.crt"
        volumeMounts:
        - name: data
          mountPath: /var/lib/knowd
        - name: certs
          mountPath: /etc/knowd/certs
          readOnly: true
      volumes:
      - name: certs
        secret:
          secretName: knowd-tls
  volumeClaimTemplates:
  - metadata:
      name: data
    spec:
      accessModes: ["ReadWriteOnce"]
      resources:
        requests:
          storage: 100Gi
```

### Services

**Headless services for cluster communication:**
```yaml
# services.yaml
apiVersion: v1
kind: Service
metadata:
  name: knowd-leader
  namespace: knowd
spec:
  clusterIP: None
  selector:
    app: knowd
    role: leader

---
apiVersion: v1
kind: Service
metadata:
  name: knowd-follower
  namespace: knowd
spec:
  clusterIP: None
  selector:
    app: knowd
    role: follower

---
apiVersion: v1
kind: Service
metadata:
  name: knowd-loadbalancer
  namespace: knowd
spec:
  type: LoadBalancer
  ports:
  - name: http
    port: 80
    targetPort: 8090
  - name: grpc
    port: 8091
    targetPort: 8091
  selector:
    app: knowd
```

### ConfigMaps and Secrets

**Configuration:**
```yaml
# configmap.yaml
apiVersion: v1
kind: ConfigMap
metadata:
  name: knowd-config
  namespace: knowd
data:
  KNOWD_ADDR: ":8090"
  KNOWD_STORE: "disk"
  KNOWD_SNAPSHOT_SEC: "300"
  KNOWD_LOG_LEVEL: "info"
```

**TLS certificates:**
```yaml
# secret.yaml
apiVersion: v1
kind: Secret
metadata:
  name: knowd-tls
  namespace: knowd
type: kubernetes.io/tls
data:
  tls.crt: <base64-encoded-certificate>
  tls.key: <base64-encoded-private-key>
  ca.crt: <base64-encoded-ca-certificate>
```

## Helm Charts

### Installation

**Add Helm repository:**
```bash
helm repo add knowd https://charts.unrdf.org
helm repo update
```

**Install with default values:**
```bash
helm install knowd knowd/knowd
```

**Install with custom values:**
```bash
helm install knowd knowd/knowd \
  --set replicaCount=3 \
  --set image.tag=v1.0.0 \
  --set persistence.size=100Gi
```

### Custom Values

**values.yaml:**
```yaml
# Replica configuration
replicaCount: 3

# Image configuration
image:
  repository: unrdf/knowd
  tag: "v1.0.0"
  pullPolicy: IfNotPresent

# Persistence
persistence:
  enabled: true
  size: "100Gi"
  storageClass: "fast-ssd"

# Resources
resources:
  requests:
    memory: "4Gi"
    cpu: "2000m"
  limits:
    memory: "8Gi"
    cpu: "4000m"

# Configuration
config:
  KNOWD_CLUSTER_MODE: "leader"
  KNOWD_PEER_ADDRS: "knowd-0.knowd:8090,knowd-1.knowd:8090,knowd-2.knowd:8090"

# TLS
tls:
  enabled: true
  secretName: "knowd-tls"

# Monitoring
monitoring:
  enabled: true
  prometheusRule:
    enabled: true
```

## Docker Registry

### Publishing Images

**Build and push:**
```bash
# Build for multiple architectures
docker buildx build \
  --platform linux/amd64,linux/arm64 \
  --tag unrdf/knowd:v1.0.0 \
  --push .

# Or build for single architecture
docker build -t unrdf/knowd:v1.0.0 .
docker push unrdf/knowd:v1.0.0
```

**Automated builds:**
```bash
# GitHub Actions workflow
name: Build and Push
on:
  push:
    tags: ['v*']

jobs:
  build:
    runs-on: ubuntu-latest
    steps:
    - uses: actions/checkout@v3
    - name: Build and push Docker image
      run: |
        docker build -t unrdf/knowd:${{ github.ref_name }} .
        docker push unrdf/knowd:${{ github.ref_name }}
```

## Container Security

### Security Best Practices

**Non-root user:**
```dockerfile
# Create knowd user
RUN addgroup -g 1000 knowd && \
    adduser -D -s /bin/sh -u 1000 -G knowd knowd

# Set ownership and switch user
COPY --from=builder /app/knowd /usr/local/bin/knowd
RUN chown knowd:knowd /usr/local/bin/knowd
USER knowd
```

**Minimal base image:**
```dockerfile
FROM alpine:latest  # Instead of ubuntu:latest

# Install only required packages
RUN apk --no-cache add ca-certificates tzdata
```

**Read-only filesystem:**
```yaml
# Kubernetes security context
securityContext:
  readOnlyRootFilesystem: true
  runAsNonRoot: true
  runAsUser: 1000
```

### Vulnerability Scanning

**Container image scanning:**
```bash
# Using Trivy
trivy image unrdf/knowd:v1.0.0

# Using Clair
clair scan unrdf/knowd:v1.0.0

# Using Docker Scout
docker scout cves unrdf/knowd:v1.0.0
```

**Runtime security:**
```bash
# Install security monitoring
apk add --no-cache falco

# Enable audit logging
KNOWD_LOG_LEVEL=info \
KNOWD_LOG_FORMAT=json \
./knowd
```

## Monitoring and Logging

### Container Logging

**Structured logging:**
```bash
# Enable JSON logging in container
KNOWD_LOG_FORMAT=json \
KNOWD_LOG_LEVEL=info \
./knowd
```

**Log aggregation:**
```yaml
# Fluent Bit sidecar for log shipping
- name: fluent-bit
  image: fluent/fluent-bit:latest
  volumeMounts:
  - name: varlogcontainers
    mountPath: /var/log/containers
  - name: fluent-bit-config
    mountPath: /fluent-bit/etc/
```

### Health Checks

**Docker health checks:**
```dockerfile
HEALTHCHECK --interval=30s --timeout=10s --start-period=5s --retries=3 \
  CMD wget --no-verbose --tries=1 --spider http://localhost:8090/healthz || exit 1
```

**Kubernetes liveness and readiness probes:**
```yaml
livenessProbe:
  httpGet:
    path: /healthz
    port: 8090
  initialDelaySeconds: 30
  periodSeconds: 10

readinessProbe:
  httpGet:
    path: /healthz
    port: 8090
  initialDelaySeconds: 5
  periodSeconds: 5
```

## Troubleshooting

### Common Issues

**Container won't start:**
```bash
# Check logs
docker logs knowd

# Check resource limits
docker stats knowd

# Verify configuration
docker exec knowd cat /var/lib/knowd/config.yaml
```

**Network connectivity:**
```bash
# Test service discovery
kubectl exec knowd-leader-0 -- nslookup knowd-follower-0.knowd-follower

# Check cluster status
kubectl exec knowd-leader-0 -- curl http://localhost:8090/v1/cluster/status
```

**Storage issues:**
```bash
# Check disk space
kubectl exec knowd-leader-0 -- df -h /var/lib/knowd

# Verify PVC status
kubectl describe pvc data-knowd-leader-0
```

### Debugging Commands

**Container debugging:**
```bash
# Execute commands in container
docker exec -it knowd bash

# Copy files from container
docker cp knowd:/var/lib/knowd/snapshots/latest.snapshot ./snapshot

# View container resource usage
docker stats knowd
```

**Kubernetes debugging:**
```bash
# View pod logs
kubectl logs knowd-leader-0

# Describe pod for events
kubectl describe pod knowd-leader-0

# Execute commands in pod
kubectl exec knowd-leader-0 -- knowd --version
```

This Docker deployment guide provides comprehensive coverage for containerizing Knowd with Docker, Kubernetes, and related tooling for production deployments.
