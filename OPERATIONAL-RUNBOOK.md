# KGC Sidecar Operational Runbook

## Overview

This operational runbook provides comprehensive guidance for operating, monitoring, and troubleshooting the KGC (Knowledge Graph Control) JavaScript Sidecar in production environments.

## Table of Contents

- [System Architecture](#system-architecture)
- [Deployment](#deployment)
- [Configuration](#configuration)
- [Monitoring](#monitoring)
- [Troubleshooting](#troubleshooting)
- [Performance Tuning](#performance-tuning)
- [Security](#security)
- [Backup and Recovery](#backup-and-recovery)
- [Scaling](#scaling)
- [Maintenance](#maintenance)

## System Architecture

### Core Components

1. **Transaction Manager**: Orchestrates atomic knowledge graph transactions
2. **Knowledge Hook Manager**: Manages reactive knowledge hooks
3. **Policy Pack Manager**: Handles versioned governance policies
4. **Effect Sandbox**: Provides secure execution environment
5. **Lockchain Writer**: Creates cryptographic audit trails
6. **Resolution Layer**: Facilitates multi-agent coordination
7. **Observability Manager**: Provides monitoring and telemetry

### Data Flow

```
Host Application → Transaction Manager → Knowledge Hook Manager → Effect Sandbox
                                    ↓
                              Policy Pack Manager
                                    ↓
                              Resolution Layer (optional)
                                    ↓
                              Lockchain Writer (optional)
                                    ↓
                              Observability Manager
```

## Deployment

### Prerequisites

- Node.js 18.0.0 or higher
- pnpm 10.15.0 or higher
- Kubernetes cluster (for containerized deployment)
- Git repository (for lockchain anchoring)

### Container Deployment

#### 1. Docker Image

```dockerfile
FROM node:18-alpine

WORKDIR /app

# Install pnpm
RUN npm install -g pnpm@10.15.0

# Copy package files
COPY package.json pnpm-lock.yaml ./

# Install dependencies
RUN pnpm install --frozen-lockfile --production

# Copy source code
COPY dist/ ./dist/

# Create non-root user
RUN addgroup -g 1001 -S nodejs
RUN adduser -S sidecar -u 1001

# Change ownership
RUN chown -R sidecar:nodejs /app
USER sidecar

# Health check
HEALTHCHECK --interval=30s --timeout=3s --start-period=5s --retries=3 \
  CMD node -e "import('./dist/index.mjs').then(() => process.exit(0)).catch(() => process.exit(1))"

EXPOSE 3000

CMD ["node", "dist/index.mjs"]
```

#### 2. Kubernetes Deployment

```yaml
apiVersion: apps/v1
kind: Deployment
metadata:
  name: kgc-sidecar
  labels:
    app: kgc-sidecar
spec:
  replicas: 3
  selector:
    matchLabels:
      app: kgc-sidecar
  template:
    metadata:
      labels:
        app: kgc-sidecar
    spec:
      containers:
      - name: kgc-sidecar
        image: unrdf/kgc-sidecar:latest
        ports:
        - containerPort: 3000
        env:
        - name: NODE_ENV
          value: "production"
        - name: LOG_LEVEL
          value: "info"
        - name: ENABLE_OBSERVABILITY
          value: "true"
        - name: OBSERVABILITY_ENDPOINT
          value: "http://jaeger:14268/api/traces"
        resources:
          requests:
            memory: "256Mi"
            cpu: "250m"
          limits:
            memory: "512Mi"
            cpu: "500m"
        livenessProbe:
          httpGet:
            path: /health
            port: 3000
          initialDelaySeconds: 30
          periodSeconds: 10
        readinessProbe:
          httpGet:
            path: /ready
            port: 3000
          initialDelaySeconds: 5
          periodSeconds: 5
        volumeMounts:
        - name: config
          mountPath: /app/config
        - name: data
          mountPath: /app/data
      volumes:
      - name: config
        configMap:
          name: kgc-sidecar-config
      - name: data
        persistentVolumeClaim:
          claimName: kgc-sidecar-data
```

### Configuration

#### 1. Environment Variables

```bash
# Core Configuration
NODE_ENV=production
LOG_LEVEL=info
BASE_PATH=/app/data

# Performance Configuration
ENABLE_FAST_PATH=true
ENABLE_CACHING=true
ENABLE_BATCH_PROCESSING=true
MAX_CONCURRENCY=10
CACHE_SIZE=10000
BATCH_SIZE=1000
TIMEOUT_MS=2000
MAX_HOOKS=10000

# Observability Configuration
ENABLE_OBSERVABILITY=true
OBSERVABILITY_ENDPOINT=http://jaeger:14268/api/traces
SERVICE_NAME=kgc-sidecar
SERVICE_VERSION=1.0.0
SAMPLING_RATIO=1.0

# Security Configuration
ENABLE_SANDBOXING=true
SANDBOX_TYPE=worker
SANDBOX_TIMEOUT=30000
SANDBOX_MEMORY_LIMIT=67108864
STRICT_MODE=true

# Lockchain Configuration
ENABLE_LOCKCHAIN=true
LOCKCHAIN_REPOSITORY=/app/data/lockchain
GIT_ANCHORING=true
BATCH_MODE=true
BATCH_SIZE=100

# Resolution Layer Configuration
ENABLE_RESOLUTION=false
RESOLUTION_STRATEGY=voting
CONSENSUS_THRESHOLD=0.6
TIMEOUT_MS=5000
```

#### 2. Configuration File

```javascript
// config/production.mjs
export default {
  basePath: process.env.BASE_PATH || '/app/data',
  strictMode: process.env.STRICT_MODE === 'true',
  enableConditionEvaluation: true,
  maxHooks: parseInt(process.env.MAX_HOOKS) || 10000,
  timeout: parseInt(process.env.TIMEOUT_MS) || 2000,
  enableCache: process.env.ENABLE_CACHING === 'true',
  cacheMaxAge: 300000,
  enableMetrics: true,
  logLevel: process.env.LOG_LEVEL || 'info',
  
  observability: {
    enableTracing: process.env.ENABLE_OBSERVABILITY === 'true',
    enableMetrics: true,
    enableLogging: true,
    serviceName: process.env.SERVICE_NAME || 'kgc-sidecar',
    serviceVersion: process.env.SERVICE_VERSION || '1.0.0',
    endpoint: process.env.OBSERVABILITY_ENDPOINT,
    samplingRatio: parseFloat(process.env.SAMPLING_RATIO) || 1.0
  },
  
  performance: {
    enableProfiling: false,
    maxConcurrency: parseInt(process.env.MAX_CONCURRENCY) || 10,
    afterHashOnly: process.env.ENABLE_FAST_PATH === 'true',
    timeoutMs: parseInt(process.env.TIMEOUT_MS) || 2000,
    maxHooks: parseInt(process.env.MAX_HOOKS) || 10000
  },
  
  sandbox: {
    type: process.env.SANDBOX_TYPE || 'worker',
    timeout: parseInt(process.env.SANDBOX_TIMEOUT) || 30000,
    memoryLimit: parseInt(process.env.SANDBOX_MEMORY_LIMIT) || 67108864,
    enableNetwork: false,
    enableFileSystem: false,
    enableProcess: false,
    strictMode: process.env.STRICT_MODE === 'true'
  },
  
  lockchain: {
    enableGitAnchoring: process.env.GIT_ANCHORING === 'true',
    repository: process.env.LOCKCHAIN_REPOSITORY || '/app/data/lockchain',
    batchMode: process.env.BATCH_MODE === 'true',
    batchSize: parseInt(process.env.BATCH_SIZE) || 100
  },
  
  resolution: {
    enabled: process.env.ENABLE_RESOLUTION === 'true',
    strategy: process.env.RESOLUTION_STRATEGY || 'voting',
    consensusThreshold: parseFloat(process.env.CONSENSUS_THRESHOLD) || 0.6,
    timeoutMs: parseInt(process.env.TIMEOUT_MS) || 5000
  }
};
```

## Monitoring

### 1. Health Checks

#### Liveness Probe

```javascript
// health/liveness.mjs
export async function livenessCheck() {
  try {
    // Check if the application is running
    const memoryUsage = process.memoryUsage();
    const uptime = process.uptime();
    
    return {
      status: 'healthy',
      timestamp: new Date().toISOString(),
      uptime,
      memory: {
        rss: memoryUsage.rss,
        heapUsed: memoryUsage.heapUsed,
        heapTotal: memoryUsage.heapTotal,
        external: memoryUsage.external
      }
    };
  } catch (error) {
    return {
      status: 'unhealthy',
      timestamp: new Date().toISOString(),
      error: error.message
    };
  }
}
```

#### Readiness Probe

```javascript
// health/readiness.mjs
export async function readinessCheck() {
  try {
    // Check if the application is ready to serve requests
    const checks = await Promise.allSettled([
      checkDatabase(),
      checkCache(),
      checkObservability(),
      checkSandbox()
    ]);
    
    const failures = checks.filter(check => check.status === 'rejected');
    
    if (failures.length > 0) {
      return {
        status: 'not_ready',
        timestamp: new Date().toISOString(),
        failures: failures.map(f => f.reason.message)
      };
    }
    
    return {
      status: 'ready',
      timestamp: new Date().toISOString()
    };
  } catch (error) {
    return {
      status: 'not_ready',
      timestamp: new Date().toISOString(),
      error: error.message
    };
  }
}
```

### 2. Metrics

#### Key Performance Indicators (KPIs)

```javascript
// metrics/kpis.mjs
export const KPIs = {
  // Transaction Metrics
  transactionLatency: {
    p50: 'kgc.transaction.latency.p50',
    p95: 'kgc.transaction.latency.p95',
    p99: 'kgc.transaction.latency.p99',
    max: 'kgc.transaction.latency.max'
  },
  
  // Hook Execution Metrics
  hookExecutionRate: 'kgc.hooks.executed.per_minute',
  hookSuccessRate: 'kgc.hooks.success.rate',
  hookErrorRate: 'kgc.hooks.error.rate',
  
  // System Metrics
  memoryUsage: 'kgc.system.memory.usage',
  cpuUsage: 'kgc.system.cpu.usage',
  cacheHitRate: 'kgc.cache.hit.rate',
  
  // Error Metrics
  errorRate: 'kgc.errors.rate',
  errorCount: 'kgc.errors.count',
  
  // Backpressure Metrics
  queueDepth: 'kgc.queue.depth',
  watermarks: {
    high: 'kgc.queue.watermark.high',
    low: 'kgc.queue.watermark.low'
  }
};
```

#### Alerting Rules

```yaml
# alerts/kgc-sidecar.yml
groups:
- name: kgc-sidecar
  rules:
  - alert: HighTransactionLatency
    expr: kgc_transaction_latency_p99 > 2
    for: 5m
    labels:
      severity: warning
    annotations:
      summary: "High transaction latency detected"
      description: "Transaction latency p99 is {{ $value }}ms, exceeding 2ms threshold"
      
  - alert: HighErrorRate
    expr: kgc_errors_rate > 0.01
    for: 2m
    labels:
      severity: critical
    annotations:
      summary: "High error rate detected"
      description: "Error rate is {{ $value }}%, exceeding 1% threshold"
      
  - alert: HighMemoryUsage
    expr: kgc_system_memory_usage > 0.8
    for: 5m
    labels:
      severity: warning
    annotations:
      summary: "High memory usage detected"
      description: "Memory usage is {{ $value }}%, exceeding 80% threshold"
      
  - alert: LowCacheHitRate
    expr: kgc_cache_hit_rate < 0.7
    for: 10m
    labels:
      severity: warning
    annotations:
      summary: "Low cache hit rate detected"
      description: "Cache hit rate is {{ $value }}%, below 70% threshold"
```

### 3. Dashboards

#### Grafana Dashboard

```json
{
  "dashboard": {
    "title": "KGC Sidecar Dashboard",
    "panels": [
      {
        "title": "Transaction Latency",
        "type": "graph",
        "targets": [
          {
            "expr": "kgc_transaction_latency_p50",
            "legendFormat": "p50"
          },
          {
            "expr": "kgc_transaction_latency_p95",
            "legendFormat": "p95"
          },
          {
            "expr": "kgc_transaction_latency_p99",
            "legendFormat": "p99"
          }
        ]
      },
      {
        "title": "Hook Execution Rate",
        "type": "graph",
        "targets": [
          {
            "expr": "rate(kgc_hooks_executed_total[1m])",
            "legendFormat": "Executions/min"
          }
        ]
      },
      {
        "title": "Error Rate",
        "type": "graph",
        "targets": [
          {
            "expr": "rate(kgc_errors_total[1m])",
            "legendFormat": "Errors/min"
          }
        ]
      },
      {
        "title": "Memory Usage",
        "type": "graph",
        "targets": [
          {
            "expr": "kgc_system_memory_usage",
            "legendFormat": "Memory Usage"
          }
        ]
      }
    ]
  }
}
```

## Troubleshooting

### 1. Common Issues

#### High Transaction Latency

**Symptoms**:
- Transaction latency p99 > 2ms
- Slow response times
- Timeout errors

**Causes**:
- High hook execution time
- Cache misses
- Resource constraints
- Network latency

**Solutions**:
1. **Enable Fast Path**: Set `ENABLE_FAST_PATH=true`
2. **Optimize Hooks**: Review and optimize hook execution
3. **Increase Resources**: Scale up CPU/memory
4. **Enable Caching**: Set `ENABLE_CACHING=true`
5. **Batch Processing**: Enable batch processing for multiple transactions

#### High Error Rate

**Symptoms**:
- Error rate > 1%
- Failed transactions
- Hook execution failures

**Causes**:
- Invalid input data
- Hook execution errors
- Resource exhaustion
- Configuration issues

**Solutions**:
1. **Validate Input**: Ensure input data validation
2. **Review Hooks**: Check hook implementation
3. **Increase Resources**: Scale up resources
4. **Check Configuration**: Verify configuration settings
5. **Enable Strict Mode**: Set `STRICT_MODE=true` for better error handling

#### Memory Leaks

**Symptoms**:
- Continuously increasing memory usage
- Out of memory errors
- Performance degradation

**Causes**:
- Unclosed resources
- Circular references
- Event listener leaks
- Cache growth

**Solutions**:
1. **Monitor Memory**: Use memory monitoring tools
2. **Review Code**: Check for memory leaks in hooks
3. **Limit Cache**: Set appropriate cache limits
4. **Cleanup Resources**: Ensure proper resource cleanup
5. **Restart Services**: Periodic service restarts

#### Sandbox Failures

**Symptoms**:
- Hook execution failures
- Sandbox timeout errors
- Security violations

**Causes**:
- Malicious hook code
- Resource limits exceeded
- Security policy violations
- Configuration issues

**Solutions**:
1. **Review Hooks**: Check hook code for issues
2. **Adjust Limits**: Increase timeout/memory limits
3. **Security Audit**: Review security policies
4. **Enable Logging**: Enable detailed logging
5. **Isolate Execution**: Use worker thread isolation

### 2. Diagnostic Commands

#### System Status

```bash
# Check system status
kubectl get pods -l app=kgc-sidecar

# Check logs
kubectl logs -l app=kgc-sidecar --tail=100

# Check resource usage
kubectl top pods -l app=kgc-sidecar

# Check events
kubectl get events --sort-by=.metadata.creationTimestamp
```

#### Performance Analysis

```bash
# Check transaction metrics
curl http://localhost:3000/metrics | grep kgc_transaction

# Check hook execution metrics
curl http://localhost:3000/metrics | grep kgc_hooks

# Check error metrics
curl http://localhost:3000/metrics | grep kgc_errors

# Check memory usage
curl http://localhost:3000/metrics | grep kgc_system_memory
```

#### Debug Mode

```bash
# Enable debug logging
export LOG_LEVEL=debug

# Enable profiling
export ENABLE_PROFILING=true

# Enable detailed metrics
export ENABLE_DETAILED_METRICS=true

# Restart service
kubectl rollout restart deployment/kgc-sidecar
```

### 3. Log Analysis

#### Log Patterns

```bash
# Transaction errors
grep "Transaction failed" /var/log/kgc-sidecar.log

# Hook execution errors
grep "Hook execution failed" /var/log/kgc-sidecar.log

# Performance issues
grep "High latency" /var/log/kgc-sidecar.log

# Memory issues
grep "Memory usage" /var/log/kgc-sidecar.log
```

#### Log Aggregation

```yaml
# fluentd configuration
apiVersion: v1
kind: ConfigMap
metadata:
  name: fluentd-config
data:
  fluent.conf: |
    <source>
      @type tail
      path /var/log/kgc-sidecar.log
      pos_file /var/log/fluentd-kgc-sidecar.log.pos
      tag kgc-sidecar
      format json
    </source>
    
    <match kgc-sidecar>
      @type elasticsearch
      host elasticsearch.logging.svc.cluster.local
      port 9200
      index_name kgc-sidecar
      type_name _doc
    </match>
```

## Performance Tuning

### 1. Configuration Optimization

#### Performance Settings

```javascript
// High-performance configuration
export const performanceConfig = {
  // Enable all optimizations
  enableFastPath: true,
  enableCaching: true,
  enableBatchProcessing: true,
  enableParallelExecution: true,
  
  // Resource limits
  maxConcurrency: 20,
  cacheSize: 50000,
  batchSize: 2000,
  timeoutMs: 1000,
  maxHooks: 20000,
  
  // Memory optimization
  enableMemoryOptimization: true,
  enableQueryOptimization: true,
  
  // After hash only for speed
  afterHashOnly: true
};
```

#### Resource Allocation

```yaml
# High-performance resource allocation
resources:
  requests:
    memory: "1Gi"
    cpu: "1000m"
  limits:
    memory: "2Gi"
    cpu: "2000m"
```

### 2. Scaling Strategies

#### Horizontal Scaling

```yaml
# Horizontal Pod Autoscaler
apiVersion: autoscaling/v2
kind: HorizontalPodAutoscaler
metadata:
  name: kgc-sidecar-hpa
spec:
  scaleTargetRef:
    apiVersion: apps/v1
    kind: Deployment
    name: kgc-sidecar
  minReplicas: 3
  maxReplicas: 10
  metrics:
  - type: Resource
    resource:
      name: cpu
      target:
        type: Utilization
        averageUtilization: 70
  - type: Resource
    resource:
      name: memory
      target:
        type: Utilization
        averageUtilization: 80
```

#### Vertical Scaling

```yaml
# Vertical Pod Autoscaler
apiVersion: autoscaling.k8s.io/v1
kind: VerticalPodAutoscaler
metadata:
  name: kgc-sidecar-vpa
spec:
  targetRef:
    apiVersion: apps/v1
    kind: Deployment
    name: kgc-sidecar
  updatePolicy:
    updateMode: "Auto"
  resourcePolicy:
    containerPolicies:
    - containerName: kgc-sidecar
      minAllowed:
        cpu: 250m
        memory: 256Mi
      maxAllowed:
        cpu: 2000m
        memory: 4Gi
```

### 3. Caching Strategies

#### Multi-Level Caching

```javascript
// Cache configuration
export const cacheConfig = {
  // L1: In-memory cache
  l1: {
    type: 'memory',
    maxSize: 10000,
    ttl: 300000 // 5 minutes
  },
  
  // L2: Redis cache
  l2: {
    type: 'redis',
    host: 'redis-cluster',
    port: 6379,
    maxSize: 100000,
    ttl: 3600000 // 1 hour
  },
  
  // L3: Persistent cache
  l3: {
    type: 'persistent',
    path: '/app/data/cache',
    maxSize: 1000000,
    ttl: 86400000 // 24 hours
  }
};
```

## Security

### 1. Security Hardening

#### Container Security

```dockerfile
# Security-hardened Dockerfile
FROM node:18-alpine

# Install security updates
RUN apk update && apk upgrade

# Create non-root user
RUN addgroup -g 1001 -S nodejs
RUN adduser -S sidecar -u 1001

# Set security context
USER sidecar
WORKDIR /app

# Copy files with proper permissions
COPY --chown=sidecar:nodejs dist/ ./dist/
COPY --chown=sidecar:nodejs package.json ./

# Set read-only root filesystem
# Note: This requires careful configuration of writable directories
```

#### Network Security

```yaml
# Network policies
apiVersion: networking.k8s.io/v1
kind: NetworkPolicy
metadata:
  name: kgc-sidecar-netpol
spec:
  podSelector:
    matchLabels:
      app: kgc-sidecar
  policyTypes:
  - Ingress
  - Egress
  ingress:
  - from:
    - podSelector:
        matchLabels:
          app: host-application
    ports:
    - protocol: TCP
      port: 3000
  egress:
  - to:
    - podSelector:
        matchLabels:
          app: observability
    ports:
    - protocol: TCP
      port: 14268
```

### 2. Access Control

#### RBAC Configuration

```yaml
# Role-based access control
apiVersion: rbac.authorization.k8s.io/v1
kind: Role
metadata:
  name: kgc-sidecar-role
rules:
- apiGroups: [""]
  resources: ["configmaps", "secrets"]
  verbs: ["get", "list", "watch"]
- apiGroups: [""]
  resources: ["pods"]
  verbs: ["get", "list", "watch"]
```

#### Service Account

```yaml
# Service account
apiVersion: v1
kind: ServiceAccount
metadata:
  name: kgc-sidecar-sa
---
apiVersion: rbac.authorization.k8s.io/v1
kind: RoleBinding
metadata:
  name: kgc-sidecar-binding
subjects:
- kind: ServiceAccount
  name: kgc-sidecar-sa
roleRef:
  kind: Role
  name: kgc-sidecar-role
  apiGroup: rbac.authorization.k8s.io
```

### 3. Secrets Management

#### Kubernetes Secrets

```yaml
# Secrets configuration
apiVersion: v1
kind: Secret
metadata:
  name: kgc-sidecar-secrets
type: Opaque
data:
  # Base64 encoded secrets
  api-key: <base64-encoded-api-key>
  database-password: <base64-encoded-password>
  encryption-key: <base64-encoded-key>
```

#### External Secrets

```yaml
# External secrets operator
apiVersion: external-secrets.io/v1beta1
kind: SecretStore
metadata:
  name: vault-secret-store
spec:
  provider:
    vault:
      server: "https://vault.example.com"
      path: "secret"
      version: "v2"
      auth:
        kubernetes:
          mountPath: "kubernetes"
          role: "kgc-sidecar"
---
apiVersion: external-secrets.io/v1beta1
kind: ExternalSecret
metadata:
  name: kgc-sidecar-external-secret
spec:
  refreshInterval: 1h
  secretStoreRef:
    name: vault-secret-store
    kind: SecretStore
  target:
    name: kgc-sidecar-secrets
    creationPolicy: Owner
  data:
  - secretKey: api-key
    remoteRef:
      key: kgc-sidecar
      property: api-key
```

## Backup and Recovery

### 1. Data Backup

#### Lockchain Backup

```bash
#!/bin/bash
# Lockchain backup script

LOCKCHAIN_DIR="/app/data/lockchain"
BACKUP_DIR="/app/backups/lockchain"
DATE=$(date +%Y%m%d_%H%M%S)

# Create backup directory
mkdir -p "$BACKUP_DIR"

# Create backup
tar -czf "$BACKUP_DIR/lockchain_$DATE.tar.gz" -C "$LOCKCHAIN_DIR" .

# Keep only last 7 days of backups
find "$BACKUP_DIR" -name "lockchain_*.tar.gz" -mtime +7 -delete

echo "Lockchain backup completed: $BACKUP_DIR/lockchain_$DATE.tar.gz"
```

#### Configuration Backup

```bash
#!/bin/bash
# Configuration backup script

CONFIG_DIR="/app/config"
BACKUP_DIR="/app/backups/config"
DATE=$(date +%Y%m%d_%H%M%S)

# Create backup directory
mkdir -p "$BACKUP_DIR"

# Create backup
tar -czf "$BACKUP_DIR/config_$DATE.tar.gz" -C "$CONFIG_DIR" .

# Keep only last 30 days of backups
find "$BACKUP_DIR" -name "config_*.tar.gz" -mtime +30 -delete

echo "Configuration backup completed: $BACKUP_DIR/config_$DATE.tar.gz"
```

### 2. Recovery Procedures

#### Disaster Recovery

```bash
#!/bin/bash
# Disaster recovery script

BACKUP_DIR="/app/backups"
LOCKCHAIN_DIR="/app/data/lockchain"
CONFIG_DIR="/app/config"

# Restore lockchain
if [ -d "$BACKUP_DIR/lockchain" ]; then
    echo "Restoring lockchain..."
    tar -xzf "$BACKUP_DIR/lockchain/lockchain_latest.tar.gz" -C "$LOCKCHAIN_DIR"
    echo "Lockchain restored"
fi

# Restore configuration
if [ -d "$BACKUP_DIR/config" ]; then
    echo "Restoring configuration..."
    tar -xzf "$BACKUP_DIR/config/config_latest.tar.gz" -C "$CONFIG_DIR"
    echo "Configuration restored"
fi

# Restart services
echo "Restarting services..."
kubectl rollout restart deployment/kgc-sidecar

echo "Disaster recovery completed"
```

#### Data Recovery

```bash
#!/bin/bash
# Data recovery script

# Check lockchain integrity
echo "Checking lockchain integrity..."
cd /app/data/lockchain
git fsck --full

# Recover from git
echo "Recovering from git..."
git reflog
git reset --hard HEAD@{1}

# Verify recovery
echo "Verifying recovery..."
node -e "
import('./dist/knowledge-engine.mjs').then(engine => {
  console.log('Engine loaded successfully');
  process.exit(0);
}).catch(err => {
  console.error('Recovery failed:', err);
  process.exit(1);
});
"

echo "Data recovery completed"
```

## Scaling

### 1. Horizontal Scaling

#### Load Balancing

```yaml
# Service with load balancing
apiVersion: v1
kind: Service
metadata:
  name: kgc-sidecar-service
spec:
  selector:
    app: kgc-sidecar
  ports:
  - port: 3000
    targetPort: 3000
    protocol: TCP
  type: ClusterIP
---
# Ingress for external access
apiVersion: networking.k8s.io/v1
kind: Ingress
metadata:
  name: kgc-sidecar-ingress
  annotations:
    nginx.ingress.kubernetes.io/load-balance: "round_robin"
    nginx.ingress.kubernetes.io/upstream-hash-by: "$request_uri"
spec:
  rules:
  - host: kgc-sidecar.example.com
    http:
      paths:
      - path: /
        pathType: Prefix
        backend:
          service:
            name: kgc-sidecar-service
            port:
              number: 3000
```

#### Auto Scaling

```yaml
# Horizontal Pod Autoscaler
apiVersion: autoscaling/v2
kind: HorizontalPodAutoscaler
metadata:
  name: kgc-sidecar-hpa
spec:
  scaleTargetRef:
    apiVersion: apps/v1
    kind: Deployment
    name: kgc-sidecar
  minReplicas: 3
  maxReplicas: 20
  metrics:
  - type: Resource
    resource:
      name: cpu
      target:
        type: Utilization
        averageUtilization: 70
  - type: Resource
    resource:
      name: memory
      target:
        type: Utilization
        averageUtilization: 80
  - type: Pods
    pods:
      metric:
        name: kgc_transaction_latency_p99
      target:
        type: AverageValue
        averageValue: "2"
```

### 2. Vertical Scaling

#### Resource Optimization

```yaml
# Resource optimization
apiVersion: apps/v1
kind: Deployment
metadata:
  name: kgc-sidecar
spec:
  template:
    spec:
      containers:
      - name: kgc-sidecar
        resources:
          requests:
            memory: "512Mi"
            cpu: "500m"
          limits:
            memory: "2Gi"
            cpu: "2000m"
        env:
        - name: NODE_OPTIONS
          value: "--max-old-space-size=1536"
```

## Maintenance

### 1. Regular Maintenance

#### Health Checks

```bash
#!/bin/bash
# Daily health check script

echo "Starting daily health check..."

# Check pod status
kubectl get pods -l app=kgc-sidecar

# Check resource usage
kubectl top pods -l app=kgc-sidecar

# Check logs for errors
kubectl logs -l app=kgc-sidecar --since=24h | grep -i error

# Check metrics
curl -s http://localhost:3000/metrics | grep kgc_errors_total

# Check lockchain integrity
cd /app/data/lockchain && git fsck --full

echo "Daily health check completed"
```

#### Performance Monitoring

```bash
#!/bin/bash
# Performance monitoring script

echo "Starting performance monitoring..."

# Check transaction latency
LATENCY_P99=$(curl -s http://localhost:3000/metrics | grep kgc_transaction_latency_p99 | awk '{print $2}')
if (( $(echo "$LATENCY_P99 > 2" | bc -l) )); then
    echo "WARNING: Transaction latency p99 is $LATENCY_P99 ms (threshold: 2 ms)"
fi

# Check error rate
ERROR_RATE=$(curl -s http://localhost:3000/metrics | grep kgc_errors_rate | awk '{print $2}')
if (( $(echo "$ERROR_RATE > 0.01" | bc -l) )); then
    echo "WARNING: Error rate is $ERROR_RATE (threshold: 0.01)"
fi

# Check memory usage
MEMORY_USAGE=$(curl -s http://localhost:3000/metrics | grep kgc_system_memory_usage | awk '{print $2}')
if (( $(echo "$MEMORY_USAGE > 0.8" | bc -l) )); then
    echo "WARNING: Memory usage is $MEMORY_USAGE (threshold: 0.8)"
fi

echo "Performance monitoring completed"
```

### 2. Updates and Upgrades

#### Rolling Updates

```bash
#!/bin/bash
# Rolling update script

echo "Starting rolling update..."

# Update image
kubectl set image deployment/kgc-sidecar kgc-sidecar=unrdf/kgc-sidecar:latest

# Wait for rollout
kubectl rollout status deployment/kgc-sidecar

# Verify update
kubectl get pods -l app=kgc-sidecar

echo "Rolling update completed"
```

#### Blue-Green Deployment

```yaml
# Blue-green deployment
apiVersion: argoproj.io/v1alpha1
kind: Rollout
metadata:
  name: kgc-sidecar-rollout
spec:
  replicas: 3
  strategy:
    blueGreen:
      activeService: kgc-sidecar-active
      previewService: kgc-sidecar-preview
      autoPromotionEnabled: false
      scaleDownDelaySeconds: 30
      prePromotionAnalysis:
        templates:
        - templateName: success-rate
        args:
        - name: service-name
          value: kgc-sidecar-preview
      postPromotionAnalysis:
        templates:
        - templateName: success-rate
        args:
        - name: service-name
          value: kgc-sidecar-active
  selector:
    matchLabels:
      app: kgc-sidecar
  template:
    metadata:
      labels:
        app: kgc-sidecar
    spec:
      containers:
      - name: kgc-sidecar
        image: unrdf/kgc-sidecar:latest
        ports:
        - containerPort: 3000
```

### 3. Monitoring and Alerting

#### Prometheus Rules

```yaml
# Prometheus alerting rules
groups:
- name: kgc-sidecar
  rules:
  - alert: KGC Sidecar Down
    expr: up{job="kgc-sidecar"} == 0
    for: 1m
    labels:
      severity: critical
    annotations:
      summary: "KGC Sidecar is down"
      description: "KGC Sidecar has been down for more than 1 minute"
      
  - alert: High Transaction Latency
    expr: kgc_transaction_latency_p99 > 2
    for: 5m
    labels:
      severity: warning
    annotations:
      summary: "High transaction latency"
      description: "Transaction latency p99 is {{ $value }}ms"
      
  - alert: High Error Rate
    expr: kgc_errors_rate > 0.01
    for: 2m
    labels:
      severity: critical
    annotations:
      summary: "High error rate"
      description: "Error rate is {{ $value }}%"
```

#### Grafana Dashboard

```json
{
  "dashboard": {
    "title": "KGC Sidecar Operations",
    "panels": [
      {
        "title": "System Overview",
        "type": "stat",
        "targets": [
          {
            "expr": "up{job=\"kgc-sidecar\"}",
            "legendFormat": "Status"
          }
        ]
      },
      {
        "title": "Transaction Latency",
        "type": "graph",
        "targets": [
          {
            "expr": "kgc_transaction_latency_p50",
            "legendFormat": "p50"
          },
          {
            "expr": "kgc_transaction_latency_p95",
            "legendFormat": "p95"
          },
          {
            "expr": "kgc_transaction_latency_p99",
            "legendFormat": "p99"
          }
        ]
      }
    ]
  }
}
```

## Conclusion

This operational runbook provides comprehensive guidance for operating the KGC JavaScript Sidecar in production environments. Regular monitoring, proactive maintenance, and proper scaling strategies are essential for ensuring optimal performance and reliability.

For additional support or questions, please refer to the project documentation or create an issue in the GitHub repository.
