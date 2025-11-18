# UNRDF Federation Deployment Guide

## Overview

This guide covers deploying a distributed federation of RDF stores using UNRDF's federation protocol. The federation provides distributed query execution, data replication, and RAFT consensus for high availability.

## Architecture

```
┌─────────────────────────────────────────────────────────────┐
│                     Federation Layer                        │
│  ┌──────────────┐  ┌──────────────┐  ┌──────────────┐      │
│  │   RAFT       │  │   Query      │  │ Replication  │      │
│  │  Consensus   │  │   Engine     │  │   Manager    │      │
│  └──────────────┘  └──────────────┘  └──────────────┘      │
└─────────────────────────────────────────────────────────────┘
                            │
        ┌───────────────────┼───────────────────┐
        │                   │                   │
   ┌────▼────┐         ┌────▼────┐         ┌────▼────┐
   │ Store 1 │         │ Store 2 │         │ Store 3 │
   │  (US)   │         │  (EU)   │         │  (ASIA) │
   └─────────┘         └─────────┘         └─────────┘
```

## Prerequisites

- Node.js >= 18.0.0
- pnpm >= 7.0.0
- Docker (optional, for containerized deployment)
- Kubernetes cluster (optional, for k8s deployment)

## Quick Start

### 1. Install Dependencies

```bash
pnpm install
```

### 2. Basic Federation Setup

```javascript
import { createFederatedSystem } from 'unrdf/federation';

const federation = await createFederatedSystem({
  federationId: 'my-federation',
  enableConsensus: true,
  replicationTopology: 'full-mesh',
  loadBalancingStrategy: 'weighted'
});

// Register stores
await federation.registerStore({
  storeId: 'store-1',
  endpoint: 'http://store1.example.com:3000',
  weight: 1.0
});

await federation.registerStore({
  storeId: 'store-2',
  endpoint: 'http://store2.example.com:3000',
  weight: 0.8
});

// Query across federation
const results = await federation.query(`
  SELECT * WHERE { ?s ?p ?o } LIMIT 100
`);
```

## Configuration Options

### Federation Coordinator

```javascript
{
  federationId: 'unique-federation-id',
  enableConsensus: true,              // Enable RAFT consensus
  healthCheckInterval: 5000,          // Health check interval (ms)
  healthCheckTimeout: 2000,           // Health check timeout (ms)
  maxRetries: 3,                      // Max retry attempts
  loadBalancingStrategy: 'weighted'   // 'round-robin', 'weighted', 'least-loaded'
}
```

### Query Engine

```javascript
{
  timeout: 30000,                     // Query timeout (ms)
  maxParallelism: 10,                 // Max parallel queries
  executionStrategy: 'adaptive',      // 'parallel', 'sequential', 'adaptive'
  enablePushdown: true,               // Enable filter pushdown
  enableJoinOptimization: true,       // Enable join optimization
  streamResults: false                // Stream results
}
```

### Replication Manager

```javascript
{
  topology: 'full-mesh',              // 'full-mesh', 'star', 'ring', 'tree'
  mode: 'bidirectional',              // 'push', 'pull', 'bidirectional'
  conflictResolution: 'last-write-wins', // Conflict strategy
  batchSize: 100,                     // Batch size for replication
  batchInterval: 1000,                // Batch interval (ms)
  enableStreaming: false,             // Enable streaming replication
  maxRetries: 3,                      // Max retry attempts
  retryDelay: 1000                    // Retry delay (ms)
}
```

## Deployment Scenarios

### Single Node Development

For local development and testing:

```bash
# Start federation with in-memory stores
node examples/federation/basic-federation.mjs
```

### Multi-Node Production

#### Using Docker Compose

Create `docker-compose.yml`:

```yaml
version: '3.8'

services:
  coordinator:
    image: unrdf/federation:latest
    environment:
      - FEDERATION_ID=prod-federation
      - ENABLE_CONSENSUS=true
      - REPLICATION_TOPOLOGY=full-mesh
    ports:
      - "8080:8080"
    networks:
      - federation-net

  store-1:
    image: unrdf/rdf-store:latest
    environment:
      - STORE_ID=store-1
      - COORDINATOR_URL=http://coordinator:8080
    ports:
      - "3001:3000"
    networks:
      - federation-net
    volumes:
      - store1-data:/data

  store-2:
    image: unrdf/rdf-store:latest
    environment:
      - STORE_ID=store-2
      - COORDINATOR_URL=http://coordinator:8080
    ports:
      - "3002:3000"
    networks:
      - federation-net
    volumes:
      - store2-data:/data

  store-3:
    image: unrdf/rdf-store:latest
    environment:
      - STORE_ID=store-3
      - COORDINATOR_URL=http://coordinator:8080
    ports:
      - "3003:3000"
    networks:
      - federation-net
    volumes:
      - store3-data:/data

networks:
  federation-net:
    driver: bridge

volumes:
  store1-data:
  store2-data:
  store3-data:
```

Deploy:

```bash
docker-compose up -d
```

#### Using Kubernetes

Create `k8s/federation-deployment.yaml`:

```yaml
apiVersion: v1
kind: ConfigMap
metadata:
  name: federation-config
data:
  federation.json: |
    {
      "federationId": "k8s-federation",
      "enableConsensus": true,
      "replicationTopology": "full-mesh",
      "loadBalancingStrategy": "weighted"
    }

---
apiVersion: apps/v1
kind: Deployment
metadata:
  name: federation-coordinator
spec:
  replicas: 3
  selector:
    matchLabels:
      app: federation-coordinator
  template:
    metadata:
      labels:
        app: federation-coordinator
    spec:
      containers:
      - name: coordinator
        image: unrdf/federation:latest
        ports:
        - containerPort: 8080
        env:
        - name: FEDERATION_CONFIG
          valueFrom:
            configMapKeyRef:
              name: federation-config
              key: federation.json
        resources:
          requests:
            memory: "512Mi"
            cpu: "500m"
          limits:
            memory: "1Gi"
            cpu: "1000m"

---
apiVersion: v1
kind: Service
metadata:
  name: federation-service
spec:
  selector:
    app: federation-coordinator
  ports:
  - port: 8080
    targetPort: 8080
  type: LoadBalancer

---
apiVersion: apps/v1
kind: StatefulSet
metadata:
  name: rdf-store
spec:
  serviceName: rdf-store
  replicas: 3
  selector:
    matchLabels:
      app: rdf-store
  template:
    metadata:
      labels:
        app: rdf-store
    spec:
      containers:
      - name: store
        image: unrdf/rdf-store:latest
        ports:
        - containerPort: 3000
        volumeMounts:
        - name: data
          mountPath: /data
        resources:
          requests:
            memory: "1Gi"
            cpu: "500m"
          limits:
            memory: "2Gi"
            cpu: "1000m"
  volumeClaimTemplates:
  - metadata:
      name: data
    spec:
      accessModes: [ "ReadWriteOnce" ]
      resources:
        requests:
          storage: 10Gi
```

Deploy to Kubernetes:

```bash
kubectl apply -f k8s/federation-deployment.yaml
```

## Monitoring and Observability

### OpenTelemetry Integration

The federation protocol includes built-in OTEL instrumentation:

```javascript
import { NodeSDK } from '@opentelemetry/sdk-node';
import { OTLPTraceExporter } from '@opentelemetry/exporter-otlp-http';

const sdk = new NodeSDK({
  traceExporter: new OTLPTraceExporter({
    url: 'http://otel-collector:4318/v1/traces'
  })
});

sdk.start();

// Federation automatically creates spans for:
// - Query execution
// - Replication operations
// - Consensus operations
// - Health checks
```

### Key Metrics

Monitor these metrics for production deployments:

| Metric | Description | Target |
|--------|-------------|--------|
| `federation.query.duration` | Query execution time | p95 < 200ms |
| `federation.query.total` | Total queries executed | Track growth |
| `federation.replication.latency` | Replication latency | p95 < 100ms |
| `federation.stores.healthy` | Number of healthy stores | = total stores |
| `federation.replication.conflicts` | Replication conflicts | < 1% of operations |

### Jaeger Tracing

View distributed traces in Jaeger:

```bash
docker run -d --name jaeger \
  -p 16686:16686 \
  -p 4318:4318 \
  jaegertracing/all-in-one:latest
```

Access UI: http://localhost:16686

### Prometheus Metrics

Export metrics to Prometheus:

```javascript
import { PrometheusExporter } from '@opentelemetry/exporter-prometheus';

const exporter = new PrometheusExporter({
  port: 9464
});

// Metrics available at: http://localhost:9464/metrics
```

## Performance Tuning

### Query Optimization

1. **Enable Pushdown**: Push filters to stores
```javascript
queryEngine: { enablePushdown: true }
```

2. **Adjust Parallelism**: Tune for your workload
```javascript
queryEngine: { maxParallelism: 20 }
```

3. **Use Adaptive Strategy**: Let engine decide
```javascript
queryEngine: { executionStrategy: 'adaptive' }
```

### Replication Optimization

1. **Batch Operations**: Increase batch size for bulk updates
```javascript
replication: { batchSize: 500, batchInterval: 500 }
```

2. **Choose Topology**: Match topology to use case
   - **Full Mesh**: High consistency, high overhead
   - **Star**: Centralized, moderate overhead
   - **Ring**: Eventual consistency, low overhead

3. **Streaming**: Enable for real-time replication
```javascript
replication: { enableStreaming: true }
```

## Security

### Authentication

Implement authentication for store endpoints:

```javascript
await federation.registerStore({
  storeId: 'secure-store',
  endpoint: 'https://store.example.com:3000',
  metadata: {
    auth: {
      type: 'bearer',
      token: process.env.STORE_AUTH_TOKEN
    }
  }
});
```

### Encryption

Use TLS for all inter-store communication:

```javascript
// In production, always use HTTPS endpoints
endpoint: 'https://store.example.com:3000'
```

### Network Policies

Restrict network access using Kubernetes network policies:

```yaml
apiVersion: networking.k8s.io/v1
kind: NetworkPolicy
metadata:
  name: federation-network-policy
spec:
  podSelector:
    matchLabels:
      app: rdf-store
  policyTypes:
  - Ingress
  ingress:
  - from:
    - podSelector:
        matchLabels:
          app: federation-coordinator
```

## Troubleshooting

### Common Issues

1. **Store Health Checks Failing**
   - Check network connectivity
   - Verify store endpoints
   - Increase health check timeout

2. **Query Timeouts**
   - Increase query timeout
   - Check store performance
   - Reduce result set size

3. **Replication Lag**
   - Check network bandwidth
   - Increase batch size
   - Verify conflict resolution

### Debug Mode

Enable debug logging:

```javascript
process.env.DEBUG = 'unrdf:federation:*';
```

### OTEL Validation

Run OTEL validation to verify instrumentation:

```bash
node validation/federation.validation.mjs
```

## Best Practices

1. **Start Small**: Begin with 2-3 stores
2. **Monitor Metrics**: Track p95 query latency
3. **Use Health Checks**: Enable automatic failover
4. **Plan Topology**: Choose based on consistency needs
5. **Test Failures**: Simulate network partitions
6. **Backup Data**: Regular snapshots of stores
7. **Version Control**: Track configuration changes
8. **Document Decisions**: Keep ADRs for major changes

## Support

- Documentation: https://github.com/unrdf/unrdf
- Issues: https://github.com/unrdf/unrdf/issues
- Community: https://discord.gg/unrdf

## License

MIT
