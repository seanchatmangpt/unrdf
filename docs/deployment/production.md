# Production Deployment Guide

This guide covers deploying UNRDF in production environments with Docker, Kubernetes, and cloud platforms.

## Table of Contents

- [Prerequisites](#prerequisites)
- [Environment Variables](#environment-variables)
- [Docker Deployment](#docker-deployment)
- [Kubernetes Deployment](#kubernetes-deployment)
- [Cloud Platforms](#cloud-platforms)
- [Health Checks](#health-checks)
- [Monitoring](#monitoring)
- [Scaling Guidelines](#scaling-guidelines)
- [Security](#security)
- [Troubleshooting](#troubleshooting)

---

## Prerequisites

### System Requirements

**Minimum:**
- CPU: 2 cores
- RAM: 4 GB
- Storage: 10 GB SSD
- Node.js: 18.0.0+

**Recommended (Production):**
- CPU: 4+ cores
- RAM: 8+ GB
- Storage: 50+ GB SSD (NVMe preferred)
- Node.js: 18.19.0+ (LTS)

### Software Dependencies

- **Node.js:** 18.0.0 or higher
- **pnpm:** 7.0.0+ (for monorepo deployments)
- **Docker:** 20.10+ (for containerized deployments)
- **Redis:** 6.0+ (optional, for distributed caching)
- **PostgreSQL/SQLite:** (optional, for persistent Oxigraph backend)

---

## Environment Variables

Create a `.env` file for production configuration:

```bash
# Application
NODE_ENV=production
PORT=3000
HOST=0.0.0.0

# RDF Store Backend
RDF_BACKEND=oxigraph          # Options: memory, oxigraph, sqlite
OXIGRAPH_PATH=/data/rdf.db    # Path to Oxigraph database file

# SPARQL Endpoint
SPARQL_ENDPOINT_URL=http://localhost:3000/sparql
SPARQL_TIMEOUT=30000          # Query timeout in milliseconds

# Observability (OpenTelemetry)
OTEL_ENABLED=true
OTEL_SERVICE_NAME=unrdf-api
OTEL_EXPORTER_OTLP_ENDPOINT=http://otel-collector:4318
OTEL_TRACES_SAMPLER=always_on

# Logging
LOG_LEVEL=info                # Options: debug, info, warn, error
LOG_FORMAT=json               # Options: json, pretty

# Security
API_KEY_REQUIRED=true
API_KEY=your-secret-api-key   # Change this!

# Performance
MAX_QUERY_COMPLEXITY=100      # Prevent expensive queries
MAX_RESULTS_LIMIT=10000       # Maximum results per query
ENABLE_QUERY_CACHE=true
CACHE_TTL=300                 # Cache TTL in seconds

# Health Checks
HEALTH_CHECK_INTERVAL=30000   # Health check interval (ms)
```

**Security Warning:** Never commit `.env` files to version control. Use secrets management (AWS Secrets Manager, HashiCorp Vault, etc.) in production.

---

## Docker Deployment

### Dockerfile

Create `Dockerfile` in your project root:

```dockerfile
# syntax=docker/dockerfile:1

# Build stage
FROM node:18-alpine AS builder

WORKDIR /app

# Install pnpm
RUN npm install -g pnpm@8

# Copy package files
COPY package.json pnpm-lock.yaml pnpm-workspace.yaml ./
COPY packages/*/package.json ./packages/

# Install dependencies
RUN pnpm install --frozen-lockfile

# Copy source code
COPY . .

# Build all packages
RUN pnpm build

# Production stage
FROM node:18-alpine AS production

WORKDIR /app

# Install pnpm
RUN npm install -g pnpm@8

# Copy package files
COPY package.json pnpm-lock.yaml pnpm-workspace.yaml ./
COPY packages/*/package.json ./packages/

# Install production dependencies only
RUN pnpm install --frozen-lockfile --prod

# Copy built artifacts from builder
COPY --from=builder /app/packages/*/dist ./packages/

# Create data directory for Oxigraph
RUN mkdir -p /data && chown -R node:node /data

# Switch to non-root user
USER node

# Expose port
EXPOSE 3000

# Health check
HEALTHCHECK --interval=30s --timeout=3s --start-period=10s --retries=3 \
  CMD node -e "require('http').get('http://localhost:3000/health', (r) => process.exit(r.statusCode === 200 ? 0 : 1))"

# Start application
CMD ["node", "packages/cli/src/server.mjs"]
```

### Docker Compose

Create `docker-compose.yml` for local production-like environment:

```yaml
version: '3.8'

services:
  unrdf-api:
    build:
      context: .
      dockerfile: Dockerfile
    image: unrdf-api:latest
    container_name: unrdf-api
    ports:
      - "3000:3000"
    environment:
      NODE_ENV: production
      PORT: 3000
      RDF_BACKEND: oxigraph
      OXIGRAPH_PATH: /data/rdf.db
      OTEL_ENABLED: true
      OTEL_EXPORTER_OTLP_ENDPOINT: http://otel-collector:4318
      LOG_LEVEL: info
      LOG_FORMAT: json
    volumes:
      - rdf-data:/data
      - ./config:/app/config:ro
    restart: unless-stopped
    healthcheck:
      test: ["CMD", "wget", "--quiet", "--tries=1", "--spider", "http://localhost:3000/health"]
      interval: 30s
      timeout: 3s
      retries: 3
      start_period: 10s
    networks:
      - unrdf-network

  # OpenTelemetry Collector (Observability)
  otel-collector:
    image: otel/opentelemetry-collector-contrib:0.91.0
    container_name: otel-collector
    command: ["--config=/etc/otel-collector-config.yaml"]
    volumes:
      - ./otel-collector-config.yaml:/etc/otel-collector-config.yaml:ro
    ports:
      - "4318:4318"   # OTLP HTTP receiver
      - "8888:8888"   # Prometheus metrics
    networks:
      - unrdf-network

  # Jaeger (Distributed Tracing)
  jaeger:
    image: jaegertracing/all-in-one:1.52
    container_name: jaeger
    ports:
      - "16686:16686"  # Jaeger UI
      - "14268:14268"  # Jaeger collector HTTP
    environment:
      COLLECTOR_OTLP_ENABLED: true
    networks:
      - unrdf-network

  # Prometheus (Metrics)
  prometheus:
    image: prom/prometheus:v2.48.0
    container_name: prometheus
    command:
      - '--config.file=/etc/prometheus/prometheus.yml'
      - '--storage.tsdb.path=/prometheus'
      - '--web.console.libraries=/usr/share/prometheus/console_libraries'
      - '--web.console.templates=/usr/share/prometheus/consoles'
    volumes:
      - ./prometheus.yml:/etc/prometheus/prometheus.yml:ro
      - prometheus-data:/prometheus
    ports:
      - "9090:9090"
    networks:
      - unrdf-network

  # Grafana (Visualization)
  grafana:
    image: grafana/grafana:10.2.3
    container_name: grafana
    ports:
      - "3001:3000"
    environment:
      GF_SECURITY_ADMIN_USER: admin
      GF_SECURITY_ADMIN_PASSWORD: admin
      GF_INSTALL_PLUGINS: grafana-piechart-panel
    volumes:
      - grafana-data:/var/lib/grafana
      - ./grafana/dashboards:/etc/grafana/provisioning/dashboards:ro
      - ./grafana/datasources:/etc/grafana/provisioning/datasources:ro
    networks:
      - unrdf-network

volumes:
  rdf-data:
  prometheus-data:
  grafana-data:

networks:
  unrdf-network:
    driver: bridge
```

### Build and Run

```bash
# Build image
docker build -t unrdf-api:latest .

# Run with Docker Compose
docker-compose up -d

# View logs
docker-compose logs -f unrdf-api

# Stop services
docker-compose down

# Stop and remove volumes (WARNING: deletes data)
docker-compose down -v
```

---

## Kubernetes Deployment

### Kubernetes Manifests

Create `k8s/deployment.yaml`:

```yaml
apiVersion: apps/v1
kind: Deployment
metadata:
  name: unrdf-api
  labels:
    app: unrdf-api
spec:
  replicas: 3
  selector:
    matchLabels:
      app: unrdf-api
  template:
    metadata:
      labels:
        app: unrdf-api
    spec:
      containers:
      - name: unrdf-api
        image: unrdf-api:latest
        imagePullPolicy: IfNotPresent
        ports:
        - containerPort: 3000
          protocol: TCP
        env:
        - name: NODE_ENV
          value: "production"
        - name: PORT
          value: "3000"
        - name: RDF_BACKEND
          value: "oxigraph"
        - name: OXIGRAPH_PATH
          value: "/data/rdf.db"
        - name: OTEL_ENABLED
          value: "true"
        - name: OTEL_EXPORTER_OTLP_ENDPOINT
          value: "http://otel-collector:4318"
        - name: API_KEY
          valueFrom:
            secretKeyRef:
              name: unrdf-secrets
              key: api-key
        resources:
          requests:
            memory: "512Mi"
            cpu: "250m"
          limits:
            memory: "2Gi"
            cpu: "1000m"
        livenessProbe:
          httpGet:
            path: /health
            port: 3000
          initialDelaySeconds: 10
          periodSeconds: 30
          timeoutSeconds: 3
          failureThreshold: 3
        readinessProbe:
          httpGet:
            path: /ready
            port: 3000
          initialDelaySeconds: 5
          periodSeconds: 10
          timeoutSeconds: 3
          failureThreshold: 3
        volumeMounts:
        - name: rdf-data
          mountPath: /data
      volumes:
      - name: rdf-data
        persistentVolumeClaim:
          claimName: unrdf-pvc
---
apiVersion: v1
kind: Service
metadata:
  name: unrdf-api
spec:
  type: LoadBalancer
  ports:
  - port: 80
    targetPort: 3000
    protocol: TCP
  selector:
    app: unrdf-api
---
apiVersion: v1
kind: PersistentVolumeClaim
metadata:
  name: unrdf-pvc
spec:
  accessModes:
    - ReadWriteOnce
  resources:
    requests:
      storage: 50Gi
  storageClassName: fast-ssd
---
apiVersion: v1
kind: Secret
metadata:
  name: unrdf-secrets
type: Opaque
stringData:
  api-key: "your-secret-api-key"
```

### Deploy to Kubernetes

```bash
# Apply manifests
kubectl apply -f k8s/

# Check deployment status
kubectl get deployments
kubectl get pods
kubectl get services

# View logs
kubectl logs -f deployment/unrdf-api

# Scale deployment
kubectl scale deployment unrdf-api --replicas=5

# Delete deployment
kubectl delete -f k8s/
```

---

## Cloud Platforms

### AWS Elastic Beanstalk

```bash
# Install EB CLI
pip install awsebcli

# Initialize EB application
eb init -p node.js-18 unrdf-api

# Create environment
eb create unrdf-production --instance-type t3.medium

# Deploy
eb deploy

# View logs
eb logs

# Terminate environment
eb terminate unrdf-production
```

### Google Cloud Run

```bash
# Build and push image
gcloud builds submit --tag gcr.io/PROJECT_ID/unrdf-api

# Deploy to Cloud Run
gcloud run deploy unrdf-api \
  --image gcr.io/PROJECT_ID/unrdf-api \
  --platform managed \
  --region us-central1 \
  --allow-unauthenticated \
  --memory 2Gi \
  --cpu 2

# View service URL
gcloud run services describe unrdf-api --region us-central1
```

### Azure Container Instances

```bash
# Create resource group
az group create --name unrdf-rg --location eastus

# Deploy container
az container create \
  --resource-group unrdf-rg \
  --name unrdf-api \
  --image unrdf-api:latest \
  --cpu 2 \
  --memory 4 \
  --ports 3000 \
  --environment-variables \
    NODE_ENV=production \
    PORT=3000 \
    RDF_BACKEND=oxigraph

# Get container IP
az container show --resource-group unrdf-rg --name unrdf-api --query ipAddress.ip
```

---

## Health Checks

### HTTP Endpoints

```javascript
// Health check endpoint (liveness)
GET /health

Response:
{
  "status": "healthy",
  "timestamp": "2024-01-15T10:30:00Z",
  "uptime": 3600,
  "version": "5.0.1"
}

// Readiness check endpoint
GET /ready

Response:
{
  "status": "ready",
  "checks": {
    "database": "ok",
    "rdfStore": "ok",
    "cache": "ok"
  }
}
```

### Configuration

```javascript
// server.mjs
import express from 'express';

const app = express();

app.get('/health', (req, res) => {
  res.json({
    status: 'healthy',
    timestamp: new Date().toISOString(),
    uptime: process.uptime(),
    version: process.env.npm_package_version
  });
});

app.get('/ready', async (req, res) => {
  const checks = {
    database: await checkDatabase(),
    rdfStore: await checkRDFStore(),
    cache: await checkCache()
  };

  const allReady = Object.values(checks).every(status => status === 'ok');

  res.status(allReady ? 200 : 503).json({
    status: allReady ? 'ready' : 'not_ready',
    checks
  });
});
```

---

## Monitoring

### OpenTelemetry Configuration

Create `otel-collector-config.yaml`:

```yaml
receivers:
  otlp:
    protocols:
      http:
        endpoint: 0.0.0.0:4318
      grpc:
        endpoint: 0.0.0.0:4317

processors:
  batch:
    timeout: 10s
    send_batch_size: 1024

exporters:
  jaeger:
    endpoint: jaeger:14250
    tls:
      insecure: true

  prometheus:
    endpoint: "0.0.0.0:8889"

  logging:
    loglevel: debug

service:
  pipelines:
    traces:
      receivers: [otlp]
      processors: [batch]
      exporters: [jaeger, logging]
    metrics:
      receivers: [otlp]
      processors: [batch]
      exporters: [prometheus]
```

### Grafana Dashboards

Create `grafana/dashboards/unrdf-dashboard.json`:

```json
{
  "dashboard": {
    "title": "UNRDF Production Metrics",
    "panels": [
      {
        "title": "Request Rate",
        "targets": [{
          "expr": "rate(http_requests_total[5m])"
        }]
      },
      {
        "title": "SPARQL Query Duration",
        "targets": [{
          "expr": "histogram_quantile(0.95, sparql_query_duration_seconds_bucket)"
        }]
      },
      {
        "title": "RDF Store Size",
        "targets": [{
          "expr": "rdf_store_triples_total"
        }]
      },
      {
        "title": "Memory Usage",
        "targets": [{
          "expr": "process_resident_memory_bytes"
        }]
      }
    ]
  }
}
```

---

## Scaling Guidelines

### Horizontal Scaling

**When to scale:**
- CPU usage > 70%
- Memory usage > 80%
- Request latency > 1s (p95)
- Error rate > 1%

**Auto-scaling configuration (Kubernetes):**

```yaml
apiVersion: autoscaling/v2
kind: HorizontalPodAutoscaler
metadata:
  name: unrdf-api-hpa
spec:
  scaleTargetRef:
    apiVersion: apps/v1
    kind: Deployment
    name: unrdf-api
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

### Vertical Scaling

**Resource recommendations:**

| Load Level | CPU | RAM | Storage |
|------------|-----|-----|---------|
| Small (<1K queries/min) | 2 cores | 4 GB | 20 GB |
| Medium (1K-10K queries/min) | 4 cores | 8 GB | 50 GB |
| Large (10K-100K queries/min) | 8 cores | 16 GB | 200 GB |
| XLarge (>100K queries/min) | 16+ cores | 32+ GB | 500+ GB |

---

## Security

### Best Practices

1. **API Authentication:**
   ```javascript
   // Require API key for all requests
   app.use((req, res, next) => {
     const apiKey = req.headers['x-api-key'];
     if (apiKey !== process.env.API_KEY) {
       return res.status(401).json({ error: 'Unauthorized' });
     }
     next();
   });
   ```

2. **Rate Limiting:**
   ```javascript
   import rateLimit from 'express-rate-limit';

   const limiter = rateLimit({
     windowMs: 15 * 60 * 1000, // 15 minutes
     max: 100 // limit each IP to 100 requests per windowMs
   });

   app.use('/sparql', limiter);
   ```

3. **Query Complexity Limits:**
   ```javascript
   // Prevent expensive queries
   const MAX_QUERY_COMPLEXITY = 100;

   function estimateQueryComplexity(sparql) {
     // Analyze query structure
     // Return complexity score
   }

   app.post('/sparql', (req, res) => {
     const complexity = estimateQueryComplexity(req.body.query);
     if (complexity > MAX_QUERY_COMPLEXITY) {
       return res.status(400).json({ error: 'Query too complex' });
     }
     // Execute query
   });
   ```

4. **HTTPS Only:**
   ```javascript
   // Redirect HTTP to HTTPS
   app.use((req, res, next) => {
     if (req.headers['x-forwarded-proto'] !== 'https') {
       return res.redirect(`https://${req.hostname}${req.url}`);
     }
     next();
   });
   ```

---

## Troubleshooting

### Common Issues

**Problem: High memory usage**

```bash
# Check memory stats
docker stats unrdf-api

# Analyze heap snapshot
node --inspect packages/cli/src/server.mjs
# Open Chrome DevTools → Memory → Take heap snapshot
```

**Solution:**
- Increase container memory limit
- Enable query result streaming
- Implement pagination for large result sets

**Problem: Slow SPARQL queries**

```bash
# Enable query logging
LOG_LEVEL=debug docker-compose up

# Check OTEL traces in Jaeger
# http://localhost:16686
```

**Solution:**
- Add indexes to Oxigraph store
- Optimize SPARQL queries (use LIMIT, FILTER early)
- Enable query caching

**Problem: Container crashes on startup**

```bash
# View container logs
docker logs unrdf-api

# Check filesystem permissions
docker exec -it unrdf-api ls -la /data
```

**Solution:**
- Verify `/data` directory is writable
- Check Oxigraph database file is not corrupted
- Increase startup timeout in health check

---

For more troubleshooting, see [docs/troubleshooting.md](troubleshooting.md).
