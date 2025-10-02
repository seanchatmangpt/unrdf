# UNRDF Production Deployment Guide

## 📋 Pre-Deployment Checklist

### 1. Environment Configuration

- [ ] Copy `.env.production` to `.env` or source it
- [ ] Configure OTEL endpoint for your monitoring infrastructure
- [ ] Set up Redis instance (optional, for caching)
- [ ] Set up PostgreSQL instance (optional, for lockchain)
- [ ] Configure KGC Sidecar endpoint
- [ ] Set `UNRDF_BASE_IRI` to your domain
- [ ] Review and adjust performance limits
- [ ] Set secrets in `.env.local` (not in version control)

### 2. Infrastructure Validation

```bash
# Make validation script executable
chmod +x scripts/validate-env.sh

# Load production environment
source .env.production

# Run validation
./scripts/validate-env.sh
```

### 3. Service Dependencies

#### Required Services
- **Node.js**: >= v18.0.0
- **Oxigraph RDF Store**: Built into UNRDF (no external setup)

#### Optional Services
- **Jaeger**: For distributed tracing (OTEL)
  ```bash
  docker run -d --name jaeger \
    -p 14250:14250 \
    -p 16686:16686 \
    jaegertracing/all-in-one:latest
  ```

- **Redis**: For query result caching
  ```bash
  docker run -d --name redis \
    -p 6379:6379 \
    redis:alpine
  ```

- **PostgreSQL**: For lockchain (cryptographic provenance)
  ```bash
  docker run -d --name postgres \
    -e POSTGRES_DB=unrdf \
    -e POSTGRES_USER=postgres \
    -e POSTGRES_PASSWORD=your_password \
    -p 5432:5432 \
    postgres:15
  ```

### 4. Build and Test

```bash
# Install dependencies
npm ci

# Run tests
npm test

# Run E2E tests
npm run test:e2e

# Build (if applicable)
npm run build
```

### 5. Security Hardening

- [ ] Enable audit logging: `ENABLE_AUDIT_LOG=true`
- [ ] Set query complexity limits: `MAX_QUERY_COMPLEXITY=1000`
- [ ] Enable query sanitization: `ENABLE_QUERY_SANITIZATION=true`
- [ ] Configure TLS for KGC Sidecar: `KGC_SIDECAR_TLS_ENABLED=true`
- [ ] Rotate secrets regularly
- [ ] Use read-only mode if applicable: `UNRDF_STORE_READONLY=true`
- [ ] Review and restrict `CONCURRENT_QUERY_LIMIT` based on load

## 🚀 Deployment Steps

### Step 1: Environment Setup

```bash
# On production server
git clone <your-repo>
cd unrdf

# Load production environment
cp .env.production .env

# Install production dependencies only
npm ci --production
```

### Step 2: Initialize UNRDF

```bash
# Create directory structure
mkdir -p .unrdf/{hooks,policies,cache,logs}

# Initialize RDF store
node cli/unrdf.mjs store create

# Verify installation
node cli/unrdf.mjs --version
```

### Step 3: Load Initial Data

```bash
# Import your RDF data
node cli/unrdf.mjs store import data/ontology.ttl
node cli/unrdf.mjs store import data/instances.ttl

# Verify import
node cli/unrdf.mjs store stats
```

### Step 4: Configure Policies

```bash
# Create default policy
cat > .unrdf/policies/default.ttl << 'EOF'
@prefix unrdf: <http://unrdf.io/ontology#> .
@prefix sh: <http://www.w3.org/ns/shacl#> .

unrdf:DefaultPolicy a unrdf:Policy ;
    unrdf:name "Default Production Policy" ;
    unrdf:validation sh:MaxCountConstraintComponent ;
    unrdf:enforcement "strict" .
EOF

# Validate policy
node cli/unrdf.mjs policy validate .unrdf/policies/default.ttl
```

### Step 5: Start Services

```bash
# Start OTEL collector (Jaeger)
docker-compose up -d jaeger

# Start cache (Redis)
docker-compose up -d redis

# Start lockchain (PostgreSQL)
docker-compose up -d postgres

# Verify all services
./scripts/validate-env.sh
```

### Step 6: Health Check

```bash
# Test SPARQL query
node cli/unrdf.mjs query "SELECT * WHERE { ?s ?p ?o } LIMIT 10"

# Test KGC integration
node cli/unrdf.mjs kgc validate

# Check OTEL traces
curl http://localhost:16686/api/traces?service=unrdf-cli

# Test Knowledge Hooks
node cli/unrdf.mjs hooks test
```

## 📊 Monitoring

### Jaeger UI
- Access: `http://localhost:16686`
- View traces and spans for all UNRDF operations
- Monitor performance and errors

### OTEL Metrics
```bash
# Enable metrics export
export OTEL_METRICS_EXPORTER=prometheus
export OTEL_EXPORTER_PROMETHEUS_PORT=9464

# Access metrics
curl http://localhost:9464/metrics
```

### Application Logs
```bash
# Tail application logs
tail -f .unrdf/logs/unrdf.log

# Tail audit logs
tail -f .unrdf/logs/audit.log

# Search for errors
grep "ERROR" .unrdf/logs/unrdf.log
```

## 🔧 Troubleshooting

### Issue: Cannot Connect to Jaeger
```bash
# Check Jaeger status
docker ps | grep jaeger

# Verify endpoint
curl http://localhost:14250

# Fallback: Disable OTEL
export OTEL_TRACES_SAMPLER=always_off
```

### Issue: Redis Connection Failed
```bash
# Check Redis status
redis-cli ping

# Fallback: Disable caching
export REDIS_CACHE_TTL=0
```

### Issue: KGC Sidecar Unreachable
```bash
# Check gRPC endpoint
grpcurl -plaintext localhost:50051 list

# Fallback: Disable KGC features
export ENABLE_KNOWLEDGE_HOOKS=false
```

### Issue: Query Timeout
```bash
# Increase timeout
export QUERY_TIMEOUT_MS=60000

# Reduce concurrent queries
export CONCURRENT_QUERY_LIMIT=5

# Enable caching
export REDIS_CACHE_TTL=7200
```

## 🔄 Updates and Rollback

### Update Procedure
```bash
# Backup current store
cp .unrdf-store.nq .unrdf-store.nq.backup

# Pull latest code
git pull origin main

# Install dependencies
npm ci

# Run migrations (if any)
npm run migrate

# Restart services
docker-compose restart
```

### Rollback Procedure
```bash
# Restore previous version
git checkout <previous-tag>

# Restore store backup
cp .unrdf-store.nq.backup .unrdf-store.nq

# Reinstall dependencies
npm ci

# Restart services
docker-compose restart
```

## 📈 Performance Tuning

### Query Optimization
- Use `CONCURRENT_QUERY_LIMIT` to control parallelism
- Enable Redis caching: `REDIS_CACHE_TTL=3600`
- Adjust `QUERY_TIMEOUT_MS` based on query complexity
- Set `MAX_QUERY_COMPLEXITY` to prevent expensive queries

### Import Optimization
- Use batch imports: `BATCH_INSERT_SIZE=1000`
- Increase `MAX_IMPORT_SIZE_MB` for large datasets
- Use N-Quads format for faster imports

### Memory Management
- Monitor `CACHE_MAX_SIZE_MB` and adjust based on available memory
- Configure PostgreSQL connection pool: `POSTGRES_MAX_CONNECTIONS`
- Set appropriate `LOG_RETENTION_DAYS` to manage disk space

## 🛡️ Security Best Practices

1. **Never commit secrets** to version control
   - Use `.env.local` for sensitive values
   - Add `.env.local` to `.gitignore`

2. **Enable audit logging**
   ```bash
   export ENABLE_AUDIT_LOG=true
   export AUDIT_LOG_PATH=.unrdf/logs/audit.log
   ```

3. **Use TLS for external services**
   ```bash
   export KGC_SIDECAR_TLS_ENABLED=true
   export REDIS_TLS_ENABLED=true
   export POSTGRES_SSL_MODE=require
   ```

4. **Restrict query complexity**
   ```bash
   export MAX_QUERY_COMPLEXITY=1000
   export ENABLE_QUERY_SANITIZATION=true
   ```

5. **Regular security updates**
   ```bash
   npm audit
   npm audit fix
   ```

## 📞 Support

- **Documentation**: `/docs`
- **GitHub Issues**: Create an issue with deployment logs
- **OTEL Traces**: Include trace ID when reporting issues
- **Logs**: Share relevant logs from `.unrdf/logs/`

## ✅ Post-Deployment Verification

```bash
# Run full test suite
npm test

# Verify OTEL integration
curl http://localhost:16686/api/traces?service=unrdf-cli | jq

# Check store integrity
node cli/unrdf.mjs store validate

# Test Knowledge Hooks
node cli/unrdf.mjs hooks test

# Verify all services
./scripts/validate-env.sh

# Check logs for errors
grep -i "error" .unrdf/logs/unrdf.log
```

---

**Deployment Status**: ✅ Production Ready

**Last Updated**: 2025-10-01

**Validated By**: Environment Validation Script v1.0.0
