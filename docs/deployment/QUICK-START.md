# UNRDF Production Quick Start

## TL;DR - Get Running in 5 Minutes

### 1. Environment Setup (1 min)
```bash
cd unrdf
cp .env.production .env
chmod +x scripts/validate-env.sh
```

### 2. Validate Configuration (30 sec)
```bash
set -a && source .env && set +a
./scripts/validate-env.sh
```

### 3. Start Optional Services (2 min)
```bash
# Jaeger (OTEL tracing)
docker run -d --name jaeger \
  -p 14250:14250 -p 16686:16686 \
  jaegertracing/all-in-one:latest

# Redis (caching - optional)
docker run -d --name redis -p 6379:6379 redis:alpine

# PostgreSQL (lockchain - optional)
docker run -d --name postgres \
  -e POSTGRES_DB=unrdf \
  -e POSTGRES_PASSWORD=changeme \
  -p 5432:5432 postgres:15
```

### 4. Initialize UNRDF (1 min)
```bash
# Install dependencies
npm ci

# Run tests
npm test

# Create directories
mkdir -p .unrdf/{hooks,policies,cache,logs}
```

### 5. Verify Installation (30 sec)
```bash
# Re-run validation with services
./scripts/validate-env.sh

# Test CLI
node cli/unrdf.mjs --version

# Import test data
node cli/unrdf.mjs store import test/e2e/cleanroom/fixtures/test-data.ttl

# Run a query
node cli/unrdf.mjs query "SELECT * WHERE { ?s ?p ?o } LIMIT 5"
```

## View Traces

Open Jaeger UI: http://localhost:16686

Search for service: `unrdf-cli`

## Next Steps

- Read full deployment guide: [DEPLOYMENT.md](./DEPLOYMENT.md)
- Configure policies: `.unrdf/policies/`
- Set up security: Enable TLS, audit logging
- Tune performance: Adjust cache and query limits

## Common Issues

**Issue**: Environment validation fails
```bash
# Check what's missing
./scripts/validate-env.sh | grep "Missing"

# Fix by editing .env
nano .env
```

**Issue**: Services not reachable
```bash
# Check Docker containers
docker ps

# Restart services
docker restart jaeger redis postgres
```

**Issue**: Import fails
```bash
# Check file format (must be .ttl, .nq, .nt, etc.)
# Check file size (default limit: 100MB)
export MAX_IMPORT_SIZE_MB=500

# Try with verbose logging
LOG_LEVEL=debug node cli/unrdf.mjs store import data.ttl
```

## Production Checklist

- [ ] `.env.production` copied to `.env`
- [ ] All required env vars set
- [ ] `./scripts/validate-env.sh` passes
- [ ] Jaeger running (optional but recommended)
- [ ] Redis running (optional for performance)
- [ ] PostgreSQL running (optional for lockchain)
- [ ] `npm test` passes
- [ ] Test query returns results
- [ ] Jaeger UI shows traces
- [ ] Logs appear in `.unrdf/logs/`

---

**Status**: Ready for Production

**Time to Deploy**: ~5 minutes

**Dependencies**: Node.js v18+, Docker (optional)
