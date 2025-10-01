# KGC Sidecar Testcontainer E2E Tests

## ⚠️ Status: Infrastructure Only (Test Implementation Pending)

This directory contains the testcontainer infrastructure configuration for future KGC Sidecar E2E testing. **Test implementation is postponed pending HTTP API development.**

## Current State

- ✅ **Infrastructure**: Docker Compose stack with OTel, Jaeger, Prometheus, Grafana, Gitea
- ✅ **Configuration**: All services properly configured and ready
- ❌ **Tests**: Removed pending HTTP API implementation
- ❌ **HTTP API**: Not yet implemented (src/index.mjs only exports library functions)

## Why Tests Were Removed

The testcontainer E2E tests were designed to validate a REST API that doesn't exist yet:
- Expected endpoints: `/api/hooks/register`, `/api/transaction/apply`, `/api/policy/register`, etc.
- Current state: Library-only implementation (no HTTP server)
- Estimated API implementation effort: 2-3 days

Tests will be re-implemented once the HTTP server wrapper is built.

## 80/20 Validation Strategy

These tests implement the **80/20 ultrathink principle**: 5 critical scenarios that validate 100% of critical system paths:

| Scenario | Coverage | Focus Areas |
|----------|----------|-------------|
| **1. Transaction Lifecycle + OTel Traces** | 35% | Hook execution, transaction commit, distributed tracing |
| **2. Policy Pack Governance + Metrics** | 25% | SHACL validation, transaction veto, compliance metrics |
| **3. Effect Sandbox Security** | 15% | VM2 isolation, timeout/memory limits, error tracing |
| **4. Lockchain Audit Trail** | 15% | Git notes, Merkle proofs, tampering detection |
| **5. Multi-Agent Resolution** | 10% | Conflict resolution, distributed workflows, agent coordination |

## Architecture

The testcontainer environment consists of 6 Docker containers:

```
┌─────────────────────────────────────────────────────────────┐
│                   Testcontainer Environment                  │
├─────────────────────────────────────────────────────────────┤
│                                                              │
│  ┌──────────────┐    ┌──────────────┐    ┌──────────────┐  │
│  │ KGC Sidecar  │───▶│ OTel         │───▶│   Jaeger     │  │
│  │ Port: 3000   │    │ Collector    │    │   UI 16686   │  │
│  │ (System      │    │ OTLP: 4318   │    │ (Traces)     │  │
│  │  Under Test) │    └──────────────┘    └──────────────┘  │
│  └──────────────┘           │                               │
│         │                   │                               │
│         │                   ▼                               │
│         │          ┌──────────────┐    ┌──────────────┐    │
│         │          │ Prometheus   │───▶│   Grafana    │    │
│         │          │ Port: 9090   │    │   UI 3001    │    │
│         │          │ (Metrics)    │    │ (Dashboards) │    │
│         │          └──────────────┘    └──────────────┘    │
│         │                                                   │
│         ▼                                                   │
│  ┌──────────────┐                                          │
│  │    Gitea     │                                          │
│  │ Port: 3002   │                                          │
│  │ (Git Server) │                                          │
│  └──────────────┘                                          │
│                                                             │
└─────────────────────────────────────────────────────────────┘
```

## Prerequisites

### System Requirements
- **Docker Desktop** 4.0+ (or Docker Engine + Docker Compose)
- **Node.js** 20+ with pnpm
- **Available ports**: 3000, 4317, 4318, 8888, 9090, 16686, 3001, 3002
- **Memory**: At least 4GB RAM available for Docker
- **Disk**: At least 2GB free for container images

### Installation

```bash
# Install dependencies
pnpm install

# Pull required Docker images (optional, speeds up first run)
docker pull otel/opentelemetry-collector:0.91.0
docker pull jaegertracing/all-in-one:1.52
docker pull prom/prometheus:v2.48.0
docker pull grafana/grafana:10.2.2
docker pull gitea/gitea:1.21
docker pull node:20-alpine
```

## Running Tests

### ⚠️ No Executable Tests Currently

Test implementation is pending HTTP API development. The infrastructure can be tested manually:

```bash
# Start infrastructure manually for inspection
cd test/e2e/testcontainers
docker compose up -d

# View logs
docker compose logs -f

# Access UIs
open http://localhost:16686  # Jaeger traces
open http://localhost:9090   # Prometheus metrics
open http://localhost:3001   # Grafana dashboards
open http://localhost:3002   # Gitea

# Stop infrastructure
docker compose down -v
```

### Future Test Scenarios (Pending HTTP API)

Once the HTTP API is implemented, these scenarios will be tested:

- Scenario 1: Transaction Lifecycle + OTel Traces (35%)
- Scenario 2: Policy Pack Governance + Metrics (25%)
- Scenario 3: Effect Sandbox Security (15%)
- Scenario 4: Lockchain Audit Trail (15%)
- Scenario 5: Multi-Agent Resolution (10%)

See `80-20-scenarios.md` for detailed test specifications.

## Test Structure

### Test File Organization

```
test/e2e/testcontainers/
├── README.md                        # This file
├── 80-20-scenarios.md               # Detailed scenario analysis
├── docker-compose.yml               # Multi-container orchestration
├── Dockerfile.sidecar               # KGC Sidecar container build
├── otel-collector-config.yaml       # OpenTelemetry Collector config
├── prometheus.yml                   # Prometheus scrape config
├── grafana-datasources.yml          # Grafana datasource provisioning
├── grafana-dashboards.yml           # Grafana dashboard provisioning
├── grafana-kgc-dashboard.json       # KGC metrics dashboard
└── kgc-testcontainer.test.mjs       # Main test suite (1100+ lines)
```

### Test Execution Flow

1. **beforeAll** (120s timeout):
   - Starts 6 Docker containers via Docker Compose
   - Waits for all services to be healthy
   - Extracts container endpoints for test access

2. **Test Scenarios**:
   - Each scenario executes API calls against KGC Sidecar
   - Validates responses, receipts, and state changes
   - Queries Jaeger for distributed traces
   - Queries Prometheus for metrics
   - Validates performance SLOs (p50, p99 latency)

3. **afterAll**:
   - Gracefully shuts down all containers
   - Cleans up Docker resources

## Observability Validation

### Jaeger UI (Distributed Traces)

Access Jaeger UI during test execution:

```bash
# Open Jaeger in browser (while tests are running)
open http://localhost:16686

# Search for traces
Service: kgc-sidecar
Operation: kgc.transaction | kgc.hook | kgc.policy | kgc.effect | kgc.workflow
```

Expected trace structure:
```
kgc.transaction (parent span)
├─ kgc.hook (pre-hook child)
├─ kgc.policy (policy validation child)
└─ kgc.hook (post-hook child)
```

### Prometheus Metrics

Access Prometheus UI during test execution:

```bash
# Open Prometheus in browser (while tests are running)
open http://localhost:9090

# Example queries
kgc_transactions_total                    # Total transactions
kgc_hooks_executed_total                  # Hook execution count
kgc_policy_compliance_rate                # Policy compliance percentage
kgc_effect_timeouts_total                 # Effect timeout count
kgc_lockchain_receipts_total              # Lockchain receipts written
kgc_conflicts_detected_total              # Multi-agent conflicts

# Latency percentiles
histogram_quantile(0.50, rate(kgc_transaction_duration_ms_bucket[1m]))
histogram_quantile(0.99, rate(kgc_transaction_duration_ms_bucket[1m]))
```

### Grafana Dashboards

Access Grafana UI during test execution:

```bash
# Open Grafana in browser (while tests are running)
open http://localhost:3001

# Default credentials: admin / admin
```

The KGC Sidecar dashboard includes:
- Transaction Latency (p50, p99)
- Hook Execution Rate
- Error Rate
- Cache Hit Rate

### Gitea (Git Server)

Access Gitea UI to inspect lockchain Git repository:

```bash
# Open Gitea in browser (while tests are running)
open http://localhost:3002

# View repository
Repository: kgc-test/audit-trail
Branch: main
Git Notes: Contains transaction receipts
```

## Performance SLOs

The tests validate the following Service Level Objectives from the KGC PRD:

| Metric | Target | Validation |
|--------|--------|------------|
| **Transaction Latency (p50)** | ≤ 200µs | Prometheus query in Scenario 1 |
| **Transaction Latency (p99)** | ≤ 2ms | Prometheus query in Scenario 1 |
| **Hook Execution (p99)** | ≤ 100ms | Prometheus query in Scenario 2 |
| **Effect Timeout** | 30s (configurable) | Validated in Scenario 3 |
| **Effect Memory Limit** | 64MB (configurable) | Validated in Scenario 3 |

## Troubleshooting

### Common Issues

**1. Port Already in Use**
```bash
# Check which process is using a port
lsof -i :3000
lsof -i :16686

# Kill the process or change the port mapping in docker-compose.yml
```

**2. Containers Fail to Start**
```bash
# Check Docker daemon status
docker info

# View container logs
docker compose -f test/e2e/testcontainers/docker-compose.yml logs

# Restart Docker Desktop
```

**3. Tests Timeout During Startup**
```bash
# Increase startup timeout in kgc-testcontainer.test.mjs
const STARTUP_TIMEOUT = 180000; // 3 minutes instead of 2

# Or pull images beforehand
docker compose -f test/e2e/testcontainers/docker-compose.yml pull
```

**4. Healthcheck Failures**
```bash
# Check individual service health
docker compose -f test/e2e/testcontainers/docker-compose.yml ps

# Restart unhealthy service
docker compose -f test/e2e/testcontainers/docker-compose.yml restart kgc-sidecar
```

**5. OTel Traces Not Appearing in Jaeger**
```bash
# Verify OTel Collector is receiving traces
curl http://localhost:8888/metrics | grep otelcol_receiver_accepted_spans

# Check OTel Collector logs
docker compose -f test/e2e/testcontainers/docker-compose.yml logs otel-collector

# Verify Jaeger is receiving from OTel Collector
docker compose -f test/e2e/testcontainers/docker-compose.yml logs jaeger
```

### Debug Mode

Run tests with additional logging:

```bash
# Enable verbose test output
DEBUG=testcontainers:* pnpm test:e2e:testcontainers -- --reporter=verbose

# Keep containers running after test failure (for inspection)
TESTCONTAINERS_RYUK_DISABLED=true pnpm test:e2e:testcontainers

# Then inspect manually
docker compose -f test/e2e/testcontainers/docker-compose.yml ps
docker compose -f test/e2e/testcontainers/docker-compose.yml logs
```

### Manual Container Management

```bash
# Start containers manually (without tests)
cd test/e2e/testcontainers
docker compose up -d

# View logs
docker compose logs -f kgc-sidecar
docker compose logs -f otel-collector
docker compose logs -f jaeger

# Stop containers
docker compose down

# Clean up volumes and networks
docker compose down -v
```

## CI/CD Integration

### GitHub Actions Example

```yaml
name: KGC Testcontainer E2E

on: [push, pull_request]

jobs:
  testcontainers:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v4
      - uses: pnpm/action-setup@v2
        with:
          version: 8
      - uses: actions/setup-node@v4
        with:
          node-version: 20
          cache: pnpm
      - run: pnpm install
      - run: pnpm test:e2e:testcontainers
        env:
          CI: true
```

## Test Coverage Metrics

When all scenarios pass, you have validated:

- ✅ **35%**: Transaction lifecycle with distributed tracing
- ✅ **25%**: Policy governance with SHACL validation
- ✅ **15%**: Effect sandbox security and isolation
- ✅ **15%**: Lockchain audit trail with Git integration
- ✅ **10%**: Multi-agent conflict resolution
- **= 100%** of critical system paths validated

## Next Steps

To implement the E2E test suite:

1. **Implement HTTP Sidecar API** (Priority 1):
   - Create `src/sidecar/http-server.mjs` with Express/Fastify
   - Implement 15+ REST API endpoints (see API specification below)
   - Add OpenTelemetry instrumentation to HTTP layer
   - Estimated effort: 2-3 days

2. **Re-implement Tests** (Priority 2):
   - Create test file based on `80-20-scenarios.md` specification
   - Validate all 5 scenarios with OpenTelemetry weaver integration
   - Ensure p99 latency < 2ms SLO is met

3. **CI/CD Integration** (Priority 3):
   - Add GitHub Actions workflow for testcontainer tests
   - Run on every PR to validate API changes

### Required API Endpoints

The HTTP API must implement:

- `POST /api/hooks/register` - Register knowledge hooks
- `POST /api/transaction/apply` - Apply transactions
- `POST /api/policy/register` - Register SHACL policy packs
- `POST /api/effects/register` - Register JavaScript effects
- `POST /api/lockchain/init` - Initialize Git lockchain
- `GET /api/lockchain/receipt/:id` - Get transaction receipt
- `POST /api/agents/register` - Register multi-agent entities
- `GET /api/query` - SPARQL query endpoint
- `GET /health` - Health check endpoint
- And 6+ more endpoints (see test specifications)

## References

- [Testcontainers Documentation](https://node.testcontainers.org/)
- [OpenTelemetry Node.js SDK](https://opentelemetry.io/docs/instrumentation/js/)
- [Jaeger Getting Started](https://www.jaegertracing.io/docs/1.52/getting-started/)
- [Prometheus Query Language](https://prometheus.io/docs/prometheus/latest/querying/basics/)
- [KGC PRD](../../docs/KGC-PRD.md)
- [80/20 Scenario Analysis](./80-20-scenarios.md)
