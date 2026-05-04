# pm4py-mcp → pm4py-mcp Migration: Second-Order Issues & Plan

**Date:** 2026-04-11
**Scope:** Complete migration from pm4py-mcp (Rust, HTTP, port 7015) to pm4py-mcp (Python, MCP protocol)

---

## Executive Summary

**Impact Scale:** 47 files across 6 systems, ~3,500 lines of code
**Estimated Effort:** 8-10 weeks
**Risk Level:** HIGH (protocol-level change)

**Systems Affected:**

1. BusinessOS (Go backend) - CRITICAL
2. Canopy (Elixir workflows) - MEDIUM
3. OSA (scheduler) - MEDIUM
4. Docker orchestration (compose files) - HIGH
5. Shell scripts (ops/monitoring) - MEDIUM
6. Documentation (all reference docs) - LOW

---

## 1. Critical Dependencies (Must Address First)

### 1.1 BusinessOS Go Handler - PRIMARY BLOCKER

**File:** `BusinessOS/desktop/backend-go/internal/handlers/bos_gateway.go`

**Current Architecture:**

```
Go Handler → HTTP Client → pm4py-mcp:7015 → Response
```

**Required Changes:**

- Remove HTTP client (lines 84-90)
- Remove pm4py-mcp URL constants (lines 28-34)
- Implement MCP client (NEW)
- Rewrite 3 handlers: Discover, CheckConformance, GetStatistics
- Update XES parsing to use pm4py-mcp tool

**Breaking Changes:**

- All request/response schemas change (HTTP JSON → MCP JSON-RPC)
- Error codes change (HTTP 503 → MCP -32600)
- Timeout semantics differ

### 1.2 Canopy Elixir Workflows

**File:** `canopy/backend/lib/canopy/workflows/process_discovery.ex`

**Required Changes:**

- Replace HTTPoison calls with MCP client
- Update GenServer state to hold MCP connection
- Rewrite 3 workflow steps

### 1.3 OSA Scheduler

**File:** `lib/osa/monitoring/process_monitoring_scheduler.ex`

**Required Changes:**

- Same as Canopy - Elixir MCP client needed

---

## 2. MCP Client Implementation (NEW DEPENDENCY)

### 2.1 Go MCP Client Required

**Package:** Need Go library for MCP protocol (JSON-RPC 2.0)

**Options:**

1. `github.com/markbates/go-jsonrpc` - JSON-RPC 2.0 client
2. Custom implementation using `net/rpc/jsonrpc`
3. MCP-specific Go library (if exists)

**Implementation:**

```go
// internal/mcp/client.go
type MCPClient struct {
    transport string  // "stdio" or "http"
    endpoint  string  // server path or URL
    timeout   time.Duration
}

func (c *MCPClient) CallTool(ctx context.Context, name string, params map[string]interface{}) (json.RawMessage, error) {
    // JSON-RPC 2.0 request
    request := JSONRPCRequest{
        JSONRPC: "2.0",
        ID:      generateID(),
        Method:  "tools/call",
        Params: map[string]interface{}{
            "name":   name,
            "arguments": params,
        },
    }

    // Send request, parse response
    // Handle MCP-specific errors
}
```

### 2.2 Elixir MCP Client Required

**Package:** Need Elixir library for MCP protocol

**Options:**

1. `mcp_client` from Hex (if available)
2. Custom implementation using `JSON-RPC 2.0`
3. Wrap Python pm4py-mcp in HTTP (MCP-over-HTTP)

---

## 3. Configuration Migration

### 3.1 Environment Variables

**Current Pattern (47 files):**

```bash
PM4PY_MCP_URL=http://localhost:7015
PM4PY_MCP_PORT=7015
```

**New Pattern:**

```bash
PM4PY_MCP_URL=http://localhost:7015  # or stdio path
PM4PY_MCP_PORT=7015
```

**Transition Strategy:**

- Support BOTH env vars during migration
- Deprecation warning for PM4PY_MCP_URL
- Default to PM4PY_MCP_URL if both present

### 3.2 Config Struct Updates

**File:** `internal/config/config_types.go`

```go
type Config struct {
    // NEW
    PM4PYMCPURL string `mapstructure:"PM4PY_MCP_URL"`

    // DEPRECATED (keep for transition)
    PM4PyRustURL string `mapstructure:"PM4PY_MCP_URL"`
}
```

---

## 4. Docker Orchestration Changes

### 4.1 Service Replacement

**Current (docker-compose.yml):**

```yaml
pm4py-mcp:
  build: ../pm4py-mcp
  ports: ['7015:7015']
  healthcheck:
    test: ['CMD-SHELL', 'curl -sf http://localhost:7015/api/health']
```

**New:**

```yaml
pm4py-mcp:
  image: pm4py-mcp:latest
  # OR build from ../unrdf/otel/pm4py-mcp
  ports: ['7015:7015']
  environment:
    PM4PY_MCP_PORT: '7015'
    TEMPO_URL: 'http://tempo:3200'
    LOKI_URL: 'http://loki:3100'
  healthcheck:
    test: ['CMD', 'python3', '-c', 'import mcp; mcp.ping()']
```

### 4.2 Network Dependencies

**Files to Update:**

- `BusinessOS/docker-compose.yml` (lines 72-98, 255, 277-278)
- `docker-compose.yml` (lines 151-190, 221)
- 27 other docker-compose.yml files in worktrees

### 4.3 Health Check Changes

**Current:** HTTP GET /api/health
**New:** MCP ping or health endpoint

---

## 5. Test Suite Refactoring (MAJOR EFFORT)

### 5.1 BusinessOS Tests

**File:** `internal/handlers/bos_gateway_pm4py_test.go`

**Current:** 15 tests using mock HTTP server (lines 154-583)

**Required:**

- Remove mock HTTP server
- Implement mock MCP server
- Rewrite all 15 test cases
- Test MCP error handling (timeout, invalid params, etc.)

**New Test Pattern:**

```go
func TestDiscoverRealMCP_Success(t *testing.T) {
    mockMCP := NewMockMCPServer()
    mockMCP.ExpectToolCall("pm4py_discover", map[string]interface{}{
        "event_log": json.RawMessage(`[]`),
        "variant": "inductive_miner",
    }).Return(json.RawMessage(`{
        "model_id": "test",
        "process_model": {"places": 5, "transitions": 8}
    }`))

    handler := &BOSGatewayHandler{mcpClient: mockMCP}
    // ... test code ...
}
```

### 5.2 Canopy Tests

**Files:**

- `canopy/backend/test/canopy/adapters/pm4py_mcp_test.exs`
- `canopy/backend/test/canopy/workflows/process_discovery_test.exs`

**Required:** Same pattern - mock MCP server

---

## 6. Semantic Conventions & Observability

### 6.1 Span Name Updates

**Files:**

- `internal/semconv/bos_span_names.go`
- `internal/semconv/mining_span_names.go`

**Current:**

```go
const BosGatewayDiscoverSpan = "bos.gateway.discover"
const BosGatewayPM4PyRustSpan = "pm4py_mcp.discover"
```

**New:**

```go
const BosGatewayPM4PyMCPSpan = "pm4py_mcp.discover"
```

### 6.2 Registry Schema Updates

**Files:**

- `semconv/model/business_os/spans.yaml`
- `semconv/model/business_os/registry.yaml`

**Required:**

- Update span descriptions
- Change protocol from HTTP to MCP
- Update attribute types

---

## 7. Shell Script Updates (Operational Impact)

### 7.1 Test Scripts

**Files:**

- `scripts/wave9_agent1_xxe_verification.sh` (lines 13, 33)
- `scripts/a2a-stack-work-test.sh` (line 7)
- `BusinessOS/desktop/backend-go/scripts/test-pm4py-integration.sh` (line 18)

**Changes:**

- `PM4PY_MCP_URL` → `PM4PY_MCP_URL`
- Port 7015 → 7015
- Health check endpoint changes

### 7.2 Monitoring Scripts

**Files:**

- `scripts/recovery_monitor.sh` (line 140)
- `scripts/chaos_recovery_test.sh` (line 28)
- `scripts/e2e-chain-smoke-test.sh` (line 355)

**Changes:**

- Service name: pm4py-mcp → pm4py-mcp
- Port monitoring: 7015 → 7015
- Recovery logic for MCP protocol

---

## 8. Documentation Overhaul

### 8.1 Files to Update (47+ documents)

**Integration Guides:**

- `docs/integrations/BUSINESSOS_PM4PY_MCP_API.md` - COMPLETE REWRITE
- `docs/CANOPY_PM4PY_INTEGRATION.md` - COMPLETE REWRITE
- `docs/CROSS_PROJECT_INTEGRATION_ARCHITECTURE.md` - Update diagrams

**API Documentation:**

- `BusinessOS/docs/diataxis/reference/configuration-options.md` (line 195)
- `BusinessOS/docs/diataxis/reference/api-endpoints.md` (line 545)
- `BusinessOS/docs/diataxis/how-to/bos-gateway-pattern.md` (line 31)

**Operations Guides:**

- `docs/OPERATIONS_GUIDE_WAVE_9.md` (line 219)
- `docs/WAVE_7_COMPLETION_SUMMARY.md` (line 34)

**AI Context:**

- `BusinessOS/CLAUDE.md` (lines 133, 164)
- `canopy/CLAUDE.md` (lines 65, 66)
- Root `CLAUDE.md` - Integration chain description

### 8.2 New Documentation Required

1. **MCP Client Configuration Guide**
   - How to configure Go MCP client
   - How to configure Elixir MCP client
   - Connection pooling and timeouts

2. **Migration Guide**
   - Step-by-step migration process
   - Rollback procedures
   - Troubleshooting common issues

3. **Troubleshooting Guide**
   - MCP-specific errors
   - Performance debugging
   - Protocol-level debugging

---

## 9. Performance & Error Handling Changes

### 9.1 Protocol Overhead

**HTTP → MCP:**

- Additional JSON-RPC layer
- Tool invocation overhead
- Response parsing differences

**Estimated Impact:**

- Latency increase: 10-50ms per call
- Throughput decrease: ~20%

**Mitigation:**

- Connection pooling for MCP client
- Async tool invocation where possible
- Cache results where appropriate

### 9.2 Error Code Changes

**Current HTTP Errors:**

- 502 Bad Gateway
- 503 Service Unavailable
- 504 Gateway Timeout

**New MCP Errors:**

- -32600 Invalid Request
- -32601 Tool not found
- -32602 Invalid arguments
- -32603 Internal error

**Required:**

- Update all error handling code
- Map MCP errors to appropriate HTTP responses
- Update retry logic for MCP error codes

### 9.3 Timeout Semantics

**Current:** HTTP client timeout (30 seconds)
**New:** MCP call timeout (may differ)

**Required:**

- Implement timeout per MCP tool call
- Handle long-running operations (async)
- Update timeout configurations

---

## 10. Migration Phases (Recommended Timeline)

### Phase 1: Foundation (Weeks 1-2)

**Goal:** Implement MCP clients

**Tasks:**

1. Implement Go MCP client library
2. Implement Elixir MCP client library
3. Set up pm4py-mcp server in development
4. Create feature branch: `feat/pm4py-mcp-migration`
5. Set up staging environment

**Deliverables:**

- Working Go MCP client
- Working Elixir MCP client
- pm4py-mcp server running locally

### Phase 2: Core Migration (Weeks 3-5)

**Goal:** Migrate core handlers

**Tasks:**

1. Update BusinessOS bos_gateway.go
2. Update BusinessOS ocpm.go
3. Update Canopy process_discovery.ex
4. Update OSA process_monitoring_scheduler.ex
5. Implement dual operation (both services running)

**Deliverables:**

- All handlers using MCP clients
- Both pm4py-mcp and pm4py-mcp running
- Feature flag for protocol selection

### Phase 3: Testing (Weeks 6-7)

**Goal:** Comprehensive test coverage

**Tasks:**

1. Rewrite BusinessOS test suite (15 tests)
2. Rewrite Canopy test suite
3. Rewrite OSA test suite
4. Integration testing with both services
5. Performance testing

**Deliverables:**

- All tests passing with pm4py-mcp
- Performance baseline established
- Integration test suite

### Phase 4: Infrastructure (Week 8)

**Goal:** Update deployment

**Tasks:**

1. Update all docker-compose.yml files (30+ files)
2. Update all shell scripts (15+ files)
3. Update CI/CD pipelines
4. Update monitoring dashboards
5. Update runbooks

**Deliverables:**

- All infrastructure updated
- CI/CD pipeline updated
- Monitoring configured

### Phase 5: Documentation (Week 9)

**Goal:** Update all docs

**Tasks:**

1. Update 47 documentation files
2. Write MCP client configuration guide
3. Write migration guide
4. Write troubleshooting guide
5. Update CLAUDE.md files

**Deliverables:**

- All documentation updated
- New guides written
- AI context updated

### Phase 6: Rollout (Week 10)

**Goal:** Production deployment

**Tasks:**

1. Canary deployment (10% traffic)
2. Monitor metrics closely
3. Gradual rollout to 100%
4. Keep pm4py-mcp available for rollback
5. Remove pm4py-mcp after 30 days

**Deliverables:**

- Production running pm4py-mcp
- Rollback plan documented
- pm4py-mcp decommissioned

---

## 11. Risk Mitigation Strategies

### 11.1 High-Risk Areas

**1. BusinessOS Handlers (CRITICAL)**

- Risk: Core business logic breaks
- Mitigation: Feature flags, dual operation, extensive testing

**2. Docker Orchestration (HIGH)**

- Risk: Deployment failures
- Mitigation: Staging environment, gradual rollout

**3. Test Suites (MEDIUM)**

- Risk: Coverage gaps
- Mitigation: 100% coverage requirement, integration tests

### 11.2 Rollback Plan

**Trigger Conditions:**

- Error rate > 1% on pm4py-mcp
- Latency > 2x baseline
- Any data corruption detected

**Rollback Steps:**

1. Switch feature flag to pm4py-mcp
2. Restart affected services
3. Verify health checks pass
4. Investigate failure in staging

**Rollback Window:** Keep pm4py-mcp deployable for 30 days post-migration

### 11.3 Success Criteria

**Migration Complete When:**

- [ ] All 47 files updated
- [ ] All tests passing with pm4py-mcp
- [ ] Performance within 20% of baseline
- [ ] Error rate < 0.1%
- [ ] Documentation updated
- [ ] pm4py-mcp decommissioned

---

## 12. Open Questions & Decisions Needed

### 12.1 MCP Server Hosting

**Question:** Run pm4py-mcp as separate service or embed in BusinessOS?

**Options:**
A. Separate service (current plan)
B. Embedded in BusinessOS container
C. Sidecar pattern

**Recommendation:** Separate service for flexibility

### 12.2 MCP Transport Protocol

**Question:** Use stdio or HTTP for MCP?

**Options:**
A. stdio (faster, but requires same host)
B. HTTP (slower, but networked)

**Recommendation:** HTTP for microservices architecture

### 12.3 Port Allocation

**Question:** What port for pm4py-mcp?

**Options:**
A. 7015 (ostar default)
B. 7015 (keep pm4py-mcp port)
C. New port (8091)

**Recommendation:** 7015 for backward compatibility, change later

### 12.4 Feature Flag Implementation

**Question:** How to implement dual operation?

**Options:**
A. Environment variable (PM4PY_PROTOCOL=mcp|http)
B. Config file setting
C. Runtime API toggle

**Recommendation:** Environment variable for simplicity

---

## 13. Checklist Summary

### Code Changes (24 files)

- [ ] BusinessOS: bos_gateway.go
- [ ] BusinessOS: ocpm.go
- [ ] BusinessOS: config_types.go
- [ ] BusinessOS: config_load.go
- [ ] BusinessOS: routes.go
- [ ] BusinessOS: mcp/client.go (NEW)
- [ ] BusinessOS: mcp/pm4py_tools.go (NEW)
- [ ] Canopy: process_discovery.ex
- [ ] Canopy: jtbd/runner.ex
- [ ] OSA: process_monitoring_scheduler.ex
- [ ] Canopy: mcp/client.ex (NEW)
- [ ] OSA: mcp/client.ex (NEW)
- [ ] Test files (rewrite all)

### Infrastructure Changes (30+ files)

- [ ] BusinessOS/docker-compose.yml
- [ ] docker-compose.yml
- [ ] 27 other docker-compose.yml files
- [ ] 15 shell scripts
- [ ] CI/CD pipelines

### Documentation Changes (47+ files)

- [ ] All CLAUDE.md files
- [ ] All integration guides
- [ ] All API documentation
- [ ] All operations guides
- [ ] New: MCP client guide
- [ ] New: Migration guide
- [ ] New: Troubleshooting guide

### Testing Changes

- [ ] Rewrite 15 BusinessOS tests
- [ ] Rewrite Canopy tests
- [ ] Rewrite OSA tests
- [ ] Integration tests
- [ ] Performance tests

---

**End of Second-Order Issues Analysis**

**Next Steps:**

1. Review this plan with stakeholders
2. Decide on open questions
3. Create feature branch
4. Begin Phase 1: MCP client implementation
