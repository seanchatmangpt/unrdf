# pm4py-rust Replacement Plan

**Date:** 2026-04-11
**Goal:** Replace pm4py-rust (Rust) with Python pm4py (pm4py-mcp) in ChatmanGPT

---

## Current State Analysis

### pm4py-rust Integration Points

**1. BusinessOS Go Handler** (`bos_gateway.go`)

- Makes HTTP calls to `http://localhost:8090`
- 3 endpoints:
  - `POST /api/discovery/alpha` - Process model discovery
  - `POST /api/conformance/token-replay` - Fitness checking
  - `POST /api/statistics` - Log analysis
- Request/response schemas defined in Go structs

**2. Docker Compose** (`BusinessOS/docker-compose.yml`)

```yaml
pm4py-rust:
  build: ../pm4py-rust
  ports: ['8090:8090']
  healthcheck: curl -sf http://localhost:8090/api/health
```

**3. Tests**

- 15 test cases in `bos_gateway_pm4py_test.go`
- Uses mock pm4py-rust server on random port
- Validates response schema parsing

**4. Environment Variables**

- `PM4PY_RUST_URL` - Base URL (default: `http://localhost:8090`)

---

## Replacement Strategy

### Option A: HTTP API Wrapper (Recommended)

Wrap the Python pm4py-mcp tools with a Flask/FastAPI HTTP server that matches pm4py-rust's API contract.

**Pros:**

- Zero changes to BusinessOS Go code
- Backward compatible (same port, same API)
- Gradual migration (can run both in parallel)

**Cons:**

- New HTTP server to maintain
- Response schema translation layer needed

### Option B: Direct MCP Integration

Have BusinessOS call pm4py-mcp directly via MCP protocol.

**Pros:**

- Uses existing pm4py-mcp server
- More aligned with long-term architecture

**Cons:**

- Requires BusinessOS to implement MCP client
- Larger code changes
- More complex deployment

---

## Implementation Plan (Option A)

### Phase 1: Create HTTP API Wrapper

**File:** `~/chatmangpt/unrdf/otel/pm4py-http/server.py`

Create a FastAPI server that:

1. Wraps pm4py-mcp tools
2. Matches pm4py-rust's request/response schemas
3. Listens on port 8090

**Endpoints to implement:**

```python
# POST /api/discovery/alpha
@app.post("/api/discovery/alpha")
async def discovery_alpha(request: DiscoveryRequest):
    result = await pm4py_discover(
        hours=24,  # Not used - we have the full log
        limit=1000
    )
    # Transform to pm4py-rust schema
    return {
        "model_id": str(uuid.uuid4()),
        "algorithm": request.variant,
        "petri_net": {
            "places": [...],
            "transitions": [...],
            "arcs": [...]
        }
    }

# POST /api/conformance/token-replay
@app.post("/api/conformance/token-replay")
async def conformance_token_replay(request: ConformanceRequest):
    result = await pm4py_conformance(
        hours=24,
        limit=1000
    )
    return {
        "traces_checked": ...,
        "fitting_traces": ...,
        "fitness": ...,
        "precision": ...,
        "generalization": ...,
        "simplicity": ...
    }

# POST /api/statistics
@app.post("/api/statistics")
async def statistics(request: StatisticsRequest):
    result = await pm4py_full(...)  # Full analysis
    return {
        "trace_count": ...,
        "event_count": ...,
        "unique_activities": ...,
        "activity_frequency": [...],
        "case_duration": {...}
    }

# GET /api/health
@app.get("/api/health")
async def health():
    return {"status": "ok"}

# POST /api/io/parse-xes
@app.post("/api/io/parse-xes")
async def parse_xes(xes_content: str):
    # Use pm4py.read_xes + convert to JSON event log
    ...
```

### Phase 2: Docker Image

**File:** `~/chatmangpt/unrdf/otel/pm4py-http/Dockerfile`

```dockerfile
FROM python:3.14-slim

WORKDIR /app

# Install dependencies
COPY requirements.txt .
RUN pip install --no-cache-dir -r requirements.txt

# Copy pm4py-mcp and HTTP wrapper
COPY pm4py-mcp/ ./pm4py-mcp/
COPY server.py .

# Expose port 8090 (same as pm4py-rust)
EXPOSE 8090

# Run FastAPI server
CMD ["uvicorn", "server:app", "--host", "0.0.0.0", "--port", "8090"]
```

**requirements.txt:**

```
fastapi==0.115.0
uvicorn[standard]==0.30.0
pm4py>=2.7.0
pandas>=2.0.0
pydantic>=2.0.0
```

### Phase 3: Update docker-compose.yml

**File:** `BusinessOS/docker-compose.yml`

```yaml
# Replace pm4py-rust service with pm4py-python
pm4py-python:
  build:
    context: ../unrdf/otel/pm4py-http
    dockerfile: Dockerfile
  image: pm4py-python:local
  container_name: businessos-pm4py-python
  restart: unless-stopped
  ports:
    - '${PM4PY_PORT:-8090}:8090'
  environment:
    PM4PY_PORT: '8090'
    TEMPO_URL: 'http://tempo:3200'
    LOKI_URL: 'http://loki:3100'
    OTEL_SERVICE_NAME: 'pm4py-python'
  healthcheck:
    test: ['CMD-SHELL', 'curl -sf http://localhost:8090/api/health || exit 1']
    interval: 10s
    timeout: 5s
    retries: 3
    start_period: 15s
  networks:
    - businessos-network
```

**Environment variable rename (optional):**

- Keep `PM4PY_RUST_URL` for backward compatibility
- Or add `PM4PY_URL` as new standard

### Phase 4: Update BusinessOS Configuration

**File:** `BusinessOS/desktop/backend-go/internal/config/config_types.go`

```go
// Add PM4PY_URL (keep PM4PY_RUST_URL as alias for compat)
PM4PyURL string `mapstructure:"PM4PY_URL"`
PM4PyRustURL string `mapstructure:"PM4PY_RUST_URL"` // Deprecated
```

**File:** `BusinessOS/desktop/backend-go/internal/handlers/bos_gateway.go`

```go
// Try PM4PY_URL first, fall back to PM4PY_RUST_URL
pm4pyURL := "http://localhost:8090"
if envURL := os.Getenv("PM4PY_URL"); envURL != "" {
    pm4pyURL = envURL
} else if envURL := os.Getenv("PM4PY_RUST_URL"); envURL != "" {
    pm4pyURL = envURL
}
```

### Phase 5: Update Tests

**File:** `BusinessOS/desktop/backend-go/internal/handlers/bos_gateway_pm4py_test.go`

The mock server already tests HTTP contract - no changes needed if the new server returns the same schema.

**Verification:**

```bash
cd BusinessOS/desktop/backend-go
go test ./internal/handlers -run "TestDiscover|TestConformance|TestStatistics" -v
```

### Phase 6: Documentation Updates

**Files to update:**

1. `BusinessOS/CLAUDE.md` - Replace "pm4py-rust" with "pm4py-python"
2. `BusinessOS/desktop/backend-go/docs/BOS_GATEWAY_PM4PY_INTEGRATION.md` - Rewrite for Python
3. `CLAUDE.md` (root) - Update integration chain description
4. `make/includes/95-tps.mk` - Update health check if needed

---

## Schema Mapping

### pm4py-rust → pm4py-mcp Response Mapping

| Operation   | pm4py-rust Response       | pm4py-mcp Tool      | Mapping                                               |
| ----------- | ------------------------- | ------------------- | ----------------------------------------------------- |
| Discovery   | `petri_net.places[]`      | `pm4py_discover`    | `process_model.places` → `petri_net.places`           |
|             | `petri_net.transitions[]` |                     | `process_model.transitions` → `petri_net.transitions` |
|             | `petri_net.arcs[]`        |                     | `process_model.arcs` → `petri_net.arcs`               |
| Conformance | `fitness`                 | `pm4py_conformance` | `conformance.fitness.average_trace_fitness`           |
|             | `precision`               |                     | `conformance.precision`                               |
|             | `generalization`          |                     | Compute or default to 0.9                             |
|             | `simplicity`              |                     | Compute or default to 0.9                             |
| Statistics  | `trace_count`             | `pm4py_full`        | `trace_events` or `unique_traces`                     |
|             | `event_count`             |                     | `trace_events`                                        |
|             | `unique_activities`       |                     | `unique_activities`                                   |
|             | `activity_frequency[]`    |                     | Parse from full analysis                              |
|             | `case_duration`           |                     | Parse from full analysis                              |

---

## Rollback Plan

If issues arise:

1. Revert docker-compose.yml change
2. Rebuild pm4py-rust container
3. No code changes needed in BusinessOS (API contract unchanged)

---

## Success Criteria

- [ ] All 15 existing tests pass without modification
- [ ] `make dev` boots successfully with pm4py-python
- [ ] Discovery returns valid Petri net (places, transitions, arcs)
- [ ] Conformance returns fitness/precision in [0.0, 1.0]
- [ ] Statistics returns activity frequencies
- [ ] Health check responds to GET /api/health
- [ ] OTEL spans trace through BusinessOS → pm4py-python

---

## Open Questions

1. **Should we keep the Rust code?**
   - Archive to `attic/pm4py-rust/` for reference
   - Or delete entirely after verification

2. **Port mapping:**
   - Keep 8090 for backward compatibility
   - Or use new port (e.g., 8091) and update all references

3. **Response schema differences:**
   - pm4py-mcp may return different field names
   - Translation layer in HTTP wrapper handles this

4. **Performance:**
   - Python pm4py may be slower than Rust
   - Acceptable for current use case (analysis, not real-time)

---

## Files to Create

| File                                       | Purpose            |
| ------------------------------------------ | ------------------ |
| `~/unrdf/otel/pm4py-http/server.py`        | FastAPI wrapper    |
| `~/unrdf/otel/pm4py-http/Dockerfile`       | Container image    |
| `~/unrdf/otel/pm4py-http/requirements.txt` | Python deps        |
| `~/unrdf/otel/pm4py-http/README.md`        | Setup instructions |

---

## Files to Modify

| File                                                                  | Changes                           |
| --------------------------------------------------------------------- | --------------------------------- |
| `BusinessOS/docker-compose.yml`                                       | Replace pm4py-rust service        |
| `BusinessOS/desktop/backend-go/internal/handlers/bos_gateway.go`      | Update PM4PY_URL env var handling |
| `BusinessOS/desktop/backend-go/internal/config/config_types.go`       | Add PM4PY_URL field               |
| `BusinessOS/CLAUDE.md`                                                | Replace pm4py-rust references     |
| `BusinessOS/desktop/backend-go/docs/BOS_GATEWAY_PM4PY_INTEGRATION.md` | Rewrite for Python                |
| `CLAUDE.md`                                                           | Update integration chain          |
| `make/includes/95-tps.mk`                                             | Update health check (if needed)   |

---

**End of Replacement Plan**
