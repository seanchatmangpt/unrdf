# pm4py-mcp Replacement Plan — Direct MCP Integration

**Date:** 2026-04-11
**Goal:** BusinessOS Go calls pm4py-mcp tools directly via MCP protocol

---

## Current State

**BusinessOS → pm4py-mcp (HTTP on port 7015):**

```
Go Handler → HTTP POST → pm4py-mcp → Response
```

**New: BusinessOS → pm4py-mcp (MCP protocol):**

```
Go Handler → MCP Client → pm4py-mcp → Response
```

---

## Implementation Plan

### Phase 1: Add pm4py-mcp Tools to BusinessOS MCP Client

**File:** `BusinessOS/desktop/backend-go/internal/mcp-tools/pm4py_tools.go`

```go
package mcptools

import (
    "context"
    "encoding/json"
    "fmt"
)

// PM4Py MCP Tool Calls
// See: ~/unrdf/otel/pm4py-mcp/pm4py_mcp/server.py

// PM4PyDiscover calls pm4py_discover MCP tool
func PM4PyDiscover(ctx context.Context, eventLog json.RawMessage, variant string) (*DiscoverResult, error) {
    result, err := mcpClient.CallTool(ctx, "pm4py_discover", map[string]interface{}{
        "event_log": eventLog,
        "variant":   variant,
    })
    if err != nil {
        return nil, fmt.Errorf("pm4py_discover: %w", err)
    }

    var resp DiscoverResult
    if err := json.Unmarshal(result, &resp); err != nil {
        return nil, fmt.Errorf("parse discover response: %w", err)
    }
    return &resp, nil
}

// PM4PyConformance calls pm4py_conformance MCP tool
func PM4PyConformance(ctx context.Context, eventLog, petriNet json.RawMessage) (*ConformanceResult, error) {
    result, err := mcpClient.CallTool(ctx, "pm4py_conformance", map[string]interface{}{
        "event_log": eventLog,
        "petri_net": petriNet,
    })
    if err != nil {
        return nil, fmt.Errorf("pm4py_conformance: %w", err)
    }

    var resp ConformanceResult
    if err := json.Unmarshal(result, &resp); err != nil {
        return nil, fmt.Errorf("parse conformance response: %w", err)
    }
    return &resp, nil
}

// PM4PyStatistics calls pm4py_full MCP tool (includes statistics)
func PM4PyStatistics(ctx context.Context, eventLog json.RawMessage) (*StatisticsResult, error) {
    result, err := mcpClient.CallTool(ctx, "pm4py_full", map[string]interface{}{
        "event_log": eventLog,
    })
    if err != nil {
        return nil, fmt.Errorf("pm4py_full: %w", err)
    }

    var resp StatisticsResult
    if err := json.Unmarshal(result, &resp); err != nil {
        return nil, fmt.Errorf("parse statistics response: %w", err)
    }
    return &resp, nil
}

// Response Types (matching pm4py-mcp output)

type DiscoverResult struct {
    ModelID     string         `json:"model_id"`
    Algorithm   string         `json:"algorithm"`
    ProcessModel ProcessModel   `json:"process_model"`
    TraceEvents int            `json:"trace_events"`
    UniqueTraces int            `json:"unique_traces"`
}

type ProcessModel struct {
    Places      int `json:"places"`
    Transitions int `json:"transitions"`
    Arcs        int `json:"arcs"`
}

type ConformanceResult struct {
    Fitness        float64 `json:"fitness"`
    Precision      float64 `json:"precision"`
    Generalization float64 `json:"generalization"`
    Simplicity     float64 `json:"simplicity"`
    TraceEvents    int     `json:"trace_events"`
}

type StatisticsResult struct {
    TraceEvents        int                    `json:"trace_events"`
    UniqueTraces       int                    `json:"unique_traces"`
    UniqueActivities   int                    `json:"unique_activities"`
    ActivityFrequency  []ActivityFrequency    `json:"activity_frequency"`
    CaseDuration       CaseDuration           `json:"case_duration"`
    Bottleneck         []BottleneckActivity    `json:"bottleneck"`
    Variants           []Variant               `json:"variants"`
}

type ActivityFrequency struct {
    Activity   string  `json:"activity"`
    Frequency  int     `json:"frequency"`
    Percentage float64 `json:"percentage"`
}

type CaseDuration struct {
    MinSeconds    float64 `json:"min_seconds"`
    MaxSeconds    float64 `json:"max_seconds"`
    AvgSeconds    float64 `json:"avg_seconds"`
    MedianSeconds float64 `json:"median_seconds"`
}

type BottleneckActivity struct {
    Activity   string  `json:"activity"`
    MeanMs     float64 `json:"mean_ms"`
    MedianMs   float64 `json:"median_ms"`
}

type Variant struct {
    Variant    string `json:"variant"`
    Count      int    `json:"count"`
    Percentage float64 `json:"percentage"`
}
```

### Phase 2: Update BOSGatewayHandler to Use MCP Tools

**File:** `BusinessOS/desktop/backend-go/internal/handlers/bos_gateway.go`

**Remove:**

- `pm4pyURL` field
- `httpClient` field
- All HTTP request code

**Add:**

- Import `mcptools` package
- Call MCP tools directly

```go
import (
    "github.com/rhl/businessos-backend/internal/mcptools"
)

// Discover handles POST /api/bos/discover
func (h *BOSGatewayHandler) Discover(c *gin.Context) {
    startTime := time.Now()

    // ... (existing validation code) ...

    // Load event log
    eventLog, err := h.loadEventLogForGateway(c.Request.Context(), req.LogPath)
    if err != nil {
        // ... error handling ...
    }

    // Call pm4py-mcp directly (no HTTP!)
    result, err := mcptools.PM4PyDiscover(c.Request.Context(), eventLog, req.Algorithm)
    if err != nil {
        h.logger.Error("discover: pm4py-mcp call failed", "error", err.Error())
        c.JSON(http.StatusServiceUnavailable, gin.H{"error": "pm4py-mcp unavailable"})
        return
    }

    response := BOSDiscoverResponse{
        ModelID:     result.ModelID,
        Algorithm:   result.Algorithm,
        Places:      result.ProcessModel.Places,
        Transitions: result.ProcessModel.Transitions,
        Arcs:        result.ProcessModel.Arcs,
        LatencyMs:   uint64(time.Since(startTime).Milliseconds()),
    }

    // ... (WAL, persist, cleanup code unchanged) ...

    c.JSON(http.StatusOK, response)
}

// CheckConformance handles POST /api/bos/conformance
func (h *BOSGatewayHandler) CheckConformance(c *gin.Context) {
    startTime := time.Now()

    // ... (existing validation code) ...

    eventLog, err := h.loadEventLogForGateway(c.Request.Context(), req.LogPath)
    if err != nil {
        // ... error handling ...
    }

    // Load Petri net
    var petriNetRaw json.RawMessage
    if req.ModelPath != "" {
        petriNetRaw, err = h.loadEventLogForGateway(c.Request.Context(), req.ModelPath)
    } else {
        // Recover from WAL
        walResult, walErr := h.recoverFromWAL(req.ModelID)
        if walErr == nil && walResult != nil {
            var modelMap map[string]json.RawMessage
            json.Unmarshal(walResult.ModelData, &modelMap)
            petriNetRaw = modelMap["petri_net"]
        }
    }

    // Call pm4py-mcp directly
    result, err := mcptools.PM4PyConformance(c.Request.Context(), eventLog, petriNetRaw)
    if err != nil {
        h.logger.Error("conformance: pm4py-mcp call failed", "error", err.Error())
        c.JSON(http.StatusServiceUnavailable, gin.H{"error": "pm4py-mcp unavailable"})
        return
    }

    response := BOSConformanceResponse{
        Fitness:        result.Fitness,
        Precision:      result.Precision,
        Generalization: result.Generalization,
        Simplicity:     result.Simplicity,
        LatencyMs:      uint64(time.Since(startTime).Milliseconds()),
    }

    // ... (DB update, logging unchanged) ...

    c.JSON(http.StatusOK, response)
}

// GetStatistics handles POST /api/bos/statistics
func (h *BOSGatewayHandler) GetStatistics(c *gin.Context) {
    startTime := time.Now()

    // ... (existing validation code) ...

    eventLog, err := h.loadEventLogForGateway(c.Request.Context(), req.LogPath)
    if err != nil {
        // ... error handling ...
    }

    // Call pm4py-mcp directly
    result, err := mcptools.PM4PyStatistics(c.Request.Context(), eventLog)
    if err != nil {
        h.logger.Error("statistics: pm4py-mcp call failed", "error", err.Error())
        c.JSON(http.StatusServiceUnavailable, gin.H{"error": "pm4py-mcp unavailable"})
        return
    }

    // Map to response format
    activityFreq := make([]BOSActivityStatistic, len(result.ActivityFrequency))
    for i, af := range result.ActivityFrequency {
        activityFreq[i] = BOSActivityStatistic{
            Activity:   af.Activity,
            Frequency:  af.Frequency,
            Percentage: af.Percentage,
        }
    }

    caseDuration := BOSCaseDurationStatistic{
        MinSeconds:    int64(result.CaseDuration.MinSeconds),
        MaxSeconds:    int64(result.CaseDuration.MaxSeconds),
        AvgSeconds:    result.CaseDuration.AvgSeconds,
        MedianSeconds: result.CaseDuration.MedianSeconds,
    }

    response := BOSStatisticsResponse{
        NumTraces:           result.UniqueTraces,
        NumEvents:           result.TraceEvents,
        NumUniqueActivities: result.UniqueActivities,
        ActivityFrequency:   activityFreq,
        CaseDuration:        caseDuration,
        LatencyMs:           uint64(time.Since(startTime).Milliseconds()),
    }

    c.JSON(http.StatusOK, response)
}
```

### Phase 3: Configure MCP Client for pm4py-mcp

**File:** `BusinessOS/desktop/backend-go/internal/mcp-tools/client.go`

Add pm4py-mcp server configuration:

```go
// MCP server configurations
var mcpServers = map[string]MCPServer{
    "pm4py": {
        Command: "python3",
        Args:    []string{"-m", "pm4py_mcp.server"},
        Env:     []string{},
    },
    // ... other MCP servers ...
}
```

**Or use HTTP MCP (if pm4py-mcp supports it):**

```go
var mcpServers = map[string]MCPServer{
    "pm4py": {
        URL: "http://localhost:7015/mcp",  // If pm4py-mcp exposes HTTP
    },
}
```

### Phase 4: Update docker-compose.yml

**Remove pm4py-mcp service entirely:**

```yaml
# DELETE these lines:
# pm4py-mcp:
#   build: ../pm4py-mcp
#   ...
```

**Add pm4py-mcp as sidecar or standalone:**

```yaml
# Option A: Run in BusinessOS backend container (simplest)
# Add to backend service:
backend:
  # ... existing config ...
  volumes:
    - ../unrdf/otel/pm4py-mcp:/app/pm4py-mcp:ro
  environment:
    - PM4PY_MCP_ENABLED=true
    - PYTHONPATH=/app/pm4py-mcp:$PYTHONPATH

# Option B: Separate container
pm4py-mcp:
  build:
    context: ../unrdf/otel/pm4py-mcp
    dockerfile: Dockerfile
  container_name: businessos-pm4py-mcp
  restart: unless-stopped
  networks:
    - businessos-network
```

### Phase 5: Update Tests

**File:** `BusinessOS/desktop/backend-go/internal/handlers/bos_gateway_pm4py_test.go`

**Remove:**

- Mock HTTP server (`startPM4PyMockServer()`)
- HTTP client setup

**Add:**

- Mock MCP client
- Mock tool responses

```go
func setupPM4PyGatewayTest(t *testing.T) (*BOSGatewayHandler, *MockMCPClient) {
    mockMCP := NewMockMCPClient()

    handler := &BOSGatewayHandler{
        pool:   nil,  // Use mock DB
        logger: slog.Default(),
        stats: &GatewayStatistics{StartedAt: time.Now()},
        mcpClient: mockMCP,  // Inject mock
    }

    return handler, mockMCP
}

func TestDiscoverRealMCP_Success(t *testing.T) {
    handler, mockMCP := setupPM4PyGatewayTest(t)

    // Mock pm4py_discover response
    mockMCP.ExpectCall("pm4py_discover", map[string]interface{}{
        "event_log": json.RawMessage(`[]`),
        "variant":   "inductive_miner",
    }).Return(json.RawMessage(`{
        "model_id": "test-model",
        "algorithm": "inductive_miner",
        "process_model": {"places": 5, "transitions": 8, "arcs": 12}
    }`))

    // ... test code ...
}
```

---

## Files to Create

| File                                  | Purpose                   |
| ------------------------------------- | ------------------------- |
| `internal/mcp-tools/pm4py_tools.go`   | pm4py-mcp tool wrappers   |
| `internal/mcp-tools/mock_mcp_test.go` | Mock MCP client for tests |

---

## Files to Modify

| File                                          | Changes                                |
| --------------------------------------------- | -------------------------------------- |
| `internal/handlers/bos_gateway.go`            | Replace HTTP calls with MCP tool calls |
| `internal/handlers/bos_gateway_pm4py_test.go` | Replace mock HTTP server with mock MCP |
| `docker-compose.yml`                          | Remove pm4py-mcp service              |
| `internal/mcp-tools/client.go`                | Add pm4py-mcp server config            |

---

## Advantages

1. **No HTTP overhead** — Direct MCP protocol (stdio or HTTP)
2. **Simpler architecture** — One less network hop
3. **Type-safe** — MCP tools have defined schemas
4. **Unified integration** — Same pattern as other MCP tools

---

## Migration Steps

1. Implement `mcptools/pm4py_tools.go`
2. Update `bos_gateway.go` to use MCP tools
3. Update tests to use mock MCP client
4. Remove pm4py-mcp from docker-compose.yml
5. Verify all tests pass
6. Update documentation

---

**End of Plan**
