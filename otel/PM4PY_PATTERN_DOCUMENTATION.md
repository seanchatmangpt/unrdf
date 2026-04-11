# pm4py-rust Deprecation Pattern Documentation

**Date:** 2026-04-11
**Purpose:** Document the Python-based pm4py interaction pattern before deprecating pm4py-rust

---

## Executive Summary

The pm4py integration lives entirely in Python at `~/chatmangpt/unrdf/otel/`. **pm4py-rust is NOT used** in this integration. The pattern is:

1. **Tempo** stores OTel traces (HTTP API at `/api/search`, `/api/traces/{trace_id}`)
2. **Loki** stores logs (HTTP API at `/loki/api/v1/query_range`)
3. **pm4py-analyze.py** fetches from both, converts to event log, runs process mining
4. **pm4py-mcp/server.py** exposes 27 MCP tools wrapping the analysis functions
5. **CI gate** checks conformance thresholds (fitness, precision)

---

## Core Components

### 1. pm4py-analyze.py (~2,700 lines)

**Location:** `/Users/sac/chatmangpt/unrdf/otel/pm4py-analyze.py`

**Purpose:** Main analysis engine that fetches OTel data and runs pm4py algorithms

**Key Functions:**

| Function                                       | Purpose                                                   |
| ---------------------------------------------- | --------------------------------------------------------- |
| `fetch_traces_from_tempo(hours, limit)`        | Fetch traces from Tempo HTTP API                          |
| `fetch_logs_from_loki(hours, limit)`           | Fetch logs from Loki HTTP API                             |
| `discover_process_model(df)`                   | Inductive Miner → Petri net (places, transitions, arcs)   |
| `conformance_check(df, net, im, fm)`           | Token-based replay → fitness, precision                   |
| `alignment_conformance_check(df, net, im, fm)` | Optimal alignments → deviations                           |
| `bottleneck_analysis(df)`                      | Activities with highest mean/median durations             |
| `variant_analysis(df)`                         | Most common execution patterns (trace variants)           |
| `predictive_monitoring(df)`                    | Prefix-based fitness, case duration prediction            |
| `object_centric_mining(df)`                    | OCEL format, object-centric Petri net                     |
| `anomaly_detection(df)`                        | Isolation Forest + TPOT2 AutoML                           |
| `temporal_profile_conformance(df)`             | Expected time between activities, SLA violations          |
| `rework_detection(df)`                         | Activities that repeat within traces                      |
| `batch_detection(df)`                          | Batch processing patterns                                 |
| `social_network_analysis(df)`                  | Handover-of-work, working-together networks               |
| `decision_mining(df, net, im, fm)`             | Data-driven routing decisions at gateways                 |
| `performance_spectrum_analysis(df)`            | Per-activity latency distributions                        |
| `declare_discovery(df)`                        | Declarative process rules (precedence, response, absence) |
| `declare_conformance(df)`                      | Declare constraint checking                               |
| `process_cube_analysis(df)`                    | Multi-dimensional OLAP aggregation                        |
| `process_simulation(df)`                       | Play out Petri net → synthetic traces                     |
| `streaming_analysis(df)`                       | Streaming DFG via frequency counting                      |
| `event_data_quality(df)`                       | Completeness, validity, timeliness, uniqueness            |
| `act_remediation(df)`                          | Declare conformance + actionable suggestions              |
| `goal_oriented_analysis(df)`                   | Planned vs executed, most common variant as plan          |
| `process_drift_detection(df, window_hours)`    | Bose algorithm + variant comparison                       |
| `stochastic_process_mining(df)`                | Probabilistic process model                               |
| `hierarchical_process_mining(df)`              | Multi-level process discovery                             |
| `counterfactual_analysis(df)`                  | What-if scenario analysis                                 |
| `normative_process_mining(df)`                 | Normative model vs actual behavior                        |
| `multi_agent_orchestration(df)`                | Agent coordination patterns                               |
| `self_reflective_analysis(df)`                 | Process self-assessment                                   |
| `normative_process_mining(df)`                 | Normative model discovery                                 |
| `hierarchical_process_mining(df)`              | Hierarchical decomposition                                |
| `counterfactual_analysis(df)`                  | Counterfactual reasoning                                  |
| `normative_process_mining(df)`                 | Normative patterns                                        |
| `multi_agent_orchestration(df)`                | Multi-agent workflows                                     |

**Event Log Schema:**

```python
{
    "case:concept:name": "trace_id_hex",      # Trace ID groups events into cases
    "concept:name": "span_name",               # Activity name (operation)
    "time:timestamp": "2026-04-11T12:00:00Z",  # ISO 8601 timestamp
    "org:resource": "service_name",            # Resource (service performing activity)
    "service.name": "service_name",            # OTel service name attribute
    "span.id": "span_id_hex",                  # Span ID
    # ... additional span attributes as columns
}
```

---

### 2. pm4py-mcp/server.py (~730 lines)

**Location:** `/Users/sac/chatmangpt/unrdf/otel/pm4py-mcp/pm4py_mcp/server.py`

**Purpose:** MCP server exposing 27 tools for process mining analysis

**Pattern:** Uses `importlib` to load `pm4py-analyze.py` (hyphen filename workaround)

```python
_OTEL_DIR = os.path.dirname(os.path.dirname(os.path.dirname(os.path.abspath(__file__))))
_PM4PY_ANALYZE_PATH = os.path.join(_OTEL_DIR, "pm4py-analyze.py")

_spec = importlib.util.spec_from_file_location("pm4py_analyze", _PM4PY_ANALYZE_PATH)
_pm4py_analyze = importlib.util.module_from_spec(_spec)
_spec.loader.exec_module(_pm4py_analyze)

# Bind functions into scope
fetch_traces_from_tempo = _pm4py_analyze.fetch_traces_from_tempo
# ... etc for all 27 functions
```

**MCP Tools (27 total):**

| Tool                     | Parameters                                    | Returns                              |
| ------------------------ | --------------------------------------------- | ------------------------------------ |
| `pm4py_health`           | hours, limit                                  | Trace count, log count, basic stats  |
| `pm4py_discover`         | hours, limit                                  | Petri net: places, transitions, arcs |
| `pm4py_conformance`      | hours, limit                                  | Fitness, precision metrics           |
| `pm4py_align`            | hours, limit                                  | Alignment deviations                 |
| `pm4py_bottleneck`       | hours, limit                                  | Activities with highest durations    |
| `pm4py_variant`          | hours, limit                                  | Common execution patterns            |
| `pm4py_predict`          | hours, limit                                  | Case duration predictions            |
| `pm4py_ocel`             | hours, limit                                  | Object-centric Petri net             |
| `pm4py_anomaly`          | hours, limit                                  | Anomalous traces (Isolation Forest)  |
| `pm4py_temporal`         | hours, limit                                  | SLA violations via zeta scores       |
| `pm4py_rework`           | hours, limit                                  | Repeating activities                 |
| `pm4py_batch`            | hours, limit                                  | Batch processing patterns            |
| `pm4py_sna`              | hours, limit                                  | Social network (handoff, together)   |
| `pm4py_decision`         | hours, limit                                  | Decision trees at gateways           |
| `pm4py_spectrum`         | hours, limit                                  | Per-activity latency distributions   |
| `pm4py_declare_discover` | hours, limit                                  | Declare constraints                  |
| `pm4py_declare_check`    | hours, limit                                  | Declare conformance                  |
| `pm4py_cube`             | hours, limit                                  | OLAP aggregation                     |
| `pm4py_simulate`         | hours, limit                                  | Synthetic trace generation           |
| `pm4py_stream`           | hours, limit                                  | Streaming DFG                        |
| `pm4py_quality`          | hours, limit                                  | Data quality assessment              |
| `pm4py_act`              | hours, limit                                  | Remediation suggestions              |
| `pm4py_goal`             | hours, limit                                  | Planned vs executed                  |
| `pm4py_drift`            | hours, limit, window_hours                    | Process drift detection              |
| `pm4py_full`             | hours, limit                                  | Composite: all analyses              |
| `pm4py_ci_gate`          | hours, limit, min_fitness, min_precision      | CI pass/fail                         |
| `pm4py_discover_powl`    | xes_content, variant, filtering_weight_factor | POWL model from XES                  |

---

### 3. synthetic-trace-gen.py (~150 lines)

**Location:** `/Users/sac/chatmangpt/unrdf/otel/synthetic-traces/synthetic-trace-gen.py`

**Purpose:** Generate synthetic OTLP traces for regression testing

**Operations (with latency bounds):**

```python
OPERATIONS = {
    'mcp.query':      {'mean': 50,  'std': 15,  'expected_max': 200},
    'mcp.graph_load': {'mean': 120, 'std': 40,  'expected_max': 500},
    'mcp.hooks_exec': {'mean': 80,  'std': 25,  'expected_max': 300},
    'daemon.schedule': {'mean': 30, 'std': 10,  'expected_max': 100},
    'daemon.health':   {'mean': 10, 'std': 5,   'expected_max': 50},
}
```

**Usage:**

```bash
python3 synthetic-trace-gen.py --endpoint http://localhost:4318 --interval 60
```

---

### 4. scripts/pm4py-ci-gate.py (~220 lines)

**Location:** `/Users/sac/chatmangpt/unrdf/otel/scripts/pm4py-ci-gate.py`

**Purpose:** CI gate for process mining conformance

**Environment Variables:**

- `PM4PY_MIN_FITNESS` (default: 0.8)
- `PM4PY_MIN_PRECISION` (default: 0.7)

**Exit Codes:**

- 0: PASS (fitness and precision meet thresholds)
- 1: FAIL (one or more thresholds not met)
- 2: ERROR (invalid input, missing dependency)

**Usage:**

```bash
python3 scripts/pm4py-ci-gate.py traces.json
PM4PY_MIN_FITNESS=0.9 python3 scripts/pm4py-ci-gate.py traces.json
```

---

### 5. run-process-mining.sh (~65 lines)

**Location:** `/Users/sac/chatmangpt/unrdf/otel/run-process-mining.sh`

**Purpose:** Run pm4py Jupyter notebook via papermill in Docker

**Usage:**

```bash
./run-process-mining.sh                    # Last 24h
./run-process-mining.sh --hours 6          # Last 6h
./run-process-mining.sh --hours 1 --limit 500
```

---

## Data Flow Diagram

```
┌─────────────────────────────────────────────────────────────────────────────┐
│                          OTel Stack (Docker Compose)                        │
├─────────────────────────────────────────────────────────────────────────────┤
│                                                                             │
│  ┌─────────────┐         ┌─────────────┐         ┌─────────────┐          │
│  │   Tempo     │         │    Loki     │         │  Grafana    │          │
│  │  (traces)   │         │   (logs)    │         │ (dashboards)│          │
│  │  :3200      │         │   :3100     │         │  :3000      │          │
│  └──────┬──────┘         └──────┬──────┘         └─────────────┘          │
│         │ HTTP API               │ HTTP API                                     │
│         │ /api/search            │ /loki/api/v1/query_range                   │
│         │ /api/traces/{id}       │                                              │
│         └────────────┬────────────┘                                              │
│                      │                                                          │
│                      ▼                                                          │
│  ┌──────────────────────────────────────────────────────────────────────┐    │
│  │                     pm4py-analyze.py                                 │    │
│  │  ┌─────────────────┐  ┌─────────────────┐  ┌─────────────────┐      │    │
│  │  │ fetch_traces()  │  │  fetch_logs()   │  │  otlp_to_log()  │      │    │
│  │  └────────┬────────┘  └────────┬────────┘  └────────┬────────┘      │    │
│  │           │                    │                    │                │    │
│  │           └────────────────────┴────────────────────┘                │    │
│  │                              │                                       │    │
│  │                              ▼                                       │    │
│  │                    ┌──────────────────┐                            │    │
│  │                    │  Event Log (DF)  │                            │    │
│  │                    │  case:concept... │                            │    │
│  │                    │  concept:name    │                            │    │
│  │                    │  time:timestamp  │                            │    │
│  │                    └────────┬─────────┘                            │    │
│  │                             │                                       │    │
│  │         ┌───────────────────┼───────────────────┐                  │    │
│  │         ▼                   ▼                   ▼                  │    │
│  │  ┌──────────┐        ┌──────────┐        ┌──────────┐             │    │
│  │  │ discover │        │conformance│       │bottleneck│             │    │
│  │  │   pm4py  │        │   pm4py   │        │   pm4py  │             │    │
│  │  └──────────┘        └──────────┘        └──────────┘             │    │
│  └──────────────────────────────────────────────────────────────────────┘    │
│                                  │                                             │
│                                  ▼                                             │
│  ┌──────────────────────────────────────────────────────────────────────┐    │
│  │                    pm4py-mcp/server.py (FastMCP)                     │    │
│  │                         27 tools                                     │    │
│  └──────────────────────────────────────────────────────────────────────┘    │
│                                                                             │
└─────────────────────────────────────────────────────────────────────────────┘
```

---

## Integration Points

### Tempo HTTP API

**Base URL:** `http://localhost:3200`

**Endpoints:**

- `POST /api/search` - Search traces by criteria
- `GET /api/traces/{trace_id}` - Get specific trace

**Request (search):**

```json
{
  "query": {
    "startTime": "2026-04-11T00:00:00Z",
    "endTime": "2026-04-11T23:59:59Z",
    "limit": 1000
  }
}
```

**Response:** OTLP JSON resource spans

---

### Loki HTTP API

**Base URL:** `http://localhost:3100`

**Endpoints:**

- `GET /loki/api/v1/query_range` - Query logs with time range

**Request:**

```
GET /loki/api/v1/query_range?query={service_name}.*&start=...&end=...&limit=1000
```

**Response:** Loki JSON log streams

---

### Docker Compose Services

**File:** `/Users/sac/chatmangpt/unrdf/otel/docker-compose.yml`

**Services:**

- `tempo` - OTel trace storage
- `loki` - Log aggregation
- `prometheus` - Metrics
- `grafana` - Dashboards
- `alloy` - OTel collector (alternative to otel-collector)
- `unrdf-pm4py` - Python pm4py analysis container

---

## pm4py-rust Deprecation Strategy

### Current State

- **pm4py-rust** exists at `/Users/sac/chatmangpt/pm4py-rust/` with Rust bindings
- **NOT USED** in the UNRDF otel integration
- All process mining uses direct Python pm4py via `pm4py-analyze.py`

### Deprecation Steps

1. **Verify no dependencies on pm4py-rust**

   ```bash
   cd /Users/sac/chatmangpt
   grep -r "pm4py-rust" --exclude-dir=target --exclude-dir=.git
   grep -r "pm4py" OSA/ BusinessOS/ canopy/
   ```

2. **Document migration path** (this document)

3. **Remove pm4py-rust from integration chain**
   - Remove from docker-compose.yml (if present)
   - Remove from make targets (if present)
   - Update health checks (8090 port checks)

4. **Archive pm4py-rust repository**
   - Tag final commit: `pm4py-rust-deprecated-v1.0.0`
   - Move to `attic/` or archive organization
   - Add README with pointer to Python pm4py integration

5. **Update documentation**
   - CLAUDE.md references
   - Build commands (remove pm4py-rust)
   - Port map (remove 8090)

---

## Verification Commands

After deprecation, verify the Python pm4py integration works:

```bash
# 1. Start OTEL stack
cd /Users/sac/chatmangpt/unrdf/otel
docker compose up -d tempo loki prometheus grafana

# 2. Start pm4py-mcp server
cd pm4py-mcp
python3 -m pm4py_mcp.server

# 3. Test health check
# (via MCP client or direct HTTP if exposed)

# 4. Run CI gate
python3 scripts/pm4py-ci-gate.py traces.json

# 5. Generate synthetic traces
python3 synthetic-traces/synthetic-trace-gen.py --endpoint http://localhost:4318
```

---

## Key Insights

1. **No Rust wrapper needed** - Python pm4py handles everything
2. **Tempo + Loki = Source of truth** - All event data comes from OTel APIs
3. **MCP for integration** - 27 tools exposed via FastMCP
4. **CI gate for quality** - Fitness/precision thresholds enforce conformance
5. **Synthetic traces for testing** - Regression detection via known patterns

---

## Files to Preserve After Deprecation

| File                                      | Purpose                             |
| ----------------------------------------- | ----------------------------------- |
| `pm4py-analyze.py`                        | Core analysis engine (27 functions) |
| `pm4py-mcp/pm4py_mcp/server.py`           | MCP server wrapper (27 tools)       |
| `scripts/pm4py-ci-gate.py`                | CI conformance gate                 |
| `synthetic-traces/synthetic-trace-gen.py` | Synthetic trace generator           |
| `run-process-mining.sh`                   | Jupyter notebook execution          |
| `pm4py-process-mining.ipynb`              | Analysis notebook                   |
| `docker-compose.yml`                      | OTEL stack orchestration            |

---

**End of Pattern Documentation**
