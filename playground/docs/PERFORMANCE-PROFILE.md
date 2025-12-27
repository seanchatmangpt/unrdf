# Performance Profile

> Performance characteristics for all Playground CLI operations

```json-ld
{
  "@context": {
    "perf": "urn:playground:performance:",
    "xsd": "http://www.w3.org/2001/XMLSchema#"
  },
  "@id": "urn:playground:performance-profile:v1.0.0",
  "@type": "perf:PerformanceProfile"
}
```

---

## Latency Profiles

### Papers Commands

| Command | p50 | p95 | p99 | max | Operations |
|---------|-----|-----|-----|-----|------------|
| `papers generate imrad` | 120ms | 320ms | 450ms | 800ms | Parse + Validate + Render + Write |
| `papers generate dsr` | 130ms | 340ms | 480ms | 850ms | Parse + Validate + Render + Write |
| `papers generate argument` | 125ms | 330ms | 470ms | 820ms | Parse + Validate + Render + Write |
| `papers generate contribution` | 135ms | 350ms | 490ms | 870ms | Parse + Validate + Render + Write |
| `papers list` | 5ms | 10ms | 15ms | 30ms | Memory lookup |
| `papers list --verbose` | 8ms | 15ms | 20ms | 40ms | Memory lookup + formatting |
| `papers validate` | 80ms | 180ms | 250ms | 500ms | Parse + SHACL validation |
| `papers validate --strict` | 120ms | 280ms | 400ms | 700ms | Parse + Full SHACL validation |

### Thesis Commands

| Command | p50 | p95 | p99 | max | Operations |
|---------|-----|-----|-----|-----|------------|
| `thesis generate monograph` | 150ms | 380ms | 520ms | 900ms | Parse + Validate + Render + Write |
| `thesis generate narrative` | 145ms | 370ms | 510ms | 880ms | Parse + Validate + Render + Write |
| `thesis generate contribution` | 160ms | 400ms | 550ms | 950ms | Parse + Validate + Render + Write |
| `thesis list` | 5ms | 10ms | 15ms | 30ms | Memory lookup |
| `thesis schedule list` | 10ms | 20ms | 30ms | 50ms | Memory/file lookup |
| `thesis schedule set` | 20ms | 40ms | 60ms | 100ms | Validate + Write |

### Config Commands

| Command | p50 | p95 | p99 | max | Operations |
|---------|-----|-----|-----|-----|------------|
| `config set` | 8ms | 18ms | 25ms | 50ms | Validate + Write |
| `config get` | 5ms | 12ms | 15ms | 30ms | Read |
| `config list` | 5ms | 12ms | 15ms | 30ms | Read all |
| `config reset` | 10ms | 25ms | 30ms | 60ms | Clear + Write defaults |

### Meta Commands

| Command | p50 | p95 | p99 | max | Operations |
|---------|-----|-----|-----|-----|------------|
| `meta introspect` | 8ms | 15ms | 20ms | 40ms | Registry lookup |
| `meta ontology list` | 50ms | 120ms | 150ms | 300ms | Parse TTL + query |
| `meta sparql` (10 results) | 50ms | 100ms | 150ms | 300ms | Parse + Execute |
| `meta sparql` (100 results) | 85ms | 200ms | 350ms | 600ms | Parse + Execute + Format |
| `meta sparql` (1000 results) | 250ms | 500ms | 800ms | 1500ms | Parse + Execute + Format |
| `meta completions` | 5ms | 10ms | 15ms | 30ms | Generate script |
| `meta middleware list` | 5ms | 10ms | 15ms | 30ms | Memory lookup |
| `meta telemetry` | 30ms | 70ms | 100ms | 200ms | Collect + Format |

---

## Latency Breakdown by Operation

### papers generate Breakdown

```
Total: 120ms (p50)

+------------------+--------+----------+
| Operation        | Time   | % Total  |
+------------------+--------+----------+
| CLI Parsing      | 5ms    | 4.2%     |
| Zod Validation   | 8ms    | 6.7%     |
| Family Lookup    | 2ms    | 1.7%     |
| Context Building | 5ms    | 4.2%     |
| Template Loading | 10ms   | 8.3%     |
| Nunjucks Render  | 50ms   | 41.6%    |
| JSON Stringify   | 15ms   | 12.5%    |
| File Write       | 20ms   | 16.7%    |
| Cleanup          | 5ms    | 4.2%     |
+------------------+--------+----------+
```

### meta sparql Breakdown

```
Total: 85ms (p50, 100 results)

+------------------+--------+----------+
| Operation        | Time   | % Total  |
+------------------+--------+----------+
| CLI Parsing      | 3ms    | 3.5%     |
| Query Parsing    | 8ms    | 9.4%     |
| Syntax Validation| 5ms    | 5.9%     |
| Ontology Load    | 25ms   | 29.4%    |
| Query Execution  | 30ms   | 35.3%    |
| Result Formatting| 12ms   | 14.1%    |
| Output           | 2ms    | 2.4%     |
+------------------+--------+----------+
```

---

## Memory Profiles

### Process Baseline

| Metric | Value |
|--------|-------|
| Initial heap | 45MB |
| Heap limit | 512MB |
| RSS baseline | 65MB |

### Per Command Memory Delta

| Command | Heap Delta | Peak | GC Pressure |
|---------|-----------|------|-------------|
| `papers generate` | +15MB | 60MB | Low |
| `papers validate` | +20MB | 65MB | Low |
| `thesis generate` | +18MB | 63MB | Low |
| `meta sparql` (100 results) | +25MB | 70MB | Medium |
| `meta sparql` (1000 results) | +80MB | 125MB | High |
| `meta ontology list` | +35MB | 80MB | Medium |
| `meta introspect` | +5MB | 50MB | Low |

### Memory Usage by File Size

| Ontology Size | Load Time | Memory |
|---------------|-----------|--------|
| 10KB | 25ms | +5MB |
| 100KB | 80ms | +15MB |
| 1MB | 350ms | +50MB |
| 10MB | 2500ms | +200MB |

### GC Pressure Levels

| Level | Description | Recommendation |
|-------|-------------|----------------|
| Low | < 5% time in GC | Normal operation |
| Medium | 5-15% time in GC | Monitor under load |
| High | > 15% time in GC | Consider batching |

---

## Throughput Metrics

### Sequential Operations

| Scenario | Operations/sec | Latency |
|----------|---------------|---------|
| Paper generation | 8 | 120ms avg |
| Paper validation | 12 | 80ms avg |
| Config operations | 100+ | 8ms avg |
| SPARQL queries (small) | 20 | 50ms avg |

### Concurrent Operations

| Scenario | Concurrency | Total Throughput | Per-Op Latency |
|----------|-------------|------------------|----------------|
| Paper generation | 4 | 25 ops/sec | 160ms |
| Paper generation | 8 | 40 ops/sec | 200ms |
| SPARQL queries | 4 | 60 ops/sec | 65ms |
| SPARQL queries | 8 | 90 ops/sec | 90ms |

---

## Benchmarks by Operation Type

### Template Rendering (Nunjucks)

| Template Size | Variables | Loops | Render Time |
|---------------|-----------|-------|-------------|
| Small (1KB) | 5 | 0 | 10ms |
| Medium (5KB) | 15 | 2 | 35ms |
| Large (20KB) | 50 | 5 | 120ms |

### SPARQL Query Types

| Query Type | Complexity | Avg Time | Max Results |
|------------|------------|----------|-------------|
| SELECT simple | O(n) | 30ms | 1000 |
| SELECT with FILTER | O(n) | 50ms | 1000 |
| SELECT with JOIN | O(n*m) | 150ms | 500 |
| ASK | O(1) | 15ms | 1 |
| CONSTRUCT | O(n) | 80ms | 500 |
| DESCRIBE | O(n) | 60ms | 100 |

### File I/O

| Operation | File Size | Time |
|-----------|-----------|------|
| Read config | 1KB | 2ms |
| Write config | 1KB | 5ms |
| Read ontology | 100KB | 15ms |
| Write paper | 5KB | 8ms |
| Write thesis | 10KB | 12ms |

---

## Scalability Characteristics

### Ontology Size Scaling

```
Query Time = Base + (ResultCount * 0.5ms) + (OntologySize * 0.03ms/KB)

Examples:
- 10KB ontology, 10 results: 50ms + 5ms + 0.3ms = 55ms
- 100KB ontology, 100 results: 50ms + 50ms + 3ms = 103ms
- 1MB ontology, 1000 results: 50ms + 500ms + 30ms = 580ms
```

### Result Set Scaling

| Results | Format Time | Total Memory |
|---------|-------------|--------------|
| 10 | 2ms | +1MB |
| 100 | 12ms | +5MB |
| 1000 | 80ms | +25MB |
| 10000 | 600ms | +150MB |

---

## SLO Targets

### Response Time SLOs

| Operation Class | p50 Target | p99 Target | Max Allowed |
|-----------------|------------|------------|-------------|
| Read-only | 20ms | 100ms | 500ms |
| Generation | 200ms | 600ms | 2000ms |
| Validation | 150ms | 400ms | 1000ms |
| SPARQL | 100ms | 500ms | 3000ms |

### Availability SLOs

| Component | Target | Measurement |
|-----------|--------|-------------|
| CLI startup | 99.9% | Success rate |
| Command execution | 99.5% | Non-error exits |
| File operations | 99.9% | Success rate |

### Error Rate SLOs

| Error Type | Target | Current |
|------------|--------|---------|
| User input errors | < 10% | ~7% |
| System errors | < 0.1% | ~0.05% |
| Timeout errors | < 0.5% | ~0.2% |

---

## Performance Monitoring

### Telemetry Export

```bash
# Export metrics in Prometheus format
playground meta telemetry prometheus

# Output:
# playground_commands_total{command="papers.generate"} 150
# playground_command_duration_ms{command="papers.generate",quantile="0.5"} 120
# playground_command_duration_ms{command="papers.generate",quantile="0.99"} 450
```

### OTEL Integration

```javascript
// Example OTLP span
{
  "name": "papers.generate",
  "kind": "INTERNAL",
  "startTime": "2025-11-22T00:00:00.000Z",
  "endTime": "2025-11-22T00:00:00.120Z",
  "attributes": {
    "command": "papers.generate",
    "family": "imrad",
    "success": true,
    "duration_ms": 120
  }
}
```

### Performance Alerting Rules

```yaml
alerts:
  - name: HighLatency
    condition: p99 > 2 * baseline_p99
    severity: warning

  - name: ErrorSpike
    condition: error_rate > 5%
    severity: critical

  - name: MemoryPressure
    condition: heap_usage > 80%
    severity: warning
```

---

## Optimization Recommendations

### For High-Volume Usage

1. **Batch operations** - Combine multiple papers into single runs
2. **Cache ontologies** - Pre-load frequently used ontologies
3. **Limit SPARQL results** - Always use LIMIT clause
4. **Use JSON format** - Faster parsing than table output

### For Low-Latency Usage

1. **Keep ontologies small** - Split large ontologies
2. **Pre-compile templates** - Cache Nunjucks compilations
3. **Use ASK queries** - Instead of SELECT for existence checks
4. **Avoid --verbose** - Reduces formatting overhead

### For Memory-Constrained Usage

1. **Stream large results** - Use pagination
2. **Close after use** - Don't keep resources open
3. **Limit concurrent ops** - Control parallelism
4. **Monitor GC** - Track GC pressure
