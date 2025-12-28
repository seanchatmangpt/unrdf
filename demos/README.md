# WASM Integration Demos

This directory contains runnable demonstrations of WASM integration in UNRDF v6.

## Demos

### 1. Oxigraph WASM Roundtrip

**File**: `wasm-roundtrip-oxigraph.mjs`

**Demonstrates**:
- JavaScript → WASM → JavaScript data flow
- RDF triple storage and query performance
- WASM instantiation overhead

**Run**:
```bash
node demos/wasm-roundtrip-oxigraph.mjs
```

**Expected Output**:
- WASM instantiation: ~10-20ms
- Add operation: <0.1ms
- Query operation: <0.1ms
- Total: <20ms (within 10ms SLA after init)

---

### 2. BEAM Pattern Matching

**File**: `wasm-roundtrip-beam.mjs`

**Demonstrates**:
- Erlang-style pattern matching on RDF triples
- Performance comparison with SPARQL
- Batch processing throughput

**Run**:
```bash
node demos/wasm-roundtrip-beam.mjs
```

**Expected Output**:
- Single match: <0.01ms
- Batch 1000: <10ms
- Throughput: >100,000 matches/sec

---

## Additional Demos

For more advanced demos, see:
- `/packages/atomvm/experiments/wasm-integration/` - 3 production demos
  - `demo-1-wasm-actor.mjs` - WASM actor with mailbox
  - `demo-2-supervision.mjs` - Fault-isolated WASM workers
  - `demo-3-roundtrip.mjs` - Full SLA tracking

---

## Requirements

- Node.js 18+
- No external dependencies (uses @unrdf packages)

---

## Related Documentation

- `/home/user/unrdf/WASM-INTEGRATION-ANALYSIS.md` - Full analysis
- `/packages/atomvm/docs/wasm-integration.md` - WASM integration guide
- `/packages/atomvm/docs/ADR/001-beam-rdf-integration.md` - Architecture decision
