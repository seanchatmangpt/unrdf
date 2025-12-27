# KGC Probe Report

**Run ID**: run_2025-12-27T08-36-16-988  
**Observations**: 8  
**Hash**: sha256:1f17b4fc5a430701

## Executive Summary

- **Platform**: linux x64
- **Runtime**: Node.js v22.21.1
- **WASM**: Available
- **Workers**: Available
- **Capabilities**: 4 discovered
- **Constraints**: 6 detected

## Capabilities

### Concurrency

- ✅ **concurrency.concurrency.shared_array_buffer**
  ```json
  {
    "available": true,
    "testSize": 8,
    "functional": true
  }
  ```
- ✅ **concurrency.concurrency.worker_threads_available**
  ```json
  {
    "available": true,
    "module": "worker_threads",
    "nodeVersion": "v22.21.1"
  }
  ```

### Runtime

- ✅ **runtime.wasm**
  ```json
  {
    "support": "available"
  }
  ```

### Wasm

- ✅ **wasm.wasm.compilation_available**
  ```json
  {
    "available": true,
    "wasm": true,
    "maxMemory": 4294967296
  }
  ```

## Constraints

### Error Boundary

- ⚠️  Network request blocked by guard
- ⚠️  Filesystem access outside allowed roots

### Guard Denial

- ⚠️  Operation denied by guard
- ⚠️  Operation denied by guard

### Memory Limit

- ⚠️  Maximum memory: 4294967296

### Stack Depth Limit

- ⚠️  Maximum stack depth: 8946

## Performance

| Method | Mean | Median | P95 | Min | Max | Unit | Samples |
|--------|------|--------|-----|-----|-----|------|--------|
| concurrency.event_loop_latency | 2.64 | 0.08 | 22.71 | 0.02 | 22.71 | ms | 10 |

## Guard Denials

2 operation(s) were denied by security guards:

- **network.fetch_blocked**
  - Guard: `unknown`
  - Reason: Access denied
  - Hash: `578e281b0cfca2ce`

- **filesystem.access_denied**
  - Guard: `unknown`
  - Reason: Access denied
  - Hash: `f66cd54a4793d7a5`

## Provenance

- **Observation count**: 8
- **Hash chain**: `sha256:1f17b4fc5a430701`
- **Domains probed**: 5

**Observations by domain**:
  - runtime: 1
  - network: 1
  - concurrency: 4
  - wasm: 1
  - filesystem: 1

---

*Report generated at 2025-12-27T09:16:35.003Z*
