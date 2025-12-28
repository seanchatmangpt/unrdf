# @unrdf/streaming Capability Map

**Version**: 5.0.1
**Status**: Production Ready
**Runtime**: Node.js ≥18.0.0 | Browser (ES2020+)
**Last Updated**: 2025-12-28

---

## Overview

Change Feeds and Real-time Synchronization for UNRDF. Provides WebSocket-based streaming, delta sync protocol, backpressure control, and RDF stream parsing with validation.

**Key Capabilities**:
- **Change Feeds**: Real-time RDF delta notifications
- **Sync Protocol**: Bidirectional delta synchronization with checksums
- **Stream Processing**: Pipeline-based RDF stream transformations
- **Real-time Validation**: SHACL validation on streaming data
- **Backpressure Control**: Flow control for high-throughput scenarios

**Package Exports**:
```javascript
import {
  createChangeFeed,
  createSyncMessage,
  RDFStreamParser,
  PerformanceMonitor
} from '@unrdf/streaming';
```

**Dependencies**:
- Required: `@unrdf/core` (workspace), `@unrdf/hooks` (workspace), `@unrdf/oxigraph` (workspace), `ws` (^8.18.3), `zod` (^4.1.13)
- Optional: `lru-cache` (^10.0.0) for caching

**Evidence**:
- Test Coverage: Not specified
- Test Files: vitest-based test suite
- OTEL Validation: Not specified
- Example Files: Streaming benchmarks

---

## Capability Atoms

### Core Capabilities (Tier 1)

| Atom | Type | Runtime | Evidence | Compositions |
|------|------|---------|----------|--------------|
| `createChangeFeed()` | Function | Node | [src/streaming/change-feed.mjs](file:///home/user/unrdf/packages/streaming/src/streaming/change-feed.mjs) | C1 |
| `createSubscriptionManager()` | Function | Node | [src/streaming/subscription-manager.mjs](file:///home/user/unrdf/packages/streaming/src/streaming/subscription-manager.mjs) | C1 |
| `createStreamProcessor()` | Function | Node, Browser | [src/streaming/stream-processor.mjs](file:///home/user/unrdf/packages/streaming/src/streaming/stream-processor.mjs) | C2 |
| `createSyncMessage()` | Function | Node, Browser | [src/sync-protocol.mjs](file:///home/user/unrdf/packages/streaming/src/sync-protocol.mjs) | C3 |

**Verification**:
```bash
timeout 5s pnpm --filter @unrdf/streaming test
```

### Advanced Capabilities (Tier 2)

| Atom | Type | Runtime | Evidence | Compositions |
|------|------|---------|----------|--------------|
| `RealTimeValidator` | Class | Node, Browser | [src/streaming/real-time-validator.mjs](file:///home/user/unrdf/packages/streaming/src/streaming/real-time-validator.mjs) | C4 |
| `RDFStreamParser` | Class | Node, Browser | [src/rdf-stream-parser.mjs](file:///home/user/unrdf/packages/streaming/src/rdf-stream-parser.mjs) | C5 |
| `PerformanceMonitor` | Class | Node, Browser | [src/performance-monitor.mjs](file:///home/user/unrdf/packages/streaming/src/performance-monitor.mjs) | C6 |

### Experimental Capabilities (Tier 3 - Benchmarks)

| Atom | Type | Runtime | Evidence | Compositions |
|------|------|---------|----------|--------------|
| `benchmarkParsingThroughput()` | Function | Node | [src/benchmarks.mjs](file:///home/user/unrdf/packages/streaming/src/benchmarks.mjs) | C7 |
| `benchmarkChangeFeedLatency()` | Function | Node | [src/benchmarks.mjs](file:///home/user/unrdf/packages/streaming/src/benchmarks.mjs) | C7 |
| `benchmarkBackpressure()` | Function | Node | [src/benchmarks.mjs](file:///home/user/unrdf/packages/streaming/src/benchmarks.mjs) | C7 |

---

## Composition Patterns

**C1**: **Change Feed** - Subscribe → Publish deltas → Broadcast
```javascript
import { createChangeFeed, createSubscriptionManager } from '@unrdf/streaming';

const feed = createChangeFeed();
const subscriptions = createSubscriptionManager();

subscriptions.subscribe('client-1', (delta) => {
  console.log('Delta received:', delta);
});

await feed.publish({ added: [quad1, quad2], removed: [quad3] });
```

**C2**: **Stream Processing** - Create pipeline → Transform → Output
```javascript
import { createStreamProcessor } from '@unrdf/streaming';

const processor = createStreamProcessor({
  transforms: [
    (quad) => normalizeQuad(quad),
    (quad) => validateQuad(quad)
  ]
});

for await (const quad of processor.process(inputStream)) {
  // Processed quad
}
```

**C3**: **Sync Protocol** - Create sync message → Apply to store
```javascript
import { createSyncMessage, applySyncMessage } from '@unrdf/streaming';

const syncMsg = createSyncMessage({
  delta: { added: [quad1], removed: [] },
  checksum: calculateChecksum(delta),
  timestamp: Date.now()
});

await applySyncMessage(store, syncMsg);
```

**C4**: **Real-time Validation** - Validate stream → Pass/reject quads
```javascript
import { RealTimeValidator } from '@unrdf/streaming';

const validator = new RealTimeValidator({
  mode: 'strict', // or 'permissive'
  shaclShapes: [/* shapes */]
});

const valid = await validator.validateQuad(quad);
```

**C5**: **RDF Stream Parsing** - Parse streaming RDF → Quads
```javascript
import { RDFStreamParser, parseRDFStream } from '@unrdf/streaming';

const parser = new RDFStreamParser({ format: 'turtle' });
for await (const quad of parseRDFStream(readableStream, 'turtle')) {
  // Process quad
}
```

**C6**: **Performance Monitoring** - Track throughput → Detect bottlenecks
```javascript
import { PerformanceMonitor } from '@unrdf/streaming';

const monitor = new PerformanceMonitor();
monitor.start();
// ... streaming operations
const stats = monitor.getStats();
console.log(`Throughput: ${stats.quadsPerSecond}`);
```

**C7**: **Benchmarking** - Measure latency and throughput
```javascript
import { runComprehensiveBenchmarks } from '@unrdf/streaming';

const results = await runComprehensiveBenchmarks({
  quadCounts: [100, 1000, 10000],
  formats: ['turtle', 'nquads']
});
```

---

## Performance Model

**Theoretical Performance**:

Based on streaming architecture:
- Time Complexity: O(1) for subscription, O(s) for broadcast (s=subscribers)
- Space Complexity: O(b) for backpressure buffer (b=buffer size)
- Scalability: Memory-limited by replay buffer

**Empirical Benchmarks** (from performance-analysis.md):

| Operation | Dataset Size | Execution Time | Notes |
|-----------|--------------|----------------|-------|
| Publish latency | 1 delta | <10ms (p95) | WebSocket overhead |
| Subscription overhead | Per subscriber | <1ms | Minimal |
| Max subscribers | Memory-limited | 1000+ | Per node |
| Delta throughput | Per subscriber | 100-500 deltas/sec | Client-dependent |

**Performance Characteristics**:
- Hook-native change detection: O(1) event emission
- WebSocket transport: Low latency (<10ms)
- No guaranteed delivery (at-most-once semantics)
- Backpressure requires client cooperation

**Optimization Strategies**:
1. **Batching**: Batch multiple deltas into single message
2. **Compression**: Compress WebSocket frames
3. **Selective Subscriptions**: Filter deltas server-side

**Verification**:
```bash
timeout 30s pnpm --filter @unrdf/streaming run benchmark
```

---

## Runtime Compatibility Matrix

| Capability | Node.js | Browser | BEAM/WASM | Notes |
|------------|---------|---------|-----------|-------|
| Change Feed | ✅ ≥18.0 | ⚠️ Partial | ⏳ Planned | Browser = SSE client |
| Sync Protocol | ✅ ≥18.0 | ✅ ES2020+ | ✅ Supported | Universal |
| Stream Processing | ✅ ≥18.0 | ✅ ES2020+ | ✅ Supported | Universal |
| RDF Parsing | ✅ ≥18.0 | ✅ ES2020+ | ✅ Supported | Universal |
| WebSocket Server | ✅ ≥18.0 | ❌ Not supported | ❌ Not supported | Server-only |

**Legend**:
- ✅ Fully supported
- ⏳ Planned/In progress
- ❌ Not supported
- ⚠️ Partial support (see notes)

**Browser Considerations**:
- WebSocket client only (not server)
- SSE for server-to-client streaming

**Node.js Considerations**:
- ESM-only: Requires `"type": "module"`

---

## Evidence & Verification

### Source Code References

All capability atoms are traceable to source:
- Change Feed: [src/streaming/change-feed.mjs](file:///home/user/unrdf/packages/streaming/src/streaming/change-feed.mjs)
- Sync Protocol: [src/sync-protocol.mjs](file:///home/user/unrdf/packages/streaming/src/sync-protocol.mjs)
- Parsing: [src/rdf-stream-parser.mjs](file:///home/user/unrdf/packages/streaming/src/rdf-stream-parser.mjs)

### Verification Commands

**Quick Verification** (< 5 seconds):
```bash
timeout 5s pnpm --filter @unrdf/streaming test
```

---

## Cross-References

### Related Packages
- **@unrdf/hooks**: Change detection
- **@unrdf/oxigraph**: RDF storage

---

**Document Metadata**:
- **Template Version**: 1.0.0
- **Generated**: 2025-12-28
- **Maintainer**: @unrdf/core-team
- **Last Review**: 2025-12-28
- **Next Review**: 2026-03-28
