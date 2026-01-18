# Performance Tuning Guide - UNRDF v6.0.0

**Optimize UNRDF daemon operations for production performance**

## Overview

This guide provides performance optimization strategies for UNRDF v6.0.0 daemon operations, covering configuration tuning, caching, monitoring, and benchmarking.

**Target Performance**:
- API Key Authentication: <5ms
- SPARQL Query (simple): <10ms
- Receipt Generation: <1ms
- Security Validation: <1ms
- Overall Request Latency (P95): <100ms

---

## Table of Contents

1. [Node.js Configuration](#nodejs-configuration)
2. [Daemon Configuration](#daemon-configuration)
3. [Database Optimization](#database-optimization)
4. [Caching Strategies](#caching-strategies)
5. [Concurrency Tuning](#concurrency-tuning)
6. [Memory Management](#memory-management)
7. [Network Optimization](#network-optimization)
8. [Monitoring & Profiling](#monitoring--profiling)

---

## Node.js Configuration

### V8 Heap Tuning

```bash
# /etc/systemd/system/unrdf-daemon.service
[Service]
# Allocate 4GB heap (adjust based on available RAM)
Environment=NODE_OPTIONS="--max-old-space-size=4096"

# Enable optimizations
Environment=NODE_OPTIONS="--max-old-space-size=4096 --optimize-for-size"

# For high-performance scenarios
Environment=NODE_OPTIONS="--max-old-space-size=8192 --max-semi-space-size=128"
```

**Heap Size Recommendations**:
| Workload | RAM Available | Heap Size | Semi-Space |
|----------|---------------|-----------|------------|
| Light | 2GB | 1GB (1024) | 32MB |
| Medium | 4GB | 2GB (2048) | 64MB |
| Heavy | 8GB | 4GB (4096) | 128MB |
| Enterprise | 16GB+ | 8GB (8192) | 256MB |

### Event Loop Optimization

```javascript
// src/config/performance.mjs
export const performanceConfig = {
  // UV thread pool size (default: 4)
  // Increase for I/O-heavy workloads
  UV_THREADPOOL_SIZE: process.env.UV_THREADPOOL_SIZE || 8,

  // V8 flags
  v8Flags: [
    '--max-old-space-size=4096',
    '--optimize-for-size',
    '--gc-interval=100',
  ],
};

// Apply at startup
process.env.UV_THREADPOOL_SIZE = performanceConfig.UV_THREADPOOL_SIZE;
```

### Cluster Mode

```javascript
// src/cluster.mjs
import cluster from 'cluster';
import os from 'os';
import { UnrdfDaemon } from '@unrdf/daemon';

if (cluster.isPrimary) {
  const numWorkers = process.env.CLUSTER_WORKERS || os.cpus().length;

  console.log(`Primary ${process.pid} is running`);
  console.log(`Forking ${numWorkers} workers...`);

  for (let i = 0; i < numWorkers; i++) {
    cluster.fork();
  }

  cluster.on('exit', (worker, code, signal) => {
    console.log(`Worker ${worker.process.pid} died. Restarting...`);
    cluster.fork();
  });
} else {
  // Worker process
  const daemon = new UnrdfDaemon({
    daemonId: `worker-${cluster.worker.id}`,
    port: 8080 + cluster.worker.id,
    concurrency: 5,  // Lower per-worker concurrency
  });

  daemon.start();

  console.log(`Worker ${process.pid} started`);
}
```

---

## Daemon Configuration

### Optimal Concurrency Settings

```javascript
import { UnrdfDaemon } from '@unrdf/daemon';

const daemon = new UnrdfDaemon({
  daemonId: 'production-daemon',

  // Concurrency = (CPU cores * 2) - 1
  // For 4-core system: (4 * 2) - 1 = 7
  concurrency: parseInt(process.env.DAEMON_CONCURRENCY) || 7,

  // Health check frequency (balance monitoring vs overhead)
  healthCheckIntervalMs: 30000,  // 30 seconds

  // Metrics retention (balance history vs memory)
  metricsRetentionMs: 3600000,  // 1 hour

  // Global retry policy
  globalRetryPolicy: {
    maxAttempts: 3,
    backoffMs: 1000,
    backoffMultiplier: 2,
    maxBackoffMs: 10000,
    jitterFactor: 0.1,
  },
});
```

**Concurrency Tuning Guidelines**:
| CPU Cores | Recommended Concurrency | Notes |
|-----------|-------------------------|-------|
| 2 | 3-5 | Small VMs |
| 4 | 7-10 | Standard production |
| 8 | 15-20 | High-performance |
| 16+ | 30-50 | Enterprise scale |

### Operation Timeouts

```javascript
daemon.schedule({
  id: 'sparql-query',
  handler: async (context) => {
    // Set reasonable timeout
    const controller = new AbortController();
    const timeout = setTimeout(() => controller.abort(), 30000);

    try {
      const results = await executeQuery(context.data.query, {
        signal: controller.abort,
      });
      clearTimeout(timeout);
      return results;
    } catch (error) {
      clearTimeout(timeout);
      if (error.name === 'AbortError') {
        throw new Error('Query timeout exceeded');
      }
      throw error;
    }
  },
});
```

---

## Database Optimization

### Connection Pooling

```javascript
import { Pool } from 'pg';

const pool = new Pool({
  host: process.env.DB_HOST,
  database: process.env.DB_NAME,
  user: process.env.DB_USER,
  password: process.env.DB_PASSWORD,

  // Pool configuration
  max: 20,                    // Max connections
  min: 5,                     // Min connections
  idleTimeoutMillis: 30000,   // Idle timeout
  connectionTimeoutMillis: 2000,  // Connection timeout

  // Statement timeout
  statement_timeout: 30000,   // 30 seconds
});

// Ensure cleanup
process.on('SIGINT', () => pool.end());
```

**Pool Size Recommendations**:
```
Max Pool Size = ((CPU cores * 2) + effective_spindle_count)
For 4-core VM with SSD: (4 * 2) + 1 = 9 → Use 10-20
```

### Query Optimization

```sql
-- Add indexes for frequent queries
CREATE INDEX idx_triples_subject ON triples(subject);
CREATE INDEX idx_triples_predicate ON triples(predicate);
CREATE INDEX idx_triples_object ON triples(object);
CREATE INDEX idx_triples_graph ON triples(graph);

-- Composite index for common patterns
CREATE INDEX idx_triples_sp ON triples(subject, predicate);
CREATE INDEX idx_triples_po ON triples(predicate, object);

-- Analyze tables
ANALYZE triples;
```

### PostgreSQL Configuration

```conf
# postgresql.conf

# Memory
shared_buffers = 2GB                    # 25% of RAM
effective_cache_size = 6GB              # 75% of RAM
work_mem = 50MB                         # Per connection
maintenance_work_mem = 512MB

# Checkpoints
checkpoint_completion_target = 0.9
wal_buffers = 16MB
max_wal_size = 2GB
min_wal_size = 1GB

# Query Planner
random_page_cost = 1.1                  # For SSDs
effective_io_concurrency = 200          # For SSDs

# Workers
max_worker_processes = 8
max_parallel_workers_per_gather = 4
max_parallel_workers = 8
```

---

## Caching Strategies

### In-Memory Caching

```javascript
import { LRUCache } from 'lru-cache';

// Query result cache
const queryCache = new LRUCache({
  max: 500,                  // Max entries
  maxSize: 50 * 1024 * 1024, // 50MB
  sizeCalculation: (value) => JSON.stringify(value).length,
  ttl: 1000 * 60 * 5,        // 5 minutes
});

async function cachedQuery(sparqlQuery) {
  const cacheKey = hashQuery(sparqlQuery);

  // Check cache
  const cached = queryCache.get(cacheKey);
  if (cached) {
    return cached;
  }

  // Execute query
  const results = await executeQuery(sparqlQuery);

  // Cache results
  queryCache.set(cacheKey, results);

  return results;
}

function hashQuery(query) {
  return crypto.createHash('sha256').update(query).digest('hex');
}
```

### Redis Caching

```javascript
import Redis from 'ioredis';

const redis = new Redis({
  host: process.env.REDIS_HOST || 'localhost',
  port: process.env.REDIS_PORT || 6379,
  maxRetriesPerRequest: 3,
  enableReadyCheck: true,
  lazyConnect: false,
});

async function cachedQueryRedis(sparqlQuery) {
  const cacheKey = `query:${hashQuery(sparqlQuery)}`;

  // Try cache first
  const cached = await redis.get(cacheKey);
  if (cached) {
    return JSON.parse(cached);
  }

  // Execute query
  const results = await executeQuery(sparqlQuery);

  // Cache with 5-minute TTL
  await redis.setex(cacheKey, 300, JSON.stringify(results));

  return results;
}
```

### HTTP Response Caching

```javascript
import apicache from 'apicache';

const cache = apicache.middleware;

// Cache GET requests for 5 minutes
app.get('/api/daemon/health', cache('5 minutes'), (req, res) => {
  const health = daemon.getHealth();
  res.json(health);
});

// Cache query results based on query hash
app.post('/api/daemon/query', async (req, res) => {
  const cacheKey = hashQuery(req.body.query);
  const cached = await queryCache.get(cacheKey);

  if (cached) {
    res.set('X-Cache-Hit', 'true');
    return res.json(cached);
  }

  const results = await executeQuery(req.body.query);
  queryCache.set(cacheKey, results);

  res.set('X-Cache-Hit', 'false');
  res.json(results);
});
```

---

## Concurrency Tuning

### Queue Management

```javascript
import { UnrdfDaemon } from '@unrdf/daemon';

const daemon = new UnrdfDaemon({
  daemonId: 'optimized-daemon',
  concurrency: 10,

  // Queue configuration
  queueConfig: {
    maxQueueSize: 1000,         // Max pending operations
    priority: true,             // Enable priority queue
    timeout: 30000,             // Queue timeout
  },
});

// Schedule with priority
daemon.schedule({
  id: 'high-priority-task',
  priority: 10,  // Higher = more priority
  handler: async () => { /* ... */ },
});

daemon.schedule({
  id: 'low-priority-task',
  priority: 1,   // Lower priority
  handler: async () => { /* ... */ },
});
```

### Batch Processing

```javascript
// Batch multiple operations
async function batchInsertTriples(triples) {
  const batchSize = 1000;
  const batches = [];

  for (let i = 0; i < triples.length; i += batchSize) {
    batches.push(triples.slice(i, i + batchSize));
  }

  // Process batches concurrently (up to concurrency limit)
  const results = await Promise.all(
    batches.map(batch => insertBatch(batch))
  );

  return results.flat();
}

async function insertBatch(batch) {
  // Single database transaction for batch
  return await pool.query(
    'INSERT INTO triples (subject, predicate, object) VALUES ' +
    batch.map((_, i) => `($${i*3+1}, $${i*3+2}, $${i*3+3})`).join(','),
    batch.flatMap(t => [t.subject, t.predicate, t.object])
  );
}
```

---

## Memory Management

### Garbage Collection Tuning

```javascript
// Expose GC (for monitoring only)
if (global.gc) {
  setInterval(() => {
    const before = process.memoryUsage();
    global.gc();
    const after = process.memoryUsage();

    console.log('GC freed:', {
      heapUsed: (before.heapUsed - after.heapUsed) / 1024 / 1024,
      external: (before.external - after.external) / 1024 / 1024,
    });
  }, 60000); // Every minute
}
```

### Memory Leak Detection

```javascript
import memwatch from '@airbnb/node-memwatch';

memwatch.on('leak', (info) => {
  console.error('Memory leak detected:', info);
  // Alert monitoring system
});

memwatch.on('stats', (stats) => {
  console.log('GC Stats:', {
    used_heap_size: stats.current_base / 1024 / 1024,
    growth: stats.estimated_base / 1024 / 1024,
  });
});
```

### Stream Processing

```javascript
// Use streams for large datasets
import { pipeline } from 'stream/promises';
import { Transform } from 'stream';

async function processLargeRdfFile(filepath) {
  const parseStream = new Transform({
    transform(chunk, encoding, callback) {
      const triples = parseRdfChunk(chunk);
      callback(null, triples);
    }
  });

  const insertStream = new Transform({
    transform(triples, encoding, callback) {
      insertBatch(triples).then(() => callback());
    }
  });

  await pipeline(
    fs.createReadStream(filepath),
    parseStream,
    insertStream
  );
}
```

---

## Network Optimization

### HTTP/2 Support

```javascript
import http2 from 'http2';
import fs from 'fs';

const server = http2.createSecureServer({
  key: fs.readFileSync('/path/to/key.pem'),
  cert: fs.readFileSync('/path/to/cert.pem'),
});

server.on('stream', (stream, headers) => {
  // HTTP/2 multiplexing
  stream.respond({
    'content-type': 'application/json',
    ':status': 200
  });

  stream.end(JSON.stringify({ status: 'ok' }));
});

server.listen(8443);
```

### Connection Keepalive

```javascript
import http from 'http';

const server = http.createServer(app);

server.keepAliveTimeout = 65000;  // 65 seconds
server.headersTimeout = 66000;    // 66 seconds

server.listen(8080);
```

### Compression

```javascript
import compression from 'compression';

app.use(compression({
  level: 6,              // Compression level (0-9)
  threshold: 1024,       // Min size to compress (bytes)
  filter: (req, res) => {
    // Don't compress if client doesn't support
    if (req.headers['x-no-compression']) {
      return false;
    }
    return compression.filter(req, res);
  }
}));
```

---

## Monitoring & Profiling

### Performance Metrics

```javascript
import prometheus from 'prom-client';

// Create histogram for operation latency
const operationLatency = new prometheus.Histogram({
  name: 'unrdf_operation_duration_seconds',
  help: 'Operation execution time',
  labelNames: ['operation', 'status'],
  buckets: [0.001, 0.005, 0.01, 0.05, 0.1, 0.5, 1, 5],
});

// Track operation performance
async function trackedOperation(operationId, handler) {
  const start = Date.now();

  try {
    const result = await handler();
    const duration = (Date.now() - start) / 1000;

    operationLatency.observe({
      operation: operationId,
      status: 'success'
    }, duration);

    return result;
  } catch (error) {
    const duration = (Date.now() - start) / 1000;

    operationLatency.observe({
      operation: operationId,
      status: 'failure'
    }, duration);

    throw error;
  }
}
```

### CPU Profiling

```javascript
import v8Profiler from 'v8-profiler-next';

// Start profiling
const profiler = v8Profiler.startProfiling('CPU Profile', true);

// Run operations...
await daemon.execute('my-operation');

// Stop profiling and save
const profile = profiler.stopProfiling('CPU Profile');

profile.export((error, result) => {
  fs.writeFileSync('/tmp/profile.cpuprofile', result);
  profile.delete();

  console.log('Profile saved to /tmp/profile.cpuprofile');
  console.log('Analyze with: chrome://inspect');
});
```

### Memory Profiling

```javascript
import heapdump from 'heapdump';

// Capture heap snapshot
heapdump.writeSnapshot('/tmp/' + Date.now() + '.heapsnapshot', (err, filename) => {
  console.log('Heap snapshot written to', filename);
  console.log('Analyze with Chrome DevTools');
});

// Or programmatically on high memory usage
setInterval(() => {
  const { heapUsed } = process.memoryUsage();
  const heapUsedMB = heapUsed / 1024 / 1024;

  if (heapUsedMB > 1000) {  // > 1GB
    console.warn('High memory usage:', heapUsedMB, 'MB');
    heapdump.writeSnapshot('/tmp/high-memory-' + Date.now() + '.heapsnapshot');
  }
}, 60000);
```

### Load Testing

```javascript
// Using autocannon
import autocannon from 'autocannon';

const result = await autocannon({
  url: 'http://localhost:8080/api/daemon/health',
  connections: 100,        // Concurrent connections
  duration: 30,            // 30 seconds
  pipelining: 10,          // Requests per connection
  headers: {
    'x-api-key': process.env.API_KEY,
  },
});

console.log('Load Test Results:');
console.log('Requests/sec:', result.requests.average);
console.log('Latency (P95):', result.latency.p95, 'ms');
console.log('Errors:', result.errors);
```

---

## Benchmarking

### Authentication Performance

```bash
# Benchmark authentication
node benchmarks/auth-benchmark.mjs

# Expected results:
# Authentication: <5ms (P95)
# BLAKE3 hashing: <1ms (P95)
# Verification: <2ms (P95)
```

### Query Performance

```bash
# Benchmark SPARQL queries
node benchmarks/query-benchmark.mjs

# Expected results:
# Simple SELECT: <10ms (P95)
# Complex JOIN: <50ms (P95)
# Full-text search: <100ms (P95)
```

### Overall System

```bash
# Run comprehensive benchmark suite
pnpm run benchmark

# Expected metrics:
# - Throughput: >1000 ops/sec
# - P95 Latency: <100ms
# - P99 Latency: <500ms
# - Error Rate: <0.1%
```

---

## Performance Checklist

### ✅ Before Production

- [ ] Node.js heap size optimized for RAM
- [ ] Daemon concurrency tuned for CPU cores
- [ ] Database connection pool configured
- [ ] Caching strategy implemented
- [ ] Query indexes created
- [ ] Load testing completed
- [ ] Memory leak detection enabled
- [ ] Monitoring dashboards configured

### ✅ In Production

- [ ] Monitor P95/P99 latency
- [ ] Track error rates
- [ ] Review GC patterns
- [ ] Analyze slow queries
- [ ] Optimize hot paths
- [ ] Regular performance reviews

---

## Troubleshooting

### High Latency

1. **Check database**:
   ```sql
   SELECT * FROM pg_stat_activity WHERE state = 'active';
   SELECT * FROM pg_stat_statements ORDER BY total_time DESC LIMIT 10;
   ```

2. **Profile CPU**:
   ```bash
   node --prof src/index.mjs
   node --prof-process isolate-*.log > profile.txt
   ```

3. **Check event loop lag**:
   ```javascript
   const lag = require('event-loop-lag');
   const lagMonitor = lag(1000);  // Check every second

   setInterval(() => {
     console.log('Event loop lag:', lagMonitor(), 'ms');
   }, 5000);
   ```

### Memory Issues

1. **Check heap usage**:
   ```javascript
   const { heapUsed, heapTotal } = process.memoryUsage();
   console.log(`Heap: ${heapUsed / heapTotal * 100}% used`);
   ```

2. **Find memory leaks**:
   ```bash
   node --inspect src/index.mjs
   # Connect Chrome DevTools
   # Take heap snapshots
   # Compare snapshots to find leaks
   ```

---

## Resources

- [Production Deployment](PRODUCTION_DEPLOYMENT.md)
- [Security Configuration](SECURITY_CONFIGURATION.md)
- [Migration Guide](../MIGRATING_TO_V6.md)
- [Benchmarks](../../packages/daemon/benchmarks/)

**Performance Status**: Optimized ✅
**Last Updated**: 2026-01-11
