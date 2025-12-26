# UNRDF Production Runbook

**Version**: 1.0.0
**Last Updated**: 2025-12-25
**Purpose**: Step-by-step incident response procedures

---

## Table of Contents

1. [High Test Failure Rate](#high-test-failure-rate)
2. [OTEL Score Low](#otel-score-low)
3. [High Error Rate](#high-error-rate)
4. [High Latency](#high-latency)
5. [Slow Queries](#slow-queries)
6. [High Memory Usage](#high-memory-usage)
7. [Memory Leak](#memory-leak)
8. [High CPU Usage](#high-cpu-usage)
9. [Service Down](#service-down)
10. [Dependency Down](#dependency-down)
11. [Health Check Fail](#health-check-fail)
12. [Missing OTEL Spans](#missing-otel-spans)
13. [Low Pattern Reuse](#low-pattern-reuse)
14. [Low Static Coverage](#low-static-coverage)

---

## High Test Failure Rate

**Alert**: `HighTestFailureRate`
**Severity**: Critical
**Threshold**: Test failure rate >5% for 5 minutes

### Symptoms
- Test suite showing failures
- CI/CD pipeline failing
- OTEL validation score dropping

### Diagnosis

```bash
# 1. Run tests and capture output
timeout 5s npm test 2>&1 | tee test-output.log

# 2. Check failure rate
grep -c "FAIL" test-output.log
grep -c "PASS" test-output.log

# 3. Identify failing tests
grep "FAIL" test-output.log | head -10

# 4. Check for environmental issues
node --version
npm --version
env | grep NODE_ENV
```

### Root Cause Analysis

| Symptom | Root Cause | Evidence |
|---------|------------|----------|
| Specific test(s) failing | Code regression | Same tests pass in previous commit |
| Random failures | Flaky tests | Tests pass on retry |
| All tests failing | Environment issue | Tests pass locally |
| Timeout errors | Performance degradation | Tests took >5s |

### Fix

**1. Code Regression**
```bash
# Identify breaking commit
git log --oneline -10
git bisect start
git bisect bad HEAD
git bisect good <last-known-good-commit>

# Revert or fix
git revert <bad-commit>
# OR
# Fix code and commit
```

**2. Flaky Tests**
```bash
# Isolate flaky test
npm test -- --grep "flaky test name" --repeat 10

# Fix: Add proper cleanup, increase timeout, or remove race condition
# See: docs/testing-best-practices.md
```

**3. Environment Issue**
```bash
# Reset environment
rm -rf node_modules package-lock.json
npm install

# Verify Node version
nvm use 18.19.0

# Check for missing dependencies
npm audit
```

**4. Performance Degradation**
```bash
# Profile slow tests
npm test -- --reporter=verbose

# Fix: Optimize test setup, use test doubles, reduce test scope
```

### Prevention

- Run tests in CI before merge
- Set test timeout to 5s (Andon principle)
- Monitor test duration trends
- Maintain test quality ≥95% pass rate

---

## OTEL Score Low

**Alert**: `OTELScoreBelowThreshold`
**Severity**: Critical
**Threshold**: OTEL validation score <80/100 for 5 minutes

### Symptoms
- OTEL validation failing
- Missing spans in traces
- Incorrect span attributes

### Diagnosis

```bash
# 1. Run OTEL validation
timeout 5s node validation/run-all.mjs comprehensive 2>&1 | tee otel-output.log

# 2. Check score
grep "Score:" otel-output.log

# 3. Identify failures
grep "FAILED\|violation" otel-output.log

# 4. Check span generation
grep "Processing.*spans" otel-output.log
```

### Root Cause Analysis

| Score Range | Root Cause | Action |
|-------------|------------|--------|
| 70-79 | Missing optional attributes | Add missing attributes |
| 50-69 | Missing spans | Check instrumentation |
| 30-49 | OTEL provider not initialized | Check otel-provider.mjs |
| 0-29 | OTEL completely broken | Restart service, check logs |

### Fix

**1. Missing Attributes (70-79)**
```javascript
// Add missing span attributes
const span = tracer.startSpan('operation.name');
span.setAttributes({
  'service.name': 'unrdf',
  'operation.type': 'query',
  'input.size': inputData.length,
  'output.size': outputData.length
});
```

**2. Missing Spans (50-69)**
```javascript
// Ensure spans are created for all operations
import { trace } from '@opentelemetry/api';

async function criticalOperation() {
  const tracer = trace.getTracer('unrdf');
  const span = tracer.startSpan('critical.operation');

  try {
    const result = await doWork();
    span.setStatus({ code: 1 }); // OK
    return result;
  } catch (error) {
    span.setStatus({ code: 2, message: error.message }); // ERROR
    throw error;
  } finally {
    span.end();
  }
}
```

**3. OTEL Provider Issues (30-49)**
```bash
# Check provider initialization
grep "OTEL Provider" logs/*.log

# Restart with verbose logging
LOG_LEVEL=debug node app.mjs

# Verify OTEL collector is reachable
curl http://localhost:4318/v1/traces -I
```

**4. OTEL Completely Broken (0-29)**
```bash
# Check for OTEL errors
grep -i "otel\|telemetry" logs/*.log | grep -i "error"

# Restart service
systemctl restart unrdf

# Verify OTEL SDK version
npm list @opentelemetry/api
```

### Prevention

- Run OTEL validation in CI (must pass ≥80/100)
- Monitor OTEL score continuously
- Add OTEL validation to pre-commit hooks
- Document required span attributes

---

## High Error Rate

**Alert**: `HighErrorRate`
**Severity**: Critical
**Threshold**: Error rate >1% for 5 minutes

### Symptoms
- Errors in application logs
- Failed requests
- User reports of errors

### Diagnosis

```bash
# 1. Check error rate
curl http://localhost:3000/health/metrics | jq '.requests.errorRate'

# 2. Identify error types
grep '"level":"error"' logs/*.log | jq -r '.message' | sort | uniq -c | sort -rn

# 3. Check error trends
grep '"level":"error"' logs/*.log | jq -r '.timestamp' | head -20

# 4. Get error details
grep '"level":"error"' logs/*.log | jq '.' | head -50
```

### Root Cause Analysis

| Error Pattern | Root Cause | Evidence |
|---------------|------------|----------|
| Database errors | DB connection issue | "ECONNREFUSED", "timeout" |
| Validation errors | Invalid input | "ValidationError", "schema" |
| Timeout errors | Performance issue | "ETIMEDOUT", "exceeded" |
| Memory errors | Memory leak | "heap out of memory" |

### Fix

**1. Database Connection Issues**
```bash
# Check database connectivity
nc -zv db-host 5432

# Check connection pool
curl http://localhost:3000/health/ready | jq '.dependencies.database'

# Fix: Increase connection pool, check firewall, restart DB
```

**2. Validation Errors**
```javascript
// Add better error handling
import { z } from 'zod';

const schema = z.object({
  query: z.string().min(1).max(1000)
});

try {
  const validated = schema.parse(input);
} catch (error) {
  logger.warn('Validation failed', { input, error: error.errors });
  return { error: 'Invalid input', details: error.errors };
}
```

**3. Timeout Errors**
```javascript
// Add timeout to operations
const result = await Promise.race([
  doWork(),
  new Promise((_, reject) =>
    setTimeout(() => reject(new Error('Operation timeout')), 5000)
  )
]);
```

**4. Memory Errors**
See [Memory Leak](#memory-leak)

### Prevention

- Set error rate alert threshold to 1%
- Add input validation with Zod
- Implement circuit breakers for external services
- Monitor error trends

---

## High Latency

**Alert**: `HighP95Latency` or `HighP99Latency`
**Severity**: Warning (P95), Critical (P99)
**Threshold**: P95 >100ms, P99 >500ms for 5 minutes

### Symptoms
- Slow response times
- User complaints
- Timeout errors

### Diagnosis

```bash
# 1. Check current latency
curl http://localhost:3000/health/metrics | jq '.requests'

# 2. Identify slow endpoints
grep '"type":"performance"' logs/*.log | jq -r '"\(.operation): \(.duration)ms"' | sort -t: -k2 -rn | head -20

# 3. Check slow queries
grep '"type":"slow_query"' logs/*.log | jq '.'

# 4. Profile application
node --prof app.mjs
# Run load test
# Process profile
node --prof-process isolate-*.log > profile.txt
```

### Root Cause Analysis

| P95 Latency | P99 Latency | Root Cause |
|-------------|-------------|------------|
| <100ms | >500ms | Occasional slow query |
| >100ms | >500ms | Systematic performance issue |
| >100ms | <200ms | Consistent load, not spikes |
| >200ms | >1000ms | Critical performance issue |

### Fix

**1. Slow Database Queries**
```sql
-- Add indexes for frequent queries
CREATE INDEX idx_users_email ON users(email);

-- Analyze query plan
EXPLAIN ANALYZE SELECT * FROM users WHERE email = 'user@example.com';

-- Consider query caching
```

**2. N+1 Query Problem**
```javascript
// ❌ Bad: N+1 queries
for (const user of users) {
  const posts = await db.query('SELECT * FROM posts WHERE user_id = ?', [user.id]);
}

// ✅ Good: Single query with join
const usersWithPosts = await db.query(`
  SELECT users.*, posts.*
  FROM users
  LEFT JOIN posts ON posts.user_id = users.id
`);
```

**3. Large Data Processing**
```javascript
// ❌ Bad: Load all data into memory
const allRecords = await db.query('SELECT * FROM large_table');
return allRecords.map(transform);

// ✅ Good: Stream processing
const stream = db.stream('SELECT * FROM large_table');
for await (const record of stream) {
  yield transform(record);
}
```

**4. External API Calls**
```javascript
// Add caching
import { createCache } from './cache.mjs';
const cache = createCache({ ttl: 300 }); // 5 minutes

async function fetchData(key) {
  const cached = await cache.get(key);
  if (cached) return cached;

  const data = await externalAPI.fetch(key);
  await cache.set(key, data);
  return data;
}
```

### Prevention

- Monitor P95/P99 latency continuously
- Set slow query threshold to 100ms
- Add database query monitoring
- Regular performance testing

---

## Slow Queries

**Alert**: `SlowQueryDetected`
**Severity**: Warning
**Threshold**: >0.1 slow queries/sec for 5 minutes

### Symptoms
- Slow query warnings in logs
- Increased latency
- Database load spikes

### Diagnosis

```bash
# 1. Identify slow queries
grep '"type":"slow_query"' logs/*.log | jq -r '"\(.query): \(.duration)ms"' | sort -t: -k2 -rn

# 2. Check database slow query log
tail -f /var/log/postgresql/slow-queries.log

# 3. Analyze query patterns
grep '"type":"slow_query"' logs/*.log | jq -r '.query' | sort | uniq -c | sort -rn
```

### Fix

See [High Latency](#high-latency) - Database Queries section

### Prevention

- Add database indexes
- Use query explain plans
- Set slow query threshold to 100ms
- Monitor query performance

---

## High Memory Usage

**Alert**: `HighMemoryUsage`
**Severity**: Warning
**Threshold**: Memory usage >85% for 10 minutes

### Symptoms
- Increasing memory usage
- Slower performance
- Potential OOM errors

### Diagnosis

```bash
# 1. Check current memory
curl http://localhost:3000/health/metrics | jq '.memory'

# 2. Check memory trends
grep '"memory"' logs/*.log | jq '.memory.heapUsed' | tail -100

# 3. Capture heap snapshot
kill -USR2 $(pidof node)  # If app configured for heap dump
# OR
node --heapsnapshot-signal=SIGUSR2 app.mjs

# 4. Analyze with Chrome DevTools
# Load heap snapshot in Chrome DevTools > Memory
```

### Fix

**1. Clear Unused Objects**
```javascript
// ✅ Good: Clear references when done
let largeData = await loadData();
processData(largeData);
largeData = null; // Allow GC
```

**2. Use Streams for Large Data**
```javascript
// ❌ Bad: Load entire file
const data = await fs.readFile('large-file.json');

// ✅ Good: Stream file
const stream = fs.createReadStream('large-file.json');
for await (const chunk of stream) {
  process(chunk);
}
```

**3. Implement Memory Limits**
```javascript
// Set memory limit (1GB)
node --max-old-space-size=1024 app.mjs
```

### Prevention

- Set memory alert at 85%
- Monitor memory trends
- Use heap snapshots regularly
- Implement memory profiling in tests

---

## Memory Leak

**Alert**: `MemoryLeak`
**Severity**: Critical
**Threshold**: Memory increasing >1MB/s for 1 hour

### Symptoms
- Steadily increasing memory usage
- Eventually OOM crashes
- Slower performance over time

### Diagnosis

```bash
# 1. Confirm leak
curl http://localhost:3000/health/metrics | jq '.memory.heapUsed'
# Wait 5 minutes
curl http://localhost:3000/health/metrics | jq '.memory.heapUsed'
# Memory should be stable or decreasing after GC

# 2. Capture multiple heap snapshots
node --expose-gc app.mjs
# Take snapshot 1
# Use app for 10 minutes
# Take snapshot 2
# Compare in Chrome DevTools

# 3. Check for common leak patterns
grep -r "setInterval\|setTimeout" src/
grep -r "on(" src/ | grep -v "once("
grep -r "new " src/ | grep -v "new Date\|new Error"
```

### Common Leak Patterns

**1. Event Listeners Not Removed**
```javascript
// ❌ Bad: Listener never removed
eventEmitter.on('data', handler);

// ✅ Good: Remove when done
eventEmitter.once('data', handler);
// OR
const cleanup = () => eventEmitter.off('data', handler);
```

**2. Timers Not Cleared**
```javascript
// ❌ Bad: Timer never cleared
setInterval(() => doWork(), 1000);

// ✅ Good: Clear on shutdown
const timer = setInterval(() => doWork(), 1000);
process.on('SIGTERM', () => clearInterval(timer));
```

**3. Cached Data Growing Unbounded**
```javascript
// ❌ Bad: Cache grows forever
const cache = new Map();
cache.set(key, value);

// ✅ Good: LRU cache with size limit
import { LRUCache } from 'lru-cache';
const cache = new LRUCache({ max: 1000 });
```

### Fix

1. Identify leak source from heap snapshot comparison
2. Fix leak (see patterns above)
3. Deploy fix
4. Monitor memory usage for 24 hours
5. Verify memory stabilizes

### Prevention

- Monitor memory trends
- Regular heap snapshot analysis
- Code review for leak patterns
- Memory profiling in CI

---

## High CPU Usage

**Alert**: `HighCPUUsage`
**Severity**: Warning
**Threshold**: CPU >80% for 10 minutes

### Diagnosis

```bash
# 1. Check CPU usage
top -p $(pidof node)

# 2. Profile CPU
node --prof app.mjs
# Run load test
node --prof-process isolate-*.log > cpu-profile.txt

# 3. Check for CPU-intensive operations
grep '"type":"performance"' logs/*.log | jq -r '"\(.operation): \(.duration)ms"' | sort -t: -k2 -rn
```

### Fix

**1. Optimize Hot Paths**
```javascript
// ❌ Bad: Inefficient algorithm
function findUser(users, id) {
  return users.find(u => u.id === id); // O(n)
}

// ✅ Good: Use index
const userIndex = new Map(users.map(u => [u.id, u]));
function findUser(id) {
  return userIndex.get(id); // O(1)
}
```

**2. Use Worker Threads for CPU-Intensive Tasks**
```javascript
import { Worker } from 'worker_threads';

function heavyComputation(data) {
  return new Promise((resolve, reject) => {
    const worker = new Worker('./compute-worker.mjs', {
      workerData: data
    });
    worker.on('message', resolve);
    worker.on('error', reject);
  });
}
```

### Prevention

- Monitor CPU usage
- Profile regularly
- Use appropriate algorithms
- Consider worker threads

---

## Service Down

**Alert**: `ServiceDown`
**Severity**: Critical
**Threshold**: Service not responding for 1 minute

### Diagnosis

```bash
# 1. Check if process is running
ps aux | grep node

# 2. Check service status (if using systemd)
systemctl status unrdf

# 3. Check recent logs
tail -100 /var/log/unrdf/error.log

# 4. Check port availability
netstat -tlnp | grep 3000
```

### Fix

**1. Process Crashed**
```bash
# Check crash reason in logs
grep -i "error\|crash\|fatal" logs/*.log | tail -50

# Restart service
systemctl restart unrdf

# If recurring, check for:
# - Uncaught exceptions
# - Memory leaks (OOM)
# - Segmentation faults
```

**2. Port Already in Use**
```bash
# Find process using port
lsof -i :3000

# Kill process or change port
kill -9 <PID>
```

**3. Configuration Error**
```bash
# Validate configuration
node -c app.mjs

# Check environment variables
env | grep UNRDF_
```

### Prevention

- Use process manager (PM2, systemd)
- Implement graceful shutdown
- Add uncaught exception handler
- Monitor process health

---

## Dependency Down

**Alert**: `DependencyUnavailable`
**Severity**: Critical
**Threshold**: Dependency not connected for 5 minutes

### Diagnosis

```bash
# 1. Check dependency status
curl http://localhost:3000/health/ready | jq '.dependencies'

# 2. Test connectivity
nc -zv db-host 5432
curl -I http://external-api:8080/health

# 3. Check logs
grep -i "connection\|timeout" logs/*.log | tail -50
```

### Fix

**Database Down**
```bash
# Check database status
systemctl status postgresql

# Restart database
systemctl restart postgresql

# Verify connection
psql -h db-host -U user -d database
```

**External API Down**
```javascript
// Implement circuit breaker
import { CircuitBreaker } from 'circuit-breaker-js';

const breaker = new CircuitBreaker({
  timeout: 5000,
  errorThreshold: 50,
  volumeThreshold: 10
});

async function callExternalAPI() {
  return breaker.fire(() => fetch('http://external-api:8080'));
}
```

### Prevention

- Monitor all dependencies
- Implement circuit breakers
- Add retry logic with backoff
- Have fallback mechanisms

---

## Best Practices

1. **Always Check OTEL Score First** - If ≥80/100, system is healthy
2. **Trust Metrics Over Intuition** - Data > assumptions
3. **Fix Root Cause, Not Symptoms** - Andon principle
4. **Document Incidents** - Add to this runbook
5. **Test Alerts** - Ensure alerts fire correctly

---

## Escalation

If issue cannot be resolved:
1. Check this runbook for similar issues
2. Review monitoring dashboards
3. Analyze logs and metrics
4. Consult team lead or on-call engineer
5. Create incident report with evidence

Remember: **Evidence-based diagnosis, not guesswork.**
