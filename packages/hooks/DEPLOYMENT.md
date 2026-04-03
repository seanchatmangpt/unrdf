# @unrdf/hooks Deployment Guide

Production deployment, monitoring, and operational guidelines.

## Table of Contents

1. [Pre-Deployment Checklist](#pre-deployment-checklist)
2. [SHACL Enforcement Mode Selection](#shacl-enforcement-mode-selection)
3. [Performance Tuning](#performance-tuning)
4. [Monitoring & Observability](#monitoring--observability)
5. [Receipt Chain Validation](#receipt-chain-validation)
6. [Troubleshooting](#troubleshooting)
7. [Capacity Planning](#capacity-planning)

---

## Pre-Deployment Checklist

### Code Quality

- [ ] All hooks pass Zod validation (`validateKnowledgeHook()`)
- [ ] SPARQL CONSTRUCT queries validated against store schema
- [ ] SHACL shapes validated with test data
- [ ] N3 rules tested in isolation with EYE
- [ ] Datalog rules verified with test facts
- [ ] No circular dependencies in rule chains
- [ ] JSDoc comments complete for all custom hooks

### Testing

- [ ] Unit tests pass: `pnpm test:hooks`
- [ ] Integration tests pass: `pnpm test:integration`
- [ ] End-to-end test with production-like data
- [ ] Condition evaluation latency acceptable (<100ms P95)
- [ ] Effect execution latency acceptable (<100ms P95)
- [ ] Receipt chain verification passes
- [ ] Error handling tested with malformed inputs
- [ ] Timeout handling tested (default 30s)

### Configuration

- [ ] All external references (shapes, rules) are content-addressed
- [ ] File hashes (SHA-256) verified for integrity
- [ ] Cache TTL configured appropriately (or disabled)
- [ ] Condition result cache memory bounded
- [ ] Retry logic configured per effect (default: 1 retry)
- [ ] Timeout values reasonable per condition kind

### Documentation

- [ ] Hook definitions documented in README
- [ ] SHACL shapes have descriptive labels and comments
- [ ] N3 rules have inline documentation
- [ ] Datalog rules explained in separate doc
- [ ] Runbook created for operational issues
- [ ] On-call team trained on hooks system

### Security

- [ ] No hardcoded credentials in hooks
- [ ] SHACL shapes validate security constraints
- [ ] Function effects (if used) sandboxed
- [ ] Input validation tested against injection attempts
- [ ] Error messages don't leak sensitive data
- [ ] Receipt chain stored with access control

### Performance Baseline

- [ ] Measure P50/P95/P99 latencies
- [ ] Measure memory consumption
- [ ] Establish performance baseline
- [ ] Load test with expected QPS
- [ ] Identify bottlenecks with profiling

### Deployment Plan

- [ ] Canary deployment to 5% of traffic
- [ ] Monitor error rates during canary
- [ ] Gradual rollout plan (5% → 25% → 50% → 100%)
- [ ] Rollback procedure documented
- [ ] Incident response plan prepared

---

## SHACL Enforcement Mode Selection

### Selection Matrix

| Scenario | Mode | Reason | Trade-off |
|---|---|---|---|
| Regulatory compliance (immutable) | block | Strict governance required | Rejections may interrupt flow |
| Risk management (warnings) | annotate | Soft-fail + audit trail | May allow invalid data through |
| Data quality (auto-fix) | repair | Self-healing ideal | Repairs may introduce errors |

### Decision Flow

```
Is this constraint regulatory (e.g., KYC, AML)?
├─ YES → Use block mode
│         - Hard fail on violations
│         - Immutable audit trail
│         - Operationally: escalate to compliance team
│
└─ NO → Is auto-repair possible/safe?
        ├─ YES → Use repair mode
        │        - Auto-fix known patterns
        │        - Re-validate after repair
        │        - Operationally: monitor repair rate
        │
        └─ NO → Use annotate mode
                - Log violations as RDF
                - Execute hooks anyway (soft-fail)
                - Operationally: alert on high violation rate
```

---

### Block Mode (Strict)

**Configuration**:

```javascript
const hook = createKnowledgeHook({
  name: 'compliance-gate',
  condition: {
    kind: 'shacl',
    ref: { uri: 'file:///shapes/regulatory.ttl' },
    enforcementMode: 'block'  // FAIL if violations
  },
  effects: [{ /* ... */ }]
});
```

**Behavior**:
- Shape validation fails → Hook execution fails
- Store remains unchanged
- Exception thrown to caller
- Caller must handle error (retry, rollback, escalate)

**Operational Impact**:
```
Error Rate: Depends on violation likelihood
Throughput: May decrease due to rejections
Latency: Immediate (no repair time)
Data Quality: High (violations blocked)
```

**Use Cases**:
- Financial regulatory compliance
- Data governance enforcements
- Immutable audit trail requirements

**Runbook**:
```
Event: Compliance gate blocking trades
1. Identify violation pattern
2. Escalate to compliance team
3. Update SHACL shape or data fix
4. Re-execute with new shape
```

---

### Annotate Mode (Soft-Fail)

**Configuration**:

```javascript
const hook = createKnowledgeHook({
  name: 'risk-scorer',
  condition: {
    kind: 'shacl',
    ref: { uri: 'file:///shapes/risk.ttl' },
    enforcementMode: 'annotate'  // LOG violations, continue
  },
  effects: [{ /* ... */ }]
});
```

**Behavior**:
- Shape validation fails → Violations stored as RDF triples
- Hook effects executed anyway
- No exception thrown
- Result includes violations list

**Operational Impact**:
```
Error Rate: 0 (never errors, but logs violations)
Throughput: High (no blocking)
Latency: Medium (validation + effect execution)
Data Quality: May be lower (invalid data processed)
```

**Result Structure**:

```javascript
{
  successful: 1,
  failed: 0,
  violations: [
    {
      focusNode: 'ex:trade1',
      resultMessage: 'Minimum count violation',
      severity: 'sh:Violation'
    }
  ]
}
```

**Storage in RDF**:
```turtle
[ a sh:ValidationResult ;
  sh:focusNode ex:trade1 ;
  sh:resultMessage "Minimum count violation" ;
  sh:severity sh:Violation ;
] .
```

**Use Cases**:
- Risk scoring (violations don't prevent processing)
- Warnings system (notify but continue)
- Audit trail (violations queryable via SPARQL)

**Runbook**:
```
Event: High violation rate detected
1. Query violations via SPARQL
2. Analyze patterns in RDF triples
3. Determine if underlying data issue
4. Update quality hooks to address root cause
5. Monitor violation rate trend
```

---

### Repair Mode (Self-Healing)

**Configuration**:

```javascript
const hook = createKnowledgeHook({
  name: 'data-cleaner',
  condition: {
    kind: 'shacl',
    ref: { uri: 'file:///shapes/quality.ttl' },
    enforcementMode: 'repair',
    repairConstruct: `
      CONSTRUCT {
        ?violatedEntity ex:status ex:Repaired ;
                        ex:repairedAt ?now ;
                        ex:repairReason "Missing required field" .
      }
      WHERE {
        ?violation a sh:ValidationResult ;
                   sh:focusNode ?violatedEntity ;
                   sh:resultMessage "Minimum count violation" .
        BIND (NOW() as ?now)
      }
    `
  },
  effects: [{ /* ... */ }]
});
```

**Behavior**:
1. Shape validation fails
2. Execute repairConstruct query
3. Update store with repairs
4. Re-validate shape
5. If still violations, return result
6. Execute effects

**Operational Impact**:
```
Error Rate: 0 (repairs fix violations)
Throughput: Medium (repair overhead)
Latency: High (validation + repair + re-validation)
Data Quality: High (auto-corrected)
Repair Rate: Key metric to monitor
```

**Result Structure**:

```javascript
{
  successful: 1,
  failed: 0,
  repairs: 5,  // Number of repairs applied
  violations: [
    // May still have violations if repair incomplete
  ]
}
```

**Use Cases**:
- Data quality (fill missing fields)
- Auto-remediation (fix known issues)
- Self-healing systems (autonomous correction)

**Monitoring**:
```
Key Metric: Repair rate
- Normal: <5% of entities
- Alert: >10% of entities
- Critical: >50% of entities (indicates root issue)
```

**Runbook**:
```
Event: Repair rate exceeds threshold
1. Analyze repairs applied
2. Identify root cause (upstream data quality?)
3. Fix upstream issue
4. Disable repair temporarily while investigating
5. Resume repairs only for known good patterns
```

---

## Performance Tuning

### Condition Caching

**Enable for read-heavy workloads**:

```javascript
const engine = new KnowledgeHookEngine(store, {
  conditionCache: {
    enabled: true,
    ttlSeconds: 60,  // Cache for 1 minute
    maxSize: 10000   // LRU eviction at 10K entries
  }
});
```

**Cache Hit Impact**:
```
Condition (uncached):     10-100ms
Condition (cached):       <1ms
Potential speedup:        10-100x
```

**When to Enable**:
- Repeated conditions with same parameters
- Read-heavy workloads (>1000 QPS)
- Conditions are slow (N3, Datalog)

**When to Disable**:
- Conditions depend on fresh data
- Memory is constrained
- Data changes frequently

### Query Optimization

**1. SPARQL Query Optimization**:

```javascript
// ❌ SLOW: Unoptimized query
{
  kind: 'sparql-ask',
  query: 'ASK { ?s a ex:Person . ?s ex:name ?name . ?s ex:age ?age . FILTER (?age > 18) }'
}

// ✅ FAST: Optimized query
{
  kind: 'sparql-ask',
  query: 'ASK { ?s a ex:Person ; ex:age ?age . FILTER (?age > 18) }'
  // Path notation is more efficient
}
```

**2. Index Usage**:

Ensure Oxigraph has indexes on frequently queried predicates:
```turtle
# Index on ex:age for faster FILTER
ex:Person rdfs:property ex:age ;
          rdfs:indexed true .
```

**3. Limit Result Size**:

```javascript
// ❌ Unbounded SELECT
{
  kind: 'sparql-select',
  query: 'SELECT ?s WHERE { ?s a ex:Trade }'
}

// ✅ Bounded with LIMIT
{
  kind: 'sparql-select',
  query: 'SELECT ?s WHERE { ?s a ex:Trade } LIMIT 1000'
}
```

### Store Performance

**Monitor store metrics**:

```javascript
const store = createStore();

// Get stats periodically
const stats = store.statistics();
console.log('Triple count:', stats.triples);
console.log('Query latency P95:', stats.queryLatencyP95);
```

**Optimization strategies**:

| Issue | Symptom | Fix |
|---|---|---|
| Large store | P95 > 50ms | Partition by time or domain |
| Many queries | CPU > 80% | Enable condition cache |
| Memory growth | OOM | Implement time-based purge |
| Index misses | Latency spikes | Add missing indexes |

---

## Monitoring & Observability

### Telemetry Schema

Instrument hooks execution with OpenTelemetry:

```javascript
import { trace, metrics } from '@opentelemetry/api';

const tracer = trace.getTracer('hooks');
const meter = metrics.getMeter('hooks');

// Span example
const span = tracer.startSpan('hook.execute');
span.setAttributes({
  'hook.name': hook.name,
  'condition.kind': hook.condition.kind,
  'effect.count': hook.effects.length
});
// ... execution ...
span.end();

// Metric example
const latencyHistogram = meter.createHistogram('hook.latency', {
  unit: 'ms',
  description: 'Hook execution latency'
});
latencyHistogram.record(duration, {
  'hook.name': hook.name,
  'condition.kind': hook.condition.kind
});
```

### Key Metrics

#### Condition Evaluation

```javascript
// Latency by kind
hooks.condition.latency
  ├─ dimension: condition.kind
  │   ├─ sparql-ask: P95 = 5ms
  │   ├─ sparql-select: P95 = 8ms
  │   ├─ shacl: P95 = 15ms
  │   ├─ n3: P95 = 50ms
  │   └─ datalog: P95 = 30ms
  └─ dimension: cached (true/false)

// Condition results
hooks.condition.result
  ├─ dimension: condition.kind
  │   └─ values: passed/failed
  └─ dimension: cached
```

#### Effect Execution

```javascript
// Latency by kind
hooks.effect.latency
  ├─ dimension: effect.kind
  │   ├─ sparql-construct: P95 = 10ms
  │   └─ function: P95 = variable
  └─ dimension: hook.name

// Effect success
hooks.effect.success
  ├─ dimension: effect.kind
  └─ dimension: hook.name

// Changes made
hooks.effect.delta
  ├─ dimension: effect.kind
  │   └─ values: count of adds/deletes
  └─ dimension: hook.name
```

#### Receipt Generation

```javascript
// Receipt latency
hooks.receipt.latency: P95 = <1ms

// Chain length
hooks.receipt.chain_length: current depth

// Verification
hooks.receipt.chain_valid: percentage verified
```

### Alerting Rules

```yaml
alerts:
  - name: ConditionLatencyHigh
    expr: hooks_condition_latency_p95 > 100  # ms
    severity: warning
    action: Page on-call
    
  - name: EffectFailureRate
    expr: hooks_effect_success_rate < 0.99   # 99% threshold
    severity: critical
    action: Page on-call + escalate
    
  - name: Shacl_ViolationRate
    expr: hooks_shacl_violations / hooks_shacl_total > 0.1  # 10% threshold
    severity: warning
    action: Create ticket
    
  - name: ReceiptChainBroken
    expr: hooks_receipt_chain_valid < 1.0
    severity: critical
    action: Page on-call immediately
    
  - name: CacheHitRate
    expr: hooks_condition_cache_hit_rate < 0.5  # 50% threshold
    severity: info
    action: Tune cache parameters
```

### Dashboards

**Hook Execution Overview**:
```
┌─────────────────────────────────────────┐
│ Condition Evaluation                    │
│ • P50/P95/P99 by kind (line chart)      │
│ • Cache hit rate (gauge)                │
│ • Result distribution (pie)             │
└─────────────────────────────────────────┘

┌─────────────────────────────────────────┐
│ Effect Execution                        │
│ • P50/P95/P99 by kind (line chart)      │
│ • Success rate (gauge)                  │
│ • Delta magnitude (histogram)           │
└─────────────────────────────────────────┘

┌─────────────────────────────────────────┐
│ Receipts & Audit Trail                  │
│ • Chain length (number)                 │
│ • Chain valid (boolean)                 │
│ • Receipt generation latency (line)     │
└─────────────────────────────────────────┘

┌─────────────────────────────────────────┐
│ Error Tracking                          │
│ • Error rate by hook (table)            │
│ • Error types (bar)                     │
│ • Recent errors (log stream)            │
└─────────────────────────────────────────┘
```

---

## Receipt Chain Validation

### Offline Verification

```javascript
import { createStore } from '@unrdf/oxigraph';
import { KnowledgeHookEngine } from '@unrdf/hooks';

const store = createStore();
const engine = new KnowledgeHookEngine(store);

// Load execution results from file
const results = JSON.parse(fs.readFileSync('results.json'));

// Verify chain
function verifyChain(receipts) {
  console.log(`Verifying chain of ${receipts.length} receipts...`);
  
  for (let i = 1; i < receipts.length; i++) {
    const prev = receipts[i - 1];
    const curr = receipts[i];
    
    // Check linkage
    if (curr.previousReceiptHash !== prev.receiptHash) {
      console.error(`❌ Receipt ${i}: Linkage broken`);
      console.error(`  Expected: ${prev.receiptHash}`);
      console.error(`  Got: ${curr.previousReceiptHash}`);
      return false;
    }
    
    // Check continuity
    if (curr.input_hash !== prev.output_hash) {
      console.error(`❌ Receipt ${i}: State mismatch`);
      console.error(`  Previous output: ${prev.output_hash}`);
      console.error(`  Current input: ${curr.input_hash}`);
      return false;
    }
    
    // Check deltas sum correctly
    const prevAdds = prev.delta.adds.length;
    const prevDeletes = prev.delta.deletes.length;
    const currAdds = curr.delta.adds.length;
    const currDeletes = curr.delta.deletes.length;
    
    console.log(`✅ Receipt ${i}: Valid`);
    console.log(`  Linkage: OK`);
    console.log(`  State continuity: OK`);
    console.log(`  Delta: +${currAdds} -${currDeletes}`);
  }
  
  return true;
}

const isValid = verifyChain(results.receipts);
console.log(`\nChain verification: ${isValid ? 'PASSED' : 'FAILED'}`);
```

### Recovery Procedures

**If Chain Validation Fails**:

```
1. Identify first invalid receipt (N)
   ├─ Check if N.input_hash matches (N-1).output_hash
   └─ If not, (N-1) execution may have been corrupted

2. Compare stored receipt vs recalculated receipt
   ├─ Run hooks again with same context
   ├─ Compare receiptHash
   └─ If different, execution not deterministic

3. Check previousReceiptHash linkage
   ├─ Verify (N-1).receiptHash equals N.previousReceiptHash
   ├─ If not, receipts reordered or malformed

4. Resolution Options
   ├─ Re-execute from known good receipt
   ├─ Manual audit of store state
   └─ Escalate to forensics team
```

---

## Troubleshooting

### Issue: High Condition Latency

**Symptoms**:
- P95 latency > 100ms
- Condition kind specific (e.g., N3, Datalog)

**Diagnosis**:

```javascript
// 1. Check condition type
const problematicCondition = /* ... */;
console.log('Kind:', problematicCondition.kind);

// 2. Measure in isolation
const start = performance.now();
const result = await engine.evaluateCondition(problematicCondition);
const duration = performance.now() - start;
console.log('Duration:', duration, 'ms');

// 3. Check cache status
const cacheStats = engine.getConditionCacheStats();
console.log('Cache hit rate:', cacheStats.hitRate);
```

**Resolution**:

| Kind | Cause | Fix |
|---|---|---|
| sparql-* | Query complexity | Optimize SPARQL |
| shacl | Large shape | Partition shape |
| n3 | Rule complexity | Simplify rules |
| datalog | Large fact set | Implement indexing |

### Issue: High Memory Usage

**Symptoms**:
- Memory growth over time
- GC pauses increasing

**Diagnosis**:

```javascript
// 1. Check receipt chain length
const chain = engine.getReceiptChain();
console.log('Receipt chain length:', chain.length);
console.log('Memory per receipt:', 2048, 'bytes');
console.log('Total receipt memory:', chain.length * 2048 / 1024 / 1024, 'MB');

// 2. Check condition cache size
const cacheStats = engine.getConditionCacheStats();
console.log('Cache size:', cacheStats.size);

// 3. Check store size
const storeStats = store.statistics();
console.log('Triple count:', storeStats.triples);
console.log('Memory:', storeStats.memory, 'MB');
```

**Resolution**:

| Component | Action |
|---|---|
| Receipt chain | Clear old receipts: `engine.clearReceiptChain()` |
| Condition cache | Reduce TTL or maxSize |
| Store | Implement time-based purge or partitioning |

### Issue: Effect Execution Errors

**Symptoms**:
- Hook execution fails
- Effects not applied

**Diagnosis**:

```javascript
try {
  const result = await engine.execute(ctx, [hook]);
  
  if (result.failed > 0) {
    console.error('Errors:', result.errors);
    result.errors.forEach(err => {
      console.error(`  ${err.hookName}: ${err.message}`);
    });
  }
} catch (err) {
  console.error('Execution exception:', err);
  console.error('Stack:', err.stack);
}
```

**Common Issues**:

| Error | Cause | Fix |
|---|---|---|
| SPARQL syntax error | Malformed query | Validate with SPARQL validator |
| Function timeout | Slow custom code | Increase timeout or optimize |
| RDF shape violation | Data doesn't conform | Fix data or shape |
| Datalog goal unprovable | Goal not derivable | Check facts and rules |

### Issue: Receipt Chain Broken

**Symptoms**:
- `verifyChain()` returns false
- Audit trail integrity compromised

**Diagnosis**:

```javascript
function diagnoseChain(chain) {
  for (let i = 1; i < chain.length; i++) {
    const prev = chain[i - 1];
    const curr = chain[i];
    
    const linkageOk = curr.previousReceiptHash === prev.receiptHash;
    const continuityOk = curr.input_hash === prev.output_hash;
    
    if (!linkageOk || !continuityOk) {
      console.error(`Chain broken at index ${i}`);
      console.error(`  Linkage OK: ${linkageOk}`);
      console.error(`  Continuity OK: ${continuityOk}`);
      return i;
    }
  }
  return -1;  // No break found
}

const breakIndex = diagnoseChain(engine.getReceiptChain());
```

**Resolution**:

1. Identify break point
2. Load store state from previous valid receipt
3. Re-execute hooks from break point
4. Verify receipts regenerated correctly

---

## Capacity Planning

### Load Estimation

**Hook Execution Rate**:
```
Estimated Hooks/sec = Estimated API Requests/sec × Hooks per Request

Example:
  API: 100 req/sec
  Hooks: 5 per request
  = 500 hooks/sec
```

**Resource Allocation**:

| Hooks/sec | CPU | Memory | Disk | Notes |
|---|---|---|---|---|
| <100 | 0.5 core | 512MB | 1GB | Development |
| 100-1K | 2 cores | 2GB | 5GB | Small production |
| 1K-10K | 8 cores | 8GB | 20GB | Medium production |
| >10K | 16+ cores | 16GB+ | 100GB+ | Large production |

### Scaling Strategy

**Vertical Scaling** (Single Node):
```
1. Increase CPU (better parallelism)
2. Increase Memory (larger store)
3. Optimize queries (cache + indexes)
4. Monitor GC behavior
```

**Horizontal Scaling** (Multiple Nodes):
```
1. Shard store by domain/time
2. Run hooks engine per shard
3. Aggregate receipt chains
4. Verify chain integrity across nodes
```

### Performance Targets

| Metric | Target | Action If Exceeded |
|---|---|---|
| P95 Condition Latency | <50ms | Optimize query |
| P95 Effect Latency | <50ms | Optimize CONSTRUCT |
| P95 Receipt Latency | <1ms | Profile hashing |
| Cache Hit Rate | >50% | Tune cache parameters |
| Error Rate | <0.1% | Debug root cause |
| Chain Validity | 100% | Check storage layer |

---

## Operational Checklist

### Daily

- [ ] Monitor dashboard for alerts
- [ ] Review error logs
- [ ] Check chain validation status
- [ ] Verify cache hit rates

### Weekly

- [ ] Review performance trends
- [ ] Analyze violation patterns (if annotate mode)
- [ ] Update capacity forecast
- [ ] Test incident response procedures

### Monthly

- [ ] Full load test
- [ ] Disaster recovery drill
- [ ] Performance optimization review
- [ ] Update runbooks based on learnings

---

See [README.md](./README.md) for overview and [EXAMPLES.md](./EXAMPLES.md) for usage examples.
