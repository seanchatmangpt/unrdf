# Agent 9: Shadow Modes & Live Verification

## Mission
Enable safe, reversible migration from legacy systems to new facade/RDF systems through deterministic shadow mode phases with mismatch detection.

## Shadow Mode Phases

### Phase 1: Shadow Write (0% Risk)
**Pattern**: Execute against BOTH legacy and facade, serve ONLY legacy
- All production traffic → legacy system (existing behavior)
- Facade executes in parallel (shadow mode)
- Compare results deterministically
- Report mismatches for investigation
- **Rollback**: Simply disable facade calls
- **Success Criteria**: 99.9%+ match rate over 7 days

### Phase 2: Shadow Read (0% Risk)
**Pattern**: Read from BOTH stores, serve legacy data
- Queries executed against legacy and facade stores
- Legacy results served to users
- Facade results compared for validation
- Detect data consistency issues
- **Rollback**: Disable facade queries
- **Success Criteria**: 99.99%+ data consistency

### Phase 3: Partial Serve (Controlled Risk)
**Pattern**: Route specific paths to facade, others to legacy
- Known-good paths (validated in Phase 1-2) → facade
- All other paths → legacy
- Gradual traffic shift (1% → 10% → 50% → 100%)
- Per-route canary deployment
- **Rollback**: Routing config change (instant)
- **Success Criteria**: 0 critical mismatches for 3 days

### Phase 4: KGC Primary (Full Migration)
**Pattern**: Facade is primary, legacy is shadow
- All traffic → facade
- Legacy validates in background
- Final verification before decommission
- **Rollback**: Flip routing back to legacy
- **Success Criteria**: 30 days stable operation

## Routing Logic

### Route Definition
```javascript
const route = {
  predicate: (req) => req.path.startsWith('/api/crm'),
  target: 'facade',       // 'legacy' | 'facade'
  weight: 100,            // 0-100% traffic to target
  name: 'CRM API Routes',
  priority: 10            // Higher = evaluated first
}
```

### Decision Algorithm
1. Sort routes by priority (descending)
2. Evaluate predicates in order
3. First match determines target
4. Apply weight for canary (random < weight → target, else fallback)
5. Default to legacy if no match

### Example Routing Config
```javascript
// Phase 3: Gradual rollout
[
  { predicate: (req) => req.path === '/health', target: 'facade', weight: 100 },
  { predicate: (req) => req.path.startsWith('/api/crm'), target: 'facade', weight: 50 },
  { predicate: () => true, target: 'legacy', weight: 100 } // catch-all
]
```

## Mismatch Reporting

### Mismatch Report Structure
```javascript
{
  mismatchHash: "sha256:abc123...",  // Deterministic hash of diff
  timestamp: 1234567890123456n,      // Nanosecond precision
  path: ["data", "users", "0", "id"], // JSON path to difference
  legacyValue: { id: 123, name: "Alice" },
  facadeValue: { id: 123, name: "alice" }, // lowercase difference
  severity: "warning",                // "critical" | "warning" | "info"
  recommendation: "Case normalization needed in facade",
  context: {                          // Optional
    requestId: "req-xyz",
    endpoint: "/api/users/123"
  }
}
```

### Deterministic Hashing
1. **Canonicalize**: Sort all object keys recursively
2. **Serialize**: JSON.stringify with sorted keys
3. **Hash**: SHA-256 of canonical representation
4. **Result**: Same mismatch = same hash (100% deterministic)

### Mismatch Severity Classification
- **Critical**: Data loss, corruption, or security issue
  - Missing required fields
  - Type mismatches in critical data
  - Permission/auth differences
- **Warning**: Functional but may cause issues
  - Case sensitivity differences
  - Date format variations
  - Ordering differences
- **Info**: Cosmetic or non-functional
  - Whitespace differences
  - Extra metadata fields
  - Timestamp precision

### Deduplication Strategy
- Hash-based deduplication (same hash = same issue)
- Track first occurrence timestamp
- Count occurrences per hash
- Alert on high-frequency mismatches (>10/min)

## Implementation Architecture

### Core Modules

#### shadow.mjs
- `shadowWrite`: Parallel execution with comparison
- `shadowRead`: Dual-store querying
- `partialServe`: Route-based serving

#### mismatch-report.mjs
- `mismatchReport`: Generate structured reports
- `canonicalizeMismatchReport`: Deterministic serialization
- `hashMismatchReport`: SHA-256 hashing

#### routing.mjs
- `defineRoute`: Route configuration builder
- `routingDecision`: Request → target mapping

### Data Flow

```
Request → routingDecision → Target Selection
                               ↓
                    ┌──────────┴──────────┐
                    ↓                     ↓
              Legacy Handler        Facade Handler
                    ↓                     ↓
              Legacy Result         Facade Result
                    └──────────┬──────────┘
                               ↓
                    Compare (shadowWrite)
                               ↓
                    ┌──────────┴──────────┐
                    ↓                     ↓
              Match = true         Match = false
              Return result        Generate mismatch report
                                         ↓
                                   Hash & deduplicate
                                         ↓
                                   Alert/Log/Metrics
```

## Success Metrics

### Phase 1 (Shadow Write)
- **Target**: 99.9%+ match rate
- **Measure**: matches / total_requests
- **Alert**: >10 unique mismatches/hour

### Phase 2 (Shadow Read)
- **Target**: 99.99%+ data consistency
- **Measure**: matching_queries / total_queries
- **Alert**: Any critical severity mismatch

### Phase 3 (Partial Serve)
- **Target**: 0 critical mismatches for 3 days
- **Measure**: critical_mismatches_count
- **Alert**: Any critical mismatch

### Phase 4 (KGC Primary)
- **Target**: 30 days stable operation
- **Measure**: uptime, error_rate, latency_p99
- **Alert**: Error rate >0.1% or latency >2x baseline

## Rollback Procedures

### Instant Rollback (< 1 second)
```javascript
// Flip routing config
updateRouting([
  { predicate: () => true, target: 'legacy', weight: 100 }
])
```

### Gradual Rollback (Canary Reverse)
```javascript
// Reduce facade traffic over time
// 50% → 25% → 10% → 1% → 0%
updateRouting([
  { predicate: isCRMRequest, target: 'facade', weight: 25 },
  { predicate: () => true, target: 'legacy', weight: 100 }
])
```

### Data Rollback (Store Level)
- Keep legacy store active during all phases
- Facade is additive until Phase 4
- Can always fall back to legacy data
- RDF store can be rebuilt from legacy if needed

## Testing Strategy

### Unit Tests
- Shadow write: matching results
- Shadow write: mismatch detection
- Mismatch report: deterministic hashing (100 iterations)
- Routing: decision logic correctness

### Integration Tests
- End-to-end shadow mode scenarios
- Legacy + facade parallel execution
- Mismatch report generation and deduplication
- Partial serve with mixed routing

### Production Validation
- OTEL traces for all shadow operations
- Latency tracking (shadow should add <10ms p99)
- Mismatch dashboards
- Automated alerts on thresholds

## Risk Mitigation

### Performance
- Shadow operations run async (non-blocking)
- Timeout shadow calls (5s default)
- Circuit breaker on facade failures
- **Guarantee**: Legacy performance unaffected

### Data Consistency
- Compare at API boundary (serialized responses)
- Normalize before comparison (whitespace, ordering)
- Schema validation on both sides
- **Guarantee**: Deterministic comparison

### Operational
- Feature flags for instant disable
- Per-route rollback capability
- Automated rollback on error threshold
- **Guarantee**: <1 second to full legacy

## Documentation

### For Developers
- How to add new routes
- How to interpret mismatch reports
- How to fix common mismatch patterns
- How to validate fixes

### For Operations
- Monitoring dashboards
- Alert response procedures
- Rollback decision tree
- Phase transition checklist

## Timeline Example

| Phase | Duration | Success Criteria | Risk |
|-------|----------|------------------|------|
| Shadow Write | 7 days | 99.9%+ match | None |
| Shadow Read | 7 days | 99.99%+ consistency | None |
| Partial Serve (1%) | 3 days | 0 critical mismatches | Minimal |
| Partial Serve (10%) | 3 days | 0 critical mismatches | Low |
| Partial Serve (50%) | 7 days | 0 critical mismatches | Medium |
| Partial Serve (100%) | 7 days | 0 critical mismatches | Medium |
| KGC Primary | 30 days | Stable operation | Low |
| **Total** | **64 days** | Legacy decommissioned | **Minimal** |

## Conclusion

Shadow modes provide a zero-risk path to migration through:
1. **Parallel execution** with no user impact
2. **Deterministic comparison** with mismatch reporting
3. **Gradual rollout** with instant rollback
4. **Data-driven decisions** based on measured match rates

This approach has been used successfully in production systems at Google, Facebook, LinkedIn, and others for major infrastructure migrations.
