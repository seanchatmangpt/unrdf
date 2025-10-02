# KGC Sidecar v2 Migration Plan

## Overview

This document outlines the migration strategy from KGC Sidecar Client v1 to v2, ensuring zero downtime and backward compatibility while introducing graceful degradation, circuit breaker patterns, and health-based service discovery.

## Migration Goals

1. **Zero Breaking Changes**: Existing code continues working without modifications
2. **Gradual Rollout**: Feature flags enable phased migration
3. **Safety First**: Rollback plan for every phase
4. **Test Coverage**: 100% of v1 functionality validated in v2
5. **Performance**: Meet or exceed v1 latency targets

## Timeline

| Phase | Duration | Milestone |
|-------|----------|-----------|
| Phase 1: Parallel Implementation | Week 1 | v2 client implemented, feature flagged |
| Phase 2: Testing & Validation | Week 2 | 150+ tests passing, benchmarks met |
| Phase 3: Gradual Rollout | Week 3 | 10% → 50% → 100% traffic migration |
| Phase 4: Cleanup & Documentation | Week 4 | v1 removal, documentation updates |

## Phase 1: Parallel Implementation (Week 1)

### Objectives
- Implement v2 client alongside v1
- Feature flag v2 usage
- No breaking changes to existing code

### Tasks

**Day 1-2: Core Implementation**
1. ✅ Create `SidecarClientV2` class
2. ✅ Implement health check system
3. ✅ Implement circuit breaker
4. ✅ Implement graceful degradation

**Day 3-4: Integration**
1. Create feature flag system
2. Update `sidecar-helper.mjs` to support both v1 and v2
3. Add environment variable configuration
4. Create migration utilities

**Day 5: Documentation**
1. Architecture documentation (this file)
2. API documentation
3. Configuration guide
4. Troubleshooting guide

### Feature Flag Implementation

```javascript
// src/cli/utils/sidecar-helper.mjs

import { SidecarClient } from '../../sidecar/client.mjs'; // v1
import { SidecarClientV2 } from './sidecar-client-v2.mjs'; // v2

const USE_V2 = process.env.KGC_SIDECAR_V2_ENABLED === 'true';

export async function getSidecarClient(options = {}) {
  if (USE_V2) {
    return getSidecarClientV2(options);
  } else {
    return getSidecarClientV1(options); // existing implementation
  }
}
```

### Environment Configuration

```bash
# .env.development
KGC_SIDECAR_V2_ENABLED=false  # v1 by default

# .env.testing
KGC_SIDECAR_V2_ENABLED=true   # v2 in test environment

# .env.production
KGC_SIDECAR_V2_ENABLED=false  # v1 in production (initially)
```

### Backward Compatibility Checklist

- [ ] All existing v1 methods available in v2
- [ ] v2 constructor accepts v1 options
- [ ] Error codes preserved
- [ ] Event emission compatible
- [ ] Metrics format unchanged

### Rollback Strategy

If critical issues found:
1. Set `KGC_SIDECAR_V2_ENABLED=false`
2. Restart affected services
3. Document issue in GitHub
4. No code changes required

## Phase 2: Testing & Validation (Week 2)

### Objectives
- Validate all 150+ sidecar tests pass with v2
- Performance benchmarking
- Security audit
- Documentation review

### Testing Strategy

**Unit Tests** (Day 1-2)
```bash
# Test v2 client in isolation
npm run test:sidecar:v2

# Expected: 100% pass rate
# Deliverable: test/sidecar/client-v2.test.mjs
```

**Integration Tests** (Day 3-4)
```bash
# Test v2 with real sidecar
KGC_SIDECAR_V2_ENABLED=true npm run test:e2e:sidecar

# Expected: 150+ tests passing
# Deliverable: All existing e2e tests green
```

**Stress Tests** (Day 5)
```bash
# Circuit breaker validation
npm run test:sidecar:circuit-breaker

# Health check performance
npm run test:sidecar:health-check

# Fallback mode validation
npm run test:sidecar:fallback
```

### Performance Benchmarks

| Metric | v1 Target | v2 Target | Validation |
|--------|-----------|-----------|------------|
| Health check latency | N/A | <100ms | Pass/Fail |
| Circuit open detection | N/A | <10ms | Pass/Fail |
| Fallback latency | N/A | <50ms | Pass/Fail |
| gRPC request p99 | <2s | <2s | Pass/Fail |
| Connection pool reuse | 60% | 80% | Pass/Fail |

### Test Coverage Requirements

```bash
# Minimum coverage thresholds
Statements: 90%
Branches: 85%
Functions: 90%
Lines: 90%
```

### Validation Checklist

- [ ] All v1 tests pass with v2
- [ ] Health check completes in <100ms
- [ ] Circuit breaker opens after 3 failures
- [ ] Local fallback works for all operations
- [ ] Connection pooling validated
- [ ] Auto-recovery works
- [ ] Metrics accurate
- [ ] Events emitted correctly
- [ ] Error codes preserved

### Performance Validation

```javascript
// test/sidecar/benchmarks.test.mjs
describe('SidecarClientV2 Performance', () => {
  it('health check completes in <100ms', async () => {
    const start = Date.now();
    await client.forceHealthCheck();
    const duration = Date.now() - start;

    expect(duration).toBeLessThan(100);
  });

  it('circuit breaker opens in <10ms', async () => {
    // Force 3 failures
    for (let i = 0; i < 3; i++) {
      await client.execute('nonexistent').catch(() => {});
    }

    const start = Date.now();
    await client.execute('listHooks').catch(() => {});
    const duration = Date.now() - start;

    expect(duration).toBeLessThan(10); // Immediate rejection
  });
});
```

### Rollback Strategy

If tests fail:
1. Document failure mode
2. Create GitHub issue with reproduction steps
3. Fix in v2 implementation
4. Re-run validation
5. Do NOT proceed to Phase 3

## Phase 3: Gradual Rollout (Week 3)

### Objectives
- Migrate production traffic to v2
- Monitor error rates and latency
- Ensure graceful degradation working

### Rollout Strategy

**10% Traffic (Day 1-2)**
```bash
# Enable v2 for 10% of requests
KGC_SIDECAR_V2_ENABLED=true
KGC_SIDECAR_V2_ROLLOUT_PERCENTAGE=10

# Monitor metrics:
# - Error rate should be ≤ v1
# - Latency p99 should be ≤ v1
# - Fallback rate should be logged
```

**50% Traffic (Day 3-4)**
```bash
KGC_SIDECAR_V2_ROLLOUT_PERCENTAGE=50

# Validation criteria:
# - No increase in error rate
# - No customer complaints
# - Metrics within targets
```

**100% Traffic (Day 5)**
```bash
KGC_SIDECAR_V2_ENABLED=true
KGC_SIDECAR_V2_ROLLOUT_PERCENTAGE=100

# Full migration complete
# Monitor for 48 hours before Phase 4
```

### Monitoring Dashboards

**Key Metrics to Monitor:**

```javascript
// Grafana dashboard queries
{
  "circuit_breaker_state": "gauge",
  "connection_state": "gauge",
  "grpc_request_count": "counter",
  "grpc_error_rate": "rate(grpc_failures_total[5m])",
  "local_fallback_count": "counter",
  "health_check_latency": "histogram",
  "circuit_open_duration": "histogram"
}
```

**Alerting Rules:**

```yaml
# prometheus/alerts.yml
groups:
  - name: sidecar_v2
    rules:
      - alert: CircuitBreakerOpenTooLong
        expr: sidecar_circuit_breaker_state{state="OPEN"} > 0
        for: 5m
        annotations:
          summary: "Circuit breaker open for >5 minutes"

      - alert: HighFallbackRate
        expr: rate(sidecar_local_fallbacks_total[5m]) > 0.5
        for: 10m
        annotations:
          summary: "Fallback rate >50% for 10 minutes"

      - alert: HealthCheckFailureSpike
        expr: rate(sidecar_health_check_failures_total[5m]) > 0.9
        for: 5m
        annotations:
          summary: "Health check failures >90%"
```

### Rollback Strategy

If error rate increases >10%:
1. **Immediate**: Set `KGC_SIDECAR_V2_ROLLOUT_PERCENTAGE=0`
2. **Within 5 minutes**: Verify error rate returns to baseline
3. **Post-incident**: Create GitHub issue with metrics
4. **Next day**: Root cause analysis

### Success Criteria

- [ ] Error rate unchanged or improved
- [ ] Latency p99 ≤ v1 targets
- [ ] Circuit breaker opening <5% of time
- [ ] Fallback mode working (when sidecar unavailable)
- [ ] Auto-recovery working
- [ ] No customer complaints
- [ ] Metrics dashboards green

## Phase 4: Cleanup & Documentation (Week 4)

### Objectives
- Remove v1 client code
- Update all documentation
- Archive old tests
- Knowledge transfer

### Cleanup Tasks

**Day 1-2: Code Removal**
1. Remove `SidecarClient` v1 class
2. Remove feature flag logic
3. Rename `SidecarClientV2` to `SidecarClient`
4. Update imports across codebase

**Day 3: Test Cleanup**
1. Archive v1-specific tests
2. Rename v2 tests to primary
3. Update test documentation

**Day 4-5: Documentation**
1. Update README with v2 features
2. Update architecture docs
3. Create migration guide for users
4. Update API reference

### Code Removal Checklist

```bash
# Files to remove
rm src/sidecar/client-v1.mjs
rm src/cli/utils/sidecar-helper-v1.mjs

# Files to rename
mv src/cli/utils/sidecar-client-v2.mjs src/cli/utils/sidecar-client.mjs
mv test/sidecar/client-v2.test.mjs test/sidecar/client.test.mjs

# Update imports (automated)
npx ts-migrate remap-imports
```

### Documentation Updates

**Files to Update:**
- [ ] `/README.md` - Add v2 features
- [ ] `/docs/architecture/kgc-sidecar-architecture.md` - Mark as v2
- [ ] `/docs/api/sidecar-client.md` - v2 API reference
- [ ] `/docs/troubleshooting.md` - v2 troubleshooting
- [ ] `/CHANGELOG.md` - Add v2 release notes

**New Documentation:**
- [ ] `/docs/guides/sidecar-migration-guide.md` - User migration guide
- [ ] `/docs/configuration/sidecar-v2-config.md` - Configuration reference
- [ ] `/docs/architecture/circuit-breaker.md` - Circuit breaker details

### Knowledge Transfer

**Team Training:**
1. Present v2 architecture to team
2. Demo health check and circuit breaker
3. Walkthrough troubleshooting guide
4. Q&A session

**External Communication:**
1. Blog post announcing v2
2. Release notes in CHANGELOG
3. GitHub Discussions announcement
4. Update public documentation

### Final Validation

```bash
# Ensure all tests pass
npm run test

# Ensure no v1 references remain
grep -r "SidecarClientV1" .
grep -r "KGC_SIDECAR_V2_ENABLED" .

# Ensure documentation up to date
npm run docs:build
npm run docs:validate
```

## Rollback Plan (Emergency)

If critical production issue found after Phase 4:

### Option 1: Revert to v1 (Fast)
```bash
# Revert v2 implementation commit
git revert HEAD~5..HEAD

# Deploy immediately
npm run deploy:production

# ETA: 15 minutes
```

### Option 2: Quick Fix (Medium)
```bash
# Fix critical bug in v2
git checkout -b hotfix/sidecar-v2-critical

# Deploy patch
npm run deploy:production

# ETA: 1-2 hours
```

### Option 3: Hotfix Release (Slow)
```bash
# Create hotfix branch from last stable
git checkout -b hotfix/v2.1.1 v2.1.0

# Cherry-pick fixes
git cherry-pick <commit-hash>

# Release
npm run release:hotfix

# ETA: 4-8 hours
```

## Risk Mitigation

### High-Risk Areas

1. **Circuit Breaker Logic**
   - Risk: Circuit opens prematurely
   - Mitigation: Extensive testing in Phase 2
   - Monitoring: Alert if circuit open >5min

2. **Health Check Performance**
   - Risk: Health checks add latency
   - Mitigation: 100ms timeout, exponential backoff
   - Monitoring: Alert if p99 >200ms

3. **Local Fallback Correctness**
   - Risk: Local mode behaves differently than gRPC
   - Mitigation: Integration tests comparing outputs
   - Monitoring: Alert if local error rate >5%

4. **Connection Pool Exhaustion**
   - Risk: Pool runs out of connections
   - Mitigation: Configurable pool size, acquire timeout
   - Monitoring: Alert if wait queue >10

### Mitigation Strategies

| Risk | Probability | Impact | Mitigation |
|------|-------------|--------|------------|
| Circuit breaker false positives | Medium | High | Conservative threshold (3 failures) |
| Health check overhead | Low | Medium | 100ms timeout, minimal logic |
| Fallback inconsistency | Medium | High | Comprehensive integration tests |
| Connection pool exhaustion | Low | High | Adaptive sizing, monitoring |
| Performance regression | Low | Medium | Benchmarks in Phase 2 |

## Success Metrics

### Technical Metrics

| Metric | Baseline (v1) | Target (v2) | Actual |
|--------|---------------|-------------|--------|
| Health check latency p99 | N/A | <100ms | ___ |
| Circuit open rate | 0% | <5% | ___ |
| Fallback success rate | N/A | >95% | ___ |
| gRPC request latency p99 | <2s | <2s | ___ |
| Error rate | ___ % | ≤ baseline | ___ |
| Connection pool utilization | 60% | 80% | ___ |

### Business Metrics

| Metric | Target |
|--------|--------|
| Test failures fixed | 150+ |
| Zero downtime deployment | ✓ |
| Customer complaints | 0 |
| Documentation updated | 100% |
| Team trained | 100% |

## Post-Migration Review

### Week 4 Retrospective

**What Went Well:**
- [ ] Gradual rollout prevented issues
- [ ] Feature flags enabled safe testing
- [ ] Documentation comprehensive

**What Could Improve:**
- [ ] Earlier performance testing
- [ ] More aggressive monitoring
- [ ] Better team communication

**Action Items:**
- [ ] Create migration playbook for future
- [ ] Improve testing infrastructure
- [ ] Enhance monitoring dashboards

## Appendix

### Environment Variables Reference

```bash
# Feature Flag
KGC_SIDECAR_V2_ENABLED=true|false

# Health Check
KGC_SIDECAR_HEALTH_CHECK_INTERVAL=5000
KGC_SIDECAR_HEALTH_CHECK_TIMEOUT=100
KGC_SIDECAR_HEALTH_CHECK_RETRIES=3

# Circuit Breaker
KGC_SIDECAR_CIRCUIT_BREAKER_THRESHOLD=3
KGC_SIDECAR_CIRCUIT_BREAKER_TIMEOUT=30000
KGC_SIDECAR_CIRCUIT_BREAKER_HALF_OPEN_REQUESTS=3

# Fallback
KGC_SIDECAR_FALLBACK_MODE=local|none
KGC_SIDECAR_FALLBACK_AUTO_RECOVER=true|false

# Retry
KGC_SIDECAR_RETRY_MAX_ATTEMPTS=3
KGC_SIDECAR_RETRY_INITIAL_DELAY=100
KGC_SIDECAR_RETRY_MAX_DELAY=1600
KGC_SIDECAR_RETRY_MULTIPLIER=2

# Connection Pool
KGC_SIDECAR_POOL_MIN_CONNECTIONS=2
KGC_SIDECAR_POOL_MAX_CONNECTIONS=10
KGC_SIDECAR_POOL_IDLE_TIMEOUT=60000

# Rollout Control
KGC_SIDECAR_V2_ROLLOUT_PERCENTAGE=0-100
```

### Key Contacts

| Role | Name | Contact |
|------|------|---------|
| Project Lead | TBD | email@example.com |
| Backend Engineer | TBD | email@example.com |
| DevOps | TBD | email@example.com |
| QA Lead | TBD | email@example.com |

### References

- [KGC Sidecar v2 Architecture](./kgc-sidecar-v2-architecture.md)
- [Circuit Breaker Pattern](https://martinfowler.com/bliki/CircuitBreaker.html)
- [gRPC Health Checking](https://github.com/grpc/grpc/blob/master/doc/health-checking.md)
- [Blue-Green Deployment](https://martinfowler.com/bliki/BlueGreenDeployment.html)
