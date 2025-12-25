# Production Remediation Plan
## Path to 10/10 Production Score

**Current Score:** 4.5/10
**Target Score:** 10/10
**Estimated Timeline:** 3-5 business days
**Last Updated:** 2025-12-25

---

## CRITICAL PATH

### Phase 1: Immediate Fixes (Day 1 - 4 hours)

#### 1.1 Fix Lockfile Mismatch
**Priority:** P1 (Blocker)
**Estimated Time:** 5 minutes
**Owner:** DevOps/Build Engineer

**Steps:**
```bash
cd /home/user/unrdf
pnpm install
git add pnpm-lock.yaml
git commit -m "fix: Regenerate pnpm lockfile to match package.json"
```

**Verification:**
```bash
pnpm install --frozen-lockfile  # Should succeed
echo "Exit code: $?"  # Should be 0
```

**Success Criteria:** Clean install with frozen lockfile

---

#### 1.2 Generate Test Coverage Report
**Priority:** P2
**Estimated Time:** 30 minutes
**Owner:** QA Engineer

**Steps:**
```bash
pnpm test:coverage
cat coverage/coverage-summary.json | grep -A 10 "total"
```

**Success Criteria:** Coverage report generated, percentage known

---

#### 1.3 Fix Avatar E2E Tests
**Priority:** P2
**Estimated Time:** 4 hours
**Owner:** Frontend/Docs Engineer

**Option A: Fix Tests (if tests are valid)**
```bash
cd packages/docs
npm run test:debug e2e/avatars/
# Debug and fix each failing test
```

**Option B: Skip Tests (if tests are outdated/broken)**
```bash
# Add .skip to each failing test
# OR update package.json test script to exclude avatars
"test": "vitest --exclude='**/avatars/**'"
```

**Success Criteria:** All tests pass OR tests properly skipped with documentation

---

### Phase 2: Code Quality Remediation (Days 2-3 - 16 hours)

#### 2.1 Refactor Core Utils Files
**Priority:** P1 (Blocker)
**Estimated Time:** 8 hours
**Owner:** Backend Engineer

**Files to Refactor (13 files in /packages/core/src/utils/):**

**Strategy:** Extract utility functions into smaller, focused modules

**Example: adaptive-monitor.mjs (746 → <500 lines)**
```javascript
// Before: adaptive-monitor.mjs (746 lines)
// After: Split into:
// - adaptive-monitor.mjs (core logic, 300 lines)
// - adaptive-monitor-metrics.mjs (metrics collection, 200 lines)
// - adaptive-monitor-rules.mjs (rule engine, 200 lines)
```

**Refactoring Checklist per File:**
1. Identify logical boundaries (functions, classes, modules)
2. Extract into separate files (maintain single responsibility)
3. Update imports in dependent files
4. Run tests after each extraction
5. Verify linter still passes
6. Update JSDoc with cross-references

**Files Priority Order:**
1. `core/test/sparql/executor-sync.test.mjs` (869 lines) - Split test suites
2. `core/src/utils/quality-utils.mjs` (754 lines)
3. `core/src/utils/transaction.mjs` (748 lines)
4. `core/src/utils/adaptive-monitor.mjs` (746 lines)
5. `dark-matter/src/dark-matter-core.mjs` (743 lines)
6. `core/test/sparql/branch-coverage.test.mjs` (720 lines)
7. Remaining 14 files (503-703 lines each)

**Verification:**
```bash
find packages -name "*.mjs" -o -name "*.js" | xargs wc -l | awk '$1 > 500'
# Should return ZERO results
```

**Success Criteria:** 0 files exceed 500 lines

---

#### 2.2 Refactor AtomVM Playground Files
**Priority:** P2
**Estimated Time:** 3 hours
**Owner:** Frontend Engineer

**Files:**
- `atomvm/playground/src/bridge-interceptor.mjs` (781 lines)
- `atomvm/playground/src/kgc4d-bridge.mjs` (682 lines)

**Strategy:** Extract bridge logic into domain-specific modules

**Example: bridge-interceptor.mjs**
```javascript
// Split into:
// - bridge-interceptor.mjs (core interceptor, 400 lines)
// - bridge-message-handlers.mjs (message handling, 200 lines)
// - bridge-validators.mjs (validation logic, 150 lines)
```

**Success Criteria:** Both files <500 lines, playground still functional

---

#### 2.3 Refactor Federation Files
**Priority:** P2
**Estimated Time:** 5 hours
**Owner:** Distributed Systems Engineer

**Files:**
- `federation/src/federation/data-replication.mjs` (703 lines)
- `federation/src/federation/consensus-manager.mjs` (586 lines)
- `federation/src/federation/distributed-query-engine.mjs` (568 lines)

**Strategy:** Apply microservices decomposition principles

**Example: data-replication.mjs**
```javascript
// Split into:
// - data-replication.mjs (orchestration, 300 lines)
// - replication-strategy.mjs (strategy pattern, 200 lines)
// - replication-conflict-resolver.mjs (conflict resolution, 200 lines)
```

**Success Criteria:** All 3 files <500 lines, federation tests pass

---

### Phase 3: OTEL System Repair (Day 3-4 - 12 hours)

#### 3.1 Debug OTEL Span Collection
**Priority:** P1 (Critical Blocker)
**Estimated Time:** 6 hours
**Owner:** Observability Engineer

**Investigation Steps:**
```bash
# 1. Check TracerProvider initialization
node -e "
  import { trace } from '@opentelemetry/api';
  const tracer = trace.getTracer('test');
  const span = tracer.startSpan('test-span');
  span.end();
  console.log('Span created:', span);
"

# 2. Verify exporter configuration
grep -r "SpanExporter" packages/validation/

# 3. Check processor registration
grep -r "BatchSpanProcessor\|SimpleSpanProcessor" packages/validation/

# 4. Verify provider registration
grep -r "registerGlobalTracerProvider\|setGlobalTracerProvider" packages/validation/
```

**Known Issues (from logs):**
- `[OTEL Provider] Registration failed - provider not active`
- `[OTEL Provider] Error during processor.forceFlush: forceFlush timeout after 5s`
- `[OTELValidator] Collected 0 spans, throughput: 0`

**Root Cause Hypotheses:**
1. TracerProvider not properly registered as global provider
2. Span exporter not receiving spans from processor
3. Callback registration timing issue
4. Async span processing not completing before collection

**Debugging Approach:**
```javascript
// Add detailed logging to validation/otel-provider.mjs
import { trace, context } from '@opentelemetry/api';

export function debugTracerProvider() {
  const provider = trace.getTracerProvider();
  console.log('[DEBUG] Provider type:', provider.constructor.name);
  console.log('[DEBUG] Active span:', trace.getActiveSpan());

  const tracer = provider.getTracer('debug-tracer');
  const span = tracer.startSpan('debug-span');
  console.log('[DEBUG] Span created:', {
    spanId: span.spanContext().spanId,
    traceId: span.spanContext().traceId,
    isRecording: span.isRecording()
  });
  span.end();
}
```

**Success Criteria:**
- TracerProvider initializes successfully
- Spans are created and recorded
- Spans are exported to callback
- forceFlush completes without timeout

---

#### 3.2 Fix Span Export Pipeline
**Priority:** P1
**Estimated Time:** 4 hours
**Owner:** Observability Engineer

**Tasks:**
1. Verify `InMemorySpanExporter` is receiving spans
2. Check callback registration timing
3. Ensure span processor is flushing before collection
4. Add retry logic for async operations

**Code Review Focus:**
- `packages/validation/src/otel-validator-core.mjs`
- `packages/validation/src/otel-span-builder.mjs`
- `validation/otel-provider.mjs`

**Success Criteria:** Spans successfully collected and validated

---

#### 3.3 Re-run OTEL Validation
**Priority:** P1
**Estimated Time:** 2 hours
**Owner:** Observability Engineer

**Steps:**
```bash
node validation/run-all.mjs comprehensive 2>&1 | tee validation-results.log
grep "Score:" validation-results.log
grep "FAILED" validation-results.log | wc -l  # Should be 0
```

**Success Criteria:**
- All 6 features collect spans
- Validation score ≥80/100
- 0 features fail
- No forceFlush timeouts

---

### Phase 4: Performance Validation (Day 4 - 3 hours)

#### 4.1 Run Performance Benchmarks
**Priority:** P3
**Estimated Time:** 2 hours
**Owner:** Performance Engineer

**Steps:**
```bash
# Create benchmark suite if missing
npm run benchmark || echo "Need to create benchmark suite"

# Measure test suite performance
time pnpm test
# Target: <3s for fast feedback

# Measure linter performance
time npm run lint
# Target: <18s

# Measure throughput
# Create benchmark: packages/yawl/benchmarks/throughput.mjs
node packages/yawl/benchmarks/throughput.mjs
# Target: ≥5,100 cases/sec
```

**Success Criteria:**
- Benchmarks exist and run successfully
- Baseline metrics documented
- Performance within SLA targets

---

#### 4.2 Memory Leak Detection
**Priority:** P3
**Estimated Time:** 1 hour
**Owner:** Performance Engineer

**Steps:**
```bash
# Run tests with --expose-gc
node --expose-gc node_modules/.bin/vitest run

# Profile memory usage
node --inspect packages/yawl/test/patterns/pattern-integration.test.mjs
# Use Chrome DevTools to check for leaks
```

**Success Criteria:** No memory leaks detected in core workflows

---

### Phase 5: Final Validation (Day 5 - 2 hours)

#### 5.1 Comprehensive Re-validation
**Priority:** P1
**Estimated Time:** 1 hour
**Owner:** QA Lead

**Checklist:**
```bash
# 1. Clean environment
rm -rf node_modules pnpm-lock.yaml
pnpm install

# 2. Linter (must be 0 errors)
npm run lint
echo "Linter exit code: $?"

# 3. Tests (must be 100% pass)
pnpm -r test
grep -E "Test Files.*failed" test-output.log  # Should be 0

# 4. Coverage (must be ≥80%)
pnpm test:coverage
cat coverage/coverage-summary.json | grep "pct"

# 5. Security (must be 0 CRITICAL/HIGH)
pnpm audit --production --json | grep -E "high|critical"

# 6. File sizes (must be 0 violations)
find packages -name "*.mjs" -o -name "*.js" | xargs wc -l | awk '$1 > 500'

# 7. OTEL validation (must be ≥80/100)
node validation/run-all.mjs comprehensive | grep "Score:"

# 8. Performance (must meet SLA)
npm run benchmark
```

**Success Criteria:** ALL checks pass

---

#### 5.2 Generate Final Report
**Priority:** P1
**Estimated Time:** 1 hour
**Owner:** Technical Lead

**Steps:**
```bash
# Run final validation
node validation/run-all.mjs comprehensive > final-validation.log 2>&1

# Generate deployment package
tar -czf unrdf-v5.0.1-production.tar.gz \
  packages/ \
  validation/ \
  documentation/ \
  PRODUCTION_VALIDATION_REPORT.md \
  final-validation.log
```

**Deliverables:**
1. Updated PRODUCTION_VALIDATION_REPORT.md (10/10 score)
2. Test coverage report (HTML + JSON)
3. Performance benchmark results
4. OTEL validation report (≥80/100)
5. Deployment package (.tar.gz)

**Success Criteria:** Production score = 10/10

---

## RISK MITIGATION

### High-Risk Changes
1. **File Refactoring (20 files)**
   - Risk: Breaking existing functionality
   - Mitigation: Run tests after each file refactoring
   - Rollback: Git branch per file group

2. **OTEL System Repair**
   - Risk: Complex async debugging
   - Mitigation: Add extensive logging, test incrementally
   - Rollback: Revert to previous OTEL version

### Dependencies
- Phase 3 (OTEL) is independent of Phase 2 (refactoring)
- Phase 1 must complete before Phase 5
- Phases 2 and 3 can run in parallel with different engineers

---

## RESOURCE ALLOCATION

| Phase | Engineer Type | Hours | Priority |
|-------|---------------|-------|----------|
| Phase 1 | DevOps + QA + Frontend | 4 | P1 |
| Phase 2.1 | Backend (Core) | 8 | P1 |
| Phase 2.2 | Frontend (AtomVM) | 3 | P2 |
| Phase 2.3 | Distributed Systems | 5 | P2 |
| Phase 3 | Observability | 12 | P1 |
| Phase 4 | Performance | 3 | P3 |
| Phase 5 | QA Lead + Tech Lead | 2 | P1 |
| **TOTAL** | **Multi-discipline** | **37 hours** | **Mixed** |

**Recommended Team:**
- 1 Backend Engineer (full-time, 3 days)
- 1 Observability Engineer (full-time, 2 days)
- 1 Frontend Engineer (part-time, 1 day)
- 1 QA Engineer (part-time, 1 day)
- 1 Technical Lead (oversight, final validation)

---

## PROGRESS TRACKING

### Daily Standup Questions
1. Which files have been refactored today?
2. How many files still exceed 500 lines?
3. Is OTEL collecting spans yet?
4. What blockers need escalation?

### Success Metrics Dashboard
```javascript
const metrics = {
  filesOver500Lines: 0,      // Target: 0 (current: 20)
  testsPassing: 918,          // Target: 918/918
  otelScore: 80,              // Target: ≥80/100
  securityCVEs: 0,            // Target: 0
  testCoverage: 85,           // Target: ≥80%
  linterErrors: 0,            // Target: 0
  productionScore: 10.0       // Target: 10/10
};
```

---

## CONTINGENCY PLANS

### If OTEL Cannot Be Fixed in Time
**Fallback:** Deploy without OTEL validation, but add manual testing checklist
**Risk:** Reduced observability in production
**Timeline:** Buys 2 days

### If File Refactoring Takes Too Long
**Fallback:** Accept 5-10 files over limit with documented exceptions
**Risk:** Technical debt remains
**Timeline:** Saves 1 day

### If E2E Tests Cannot Be Fixed
**Fallback:** Skip avatar tests, add manual QA checklist
**Risk:** Reduced test coverage for docs site
**Timeline:** Saves 4 hours

---

## POST-DEPLOYMENT

### Monitoring Setup
1. Configure OTEL in production
2. Set up alerting for key metrics
3. Create runbooks for common issues

### Documentation Updates
1. Update deployment guide
2. Document architectural decisions (file splits)
3. Create troubleshooting guide

### Retrospective Questions
1. Why did OTEL validation break?
2. How did 20 files grow over 500 lines?
3. What process prevents this in future?

---

**Plan Owner:** Production Validation Team
**Last Updated:** 2025-12-25
**Next Review:** After Phase 1 completion

**Status:** READY FOR EXECUTION
