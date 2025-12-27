# KGC Probe Implementation Blueprint

**Status**: PLAN PHASE - Implementation Ready
**Date**: 2025-12-27
**Agent**: Agent-2 (Code Goal Planner)
**Methodology**: SPARC + Big Bang 80/20

---

## Executive Summary

This blueprint defines the complete implementation path for the `@unrdf/kgc-probe` package. Based on analysis of:
- **SPARC Specifications**: 48 files, 38,946 lines detailing 10 probe domains
- **Architecture Document**: 44 modules across 7 directories
- **Existing Skeleton**: 8 files with basic structure, ~1,500 LoC

**Gap Analysis**: The current skeleton implements ~25% of the specification. Major gaps include:
- Receipts layer (0% complete)
- CLI commands (0% complete)
- Full agent implementations (10% complete - stubs only)
- Database storage integration (10% complete)
- OTEL instrumentation (0% complete)

**Total Implementation Effort**: ~160 hours across 10 parallel agents

---

## Milestone Structure

### Milestone 1: Core Infrastructure (Types, Guards, Storage Foundation)

**Assigned Agents**: Agent-1 (Infrastructure), Agent-5 (CLI Foundation)
**Effort**: 16 hours
**Dependencies**: None (Foundation layer)

#### Files to Create/Modify

| File Path | Action | Size Est. | Description |
|-----------|--------|-----------|-------------|
| `src/schemas/index.mjs` | CREATE | 50 LOC | Schema barrel export |
| `src/schemas/observation.schema.mjs` | CREATE | 80 LOC | Full observation Zod schema |
| `src/schemas/probe-result.schema.mjs` | CREATE | 70 LOC | Probe result schema |
| `src/schemas/guard-policy.schema.mjs` | CREATE | 60 LOC | Guard configuration schema |
| `src/schemas/merge-config.schema.mjs` | CREATE | 55 LOC | Merge strategy schema |
| `src/schemas/receipt-metadata.schema.mjs` | CREATE | 75 LOC | Receipt structure schema |
| `src/utils/index.mjs` | CREATE | 30 LOC | Utils barrel export |
| `src/utils/logger.mjs` | CREATE | 120 LOC | OTEL-ready structured logging |
| `src/utils/error-handler.mjs` | CREATE | 100 LOC | Centralized error handling |
| `src/utils/types.mjs` | CREATE | 150 LOC | JSDoc type definitions |
| `src/types.mjs` | MODIFY | +50 LOC | Add missing schema imports |
| `src/guards.mjs` | MODIFY | +100 LOC | Add policy configuration |

#### Acceptance Criteria

- [ ] All 5 schema files pass Zod compilation
- [ ] Logger outputs structured JSON with trace IDs
- [ ] Error handler provides stack traces + context
- [ ] 100% JSDoc coverage on types.mjs
- [ ] `timeout 5s npm test -- test/schemas.test.mjs` passes with 25+ tests

#### Success Proof

```bash
# Verify schemas
node -e "import('./src/schemas/index.mjs').then(s => console.log(Object.keys(s)))"
# Expected: ['ObservationSchema', 'ProbeResultSchema', 'GuardPolicySchema', ...]

# Verify logger
node -e "import('./src/utils/logger.mjs').then(l => l.createLogger().info('test'))"
# Expected: {"level":"info","msg":"test","time":"...","trace_id":"..."}

# Run tests
timeout 5s npm test -- test/schemas.test.mjs
# Expected: 25+ tests PASS
```

---

### Milestone 2: RDF Storage Layer + KnowledgeStore Integration

**Assigned Agents**: Agent-4 (Storage Specialist)
**Effort**: 20 hours
**Dependencies**: Milestone 1 (schemas)

#### Files to Create/Modify

| File Path | Action | Size Est. | Description |
|-----------|--------|-----------|-------------|
| `src/storage/probe-store.mjs` | CREATE | 180 LOC | KnowledgeStore wrapper |
| `src/storage/graph-builder.mjs` | CREATE | 150 LOC | RDF graph construction |
| `src/storage/triple-generator.mjs` | CREATE | 120 LOC | Quad generation |
| `src/storage/namespaces.mjs` | CREATE | 80 LOC | RDF namespace constants |
| `src/storage/index.mjs` | MODIFY | +50 LOC | Export new modules |

#### Integration Points

```javascript
// Required imports from existing packages
import { createStore, dataFactory } from '@unrdf/oxigraph';
import { KnowledgeStore } from '@unrdf/kgc-substrate';
```

#### Acceptance Criteria

- [ ] ProbeStore extends KnowledgeStore correctly
- [ ] GraphBuilder produces valid N-Quads
- [ ] TripleGenerator uses oxigraph dataFactory
- [ ] All namespaces conform to RDF standards
- [ ] DatabaseStorage fully operational (not stubbed)
- [ ] `timeout 10s npm test -- test/storage.test.mjs` passes with 30+ tests

#### Success Proof

```bash
# Verify ProbeStore
node -e "
import { createProbeStore } from './src/storage/probe-store.mjs';
const store = await createProbeStore({ nodeId: 'test' });
console.log('ProbeStore created:', !!store);
"
# Expected: ProbeStore created: true

# Verify triple generation
node -e "
import { TripleGenerator } from './src/storage/triple-generator.mjs';
const gen = new TripleGenerator();
const quad = gen.generateTriple('ex:s', 'ex:p', 'ex:o');
console.log('Quad valid:', !!quad.subject);
"
# Expected: Quad valid: true
```

---

### Milestone 3: 10 Agent Shards (Full Implementation)

**Assigned Agents**: Agent-4 (Parallel execution across 10 sub-tasks)
**Effort**: 40 hours (4 hours per agent)
**Dependencies**: Milestone 2 (storage layer)

#### Files to Create/Modify

| File Path | Action | Size Est. | Description |
|-----------|--------|-----------|-------------|
| `src/agents/security-probe.mjs` | CREATE | 200 LOC | Security validations |
| `src/agents/performance-probe.mjs` | CREATE | 180 LOC | Performance metrics |
| `src/agents/correctness-probe.mjs` | CREATE | 190 LOC | Correctness validation |
| `src/agents/structure-probe.mjs` | CREATE | 170 LOC | RDF structure validation |
| `src/agents/completeness-probe.mjs` | CREATE | 160 LOC | Graph completeness |
| `src/agents/consistency-probe.mjs` | CREATE | 180 LOC | Semantic consistency |
| `src/agents/compliance-probe.mjs` | CREATE | 175 LOC | Standards compliance |
| `src/agents/coverage-probe.mjs` | CREATE | 165 LOC | Code/test coverage |
| `src/agents/mutation-probe.mjs` | CREATE | 185 LOC | Mutation testing |
| `src/agents/integration-probe.mjs` | CREATE | 175 LOC | Integration validation |
| `src/agents/index.mjs` | MODIFY | +100 LOC | Register all agents |

#### Agent Implementation Pattern

Each agent follows the SPARC-verified pattern:

```javascript
export class SecurityProbe {
  static domain = 'security';
  static name = 'Security Probe';

  async execute(observation, config = {}) {
    const startTime = performance.now();
    try {
      // 1. Validate input
      const validated = ObservationSchema.parse(observation);

      // 2. Run validation logic (domain-specific)
      const assertions = await this._validate(validated);

      // 3. Calculate score
      const passed = assertions.filter(a => a.status === 'pass').length;
      const score = assertions.length > 0 ? passed / assertions.length : 1.0;

      // 4. Return result
      return {
        probe_id: crypto.randomUUID(),
        domain: SecurityProbe.domain,
        status: score >= 0.8 ? 'pass' : score >= 0.5 ? 'warning' : 'fail',
        score,
        assertions,
        duration_ms: performance.now() - startTime,
        timestamp_ns: BigInt(Date.now()) * 1_000_000n,
        metadata: {}
      };
    } catch (error) {
      return { domain: SecurityProbe.domain, status: 'fail', score: 0.0, error: error.message };
    }
  }
}
```

#### Acceptance Criteria

- [ ] Each agent produces valid ProbeResult objects
- [ ] All 10 agents registered in AgentRegistry
- [ ] Agents are parallelizable (no shared state)
- [ ] Each agent has 5+ assertions per domain
- [ ] `timeout 30s npm test -- test/agents/*.test.mjs` passes with 100+ tests

#### Success Proof

```bash
# Verify agent count
node -e "
import { createAgentRegistry } from './src/agents/index.mjs';
const registry = createAgentRegistry();
console.log('Agents registered:', registry.count());
"
# Expected: Agents registered: 10

# Verify each agent produces output
for agent in security performance correctness structure completeness consistency compliance coverage mutation integration; do
  node -e "
  import { create${agent^}Probe } from './src/agents/${agent}-probe.mjs';
  const probe = create${agent^}Probe();
  const result = await probe.execute({ /* test observation */ });
  console.log('${agent}:', result.status);
  "
done
```

---

### Milestone 4: Orchestrator Merge Logic + Receipts

**Assigned Agents**: Agent-4 (Orchestration Specialist)
**Effort**: 24 hours
**Dependencies**: Milestone 3 (all agents)

#### Files to Create/Modify

| File Path | Action | Size Est. | Description |
|-----------|--------|-----------|-------------|
| `src/orchestrator/index.mjs` | CREATE | 40 LOC | Orchestrator barrel |
| `src/orchestrator/probe-orchestrator.mjs` | CREATE | 250 LOC | Main orchestrator |
| `src/orchestrator/merge-engine.mjs` | CREATE | 180 LOC | Merge strategies |
| `src/orchestrator/conflict-resolver.mjs` | CREATE | 120 LOC | Conflict resolution |
| `src/orchestrator/aggregator.mjs` | CREATE | 100 LOC | Result aggregation |
| `src/receipts/index.mjs` | CREATE | 40 LOC | Receipt barrel |
| `src/receipts/receipt-builder.mjs` | CREATE | 200 LOC | BaseReceipt extension |
| `src/receipts/merkle-integrator.mjs` | CREATE | 180 LOC | Merkle chain integration |
| `src/receipts/verification.mjs` | CREATE | 150 LOC | Receipt verification |
| `src/orchestrator.mjs` | MODIFY | REPLACE | Delegate to new modules |

#### Integration Points

```javascript
// Required imports from existing packages
import { BaseReceipt } from '@unrdf/v6-core/receipts/base-receipt.mjs';
import { ReceiptChain } from '@unrdf/kgc-substrate';
```

#### Merge Strategies

```javascript
const MERGE_STRATEGIES = {
  consensus: (results) => /* All agree or reject */,
  max: (results) => /* Highest score wins */,
  min: (results) => /* Lowest score wins */,
  weighted_sum: (results, weights) => /* Weighted average */
};
```

#### Acceptance Criteria

- [ ] Orchestrator coordinates all 10 agents in parallel
- [ ] Merge engine supports 4 strategies
- [ ] Conflict resolver handles 3 resolution modes
- [ ] Receipt builder extends BaseReceipt correctly
- [ ] Merkle integrator chains receipts properly
- [ ] `timeout 20s npm test -- test/orchestrator.test.mjs` passes with 40+ tests

#### Success Proof

```bash
# Verify orchestrator
node -e "
import { createProbeOrchestrator } from './src/orchestrator/index.mjs';
import { createMemoryStorage } from './src/storage/index.mjs';
const orch = createProbeOrchestrator({ storage: createMemoryStorage() });
const result = await orch.scan({ universe_id: 'test' });
console.log('Scan status:', result.status);
console.log('Observations:', result.artifact.observations.length);
"
# Expected: Scan status: success, Observations: 10+

# Verify receipt generation
node -e "
import { createReceiptBuilder } from './src/receipts/index.mjs';
const builder = createReceiptBuilder();
const receipt = await builder.build({ domain: 'test', score: 0.95 });
console.log('Receipt ID:', receipt.receipt_id);
"
# Expected: Receipt ID: rcpt-...
```

---

### Milestone 5: CLI Commands

**Assigned Agents**: Agent-5 (CLI Specialist)
**Effort**: 16 hours
**Dependencies**: Milestone 4 (orchestrator + receipts)

#### Files to Create/Modify

| File Path | Action | Size Est. | Description |
|-----------|--------|-----------|-------------|
| `src/cli/index.mjs` | CREATE | 60 LOC | Command registration |
| `src/cli/run-probe.command.mjs` | CREATE | 150 LOC | `kgc probe run` |
| `src/cli/validate-observation.command.mjs` | CREATE | 120 LOC | `kgc probe validate` |
| `src/cli/merge-results.command.mjs` | CREATE | 130 LOC | `kgc probe merge` |
| `src/cli/export-receipt.command.mjs` | CREATE | 110 LOC | `kgc probe export` |

#### CLI Integration

```javascript
// Required imports from existing packages
import { registerCommand } from '@unrdf/kgc-cli';

export function registerProbeCommands(cliRegistry) {
  registerCommand(cliRegistry, {
    command: 'probe run [domain]',
    handler: runProbeHandler,
    options: [
      { name: '--observations-file', description: 'Path to JSONL file' },
      { name: '--output-format', choices: ['json', 'jsonl', 'receipt'] },
      { name: '--parallel', type: 'boolean', default: true }
    ]
  });
  // ... register other 3 commands
}
```

#### Acceptance Criteria

- [ ] All 4 CLI commands registered with @unrdf/kgc-cli
- [ ] Commands accept correct options and arguments
- [ ] Error handling provides user-friendly messages
- [ ] Exit codes are correct (0 = success, 1 = failure)
- [ ] `timeout 10s npm test -- test/cli.test.mjs` passes with 20+ tests

#### Success Proof

```bash
# Verify CLI registration
node -e "
import { registerProbeCommands } from './src/cli/index.mjs';
console.log('CLI commands registered');
"
# Expected: CLI commands registered

# Test run command
kgc probe run --help
# Expected: Usage information displayed

# Test validate command
echo '{"id":"test","agent":"test"...}' > /tmp/obs.json
kgc probe validate /tmp/obs.json
# Expected: Validation result
```

---

### Milestone 6: Tests (Unit, Integration, E2E)

**Assigned Agents**: Agent-6 (Testing Specialist)
**Effort**: 24 hours
**Dependencies**: Milestones 1-5 (all implementation)

#### Files to Create/Modify

| File Path | Action | Size Est. | Description |
|-----------|--------|-----------|-------------|
| `test/schemas.test.mjs` | CREATE | 200 LOC | Schema validation tests |
| `test/guards.test.mjs` | CREATE | 180 LOC | Guard validation tests |
| `test/storage.test.mjs` | CREATE | 220 LOC | Storage backend tests |
| `test/agents/*.test.mjs` | CREATE | 100 LOC x 10 | Per-agent tests |
| `test/orchestrator.test.mjs` | CREATE | 250 LOC | Orchestration tests |
| `test/receipts.test.mjs` | CREATE | 200 LOC | Receipt tests |
| `test/cli.test.mjs` | CREATE | 150 LOC | CLI command tests |
| `test/integration.test.mjs` | CREATE | 300 LOC | Full integration tests |
| `test/e2e.test.mjs` | CREATE | 250 LOC | End-to-end tests |
| `test/fixtures/*.json` | CREATE | 10 files | Test data fixtures |

#### Test Coverage Targets

| Domain | Target | Current |
|--------|--------|---------|
| Schemas | 100% | 0% |
| Guards | 95% | ~60% |
| Storage | 90% | ~30% |
| Agents | 85% | ~5% |
| Orchestrator | 90% | ~40% |
| Receipts | 95% | 0% |
| CLI | 85% | 0% |
| **Overall** | **90%** | **~15%** |

#### Acceptance Criteria

- [ ] 200+ unit tests passing
- [ ] 50+ integration tests passing
- [ ] 20+ E2E tests passing
- [ ] Code coverage >= 90%
- [ ] All 10 probe domains have dedicated tests
- [ ] `timeout 60s npm test` passes with 0 failures

#### Success Proof

```bash
# Run all tests with coverage
timeout 60s npm test -- --coverage
# Expected: 270+ tests PASS, Coverage: 90%+

# Check coverage report
npx c8 report --reporter=text
# Expected: All files >= 85% coverage
```

---

### Milestone 7: Code Quality + Analysis

**Assigned Agents**: Agent-7 (Code Quality)
**Effort**: 12 hours
**Dependencies**: Milestone 6 (tests)

#### Tasks

1. **Linting**: Run ESLint with 400+ rules
2. **Type Checking**: Verify JSDoc types
3. **Complexity Analysis**: Cyclomatic complexity < 10
4. **Duplication Detection**: < 3% duplicate code
5. **Security Audit**: npm audit, no critical vulnerabilities

#### Acceptance Criteria

- [ ] `timeout 10s npm run lint` passes with 0 errors
- [ ] `timeout 10s npm run typecheck` passes with 0 errors
- [ ] All functions have cyclomatic complexity < 10
- [ ] No files exceed 500 lines
- [ ] No security vulnerabilities (npm audit)

#### Success Proof

```bash
# Lint check
timeout 10s npm run lint
# Expected: 0 errors, 0 warnings

# Type check
timeout 10s npm run typecheck
# Expected: 0 type errors

# Security audit
npm audit --production
# Expected: 0 vulnerabilities
```

---

### Milestone 8: Production Validation

**Assigned Agents**: Agent-8 (Production Validator)
**Effort**: 12 hours
**Dependencies**: Milestone 7 (quality)

#### Tasks

1. **OTEL Validation**: Run comprehensive validation
2. **Performance Benchmarks**: Verify latency targets
3. **Memory Profiling**: Verify memory limits
4. **Error Rate Testing**: Verify < 1% FP/FN

#### Validation Targets

| Metric | Target | Measurement |
|--------|--------|-------------|
| Latency p50 | < 50ms | Benchmark 100 docs |
| Latency p99 | < 100ms | Benchmark 100 docs |
| Memory | < 500MB | Profile with heapdump |
| Error FP | < 1% | 100-doc corpus |
| Error FN | < 1% | 100-doc corpus |
| Reproducibility | 100% | 5 runs identical |

#### Acceptance Criteria

- [ ] OTEL validation score >= 80/100
- [ ] p99 latency < 100ms
- [ ] Memory usage < 500MB
- [ ] Error accuracy >= 99%
- [ ] 5 runs produce identical output

#### Success Proof

```bash
# OTEL validation
node validation/run-all.mjs comprehensive
grep "Score:" validation-output.log
# Expected: Score: 85/100 or higher

# Performance benchmark
npm run benchmark -- probe.scan.latency
# Expected: p99: 95ms (< 100ms)

# Reproducibility test
for i in 1 2 3 4 5; do
  kgc probe scan doc.kgcmd > report-$i.json
  sha256sum report-$i.json
done
# Expected: All 5 hashes identical
```

---

### Milestone 9: Code Review + Fixes

**Assigned Agents**: Agent-9 (Code Reviewer)
**Effort**: 8 hours
**Dependencies**: Milestone 8 (validation)

#### Review Checklist

1. **Architecture Conformance**: All modules follow spec
2. **Guard Policy Compliance**: 25+ forbidden patterns checked
3. **Error Handling**: All error paths covered
4. **Documentation**: All public APIs documented
5. **Integration Points**: All imports verified

#### Acceptance Criteria

- [ ] All 44 modules conform to architecture doc
- [ ] All 25 forbidden patterns have guards
- [ ] All public functions have JSDoc
- [ ] All imports resolve correctly
- [ ] No TODO comments remain

#### Success Proof

```bash
# Check for remaining TODOs
grep -r "TODO" src/ | wc -l
# Expected: 0

# Verify all imports
node -e "import('./src/index.mjs').then(() => console.log('All imports OK'))"
# Expected: All imports OK

# Check documentation coverage
npx documentation build src/index.mjs -f md
# Expected: All exports documented
```

---

### Milestone 10: Performance Benchmarking

**Assigned Agents**: Agent-10 (Performance Benchmarker)
**Effort**: 8 hours
**Dependencies**: Milestone 9 (review)

#### Benchmark Suite

| Benchmark | Target | Methodology |
|-----------|--------|-------------|
| scan() | < 100ms | 100 docs, measure p50/p99 |
| mergeShards() | < 50ms | 1000 observations |
| hashObservations() | < 10ms | 1000 observations |
| diffArtifacts() | < 20ms | 2 x 500 observations |
| verifyArtifact() | < 30ms | 500 observations |

#### Acceptance Criteria

- [ ] All benchmarks meet targets
- [ ] No memory leaks detected
- [ ] Throughput >= 10 docs/sec
- [ ] CPU utilization < 80% at peak

#### Success Proof

```bash
# Run full benchmark suite
npm run benchmark -- --all
# Expected: All targets met

# Throughput test
npm run benchmark -- probe.scan.throughput
# Expected: Throughput: 12 docs/sec (> 10)

# Memory leak test
node --expose-gc test/memory-leak.mjs
# Expected: No leak detected
```

---

## Critical Path Analysis

```
START
  |
  v
[M1: Core Infrastructure] -----> 16 hours (Agent-1, Agent-5)
  |
  v
[M2: RDF Storage Layer] -------> 20 hours (Agent-4)
  |
  v
[M3: 10 Agent Shards] ---------> 40 hours (Agent-4, parallel)
  |
  v
[M4: Orchestrator + Receipts] -> 24 hours (Agent-4)
  |
  v
[M5: CLI Commands] ------------> 16 hours (Agent-5)
  |                                |
  +----> [M6: Tests] -----------> 24 hours (Agent-6)
           |
           v
         [M7: Quality] ---------> 12 hours (Agent-7)
           |
           v
         [M8: Validation] ------> 12 hours (Agent-8)
           |
           v
         [M9: Review] ----------> 8 hours (Agent-9)
           |
           v
         [M10: Benchmarks] -----> 8 hours (Agent-10)
           |
           v
         END

TOTAL SEQUENTIAL: 180 hours
TOTAL WITH PARALLELIZATION: ~120 hours (Agent-1/5 parallel, M6-M10 overlap)
```

### Parallelization Opportunities

| Milestone Pair | Can Parallelize? | Notes |
|----------------|------------------|-------|
| M1 + early M5 prep | YES | CLI scaffolding while schemas built |
| M3 (10 agents) | YES | Each agent independent |
| M6 + M7 | PARTIAL | Quality runs while tests complete |
| M8 + M9 + M10 | PARTIAL | Some overlap possible |

### Minimum Time to Completion

With 10 parallel agents and optimal scheduling: **~80 hours wall-clock time**

---

## File Counts Summary

| Category | New Files | Modified Files | Total LOC |
|----------|-----------|----------------|-----------|
| Schemas | 6 | 0 | 390 |
| Utils | 4 | 0 | 400 |
| Storage | 4 | 1 | 580 |
| Agents | 10 | 1 | 1,880 |
| Orchestrator | 5 | 1 | 830 |
| Receipts | 4 | 0 | 570 |
| CLI | 5 | 0 | 570 |
| Tests | 25 | 0 | 3,000 |
| **TOTAL** | **63** | **4** | **~8,220** |

---

## Verification Commands

```bash
# Full package verification (run after all milestones)
timeout 5s npm run lint && \
timeout 5s npm run typecheck && \
timeout 60s npm test && \
timeout 30s npm run benchmark && \
echo "ALL VERIFICATION PASSED"
```

---

## Next Steps

1. **Immediate**: Begin Milestone 1 (Core Infrastructure)
2. **Parallel Start**: Agent-1 on schemas, Agent-5 on CLI prep
3. **Checkpoint**: After M1, verify all schemas compile before proceeding
4. **Big Bang**: Milestone 3 executes all 10 agents in parallel

---

**Document Status**: COMPLETE
**Ready for Implementation**: YES
**Methodology**: SPARC + Big Bang 80/20
