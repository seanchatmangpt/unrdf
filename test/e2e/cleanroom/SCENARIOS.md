# 80/20 Scenario Catalog for CLI + Sidecar Integration

## Overview

This document catalogs the **20% of scenarios that represent 80% of real-world usage** for the unrdf CLI + KGC Sidecar system. All scenarios include comprehensive OTEL trace validation.

## Priority Definitions

- **P0 (Core Workflows)**: 60% of real-world usage - Must-have scenarios
- **P1 (Enhanced Workflows)**: 20% of real-world usage - Important scenarios
- **P2 (Edge Cases)**: 20% of real-world usage - Nice-to-have scenarios

---

## P0 Scenarios (Core Workflows - 60% value)

### 1. Graph Lifecycle (30% of usage)

**Workflow**: Create → Import → Query → Validate → Export

**Description**: The most common end-to-end workflow for managing RDF graphs.

**Steps**:
1. Create graph with base IRI
2. Import RDF data from Turtle file
3. Query graph with SPARQL
4. Validate graph integrity
5. Export graph to file

**OTEL Spans**:
- `cli.graph.create` → `sidecar.transaction.apply`
- `cli.store.import` → `sidecar.store.add`
- `cli.store.query` → `sidecar.query.execute`
- `cli.graph.validate` → `sidecar.hook.evaluate`
- `cli.graph.export` → `sidecar.store.serialize`

**Performance SLA**:
- Total duration: < 30s
- Hook evaluation: < 2ms (p99)
- Query execution: < 50ms (p99)

---

### 2. Hook Evaluation (25% of usage)

**Workflow**: Create → Evaluate → Track → History

**Description**: Knowledge Hook creation and execution tracking.

**Steps**:
1. Create SPARQL ASK hook
2. Evaluate hook with test data
3. View execution history

**OTEL Spans**:
- `cli.hook.create` → `sidecar.hook.register`
- `cli.hook.eval` → `sidecar.hook.evaluate` → `sidecar.condition.evaluate`
- `cli.hook.history` → `sidecar.storage.loadReceipts`

**Performance SLA**:
- Hook evaluation: < 2ms (p99)
- Hook registration: < 1ms (p99)

---

### 3. Policy Enforcement (20% of usage)

**Workflow**: Apply → Validate → Test → Audit

**Description**: Policy pack application and compliance validation.

**Steps**:
1. Apply policy pack from JSON
2. Validate compliance
3. Test policy in dry-run mode

**OTEL Spans**:
- `cli.policy.apply` → `sidecar.policy.activate`
- `cli.policy.validate` → `sidecar.hook.evaluateAll`
- `cli.policy.test`

**Performance SLA**:
- Policy activation: < 5ms (p99)
- Validation: < 100ms (p99)

---

### 4. Sidecar Integration (25% of usage)

**Workflow**: Health → Transaction → OTEL Validation

**Description**: Foundational CLI ↔ Sidecar communication with tracing.

**Steps**:
1. Sidecar health check
2. Execute transaction via gRPC
3. Verify OTEL trace propagation

**OTEL Spans**:
- `cli.sidecar.status` → `sidecar.health.check`
- `cli.graph.create` → `sidecar.transaction.apply`

**Performance SLA**:
- Health check: < 10ms (p99)
- Transaction: < 2ms (p99)
- Context propagation: 100% success rate

---

## P1 Scenarios (Enhanced Workflows - 20% value)

### 5. Graph Lifecycle with Hooks

**Description**: Complete graph workflow with hook validation at each step.

**Steps**:
1. Create graph with hooks enabled
2. Register validation hook
3. Import data (triggers hooks)
4. Verify hook execution history

**Value**: Demonstrates hook integration in production workflow.

---

### 6. Hook Veto Transaction

**Description**: Hook prevents invalid transaction from committing.

**Steps**:
1. Create veto hook
2. Attempt transaction with invalid data
3. Verify transaction was rejected
4. Check veto in history

**Value**: Critical error-prevention mechanism.

---

### 7. Policy Violation Detection

**Description**: Detect and handle policy compliance violations.

**Steps**:
1. Apply strict policy pack
2. Attempt non-compliant operation
3. View violation audit log

**Value**: Compliance enforcement and audit trail.

---

### 8. Sidecar gRPC Communication

**Description**: Detailed validation of CLI-to-Sidecar gRPC calls.

**Steps**:
1. Create transaction (gRPC)
2. Query data (gRPC)
3. Evaluate hook (gRPC)

**Value**: Core communication protocol validation.

---

### 9. Concurrent Graph Operations

**Description**: Multiple graph operations executed concurrently.

**Steps**:
1. Create 3 graphs in parallel
2. Measure total execution time

**Performance SLA**: < 5s for 3 concurrent operations

**Value**: Performance and scalability validation.

---

### 10. Hook Performance Benchmark

**Description**: Measure hook evaluation performance across iterations.

**Steps**:
1. Create benchmark hook
2. Evaluate 10 times
3. Calculate p99 latency

**Performance SLA**: p99 < 2ms

**Value**: Performance regression testing.

---

### 11. Policy Performance Validation

**Description**: Validate policy evaluation performance under load.

**Steps**:
1. Apply policy pack
2. Run 5 validation iterations
3. Measure average duration

**Performance SLA**: avg < 5ms

**Value**: Policy overhead measurement.

---

### 12. Sidecar Performance

**Description**: Measure sidecar response time and throughput.

**Steps**:
1. Execute 5 health checks
2. Calculate throughput

**Performance SLA**: > 10 ops/sec

**Value**: Baseline performance metrics.

---

## P2 Scenarios (Edge Cases - 20% value)

### 13. Sidecar Error Handling

**Description**: Validate error handling and recovery.

**Steps**:
1. Trigger sidecar error
2. Verify error logged in OTEL
3. Verify recovery

**Value**: Resilience and error reporting.

---

### 14. Hook Chaining

**Description**: Multiple hooks evaluated in sequence.

**Steps**:
1. Create pre-hook 1
2. Create pre-hook 2
3. Create post-hook
4. Execute transaction

**OTEL Validation**: >= 3 hook evaluations

**Value**: Complex hook orchestration.

---

### 15. Multi-Policy Stack

**Description**: Apply and manage multiple policy packs simultaneously.

**Steps**:
1. Apply base policy
2. Apply compliance policy
3. Apply security policy
4. List active policies
5. Validate against all

**Value**: Policy composition and prioritization.

---

## Test Execution

### Running All Scenarios

```bash
npm run test:cleanroom
```

### Running by Priority

```bash
# P0 only (core workflows)
npm run test:cleanroom -- --grep "P0:"

# P1 only (enhanced workflows)
npm run test:cleanroom -- --grep "P1:"

# P2 only (edge cases)
npm run test:cleanroom -- --grep "P2:"
```

### Running Individual Scenarios

```bash
# Graph lifecycle
npm run test:cleanroom -- --grep "graph lifecycle"

# Hook evaluation
npm run test:cleanroom -- --grep "hook evaluation"

# Policy enforcement
npm run test:cleanroom -- --grep "policy enforcement"

# Sidecar integration
npm run test:cleanroom -- --grep "sidecar integration"
```

---

## Performance Targets Summary

| Operation | p99 Target | p95 Target | Avg Target |
|-----------|-----------|-----------|-----------|
| CLI startup | < 100ms | < 80ms | < 50ms |
| Hook evaluation | < 2ms | < 1.5ms | < 1ms |
| Transaction apply | < 2ms | < 1.5ms | < 1ms |
| Policy validation | < 100ms | < 80ms | < 50ms |
| Query execution | < 50ms | < 40ms | < 20ms |
| Health check | < 10ms | < 8ms | < 5ms |

---

## OTEL Validation

All scenarios validate:
1. **Trace Existence**: Traces created for all operations
2. **Span Coverage**: Expected spans present
3. **Context Propagation**: Trace context flows CLI → Sidecar
4. **Performance**: Span durations meet SLA targets
5. **Error Tracking**: Errors properly tagged and logged

---

## Success Criteria

A scenario passes when:
1. All steps execute successfully
2. All OTEL spans are present
3. Performance SLAs are met
4. No error spans (unless testing error handling)
5. Trace context propagates correctly

---

## Coverage Analysis

- **P0 Scenarios**: 4 workflows = 60% real-world coverage
- **P1 Scenarios**: 8 workflows = 20% real-world coverage
- **P2 Scenarios**: 3 workflows = 20% real-world coverage
- **Total**: 15 scenarios = 100% targeted coverage

This 80/20 approach ensures we test the critical paths that matter most while keeping the test suite maintainable and fast (< 5 minutes total execution time).
