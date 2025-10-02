# KGC Sidecar Testcontainer Validation - 80/20 Critical Scenarios

## 🎯 80/20 Ultrathink Analysis

**Principle**: Identify the **critical 20% of scenarios** that validate **80% of system functionality** in a cleanroom testcontainer environment.

---

## 📊 Critical Scenario Matrix

| Scenario | Coverage % | Components Validated | OTel Signals | Priority |
|----------|------------|----------------------|--------------|----------|
| **1. Transaction Lifecycle** | 35% | Transaction Manager, Hook Manager, Observability | Traces, Metrics, Logs | P0 |
| **2. Policy Pack Governance** | 25% | Policy Packs, Hook Orchestration, Validation | Metrics, Logs | P0 |
| **3. Effect Sandbox Security** | 15% | Sandbox Isolation, Effect Execution, Error Handling | Traces, Logs | P0 |
| **4. Lockchain Audit Trail** | 15% | Lockchain Writer, Git Integration, Cryptography | Logs | P1 |
| **5. Multi-Agent Resolution** | 10% | Resolution Layer, Agent Coordination, Conflict Resolution | Distributed Traces | P1 |

**Total Coverage**: 100% of critical paths
**Total Scenarios**: 5 (20% of potential scenarios = 80% coverage)

---

## 🔬 Scenario 1: Transaction Lifecycle + OTel Traces (35% Coverage)

### **Objective**
Validate end-to-end transaction processing with pre/post hooks and comprehensive OpenTelemetry instrumentation.

### **Test Flow**
```
1. CLI: Initialize KGC Sidecar with OTel Collector endpoint
2. CLI: Register pre-hook with ASK predicate
3. CLI: Register post-hook with THRESHOLD predicate
4. CLI: Execute transaction with RDF delta
5. Validate: Pre-hook fires and creates span in Jaeger
6. Validate: Transaction commits and creates parent span
7. Validate: Post-hook fires and creates child span
8. Validate: Metrics exported to OTel Collector
9. Validate: Logs include correlation IDs
10. CLI: Query transaction receipt
```

### **Success Criteria**
- ✅ Transaction completes in < 2ms (p99)
- ✅ All hooks fire with correct predicates
- ✅ Jaeger shows 3 spans (transaction + 2 hooks) with parent-child relationships
- ✅ OTel Collector receives `kgc_transactions_total` metric
- ✅ Logs include `transactionId` correlation ID
- ✅ Receipt contains cryptographic hashes (SHA3 + BLAKE3)

### **OTel Weaver Validation**
```bash
# Check Jaeger for transaction trace
curl http://localhost:16686/api/traces?service=kgc-sidecar&operation=kgc.transaction

# Verify span hierarchy
Expected:
  kgc.transaction (parent)
    ├─ kgc.hook (pre-hook child)
    └─ kgc.hook (post-hook child)

# Validate metrics in OTel Collector
curl http://localhost:4318/v1/metrics
Expected metrics:
  - kgc_transactions_total{success="true"} = 1
  - kgc_transaction_duration_ms (histogram)
  - kgc_hooks_executed_total = 2
```

---

## 🛡️ Scenario 2: Policy Pack Governance + Metrics (25% Coverage)

### **Objective**
Validate policy-driven governance with SHACL validation and metric collection.

### **Test Flow**
```
1. CLI: Create policy pack with SHACL shape (e.g., "Person must have name and email")
2. CLI: Activate policy pack
3. CLI: Attempt valid transaction (passes SHACL)
4. Validate: Transaction commits, policy hook fires
5. CLI: Attempt invalid transaction (violates SHACL)
6. Validate: Transaction vetoed, receipt shows veto reason
7. Validate: Metrics show policy compliance rate
8. CLI: Deactivate policy pack
9. CLI: Attempt invalid transaction again
10. Validate: Transaction commits (no policy enforcement)
```

### **Success Criteria**
- ✅ Policy pack loads with correct version
- ✅ SHACL validation executes in < 100ms
- ✅ Valid transactions pass, invalid transactions vetoed
- ✅ Metrics show `kgc_policy_compliance_rate = 50%` (1 pass, 1 veto)
- ✅ Veto receipt includes SHACL violation details
- ✅ Deactivated policy pack doesn't enforce

### **OTel Weaver Validation**
```bash
# Check metrics for policy compliance
curl http://localhost:4318/v1/metrics | grep kgc_policy

Expected:
  - kgc_policy_activations_total = 1
  - kgc_policy_violations_total = 1
  - kgc_shacl_validation_duration_ms (histogram)
```

---

## 🔒 Scenario 3: Effect Sandbox Security + Error Isolation (15% Coverage)

### **Objective**
Validate effect sandbox isolation with worker thread limits and error containment.

### **Test Flow**
```
1. CLI: Register hook with malicious effect (infinite loop)
2. CLI: Execute transaction
3. Validate: Effect times out after 30s (default)
4. Validate: Transaction continues, error isolated
5. Validate: Subsequent transactions unaffected
6. CLI: Register hook with memory bomb effect
7. CLI: Execute transaction
8. Validate: Effect terminates at memory limit (64MB)
9. Validate: Error logged, span recorded as failed
10. Validate: Sidecar remains stable
```

### **Success Criteria**
- ✅ Timeout enforced at 30s (configurable)
- ✅ Memory limit enforced at 64MB
- ✅ No crashes or hangs in sidecar process
- ✅ Error spans show `kgc.hook.success = false`
- ✅ Error logs include sandbox violation details
- ✅ Subsequent transactions process normally

### **OTel Weaver Validation**
```bash
# Check for error spans in Jaeger
curl http://localhost:16686/api/traces?service=kgc-sidecar&tags=error:true

Expected:
  - 2 error spans (timeout + memory)
  - Span status = ERROR
  - Exception recorded in span

# Validate error metrics
curl http://localhost:4318/v1/metrics | grep kgc_errors

Expected:
  - kgc_errors_total{type="SandboxTimeout"} = 1
  - kgc_errors_total{type="SandboxMemoryLimit"} = 1
```

---

## 📜 Scenario 4: Lockchain Audit Trail + Git Integration (15% Coverage)

### **Objective**
Validate cryptographic audit trail with Git-anchored receipts.

### **Test Flow**
```
1. CLI: Initialize Git repository for lockchain
2. CLI: Enable lockchain in sidecar config
3. CLI: Execute 10 transactions
4. Validate: Git notes created for each transaction
5. CLI: Query lockchain for transaction N=5
6. Validate: Merkle proof verifies receipt N=5
7. CLI: Tamper with receipt in Git
8. Validate: Verification fails with hash mismatch
9. CLI: Export lockchain audit report
10. Validate: Report includes all receipts with timestamps
```

### **Success Criteria**
- ✅ 10 Git notes created in `refs/notes/lockchain`
- ✅ Each note contains receipt with SHA3 + BLAKE3 hashes
- ✅ Merkle proof verifies in O(log n) = O(log 10) ≈ 4 hashes
- ✅ Tampered receipt detected with 100% success rate
- ✅ Audit report includes full chain with timestamps
- ✅ Git commits signed (if GPG configured)

### **OTel Weaver Validation**
```bash
# Check lockchain operations in logs
curl http://localhost:4318/v1/logs | grep lockchain

Expected logs:
  - "Lockchain: batch write (10 receipts)"
  - "Lockchain: Merkle root = <hash>"
  - "Lockchain: verification success"
  - "Lockchain: tampering detected"
```

---

## 🤝 Scenario 5: Multi-Agent Resolution + Distributed Traces (10% Coverage)

### **Objective**
Validate multi-agent conflict resolution with distributed tracing across agents.

### **Test Flow**
```
1. CLI: Register 3 agents (Agent A, B, C)
2. CLI: Create conflicting transactions (same resource, different values)
3. CLI: Submit Agent A transaction
4. CLI: Submit Agent B transaction (conflicts with A)
5. Validate: Resolution layer detects conflict
6. Validate: Conflict resolved via strategy (e.g., timestamp-based)
7. Validate: Distributed trace shows agent coordination
8. CLI: Query final state
9. Validate: Winner transaction committed
10. Validate: Loser transaction in conflict log
```

### **Success Criteria**
- ✅ Conflict detected with < 10ms latency
- ✅ Resolution strategy applied correctly
- ✅ Winner transaction committed
- ✅ Distributed trace shows spans across all 3 agents
- ✅ Conflict log includes resolution reasoning
- ✅ No data loss or corruption

### **OTel Weaver Validation**
```bash
# Check distributed trace in Jaeger
curl http://localhost:16686/api/traces?service=kgc-sidecar&operation=kgc.resolution

Expected trace:
  kgc.resolution (parent)
    ├─ kgc.agent[A].propose (child)
    ├─ kgc.agent[B].propose (child)
    ├─ kgc.conflict_detect (child)
    └─ kgc.resolution.apply (child)

# Validate resolution metrics
curl http://localhost:4318/v1/metrics | grep kgc_resolution

Expected:
  - kgc_conflicts_detected_total = 1
  - kgc_resolutions_applied_total = 1
  - kgc_resolution_duration_ms (histogram)
```

---

## 🧪 Testcontainer Architecture

### **Container Topology**

```
┌─────────────────────────────────────────────────────────────┐
│                     Testcontainer Network                    │
│  (Docker Bridge: testcontainers-kgc)                        │
└─────────────────────────────────────────────────────────────┘
                              │
        ┌─────────────────────┼─────────────────────┐
        │                     │                     │
┌───────▼────────┐  ┌─────────▼────────┐  ┌────────▼────────┐
│  KGC Sidecar   │  │ OTel Collector   │  │  Jaeger UI      │
│  (Node 20)     │  │  (OTLP receiver) │  │  (Trace UI)     │
│                │  │                  │  │                 │
│  Port: 3000    │  │  Port: 4318      │  │  Port: 16686    │
│                │  │       4317       │  │       6831      │
└────────────────┘  └──────────────────┘  └─────────────────┘
        │                     │                     │
        │                     │                     │
        └─────────────────────┴─────────────────────┘
                              │
                    ┌─────────▼─────────┐
                    │   Git Server      │
                    │   (Lockchain)     │
                    │                   │
                    │   Port: 22 (SSH)  │
                    └───────────────────┘
```

### **Container Images**

1. **KGC Sidecar**: `node:20-alpine` with sidecar source mounted
2. **OTel Collector**: `otel/opentelemetry-collector:latest`
3. **Jaeger**: `jaegertracing/all-in-one:latest`
4. **Git Server**: `gitea/gitea:latest-rootless`

### **Volume Mounts**

```yaml
kgc-sidecar:
  volumes:
    - ./src:/app/src:ro
    - ./test/e2e/fixtures:/app/fixtures:ro
    - ./test/e2e/config:/app/config:ro
    - kgc-data:/app/data

otel-collector:
  volumes:
    - ./test/e2e/otel-collector-config.yaml:/etc/otel-collector-config.yaml:ro

git-server:
  volumes:
    - git-data:/data
```

---

## 📋 CLI Test Harness

### **CLI Commands to Test**

```bash
# Scenario 1: Transaction Lifecycle
kgc-cli init --otel-endpoint=http://otel-collector:4318
kgc-cli hook register --file=fixtures/pre-hook-ask.json
kgc-cli hook register --file=fixtures/post-hook-threshold.json
kgc-cli transaction apply --delta=fixtures/delta-add-person.json
kgc-cli transaction receipt --id=<transaction-id>

# Scenario 2: Policy Pack
kgc-cli policy load --file=fixtures/person-policy-pack.json
kgc-cli policy activate --id=person-policy
kgc-cli transaction apply --delta=fixtures/delta-valid-person.json
kgc-cli transaction apply --delta=fixtures/delta-invalid-person.json
kgc-cli policy deactivate --id=person-policy

# Scenario 3: Effect Sandbox
kgc-cli hook register --file=fixtures/hook-infinite-loop.json
kgc-cli transaction apply --delta=fixtures/delta-trigger-hook.json
kgc-cli hook register --file=fixtures/hook-memory-bomb.json
kgc-cli transaction apply --delta=fixtures/delta-trigger-hook.json

# Scenario 4: Lockchain
kgc-cli lockchain init --git-repo=/app/data/lockchain.git
kgc-cli config set lockchain.enabled=true
for i in {1..10}; do
  kgc-cli transaction apply --delta=fixtures/delta-add-person-$i.json
done
kgc-cli lockchain verify --transaction-id=<tx-5>
kgc-cli lockchain export --output=/tmp/audit-trail.json

# Scenario 5: Multi-Agent
kgc-cli agent register --id=agent-a --priority=1
kgc-cli agent register --id=agent-b --priority=2
kgc-cli agent register --id=agent-c --priority=3
kgc-cli transaction apply --agent=agent-a --delta=fixtures/delta-conflict-1.json
kgc-cli transaction apply --agent=agent-b --delta=fixtures/delta-conflict-2.json
kgc-cli resolution status
```

---

## ✅ Validation Checklist

### **Pre-Validation**
- [ ] Testcontainers package installed (`npm install testcontainers`)
- [ ] Docker daemon running
- [ ] Sufficient resources (4 GB RAM, 10 GB disk)
- [ ] Ports available (3000, 4318, 4317, 16686, 6831, 22)

### **Post-Validation**
- [ ] All 5 scenarios pass
- [ ] Jaeger UI shows traces for all scenarios
- [ ] OTel Collector metrics endpoint responsive
- [ ] Git server has lockchain repository
- [ ] CLI commands execute without errors
- [ ] No memory leaks (< 5% growth over 100 transactions)
- [ ] No zombie processes or dangling containers

---

## 📈 Success Metrics

| Metric | Target | Measurement |
|--------|--------|-------------|
| **Scenario Pass Rate** | 100% (5/5) | All scenarios complete successfully |
| **OTel Trace Coverage** | ≥ 90% | Spans recorded for all major operations |
| **Transaction Latency** | p99 < 2ms | Measured via OTel histograms |
| **Error Isolation** | 100% | Sandbox errors don't crash sidecar |
| **Lockchain Integrity** | 100% | All receipts verifiable |
| **Multi-Agent Resolution** | < 50ms | Conflict resolution latency |
| **Memory Stability** | < 5% growth | Over 100 transactions |
| **Container Startup** | < 30s | All containers healthy |

---

## 🚀 Execution Plan

### **Phase 1: Infrastructure Setup** (2 hours)
1. Create `docker-compose.yml` for testcontainer topology
2. Create OTel Collector config
3. Create testcontainer orchestration script
4. Create fixture files (hooks, deltas, policies)

### **Phase 2: Scenario Implementation** (4 hours)
1. Implement Scenario 1 test (Transaction Lifecycle)
2. Implement Scenario 2 test (Policy Pack)
3. Implement Scenario 3 test (Effect Sandbox)
4. Implement Scenario 4 test (Lockchain)
5. Implement Scenario 5 test (Multi-Agent)

### **Phase 3: Validation & Debugging** (2 hours)
1. Run all scenarios in sequence
2. Validate OTel weaver integration
3. Debug failures and fix issues
4. Document results

**Total Effort**: 8 hours for 80% validation coverage

---

## 📚 References

- **Testcontainers**: https://node.testcontainers.org/
- **OpenTelemetry Collector**: https://opentelemetry.io/docs/collector/
- **Jaeger**: https://www.jaegertracing.io/docs/
- **KGC Sidecar DoD**: `/Users/sac/unrdf/KGC-SIDECAR-DEFINITION-OF-DONE.md`

---

**Next Step**: Implement testcontainer infrastructure and scenario tests.
