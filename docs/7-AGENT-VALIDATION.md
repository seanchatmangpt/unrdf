# 7-Agent Validation System for UNRDF

## Overview

Replace hook-based feedback with 7 specialized agents using ggen's A2A protocol and TPS quality gates. Each agent validates a specific quality dimension with autonomous fault tolerance and Byzantine consensus.

## Architecture

```
┌─────────────────────────────────────────────────────────────────┐
│                    7-Agent Validation System                   │
├─────────────────────────────────────────────────────────────────┤
│                                                                  │
│  ┌──────────────┐  ┌──────────────┐  ┌──────────────┐       │
│  │ Agent 1      │  │ Agent 2      │  │ Agent 3      │       │
│  │ Compiler     │  │ Test         │  │ Lint         │       │
│  │ Gate         │  │ Gate         │  │ Gate         │       │
│  └──────────────┘  └──────────────┘  └──────────────┘       │
│                                                                  │
│  ┌──────────────┐  ┌──────────────┐  ┌──────────────┐       │
│  │ Agent 4      │  │ Agent 5      │  │ Agent 6      │       │
│  │ SHACL        │  │ OTEL         │  │ Security     │       │
│  │ Gate         │  │ Gate         │  │ Gate         │       │
│  └──────────────┘  └──────────────┘  └──────────────┘       │
│                                                                  │
│  ┌──────────────┐                                               │
│  │ Agent 7      │                                               │
│  │ Performance │                                               │
│  │ Gate         │                                               │
│  └──────────────┘                                               │
│                                                                  │
│  ┌──────────────────────────────────────────────────────────┐   │
│  │           Consensus Layer (PBFT)                        │   │
│  │   5-of-7 quorum for validation decisions                  │   │
│  └──────────────────────────────────────────────────────────┘   │
│                                                                  │
│  ┌──────────────────────────────────────────────────────────┐   │
│  │           Supervisor Tree                               │   │
│  │   Crash detection, auto-restart, graceful shutdown      │   │
│  └──────────────────────────────────────────────────────────┘   │
└─────────────────────────────────────────────────────────────────┘
```

## Agent Specifications

### Agent 1: Compiler Gate Agent

**Purpose**: Validates code compiles without errors
**Quality Gate**: `ggen-jidoka::CompilerGate`
**Validation**:

```rust
pub struct CompilerGateAgent {
    agent: Agent,
    gate: CompilerGate,
}

impl CompilerGateAgent {
    pub async fn validate(&self, package: &str) -> GateResult {
        self.gate.check(package).await
            .check_compilation()
            .check_no_warnings()
            .execute()
    }
}
```

**Success Criteria**:

- Exit code 0 from compilation
- Zero compiler warnings
- Binary artifacts generated

**Failure Handling**:

- Retry with fix suggestions
- Mark package as "compilation-failed"
- Escalate to Supervisor after 3 attempts

---

### Agent 2: Test Gate Agent

**Purpose**: Validates all tests pass (Chicago TDD)
**Quality Gate**: `ggen-jidoka::TestGate`
**Validation**:

```rust
pub struct TestGateAgent {
    agent: Agent,
    gate: TestGate,
}

impl TestGateAgent {
    pub async fn validate(&self, package: &str) -> GateResult {
        self.gate.check(package).await
            .check_test_coverage(0.80) // 80% minimum
            .check_no_skipped_tests()
            .check_determinism()
            .execute()
    }
}
```

**Success Criteria**:

- 100% tests passing
- ≥80% code coverage (line/branch/function)
- Zero skipped tests
- Deterministic (run 10x, same result)

**Failure Handling**:

- Identify failing tests by name
- Generate test report with stack traces
- Mark package as "test-failed"

---

### Agent 3: Lint Gate Agent

**Purpose**: Validates linting passes with zero errors
**Quality Gate**: `ggen-jidoka::LintGate`
**Validation**:

```rust
pub struct LintGateAgent {
    agent: Agent,
    gate: LintGate,
}

impl LintGateAgent {
    pub async fn validate(&self, package: &str) -> GateResult {
        self.gate.check(package).await
            .check_zero_errors()
            .check_max_warnings(0)
            .execute()
    }
}
```

**Success Criteria**:

- Zero lint errors
- Zero warnings (max_warnings=0)
- All auto-fixes applied

**Failure Handling**:

- Run auto-fix if available
- Report violations by file:line
- Mark package as "lint-failed"

---

### Agent 4: SHACL Gate Agent

**Purpose**: Validates RDF ontology conformance
**Quality Gate**: `ggen-jidoka::SHACLGate`
**Validation**:

```rust
pub struct ShaclGateAgent {
    agent: Agent,
    gate: ShaclGate,
}

impl ShaclGateAgent {
    pub async fn validate(&self, ontology: &str) -> GateResult {
        self.gate.check(ontology).await
            .check_shape_conformance()
            .check_constraint_violations()
            .check_inference_consistency()
            .execute()
    }
}
```

**Success Criteria**:

- Zero SHACL constraint violations
- All shapes conform to schema
- Inference results consistent

**Failure Handling**:

- Report violating triples
- Show expected vs actual shapes
- Mark ontology as "shacl-failed"

---

### Agent 5: OTEL Gate Agent

**Purpose**: Validates OpenTelemetry spans exist
**Quality Gate**: Custom OTEL validation
**Validation**:

```rust
pub struct OtelGateAgent {
    agent: Agent,
    tracer: Tracer,
}

impl OtelGateAgent {
    pub async fn validate(&self, operation: &str) -> GateResult {
        let span = self.tracer.get_span(operation).await?;

        GateResult::success()
            .check(span.name == operation, "Span name mismatch")
            .check(span.status == "ok", "Span status not ok")
            .check(!span.attributes.is_empty(), "No attributes")
            .check(span.duration_us > 0, "Zero duration")
            .execute()
    }
}
```

**Success Criteria**:

- Span exists in Jaeger (http://localhost:16686)
- Span name matches operation
- Status is "ok" (not error, not omitted)
- Attributes contain actual values (not null/empty)
- Duration ≥ 0 (microseconds)

**Failure Handling**:

- Report missing span details
- Show Jaeger query results
- Mark operation as "otel-missing"

---

### Agent 6: Security Gate Agent

**Purpose**: Validates security policies
**Quality Gate**: `ggen-domain::SecurityScanner`
**Validation**:

```rust
pub struct SecurityGateAgent {
    agent: Agent,
    scanner: SecurityScanner,
}

impl SecurityGateAgent {
    pub async fn validate(&self, package: &str) -> GateResult {
        self.scanner.scan(package).await
            .check_no_secrets()
            .check_no_hardcoded_credentials()
            .check_sql_injection_safe()
            .check_xss_vectors()
            .execute()
    }
}
```

**Success Criteria**:

- Zero hardcoded secrets/API keys
- Parameterized queries only (no SQL injection)
- Input validation on all system boundaries
- No XSS vectors in templates

**Failure Handling**:

- Report security violations by file:line
- Suggest secure alternatives
- Mark package as "security-failed"

---

### Agent 7: Performance Gate Agent

**Purpose**: Validates performance benchmarks
**Quality Gate**: Custom performance validation
**Validation**:

```rust
pub struct PerformanceGateAgent {
    agent: Agent,
    benchmarks: BenchmarkSuite,
}

impl PerformanceGateAgent {
    pub async fn validate(&self, package: &str) -> GateResult {
        self.benchmarks.run(package).await
            .check_latency_p99_ms(100) // P99 < 100ms
            .check_throughput_qps(1000) // ≥1000 QPS
            .check_memory_mb(512) // <512MB
            .execute()
    }
}
```

**Success Criteria**:

- P99 latency < 100ms
- Throughput ≥ 1000 QPS
- Memory < 512MB
- No memory leaks (steady state after 1000 iterations)

**Failure Handling**:

- Generate flame graph for slow operations
- Report memory allocation sites
- Mark package as "performance-failed"

---

## Consensus Protocol (PBFT)

### Quorum Rules

- **5-of-7 quorum**: Requires 5 agents to agree
- **Byzantine fault tolerance**: Tolerates up to 2 malicious agents
- **Voting phases**: Pre-prepare → Prepare → Commit

### Vote Collection

```rust
pub struct ConsensusLayer {
    agents: Vec<Agent>,
    quorum: usize, // 5
}

impl ConsensusLayer {
    pub async fn validate_package(&self, package: &str) -> ConsensusResult {
        // Phase 1: Pre-prepare (send validation request)
        let votes = self.collect_votes(package).await?;

        // Phase 2: Prepare (wait for 5-of-7)
        if votes.len() < self.quorum {
            return Err(ConsensusError::NoQuorum);
        }

        // Phase 3: Commit (finalize decision)
        let decision = self.compute_decision(votes)?;
        self.broadcast_decision(decision).await?;

        Ok(decision)
    }
}
```

### Decision Matrix

| Agent       | Vote  | Weight | Reason                        |
| ----------- | ----- | ------ | ----------------------------- |
| Compiler    | ✅/❌ | 1      | Blocking (must compile)       |
| Test        | ✅/❌ | 1      | Blocking (must pass)          |
| Lint        | ✅/❌ | 1      | Blocking (must lint)          |
| SHACL       | ✅/❌ | 1      | Conditional (ontologies only) |
| OTEL        | ✅/❌ | 1      | Conditional (operations only) |
| Security    | ✅/❌ | 1      | Blocking (must be secure)     |
| Performance | ✅/❌ | 0.5    | Non-blocking (warning only)   |

---

## Supervisor Tree

### Root Supervisor

```rust
pub struct ValidationSupervisor {
    agents: HashMap<AgentId, Agent>,
    consensus: ConsensusLayer,
    state: SupervisorState,
}

impl ValidationSupervisor {
    pub async fn start(&mut self) -> Result<()> {
        self.state = SupervisorState::Supervising;

        // Spawn all 7 agents
        for agent in self.create_agents() {
            self.spawn_agent(agent).await?;
        }

        // Start health monitoring loop
        tokio::spawn(self.health_check_loop());

        Ok(())
    }

    async fn health_check_loop(&self) {
        loop {
            tokio::time::sleep(Duration::from_secs(5)).await;

            // Check each agent's health
            for agent_id in self.agents.keys() {
                if let Some(agent) = self.get_agent(agent_id) {
                    if !agent.heartbeat().await {
                        self.handle_crash(agent_id, CrashReason::NoHeartbeat).await;
                    }
                }
            }
        }
    }
}
```

### Crash Recovery

- **Exponential backoff**: 100ms → 200ms → 400ms
- **Max restarts**: 3 attempts per agent
- **Dead letter queue**: Failed validations retried later
- **Graceful shutdown**: 5s timeout to finish in-flight work

---

## OpenTelemetry Integration

### Required Spans

All agents emit OTEL spans for observability:

```rust
// Agent validation span
{
    "name": "agent.validate",
    "service": "validation-system",
    "attributes": {
        "agent.id": "compiler-gate",
        "package": "@unrdf/core",
        "gate.type": "compiler",
        "validation.result": "pass" | "fail",
        "duration_ms": 123
    },
    "status": "ok"
}
```

### Consensus Spans

```rust
{
    "name": "consensus.reach_quorum",
    "service": "validation-system",
    "attributes": {
        "quorum.required": 5,
        "votes.collected": 6,
        "consensus.result": "commit",
        "decision": "approved"
    },
    "status": "ok"
}
```

---

## MCP Integration

### Available Tools

```bash
# Start MCP server
cd /Users/sac/ggen
cargo run --bin ggen mcp start-server --transport stdio

# Available tools:
ggen_generate          # Run μ₁-μ₅ code generation
ggen_validate          # Parse Turtle for syntax
ggen_sync              # Full sync with dry-run
ggen_validate_pipeline # Run 6 quality gates
ggen_query_ontology    # SPARQL SELECT queries
```

### Usage in Validation

```bash
# Validate package via MCP
ggen mcp call ggen_validate_pipeline \
  --arg package="@unrdf/core" \
  --arg gates="compiler,test,lint,shacl,otel"

# Query ontology for violations
ggen mcp call ggen_query_ontology \
  --arg query="SELECT ?violation WHERE { ?violation a :ShaclViolation }"
```

---

## Usage Example

### Validate a Package

```bash
# 1. Create validation request
cat > validation-request.ttl <<EOF
@prefix val: <http://example.org/validation#>.

val:ValidateRequest
    val:package "@unrdf/core" ;
    val:gates val:compiler, val:test, val:lint, val:shacl, val:otel, val:security, val:performance ;
    val:quorum 5 .
EOF

# 2. Run validation
cd /Users/sac/ggen
cargo run --bin validation-system -- validate validation-request.ttl

# 3. Check results
# Results stored in validation-results.ttl
# OTEL spans in Jaeger at http://localhost:16686
```

### Example Output

```
✅ Agent 1 (Compiler): PASS - 0 errors, 0 warnings
✅ Agent 2 (Test): PASS - 699/699 tests, 87% coverage
✅ Agent 3 (Lint): PASS - 0 errors, 0 warnings
✅ Agent 4 (SHACL): PASS - 0 violations
✅ Agent 5 (OTEL): PASS - 23 spans found
✅ Agent 6 (Security): PASS - 0 vulnerabilities
⚠️  Agent 7 (Performance): WARNING - P99=115ms (threshold 100ms)

Consensus: 6/7 agents voted APPROVE (quorum: 5/7)
Decision: APPROVED with performance warning
```

---

## Implementation Steps

### Phase 1: Create Agent Definitions

```bash
cd /Users/sac/ggen
mkdir -p examples/7-agent-validation
cd examples/7-agent-validation

# Initialize ggen project
cargo init --name validation-system
```

### Phase 2: Define Agent Ontologies

```bash
# Create ontology for 7 agents
cat > schema/agents.ttl <<EOF
@prefix agent: <http://example.org/agents#>.

agent:CompilerGateAgent a agent:Agent ;
    agent:has_gate agent:CompilerGate ;
    agent:validates agent:Compilation .

agent:TestGateAgent a agent:Agent ;
    agent:has_gate agent:TestGate ;
    agent:validates agent:TestCoverage .

# ... define all 7 agents
EOF
```

### Phase 3: Generate Agent Code

```bash
# Generate from ontology
cargo run --bin ggen sync --schema schema/agents.ttl --templates templates/agents/

# 7 agent Rust files generated:
# - compiler_gate_agent.rs
# - test_gate_agent.rs
# - lint_gate_agent.rs
# - shacl_gate_agent.rs
# - otel_gate_agent.rs
# - security_gate_agent.rs
# - performance_gate_agent.rs
```

### Phase 4: Implement Consensus Layer

```rust
// consensus_layer.rs
use ggen_consensus::{PbftConsensus, ConsensusProtocol};

pub struct ValidationConsensus {
    pbft: PbftConsensus<7>, // 7 agents
}

impl ValidationConsensus {
    pub fn new() -> Self {
        Self {
            pbft: PbftConsensus::new(5), // 5-of-7 quorum
        }
    }

    pub async fn validate(&self, package: &str) -> ConsensusResult {
        // Collect votes from 7 agents
        let votes = self.collect_agent_votes(package).await?;

        // Run PBFT consensus
        let result = self.pbft.propose(votes).await?;
        self.pbft.commit(result).await?;

        Ok(result)
    }
}
```

### Phase 5: Integrate with UNRDF

```bash
# In UNRDF repository
cd /Users/sac/chatmangpt/unrdf

# Add validation-system dependency
cargo add --path /Users/sac/ggen/examples/7-agent-validation

# Run validation before commit
cargo run --bin validation-system -- validate packages/core

# Commit only if consensus reached
git commit -m "feat: add feature X (validated by 7-agent system)"
```

---

## Testing

### Unit Tests

```bash
cd /Users/sac/ggen/examples/7-agent-validation

# Test individual agents
cargo test --test agent_compiler
cargo test --test agent_test
cargo test --test agent_lint

# Test consensus
cargo test --test consensus_5_of_7
cargo test --test consensus_byzantine

# Test supervisor
cargo test --test supervisor_crash_recovery
cargo test --test supervisor_health_check
```

### Integration Tests

```bash
# Test full validation pipeline
cargo test --test validation_pipeline_e2e

# Test with real UNRDF packages
cargo test --test validate_unrdf_core
cargo test --test validate_unrdf_hooks
```

---

## Rollout Plan

### Week 1: Implement Core Agents

- [ ] Agent 1: Compiler Gate
- [ ] Agent 2: Test Gate
- [ ] Agent 3: Lint Gate

### Week 2: Implement Advanced Agents

- [ ] Agent 4: SHACL Gate
- [ ] Agent 5: OTEL Gate
- [ ] Agent 6: Security Gate

### Week 3: Performance & Consensus

- [ ] Agent 7: Performance Gate
- [ ] Consensus layer (PBFT)
- [ ] Supervisor tree

### Week 4: Integration & Testing

- [ ] MCP integration
- [ ] OpenTelemetry tracing
- [ ] E2E testing

### Week 5: UNRDF Integration

- [ ] Replace pre-commit hooks
- [ ] Update CI/CD pipeline
- [ ] Documentation

---

## Success Metrics

| Metric                  | Target           | Measurement                       |
| ----------------------- | ---------------- | --------------------------------- |
| **Validation Time**     | <30s per package | Time from request to decision     |
| **False Positive Rate** | <5%              | Valid code incorrectly rejected   |
| **False Negative Rate** | <1%              | Invalid code incorrectly approved |
| **Consensus Agreement** | ≥95%             | Agents agree on decisions         |
| **Crash Recovery**      | 100%             | All agent crashes recovered       |
| **OTEL Span Coverage**  | 100%             | All validations emit spans        |

---

## References

- **ggen Documentation**: `/Users/sac/ggen/CLAUDE.md`
- **A2A Protocol**: `/Users/sac/ggen/examples/a2a-agent-lifecycle/`
- **TPS Quality Gates**: `/Users/sac/ggen/crates/ggen-jidoka/`
- **Consensus**: `/Users/sac/ggen/crates/ggen-consensus/`
- **MCP Tools**: `/Users/sac/ggen/crates/ggen-a2a-mcp/`

---

**Next Steps:**

1. Create example project in `/Users/sac/ggen/examples/7-agent-validation/`
2. Define agent ontologies in RDF/Turtle
3. Generate agent code using ggen sync
4. Implement consensus layer with PBFT
5. Integrate with UNRDF pre-commit hooks
6. Deploy and monitor with OpenTelemetry
