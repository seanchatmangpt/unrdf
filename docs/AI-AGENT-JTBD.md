# AI Agent Jobs-To-Be-Done (JTBD) Framework

**System Design Philosophy**: UNRDF v5 is fundamentally an **AI-first knowledge graph substrate**, NOT a human-centric tool. Every interface, command, and API is optimized for autonomous agent consumption and multi-agent coordination.

---

## Core Insight

**Traditional Knowledge Graphs**: Designed for humans to query and explore
**UNRDF v5**: Designed for AI agents to **autonomously build, validate, merge, and reason over** knowledge at scale

---

## Jobs-To-Be-Done Hierarchy

### Meta-Jobs (Why AI Agents Exist)

1. **Knowledge Synthesis** - Combine disparate data sources into unified knowledge
2. **Continuous Validation** - Ensure knowledge graph integrity in real-time
3. **Distributed Coordination** - Synchronize knowledge across agent networks
4. **Autonomous Reasoning** - Derive new knowledge from existing triples
5. **Event-Driven Adaptation** - Respond to knowledge graph changes instantly

---

## Primary Jobs-To-Be-Done

### 1. Knowledge Ingestion Agent
**Job**: When I encounter new data sources, I need to transform and integrate them into the knowledge graph with validation, so that the graph remains consistent and queryable.

**Sub-Jobs**:
- **J1.1**: Parse heterogeneous formats (CSV, JSON, XML, APIs) → RDF triples
- **J1.2**: Validate triples against SHACL shapes before ingestion
- **J1.3**: Deduplicate entities using SPARQL queries
- **J1.4**: Enrich triples with inferred predicates (rdfs:seeAlso, owl:sameAs)
- **J1.5**: Emit OTEL traces for ingestion pipeline observability

**CLI Commands Used**:
```bash
# Validate before ingestion
unrdf hook eval validation-rules.json --data incoming.ttl

# Create target graph
unrdf graph create integration-$(date +%s) --base-iri https://agent.internal/

# Import with transformation
unrdf convert incoming.csv output.ttl --format turtle

# Validate post-ingestion
unrdf graph validate integration-latest --shacl shapes.ttl
```

**Success Criteria**:
- 100% triples validated before commit
- <500ms p95 ingestion latency per 1K triples
- Zero duplicate entities (verified via SPARQL)
- OTEL spans show complete pipeline trace

---

### 2. Knowledge Validation Agent
**Job**: When knowledge graph is updated, I need to verify integrity constraints continuously, so that downstream agents consume only valid knowledge.

**Sub-Jobs**:
- **J2.1**: Execute SHACL validation on graph updates
- **J2.2**: Detect constraint violations (cardinality, datatypes, ranges)
- **J2.3**: Trigger remediation workflows on violations
- **J2.4**: Maintain validation audit log in separate graph
- **J2.5**: Emit metrics (violation count, severity distribution)

**CLI Commands Used**:
```bash
# Continuous validation loop
while true; do
  unrdf graph validate production --shacl constraints.ttl \
    --output violations.json --format json

  if [ $? -ne 0 ]; then
    unrdf hook eval remediation-policy.json --data violations.json
  fi

  sleep 30
done
```

**Success Criteria**:
- Violations detected within 30s of graph update
- 100% SHACL constraint coverage
- Remediation triggered automatically
- Audit trail queryable via SPARQL

---

### 3. Knowledge Synchronization Agent
**Job**: When distributed knowledge graphs diverge, I need to merge changes with conflict resolution, so that all agents have consistent worldview.

**Sub-Jobs**:
- **J3.1**: Detect graph deltas via SPARQL DIFF queries
- **J3.2**: Apply CRDTs (Conflict-Free Replicated Data Types) for merge
- **J3.3**: Resolve conflicts using owl:sameAs and provenance
- **J3.4**: Broadcast merged graph to agent network
- **J3.5**: Verify eventual consistency via quorum queries

**CLI Commands Used**:
```bash
# Export local graph state
unrdf graph export local --format ntriples > local-state.nt

# Fetch remote graph state
curl https://remote-agent/graph/export > remote-state.nt

# Compute delta (requires custom SPARQL)
unrdf query-file diff-query.rq local-state.nt remote-state.nt > delta.nt

# Apply delta with conflict resolution
unrdf graph update local --delta delta.nt --strategy crdt

# Verify consistency
unrdf query "SELECT (COUNT(*) AS ?count) WHERE { ?s ?p ?o }" \
  --compare-with https://remote-agent/query
```

**Success Criteria**:
- Merge completes in <5s for graphs up to 1M triples
- Zero data loss during conflict resolution
- Convergence time <60s across 10-node network
- Provenance preserved for all merged triples

---

### 4. Inference & Reasoning Agent
**Job**: When new triples are added, I need to derive implicit knowledge via OWL/RDFS reasoning, so that queries return complete results.

**Sub-Jobs**:
- **J4.1**: Apply RDFS entailment (rdfs:subClassOf, rdfs:subPropertyOf)
- **J4.2**: Execute OWL reasoning (transitivity, inverse properties)
- **J4.3**: Materialize inferred triples into reasoning graph
- **J4.4**: Index inferred triples for fast query access
- **J4.5**: Incrementally update materialization on graph changes

**CLI Commands Used**:
```bash
# Load base ontology
unrdf graph create reasoning-base --base-iri https://ontology.internal/

# Trigger RDFS reasoning
unrdf hook eval rdfs-entailment.json --data base-graph.ttl \
  --output inferred.ttl

# Merge inferred triples
unrdf graph update reasoning-base --add inferred.ttl

# Query with reasoning
unrdf query "SELECT ?x WHERE { ?x rdf:type :Person }" \
  --reasoning rdfs --graph reasoning-base
```

**Success Criteria**:
- Entailment completeness: 100% of valid inferences derived
- Incremental update latency: <100ms for 100 new triples
- Query performance: Reasoning overhead <20%
- Materialization storage: <2x original graph size

---

### 5. Event-Driven Orchestration Agent
**Job**: When knowledge graph events occur (add/remove/update), I need to trigger downstream agent workflows, so that the system adapts autonomously.

**Sub-Jobs**:
- **J5.1**: Subscribe to graph change notifications (webhooks, SPARQL update logs)
- **J5.2**: Filter events via SPARQL patterns (only relevant changes)
- **J5.3**: Dispatch events to agent task queues
- **J5.4**: Coordinate multi-agent workflows (validation → reasoning → sync)
- **J5.5**: Monitor workflow execution via OTEL distributed traces

**CLI Commands Used**:
```bash
# Monitor graph for changes (polling example)
unrdf context use production-graph

while read -r event; do
  # Event format: {"type": "add", "quads": [...]}
  echo "$event" | jq -r '.quads[]' > event-quads.nt

  # Trigger validation
  unrdf hook eval validation.json --data event-quads.nt

  # Trigger reasoning if validation passed
  if [ $? -eq 0 ]; then
    unrdf hook eval reasoning.json --data event-quads.nt
  fi
done < <(unrdf graph describe production --watch --format jsonstream)
```

**Success Criteria**:
- Event processing latency: <50ms p95
- Workflow orchestration: Support 100+ concurrent events
- Fault tolerance: Retry failed workflows with exponential backoff
- Observability: Complete distributed trace per workflow

---

### 6. Query Optimization Agent
**Job**: When executing complex SPARQL queries, I need to rewrite and optimize them, so that results return in <100ms even on large graphs.

**Sub-Jobs**:
- **J6.1**: Analyze SPARQL query patterns (selectivity, join order)
- **J6.2**: Rewrite queries using heuristics (push filters, use indexes)
- **J6.3**: Cache frequent query results with TTL
- **J6.4**: Materialize common query patterns as views
- **J6.5**: Monitor query performance and adapt strategies

**CLI Commands Used**:
```bash
# Analyze query execution plan
unrdf query "SELECT * WHERE { ?s :knows ?o . ?o :age ?age }" \
  --explain --graph social-network

# Rewrite query with optimization hints
unrdf query "SELECT * WHERE { ?s :knows ?o . FILTER(?age > 25) ?o :age ?age }" \
  --optimize --graph social-network

# Create materialized view for common pattern
unrdf query "CONSTRUCT { ?s :friend ?o } WHERE { ?s :knows ?o }" \
  --materialize friend-network

# Query from materialized view
unrdf query "SELECT * WHERE { ?s :friend ?o }" --graph friend-network
```

**Success Criteria**:
- Query latency reduction: >40% for complex JOINs
- Cache hit rate: >70% for frequent queries
- Materialization refresh: <10s for 100K triple views
- Adaptive optimization: Strategy selection based on query history

---

### 7. Provenance Tracking Agent
**Job**: When knowledge is derived or merged, I need to record provenance metadata, so that trust and lineage are verifiable.

**Sub-Jobs**:
- **J7.1**: Attach prov:wasDerivedFrom to inferred triples
- **J7.2**: Record agent identity and timestamp for all operations
- **J7.3**: Create provenance graph separate from knowledge graph
- **J7.4**: Enable temporal queries (graph state at time T)
- **J7.5**: Support SPARQL queries over provenance chains

**CLI Commands Used**:
```bash
# Create provenance graph
unrdf graph create provenance --base-iri https://prov.internal/

# Add provenance metadata during operation
unrdf graph update knowledge \
  --add new-triples.ttl \
  --provenance '{
    "agent": "ingestion-bot-7",
    "timestamp": "2025-12-06T20:00:00Z",
    "source": "https://external-api.example.com/data",
    "method": "automated-ingestion"
  }' \
  --provenance-graph provenance

# Query provenance chain
unrdf query "
  SELECT ?triple ?agent ?timestamp ?source
  WHERE {
    GRAPH <provenance> {
      ?triple prov:wasGeneratedBy ?activity .
      ?activity prov:wasAssociatedWith ?agent .
      ?activity prov:startedAtTime ?timestamp .
      ?activity prov:used ?source .
    }
  }
"
```

**Success Criteria**:
- 100% triples have provenance metadata
- Provenance graph size: <10% of knowledge graph
- Temporal queries: <500ms for 1M triple history
- Lineage traceability: Full chain from raw data to inference

---

### 8. Anomaly Detection Agent
**Job**: When graph patterns deviate from expected distributions, I need to flag anomalies, so that data quality issues are caught early.

**Sub-Jobs**:
- **J8.1**: Compute graph statistics (degree distribution, predicate frequency)
- **J8.2**: Compare current statistics to historical baselines
- **J8.3**: Detect outliers (e.g., entity with 10,000x more triples than median)
- **J8.4**: Classify anomaly types (schema drift, data corruption, attack)
- **J8.5**: Trigger alerts and remediation workflows

**CLI Commands Used**:
```bash
# Compute graph statistics
unrdf graph describe production --stats > current-stats.json

# Compare with baseline
jq -s '.[0].tripleCount / .[1].tripleCount' current-stats.json baseline-stats.json

# Detect outlier entities
unrdf query "
  SELECT ?entity (COUNT(?p) AS ?predicateCount)
  WHERE { ?entity ?p ?o }
  GROUP BY ?entity
  HAVING (COUNT(?p) > 1000)
  ORDER BY DESC(?predicateCount)
" --graph production

# Flag anomalies
if [ "$(wc -l < outliers.txt)" -gt 0 ]; then
  unrdf hook eval anomaly-detection.json --data outliers.txt
fi
```

**Success Criteria**:
- Anomaly detection latency: <10s for 1M triple graph
- False positive rate: <5%
- Detection coverage: Schema drift, cardinality violations, type errors
- Automated remediation: 80% of anomalies auto-fixed

---

### 9. Multi-Source Linking Agent
**Job**: When integrating multiple knowledge graphs, I need to identify equivalent entities, so that queries span all sources without duplication.

**Sub-Jobs**:
- **J9.1**: Compute entity fingerprints (hash of key predicates)
- **J9.2**: Execute blocking strategies (group by type, locality)
- **J9.3**: Apply similarity measures (Jaccard, Levenshtein, embedding distance)
- **J9.4**: Generate owl:sameAs links with confidence scores
- **J9.5**: Validate links via bidirectional queries

**CLI Commands Used**:
```bash
# Export entities from source A
unrdf query "CONSTRUCT { ?s ?p ?o } WHERE { ?s a :Person . ?s ?p ?o }" \
  --graph source-a > entities-a.ttl

# Export entities from source B
unrdf query "CONSTRUCT { ?s ?p ?o } WHERE { ?s a :Person . ?s ?p ?o }" \
  --graph source-b > entities-b.ttl

# Run linking algorithm (external tool integration)
entity-linker entities-a.ttl entities-b.ttl > links.ttl

# Add links to integration graph
unrdf graph create links --base-iri https://links.internal/
unrdf graph update links --add links.ttl

# Query across sources using links
unrdf query "
  SELECT ?person ?name ?email
  WHERE {
    {
      GRAPH <source-a> { ?person :name ?name }
    } UNION {
      GRAPH <source-b> { ?person :email ?email }
    }
    GRAPH <links> { ?person owl:sameAs ?personB }
  }
"
```

**Success Criteria**:
- Linking precision: >90% correct owl:sameAs assertions
- Linking recall: >80% of true duplicates identified
- Throughput: 10K entities/sec on 4-core CPU
- Scalability: Support 10M+ entities across 100+ sources

---

### 10. Schema Evolution Agent
**Job**: When ontology schemas change, I need to migrate existing data to new schema, so that graph remains queryable under new structure.

**Sub-Jobs**:
- **J10.1**: Detect schema changes (new classes, deprecated predicates)
- **J10.2**: Generate migration plan (SPARQL UPDATE queries)
- **J10.3**: Execute migration in batches with rollback capability
- **J10.4**: Validate migrated data against new schema
- **J10.5**: Update downstream agents with new schema definitions

**CLI Commands Used**:
```bash
# Backup current graph before migration
unrdf graph export production --format ntriples > backup-$(date +%s).nt

# Analyze schema diff
diff old-schema.ttl new-schema.ttl > schema-changes.diff

# Generate migration queries (custom tool)
schema-migrator schema-changes.diff > migration.rq

# Execute migration
unrdf query-file migration.rq production

# Validate migrated data
unrdf graph validate production --shacl new-schema-shapes.ttl

# If validation fails, rollback
if [ $? -ne 0 ]; then
  unrdf graph delete production
  unrdf graph create production --base-iri https://production.internal/
  unrdf graph update production --add backup-latest.nt
fi
```

**Success Criteria**:
- Migration success rate: 100% (with rollback on failure)
- Data loss: 0 triples lost during migration
- Downtime: <5min for 10M triple graph
- Backward compatibility: Old queries still work via mapping rules

---

## Multi-Agent Coordination Patterns

### Pattern 1: Pipeline Coordination
**Scenario**: Ingestion → Validation → Reasoning → Sync (4 agents)

```bash
# Agent 1: Ingestion (outputs: raw-triples.ttl)
unrdf convert api-response.json raw-triples.ttl

# Agent 2: Validation (outputs: validated-triples.ttl OR exit 1)
unrdf hook eval validation.json --data raw-triples.ttl && \
  cp raw-triples.ttl validated-triples.ttl

# Agent 3: Reasoning (outputs: inferred-triples.ttl)
unrdf hook eval reasoning.json --data validated-triples.ttl \
  --output inferred-triples.ttl

# Agent 4: Sync (broadcasts to network)
cat validated-triples.ttl inferred-triples.ttl | \
  curl -X POST https://graph-network/sync -d @-
```

**Coordination**: Agents use file-based handoff + exit codes for success/failure signaling.

---

### Pattern 2: Distributed Consensus
**Scenario**: 10 agents vote on owl:sameAs link quality

```bash
# Each agent evaluates link quality
for agent in agent-{1..10}; do
  ssh $agent "unrdf hook eval link-quality.json --data proposed-link.ttl" \
    > votes/$agent.json &
done
wait

# Aggregate votes
jq -s 'map(.confidence) | add / length' votes/*.json > consensus-score.txt

# Accept link if consensus > 0.8
if [ "$(cat consensus-score.txt)" -gt 0.8 ]; then
  unrdf graph update links --add proposed-link.ttl
fi
```

**Coordination**: Agents execute in parallel, results aggregated via consensus algorithm.

---

### Pattern 3: Event-Driven Pub/Sub
**Scenario**: Graph update triggers 5 downstream agent workflows

```bash
# Publisher: Graph update agent
unrdf graph update production --add new-data.ttl --emit-event

# Subscriber 1: Validation agent
unrdf context use production
unrdf hook eval validation.json --subscribe graph-updates

# Subscriber 2: Reasoning agent
unrdf hook eval reasoning.json --subscribe graph-updates

# Subscriber 3: Sync agent
# (pushes updates to remote graphs)

# Subscriber 4: Analytics agent
# (recomputes graph statistics)

# Subscriber 5: Notification agent
# (alerts human operators of significant changes)
```

**Coordination**: Event bus (Kafka/RabbitMQ) mediates pub/sub, agents subscribe to relevant event patterns.

---

### Pattern 4: Quorum-Based Replication
**Scenario**: Write to 3 of 5 replicas before acknowledging success

```bash
# Write to all 5 replicas in parallel
for replica in replica-{1..5}; do
  (
    unrdf graph update production --add new-triples.ttl \
      --target $replica &&
    echo "SUCCESS:$replica"
  ) &
done | head -3  # Wait for 3 successes

# If quorum achieved (3/5), commit
if [ "$(echo "$successes" | wc -l)" -ge 3 ]; then
  echo "COMMITTED"
  exit 0
else
  echo "QUORUM FAILED - ROLLBACK"
  # Trigger rollback on successful writes
  exit 1
fi
```

**Coordination**: Agents use quorum consensus to balance consistency and availability.

---

## OTEL Observability for Agent Coordination

Every CLI command emits OpenTelemetry spans for agent debugging:

```javascript
// Automatic OTEL instrumentation in CLI
import { trace } from '@opentelemetry/api';

const span = trace.getTracer('unrdf-cli').startSpan('graph.update', {
  attributes: {
    'agent.id': process.env.AGENT_ID,
    'graph.name': args.graphName,
    'operation': 'add',
    'triple.count': triples.length,
  },
});

try {
  await executeGraphUpdate(triples);
  span.setStatus({ code: SpanStatusCode.OK });
} catch (error) {
  span.recordException(error);
  span.setStatus({ code: SpanStatusCode.ERROR });
} finally {
  span.end();
}
```

**Agent Coordination via OTEL**:
- Parent span: Multi-agent workflow
- Child spans: Each agent's CLI invocation
- Distributed trace: Complete pipeline visualization in Jaeger

---

## Performance SLAs for Agent Operations

| Operation | SLA (p95) | Agent Impact |
|-----------|-----------|--------------|
| Graph create | 50ms | Fast context switching |
| Graph update (1K triples) | 200ms | Real-time ingestion |
| SPARQL query (simple) | 25ms | Low-latency reasoning |
| SPARQL query (complex) | 100ms | Acceptable for multi-hop |
| Validation (SHACL) | 500ms | Batch validation OK |
| Graph export (1M triples) | 10s | Sync agent batch transfer |
| Hook evaluation | 50ms | Event-driven orchestration |

**Agent Design Implication**: All operations are synchronous and deterministic for predictable agent behavior.

---

## Agent Failure Modes & Recovery

### Failure Mode 1: Ingestion Agent Crash Mid-Import
**Detection**: OTEL span remains open >5min
**Recovery**:
```bash
# Detect orphaned transaction
unrdf graph list --status incomplete

# Rollback partial import
unrdf graph delete incomplete-import-12345

# Retry from checkpoint
unrdf convert --resume checkpoint-12345.json
```

### Failure Mode 2: Validation Agent Rejects Valid Data
**Detection**: False positive rate spike in metrics
**Recovery**:
```bash
# Override validation for emergency ingestion
unrdf graph update production --add critical-data.ttl --skip-validation

# Log override for audit
unrdf graph update audit --add "
  <override-001> a :ValidationOverride ;
    :timestamp '$(date -Iseconds)' ;
    :reason 'Emergency data ingestion' ;
    :approvedBy :agent-supervisor .
"
```

### Failure Mode 3: Sync Agent Network Partition
**Detection**: Quorum write fails (< 3/5 replicas)
**Recovery**:
```bash
# Enter degraded mode (local writes only)
unrdf context use local-replica

# Queue updates for later sync
unrdf graph update local --add pending-updates.ttl --queue

# When network recovers, replay queue
unrdf graph update production --replay-queue
```

---

## Agent Identity & Authentication

Each agent has cryptographic identity for provenance:

```bash
# Generate agent keypair
unrdf context create agent-42 --generate-key

# Sign graph operations
unrdf graph update production --add new-data.ttl \
  --sign-with ~/.unrdf/agent-42.key

# Verify signature on query
unrdf query "SELECT ?s ?p ?o WHERE { ?s ?p ?o }" \
  --verify-signature --required-agent agent-42
```

**Trust Model**:
- Agent capabilities defined in RDF graph
- SPARQL queries enforce access control
- All operations auditable via provenance

---

## Agent Development Workflow

### 1. Agent Skeleton (Python example)

```python
#!/usr/bin/env python3
import subprocess
import json
import sys

class ValidationAgent:
    def __init__(self, agent_id):
        self.agent_id = agent_id
        self.graph = "validation-results"

    def validate(self, data_file):
        """Run SHACL validation via CLI"""
        result = subprocess.run([
            'unrdf', 'hook', 'eval', 'validation-rules.json',
            '--data', data_file,
            '--format', 'json'
        ], capture_output=True, text=True)

        if result.returncode != 0:
            self.record_violation(result.stdout)
            return False
        return True

    def record_violation(self, violation_json):
        """Store violation in graph for analysis"""
        subprocess.run([
            'unrdf', 'graph', 'update', self.graph,
            '--add', f'<violation-{self.agent_id}> a :ValidationViolation ; :data "{violation_json}" .'
        ])

if __name__ == '__main__':
    agent = ValidationAgent('val-agent-007')
    success = agent.validate(sys.argv[1])
    sys.exit(0 if success else 1)
```

### 2. Agent Deployment (Docker)

```dockerfile
FROM node:18-alpine
RUN npm install -g @unrdf/cli@5.0.0-beta.3

COPY agent.py /app/agent.py
COPY validation-rules.json /app/

ENTRYPOINT ["python3", "/app/agent.py"]
```

### 3. Agent Orchestration (Kubernetes)

```yaml
apiVersion: batch/v1
kind: CronJob
metadata:
  name: validation-agent
spec:
  schedule: "*/5 * * * *"  # Every 5 minutes
  jobTemplate:
    spec:
      template:
        spec:
          containers:
          - name: validator
            image: unrdf-validation-agent:latest
            env:
            - name: AGENT_ID
              value: "k8s-val-agent"
            - name: OTEL_EXPORTER_OTLP_ENDPOINT
              value: "http://jaeger:4318"
```

---

## Summary: AI-First Design Principles

1. **Deterministic Interfaces** - Same input = same output, every time
2. **Observability Built-In** - OTEL spans for every operation
3. **Fail-Fast Semantics** - Exit codes + JSON errors for programmatic handling
4. **Batch-Optimized** - Commands handle 1M+ triples efficiently
5. **Composable Pipelines** - stdin/stdout for Unix-style chaining
6. **Stateless Execution** - Agents can restart without losing context
7. **Distributed-First** - Multi-graph, multi-replica, multi-agent by default
8. **Provenance Mandatory** - All operations traceable to agent + timestamp
9. **Schema-Driven** - SHACL/OWL validation enforces correctness
10. **Event-Driven** - Hooks enable reactive agent behaviors

**This is not a CLI for humans. This is a substrate for autonomous knowledge agents.**

---

**Document Version**: 1.0.0
**Created**: 2025-12-06
**Target Audience**: AI agent developers, multi-agent system architects
**Status**: Foundation for agent avatar documentation
