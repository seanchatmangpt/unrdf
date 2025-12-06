# AI Agent Avatars: Autonomous Knowledge Graph Operators

**Design Philosophy**: These are not user personas. These are **autonomous computational entities** with specific capabilities, constraints, and coordination protocols. Each avatar represents a class of agents in a distributed knowledge graph infrastructure.

---

## Avatar Classification System

### Tier 1: Core Infrastructure Agents
**Purpose**: Maintain graph integrity, availability, and performance
**Autonomy**: Fully autonomous, no human oversight
**Deployment**: Always-on, containerized, Kubernetes-managed

### Tier 2: Domain-Specific Agents
**Purpose**: Execute business logic, apply domain rules
**Autonomy**: Semi-autonomous, human-defined policies
**Deployment**: Event-triggered, serverless functions

### Tier 3: Cognitive Reasoning Agents
**Purpose**: Derive new knowledge, answer complex queries
**Autonomy**: Adaptive, machine learning-enabled
**Deployment**: GPU-accelerated, batch + interactive

---

## Tier 1 Avatars

### Avatar 1: The Guardian (Validation Agent)

**Identity**:
```json
{
  "agent_id": "guardian-001",
  "type": "validation",
  "capabilities": ["shacl", "owl-consistency", "cardinality-check"],
  "authority": "reject-invalid-triples",
  "sla": {
    "latency_p95": "500ms",
    "throughput": "10K triples/sec"
  }
}
```

**Personality** (computational traits):
- **Strict**: Zero tolerance for constraint violations
- **Predictable**: Same validation rules always produce same result
- **Transparent**: Emits detailed OTEL spans for every check
- **Resilient**: Continues validation even if individual rules fail

**Daily Workflow** (24/7 operation):
```bash
#!/bin/bash
# Guardian agent main loop

while true; do
  # Watch for graph update events
  unrdf graph describe production --watch --format jsonstream | while read event; do
    echo "$event" | jq -r '.quads[]' > /tmp/event-quads.nt

    # Validate against SHACL shapes
    if ! unrdf hook eval /rules/shacl-validation.json \
      --data /tmp/event-quads.nt \
      --output /violations/$(date +%s).json; then

      # Reject invalid triples
      curl -X POST http://graph-api/rollback \
        -d "{\"event_id\": \"$(echo $event | jq -r '.id')\"}"

      # Alert coordination layer
      curl -X POST http://alert-api/violation \
        -d @/violations/$(date +%s).json
    fi
  done

  sleep 1
done
```

**Coordination Protocol**:
- **Input**: Graph update events (JSON stream)
- **Output**: Validation pass/fail + violation details
- **Interactions**: Blocks invalid writes, alerts remediation agents
- **Failure Mode**: Rejects everything on error (fail-secure)

**Performance Metrics**:
```sparql
SELECT ?hour (AVG(?latency) AS ?avg_latency) (COUNT(?event) AS ?events)
WHERE {
  ?event a :ValidationEvent ;
         :agent <guardian-001> ;
         :latency ?latency ;
         :timestamp ?ts .
  BIND(HOURS(?ts) AS ?hour)
}
GROUP BY ?hour
ORDER BY ?hour
```

---

### Avatar 2: The Scribe (Provenance Agent)

**Identity**:
```json
{
  "agent_id": "scribe-alpha",
  "type": "provenance",
  "capabilities": ["prov-o", "temporal-tracking", "lineage-graph"],
  "authority": "write-provenance-metadata",
  "sla": {
    "metadata_overhead": "<10%",
    "query_latency": "200ms"
  }
}
```

**Personality**:
- **Meticulous**: Records every operation with nanosecond precision
- **Immutable**: Provenance graphs are append-only, never deleted
- **Queryable**: Optimizes provenance graph for temporal SPARQL queries
- **Distributed**: Replicates provenance across 5 nodes for durability

**Daily Workflow**:
```python
#!/usr/bin/env python3
# Scribe agent - provenance tracker

import subprocess
import json
from datetime import datetime

class ScribeAgent:
    def __init__(self):
        self.agent_id = "scribe-alpha"
        self.prov_graph = "provenance-log"

    def record_operation(self, operation):
        """Record operation provenance"""
        prov_triple = f"""
        <op-{operation['id']}> a prov:Activity ;
            prov:wasAssociatedWith <{operation['agent']}> ;
            prov:startedAtTime "{operation['start']}"^^xsd:dateTime ;
            prov:endedAtTime "{operation['end']}"^^xsd:dateTime ;
            prov:used <{operation['input']}> ;
            prov:generated <{operation['output']}> .
        """

        subprocess.run([
            'unrdf', 'graph', 'update', self.prov_graph,
            '--add', prov_triple
        ])

    def query_lineage(self, entity_uri):
        """Trace lineage back to source"""
        query = f"""
        SELECT ?source ?agent ?timestamp
        WHERE {{
          <{entity_uri}> prov:wasDerivedFrom+ ?source .
          ?activity prov:generated <{entity_uri}> ;
                    prov:wasAssociatedWith ?agent ;
                    prov:startedAtTime ?timestamp .
        }}
        ORDER BY ?timestamp
        """

        result = subprocess.run([
            'unrdf', 'query', query,
            '--graph', self.prov_graph,
            '--format', 'json'
        ], capture_output=True, text=True)

        return json.loads(result.stdout)

# Main loop
if __name__ == '__main__':
    scribe = ScribeAgent()

    # Subscribe to all graph operations
    # Record provenance for each
    while True:
        event = read_operation_event()  # From message queue
        scribe.record_operation(event)
```

**Coordination Protocol**:
- **Input**: Operation events from all agents
- **Output**: Provenance metadata in separate graph
- **Interactions**: Read-only for other agents (queryable lineage)
- **Failure Mode**: Queue events if graph unavailable, replay on recovery

---

### Avatar 3: The Replicator (Sync Agent)

**Identity**:
```json
{
  "agent_id": "replicator-prime",
  "type": "synchronization",
  "capabilities": ["crdt", "quorum-write", "conflict-resolution"],
  "authority": "cross-replica-sync",
  "sla": {
    "convergence_time": "60s",
    "consistency": "eventual"
  }
}
```

**Personality**:
- **Distributed**: Operates across 10+ replica nodes
- **Conflict-Aware**: Uses CRDTs to merge divergent states
- **Patient**: Eventual consistency over immediate consistency
- **Fault-Tolerant**: Continues syncing even if 40% of nodes are down

**Daily Workflow**:
```bash
#!/bin/bash
# Replicator agent - cross-replica sync

REPLICAS=("replica-1" "replica-2" "replica-3" "replica-4" "replica-5")
LOCAL_STATE="/data/local-graph.nt"

while true; do
  # Export local graph state
  unrdf graph export local --format ntriples > $LOCAL_STATE
  LOCAL_HASH=$(sha256sum $LOCAL_STATE | awk '{print $1}')

  # Check each replica for divergence
  for replica in "${REPLICAS[@]}"; do
    REMOTE_HASH=$(curl -s "http://$replica/graph/hash")

    if [ "$LOCAL_HASH" != "$REMOTE_HASH" ]; then
      echo "Divergence detected with $replica"

      # Fetch remote state
      curl -s "http://$replica/graph/export" > /tmp/remote-state.nt

      # Compute delta using SPARQL DIFF
      unrdf query-file diff-query.rq \
        $LOCAL_STATE /tmp/remote-state.nt > /tmp/delta.nt

      # Apply CRDT merge
      unrdf graph update local \
        --delta /tmp/delta.nt \
        --strategy crdt \
        --conflict-resolution last-write-wins

      # Push merged state back to replica
      unrdf graph export local --format ntriples | \
        curl -X POST "http://$replica/graph/import" -d @-
    fi
  done

  sleep 30  # Sync every 30 seconds
done
```

**Coordination Protocol**:
- **Input**: Local graph updates + remote graph states
- **Output**: Merged graph state distributed to all replicas
- **Interactions**: Peer-to-peer with other replicators
- **Failure Mode**: Partition-tolerant (CAP theorem: AP over C)

**Conflict Resolution Strategy**:
```javascript
// CRDT merge for conflicting triples
function mergeConflicts(localTriple, remoteTriple) {
  if (localTriple.timestamp > remoteTriple.timestamp) {
    return localTriple;  // Last-write-wins
  } else if (localTriple.timestamp === remoteTriple.timestamp) {
    // Use agent ID as tiebreaker (lexicographic order)
    return localTriple.agent < remoteTriple.agent ? localTriple : remoteTriple;
  } else {
    return remoteTriple;
  }
}
```

---

## Tier 2 Avatars

### Avatar 4: The Reasoner (Inference Agent)

**Identity**:
```json
{
  "agent_id": "reasoner-neural-7",
  "type": "inference",
  "capabilities": ["rdfs-entailment", "owl-reasoning", "custom-rules"],
  "authority": "materialize-inferred-triples",
  "sla": {
    "incremental_latency": "100ms",
    "completeness": "100%"
  }
}
```

**Personality**:
- **Logical**: Applies formal semantics (OWL/RDFS) rigorously
- **Incremental**: Updates materialization on every graph change
- **Complete**: Guarantees all entailments are derived
- **Explainable**: Provides proof trees for inferred triples

**Daily Workflow**:
```javascript
// Reasoner agent - OWL/RDFS inference

import { createStore, executeQuerySync } from '@unrdf/core';
import { registerHook } from '@unrdf/hooks';

class ReasonerAgent {
  constructor() {
    this.agentId = 'reasoner-neural-7';
    this.baseGraph = createStore();
    this.inferredGraph = createStore();
  }

  async initialize() {
    // Load ontology
    const ontology = await loadTurtleFile('ontology.ttl');
    this.baseGraph.addQuads(ontology);

    // Register hook to trigger reasoning on updates
    registerHook(this.baseGraph, 'after:add', (quad) => {
      this.deriveEntailments(quad);
    });
  }

  deriveEntailments(newQuad) {
    // RDFS: rdfs:subClassOf transitivity
    if (newQuad.predicate.value === 'http://www.w3.org/2000/01/rdf-schema#subClassOf') {
      const superClasses = this.findTransitiveSuperClasses(newQuad.object);
      superClasses.forEach(superClass => {
        this.inferredGraph.add({
          subject: newQuad.subject,
          predicate: newQuad.predicate,
          object: superClass
        });
      });
    }

    // OWL: owl:inverseOf
    const inverseProperties = this.findInverseProperties(newQuad.predicate);
    inverseProperties.forEach(invProp => {
      this.inferredGraph.add({
        subject: newQuad.object,
        predicate: invProp,
        object: newQuad.subject
      });
    });

    // Persist inferred triples
    this.persistInferredTriples();
  }

  persistInferredTriples() {
    const triples = Array.from(this.inferredGraph.getQuads());
    subprocess.run([
      'unrdf', 'graph', 'update', 'inferred',
      '--add', JSON.stringify(triples)
    ]);
  }
}

// Run agent
const agent = new ReasonerAgent();
agent.initialize();
```

**Coordination Protocol**:
- **Input**: Base graph triples + ontology
- **Output**: Inferred triples in separate graph
- **Interactions**: Read base graph, write inferred graph, no cycles
- **Failure Mode**: Re-compute all entailments from scratch on error

**Performance Optimization**:
- **Incremental**: Only recompute affected entailments
- **Indexed**: Predicate-based indexing for fast lookup
- **Parallel**: Multi-threaded reasoning for large ontologies

---

### Avatar 5: The Curator (Linking Agent)

**Identity**:
```json
{
  "agent_id": "curator-entity-linker",
  "type": "entity-linking",
  "capabilities": ["owl:sameAs", "similarity-matching", "blocking"],
  "authority": "create-entity-links",
  "sla": {
    "precision": "0.90",
    "recall": "0.80",
    "throughput": "10K entities/sec"
  }
}
```

**Personality**:
- **Discerning**: Only creates high-confidence links (>0.85 score)
- **Scalable**: Uses blocking to avoid O(n²) comparisons
- **Explainable**: Records similarity features for each link
- **Conservative**: Prefers false negatives over false positives

**Daily Workflow**:
```python
#!/usr/bin/env python3
# Curator agent - entity linking

import subprocess
import json
from sklearn.metrics.pairwise import cosine_similarity
import numpy as np

class CuratorAgent:
    def __init__(self):
        self.agent_id = "curator-entity-linker"
        self.confidence_threshold = 0.85

    def extract_entities(self, graph_uri):
        """Extract entities from graph"""
        query = """
        SELECT ?entity ?name ?email ?phone
        WHERE {
          ?entity a :Person ;
                  :name ?name .
          OPTIONAL { ?entity :email ?email }
          OPTIONAL { ?entity :phone ?phone }
        }
        """

        result = subprocess.run([
            'unrdf', 'query', query,
            '--graph', graph_uri,
            '--format', 'json'
        ], capture_output=True, text=True)

        return json.loads(result.stdout)

    def compute_similarity(self, entity_a, entity_b):
        """Compute entity similarity using multiple features"""
        features = []

        # Feature 1: Name similarity (Levenshtein)
        name_sim = self.levenshtein_similarity(
            entity_a.get('name', ''),
            entity_b.get('name', '')
        )
        features.append(name_sim)

        # Feature 2: Email domain match
        email_sim = 1.0 if (
            entity_a.get('email', '').split('@')[1] ==
            entity_b.get('email', '').split('@')[1]
        ) else 0.0
        features.append(email_sim)

        # Feature 3: Phone number match
        phone_sim = 1.0 if entity_a.get('phone') == entity_b.get('phone') else 0.0
        features.append(phone_sim)

        # Weighted average
        weights = [0.5, 0.3, 0.2]
        return sum(f * w for f, w in zip(features, weights))

    def create_links(self, source_graph, target_graph):
        """Find and create owl:sameAs links"""
        source_entities = self.extract_entities(source_graph)
        target_entities = self.extract_entities(target_graph)

        links = []
        for src in source_entities:
            for tgt in target_entities:
                sim = self.compute_similarity(src, tgt)

                if sim >= self.confidence_threshold:
                    links.append({
                        'source': src['entity'],
                        'target': tgt['entity'],
                        'confidence': sim
                    })

        # Persist links
        for link in links:
            subprocess.run([
                'unrdf', 'graph', 'update', 'entity-links',
                '--add', f"""
                <{link['source']}> owl:sameAs <{link['target']}> .
                <{link['source']}> :linkConfidence {link['confidence']} .
                """
            ])

        return links

# Main execution
if __name__ == '__main__':
    curator = CuratorAgent()
    links = curator.create_links('source-a', 'source-b')
    print(f"Created {len(links)} entity links")
```

**Coordination Protocol**:
- **Input**: Multiple source graphs
- **Output**: Entity links graph (owl:sameAs assertions)
- **Interactions**: Read-only on sources, write to links graph
- **Failure Mode**: Degrade to conservative matching (higher threshold)

---

## Tier 3 Avatars

### Avatar 6: The Oracle (Query Optimization Agent)

**Identity**:
```json
{
  "agent_id": "oracle-qopt-3",
  "type": "query-optimization",
  "capabilities": ["query-rewrite", "index-selection", "caching"],
  "authority": "rewrite-user-queries",
  "sla": {
    "latency_reduction": "40%",
    "cache_hit_rate": "70%"
  }
}
```

**Personality**:
- **Adaptive**: Learns optimal query plans from execution history
- **Predictive**: Prefetches likely query results
- **Transparent**: Explains optimization decisions via OTEL spans
- **Cost-Aware**: Balances query latency vs. resource usage

**Daily Workflow**:
```javascript
// Oracle agent - query optimization

import { analyzeQuery, rewriteQuery } from './query-optimizer.mjs';
import { cache } from './redis-cache.mjs';

class OracleAgent {
  constructor() {
    this.agentId = 'oracle-qopt-3';
    this.queryHistory = [];
  }

  async optimizeQuery(sparqlQuery) {
    // Check cache first
    const cacheKey = this.hashQuery(sparqlQuery);
    const cached = await cache.get(cacheKey);

    if (cached) {
      console.log('✅ Cache hit');
      return { results: cached, source: 'cache' };
    }

    // Analyze query complexity
    const analysis = analyzeQuery(sparqlQuery);

    if (analysis.complexity > 100) {
      // Rewrite complex query
      const optimized = rewriteQuery(sparqlQuery, {
        pushFilters: true,
        reorderJoins: true,
        useMaterializedViews: true
      });

      console.log('🔄 Query rewritten for optimization');
      sparqlQuery = optimized;
    }

    // Execute query
    const results = await this.executeQuery(sparqlQuery);

    // Cache results with TTL
    await cache.set(cacheKey, results, { ttl: 3600 });

    // Record for learning
    this.queryHistory.push({
      query: sparqlQuery,
      latency: analysis.executionTime,
      results: results.length
    });

    return { results, source: 'graph' };
  }

  async executeQuery(sparql) {
    const result = await subprocess([
      'unrdf', 'query', sparql,
      '--graph', 'production',
      '--format', 'json'
    ]);

    return JSON.parse(result.stdout);
  }
}

// API endpoint
const oracle = new OracleAgent();
app.post('/query', async (req, res) => {
  const { results, source } = await oracle.optimizeQuery(req.body.sparql);
  res.json({ results, metadata: { source, agent: oracle.agentId } });
});
```

**Coordination Protocol**:
- **Input**: SPARQL queries from client agents
- **Output**: Optimized query results + metadata
- **Interactions**: Reads graph, writes to cache, no state mutation
- **Failure Mode**: Fall back to unoptimized query execution

---

### Avatar 7: The Sentinel (Anomaly Detection Agent)

**Identity**:
```json
{
  "agent_id": "sentinel-watch-9",
  "type": "anomaly-detection",
  "capabilities": ["statistical", "ml-based", "graph-topology"],
  "authority": "flag-anomalies",
  "sla": {
    "detection_latency": "10s",
    "false_positive_rate": "0.05"
  }
}
```

**Personality**:
- **Vigilant**: Continuously monitors graph statistics
- **Adaptive**: Updates anomaly models based on historical data
- **Proactive**: Flags anomalies before they cause failures
- **Non-Invasive**: Observes only, does not mutate graph

**Daily Workflow**:
```python
#!/usr/bin/env python3
# Sentinel agent - anomaly detection

import subprocess
import json
import numpy as np
from scipy import stats

class SentinelAgent:
    def __init__(self):
        self.agent_id = "sentinel-watch-9"
        self.baseline_stats = self.load_baseline()

    def load_baseline(self):
        """Load historical graph statistics"""
        result = subprocess.run([
            'unrdf', 'query', '''
            SELECT
              (COUNT(?s) AS ?tripleCount)
              (COUNT(DISTINCT ?s) AS ?subjectCount)
              (COUNT(DISTINCT ?p) AS ?predicateCount)
            WHERE { ?s ?p ?o }
            ''',
            '--graph', 'production',
            '--format', 'json'
        ], capture_output=True, text=True)

        return json.loads(result.stdout)[0]

    def detect_anomalies(self):
        """Detect statistical anomalies in graph"""
        current_stats = self.load_baseline()  # Fetch current state

        anomalies = []

        # Anomaly 1: Triple count spike
        if current_stats['tripleCount'] > self.baseline_stats['tripleCount'] * 2:
            anomalies.append({
                'type': 'triple_count_spike',
                'severity': 'high',
                'current': current_stats['tripleCount'],
                'baseline': self.baseline_stats['tripleCount']
            })

        # Anomaly 2: Predicate distribution shift
        current_pred_dist = self.get_predicate_distribution()
        baseline_pred_dist = self.baseline_predicate_distribution

        kl_divergence = stats.entropy(current_pred_dist, baseline_pred_dist)
        if kl_divergence > 0.5:  # Threshold
            anomalies.append({
                'type': 'predicate_distribution_shift',
                'severity': 'medium',
                'divergence': kl_divergence
            })

        # Anomaly 3: Outlier entities (>1000 triples)
        outliers = self.find_outlier_entities()
        if len(outliers) > 0:
            anomalies.append({
                'type': 'outlier_entities',
                'severity': 'low',
                'count': len(outliers),
                'entities': outliers[:10]  # Top 10
            })

        return anomalies

    def find_outlier_entities(self):
        """Find entities with abnormally high triple count"""
        result = subprocess.run([
            'unrdf', 'query', '''
            SELECT ?entity (COUNT(?p) AS ?count)
            WHERE { ?entity ?p ?o }
            GROUP BY ?entity
            HAVING (COUNT(?p) > 1000)
            ORDER BY DESC(?count)
            ''',
            '--graph', 'production',
            '--format', 'json'
        ], capture_output=True, text=True)

        return json.loads(result.stdout)

    def alert(self, anomalies):
        """Send anomaly alerts"""
        if len(anomalies) > 0:
            # Write to anomaly graph
            for anom in anomalies:
                subprocess.run([
                    'unrdf', 'graph', 'update', 'anomalies',
                    '--add', f'''
                    <anomaly-{hash(str(anom))}> a :Anomaly ;
                        :type "{anom['type']}" ;
                        :severity "{anom['severity']}" ;
                        :detectedBy <{self.agent_id}> ;
                        :timestamp "{datetime.now().isoformat()}"^^xsd:dateTime .
                    '''
                ])

            # Trigger alert webhook
            requests.post('http://alert-api/anomaly', json=anomalies)

# Main loop
if __name__ == '__main__':
    sentinel = SentinelAgent()

    while True:
        anomalies = sentinel.detect_anomalies()
        if anomalies:
            sentinel.alert(anomalies)

        time.sleep(60)  # Check every minute
```

**Coordination Protocol**:
- **Input**: Graph statistics (current + historical)
- **Output**: Anomaly alerts + anomaly graph
- **Interactions**: Read-only on production, write to anomaly graph
- **Failure Mode**: Default to conservative detection (lower sensitivity)

---

## Multi-Agent Swarm Scenarios

### Scenario 1: Real-Time Data Pipeline

**Agents Involved**:
1. **Ingestion Bot** (Tier 2) - Fetches data from APIs
2. **Guardian** (Tier 1) - Validates incoming data
3. **Reasoner** (Tier 2) - Derives entailments
4. **Replicator** (Tier 1) - Syncs to replicas
5. **Scribe** (Tier 1) - Records provenance

**Workflow**:
```
API → Ingestion → Guardian → Reasoner → Replicator → Scribe
        ↓ (fail)     ↓ (pass)    ↓          ↓          ↓
      Reject      Accept     Infer    Sync     Audit
```

**Coordination**: Event-driven pipeline with Redis Streams

---

### Scenario 2: Federated Query Across 10 Graphs

**Agents Involved**:
1. **Oracle** (Tier 3) - Query optimizer
2. **10 x Local Query Agents** (Tier 2) - Execute subqueries
3. **Result Merger Agent** (Tier 2) - Combines results
4. **Cache Agent** (Tier 1) - Stores result

**Workflow**:
```
User Query → Oracle (decompose) → [10 parallel subqueries]
                ↓
          [10 results] → Merger → Cache → User
```

**Coordination**: Distributed query execution with quorum consensus

---

## Agent Communication Protocols

### Protocol 1: JSON-RPC over HTTP

```javascript
// Agent A calls Agent B
const response = await fetch('http://agent-b:8080/rpc', {
  method: 'POST',
  headers: { 'Content-Type': 'application/json' },
  body: JSON.stringify({
    jsonrpc: '2.0',
    method: 'validateTriples',
    params: { triples: [...] },
    id: 'req-12345'
  })
});

const result = await response.json();
// { jsonrpc: '2.0', result: { valid: true }, id: 'req-12345' }
```

### Protocol 2: Message Queue (RabbitMQ)

```python
import pika

# Agent publishes event
connection = pika.BlockingConnection(pika.ConnectionParameters('rabbitmq'))
channel = connection.channel()
channel.queue_declare(queue='graph-updates')

channel.basic_publish(
    exchange='',
    routing_key='graph-updates',
    body=json.dumps({
        'event': 'graph.updated',
        'graph': 'production',
        'triples_added': 1247
    })
)
```

### Protocol 3: gRPC for High-Throughput

```protobuf
service GraphService {
  rpc ValidateTriples(TripleSet) returns (ValidationResult);
  rpc QueryGraph(SPARQLQuery) returns (stream Result);
}
```

---

## Avatar Deployment Matrix

| Avatar | Container | CPU | Memory | Replicas | Cost/Month |
|--------|-----------|-----|--------|----------|------------|
| Guardian | Docker | 2 cores | 4GB | 3 | $150 |
| Scribe | K8s | 1 core | 2GB | 5 | $100 |
| Replicator | K8s | 4 cores | 8GB | 5 | $500 |
| Reasoner | GPU | 8 cores + A100 | 32GB | 2 | $2000 |
| Curator | Serverless | Auto | Auto | Auto | $50 |
| Oracle | Redis + Node | 4 cores | 16GB | 2 | $300 |
| Sentinel | K8s | 2 cores | 4GB | 1 | $75 |

**Total Infrastructure**: ~$3,175/month for 10M triple production deployment

---

## Summary: Agent Design Principles

1. **Single Responsibility** - Each agent does ONE job well
2. **Observable** - OTEL spans for every operation
3. **Stateless Execution** - Restart without losing context
4. **Fail-Fast** - Exit codes indicate success/failure unambiguously
5. **Composable** - Agents coordinate via events, not shared state
6. **Resilient** - Tolerate partial failures (circuit breakers)
7. **Scalable** - Horizontal scaling via replication
8. **Provenance** - All operations traceable to agent ID
9. **Deterministic** - Same input → same output
10. **Autonomous** - No human intervention required

**These are not user personas. These are computational entities in a distributed knowledge graph infrastructure.**

---

**Document Version**: 1.0.0
**Created**: 2025-12-06
**Target Audience**: AI agent developers, DevOps engineers, knowledge graph architects
**Next**: See `AI-AGENT-WORKFLOWS.md` for detailed implementation patterns
