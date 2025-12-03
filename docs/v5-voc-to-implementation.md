# VOC-to-Implementation Mapping: What Each User Gets

This document maps each of the 7 synthetic VOCs directly to what they'll use from UNRDF v5 and what they'll build themselves.

---

## VOC-1: Autonomous Knowledge Agent (Swarm Coordinator)

### Who They Are
Multi-agent systems coordinating RDF transformations across federated knowledge graphs. Each agent needs autonomy to enforce its own rules.

### What They Get from UNRDF v5 (Substrate)
```javascript
// Core substrate APIs they depend on
import {
  // RDF operations
  addQuad,
  removeQuad,
  queryStore,

  // SPARQL execution
  executeQuery,

  // Knowledge Hooks - policy mechanism
  defineHook,
  executeHook,

  // Federation - discover and query peers
  connectToPeer,
  remoteQuery,

  // Streaming - coordinate via change feed
  subscribeToChanges,
  publishChange
} from 'unrdf'

// They DON'T get:
// - Knowledge Engine (they implement own reasoning)
// - Dark Matter (they optimize internally)
// - Composables (not building UIs)
```

### Architecture They Build
```
┌─────────────────────────────────────────┐
│      Agent Coordinator (Their Code)     │
│  - Multi-agent orchestration            │
│  - Consensus protocols                  │
│  - Knowledge synthesis                  │
│  - Custom inference rules               │
│  - Swarm optimization                   │
└─────────────────────────────────────────┤
│  UNRDF v5 Substrate (Platform)          │
│  ├─ RDF operations                      │
│  ├─ SPARQL execution                    │
│  ├─ Knowledge Hooks (policy layer)      │
│  ├─ Federation (peer connections)       │
│  └─ Streaming (change coordination)     │
└─────────────────────────────────────────┘
```

### Key Integration Points
1. **Hooks for Policy Enforcement**: Define hooks that agents evaluate before accepting external mutations
2. **Federation for Peer Discovery**: Use federation to discover agent peers
3. **Streaming for Coordination**: Subscribe to change feeds from other agents
4. **Direct Quad Access**: Bypass any interpretation layer to examine exact state

### Code Pattern
```javascript
// Agent discovers peers
const peers = await federation.discoverPeers({
  capability: 'knowledge-synthesis'
})

// Agent defines policy (what mutations it accepts)
defineHook('agent-mutation-policy', {
  type: 'validate-before-write',
  async check(quad) {
    // Only accept quads that match agent's policy
    return quad.predicate.value === 'http://agent/policy/allows'
  }
})

// Agent subscribes to coordination changes
subscribeToChanges(localStore, (change) => {
  // Trigger re-consensus if external change detected
  if (change.origin !== 'self') {
    triggerConsensus(change)
  }
})

// Agent queries remote peers
const remoteData = await remoteQuery(peerEndpoint, sparqlQuery)
```

---

## VOC-2: Real-time Graph Sync Agent (Stream Processor)

### Who They Are
Maintains consistency across multiple UNRDF instances (multi-region, multi-datacenter). Needs bi-directional sync with guaranteed delivery.

### What They Get from UNRDF v5
```javascript
import {
  // Streaming - the core need
  subscribeToChanges,
  getChangesSince,

  // RDF operations - apply changes
  applyDelta,
  addQuad,
  removeQuad,

  // Federation - sync between instances
  connectToPeer,
  remoteQuery,

  // Knowledge Hooks - sync policies
  defineHook,

  // SPARQL - validate state
  executeQuery,

  // Observability - monitor sync health
  instrumentSpan
} from 'unrdf'
```

### What They Build
- Sync protocol layer
- Conflict resolution strategy
- Multi-region routing
- Change queue and replay logic
- Network reliability layer
- Custom metrics

### Architecture
```
Instance A (Region US)          Instance B (Region EU)
┌──────────────┐               ┌──────────────┐
│ Local Store  │               │ Local Store  │
└──────┬───────┘               └──────┬───────┘
       │                              │
       └──────────────────┬───────────┘
                   UNRDF Substrate
          (Streaming: subscribeToChanges)
                          │
         ┌────────────────┼────────────────┐
         │                │                │
    [Change Capture]  [Sync Agent]  [Conflict Resolution]
         │                │                │
         └────────────────┼────────────────┘

         (Their Code - Sync Protocol)
```

### Key Integration Points
1. **Change Feed**: Streaming subscription gives changes in order
2. **Deltas**: Get exact quad deltas, apply deterministically
3. **Federation**: Use federation for peer connection/health
4. **Hooks**: Define sync policies (accept/reject based on rules)

---

## VOC-3: Autonomous ML Agent (Pattern Learner)

### Who They Are
Learns patterns from RDF data and applies them autonomously. Read-heavy workload with controlled writes.

### What They Get from UNRDF v5
```javascript
import {
  // SPARQL - the primary need
  executeQuery,
  queryStore,

  // RDF operations - read patterns
  getQuads,
  iterateQuads,

  // Knowledge Hooks - apply learned policies
  defineHook,
  executeHook,

  // Streaming - trigger re-learning
  subscribeToChanges,

  // Canonicalization - consistent pattern matching
  canonicalize
} from 'unrdf'
```

### What They Build
- ML training pipeline
- Pattern detection algorithms
- Feature extraction
- Model persistence
- Prediction serving
- Custom validations

### Integration Pattern
```javascript
// Step 1: Learn from SPARQL query results
const patterns = await executeQuery(`
  SELECT ?subject ?object WHERE {
    ?subject rdf:type known_entity ;
             rdf:property ?object
  }
`)

// Step 2: Extract features and train model
const features = patterns.map(p => extractFeatures(p))
const model = trainPatternModel(features)

// Step 3: Define hooks to apply learned patterns
defineHook('ml-pattern-application', {
  type: 'transform-on-write',
  async apply(quad) {
    const prediction = await model.predict(canonicalize(quad))
    return prediction.confidence > 0.8 ? applyTransform(quad) : quad
  }
})

// Step 4: Re-train when new data arrives
subscribeToChanges(store, async (change) => {
  if (shouldRelearn(change)) {
    await retrainModel(store)
  }
})
```

---

## VOC-4: Autonomous Audit Agent (Compliance Monitor)

### Who They Are
Ensures RDF graphs maintain compliance policies continuously. Real-time enforcement with audit trails.

### What They Get from UNRDF v5
```javascript
import {
  // SPARQL - query for violations
  executeQuery,

  // Streaming - real-time monitoring
  subscribeToChanges,

  // Knowledge Hooks - enforce compliance
  defineHook,

  // RDF operations - trace to source
  getQuads,

  // Observability - audit logging
  span,
  recordError
} from 'unrdf'
```

### What They Build
- Compliance rule engine
- Violation detection
- Auto-remediation logic
- Audit event serialization
- Compliance reporting
- Escalation policies

### Integration Pattern
```javascript
// Step 1: Define compliance policy as hooks
defineHook('gdpr-compliance', {
  type: 'validate-before-write',
  async check(quad) {
    // Block PII without consent
    if (isPII(quad) && !hasConsent(quad.subject)) {
      recordError('GDPR violation', {
        quad,
        required: 'consent',
        quad_id: hash(quad)
      })
      return false
    }
    return true
  }
})

// Step 2: Monitor all changes
subscribeToChanges(store, async (change) => {
  const isCompliant = await executeQuery(`
    ASK {
      ${change.quad} ?p ?o .
      FILTER(compliance_rule_violated(?p, ?o))
    }
  `)

  if (!isCompliant) {
    // Log violation with full traceability
    recordComplianceViolation({
      change,
      violation_type: 'rule_breached',
      timestamp: Date.now(),
      trace_id: change.trace_id
    })
  }
})

// Step 3: Auto-remediate if policy allows
defineHook('auto-remediate', {
  type: 'transform-before-apply',
  async fix(change) {
    if (isCorporate PII(change)) {
      return redact(change)
    }
    return change
  }
})
```

---

## VOC-5: RDF Data Engineer (ETL Pipeline Builder)

### Who They Are
Builds ETL pipelines loading/transforming RDF data into UNRDF instances. Scripting and automation focus.

### What They Get from UNRDF v5
```javascript
import {
  // Graph CRUD - primary need
  createGraph,
  loadGraph,
  updateGraph,
  deleteGraph,

  // SPARQL - transform data
  executeQuery,

  // Federation - connect sources
  connectToPeer,
  remoteQuery,

  // Knowledge Hooks - validate during ingestion
  defineHook,

  // Streaming - incremental loads
  subscribeToChanges,

  // CLI - scripting
  cli
} from 'unrdf'
```

### What They Build
- ETL orchestration (dbt, Airflow, custom)
- Data transformation logic
- Source system connectors
- Validation rules
- Monitoring/alerting
- Incremental load logic

### Integration Pattern
```bash
# Via CLI (easiest for scripts)
$ unrdf graph create --name my-dataset
$ unrdf graph load --graph my-dataset --file data.ttl
$ unrdf graph export --graph my-dataset --format jsonld

# Or programmatic
import { createGraph, loadGraph } from 'unrdf/cli'

async function runETL() {
  // Create target graph
  const graph = await createGraph('etl-target')

  // Load from sources
  const sourceData = await remoteQuery(
    'https://source-system.example.com',
    'SELECT * WHERE { ?s ?p ?o }'
  )

  // Transform
  const transformed = transform(sourceData)

  // Load with validation
  defineHook('etl-validation', {
    type: 'validate-before-write',
    check(quad) {
      return isValidETLQuad(quad)
    }
  })

  // Batch apply
  for (const quad of transformed) {
    await applyQuad(graph, quad)
  }
}
```

---

## VOC-6: Knowledge Graph Application Developer (App Builder)

### Who They Are
Builds web/mobile apps using UNRDF backend. React/Vue apps needing real-time graph state.

### What They Get from UNRDF v5
```javascript
import {
  // Composables - the main interface
  useGraph,
  useDelta,
  useTerms,
  useValidator,

  // Browser SDK
  IndexedDBStore,

  // Knowledge Hooks - client-side validation
  defineHook,

  // Streaming - real-time updates
  subscribeToChanges,

  // Federation - query remote graphs
  remoteQuery,

  // CLI - bootstrap projects
  cli
} from 'unrdf'
```

### What They Build
- App business logic
- UI components
- State management
- Data validation rules
- API integration
- Custom composables
- Offline strategies

### Integration Pattern (Vue 3)
```javascript
// src/components/GraphViewer.vue
<script setup>
import { useGraph, useDelta, useValidator } from 'unrdf'

// Get reactive graph state
const { store, loading, error } = useGraph('http://api/graph', {
  persistence: 'indexeddb' // Browser storage
})

// Track changes for undo/redo
const { deltas, push, undo } = useDelta(store)

// Validate on input
const { validate } = useValidator(store)

// Real-time sync
const unsubscribe = subscribeToChanges(store, (change) => {
  console.log('Graph updated:', change)
})

// App logic
async function addEntity(label) {
  const quad = createQuad(subject, rdfs.label, label)

  if (!validate(quad)) {
    throw new Error('Invalid quad')
  }

  store.addQuad(quad)
  push(quad) // Track for undo
}

onUnmounted(() => unsubscribe())
</script>
```

---

## VOC-7: Infrastructure & Platform Operator (DevOps)

### Who They Are
Deploys, monitors, and operates UNRDF instances in production. K8s, Docker, Terraform focused.

### What They Get from UNRDF v5
```javascript
import {
  // Observability - the main need
  initOTEL,
  recordMetric,
  recordError,
  span,

  // Streaming - health monitoring
  subscribeToChanges,

  // Federation - monitor replication
  connectToPeer,

  // CLI - operations
  cli,

  // Knowledge Hooks - operational policies
  defineHook,

  // SPARQL - health checks
  executeQuery
} from 'unrdf'
```

### What They Build
- Kubernetes manifests
- Terraform configurations
- Health check scripts
- Monitoring dashboards
- Alerting rules
- Backup/restore procedures
- Deployment automation
- Scaling policies

### Integration Pattern
```javascript
// monitoring/health-check.mjs
import { executeQuery, span, recordMetric } from 'unrdf'

async function healthCheck(graphUri) {
  const checkSpan = span({
    name: 'health_check',
    attributes: { graph: graphUri }
  })

  try {
    // Check graph is accessible
    const count = await executeQuery(`
      SELECT (COUNT(*) as ?count) WHERE {
        ?s ?p ?o
      }
    `)

    recordMetric('graph.quad_count', count)
    checkSpan.recordOk()
    return { healthy: true, quads: count }
  } catch (error) {
    recordError('health_check_failed', error)
    checkSpan.recordError(error)
    return { healthy: false, error: error.message }
  }
}
```

### Deployment
```yaml
# k8s/unrdf-deployment.yaml
apiVersion: apps/v1
kind: Deployment
metadata:
  name: unrdf
spec:
  template:
    spec:
      containers:
      - name: unrdf
        image: unrdf:v5.0
        env:
        # Observability
        - name: OTEL_EXPORTER_OTLP_ENDPOINT
          value: http://jaeger:4317
        - name: OTEL_RESOURCE_ATTRIBUTES
          value: service.name=unrdf,deployment=prod
        # Federation
        - name: FEDERATION_PEERS
          value: "unrdf-us.example.com,unrdf-eu.example.com"
        # Hooks
        - name: HOOKS_PATH
          value: /etc/unrdf/hooks
```

---

## Summary Table

| VOC | Primary Use | Core Substrate | What They Build | Dependencies |
|-----|-----------|--------|-----------------|--------------|
| 1: Agent | Multi-agent coordination | RDF ops, Hooks, Federation, Streaming | Reasoning engine, consensus | Substrate only |
| 2: Sync | Real-time replication | Streaming, Federation, RDF ops | Sync protocol, conflict resolution | Substrate only |
| 3: ML | Pattern learning | SPARQL, Hooks, Streaming, Canonicalize | ML pipeline, model, validators | Substrate only |
| 4: Audit | Compliance monitoring | Streaming, Hooks, SPARQL, Observability | Rule engine, remediation | Substrate only |
| 5: Engineer | ETL pipelines | SPARQL, Hooks, Federation, CLI | ETL orchestration, transforms | Substrate only |
| 6: Developer | Web apps | Composables, Hooks, Streaming, Browser SDK | App logic, UI, state mgmt | @unrdf/composables |
| 7: DevOps | Production ops | Observability, Streaming, CLI, Hooks | K8s/Terraform, monitoring | Substrate only |

---

## Key Insight

**All 7 VOCs can be served from the substrate + 1 opt-in package (composables for web).**

This proves the substrate boundaries are correct. No knowledge engine, dark matter, or project engine needed for any of them. They're optional enhancements for specific scenarios.
