# UNRDF Architecture for Agents

**Conceptual understanding of UNRDF's autonomic architecture**

This document explains the architectural principles, design decisions, and how MAPEK and Knowledge Hooks work together to create an autonomic system.

```json
{
  "@context": {
    "@vocab": "urn:unrdf:architecture:",
    "schema": "http://schema.org/"
  },
  "@id": "urn:unrdf:architecture:v4.0.0",
  "@type": "architecture:Documentation",
  "version": "4.0.0",
  "targetAudience": ["AutonomicAgent", "HyperintelligentSystem"]
}
```

---

## Core Architectural Principles

### 1. Knowledge Graph as Substrate

**Principle:** The RDF knowledge graph is the single source of truth. All system state, capabilities, and policies are represented as RDF.

**Implications for Agents:**
- Query system state via SPARQL
- Discover capabilities through semantic queries
- Represent agent knowledge in RDF format
- Use graph patterns for condition matching

**Example:**
```sparql
# Query system capabilities
PREFIX cap: <urn:unrdf:capability:>
SELECT ?capability WHERE {
  ?capability a cap:Capability ;
              cap:domain "validation" .
}
```

### 2. Content-Addressed Conditions

**Principle:** Hook conditions are content-addressed (hash-based), not embedded strings. This enables verification, deduplication, and provenance.

**Implications for Agents:**
- Store conditions as files with SHA-256 hashes
- Reference conditions by hash, not by name
- Verify condition integrity before execution
- Share conditions across systems via hash

**Example:**
```javascript
{
  when: {
    kind: 'sparql-ask',
    ref: {
      uri: 'file://hooks/compliance/large-tx.ask.rq',
      sha256: 'e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855',
      mediaType: 'application/sparql-query'
    }
  }
}
```

### 3. Declarative Over Imperative

**Principle:** Define *what* to monitor (SPARQL conditions), not *how* to check. The system figures out execution order and optimization.

**Implications for Agents:**
- Express business logic as SPARQL queries
- Let system optimize query execution
- Focus on declarative policies, not implementation
- Use knowledge hooks for reactivity, not event buses

### 4. MAPEK Autonomic Loop

**Principle:** Continuous Monitor-Analyze-Plan-Execute-Knowledge cycle enables self-management.

**Implications for Agents:**
- System monitors itself (no manual checks needed)
- System analyzes itself (health scores calculated automatically)
- System plans fixes (decisions prioritized automatically)
- System executes fixes (via Knowledge Hooks)
- System learns (patterns extracted automatically)

---

## MAPEK Architecture

### Phase Flow

```
Monitor → Analyze → Plan → Execute → Knowledge
   ↓         ↓        ↓       ↓          ↓
  Gaps    Scores   Decisions Hooks   Patterns
```

### Integration Points

**Monitor Phase:**
- Gap Finder: SPARQL queries project structure
- Type Auditor: Compares Zod schemas with TypeScript
- Hotspot Analyzer: Calculates complexity metrics
- Drift Detector: Compares current state to baseline

**Analyze Phase:**
- Health Score Calculator: Weighted average of metrics
- Priority Assigner: Type issues > gaps > complexity
- Decision Classifier: Auto-fixable vs. manual

**Plan Phase:**
- Action Generator: Creates file generation plans
- Type Sync Planner: Plans Zod/TS synchronization
- Refactoring Queue: Queues manual review items

**Execute Phase:**
- Knowledge Hook Creator: `createAutonomicHooks()`
- Hook Registrar: Registers hooks with system
- Transaction Executor: Executes fixes via transactions

**Knowledge Phase:**
- Pattern Extractor: Identifies recurring issues
- Threshold Updater: Adjusts complexity thresholds
- Policy Updater: Updates learned policies

---

## Knowledge Hooks Architecture

### Lifecycle Phases

```
before → condition evaluation → run → after
  ↓              ↓                ↓      ↓
cancel?      triggered?      effect   audit
```

### Transaction Integration

**State Machine:**
```
PENDING → ACTIVE → HOOKS_PASSED → APPLYING → POST_HOOKS → COMMITTED
           ↓                              ↓
        ROLLING_BACK ←───────────────────┘
```

**Hook Execution Points:**
- `before` hooks: Run in `ACTIVE` state, can veto
- `after` hooks: Run in `POST_HOOKS` state, for auditing
- `delta` hooks: Run on delta graph (additions/removals only)

### Content-Addressed Conditions

**Why Content-Addressed?**
- **Verification:** Same condition = same hash = can verify integrity
- **Deduplication:** Multiple hooks can reference same condition
- **Provenance:** Conditions are standalone, auditable artifacts
- **Sharing:** Conditions can be shared across systems

**Best Practice:**
```javascript
// ❌ Bad: Inline query
when: { kind: 'sparql-ask', query: 'ASK { ?s ?p ?o }' }

// ✅ Good: Content-addressed reference
when: {
  kind: 'sparql-ask',
  ref: {
    uri: 'file://hooks/compliance/large-tx.ask.rq',
    sha256: '...',
    mediaType: 'application/sparql-query'
  }
}
```

---

## Agent Integration Patterns

### Pattern 1: Capability Discovery

```javascript
// 1. Query capability registry
const capabilities = await system.query({
  query: 'SELECT ?cap WHERE { ?cap a cap:Capability }',
  type: 'sparql-select'
});

// 2. Verify preconditions
const verification = await verifyPreconditions(capabilityUri);

// 3. Invoke capability
if (verification.allSatisfied) {
  await invokeCapability(capabilityName, input);
}
```

### Pattern 2: MAPEK-Driven Auto-Fix

```javascript
// 1. Run MAPEK cycle
const result = await runMapekIteration(state);

// 2. Create hooks from findings
const hooks = createAutonomicHooks(result, projectStore);

// 3. Register hooks
for (const hook of hooks) {
  await system.registerHook(hook);
}

// 4. Hooks now auto-fix issues
```

### Pattern 3: Continuous Monitoring

```javascript
// Run continuous loop
const loop = await runContinuousMapekLoop({
  getState: async () => buildState(),
  applyActions: async (actions) => applyFixes(actions),
  intervalMs: 5000,
  maxIterations: 10
});
```

---

## Performance Characteristics

### SLOs (Service Level Objectives)

| Metric | Target | Unit |
|--------|--------|------|
| Hook registration | p99 < 1ms | milliseconds |
| Condition evaluation | p99 < 2ms | milliseconds |
| Hook execution | p99 < 10ms | milliseconds |
| MAPEK iteration | p99 < 200ms | milliseconds |
| Full pipeline | p99 < 50ms | milliseconds |

### 80/20 Optimization

- **Tier 1 (60% usage):** `defineHook`, `executeTransaction`, `query` - Use these first
- **Tier 2 (20% usage):** `registerHook`, `reason`, `canonicalize` - Specialized
- **Tier 3 (15% usage):** `ResolutionLayer`, `PolicyPackManager` - Advanced
- **Tier 4 (5% usage):** `FederatedQuery`, `StreamProcessor` - Experimental

---

## Error Handling and Recovery

### Error Categories

1. **Validation Errors:** Invalid input (recover by fixing input)
2. **Precondition Errors:** Requirements not met (recover by meeting requirements)
3. **Execution Errors:** Runtime failures (recover by retry or rollback)
4. **System Errors:** Infrastructure failures (recover by system restart)

### Recovery Strategies

- **Automatic Rollback:** Transactions automatically rollback on hook failure
- **Retry with Backoff:** Transient errors retry with exponential backoff
- **Graceful Degradation:** System continues operating with reduced functionality
- **Error Isolation:** Hook failures don't cascade to other hooks

---

## Security and Sandboxing

### Effect Sandboxing

Hooks execute in isolated sandboxes to prevent:
- File system access
- Network access
- Process spawning
- Memory leaks

### Content Verification

- Conditions verified via SHA-256 hash
- Hooks rejected if condition hash doesn't match
- Receipts provide cryptographic proof of execution

---

## Observability

### OpenTelemetry Integration

All operations create OTEL spans:
- Transaction execution
- Hook evaluation
- Hook execution
- MAPEK phases

### SPARQL-Based Debugging

Query system state via SPARQL:
```sparql
# Query recent hook executions
PREFIX otel: <urn:opentelemetry:>
SELECT ?spanName ?duration ?status WHERE {
  ?span otel:name ?spanName ;
        otel:duration ?duration ;
        otel:status ?status .
  FILTER(STRSTARTS(?spanName, "unrdf.hook."))
}
```

---

## Summary

UNRDF's architecture enables autonomic systems through:

1. **Knowledge Graph Substrate:** Single source of truth
2. **Content-Addressed Conditions:** Verifiable, shareable artifacts
3. **Declarative Policies:** SPARQL-based business logic
4. **MAPEK Loop:** Continuous self-management
5. **Knowledge Hooks:** Reactive, policy-driven execution

Agents integrate by:
- Discovering capabilities via SPARQL
- Verifying preconditions
- Executing MAPEK cycles
- Creating and registering hooks
- Monitoring system health

