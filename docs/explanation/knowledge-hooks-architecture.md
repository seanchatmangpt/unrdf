# Understanding Knowledge Hooks Architecture

Deep explanation of how Knowledge Hooks transform static knowledge graphs into reactive systems.

## The Problem: Static Knowledge Graphs

Traditional RDF applications treat knowledge graphs as **passive data stores**:

```
Application -> Query -> Graph -> Results
              (pull)
```

To react to changes, developers build custom infrastructure:

- Event buses for change detection
- Polling loops to check for updates
- Manual audit trail implementation
- Scattered validation logic

This creates **80% "dark matter"** - glue code that doesn't provide direct business value.

## The Solution: Declarative Reactivity

Knowledge Hooks invert the model - the graph **pushes** to your code:

```
Graph Change -> Hook Condition -> Execution -> Receipt
               (declarative)    (your code)  (audit)
```

### Core Design Principles

1. **Declarative Triggers** - Define *what* to monitor, not *how*
2. **Content-Addressed Conditions** - Verifiable, standalone governance artifacts
3. **Lifecycle Functions** - before/run/after reflex arc
4. **Cryptographic Receipts** - Every evaluation is auditable

## Architecture Overview

```
┌─────────────────────────────────────────────────────────────────┐
│                        Knowledge Graph                          │
│                                                                 │
│  ┌─────────────┐    ┌─────────────┐    ┌─────────────┐        │
│  │  Graph A    │    │  Graph B    │    │  Graph C    │        │
│  │  (Delta)    │    │  (State)    │    │  (Shapes)   │        │
│  └──────┬──────┘    └──────┬──────┘    └──────┬──────┘        │
│         │                  │                  │                │
│         └──────────────────┴──────────────────┘                │
│                            │                                    │
│                            ▼                                    │
│  ┌─────────────────────────────────────────────────────────┐   │
│  │                   Hook Evaluation Engine                 │   │
│  │  ┌──────────┐  ┌──────────┐  ┌──────────┐              │   │
│  │  │ Condition│  │ Condition│  │ Condition│              │   │
│  │  │ (SPARQL) │  │ (SHACL)  │  │ (SPARQL) │              │   │
│  │  └────┬─────┘  └────┬─────┘  └────┬─────┘              │   │
│  │       │             │             │                      │   │
│  │       ▼             ▼             ▼                      │   │
│  │  ┌──────────────────────────────────────────────────┐   │   │
│  │  │              Lifecycle Executor                   │   │   │
│  │  │  before() → run() → after()                      │   │   │
│  │  └──────────────────────────────────────────────────┘   │   │
│  └─────────────────────────────────────────────────────────┘   │
│                            │                                    │
│                            ▼                                    │
│  ┌─────────────────────────────────────────────────────────┐   │
│  │                    Receipt Anchor                        │   │
│  │  (git-notes, blockchain, audit log)                     │   │
│  └─────────────────────────────────────────────────────────┘   │
└─────────────────────────────────────────────────────────────────┘
```

## The 80/20 Contract

The hook contract is designed for 80% of use cases with minimal configuration:

```javascript
defineHook({
  // Identity & Discovery
  meta: { name, description, ontology },

  // Observation Scope
  channel: { graphs, view },

  // Trigger Condition (Content-Addressed)
  when: { kind, ref: { uri, sha256, mediaType } },

  // Operational Configuration
  determinism: { seed },
  receipt: { anchor },

  // Lifecycle Functions
  before,   // Pre-gate
  run,      // Core logic
  after     // Post-audit
});
```

### Why This Structure?

| Component | FMEA Failure Mode Addressed | TRIZ Principle Applied |
|-----------|----------------------------|------------------------|
| Content-addressed `when` | Tampering with conditions | Separation (condition != implementation) |
| `channel.view` | Wrong graph state evaluated | Prior action (specify view upfront) |
| `determinism.seed` | Non-reproducible behavior | Parameter changes (explicit randomness control) |
| `receipt.anchor` | Missing audit trail | Feedback (automatic provenance) |
| Lifecycle separation | Monolithic, hard-to-test logic | Segmentation (before/run/after) |

## Content-Addressed Conditions

Conditions are **external, verifiable artifacts**, not inline strings:

```javascript
when: {
  kind: 'sparql-ask',
  ref: {
    uri: 'file://hooks/compliance/large-transaction.rq',
    sha256: 'e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855',
    mediaType: 'application/sparql-query'
  }
}
```

### Why Not Inline Queries?

| Approach | Problem | Risk |
|----------|---------|------|
| Inline strings | Query changes without version control | Audit failure |
| Inline strings | No integrity verification | Tampering |
| Inline strings | Copy-paste duplication | Inconsistency |

### Benefits of Content-Addressing

1. **Version Control** - Queries live in git
2. **Integrity** - SHA-256 prevents tampering
3. **Reuse** - Same query used by multiple hooks
4. **Auditing** - Exact condition is recorded in receipt

## The Reflex Arc Lifecycle

Hooks follow an **autonomic reflex arc** pattern:

```
Stimulus → Afferent → Processing → Efferent → Response
(Change)   (before)    (run)       (after)   (Result)
```

### before() - Afferent Gate

Pre-execution validation and transformation:

```javascript
async before({ payload, context }) {
  // 1. Validate input
  if (!payload?.transactionId) {
    return { cancel: true, reason: 'Missing transactionId' };
  }

  // 2. Enrich payload
  return {
    ...payload,
    enrichedAt: Date.now(),
    sessionId: context.env.sessionId
  };
}
```

**Responsibilities:**

- Input validation
- Payload transformation
- Early cancellation
- Context enrichment

### run() - Core Processing

Main business logic:

```javascript
async run({ payload, context }) {
  const analysis = await analyzeTransaction(payload);

  return {
    result: { status: analysis.passed ? 'approved' : 'flagged' },
    assertions: [
      quad(subject, predicate, object)  // Optional graph updates
    ]
  };
}
```

**Responsibilities:**

- Business logic execution
- Result generation
- Optional graph assertions

### after() - Efferent Cleanup

Post-execution handling:

```javascript
async after({ result, cancelled, reason }) {
  if (cancelled) {
    await logCancellation(reason);
    return { result: { finalStatus: 'cancelled' } };
  }

  await notifyStakeholders(result);
  return { result: { finalStatus: 'completed' } };
}
```

**Responsibilities:**

- Success/failure handling
- Notification/alerting
- Cleanup operations
- Final result transformation

## Graph Views

The `channel.view` property determines which graph state is evaluated:

| View | Description | Use Case |
|------|-------------|----------|
| `before` | Graph state before change | Rollback prevention |
| `after` | Graph state after change | Final validation |
| `delta` | Only added/removed triples | Change detection |

### Delta View Example

```
Original Graph:        Delta Graph:
  A knows B              + C knows D  (addition)
  A knows C              - A knows C  (removal)

After Graph:
  A knows B
  C knows D
```

Hook with `view: 'delta'` only sees the changes, not existing data.

## Receipt Anchoring

Every hook evaluation can produce a **cryptographic receipt**:

```javascript
{
  hookId: 'compliance:large-transaction',
  executedAt: '2024-01-15T10:30:00.000Z',
  conditionUri: 'file://hooks/compliance/large-transaction.rq',
  conditionHash: 'sha256:e3b0c44...',
  inputHash: 'sha256:abc123...',
  outputHash: 'sha256:def456...',
  cancelled: false,
  signature: 'ed25519:...'
}
```

### Anchoring Strategies

| Strategy | Description | Trust Model |
|----------|-------------|-------------|
| `git-notes` | Store in git notes | Version control history |
| `none` | No anchoring | Trust application logic |

Future anchoring options could include blockchain, timestamping authorities, etc.

## Determinism Control

Hooks can involve randomness (sampling, probabilistic algorithms). The `determinism.seed` ensures reproducibility:

```javascript
determinism: { seed: 42 }

// In run():
const random = seededRandom(context.seed);
const sample = random() < 0.1;  // Reproducible sampling
```

**Why determinism matters:**

- Reproducible audits
- Consistent behavior across environments
- Predictable testing

## Security Considerations

### Sandbox Execution

Hook functions execute in a controlled environment:

- No direct filesystem access
- No network access (unless explicitly granted)
- Resource limits (memory, CPU time)
- Isolated from other hooks

### Condition Integrity

SHA-256 verification prevents tampering:

```javascript
// At execution time
const actualHash = sha256(conditionFileContent);
if (actualHash !== hook.when.ref.sha256) {
  throw new SecurityError('Condition file tampered');
}
```

## Performance Characteristics

### Evaluation Pipeline

```
Condition Check    Hook Execution    Receipt Writing
   ~1-10ms           ~10-100ms          ~1-5ms
     │                   │                 │
     ▼                   ▼                 ▼
   SPARQL/SHACL    User-defined code   Git/Storage
```

### Optimization Strategies

1. **Selective graphs** - Only observe relevant named graphs
2. **Delta view** - Skip unchanged data
3. **Condition caching** - Cache parsed SPARQL/SHACL
4. **Batch evaluation** - Group related hooks

## Design Patterns

### Policy Pack Pattern

Group related hooks into reusable packs:

```javascript
const compliancePack = {
  hooks: [
    largeTransactionHook,
    suspiciousPatternHook,
    regulatoryReportingHook
  ],
  shapes: complianceShapes,
  config: { threshold: 10000 }
};
```

### Pipeline Pattern

Chain hooks for multi-stage processing:

```javascript
const pipeline = [
  validateHook,      // Stage 1: Validate
  enrichHook,        // Stage 2: Enrich
  classifyHook,      // Stage 3: Classify
  persistHook        // Stage 4: Persist
];
```

### Saga Pattern

Long-running workflows with compensation:

```javascript
const transactionSaga = defineHook({
  async run({ payload, context }) {
    try {
      await step1();
      await step2();
      await step3();
    } catch (error) {
      await compensate();
      throw error;
    }
  }
});
```

## Summary

Knowledge Hooks provide:

1. **Declarative reactivity** - What, not how
2. **Verifiable conditions** - Content-addressed governance
3. **Structured lifecycle** - before/run/after reflex arc
4. **Cryptographic audit** - Every evaluation recorded
5. **Deterministic execution** - Reproducible behavior

This architecture eliminates 80% of RDF application "dark matter" by providing reactive infrastructure as a first-class feature.

## Related

- [Knowledge Hooks Tutorial](../tutorials/knowledge-hooks.md) - Hands-on guide
- [Defining Hooks Guide](../guides/defining-hooks.md) - How-to reference
- [System Design](./system-design.md) - Overall architecture
