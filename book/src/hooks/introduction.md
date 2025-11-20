# Knowledge Hooks Introduction

Knowledge Hooks are the heart of UNRDF's autonomic capabilities, transforming your knowledge graph from a passive data store into an intelligent, self-governing system.

## What are Knowledge Hooks?

A **Knowledge Hook** is a policy-driven trigger that:

1. **Observes** - Monitors the knowledge graph for specific conditions
2. **Evaluates** - Checks if a condition is met using SPARQL, SHACL, or custom logic
3. **Reacts** - Executes effects when triggered (validate, transform, notify, veto)

```javascript
import { defineHook, registerHook } from 'unrdf';

const hook = defineHook({
  meta: {
    name: 'person-name-required',
    description: 'Ensures all persons have names'
  },
  when: {
    kind: 'sparql-ask',
    query: `
      PREFIX foaf: <http://xmlns.com/foaf/0.1/>
      ASK {
        ?person a foaf:Person .
        FILTER NOT EXISTS { ?person foaf:name ?name }
      }
    `
  },
  run: async (event) => {
    if (event.result === true) {
      throw new Error('All persons must have a name');
    }
  }
});

await registerHook(hook);
```

## Why Use Knowledge Hooks?

### Reactive RDF

Traditional RDF systems are **passive**:
- Store data
- Answer queries
- No proactive behavior

UNRDF with Knowledge Hooks is **reactive**:
- Enforces business rules automatically
- Maintains data quality invariants
- Responds to graph changes in real-time
- Provides self-healing capabilities

### Benefits

**🛡️ Data Quality Enforcement**
```javascript
// Automatically reject invalid data
defineHook({
  meta: { name: 'age-validation' },
  when: {
    kind: 'sparql-ask',
    query: 'ASK { ?person ex:age ?age . FILTER (?age < 0 || ?age > 150) }'
  },
  run: async (event) => {
    if (event.result) throw new Error('Invalid age detected');
  }
});
```

**📊 Audit Trails**
```javascript
// Log all person changes
defineHook({
  meta: { name: 'audit-person-changes' },
  when: {
    kind: 'delta',
    pattern: { predicate: 'rdf:type', object: 'foaf:Person' }
  },
  run: async (event) => {
    console.log(`Person modified: ${event.delta.additions.length} additions`);
  }
});
```

**⚡ Real-Time Notifications**
```javascript
// Alert on large transactions
defineHook({
  meta: { name: 'large-transaction-alert' },
  when: {
    kind: 'threshold',
    value: 10000,
    operator: 'gt'
  },
  run: async (event) => {
    await sendAlert(`Large transaction: ${event.value} triples`);
  }
});
```

**🔄 Automatic Transformations**
```javascript
// Normalize email addresses
defineHook({
  meta: { name: 'email-normalization' },
  when: {
    kind: 'delta',
    pattern: { predicate: 'foaf:mbox' }
  },
  run: async (event) => {
    // Transform email to lowercase
    return {
      result: 'normalized',
      assertions: [/* normalized triples */]
    };
  }
});
```

## Hook Architecture

Knowledge Hooks follow a **declarative, file-based** contract:

```
┌─────────────────────────────────────┐
│         Knowledge Hook              │
├─────────────────────────────────────┤
│  Meta                               │
│  • name: "compliance:largeTx"       │
│  • description: "..."               │
│  • ontology: ["fibo"]               │
├─────────────────────────────────────┤
│  When (Condition)                   │
│  • kind: "sparql-ask"               │
│  • ref: file://hooks/largeTx.rq     │
│  • sha256: "e3b0c44..."             │
├─────────────────────────────────────┤
│  Lifecycle                          │
│  • before(): gate/normalize         │
│  • run(): main effect               │
│  • after(): cleanup/audit           │
└─────────────────────────────────────┘
```

### Core Principles

**1. Conditions are Addressed, Not Embedded**

```javascript
// ❌ Bad: Inline query string
when: {
  kind: 'sparql-ask',
  query: 'ASK { ... }' // Hard to version and govern
}

// ✅ Good: Content-addressed file reference
when: {
  kind: 'sparql-ask',
  ref: {
    uri: 'file://hooks/compliance/largeTx.ask.rq',
    sha256: 'e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855',
    mediaType: 'application/sparql-query'
  }
}
```

**2. Reflex Arc Lifecycle**

Hooks follow a three-phase lifecycle inspired by biological reflexes:
- **Before**: Pre-condition gate (normalize payload, early cancellation)
- **Run**: Core effect execution
- **After**: Post-execution cleanup (audit, metrics)

**3. Effect Sandboxing**

All hook effects run in isolated sandboxes for security:
- Limited filesystem access
- No network access by default
- Resource constraints (memory, CPU)
- Timeout enforcement

**4. Cryptographic Provenance**

Every hook execution is logged with cryptographic proof:
- SHA3-256 hashing of hook definitions
- Git-based immutable receipts
- Merkle tree verification

## Use Cases

### 1. Compliance Enforcement

```javascript
// GDPR: Prevent storing personal data without consent
defineHook({
  meta: { name: 'gdpr:consent-required' },
  when: {
    kind: 'sparql-ask',
    query: `
      ASK {
        ?person a foaf:Person ;
                ex:personalData ?data .
        FILTER NOT EXISTS { ?person ex:hasConsent true }
      }
    `
  },
  run: async (event) => {
    if (event.result) {
      throw new Error('GDPR violation: Personal data requires consent');
    }
  }
});
```

### 2. Data Quality Gates

```javascript
// Ensure data completeness
defineHook({
  meta: { name: 'completeness:person-profile' },
  when: {
    kind: 'sparql-ask',
    query: `
      ASK {
        ?person a foaf:Person .
        FILTER NOT EXISTS {
          ?person foaf:name ?name ;
                  foaf:mbox ?email
        }
      }
    `
  },
  run: async (event) => {
    if (event.result) {
      throw new Error('Incomplete person profile: name and email required');
    }
  }
});
```

### 3. Anomaly Detection

```javascript
// Detect suspicious patterns
defineHook({
  meta: { name: 'security:unusual-activity' },
  when: {
    kind: 'window',
    duration: '5m'
  },
  run: async (event) => {
    if (event.count > 1000) {
      await sendSecurityAlert('Unusual activity detected');
    }
  }
});
```

### 4. Business Rule Automation

```javascript
// Auto-escalate high-value transactions
defineHook({
  meta: { name: 'business:auto-escalate' },
  when: {
    kind: 'threshold',
    value: 100000,
    operator: 'gte'
  },
  run: async (event) => {
    return {
      result: 'escalated',
      assertions: [
        quad(event.transaction, 'ex:requiresApproval', literal('true')),
        quad(event.transaction, 'ex:escalatedAt', literal(new Date().toISOString()))
      ]
    };
  }
});
```

## Comparison with Traditional Event Systems

| Feature | Traditional Events | Knowledge Hooks |
|---------|-------------------|----------------|
| **Condition Logic** | Imperative code | Declarative SPARQL/SHACL |
| **Provenance** | Manual logging | Cryptographic receipts |
| **Sandboxing** | None | Built-in isolation |
| **Graph-Native** | No | Yes (SPARQL queries) |
| **Versioning** | Code versions | Content-addressed files |
| **Audit Trails** | DIY | Automatic Merkle proofs |

## Getting Started

Continue to the next chapters to learn:

1. **[Hook Lifecycle](lifecycle.md)** - Understanding before/run/after phases
2. **[Predicates](predicates.md)** - Different condition types
3. **[Effects](effects.md)** - What hooks can do
4. **[Policy Packs](policy-packs.md)** - Organizing hooks into reusable packages

```admonish tip
For hands-on practice, see [Your First Hook](../getting-started/first-hook.md) to build a complete example.
```
