# Admission Engine Quick Start

## 5-Minute Quick Start

### 1. Import the Engine

```javascript
import { AdmissionEngine, DeltaCapsule } from './admission-engine.mjs';
```

### 2. Create an Engine Instance

```javascript
const engine = new AdmissionEngine({
  strictMode: true,    // Fail on any violation
  auditLog: true       // Enable logging
});
```

### 3. Create a Delta Capsule

```javascript
const delta = new DeltaCapsule({
  partition: {
    namespace: 'http://example.org/my-overlay/',
    name: 'my-changes'
  },
  changes: [{
    operation: 'add',
    quads: [
      {
        subject: { termType: 'NamedNode', value: 'http://example.org/my-overlay/Person1' },
        predicate: { termType: 'NamedNode', value: 'http://example.org/my-overlay/hasName' },
        object: { termType: 'Literal', value: 'Alice' }
      }
    ]
  }],
  invariants: [
    { name: 'Q_typing', enabled: true },
    { name: 'Q_noncollision', enabled: true },
    { name: 'Q_monotone', enabled: true },
    { name: 'Q_provenance', enabled: true }
  ],
  provenance: {
    agent: 'user-123',
    timestamp: new Date().toISOString()
  }
});
```

### 4. Submit for Admission

```javascript
const decision = await engine.admitCapsule(delta);

if (decision.allowed) {
  console.log('✅ ALLOWED:', decision.reason);
  // Apply changes to store
} else {
  console.log('❌ DENIED:', decision.reason);
  console.log('Blocked by:', decision.checks.forbiddenOperations.blockedBy);
  console.log('Failed invariants:', decision.checks.invariants.failedInvariants);
}
```

## Common Patterns

### Pattern 1: Validate Before Applying

```javascript
// Dry-run first
const validation = await engine.validateCapsule(delta);

if (validation.valid) {
  // Actually submit
  const decision = await engine.admitCapsule(delta);
  applyChanges(delta);
}
```

### Pattern 2: Batch Processing

```javascript
const deltas = [delta1, delta2, delta3];
const results = await engine.admitBatch(deltas);

const allowed = results.filter(r => r.allowed);
console.log(`Allowed: ${allowed.length}/${results.length}`);
```

### Pattern 3: Check Statistics

```javascript
const stats = engine.getStats();
console.log(`Allow rate: ${stats.allowRate}`);
console.log(`Total processed: ${stats.totalProcessed}`);
```

## What Gets DENIED?

1. **Editing protected namespaces** (RDF, RDFS, OWL)
2. **Redefining canonical terms** (rdf:type, rdfs:Class)
3. **Namespace collisions** (creating owl:myClass)
4. **Invalid RDF syntax** (literal as subject)
5. **Missing provenance** (no agent or timestamp)
6. **Too many quads** (>10,000 by default)

See [DECISION-MATRIX.md](./DECISION-MATRIX.md) for complete examples.

## File Locations

```
/home/user/unrdf/src/admission/
├── delta-capsule.mjs           # Δ capsule class
├── forbidden-operations.mjs    # Guards
├── invariants.mjs              # Invariants
├── admission-engine.mjs        # Main engine
├── README.md                   # Full documentation
├── DECISION-MATRIX.md          # Decision examples
└── QUICKSTART.md               # This file
```

## Next Steps

1. Read [DECISION-MATRIX.md](./DECISION-MATRIX.md) for decision examples
2. Read [README.md](./README.md) for complete API reference
3. Run tests: `node run-tests.mjs`
4. Integrate with your RDF store

**Happy gating! 🎛️**
