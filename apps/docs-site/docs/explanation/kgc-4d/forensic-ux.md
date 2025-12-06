# Forensic UX

**User Experience Patterns for Knowledge Construction**

Forensic UX applies principles from forensic science to user experience design in knowledge graph construction. It focuses on creating traceable, auditable, and explainable interfaces that preserve provenance and enable debugging.

## Core Principles

### 1. Traceable Interactions

Every user interaction leaves an immutable trace in the event log:

```javascript
// User clicks "Add Relationship"
Event {
  type: 'USER_ACTION',
  action: 'add_relationship',
  timestamp: 1701734400000000000n,
  user: 'alice@example.com',
  context: {
    from: 'alice',
    to: 'bob',
    relationship: 'knows'
  }
}
```

**Benefits**:
- Complete audit trail of all changes
- Ability to answer "Who made this change and why?"
- Support for compliance and regulatory requirements

### 2. Explainable State Changes

Users can see why the system is in its current state:

```javascript
// Show provenance chain
const provenance = await store.getProvenance(entityURI);

provenance.forEach(event => {
  console.log(`${event.user} performed ${event.action} at ${event.timestamp}`);
});
```

**UI Pattern**:
```
Current State: Alice knows Bob
├── Event 3 (2025-12-05 14:30): alice@example.com added relationship
├── Event 2 (2025-12-05 14:15): alice@example.com created Bob's profile
└── Event 1 (2025-12-05 14:00): alice@example.com created Alice's profile
```

### 3. Time-Travel Debugging

Users can "rewind" to see how data evolved:

```javascript
// Show state at different points in time
const timeline = [
  { time: '2025-01-01', state: await store.reconstructState(date1) },
  { time: '2025-06-01', state: await store.reconstructState(date2) },
  { time: '2025-12-01', state: await store.reconstructState(date3) },
];

// Visualize changes over time
timeline.forEach(snapshot => {
  renderState(snapshot.time, snapshot.state);
});
```

**UI Component**:
```
Timeline Scrubber
├─── Jan ──── Jun ──── Dec
│    ↓        ↓        ↓
│    State 1  State 2  State 3 (current)
└─ Drag to any point to see historical state
```

### 4. Mistake-Proofing (Poka-Yoke)

The UI prevents errors before they happen:

```javascript
// Validation happens before mutation
try {
  await store.addQuad(quad);
} catch (error) {
  if (error.code === 'DUPLICATE_QUAD') {
    showWarning('This relationship already exists');
  } else if (error.code === 'INVALID_URI') {
    showError('Invalid URI format');
  }
}
```

**24 Built-In Guards**:
- URI format validation
- Duplicate quad detection
- Type consistency checks
- Causality ordering enforcement
- Memory limits
- Clock jump detection
- And 18 more...

## UX Patterns

### Pattern 1: Confidence Indicators

Show users how certain the system is about data:

```javascript
const confidence = calculateConfidence(entity);

UI: "Alice knows Bob (95% confidence)"
    └─ Based on 3 corroborating sources
```

### Pattern 2: Provenance Overlay

Hover over any data to see its history:

```
[Hover: "Alice's age: 30"]

Tooltip:
├── Created: 2025-01-01 by alice@example.com
├── Modified: 2025-06-15 by bob@example.com (changed from 29 to 30)
└── Verified: 2025-12-01 by system (birthday validation)
```

### Pattern 3: Diff Visualization

Show changes between states:

```
Comparing 2025-01-01 vs 2025-12-01:

Added:
+ Alice knows Bob
+ Bob works at Company X

Removed:
- Alice works at Company Y

Modified:
~ Alice's age: 29 → 30
```

### Pattern 4: Undo/Redo with Provenance

Undo isn't just "go back" - it's adding a compensating event:

```javascript
// User clicks "Undo"
// Creates new event instead of deleting old one
await store.appendEvent({
  type: 'UNDO',
  compensates: previousEvent.id,
  reason: 'User requested undo'
});
```

**Result**: Full audit trail even of undos

## Implementation Examples

### Example 1: Entity Editor with Provenance

```jsx
function EntityEditor({ entityURI }) {
  const [entity, setEntity] = useState(null);
  const [provenance, setProvenance] = useState([]);

  useEffect(() => {
    const loadEntity = async () => {
      const currentState = await store.getEntity(entityURI);
      const history = await store.getProvenance(entityURI);

      setEntity(currentState);
      setProvenance(history);
    };

    loadEntity();
  }, [entityURI]);

  return (
    <div>
      <EntityView entity={entity} />
      <ProvenanceTimeline events={provenance} />
      <TimelineScrubbber onTimeChange={reconstructAtTime} />
    </div>
  );
}
```

### Example 2: Confidence-Based Rendering

```jsx
function FactDisplay({ fact }) {
  const confidence = calculateConfidence(fact);

  return (
    <div className={getConfidenceClass(confidence)}>
      {fact.label}
      <ConfidenceMeter value={confidence} />
      <ProvenanceButton onClick={() => showProvenance(fact)} />
    </div>
  );
}

function getConfidenceClass(confidence) {
  if (confidence > 0.9) return 'high-confidence';
  if (confidence > 0.7) return 'medium-confidence';
  return 'low-confidence';
}
```

### Example 3: Error Prevention UI

```jsx
function AddRelationshipForm() {
  const [from, setFrom] = useState('');
  const [to, setTo] = useState('');
  const [relationship, setRelationship] = useState('');
  const [validation, setValidation] = useState(null);

  useEffect(() => {
    // Real-time validation
    const validate = async () => {
      const result = await store.validateQuad(from, relationship, to);
      setValidation(result);
    };

    if (from && to && relationship) {
      validate();
    }
  }, [from, to, relationship]);

  return (
    <form>
      <input value={from} onChange={e => setFrom(e.target.value)} />
      <select value={relationship} onChange={e => setRelationship(e.target.value)}>
        <option value="knows">knows</option>
        <option value="worksAt">works at</option>
      </select>
      <input value={to} onChange={e => setTo(e.target.value)} />

      {validation?.errors && (
        <ErrorList errors={validation.errors} />
      )}

      {validation?.warnings && (
        <WarningList warnings={validation.warnings} />
      )}

      <button disabled={!validation?.valid}>Add Relationship</button>
    </form>
  );
}
```

## Best Practices

### 1. Show, Don't Hide Complexity

Users should understand what the system is doing:

**Bad**: "Processing..." (what's happening?)
**Good**: "Reconstructing state at 2025-01-01... 50% complete (replaying 5,000 events)"

### 2. Make History First-Class

Don't hide provenance in a dropdown:

**Bad**: Hidden "View History" button
**Good**: Timeline always visible, showing current position in history

### 3. Prevent Rather Than Correct

Use guards to prevent invalid states:

**Bad**: Let user create invalid data, then show error after save
**Good**: Real-time validation prevents form submission

### 4. Trust but Verify

Show confidence and sources:

**Bad**: "Alice is 30 years old" (how do we know?)
**Good**: "Alice is 30 years old (verified from 3 sources, 95% confidence)"

## Forensic UX Checklist

When designing a knowledge graph interface, ensure:

- [ ] Every user action creates an event (traceable)
- [ ] Users can see why data is in its current state (explainable)
- [ ] Users can time-travel to see historical states (debuggable)
- [ ] The UI prevents invalid operations (mistake-proof)
- [ ] Confidence indicators show data quality (transparent)
- [ ] Provenance is easily accessible (auditable)
- [ ] Undo/redo preserves full history (non-destructive)
- [ ] Real-time validation prevents errors (proactive)

## Related Patterns

- **Temporal Reconstruction**: How time-travel actually works
- **Event Sourcing**: Why every action is an immutable event
- **Poka-Yoke Guards**: The 24 mistake-proofing mechanisms
- **Vector Clocks**: How causality is tracked in distributed systems

---

**Learn More**:
- [KGC-4D Overview](./overview)
- [Why 4 Dimensions?](./four-dimensions)
- [Poka-Yoke Guards Reference](/docs/reference/api/guards)
