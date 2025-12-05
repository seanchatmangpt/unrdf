# Getting Started with KGC 4D

**Your first 30 minutes with Hyperdimensional Knowledge Graph Composition**

Follow this guide step-by-step. You'll understand the core concepts and be ready for deeper work.

---

## What Is KGC 4D? (2 minutes)

KGC 4D combines three powerful concepts:

1. **RDF (Resource Description Framework)**: A way to represent knowledge as simple subject-predicate-object facts called "quads"
   - Example: `(Alice, knows, Bob, 2025-12-05)` = Alice knows Bob (timestamped)

2. **Hyperdimensional Information Theory (HDIT)**: Mathematical framework treating knowledge as multidimensional vectors
   - Enables: reasoning across temporal, semantic, and domain dimensions simultaneously

3. **Event Sourcing**: Complete history of all state changes
   - Benefit: Time-travel queries (reconstruct system state at ANY point in the past)

**Result**: A knowledge graph that you can query in the past, understand semantically, and reason about across multiple dimensions.

---

## Core Concepts (5 minutes)

### Quads
A "quad" is the basic unit: `(subject, predicate, object, context)`

```
Example quads:
(alice:person, rdf:type, foaf:Person, 2025-12-05)
(alice:person, foaf:name, "Alice Smith", 2025-12-05)
(alice:person, foaf:knows, bob:person, 2025-12-05)
```

### Event Log
Every change is captured as an immutable event:
```
Event 1: Add quad (alice, knows, bob)
Event 2: Add quad (alice, name, "Alice Smith")
Event 3: Remove quad (alice, knows, bob)
Event 4: Add quad (alice, knows, bob) [reinstated]
```

**Key**: You can always replay to see state at any point in time.

### Time-Travel Reconstruction
Recreate the knowledge graph at any historical point:
```
state_at(T=2025-01-01) → "At that time, Alice knew Bob"
state_at(T=2025-06-15) → "At that time, Alice did NOT know Bob"
state_at(T=2025-12-05) → "Now, Alice knows Bob"
```

---

## Your First Query (5 minutes)

### Step 1: Create a Store
```javascript
import { createStore, dataFactory } from '@unrdf/oxigraph';

const store = createStore();
```

### Step 2: Add Knowledge
```javascript
const namedNode = dataFactory.namedNode;
const literal = dataFactory.literal;

// Create facts (quads)
const quad1 = dataFactory.quad(
  namedNode("http://example.org/alice"),
  namedNode("http://xmlns.com/foaf/0.1/name"),
  literal("Alice Smith")
);

// Add to store
store.add(quad1);
```

### Step 3: Query Back
```javascript
// Find all names for Alice
const results = store.match(
  namedNode("http://example.org/alice"),
  namedNode("http://xmlns.com/foaf/0.1/name"),
  null  // Find any object
);

results.forEach(quad => {
  console.log(quad.object.value); // "Alice Smith"
});
```

### Step 4: Time-Travel
```javascript
// Reconstruct state at specific point
const pastState = await store.reconstructState(
  targetTime: new Date('2025-01-01')
);

const aliceAtThen = pastState.match(
  namedNode("http://example.org/alice"),
  null,
  null
);
```

---

## Key Benefits (3 minutes)

### 1. Complete History
- Never lose information (event log is immutable)
- Audit trail built-in
- Non-repudiation: Who did what, when?

### 2. Time-Travel Queries
- "What was the state on January 1st?" ✓
- "How did this knowledge evolve?" ✓
- "What changed on this date?" ✓

### 3. Semantic Reasoning
- Understand relationships across domains
- Compose knowledge from multiple sources
- 74 application patterns documented

### 4. Production-Ready
- 250/250 tests passing ✓
- OTEL validated 100/100 ✓
- 0 high-risk failure modes ✓

---

## Next 10 Minutes: Pattern Learning

Each of the **74 documented patterns** solves a real problem. Pick one that interests you:

### Quick Patterns (5 min each)
- **Temporal snapshot**: "Preserve state at key moments"
- **Domain projection**: "Extract knowledge relevant to one domain"
- **Semantic composition**: "Combine multiple knowledge sources"

### Medium Patterns (15 min each)
- **Cross-domain mapping**: "Link concepts across domains"
- **Time-series analysis**: "Track how facts change over time"
- **Immutable audit trail**: "Build compliance and non-repudiation"

### Advanced Patterns (30+ min each)
- **Dimensional reasoning**: "Reason across time, semantics, and domain simultaneously"
- **Conflict resolution**: "Handle conflicting facts from different sources"
- **Optimization**: "Scale time-travel to large datasets"

**Find patterns**: See `how-to/EXTRACTED-PATTERNS.md` (all 74)
**Learn implementations**: See `tutorials/PATTERN-IMPLEMENTATIONS.md`

---

## Validate Your Setup (3 minutes)

```bash
# Check that your environment is ready
npm test                # Should see: ✓ 250/250 passing
npm run validate:otel   # Should see: Score: 100/100
npm run bench:hooks     # Should see: Performance baseline
```

All pass? ✅ **You're ready for production!**

---

## Common First Questions

### "What's the difference between RDF and KGC 4D?"
- **RDF**: Language for representing facts
- **KGC 4D**: RDF + time-travel + semantic reasoning in 4D space

### "Do I need to know SPARQL?"
No. KGC 4D provides JavaScript API with simpler semantics.

### "Can I migrate from N3 store?"
Yes. See `tutorials/PATTERN-IMPLEMENTATIONS.md` for migration example.

### "What's the performance impact?"
Single hook: 33ms @ 10K operations (acceptable for <1K ops).
With optimization: 35% faster. See `BENCHMARKS.md` for details.

### "Is it production-ready?"
Yes. See `reference/FMEA-PRODUCTION.md` (0 high-risk, 24 guards).

---

## Your Next Steps (Pick One)

### Path 1: I Want to Deploy
1. Read: `QUICK-REFERENCE.md` (1 min decision)
2. Follow: `DEPLOYMENT-CHECKLIST.md` (2 hours verification)
3. Deploy with confidence ✓

### Path 2: I Want to Understand Theory
1. Read: `INSIGHTS.md` (key findings, 20 min)
2. Study: `explanation/kgc-4d-comprehensive.pdf` (107 pages, 3-4 hours)
3. Understand HDIT & event-sourcing ✓

### Path 3: I Want to Build Patterns
1. Browse: `how-to/EXTRACTED-PATTERNS.md` (find use case, 20 min)
2. Learn: `tutorials/PATTERN-IMPLEMENTATIONS.md` (code examples, 30 min)
3. Implement your first pattern ✓

### Path 4: I Want Hands-On Learning
1. Run: The code examples above
2. Experiment: Modify the quads, add more facts
3. Query: Try time-travel on your own data ✓

---

## Terminology Guide (Lookup as Needed)

Quick definitions (full glossary: `GLOSSARY.md`):

- **Quad**: Subject-predicate-object-context tuple (basic unit of knowledge)
- **RDF**: W3C standard for knowledge representation
- **Event Sourcing**: Store all state changes as immutable events
- **Time-Travel**: Reconstruct state at any historical point
- **HDIT**: Hyperdimensional Information Theory (mathematical framework)
- **Hook**: Validation/transformation applied to operations
- **Oxigraph**: RDF triple store (what we use)
- **OTEL**: OpenTelemetry (observability framework)
- **FMEA**: Failure Mode and Effects Analysis (risk assessment)

Full definitions: See `GLOSSARY.md`

---

## Common Errors & Quick Fixes

| Error | Cause | Fix |
|-------|-------|-----|
| "quad not found" | Subject/predicate mismatch | Check URI formatting |
| "time-travel failed" | No snapshots before time | Verify event log has events |
| "hook latency high" | Too many validation hooks | See `BENCHMARKS.md` optimization |
| "memory spike" | Large dataset loaded | Check memory alert settings |

**Full troubleshooting**: See `TROUBLESHOOTING.md`

---

## Knowledge Map (Where Everything Is)

```
START HERE (You are here)
├── README.md: Navigation hub
├── QUICK-REFERENCE.md: One-page decisions
├── INDEX.md: Search 150K+ words
│
├─ LEARN THEORY
│  ├── INSIGHTS.md: Key findings (20 min)
│  └── explanation/kgc-4d-comprehensive.pdf: Full paper (3-4 hours)
│
├─ DEPLOY
│  ├── DEPLOYMENT-CHECKLIST.md: Step-by-step (2 hours)
│  └── reference/FMEA-PRODUCTION.md: Risk verification (35 min)
│
├─ BUILD PATTERNS
│  ├── how-to/EXTRACTED-PATTERNS.md: Find use case (45 min)
│  └── tutorials/PATTERN-IMPLEMENTATIONS.md: Code examples (60 min)
│
├─ UNDERSTAND PERFORMANCE
│  ├── BENCHMARKS.md: Performance data (30 min)
│  └── QUICK-REFERENCE.md: Performance table (1 min)
│
└─ GET UNSTUCK
   ├── TROUBLESHOOTING.md: Common errors
   ├── FAQ.md: Quick answers
   └── GLOSSARY.md: Define terms
```

---

## Success Checkpoint (2 minutes)

You've made progress if you can answer:

- [ ] What is a quad? (subject-predicate-object-context tuple)
- [ ] What does event sourcing provide? (immutable history + time-travel)
- [ ] How do I create and query a store? (createStore → add → match)
- [ ] Where do I find patterns? (how-to/EXTRACTED-PATTERNS.md)
- [ ] What's the production status? (OTEL 100/100, 0 high-risk)

All checked? ✅ **You're ready to proceed to deeper topics.**

---

## Where to Get Help

| Question | Answer |
|----------|--------|
| "What's this term?" | See `GLOSSARY.md` |
| "How do I solve X?" | See `FAQ.md` + `how-to/EXTRACTED-PATTERNS.md` |
| "I got an error" | See `TROUBLESHOOTING.md` |
| "How do I deploy?" | See `DEPLOYMENT-CHECKLIST.md` |
| "What's the theory?" | See `explanation/kgc-4d-comprehensive.pdf` |
| "Performance question?" | See `BENCHMARKS.md` + `QUICK-REFERENCE.md` |
| "I'm lost" | See `INDEX.md` |

---

## 30-Second Summary

**KGC 4D** = RDF knowledge graph + time-travel + semantic reasoning

**Key benefit**: Query "What was true on January 1st?" at any time

**Get started**: Create store → add quads → query → time-travel

**Production-ready**: 250/250 tests ✓ OTEL 100/100 ✓ 0 high-risk ✓

**Next**: Pick a learning path above and follow it.

---

**Congratulations! You now understand KGC 4D.**

Next step: Pick one of the four learning paths above and follow it for 30-60 minutes.

---

Last updated: December 5, 2025 | Status: Ready ✅
