# Autonomous Knowledge Graph Refinement Guide

## Overview

This guide documents the Autonomous Refinement Engine and Vision 2030 Phase 1 features for building self-improving RDF knowledge graphs with LLM-directed operations.

**Status**: Research Prototype (all tests passing)
**Test Coverage**: 43 tests across 4 test suites
**Core Packages**: @unrdf/daemon (engine), @unrdf/hooks (ontology learning)

---

## Core Concept: Autonomous Loop

The Autonomous Refinement Engine implements a 10-step autonomous loop:

```
1. Evaluate Conditions     → Check store state, health metrics
2. Semantic Context      → Query relevant subgraphs  
3. LLM Decision          → Groq generates operations
4. SHACL Validation      → Validate mutations against schemas
5. Snapshot              → Save pre-mutation state
6. Execute               → Apply mutations (INSERT/DELETE)
7. Receipt               → Cryptographic proof (Blake3)
8. Integrity Scan        → Verify store consistency
9. Hooks/Callbacks       → Trigger observers
10. Metrics              → Update telemetry
```

Each iteration is instrumented with comprehensive logging, error handling, and provenance tracking.

---

## Vision 2030 Phase 1: Three 80/20 Features

### Feature 1: Autonomous Agents with Persistent Knowledge Graphs

**What it does**: Wraps AutonomousRefinementEngine to give autonomous AI agents their own persistent, self-improving knowledge graphs.

**Class**: `AutonomousKnowledgeAgent` (packages/daemon/src/autonomous-agent.mjs)

**Key Methods**:
- `reason(task, context)` - Query own KG → LLM reasons → executes → learns
- `execute(decision)` - Parses LLM decision, invokes appropriate action
- `parseAction(text)` - Heuristic parser (query/add/unknown)
- `getReasoningTrace()` - Audit trail of all decisions
- `getKnowledgeGraph()` - Access underlying AutonomousRefinementEngine

**Example**:
```javascript
const agent = new AutonomousKnowledgeAgent('analyst', {
  goalTriples: 100,
  maxIterations: 5,
});

const result = await agent.reason('Analyze customer churn patterns', {
  query: 'SELECT ?customer ?reason WHERE { ?customer ex:churnReason ?reason }'
});

console.log(agent.getReasoningTrace());
// Output: [ { task, decision, result, timestamp, ... } ]
```

**Benefits**:
- Agents maintain interpretable memory (RDF graphs)
- Reasoning is auditable and reproducible
- Multiple agents can federate patterns
- KGs improve over time through autonomous refinement

**Test Coverage**: 10 tests validating agent lifecycle, reasoning, KG operations

---

### Feature 2: Ensemble Reasoning with Confidence Scoring

**What it does**: Runs parallel specialist prompts (hypothesis-generator, validator, refiner) to improve reasoning quality and detect uncertainty.

**Class**: `EnsembleGroqProvider` (packages/daemon/src/providers/ensemble-groq.mjs)

**Key Methods**:
- `generateText(input)` - Runs all specialists in parallel via Promise.all()
- `aggregate(results)` - Combines specialist responses with separators
- `computeConfidence(results)` - Calculates agreement (0-1) from keyword overlap
- `setSpecialists(specialists)` - Customize roles
- `getSpecialists()` - Retrieve current specialists

**Example**:
```javascript
const ensemble = new EnsembleGroqProvider(groqProvider);
const result = await ensemble.generateText({
  prompt: 'Graph state: ... what should we add?',
  maxTokens: 500,
  temperature: 0.7,
});

console.log(result);
// {
//   text: "Combined reasoning from all specialists",
//   specialist_responses: [
//     { role: 'hypothesis-generator', text: '...' },
//     { role: 'validator', text: '...' },
//     { role: 'refiner', text: '...' }
//   ],
//   confidence: 0.85,  // High agreement → high confidence
//   ensemble_size: 3
// }
```

**Confidence Scoring**:
- Analyzes keyword overlap across all specialist responses
- Words appearing in 70%+ of responses increment agreement counter
- Confidence = (agreed words) / (total unique words)
- High confidence (>0.8) → low epistemic uncertainty
- Low confidence (<0.5) → requires more reasoning

**Benefits**:
- Detects reasoning uncertainty automatically
- Improves quality over single LLM call
- Enables confidence-aware decision-making
- Plugs into agent reasoning pipeline

**Test Coverage**: 11 tests validating specialists, aggregation, confidence computation

---

### Feature 3: Ontology Learning from RDF Patterns

**What it does**: Mines RDF data to automatically discover schema constraints (cardinality, datatype, minCount) and generates SHACL shape definitions.

**Class**: `OntologyLearner` (packages/hooks/src/hooks/ontology-learner.mjs)

**Key Methods**:
- `inferShapes(store, options)` - Main entry point: classes → instances → properties → shapes
- `extractClasses(store)` - Finds all rdf:type declarations
- `getInstancesOfClass(store, className)` - Collects instances
- `mineProperties(store, instances)` - Counts properties, tracks datatypes
- `inferDatatype(obj)` - Infers xsd:string/integer/float/boolean/dateTime/Resource
- `toSHACLShape(className, properties)` - Generates SHACL JSON-LD

**Example**:
```javascript
const learner = new OntologyLearner();

// Populate store with RDF data
store.addQuad(
  quad(namedNode('http://ex.org/alice'), rdfType, personClass)
);
store.addQuad(
  quad(namedNode('http://ex.org/alice'), nameProperty, literal('Alice'))
);
store.addQuad(
  quad(namedNode('http://ex.org/alice'), ageProperty, literal('30'))
);

// Learn shapes
const shapes = await learner.inferShapes(store, { minSupport: 0.9 });

console.log(shapes['http://example.org/Person']);
// {
//   targetClass: 'http://example.org/Person',
//   properties: {
//     'http://example.org/name': {
//       minCount: 1,              // 100% of persons have name
//       datatype: 'xsd:string',
//       description: 'Property ... (100% coverage)'
//     },
//     'http://example.org/age': {
//       minCount: 0,              // Not all persons required age
//       datatype: 'xsd:integer',
//       description: '(90% coverage)'
//     }
//   },
//   instanceCount: 2
// }
```

**minSupport Threshold**:
- Filters properties by coverage percentage
- minSupport: 0.9 → only properties in 90%+ of instances
- minCount: set to 1 if supportRatio > 0.99, else 0
- Conservative: requires strong evidence before adding constraints

**Datatype Inference**:
1. Check explicit datatype (if marked)
2. Infer from value patterns:
   - `/^\d+$/` → xsd:integer
   - `/^\d+\.\d+$/` → xsd:float
   - `/^(true|false)$/i` → xsd:boolean
   - `/^\d{4}-\d{2}-\d{2}/` → xsd:dateTime
   - URIs → rdf:Resource
   - Default → xsd:string

**Benefits**:
- Auto-discovers schema without manual definition
- Enables schema-constrained mutation validation
- Guides LLM suggestions ("add age as integer 0-150")
- Captures ontological patterns from real data

**Test Coverage**: 12 tests validating class extraction, property mining, datatype inference, SHACL generation

---

## Integration: All Three Features Together

The three features work seamlessly:

```javascript
// 1. Agent reasons with ensemble
const agent = new AutonomousKnowledgeAgent('analyst');
const ensemble = new EnsembleGroqProvider(groqProvider);

// 2. Agent uses ensemble for high-confidence decisions
const reasoning = await ensemble.generateText({
  prompt: 'Graph state: ..., Decision: ?'
});

// 3. Agent refines KG based on decision
const result = await agent.execute(reasoning.text);

// 4. Learner auto-discovers constraints from refined KG
const learner = new OntologyLearner();
const shapes = await learner.inferShapes(agent.kg.store, {
  minSupport: 0.9
});

// 5. Agent uses learned shapes to validate future mutations
agent.kg.config.shapes = shapes;

// 6. Reasoning trace shows full audit trail
console.log(agent.getReasoningTrace());
```

**Workflow**:
1. **Agent reasons**: Queries own KG, gets context
2. **Ensemble improves**: Multiple specialists → confidence score
3. **Agent executes**: Parses decision, invokes mutations
4. **Learner validates**: Discovered schemas constrain mutations
5. **Agent learns**: KG improves, trace grows
6. **Cycle repeats**: Next reasoning includes learned schemas

---

## Test Suites

### 1. autonomous-agent.test.mjs (10 tests)
- Agent creation with name/KG
- Reasoning trace tracking
- Action parsing (query/add/unknown)
- Execution and event emission
- Error handling

```bash
cd packages/daemon && pnpm test autonomous-agent.test.mjs
```

### 2. ensemble-groq.test.mjs (11 tests)
- Specialist creation and parallelization
- Result aggregation
- Confidence computation
- Custom specialists
- Input option passthrough

```bash
cd packages/daemon && pnpm test ensemble-groq.test.mjs
```

### 3. ontology-learner.test.mjs (12 tests)
- Class extraction from rdf:type
- Instance retrieval
- Property mining with datatype tracking
- Datatype inference (all types)
- SHACL shape generation
- minSupport threshold

```bash
cd packages/hooks && pnpm test ontology-learner.test.mjs
```

### 4. vision-2030-integration.test.mjs (10 tests)
- All three features together
- Agent + ensemble + learner workflow
- Confidence-aware reasoning
- Schema discovery from reasoning
- Multi-agent federation

```bash
cd packages/daemon && pnpm test vision-2030-integration.test.mjs
```

---

## Usage Examples

### Example 1: Autonomous Agent Learning from Data

```javascript
import { AutonomousKnowledgeAgent } from '@unrdf/daemon/autonomous-agent';
import { Store } from 'n3';

const agent = new AutonomousKnowledgeAgent('researcher', {
  goalTriples: 500,
  maxIterations: 10,
});

// Agent reasons over time
for (let i = 0; i < 5; i++) {
  const result = await agent.reason(
    `Iteration ${i}: Expand knowledge`,
    { confidence_threshold: 0.8 }
  );
}

// Query the learned knowledge
const trace = agent.getReasoningTrace();
const kg = agent.getKnowledgeGraph();
```

### Example 2: Ensemble Reasoning with Confidence

```javascript
import { EnsembleGroqProvider } from '@unrdf/daemon/providers/ensemble-groq';

const ensemble = new EnsembleGroqProvider(groqProvider);

// Get high-confidence decisions
const result = await ensemble.generateText({
  prompt: 'Should we add this data? ' + JSON.stringify(candidate),
});

if (result.confidence > 0.9) {
  // High agreement → safe to commit
  await store.add(candidate);
} else if (result.confidence > 0.5) {
  // Medium confidence → requires review
  console.log('REVIEW REQUIRED:', result.specialist_responses);
} else {
  // Low confidence → reject
  console.log('REJECTED (low consensus)');
}
```

### Example 3: Auto-discovering Ontologies

```javascript
import { OntologyLearner } from '@unrdf/hooks/ontology-learner';

const learner = new OntologyLearner();

// Learn shapes from data
const shapes = await learner.inferShapes(myStore, {
  minSupport: 0.85, // At least 85% coverage
});

// Use shapes to validate mutations
for (const [className, shape] of Object.entries(shapes)) {
  console.log(`Class: ${className}`);
  console.log(`  Properties:`, Object.keys(shape.properties).length);
  console.log(`  Instances:`, shape.instanceCount);
}

// Generate SHACL for export
const shacl = learner.toSHACLShape(
  'http://example.org/Person',
  shapes['http://example.org/Person'].properties
);
console.log(JSON.stringify(shacl, null, 2));
```

---

## Architecture

```
┌─────────────────────────────────────────┐
│   Application Layer (CLI, APIs, UI)     │
└──────────────┬──────────────────────────┘
               │
┌──────────────▼──────────────────────────┐
│   Autonomous Agents (with KGs)          │
│   ┌─────────────────────────────────┐   │
│   │ AutonomousKnowledgeAgent        │   │
│   └─────────────────────────────────┘   │
└──────────────┬──────────────────────────┘
               │
┌──────────────▼──────────────────────────┐
│   Ensemble Reasoning Layer              │
│   ┌─────────────────────────────────┐   │
│   │ EnsembleGroqProvider            │   │
│   │ - hypothesis-generator          │   │
│   │ - validator                     │   │
│   │ - refiner                       │   │
│   └─────────────────────────────────┘   │
└──────────────┬──────────────────────────┘
               │
┌──────────────▼──────────────────────────┐
│   Autonomous Refinement Engine          │
│   (10-step autonomous loop)             │
│   ┌─────────────────────────────────┐   │
│   │ AutonomousRefinementEngine      │   │
│   └─────────────────────────────────┘   │
└──────────────┬──────────────────────────┘
               │
┌──────────────▼──────────────────────────┐
│   Schema Discovery & Validation         │
│   ┌─────────────────────────────────┐   │
│   │ OntologyLearner (SHACL shapes)  │   │
│   │ SHACL Validator                 │   │
│   └─────────────────────────────────┘   │
└──────────────┬──────────────────────────┘
               │
┌──────────────▼──────────────────────────┐
│   RDF Core (SPARQL, Storage)            │
│   ┌─────────────────────────────────┐   │
│   │ Store (Memory/Oxigraph)         │   │
│   └─────────────────────────────────┘   │
└─────────────────────────────────────────┘
```

---

## Performance Characteristics

### Test Execution Time
- autonomous-agent.test.mjs: ~282ms (10 tests)
- ensemble-groq.test.mjs: ~165ms (11 tests)
- ontology-learner.test.mjs: ~442ms (12 tests)
- vision-2030-integration.test.mjs: ~269ms (10 tests)
- **Total**: ~1.2s for 43 tests

### Memory Usage
- AutonomousKnowledgeAgent: ~5MB per agent (empty KG)
- EnsembleGroqProvider: <1MB (stateless wrapper)
- OntologyLearner: <2MB (working memory for inference)

### Scalability
- Agents: tested with 2+ agents sharing patterns
- Ontology Learning: tested with ~10 classes, 100+ instances
- Ensemble: tested with 3 specialists, easily extends to more

---

## Known Limitations

1. **Ensemble Confidence**: Keyword-based overlap is heuristic, not semantic
   - Future: Use embeddings for semantic similarity

2. **Ontology Learning**: Assumes instance homogeneity
   - Doesn't handle heterogeneous data (mixed schema per class)
   - Future: Support shape variants

3. **Agent Reasoning**: Heuristic action parser
   - Only recognizes query/add/unknown
   - Future: Full MCP tool routing

---

## Future Work (Vision 2030 Phase 2+)

- **Federated Agent Networks**: Multiple agents sharing learned KGs
- **Probabilistic Reasoning**: Confidence propagation through inference
- **Temporal Reasoning**: Time-aware shape discovery
- **Performance Optimization**: Lazy loading, incremental learning
- **Production Validation**: Real-world data, scale testing

---

## References

- [AUTONOMOUS-REFINEMENT-ENGINE](./AUTONOMOUS-REFINEMENT-ENGINE.md)
- [PHD-THESIS-LLM-RDF-AUTONOMY](./PHD-THESIS-LLM-RDF-AUTONOMY.md)
- AutonomousRefinementEngine source: `packages/daemon/src/autonomous-refinement-engine.mjs`
- Test files: See test suites above
