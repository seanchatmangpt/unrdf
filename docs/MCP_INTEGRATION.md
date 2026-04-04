# MCP Integration: Knowledge Self-Play Autonomics

> **Package:** `@unrdf/daemon` | **Since:** v26.4.4 | **Status:** Production

## What is Knowledge Self-Play Autonomics?

Knowledge Self-Play Autonomics is a closed-loop system where an RDF graph autonomously improves itself. Knowledge Hooks evaluate SPARQL conditions against the graph, fire SPARQL CONSTRUCT effects that mutate it, and the loop repeats until the graph reaches a stable state (convergence) or a maximum iteration count. Each iteration produces a cryptographic receipt, forming a tamper-evident chain of every change made.

Think of it as a "game" the knowledge graph plays against itself: the **state** is the RDF store, **moves** are hook effects, **decisions** are SPARQL conditions, **feedback** is the store delta, and the **goal** is convergence to a stable, self-consistent graph.

---

## How It Works

```
         Initial Graph
              |
              v
    +--------------------+
    | Evaluate Conditions |  <-- SPARQL ASK / SELECT on hook definitions
    +--------------------+
              |
         hooks match?
        /           \
      yes            no --> CONVERGED (stable state)
       |
       v
    +--------------------+
    | Fire Hook Effects   |  <-- SPARQL CONSTRUCT mutates the graph
    +--------------------+
              |
              v
    +--------------------+
    | Record Receipt      |  <-- input_hash vs output_hash
    +--------------------+
              |
              v
    +--------------------+
    | Check Convergence   |  <-- input_hash == output_hash? done.
    +--------------------+
              |
         max iterations?
        /           \
      no             yes --> STOPPED (force-stop)
       |
       v
      Loop back to Evaluate
```

**Key concepts:**

1. **State** -- The Oxigraph RDF store holds all triples
2. **Moves** -- Each hook fires a SPARQL CONSTRUCT effect that adds/removes triples
3. **Decision** -- SPARQL conditions in hook definitions determine which hooks fire
4. **Feedback** -- `input_hash !== output_hash` means the store changed (progress); equality means convergence
5. **Termination** -- The loop ends when either no hooks fire (converged) or `maxIterations` is reached
6. **History** -- Every step is recorded; the full episode can be materialized back into the store as RDF

---

## Architecture

The system is built around one class and one factory function:

| Export | Description |
|--------|-------------|
| `KnowledgeSelfPlayLoop` | Main class -- orchestrates the evaluate/fire/record cycle |
| `createKnowledgeSelfPlayLoop(store, engine, options?)` | Factory function -- convenience wrapper |

**Constructor options:**

| Option | Type | Default | Description |
|--------|------|---------|-------------|
| `store` | `Object` | *(required)* | Oxigraph RDF store instance |
| `engine` | `Object` | *(required)* | `KnowledgeHookEngine` with an `execute()` method |
| `maxIterations` | `number` | `10` | Maximum iterations before force-stop |
| `triggerType` | `string` | `'continuous-improvement'` | Hook trigger type to filter on |

---

## Quick Start

### Example 1: Basic Self-Play Loop

```javascript
import { createKnowledgeSelfPlayLoop } from '@unrdf/daemon/knowledge-self-play';

// Assumes you have an Oxigraph store and a KnowledgeHookEngine ready
const loop = createKnowledgeSelfPlayLoop(store, engine);

const result = await loop.run();

console.log(`Episode ${result.episodeId}`);
console.log(`Iterations: ${result.iterations}`);
console.log(`Converged: ${result.converged}`);
console.log(`Total feedback: ${result.totalFeedback}`);
```

### Example 2: Custom Configuration with Episode Materialization

```javascript
import { KnowledgeSelfPlayLoop } from '@unrdf/daemon/knowledge-self-play';

const loop = new KnowledgeSelfPlayLoop({
  store,
  engine,
  maxIterations: 20,
  triggerType: 'continuous-improvement',
});

// Run until convergence or 20 iterations
const result = await loop.run();

// Write episode metadata back into the RDF store as quads
const quads = loop.materializeEpisodeRDF(result);
console.log(`Wrote ${quads.length} episode quads to store`);

// Inspect step-by-step history
for (const step of loop.getHistory()) {
  console.log(`Step ${step.stepNumber}: feedback=${step.feedback}, changed=${step.storeChanged}`);
}
```

### Example 3: Manual Step-by-Step Control

```javascript
const loop = createKnowledgeSelfPlayLoop(store, engine, { maxIterations: 5 });

// Run steps manually for fine-grained control
let delta = {};
for (let i = 0; i < 5; i++) {
  const { receipt, feedback, storeChanged } = await loop.step(delta);
  console.log(`Step ${i}: feedback=${feedback}, changed=${storeChanged}`);

  if (!storeChanged) {
    console.log('Converged!');
    break;
  }

  delta = { previousReceipt: receipt, iterationNumber: i + 1 };
}
```

---

## API Reference

### `step(delta?)`

Execute one iteration of the self-play loop.

- **Parameters:** `delta` (Object, optional) -- context from previous iteration
- **Returns:** `Promise<{ receipt, feedback, storeChanged, hooksExecuted }>`

| Field | Type | Description |
|-------|------|-------------|
| `receipt` | `Object` | Cryptographic receipt with `input_hash`, `output_hash`, `receiptHash` |
| `feedback` | `number` | `0.1 * hooksExecuted` if store changed, `0` if converged |
| `storeChanged` | `boolean` | `true` if `input_hash !== output_hash` |
| `hooksExecuted` | `number` | Number of hooks that fired this step |

### `run(initialDelta?)`

Run the full loop until convergence or `maxIterations`.

- **Parameters:** `initialDelta` (Object, optional) -- initial context
- **Returns:** `Promise<Object>` with:

| Field | Type | Description |
|-------|------|-------------|
| `episodeId` | `string` | UUID identifying this run |
| `startTime` | `number` | Epoch ms when run started |
| `endTime` | `number` | Epoch ms when run ended |
| `iterations` | `number` | Total iterations executed |
| `receipts` | `Array` | Receipt from each iteration |
| `totalFeedback` | `number` | Sum of all step feedback values |
| `converged` | `boolean` | `true` if the loop ended by convergence |
| `avgFeedbackPerIteration` | `number` | `totalFeedback / iterations` |

### `materializeEpisodeRDF(runResult)`

Write episode metadata (type, iterations, converged, feedback, receipt chain) as RDF quads into the store.

- **Parameters:** `runResult` -- the object returned by `run()`
- **Returns:** `Array<Quad>` -- the quads written

Episode URI: `urn:unrdf:episode/{episodeId}`

### `getHistory()`

Returns a deep copy of the step history array. Each entry contains `stepNumber`, `timestamp`, `receipt`, `feedback`, `storeChanged`, and `hooksExecuted`.

### `getEpisodeId()`

Returns the UUID string for this episode.

### `getMetrics()`

Returns current episode metrics:

| Field | Type | Description |
|-------|------|-------------|
| `episodeId` | `string` | Episode UUID |
| `totalSteps` | `number` | Steps executed so far |
| `totalFeedback` | `number` | Cumulative feedback |
| `stepsConverged` | `number` | Steps where store did not change |
| `hooksExecutedTotal` | `number` | Total hooks fired across all steps |
| `avgHooksPerStep` | `number` | Average hooks per step |

---

## Configuration Guide

### Choosing `maxIterations`

| Use Case | Recommended | Why |
|----------|-------------|-----|
| Quick validation | `3-5` | Fast feedback, minimal compute |
| Standard improvement | `10` (default) | Good balance for most graphs |
| Deep optimization | `20-50` | Complex graphs with many hooks |

### Choosing `triggerType`

The `triggerType` filters which hooks are eligible to fire. Use different trigger types to run different improvement strategies:

- `'continuous-improvement'` (default) -- general-purpose graph optimization
- `'debug'` -- diagnostic hooks for troubleshooting
- Custom values matching your hook definitions

---

## Troubleshooting

**Loop never converges (hits maxIterations every time)**
- Check that your hooks produce a finite number of new triples. If hook A's output triggers hook B which triggers hook A, you have a cycle. Add guards (SPARQL `FILTER NOT EXISTS`) to prevent re-firing.

**Zero feedback / no hooks execute**
- Verify `triggerType` matches your hook definitions
- Check that the SPARQL conditions in your hooks match the current graph state
- Ensure your engine's `execute()` method returns `{ receipt, executionResults }` 

**`materializeEpisodeRDF()` quads not appearing in queries**
- Episode quads use the `urn:unrdf:episode/` namespace. Query with: `SELECT * WHERE { ?s ?p ?o . FILTER(STRSTARTS(STR(?s), "urn:unrdf:episode/")) }`
- If your store's `add()` throws, the error is logged as a warning but does not halt execution

**"store is required" / "engine is required" errors**
- Both `store` and `engine` are mandatory constructor arguments. The engine must have an `execute()` method.

---

## Related Documentation

- [packages/daemon/README.md](../packages/daemon/README.md) -- Daemon package overview
- [docs/ARCHITECTURE.md](ARCHITECTURE.md) -- System architecture
- [packages/hooks/README.md](../packages/hooks/README.md) -- Knowledge Hooks framework
