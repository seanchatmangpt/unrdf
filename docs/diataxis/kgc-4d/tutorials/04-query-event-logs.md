# Tutorial: Query Event Logs with SPARQL

**Objective:** Learn how to query immutable event history using SPARQL on the EventLog graph.

**Time:** 15 minutes  
**Level:** Intermediate

---

## Prerequisites

**Capabilities Needed:**
- Capability: "SPARQL query on EventLog graph"
- Capability: "Immutable event history"
- Capability: "Event metadata (type, timestamp, payload)"

**Prerequisites:**
- [Tutorial 02: Create and Freeze Universe](./02-create-freeze-universe.md)
- Basic SPARQL syntax (helpful)

---

## What You'll Learn

1. Query events from the EventLog graph
2. Filter by event type, timestamp, and payload
3. Sort events chronologically
4. Aggregate event statistics

---

## Step 1: Understand the EventLog Graph

```javascript
import { GRAPHS, EVENT_TYPES, PREDICATES } from '@unrdf/kgc-4d';

console.log('EventLog graph:', GRAPHS.EVENT_LOG);
// Output: http://kgc.io/EventLog

console.log('Event types:', EVENT_TYPES);
// Output: { CREATE: 'CREATE', UPDATE: 'UPDATE', DELETE: 'DELETE', SNAPSHOT: 'SNAPSHOT' }

console.log('Predicates:', PREDICATES);
// Output: { TYPE: 'http://kgc.io/type', T_NS: 'http://kgc.io/t_ns', ... }
```

**EventLog Structure:**
Each event is an RDF resource with predicates for type, timestamp, payload, etc.

---

## Step 2: Simple Event Query

```javascript
import { KGCStore, GRAPHS } from '@unrdf/kgc-4d';

const store = new KGCStore();

// Query all events
const results = await store.queryEventLog(`
  PREFIX kgc: <http://kgc.io/>
  SELECT ?event ?type ?timestamp
  WHERE {
    GRAPH <${GRAPHS.EVENT_LOG}> {
      ?event kgc:type ?type .
      ?event kgc:t_ns ?t_ns .
    }
  }
  ORDER BY ?t_ns
`);

results.forEach((result, i) => {
  console.log(`Event ${i + 1}:`, result.type.value);
});
```

---

## Step 3: Filter by Event Type

```javascript
import { EVENT_TYPES } from '@unrdf/kgc-4d';

// Query only CREATE events
const createEvents = await store.queryEventLog(`
  PREFIX kgc: <http://kgc.io/>
  SELECT ?event ?timestamp
  WHERE {
    GRAPH <${GRAPHS.EVENT_LOG}> {
      ?event kgc:type "${EVENT_TYPES.CREATE}" .
      ?event kgc:t_ns ?t_ns .
    }
  }
  ORDER BY ?t_ns
`);

console.log('CREATE events:', createEvents.length);
```

---

## Step 4: Complete Working Example

```javascript
import {
  KGCStore,
  GitBackbone,
  freezeUniverse,
  EVENT_TYPES,
  GRAPHS,
} from '@unrdf/kgc-4d';
import { dataFactory } from '@unrdf/oxigraph';

async function main() {
  const store = new KGCStore();
  const git = new GitBackbone('./tutorial-04-repo');

  // Create several events
  await store.appendEvent(
    { type: EVENT_TYPES.CREATE, payload: { actor: 'alice' } },
    [{
      type: 'add',
      subject: dataFactory.namedNode('http://example.org/Alice'),
      predicate: dataFactory.namedNode('http://xmlns.com/foaf/0.1/name'),
      object: dataFactory.literal('Alice'),
    }]
  );

  await store.appendEvent(
    { type: EVENT_TYPES.UPDATE, payload: { actor: 'bob' } },
    [{
      type: 'add',
      subject: dataFactory.namedNode('http://example.org/Alice'),
      predicate: dataFactory.namedNode('http://xmlns.com/foaf/0.1/age'),
      object: dataFactory.literal('30'),
    }]
  );

  await freezeUniverse(store, git);

  // Query 1: All events
  const allEvents = await store.queryEventLog(`
    PREFIX kgc: <http://kgc.io/>
    SELECT ?event ?type
    WHERE {
      GRAPH <${GRAPHS.EVENT_LOG}> {
        ?event kgc:type ?type .
      }
    }
  `);
  console.log('Total events:', allEvents.length);

  // Query 2: Events by type
  const createCount = await store.queryEventLog(`
    PREFIX kgc: <http://kgc.io/>
    SELECT (COUNT(?event) as ?count)
    WHERE {
      GRAPH <${GRAPHS.EVENT_LOG}> {
        ?event kgc:type "${EVENT_TYPES.CREATE}" .
      }
    }
  `);
  console.log('CREATE events:', createCount[0].count.value);

  // Query 3: Events with timestamps
  const timeline = await store.queryEventLog(`
    PREFIX kgc: <http://kgc.io/>
    SELECT ?event ?type ?t_ns
    WHERE {
      GRAPH <${GRAPHS.EVENT_LOG}> {
        ?event kgc:type ?type .
        ?event kgc:t_ns ?t_ns .
      }
    }
    ORDER BY ?t_ns
  `);

  timeline.forEach((event, i) => {
    console.log(`Event ${i + 1}:`, event.type.value, 'at', event.t_ns.value);
  });

  return { store, git };
}

main();
```

---

## Verification

Run the complete example:

```bash
node tutorial-04-query.mjs
```

Expected output:
```
Total events: 3
CREATE events: 1
Event 1: CREATE at 1735297800123456789
Event 2: UPDATE at 1735297800123457000
Event 3: SNAPSHOT at 1735297800123458000
```

---

## Evidence

**Source Code:**
- queryEventLog: `/home/user/unrdf/packages/kgc-4d/src/store.mjs` (KGCStore class)
- Constants: `/home/user/unrdf/packages/kgc-4d/src/constants.mjs`

**Tests:**
- Store tests: `/home/user/unrdf/packages/kgc-4d/test/store.test.mjs`
- Integration tests: `/home/user/unrdf/packages/kgc-4d/test/integration.test.mjs`

**Examples:**
- Event log query: `/home/user/unrdf/packages/kgc-4d/examples/basic-usage.mjs:97-119`

---

## Key Takeaways

1. **EventLog is RDF:** Query with standard SPARQL
2. **Immutable history:** Events never deleted or modified
3. **Temporal ordering:** Use ORDER BY ?t_ns for chronological sequence
4. **Audit trails:** Complete provenance via SPARQL queries

---

## Next Steps

**Continue Learning:**
- [Tutorial 05: Vector Clocks](./05-vector-clocks.md) - Distributed causality
- [How-To 03: Query Event History](../how-to/03-query-event-history.md) - Advanced patterns
- [How-To 02: Audit Decision Trail](../../how-to/02-audit-decision-trail.md) - Production audit

**Reference:**
- [Events & Predicates](../reference/events-predicates.md) - Complete schema
- [Named Graphs](../reference/named-graphs.md) - Graph architecture

---

**Navigate:** [← Previous](./03-time-travel.md) | [Tutorials](./README.md) | [Next →](./05-vector-clocks.md)
