# Tutorial 01: Federate Across Two Stores

By the end of this tutorial you will have a working federation that queries two SPARQL endpoints
in parallel and returns deduplicated results from both.

## What you will build

A small script that:

1. Creates a federation coordinator with two peer stores.
2. Queries both stores with a single SPARQL call.
3. Inspects the per-peer results and the aggregated output.

## What you need

- Two SPARQL endpoints. For this tutorial we use mock URLs; replace them with real ones.
  The coordinator will attempt HTTP connections, so either run real endpoints or intercept
  `fetch` in tests.
- `@unrdf/federation` installed: `pnpm add @unrdf/federation`

---

## Step 1 — Import the coordinator factory

```javascript
// federate-example.mjs
import { createCoordinator } from '@unrdf/federation';
```

`createCoordinator` is the primary API. It returns a coordinator object that manages peer
registration, query routing, and result aggregation.

---

## Step 2 — Create the coordinator with two peers

```javascript
const coordinator = createCoordinator({
  peers: [
    { id: 'store-a', endpoint: 'http://store-a.internal:3000/sparql' },
    { id: 'store-b', endpoint: 'http://store-b.internal:3000/sparql' },
  ],
  strategy: 'broadcast', // default: query all healthy peers
  timeout: 15000, // 15 s per peer
});
```

Both peers are registered immediately and given an initial status of `'healthy'`. Status
transitions to `'degraded'` or `'unreachable'` after a failed ping or query.

---

## Step 3 — Run a federated query

```javascript
const result = await coordinator.query(
  'SELECT ?name WHERE { ?person <http://xmlns.com/foaf/0.1/name> ?name }'
);
```

`coordinator.query()` broadcasts the SPARQL string to all healthy peers in parallel, then
aggregates and deduplicates the bindings.

The return value has this shape:

```javascript
{
  success: true,          // false when no peer succeeded
  results: [              // deduplicated bindings from all peers
    { name: { type: 'literal', value: 'Alice' } },
    { name: { type: 'literal', value: 'Bob'   } },
  ],
  peerResults: [          // individual peer responses
    { success: true,  peerId: 'store-a', data: { results: { bindings: [...] } }, duration: 42  },
    { success: true,  peerId: 'store-b', data: { results: { bindings: [...] } }, duration: 61  },
  ],
  totalDuration: 61,      // wall-clock ms (parallel, so = slowest peer)
  successCount: 2,
  failureCount: 0,
}
```

---

## Step 4 — Inspect per-peer results

```javascript
for (const peerResult of result.peerResults) {
  if (peerResult.success) {
    console.log(`${peerResult.peerId}: ${peerResult.data.results.bindings.length} bindings`);
  } else {
    console.warn(`${peerResult.peerId} failed: ${peerResult.error}`);
  }
}
```

---

## Step 5 — Handle no healthy peers

If both stores are down `coordinator.query()` returns a failure envelope rather than throwing:

```javascript
const result = await coordinator.query('SELECT * WHERE { ?s ?p ?o }');

if (!result.success) {
  console.error('Federation failed:', result.error);
  // 'No healthy peers available'
}
```

The coordinator never throws from `query()` — it always returns the envelope. Errors only
propagate when calling `addPeer()` with an invalid endpoint (Zod validation) or when the
coordinator itself encounters an unexpected internal error.

---

## Step 6 — Add a peer at runtime

You are not limited to the peers you list at construction time:

```javascript
// Registers the peer and pings it to check reachability.
const peerInfo = await coordinator.addPeer(
  'store-c',
  'http://store-c.internal:3000/sparql',
  { region: 'eu-west-1' } // optional metadata
);

console.log(peerInfo.status); // 'healthy' if reachable, otherwise a warning is logged
```

Subsequent calls to `coordinator.query()` will include `store-c` automatically.

---

## Step 7 — Clean up

When you are done, stop periodic health checks and release the interval timer:

```javascript
coordinator.destroy();
```

Always call `destroy()` before your process exits or before discarding the coordinator
reference, or the health-check `setInterval` will keep the event loop alive.

---

## Complete working script

```javascript
// federate-example.mjs
import { createCoordinator } from '@unrdf/federation';

const coordinator = createCoordinator({
  peers: [
    { id: 'store-a', endpoint: 'http://store-a.internal:3000/sparql' },
    { id: 'store-b', endpoint: 'http://store-b.internal:3000/sparql' },
  ],
  timeout: 10000,
});

const sparql = 'SELECT ?name WHERE { ?person <http://xmlns.com/foaf/0.1/name> ?name }';
const result = await coordinator.query(sparql);

if (result.success) {
  console.log(`Found ${result.results.length} unique names across both stores`);
  for (const binding of result.results) {
    console.log(' -', binding.name?.value);
  }
} else {
  console.error('Query failed:', result.error);
}

coordinator.destroy();
```

---

## What you learned

- `createCoordinator({ peers })` registers stores up front. Each peer needs an `id` and an
  HTTP `endpoint`.
- `coordinator.query(sparql)` fans out to all healthy peers and deduplicates the results.
- The return envelope always has `success`, `results`, `peerResults`, `successCount`,
  `failureCount`, and `totalDuration`.
- `coordinator.destroy()` stops the health-check timer.

## Next steps

- How-To: [Add a remote endpoint](../how-to/01-add-remote-endpoint.md) — runtime registration
  with metadata.
- How-To: [Handle federation timeout](../how-to/02-handle-federation-timeout.md) — per-query
  timeouts and partial results.
- How-To: [Query with source selection](../how-to/03-query-with-source-selection.md) — narrow
  queries to a specific peer or strategy.
