# How To: Handle Federation Timeout

This guide shows how to set timeouts at the coordinator level and per-query level, how to
detect which peers timed out, and how to work with partial results when some peers respond
and others do not.

## When to use this

- You have slow or unreliable endpoints and want to bound total query time.
- You need to surface partial results even when some peers time out.
- You want different timeout budgets for different queries (exploratory vs. production).

---

## Set a default timeout at coordinator creation

```javascript
import { createCoordinator } from '@unrdf/federation';

const coordinator = createCoordinator({
  peers: [
    { id: 'fast-store', endpoint: 'http://fast.internal:3000/sparql' },
    { id: 'slow-store', endpoint: 'http://slow.internal:3000/sparql' },
  ],
  timeout: 5000, // 5 000 ms per peer (default is 30 000)
});
```

The `timeout` applies to each individual peer's HTTP request via `AbortController`. If a peer
does not respond within `timeout` milliseconds, its result is marked as failed and the query
moves on.

---

## Override timeout for a single query

```javascript
const result = await coordinator.query(
  'SELECT * WHERE { ?s ?p ?o } LIMIT 1',
  { timeout: 2000 } // 2 s for this query only
);
```

The per-query `timeout` option overrides the coordinator's default for that call alone.

---

## Detect which peers timed out

When a peer times out the error message in its `peerResults` entry contains the string
`'AbortError'` or `'timeout'`. The coordinator recognises these and sets the peer's status to
`'unreachable'`:

```javascript
const result = await coordinator.query('SELECT * WHERE { ?s ?p ?o }');

for (const pr of result.peerResults) {
  if (!pr.success) {
    const isTimeout = pr.error && (pr.error.includes('AbortError') || pr.error.includes('timeout'));

    console.warn(
      isTimeout
        ? `${pr.peerId} timed out after ${pr.duration} ms`
        : `${pr.peerId} failed: ${pr.error}`
    );
  }
}
```

---

## Work with partial results

`result.success` is `true` as long as at least one peer responded successfully. Do not treat
a partial failure as a total failure:

```javascript
const result = await coordinator.query('SELECT ?x WHERE { ?x a <http://schema.org/Person> }');

if (!result.success) {
  // Zero peers responded — nothing to work with
  console.error('All peers failed:', result.error);
  return;
}

// Some peers may have failed; use what we have
console.log(
  `${result.successCount}/${result.peerResults.length} peers responded, ` +
    `${result.results.length} unique bindings`
);

// result.results already aggregates only the successful peers
for (const binding of result.results) {
  process(binding);
}
```

---

## Query a single peer directly with a tight timeout

When you need a guaranteed time-bound on a single endpoint, use `coordinator.queryPeer()`:

```javascript
const peerResult = await coordinator.queryPeer(
  'fast-store',
  'SELECT * WHERE { ?s ?p ?o } LIMIT 5',
  { timeout: 1000 }
);

if (peerResult.success) {
  console.log(peerResult.data.results.bindings);
} else {
  console.error(`fast-store: ${peerResult.error} (${peerResult.duration} ms)`);
}
```

`queryPeer()` returns a single-peer result object, not an aggregated result. See the
[Reference](../reference/federation-api.md#coordinatorqueryPeer) for the exact shape.

---

## Health-check timeouts

The ping used by `addPeer()`, `healthCheck()`, and automatic health checks has its own timeout
separate from query timeouts. It defaults to 5 000 ms:

```javascript
// peerManager.ping is called internally; the 5 000 ms default is not configurable
// through the coordinator API. If ping timeouts are a problem, use healthCheck()
// less frequently via healthCheckInterval:

const coordinator = createCoordinator({
  peers: [{ id: 'store-a', endpoint: 'http://store-a:3000/sparql' }],
  timeout: 10000, // query timeout
  healthCheckInterval: 120000, // ping every 2 min instead of every 60 s
});
```

---

## Notes

- After a timeout the affected peer transitions to `'unreachable'`. It will not receive further
  queries until a health check restores it to `'healthy'`.
- `totalDuration` in the result reflects wall-clock time for the parallel batch. For the
  `'broadcast'` strategy this equals the slowest peer that responded (or the timeout value
  if the slowest peer timed out).
- For sequential execution strategies (available on `executeDistributedQuery` directly) total
  duration is the sum of all peer durations.
