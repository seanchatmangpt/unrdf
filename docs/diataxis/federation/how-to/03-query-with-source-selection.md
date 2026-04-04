# How To: Query with Source Selection

This guide shows how to control which peers receive a given SPARQL query using the three
built-in routing strategies and how to target a single specific peer.

## When to use this

- You want to query only one store when you know which one holds the relevant data.
- You want the federation to return results from the first available peer rather than waiting
  for all peers.
- You want to route different query types to different stores.

---

## The three routing strategies

The coordinator's `strategy` setting controls `routeQuery()`, which selects the peers that
will receive each query:

| Strategy          | Peers queried                                                                                 | Use when…                                                       |
| ----------------- | --------------------------------------------------------------------------------------------- | --------------------------------------------------------------- |
| `broadcast`       | All healthy peers (default)                                                                   | You want union results and do not know which store has the data |
| `selective`       | All healthy peers (currently same as broadcast, reserved for future metadata-based filtering) | You plan to extend with custom routing logic                    |
| `first-available` | Only the first healthy peer                                                                   | You want a single answer quickly and do not need union coverage |

### Set strategy at construction time

```javascript
import { createCoordinator } from '@unrdf/federation';

const coordinator = createCoordinator({
  peers: [
    { id: 'primary', endpoint: 'http://primary:3000/sparql' },
    { id: 'secondary', endpoint: 'http://secondary:3000/sparql' },
  ],
  strategy: 'first-available',
});

const result = await coordinator.query('SELECT * WHERE { ?s ?p ?o } LIMIT 1');
// Only 'primary' receives the query (assuming it is healthy)
```

### Override strategy per query

```javascript
// Use broadcast for this particular query regardless of coordinator default
const result = await coordinator.query('SELECT ?x WHERE { ?x a <http://schema.org/Person> }', {
  strategy: 'broadcast',
});
```

---

## Query one specific peer

Use `coordinator.queryPeer()` to bypass routing entirely and send a query directly to one
peer by ID:

```javascript
const result = await coordinator.queryPeer(
  'primary',
  'SELECT ?label WHERE { <http://example.org/alice> <http://schema.org/name> ?label }'
);

if (result.success) {
  console.log(result.data.results.bindings);
} else {
  console.error(result.error);
}
```

`queryPeer()` returns a `QueryResult` (single-peer shape), not an `AggregatedResult`. The
`result.data` field holds the raw parsed HTTP response body.

---

## Route by metadata (custom logic)

The `selective` strategy currently passes all healthy peers through unchanged. To implement
actual metadata-based filtering, use `routeQuery()` from `@unrdf/federation` directly with
your own filter, then call `executeDistributedQuery()`:

```javascript
import {
  createCoordinator,
  createPeerManager,
  routeQuery,
  executeDistributedQuery,
} from '@unrdf/federation';

const coordinator = createCoordinator({
  peers: [
    { id: 'eu-store', endpoint: 'http://eu:3000/sparql', metadata: { region: 'eu' } },
    { id: 'us-store', endpoint: 'http://us:3000/sparql', metadata: { region: 'us' } },
  ],
});

// Select only EU peers manually
const allPeers = coordinator.listPeers({ status: 'healthy' });
const euPeers = allPeers.filter(p => p.metadata?.region === 'eu');

const sparql = 'SELECT * WHERE { ?s a <http://schema.org/Person> }';
const result = await executeDistributedQuery(euPeers, sparql, { timeout: 10000 });

console.log(result.results);
```

This approach uses the same aggregation and deduplication logic as `coordinator.query()`,
but with a custom peer selection step.

---

## Check which peers received a query

Inspect `result.peerResults` to see which peers were actually queried:

```javascript
const result = await coordinator.query('SELECT * WHERE { ?s ?p ?o }', {
  strategy: 'broadcast',
});

for (const pr of result.peerResults) {
  console.log(`${pr.peerId}: success=${pr.success}, duration=${pr.duration}ms`);
}
```

With `first-available` `peerResults` will contain exactly one entry.

---

## Notes

- `routeQuery()` operates on the list of already-filtered healthy peers. Unhealthy peers
  (`'degraded'`, `'unreachable'`) are excluded before routing.
- The `first-available` strategy always picks the peer at index 0 of the healthy peers array.
  Peer order is insertion order (JavaScript `Map` iteration order).
- There is no built-in weighted routing in `routeQuery`. Weighted load balancing is available
  in `FederationCoordinator` (v6 API), which uses `selectWeighted()` based on each store's
  `weight` field.
