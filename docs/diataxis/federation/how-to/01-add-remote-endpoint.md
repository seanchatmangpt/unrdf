# How To: Add a Remote Endpoint

This guide shows how to register a new SPARQL endpoint with an existing federation coordinator
at runtime, attach metadata to it, and verify that it is reachable before issuing queries.

## When to use this

- You have an already-running coordinator and want to expand the federation without restarting.
- You want to tag peers with region, dataset, or capability metadata for later filtering.
- You want to confirm connectivity before the peer starts receiving queries.

---

## Register the peer

```javascript
import { createCoordinator } from '@unrdf/federation';

const coordinator = createCoordinator(); // empty federation

const peerInfo = await coordinator.addPeer(
  'wikidata', // unique id — any string
  'https://query.wikidata.org/sparql', // must be a valid URL
  {
    // optional metadata object
    label: 'Wikidata SPARQL endpoint',
    region: 'global',
    dataset: 'wikidata',
    readonly: true,
  }
);
```

`addPeer` validates `id` and `endpoint` with Zod, calls `peerManager.ping()` to check
reachability, and returns a `PeerInfo` object:

```javascript
{
  id:           'wikidata',
  endpoint:     'https://query.wikidata.org/sparql',
  metadata:     { label: '…', region: 'global', … },
  registeredAt: 1712345678000,
  lastSeen:     1712345678001,
  status:       'healthy',   // or 'unreachable' if the ping failed
}
```

If the endpoint is not reachable when `addPeer` is called, the peer is still registered and
a warning is logged:

```
Peer wikidata registered but is not currently reachable
```

The peer will be included in future `query()` calls only if its status becomes `'healthy'`
(via a later health check or successful query).

---

## Verify reachability before querying

Call `coordinator.healthCheck()` to ping all registered peers and get a summary:

```javascript
const health = await coordinator.healthCheck();
// {
//   totalPeers:       1,
//   healthyPeers:     1,
//   degradedPeers:    0,
//   unreachablePeers: 0,
//   results: [
//     { id: 'wikidata', healthy: true, status: 'healthy' }
//   ]
// }

if (health.healthyPeers === 0) {
  throw new Error('No reachable endpoints');
}
```

Alternatively, start automatic health checks so the coordinator re-pings peers on an interval:

```javascript
coordinator.startHealthChecks(); // default interval: 60 000 ms
// …do work…
coordinator.stopHealthChecks(); // or coordinator.destroy()
```

---

## Retrieve a peer's current info

```javascript
const peer = coordinator.getPeer('wikidata');
// null if the peer was never registered or has been removed

if (peer) {
  console.log(peer.status); // 'healthy' | 'degraded' | 'unreachable'
  console.log(peer.lastSeen); // timestamp of last successful health check
}
```

---

## Remove a peer

```javascript
const removed = coordinator.removePeer('wikidata');
// true  — peer was found and removed
// false — peer was not registered
```

After removal the peer is excluded from all future queries and health checks.

---

## List all registered peers

```javascript
// All peers regardless of status
const all = coordinator.listPeers();

// Only healthy peers
const healthy = coordinator.listPeers({ status: 'healthy' });

// Only unreachable peers
const down = coordinator.listPeers({ status: 'unreachable' });
```

---

## Notes

- `addPeer` throws a `ZodError` if `endpoint` is not a valid URL. Wrap calls in `try/catch`
  when registering user-supplied endpoints.
- The `id` string must be unique per coordinator instance. Calling `addPeer` with an existing
  `id` updates the endpoint and metadata rather than creating a second entry.
- Metadata is free-form (`Record<string, unknown>`). It is stored but not interpreted by the
  coordinator. Use it for display, filtering, or integration with your own routing logic.
