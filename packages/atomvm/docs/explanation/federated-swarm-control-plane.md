# Federated AtomVM swarm control plane

## Purpose

The control plane connects independently operated AtomVM swarms without collapsing topology description, intent construction and infrastructure actuation into one authority-bearing component.

The governing sequence is:

```text
admit topology
    ↓
select route
    ↓
construct immutable intent
    ↓
actuate through broker
    ↓
emit integrity-bound receipt
    ↓
verify and replay
```

The route planner can describe a path. It cannot execute that path. The RDF projection can describe admitted state. It cannot mutate infrastructure.

## Components

### `AtomVMSwarmCluster`

The cluster is the domain object. It owns:

- admitted swarm identities and endpoints
- admitted undirected federation links
- deterministic route selection
- immutable operation intents
- execution receipts
- deterministic N-Quads projection

It does not own a Docker client, Erlang RPC client or secret resolver.

### Broker

A broker is supplied at the actuation boundary and must expose:

```js
{
  async execute({ intent, target, route }) {
    // perform the environment-specific operation
  }
}
```

This dependency points inward: the domain constructs the request and calls a narrow port; infrastructure implements that port. Replacing Docker, Erlang distribution or another transport does not require changing route or receipt semantics.

### Receipt

Every admitted actuation attempt produces a receipt containing:

- receipt identity
- cluster identity
- intent digest
- observed route
- start and completion timestamps
- `ALIVE` result or `BLOCKED` error evidence
- receipt digest

A broker exception does not escape as an unclassified state transition. It becomes a `BLOCKED` receipt. Refusals that occur before admission to the actuation path remain typed `SwarmClusterRefusal` errors.

## Admission boundaries

A swarm is admitted with:

```js
cluster.admitSwarm({
  id,
  gatewayNode,
  cookieRef,
  endpoint,
  metadata,
});
```

The endpoint must use the `atomvm://` scheme. `cookieRef` is an authority reference, not a raw Erlang cookie. Secret resolution belongs to the broker or its infrastructure dependencies.

A link is admitted with:

```js
cluster.connect(leftId, rightId);
```

Self-links and links involving unknown swarms are refused.

## Deterministic routing

`route(sourceId, targetId)` performs breadth-first search over admitted links. Peer identifiers are sorted before expansion, so equal-length alternatives resolve deterministically.

A returned route is evidence of graph selection, not evidence that transport connectivity exists. Transport execution must still be observed through the broker and bound into a receipt.

## Intent construction

`constructIntent` captures:

- cluster identity
- source and target swarm identities
- selected route
- operation name
- payload
- construction timestamp
- `CONSTRUCTED` standing

The canonical intent body is hashed. Mutation or reconstruction that changes its admitted fields causes `INTENT_DRIFT_REFUSED` at actuation.

## Actuation

`actuate(intent, broker)` enforces four checks before calling infrastructure:

1. The intent belongs to the current cluster.
2. Its standing is `CONSTRUCTED`.
3. Its digest still matches its body.
4. `broker.execute` exists.

The fourth check implements zero unreceipted actuation. Without a broker, the operation is refused with `BROKER_REQUIRED_REFUSED`.

The broker receives the exact intent, target swarm and route. Its return value is recorded in an `ALIVE` receipt. Its thrown error is normalized into a `BLOCKED` receipt.

## RDF projection

`toNQuads()` projects the admitted topology into one named graph. It includes:

- cluster membership
- swarm gateway nodes
- swarm endpoints
- federation links

The projection is deterministic for the same admitted graph. It excludes raw authority material and does not contain an execution hook.

## Object-centric event evidence

`receiptToOcel(receipt, intent)` produces a compact OCEL-style object/event representation with distinct identities for:

- cluster
- source swarm
- target swarm
- intent
- receipt

The event records operation, completion time, outcome, route and intent digest. This preserves multi-object relationships that would be lost in a single case identifier.

The projection is intentionally bounded. It is suitable for conformance and identity checks implemented by this package; it is not represented as a complete OCEL 2.0 interchange implementation.

## Failure model

| Condition | Result |
|---|---|
| Invalid identifier | `INVALID_ID_REFUSED` |
| Duplicate swarm | `DUPLICATE_SWARM_REFUSED` |
| Unknown swarm | `UNKNOWN_SWARM_REFUSED` |
| No admitted graph path | `NO_ROUTE_REFUSED` |
| Intent from another cluster or standing | `UNADMITTED_INTENT_REFUSED` |
| Mutated intent | `INTENT_DRIFT_REFUSED` |
| Missing broker | `BROKER_REQUIRED_REFUSED` |
| Broker succeeds | `ALIVE` receipt |
| Broker throws | `BLOCKED` receipt |
| Unknown replay target | `RECEIPT_NOT_FOUND_REFUSED` |
| Mutated receipt | `RECEIPT_DRIFT_REFUSED` |

## Evolution model

The implementation follows a working-core growth rule:

1. Preserve the smallest verified path: admit → route → construct → broker → receipt → replay.
2. Extend through public operations rather than bypassing the model.
3. Add projections and evaluators downstream of the canonical state.
4. Keep environment-specific transport behind the broker port.
5. Require new standing claims to carry observed execution evidence.

A new swarm therefore changes admitted topology. It does not require a parallel federation architecture.

## Verification scope

The focused tests establish:

- deterministic multi-swarm routing
- successful brokered actuation
- receipt integrity and replay
- refusal of unbrokered actuation
- deterministic RDF projection
- ten checkpoint evaluation
- preservation of object/event identity
- downgrade to `PARTIAL_ALIVE` when authority evidence is absent

These tests do not by themselves prove Docker availability, Erlang node reachability or production readiness in an untested environment.
