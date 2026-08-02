# Federated AtomVM swarm control plane

## Purpose

The control plane connects independently named AtomVM execution endpoints without collapsing topology description, intent construction and infrastructure actuation into one authority-bearing component.

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
execute real AtomVM process
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

It does not own a process launcher, Docker client, Erlang RPC client or secret resolver.

### Broker port

A broker is supplied at the actuation boundary and must expose:

```js
{
  async execute({ intent, target, route }) {
    // perform the environment-specific operation
  }
}
```

This dependency points inward: the domain constructs the request and calls a narrow port; infrastructure implements that port. Replacing one runtime adapter does not require changing route or receipt semantics.

### `AtomVMProcessBroker`

`AtomVMProcessBroker` is the concrete authority-bearing adapter for the Generic UNIX AtomVM runtime. It:

1. admits only the `atomvm.execute` operation
2. verifies that the selected route terminates at the target swarm
3. resolves that target to a configured AVM application and library set
4. verifies the AtomVM binary and AVM inputs exist
5. launches the binary directly with `spawn` and no shell
6. bounds execution with a timeout
7. requires a successful exit and an application-specific evidence marker
8. returns runtime identity, route, output and output digests to the cluster receipt

The broker does not construct intents or routes. It cannot bypass `AtomVMSwarmCluster.actuate` when used through the public federation path.

### `AtomVMNodeRuntime`

`AtomVMNodeRuntime` is the direct Node.js facade over the same Generic UNIX binary contract. It probes the real runtime with `AtomVM -v`, validates application and library files, launches the VM without a shell and returns observed stdout, stderr, exit status and runtime version.

It no longer depends on a generated `AtomVM-node-[VERSION].js` placeholder.

### Receipt

Every admitted actuation attempt produces a receipt containing:

- receipt identity
- cluster identity
- intent digest
- observed route
- start and completion timestamps
- `ALIVE` result or `BLOCKED` error evidence
- receipt digest

A broker exception does not escape as an unclassified state transition. It becomes a `BLOCKED` receipt. Refusals before admission to the actuation path remain typed errors.

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

The endpoint must use the `atomvm://` scheme. `cookieRef` is an authority reference, not raw secret material. Secret resolution belongs to the broker or its infrastructure dependencies.

A link is admitted with:

```js
cluster.connect(leftId, rightId);
```

Self-links and links involving unknown swarms are refused.

## Deterministic routing

`route(sourceId, targetId)` performs breadth-first search over admitted links. Peer identifiers are sorted before expansion, so equal-length alternatives resolve deterministically.

Undirected federation links are also canonicalized before RDF emission. The same admitted topology therefore produces the same N-Quads orientation regardless of link insertion order.

A returned route is evidence of graph selection, not evidence that transport connectivity exists. Runtime execution must still be observed through the broker and bound into a receipt.

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

`toNQuads()` projects admitted topology into one named graph. It includes:

- cluster membership
- swarm gateway nodes
- swarm endpoints
- canonical federation links

The projection is deterministic for the same admitted graph. It excludes raw authority material and contains no execution hook.

## Object-centric event evidence

`receiptToOcel(receipt, intent)` produces a compact OCEL-style object/event representation with identities for:

- cluster
- source swarm
- target swarm
- intent
- receipt

Object identities are deduplicated, so a source-target self-execution relates one swarm object rather than manufacturing duplicate objects with the same identifier.

The event records operation, completion time, outcome, route and intent digest. The projection is intentionally bounded; it is not represented as a complete OCEL 2.0 interchange implementation.

## Runtime verifier

`.github/workflows/atomvm-runtime-alive.yml` establishes the real-runtime path against one exact unrdf head. It:

1. clones pinned upstream AtomVM source
2. resolves and records the exact upstream commit
3. builds only `AtomVM`, `PackBEAM` and `atomvmlib`
4. compiles and packages `swarm_probe.erl`
5. executes the probe directly
6. executes it through `AtomVMNodeRuntime`
7. runs control-plane, checkpoint and broker tests
8. actuates west, central and east through `AtomVMProcessBroker`
9. verifies replay and the unbrokered-actuation negative control
10. emits hashes and a machine-readable receipt artifact

The receipt binds:

- upstream AtomVM source identity
- runtime binary hash
- AVM application and library hashes
- admitted topology
- three observed executions and routes
- per-execution receipts
- object-centric event projections
- ten checkpoint results
- aggregate receipt digest

## Scope boundary

The verifier proves a cluster of **logical unrdf swarm endpoints**, each actuated by an observed real AtomVM process. The graph connects those execution endpoints through admitted unrdf routes.

It does not prove native AtomVM-to-AtomVM distribution, EPMD membership, persistent inter-VM sockets, Docker overlay connectivity or production orchestration. The historical Docker experiment launches full Erlang/OTP and is not evidence for those properties in AtomVM.

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
| Operation outside process-broker contract | `OPERATION_NOT_ADMITTED_REFUSED` |
| Route does not terminate at target | `ROUTE_TARGET_MISMATCH_REFUSED` |
| Target runtime is not configured | `SWARM_RUNTIME_NOT_CONFIGURED_REFUSED` |
| Runtime or AVM input unavailable | typed file refusal |
| Runtime exceeds timeout | `ATOMVM_TIMEOUT_REFUSED` |
| Runtime exits unsuccessfully | `ATOMVM_EXIT_BLOCKED` |
| Runtime evidence marker absent | `ATOMVM_MARKER_MISSING_REFUSED` |
| Broker succeeds | `ALIVE` receipt |
| Broker throws | `BLOCKED` receipt |
| Unknown replay target | `RECEIPT_NOT_FOUND_REFUSED` |
| Mutated receipt | `RECEIPT_DRIFT_REFUSED` |

## Evolution model

The implementation follows a working-core growth rule:

1. Preserve the smallest verified path: admit → route → construct → broker → real VM → receipt → replay.
2. Extend through public operations rather than bypassing the model.
3. Add projections and evaluators downstream of canonical state.
4. Keep environment-specific execution behind the broker port.
5. Require new standing claims to carry observed execution evidence.

A new swarm changes admitted topology and broker configuration. It does not require a parallel federation architecture.
