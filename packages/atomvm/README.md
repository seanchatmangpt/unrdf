# @unrdf/atomvm

`@unrdf/atomvm` provides AtomVM execution surfaces for browser and Node.js environments plus a bounded control plane for federating multiple AtomVM swarms through unrdf.

The package separates three concerns:

1. **Runtime** — load and execute AtomVM modules.
2. **Construction** — admit swarms, construct routes and create immutable intents.
3. **Actuation and evidence** — execute only through a supplied broker and return integrity-bound receipts that can be verified and replayed.

The RDF graph describes admitted topology. It does not receive ambient execution authority.

## Installation

From the unrdf workspace:

```bash
pnpm install
```

Requirements depend on the surface being used:

- Node.js 18 or newer
- `pnpm` 7 or newer
- `erlc` and `packbeam` when building Erlang modules
- A browser with service-worker and cross-origin-isolation support for browser execution
- Docker and Erlang tooling only for the Docker Swarm experiments

## Browser runtime

Build an Erlang module:

```bash
pnpm run build:erlang mymodule
```

Start the development server:

```bash
pnpm dev
```

Open the application with an explicit module name:

```text
http://localhost:3000?module=mymodule
```

Programmatic use:

```js
import { AtomVMRuntime } from '@unrdf/atomvm';

const runtime = new AtomVMRuntime(terminal, 'mymodule');
await runtime.loadWASM();
await runtime.executeBeam('/mymodule.avm');
```

The browser runtime uses a service worker to establish cross-origin isolation before using `SharedArrayBuffer`.

## Node.js runtime

Execute an AVM file through the CLI:

```bash
node src/cli.mjs public/mymodule.avm
```

Programmatic use:

```js
import { AtomVMNodeRuntime } from '@unrdf/atomvm';

const runtime = new AtomVMNodeRuntime();
await runtime.load();
await runtime.execute('/absolute/path/to/module.avm');
```

## Federated swarm control plane

`AtomVMSwarmCluster` models a cluster of independently operated AtomVM swarms. A swarm must be admitted before it can participate in routing. Links must also be admitted before a cross-swarm route can be constructed.

```js
import { createAtomVMSwarmCluster } from '@unrdf/atomvm';

const cluster = createAtomVMSwarmCluster({
  clusterId: 'edge-federation',
});

cluster.admitSwarm({
  id: 'west',
  gatewayNode: 'west-gateway',
  cookieRef: 'secret://atomvm/west',
  endpoint: 'atomvm://west',
});

cluster.admitSwarm({
  id: 'east',
  gatewayNode: 'east-gateway',
  cookieRef: 'secret://atomvm/east',
  endpoint: 'atomvm://east',
});

cluster.connect('west', 'east');

const intent = cluster.constructIntent({
  sourceId: 'west',
  targetId: 'east',
  operation: 'rdf.delta.apply',
  payload: { add: 3 },
});
```

Construction does not execute the intent. Actuation requires an explicit broker:

```js
const receipt = await cluster.actuate(intent, {
  async execute({ intent, target, route }) {
    return transport.send({
      endpoint: target.endpoint,
      operation: intent.operation,
      payload: intent.payload,
      route,
    });
  },
});

if (receipt.status !== 'ALIVE') {
  throw new Error(`Swarm operation did not complete: ${receipt.status}`);
}

if (!cluster.verifyReceipt(receipt)) {
  throw new Error('Receipt integrity verification failed');
}

const replayed = cluster.replay(receipt.receiptId);
```

Calling `actuate` without `broker.execute` is refused with `BROKER_REQUIRED_REFUSED`. Broker failures become bounded `BLOCKED` receipts rather than mutating the admitted topology.

### RDF topology projection

The admitted cluster graph can be projected deterministically as N-Quads:

```js
const nquads = cluster.toNQuads();
```

This is a projection of the control-plane state. Editing the projection does not actuate infrastructure.

## Ten executable innovation checkpoints

The checkpoint evaluator tests the federation tracer bullet against ten gates derived from Gall's evolutionary constraint, object-centric process evidence and conformance, Clean Architecture boundaries, and Pragmatic Programmer feedback practices.

```js
import { evaluateInnovationCheckpoints } from '@unrdf/atomvm';

const report = evaluateInnovationCheckpoints({
  cluster,
  intent,
  receipt,
  replayedReceipt: replayed,
  brokerObserved: true,
  negativeControlPassed: true,
});

console.log(report.status); // ALIVE or PARTIAL_ALIVE
console.log(report.passed); // number of ALIVE checkpoints
console.log(report.reportDigest);
```

`ALIVE` requires all ten checkpoints. Missing authority evidence does not become an implicit success; the affected checkpoint is `UNSUPPORTED`, and the aggregate report is `PARTIAL_ALIVE`.

See:

- [Federated swarm architecture](./docs/explanation/federated-swarm-control-plane.md)
- [Innovation checkpoint reference](./docs/reference/innovation-checkpoints.md)

## Docker Swarm experiment

The repository also contains an experimental Docker Swarm/Erlang distribution demonstration:

```bash
node examples/production-messaging.mjs
```

It exercises Docker overlay networking, EPMD discovery, RPC messaging, circuit-breaker handling and supervisor behavior. Treat experiment evidence as evidence for the exact recorded configuration, not as a blanket production guarantee for every environment.

Manual deployment:

```bash
docker swarm init
docker stack deploy \
  -c experiments/docker-swarm-messaging/docker-stack-fixed.yml \
  atomvm
```

## Other package surfaces

The package also exports:

- `CircuitBreaker` and `SupervisorTree`
- Oxigraph bridges and integrated RDF stores
- RDF validation and message validation
- SPARQL pattern matching and query caching
- OpenTelemetry instrumentation and SLA monitoring
- triple-stream batching
- hardened VM construction, scheduling, sandboxing and receipt generation

Consult the source-level exports in `src/index.mjs` for the current public surface.

## Development and validation

Run the package test suite:

```bash
pnpm test
```

Run the focused federation tests directly:

```bash
node --test test/swarm-cluster.test.mjs
node --test test/innovation-checkpoints.test.mjs
```

Additional commands:

```bash
pnpm test:browser
pnpm test:playwright
pnpm build
pnpm build:vite
```

A passing focused test establishes standing only for the tested subject and configuration. Repository-wide standing requires the broader workspace gates to pass against the same source head.

## Typed refusals and standing

The federation API uses explicit refusals for invalid or unauthorized transitions, including:

- `INVALID_ID_REFUSED`
- `DUPLICATE_SWARM_REFUSED`
- `UNKNOWN_SWARM_REFUSED`
- `NO_ROUTE_REFUSED`
- `UNADMITTED_INTENT_REFUSED`
- `INTENT_DRIFT_REFUSED`
- `BROKER_REQUIRED_REFUSED`
- `RECEIPT_NOT_FOUND_REFUSED`
- `RECEIPT_DRIFT_REFUSED`

Execution receipts use `ALIVE` or `BLOCKED`. Checkpoint reports use `ALIVE`, `PARTIAL_ALIVE`, and per-checkpoint `UNSUPPORTED` where evidence is absent.

## License

MIT
