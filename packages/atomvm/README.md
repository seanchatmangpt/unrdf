# @unrdf/atomvm

`@unrdf/atomvm` provides AtomVM execution surfaces plus a bounded control plane for connecting multiple AtomVM execution endpoints through unrdf.

The package separates three concerns:

1. **Runtime** — build and execute real AtomVM applications.
2. **Construction** — admit swarms, select routes and create immutable intents.
3. **Actuation and evidence** — execute only through an explicit broker and return integrity-bound receipts that can be verified and replayed.

The RDF graph describes admitted topology. It does not receive ambient execution authority.

## Installation

From the unrdf workspace:

```bash
pnpm install
```

Requirements depend on the surface being used:

- Node.js 18 or newer
- `pnpm` 7 or newer
- `erlc` and `PackBEAM` when building Erlang modules
- a Generic UNIX AtomVM binary for native Node.js execution
- a browser with service-worker and cross-origin-isolation support for browser execution
- Docker and Erlang/OTP tooling only for the archived Docker Swarm experiment

## Build and execute real AtomVM

The exact runtime verifier is `.github/workflows/atomvm-runtime-alive.yml`. It performs the following dependency-closed path:

1. materialize the pinned upstream AtomVM source tag
2. build the Generic UNIX `AtomVM` binary, `PackBEAM` and `atomvmlib.avm`
3. compile `examples/erlang/swarm_probe.erl` with `erlc`
4. package the BEAM module as a runnable AVM application
5. execute it directly with AtomVM
6. execute it through `AtomVMNodeRuntime`
7. execute west, central and east swarm intents through `AtomVMProcessBroker`
8. verify replay, negative authority controls and all ten innovation checkpoints
9. upload hashes, logs and a machine-readable receipt

The verified marker is:

```text
{atomvm_swarm_alive,ok}
Return value: ok
```

The workflow artifact contains:

```text
atomvm-source.sha
swarm-probe/direct-runtime.log
swarm-probe/node-runtime.log
swarm-probe/packbeam-list.log
receipts/atomvm-runtime-cluster-receipt.json
receipts/verification.log
receipts/sha256sums.txt
```

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

The browser runtime uses a service worker to establish cross-origin isolation before using `SharedArrayBuffer`. Browser execution has separate standing from the Generic UNIX build and is not established by the native runtime workflow.

## Native Node.js runtime

`AtomVMNodeRuntime` executes the real Generic UNIX binary. Configure it with an explicit binary and any required AVM libraries:

```js
import { AtomVMNodeRuntime } from '@unrdf/atomvm';

const runtime = new AtomVMNodeRuntime({
  atomvmBinary: '/opt/atomvm/bin/AtomVM',
  libraryPaths: ['/opt/atomvm/lib/atomvmlib.avm'],
});

await runtime.load();
const result = await runtime.execute('/absolute/path/to/module.avm');

console.log(result.runtime);        // AtomVM
console.log(result.runtimeVersion); // observed from AtomVM -v
console.log(result.exitCode);       // 0
```

`ATOMVM_BIN` may supply the binary path when `atomvmBinary` is omitted. The CLI uses the same runtime:

```bash
ATOMVM_BIN=/opt/atomvm/bin/AtomVM \
  node src/cli.mjs public/mymodule.avm
```

The runtime refuses missing binaries, missing AVM files and nonzero VM exits. It invokes AtomVM directly without a shell.

## Federated swarm control plane

`AtomVMSwarmCluster` models independently named AtomVM execution endpoints. A swarm must be admitted before it can participate in routing. Links must also be admitted before a cross-swarm route can be constructed.

```js
import { createAtomVMSwarmCluster } from '@unrdf/atomvm';

const cluster = createAtomVMSwarmCluster({
  clusterId: 'edge-federation',
});

cluster.admitSwarm({
  id: 'west',
  gatewayNode: 'west-gateway',
  cookieRef: 'authority://atomvm/west',
  endpoint: 'atomvm://west',
});

cluster.admitSwarm({
  id: 'east',
  gatewayNode: 'east-gateway',
  cookieRef: 'authority://atomvm/east',
  endpoint: 'atomvm://east',
});

cluster.connect('west', 'east');

const intent = cluster.constructIntent({
  sourceId: 'west',
  targetId: 'east',
  operation: 'atomvm.execute',
  payload: { probe: 'swarm_probe' },
});
```

Construction does not execute the intent.

### Real AtomVM process broker

`AtomVMProcessBroker` is the authority-bearing adapter for the Generic UNIX VM:

```js
import { AtomVMProcessBroker } from '@unrdf/atomvm';

const broker = new AtomVMProcessBroker({
  atomvmBinary: '/opt/atomvm/bin/AtomVM',
  runtimeRef: 'upstream-source-sha',
  swarms: {
    east: {
      avmPath: '/srv/apps/swarm_probe.avm',
      libraryPaths: ['/opt/atomvm/lib/atomvmlib.avm'],
      expectedMarker: 'atomvm_swarm_alive',
    },
  },
});

const receipt = await cluster.actuate(intent, broker);

if (receipt.status !== 'ALIVE') {
  throw new Error(`Swarm operation did not complete: ${receipt.status}`);
}
if (!cluster.verifyReceipt(receipt)) {
  throw new Error('Receipt integrity verification failed');
}

const replayed = cluster.replay(receipt.receiptId);
```

Calling `actuate` without `broker.execute` is refused with `BROKER_REQUIRED_REFUSED`. Broker failures become bounded `BLOCKED` receipts rather than mutating admitted topology.

### Scope of “swarm”

The receipted verifier establishes three **logical unrdf swarm endpoints** connected by admitted routes. Each endpoint is actuated by a separate observed Generic UNIX AtomVM process.

It does **not** claim:

- native Erlang distribution between AtomVM instances
- EPMD membership
- persistent VM-to-VM sockets
- Docker overlay connectivity
- a production orchestration guarantee

Those are separate acceptance subjects. The older Docker experiment uses full Erlang/OTP and must not be cited as evidence that AtomVM itself provides those behaviors.

### RDF topology projection

The admitted cluster graph can be projected deterministically as N-Quads:

```js
const nquads = cluster.toNQuads();
```

This is a projection of canonical control-plane state. Editing the projection does not actuate infrastructure.

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

`ALIVE` requires all ten checkpoints. Missing authority evidence does not become implicit success; the affected checkpoint is `UNSUPPORTED`, and the aggregate report is `PARTIAL_ALIVE`.

See:

- [Federated swarm architecture](./docs/explanation/federated-swarm-control-plane.md)
- [Innovation checkpoint reference](./docs/reference/innovation-checkpoints.md)

## Docker Swarm Erlang/OTP experiment

The repository also contains a historical Docker Swarm demonstration:

```bash
node examples/production-messaging.mjs
```

That experiment installs and launches full Erlang/OTP with `erl`. It exercises Docker overlay networking, EPMD discovery and Erlang RPC in its recorded environment. It is not the AtomVM runtime verifier and is not used to establish AtomVM standing.

## Other package surfaces

The package also exports:

- `CircuitBreaker` and `SupervisorTree`
- Oxigraph bridges and integrated RDF stores
- RDF validation and message validation
- SPARQL pattern matching and query caching
- OpenTelemetry instrumentation and SLA monitoring
- triple-stream batching
- hardened VM construction, scheduling, sandboxing and receipt generation

Consult `src/index.mjs` for the current public surface.

## Development and validation

Run focused JavaScript tests:

```bash
node --test \
  test/swarm-cluster.test.mjs \
  test/innovation-checkpoints.test.mjs \
  test/process-broker.test.mjs
```

Run the package suite:

```bash
pnpm test
```

Additional commands:

```bash
pnpm test:browser
pnpm test:playwright
pnpm build
pnpm build:vite
```

A passing JavaScript test establishes standing only for the tested boundary. Real-runtime standing requires the source build, AVM packaging, direct execution, public Node runtime execution, brokered swarm execution and receipt artifact from the same exact source head.

## Typed refusals and standing

The federation and process APIs use explicit refusals, including:

- `INVALID_ID_REFUSED`
- `DUPLICATE_SWARM_REFUSED`
- `UNKNOWN_SWARM_REFUSED`
- `NO_ROUTE_REFUSED`
- `UNADMITTED_INTENT_REFUSED`
- `INTENT_DRIFT_REFUSED`
- `BROKER_REQUIRED_REFUSED`
- `RECEIPT_NOT_FOUND_REFUSED`
- `RECEIPT_DRIFT_REFUSED`
- `OPERATION_NOT_ADMITTED_REFUSED`
- `ROUTE_TARGET_MISMATCH_REFUSED`
- `SWARM_RUNTIME_NOT_CONFIGURED_REFUSED`
- `ATOMVM_BINARY_NOT_FOUND_REFUSED`
- `AVM_NOT_FOUND_REFUSED`
- `AVM_LIBRARY_NOT_FOUND_REFUSED`
- `ATOMVM_TIMEOUT_REFUSED`
- `ATOMVM_EXIT_BLOCKED`
- `ATOMVM_MARKER_MISSING_REFUSED`

Execution receipts use `ALIVE` or `BLOCKED`. Checkpoint reports use `ALIVE`, `PARTIAL_ALIVE`, and per-checkpoint `UNSUPPORTED` where evidence is absent.

## License

MIT
