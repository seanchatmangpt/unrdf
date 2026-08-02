# Innovation checkpoint API

## Imports

```js
import {
  INNOVATION_CHECKPOINTS,
  evaluateInnovationCheckpoints,
  receiptToOcel,
} from '@unrdf/atomvm';
```

## Evaluation

```js
const report = evaluateInnovationCheckpoints({
  cluster,
  intent,
  receipt,
  replayedReceipt,
  brokerObserved,
  negativeControlPassed,
});
```

The two boolean fields are execution observations:

- `brokerObserved` is true only after the supplied broker actually runs.
- `negativeControlPassed` is true only after calling `actuate` without a broker produces `BROKER_REQUIRED_REFUSED`.

Architecture inspection alone is not sufficient to set either field.

## Report

```js
{
  clusterId,
  status: 'ALIVE' | 'PARTIAL_ALIVE',
  passed,
  total: 10,
  checkpoints: [
    {
      id,
      authority,
      criterion,
      status: 'ALIVE' | 'UNSUPPORTED',
      evidence,
    },
  ],
  reportDigest,
}
```

`reportDigest` detects changes in the report body. It is not a signature or external attestation.

## Checkpoints

| ID | Required evidence |
|---|---|
| `gall-working-core` | At least two swarms, a conforming route and a verified receipt |
| `ocel-event-completeness` | Activity, completion time, outcome and related object identities |
| `object-centric-identity` | Distinct cluster, swarm, intent and receipt identifiers |
| `route-conformance` | Route endpoints match the intent and every hop is admitted |
| `dependency-rule` | Observed broker execution and observed no-broker refusal |
| `explicit-contracts` | `ALIVE` or `BLOCKED` receipt with a valid digest |
| `tracer-bullet` | Conforming route, valid receipt and matching replay |
| `orthogonality-dry` | Non-empty deterministic N-Quads projection |
| `failure-isolation` | Stable swarm/link topology and explicit receipt standing |
| `evolutionary-extension` | Extension through `admitSwarm`, `connect` and `route` |

All ten must be `ALIVE` for aggregate `ALIVE`. Missing required evidence yields `UNSUPPORTED` for that checkpoint and `PARTIAL_ALIVE` for the report.

## OCEL-style evidence projection

```js
const eventLog = receiptToOcel(receipt, intent);
```

The projection keeps separate identities for the cluster, source swarm, target swarm, intent and receipt. Its event records operation, completion time, outcome, route and intent digest.

This is a bounded OCEL-style projection for the package's evidence checks. It is not presented as a complete OCEL 2.0 interchange implementation.

## Complete example

```js
let negativeControlPassed = false;

try {
  await cluster.actuate(intent);
} catch (error) {
  negativeControlPassed = error.code === 'BROKER_REQUIRED_REFUSED';
}

let brokerObserved = false;
const receipt = await cluster.actuate(intent, {
  async execute(request) {
    brokerObserved = true;
    return transport.send(request);
  },
});

const replayedReceipt = cluster.replay(receipt.receiptId);

const report = evaluateInnovationCheckpoints({
  cluster,
  intent,
  receipt,
  replayedReceipt,
  brokerObserved,
  negativeControlPassed,
});
```

## Interpretation

- A route is not proof of network reachability.
- A constructed intent is not an executed operation.
- A receipt must pass digest verification before it has integrity standing.
- The checkpoint report applies only to the exact evidence supplied to the evaluator.
- `UNSUPPORTED` means required evidence is absent; it is not a typed refusal and not a refutation of the design.
