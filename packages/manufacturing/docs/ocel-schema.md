# OCEL Event Schema for CodeManufactory

Object-Centric Event Log (OCEL) schema for manufacturing process validation.

## Event Types

| OCEL Event            | Object Type | Description                              |
| --------------------- | ----------- | ---------------------------------------- |
| `ArtifactCreated`     | artifact    | Artifact enters lifecycle (seeded stage) |
| `ArtifactTransformed` | artifact    | Operator applied to artifact             |
| `ArtifactValidated`   | artifact    | Validation gate passed                   |
| `ArtifactProjected`   | artifact    | Projection completed                     |
| `ArtifactCompiled`    | artifact    | Compilation successful                   |
| `ArtifactBenchmarked` | artifact    | Benchmark results recorded               |
| `ArtifactReleased`    | artifact    | Release published                        |
| `ArtifactObserved`    | artifact    | Observability data collected             |
| `ReceiptGenerated`    | receipt     | BLAKE3 receipt emitted                   |
| `ProofGateEvaluated`  | proof_gate  | Gate result (pass/fail)                  |
| `OperatorExecuted`    | operator    | μ(O) operator execution                  |

## OCEL 2.0 JSON Structure

```json
{
  "@context": "https://ocelstandard.org/ocel2.0/context.json",
  "objects": {
    "artifact-1": {
      "id": "artifact-1",
      "type": "artifact",
      "lifecycle": [
        "seeded",
        "bred",
        "validated",
        "projected",
        "compiled",
        "benchmarked",
        "released",
        "observed",
        "receipted"
      ],
      "kind": "source-code"
    },
    "receipt-1": {
      "id": "receipt-1",
      "type": "receipt",
      "artifact": "artifact-1",
      "previousHash": "0000000000000000000000000000000000000000000000000000000000000",
      "hash": "abc123...",
      "stage": "validated"
    }
  },
  "events": [
    {
      "id": "event-1",
      "type": "ArtifactCreated",
      "timestamp": "2026-04-10T12:00:00Z",
      "object": "artifact-1",
      "activity": "seed",
      "attributes": {
        "operator": "breed-ontology",
        "input": "codemanufactory.ttl"
      }
    },
    {
      "id": "event-2",
      "type": "ArtifactValidated",
      "timestamp": "2026-04-10T12:00:01Z",
      "object": "artifact-1",
      "activity": "validate",
      "attributes": {
        "gate": "schema-valid",
        "result": "pass"
      }
    },
    {
      "id": "event-3",
      "type": "ReceiptGenerated",
      "timestamp": "2026-04-10T12:00:02Z",
      "object": "receipt-1",
      "activity": "emit-receipt",
      "attributes": {
        "artifact": "artifact-1",
        "stage": "validated"
      }
    }
  ]
}
```

## Temporal Constraints

### Stage Ordering (Must Hold)

```
seeded → bred → validated → projected → compiled → benchmarked → released → observed → receipted
```

### Impossible Event Sequences

- `ReceiptGenerated` before `ArtifactValidated` for same stage
- `ArtifactReleased` before `ArtifactValidated`
- `ArtifactCompiled` before `ArtifactProjected`
- `ProofGateEvaluated` (pass) before predecessor stage completed

## Conformance Checks

### 1. Replay Conformance

Can observed event log be replayed against intended pipeline model?

**pm4py check:**

```python
from pm4py.algo.conformance import footprints_conformance

# Intended model (BPMN/POWL)
model = pm4py.discover_bpmn_inductively(log)

# Conformance check
conformance = footprints_conformance(log, model)
# fitness: 1.0 = all log traces fit model
# precision: 1.0 = model allows only observed behavior
```

### 2. Object Lifecycle Completeness

Every artifact must have:

- Exactly one `ArtifactCreated` event
- Sequential stage transitions (no backwards moves)
- Terminal state (`released` or `receipted`) reached
- Receipt chain linking each stage

**OCEL query:**

```sparql
PREFIX ocel: <https://ocelstandard.org/ocel2.0/>

SELECT ?artifact WHERE {
  ?artifact a ocel:type "artifact" .
  ?artifact ocel:hasEvent ?created .
  ?created a ocel:type "ArtifactCreated" .

  # Must reach terminal state
  ?artifact ocel:hasEvent ?terminal .
  ?terminal a ocel:type ?terminalType .
  FILTER (?terminalType IN ("ArtifactReleased", "ArtifactReceipted"))
}
```

### 3. Temporal Lawfulness

No impossible overlaps or ordering violations.

**pm4py check:**

```python
from pm4py.algo.filtering.log import timestamp_filter

# Ensure event timestamps are monotonic per object
for artifact_id in artifacts:
    events = get_events_for(artifact_id)
    assert events == sorted(events, key=lambda e: e.timestamp)
```

### 4. Causal Consistency

Receipts must reference valid artifact states.

**OCEL query:**

```sparql
PREFIX ocel: <https://ocelstandard.org/ocel2.0/>

SELECT ?receipt WHERE {
  ?receipt a ocel:type "receipt" .
  ?receipt ocel:attribute ?artifactRef .
  ?receipt ocel:hasEvent ?emit .
  ?emit a ocel:timestamp ?emitTime .

  # Artifact must exist and be in valid state at emit time
  ?artifact a ocel:id ?artifactRef .
  ?artifact ocel:hasEvent ?stateEvent .
  ?stateEvent a ocel:timestamp ?stateTime .
  FILTER (?stateTime <= ?emitTime)
}
```

## Integration with Manufacturing Pipeline

Each operator execution emits OCEL events:

```javascript
class OperatorExecutor {
  async execute(operatorName, input, opts = {}) {
    const startTime = Date.now();
    const span = tracer.startSpan(`operator.${operatorName}`);

    try {
      const result = await operator.execute(input, context);

      // Emit OCEL event
      this.emitOCelEvent({
        type: 'OperatorExecuted',
        object: input.artifactId,
        activity: operatorName,
        timestamp: new Date(startTime).toISOString(),
        attributes: {
          duration_ms: Date.now() - startTime,
          trace_id: span.spanContext().traceId,
          status: 'success',
        },
      });

      span.setStatus({ code: SpanStatusCode.OK });
      return result;
    } catch (err) {
      span.recordException(err);
      span.setStatus({ code: SpanStatusCode.ERROR });
      throw err;
    } finally {
      span.end();
    }
  }
}
```
