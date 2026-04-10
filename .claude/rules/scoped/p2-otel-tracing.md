# Scoped Rule: Federation OTel Tracing

**Scope**: `@unrdf/federation` - Distributed tracing with OpenTelemetry

## Span Naming Convention

Federation spans use dot notation:

```
federation.operation_name
```

Examples:
- `federation.quorum_vote`
- `federation.chain_receipts`
- `federation.propagate_delta`

## Required Span Attributes

Every federation span MUST include:

| Attribute | Type | Description |
|-----------|------|-------------|
| `federation.node_id` | string | Unique node identifier |
| `federation.quorum_id` | string | Quorum round identifier (if applicable) |
| `federation.receipt_count` | number | Number of receipts in chain |
| `federation.peer_count` | number | Number of peers involved |

## Status Handling

Spans MUST set status explicitly:

```javascript
// Correct
span.setStatus({ code: SpanStatusCode.OK });
span.setAttribute('federation.vote', 'approve');

// Correct - error case
span.recordException(e);
span.setStatus({ code: SpanStatusCode.ERROR, message: e.message });

// Wrong - no status
// What happened? Success or failure?
```

## Quorum Operations

### Vote Span

```javascript
const span = tracer.startSpan('federation.quorum_vote', {
  attributes: {
    'federation.node_id': config.nodeId,
    'federation.quorum_id': quorumId,
    'federation.proposal_hash': proposalHash
  }
});

try {
  const vote = await castVote(proposal);
  span.setAttribute('federation.vote', vote.approve ? 'approve' : 'reject');
  span.setStatus({ code: SpanStatusCode.OK });
} catch (error) {
  span.recordException(error);
  span.setStatus({ code: SpanStatusCode.ERROR, message: error.message });
  throw;
} finally {
  span.end();
}
```

### Receipt Chain Span

```javascript
const span = tracer.startSpan('federation.chain_receipts', {
  attributes: {
    'federation.node_id': config.nodeId,
    'federation.receipt_count': receipts.length
  }
});

for (const receipt of receipts) {
  span.addEvent('receipt_linked', {
    attributes: {
      'federation.receipt_hash': receipt.hash,
      'federation.previous_hash': receipt.previousHash
    }
  });
}
```

## BLAKE3 Hashing

Receipts use BLAKE3 for hash chaining:

```javascript
import { blake3 } from '@unrdf/federation/crypto';

const hash = blake3(previousHash + payload);
span.setAttribute('federation.receipt_hash', hash);
```

## Tracer Configuration

```javascript
import { trace } from '@opentelemetry/api';

const tracer = trace.getTracer('@unrdf/federation', '26.4.9');
```

## Export Configuration

Federation traces exported to OTEL collector:

```javascript
// Collector endpoint
process.env.OTEL_EXPORTER_OTLP_ENDPOINT = 'http://localhost:4317';
process.env.OTEL_SERVICE_NAME = '@unrdf/federation';
```

## Verification

Check spans in Jaeger:

```bash
# Query federation spans
curl -s "http://localhost:16686/api/traces?service=federation&limit=20"
```

Expected span patterns:
- `federation.quorum_vote`
- `federation.chain_receipts`
- `federation.propagate_delta`

## Files Using Federation Tracing

- `packages/federation/src/quorum/*` - Quorum voting
- `packages/federation/src/receipt/*` - Receipt chaining
- `packages/federation/src/propagation/*` - Delta propagation
