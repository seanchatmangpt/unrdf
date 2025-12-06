# Phase 4.8: Protocol Reference - Complete Message Types

## Message Structure

All messages follow this envelope:

```json
{
  "type": "command|response|event|error",
  "id": "identifier",
  "timestamp": "ISO-8601",
  "version": "1.0.0",
  "payload": {},
  "meta": {}
}
```

---

## Command Messages

### executeQuery

```json
{
  "type": "command",
  "id": 1,
  "payload": {
    "action": "executeQuery",
    "params": {
      "query": "SELECT ?s ?p ?o WHERE { ?s ?p ?o }",
      "options": {
        "limit": 100,
        "offset": 0
      }
    }
  },
  "meta": {
    "timeout_ms": 5000
  }
}
```

### addTriple

```json
{
  "type": "command",
  "id": 2,
  "payload": {
    "action": "addTriple",
    "params": {
      "subject": "http://example.com/subject",
      "predicate": "http://example.com/property",
      "object": "http://example.com/object"
    }
  }
}
```

### removeTriple

```json
{
  "type": "command",
  "id": 3,
  "payload": {
    "action": "removeTriple",
    "params": {
      "subject": "http://example.com/subject",
      "predicate": "http://example.com/property",
      "object": "http://example.com/object"
    }
  }
}
```

### addGraph

```json
{
  "type": "command",
  "id": 4,
  "payload": {
    "action": "addGraph",
    "params": {
      "graph_id": "graph-1",
      "triples": [
        ["http://s1", "http://p1", "http://o1"],
        ["http://s2", "http://p2", "http://o2"]
      ]
    }
  }
}
```

### subscribe

```json
{
  "type": "command",
  "id": 5,
  "payload": {
    "action": "subscribe",
    "params": {
      "topic": "dataChanged",
      "filter": {
        "predicate": "http://example.com/name"
      }
    }
  }
}
```

---

## Response Messages

### Success Response

```json
{
  "type": "response",
  "id": 1,
  "payload": {
    "success": true,
    "data": [
      {
        "s": "http://example.com/subject",
        "p": "http://example.com/property",
        "o": "http://example.com/object"
      }
    ],
    "count": 1
  },
  "meta": {
    "duration_ms": 42,
    "execution_path": "index_lookup"
  }
}
```

### Error Response

```json
{
  "type": "error",
  "id": 999,
  "payload": {
    "error_code": "VALIDATION_ERROR",
    "message": "Invalid SPARQL query: syntax error",
    "details": {
      "line": 1,
      "column": 10,
      "context": "SELECT * INVALID"
    }
  },
  "meta": {
    "timestamp": "2024-01-01T12:00:00Z",
    "retryable": false,
    "error_id": "err-abc123"
  }
}
```

---

## Event Messages

### dataChanged

```json
{
  "type": "event",
  "id": null,
  "payload": {
    "event_type": "dataChanged",
    "details": {
      "graph": "graph-1",
      "changes": [
        {
          "type": "added",
          "triple": ["http://s", "http://p", "http://o"]
        }
      ]
    }
  },
  "meta": {
    "source": "server",
    "timestamp": "2024-01-01T12:00:00Z"
  }
}
```

### connectionStateChanged

```json
{
  "type": "event",
  "id": null,
  "payload": {
    "event_type": "connectionStateChanged",
    "details": {
      "state": "connected|disconnected|reconnecting",
      "reason": "user_initiated|timeout|network_error"
    }
  }
}
```

### healthStatusChanged

```json
{
  "type": "event",
  "id": null,
  "payload": {
    "event_type": "healthStatusChanged",
    "details": {
      "status": "ok|degraded|down",
      "message": "Server is running normally"
    }
  }
}
```

---

## Error Codes

| Code | Status | Retryable | Meaning |
|------|--------|-----------|---------|
| PROTOCOL_VERSION_MISMATCH | 400 | No | Client/server version incompatible |
| VALIDATION_ERROR | 422 | No | Invalid input (bad query, missing fields) |
| AUTHENTICATION_FAILED | 401 | No | Invalid credentials or expired token |
| AUTHORIZATION_DENIED | 403 | No | User lacks permission |
| NOT_FOUND | 404 | No | Resource doesn't exist |
| CONFLICT | 409 | No | State conflict (e.g., duplicate triple) |
| QUERY_TIMEOUT | 504 | Yes | Operation exceeded timeout |
| RESOURCE_EXHAUSTED | 429 | Yes | Server under load (rate limit, memory) |
| INTERNAL_ERROR | 500 | Yes | Unexpected server error |
| SERVICE_UNAVAILABLE | 503 | Yes | Server temporarily unavailable |

---

## Version Compatibility

### Supported Versions

```
Client v1.0.0 ↔ Server v1.0.0   ✓ Exact match
Client v1.0.0 ↔ Server v1.1.0   ✓ Minor upgrade (ignore unknown fields)
Client v1.0.0 ↔ Server v1.2.5   ✓ Patch upgrade (fully compatible)
Client v1.0.0 ↔ Server v2.0.0   ✗ Major version (reject)
```

### Version Negotiation

On first message, send protocol version:

```json
{
  "type": "command",
  "id": "handshake",
  "version": "1.0.0",
  "payload": {
    "action": "handshake",
    "params": {
      "protocol_version": "1.0.0",
      "client_type": "javascript|erlang"
    }
  }
}
```

Server responds:

```json
{
  "type": "response",
  "id": "handshake",
  "version": "1.0.0",
  "payload": {
    "success": true,
    "protocol_version": "1.0.0",
    "server_type": "erlang|atomvm"
  }
}
```

---

## Timeout Behavior

### Default Timeouts

```
Command execution: 5 seconds
Subscription: No timeout
Health check: 3 seconds
```

### Timeout Error

```json
{
  "type": "error",
  "id": 1,
  "payload": {
    "error_code": "QUERY_TIMEOUT",
    "message": "Query exceeded 5000ms timeout",
    "details": {
      "timeout_ms": 5000,
      "elapsed_ms": 5042
    }
  },
  "meta": {
    "retryable": true,
    "retry_after_ms": 1000
  }
}
```

---

## Retry Strategy

### Exponential Backoff

```
Attempt 1: Immediately
Attempt 2: 1s + random(0-100ms)
Attempt 3: 2s + random(0-200ms)
Attempt 4: 4s + random(0-400ms)
Attempt 5: 8s + random(0-800ms)
Max: 5 attempts (~15s total)
```

### When to Retry

Only retry if:
- `error_code` is retryable (see error codes table)
- `meta.retryable == true`
- `meta.retry_after_ms` specifies when

---

## Connection Lifecycle

```
Client                          Server
  │                               │
  │────CONNECT (WebSocket upgrade)│
  │                               │
  │←──CONNECTED (handshake)───────│
  │                               │
  │────COMMAND 1────────────────→ │
  │←───RESPONSE 1────────────────│
  │                               │
  │────COMMAND 2────────────────→ │
  │←───RESPONSE 2────────────────│
  │                               │
  │←──EVENT (unsolicited)────────│
  │                               │
  │────DISCONNECT──────────────→ │
  │←──DISCONNECT_ACK────────────│
  │                               │
```

---

## Payload Size Limits

```
Max message size: 10 MB
Max query length: 100 KB
Max result set: 50,000 rows
Max single value: 1 MB
```

If exceeded:

```json
{
  "type": "error",
  "payload": {
    "error_code": "RESOURCE_EXHAUSTED",
    "message": "Query result exceeds maximum size"
  }
}
```

---

## See Also

- **01-PROTOCOL-DESIGN.md** - Design rationale
- **07-INTEGRATION-TESTING.md** - Testing protocol compliance
