# Phase 4.1: Protocol Design - Binary-Level Compatibility

## Overview

The protocol layer defines how JavaScript and Erlang systems communicate. This layer must be **identical** across both runtimes to ensure equivalence.

Three components matter:

1. **Transport Contract** - How connections work
2. **Message Contract** - What gets sent
3. **Failure Contract** - How errors surface

---

## 1. Transport Contract

### Connection Lifecycle

```
Client                              Server
  |                                   |
  |--1. CONNECT (with auth)---------→|
  |←-2. AUTH_RESPONSE (success/fail)--|
  |--3. HEARTBEAT (periodic)-------→|
  |←-4. HEARTBEAT_ACK----------------|
  |--5. COMMAND (request)----------→|
  |←-6. RESPONSE (or error)----------|
  |--7. DISCONNECT-----------------→|
  |←-8. DISCONNECT_ACK---------------|
  |                                   |
```

### Protocol Modes

**WebSocket (Browser)**
```javascript
// Both OTP and AtomVM support WebSocket in the browser
const ws = new WebSocket('ws://localhost:8080/rdf-protocol');
ws.onmessage = (event) => {
  const message = JSON.parse(event.data);
  handleProtocolMessage(message);
};
```

**HTTP (Fallback)**
```javascript
// For environments without WebSocket
const response = await fetch('http://localhost:8080/rdf-protocol', {
  method: 'POST',
  body: JSON.stringify(message),
  headers: { 'Content-Type': 'application/json' }
});
```

**TCP (OTP Only)**
```erlang
% Full OTP can use raw TCP with binary encoding
{ok, Socket} = gen_tcp:connect("localhost", 5555, [binary, {packet, 4}]),
ok = gen_tcp:send(Socket, encode_message(Message)).
```

### Browser Simulation (AtomVM + WASM)

AtomVM in the browser only supports:
- WebSocket connections
- JSON encoding
- No raw TCP or binary protocols

**This is acceptable** because the protocol layer adapts:

```erlang
% Same code, different config
case runtime_mode() of
    otp ->
        % Use gen_tcp, binary encoding
        establish_tcp_connection(Host, Port);
    atomvm ->
        % Use WebSocket, JSON encoding
        establish_websocket_connection(Host, Port)
end.
```

---

## 2. Message Contract

### Envelope Format

All messages follow this structure:

```json
{
  "type": "command|response|event|error",
  "id": "uuid-or-integer",
  "timestamp": "ISO-8601",
  "version": "1.0.0",
  "payload": {},
  "meta": {
    "correlation_id": "uuid",
    "user_id": "optional",
    "request_id": "optional"
  }
}
```

### Message Types

#### 1. Commands (Client → Server)

```json
{
  "type": "command",
  "id": 42,
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

#### 2. Responses (Server → Client)

```json
{
  "type": "response",
  "id": 42,
  "payload": {
    "success": true,
    "data": [
      { "s": "http://example.com/subject", ... }
    ],
    "count": 1
  },
  "meta": {
    "duration_ms": 42,
    "execution_path": "cache|index|full_scan"
  }
}
```

#### 3. Events (Server → Client, unsolicited)

```json
{
  "type": "event",
  "id": null,
  "payload": {
    "event_type": "dataChanged|connectionStateChanged|error",
    "details": {
      "changes": ["graph_updated"],
      "timestamp": "2024-01-01T12:00:00Z"
    }
  },
  "meta": {
    "source": "server"
  }
}
```

#### 4. Errors

```json
{
  "type": "error",
  "id": 42,
  "payload": {
    "error_code": "QUERY_TIMEOUT|VALIDATION_ERROR|NOT_FOUND",
    "message": "Human-readable error message",
    "details": {
      "query": "...",
      "validation_errors": [...]
    }
  },
  "meta": {
    "timestamp": "2024-01-01T12:00:00Z",
    "retryable": true,
    "retry_after_ms": 1000
  }
}
```

### Encoding Standards

**For browser (WebSocket/AtomVM):**
```json
Content-Type: application/json
Encoding: UTF-8
Size limit: 10MB per message
```

**For OTP (can upgrade to binary):**
```erlang
% BERT (Binary ERlang Term) for performance
% Fallback to JSON for compatibility
encode(Message, Format) when Format == json ->
    jsx:encode(Message);
encode(Message, Format) when Format == bert ->
    bert:encode(Message).
```

---

## 3. Failure Contract

### Error Codes

Standardized error codes that both runtimes produce:

```
PROTOCOL_VERSION_MISMATCH
  Status: 400
  Retryable: No
  Meaning: Client/server protocol versions incompatible

VALIDATION_ERROR
  Status: 422
  Retryable: No
  Meaning: Invalid input (bad query, missing fields, type errors)

AUTHENTICATION_FAILED
  Status: 401
  Retryable: No (usually)
  Meaning: Auth credentials invalid or expired

AUTHORIZATION_DENIED
  Status: 403
  Retryable: No
  Meaning: User lacks permission for operation

NOT_FOUND
  Status: 404
  Retryable: No
  Meaning: Resource doesn't exist

QUERY_TIMEOUT
  Status: 504
  Retryable: Yes (after wait)
  Meaning: Operation exceeded timeout
  Retry-After: Specified in meta.retry_after_ms

RESOURCE_EXHAUSTED
  Status: 429
  Retryable: Yes (after backoff)
  Meaning: Server under load (rate limit, memory, etc.)
  Retry-After: Specified in meta.retry_after_ms

INTERNAL_ERROR
  Status: 500
  Retryable: Yes (with exponential backoff)
  Meaning: Unexpected server error
  Details: Error ID for logging correlation

SERVICE_UNAVAILABLE
  Status: 503
  Retryable: Yes (with exponential backoff)
  Meaning: Server temporarily unavailable
  Retry-After: Specified in meta.retry_after_ms
```

### Timeout Behavior

All operations have timeout semantics:

```
Client sends command with meta.timeout_ms
  ↓
Server starts tracking operation
  ↓
If operation exceeds timeout_ms:
  Server sends QUERY_TIMEOUT error
  Client receives error, can retry
```

Duration is **guaranteed identical** across runtimes:

```erlang
% Erlang side
timeout_handler(Timeout) ->
    Start = erlang:system_time(millisecond),
    Result = execute_operation(),
    Elapsed = erlang:system_time(millisecond) - Start,
    case Elapsed > Timeout of
        true -> {error, query_timeout};
        false -> Result
    end.
```

```javascript
// JavaScript side
async function executeWithTimeout(fn, timeoutMs) {
  const start = Date.now();
  const result = await fn();
  const elapsed = Date.now() - start;
  if (elapsed > timeoutMs) {
    throw new Error('QUERY_TIMEOUT');
  }
  return result;
}
```

### Backoff Strategy

Both runtimes implement identical exponential backoff:

```
Attempt 1: Immediate
Attempt 2: Wait 1s + random(0-100ms)
Attempt 3: Wait 2s + random(0-200ms)
Attempt 4: Wait 4s + random(0-400ms)
Attempt 5: Wait 8s + random(0-800ms)
Max retries: 5 (total time: ~15s)
```

---

## 4. Versioning Strategy

### Version Format

```
X.Y.Z
^       Major: Breaking changes
  ^     Minor: New features (backward compatible)
    ^   Patch: Bug fixes (backward compatible)
```

### Compatibility Matrix

```
Client v1.0.0 can talk to:
  - Server v1.0.0 ✓ (exact match)
  - Server v1.1.0 ✓ (minor upgrade, ignore new fields)
  - Server v1.2.5 ✓ (patch, ignore new fields)
  - Server v2.0.0 ✗ (major version, fail)

Rules:
  - Minor version: Ignore unknown fields
  - Major version: Fail with PROTOCOL_VERSION_MISMATCH
  - Patch version: Always compatible
```

### Unknown Fields

When encountering unknown fields:

```javascript
// JavaScript: Strict by default
const message = JSON.parse(data);
if (message.version !== SUPPORTED_VERSION) {
  throw new ProtocolError('PROTOCOL_VERSION_MISMATCH');
}
// Only access known fields
const { type, id, payload } = message;

// Erlang: Similar approach
case maps:get(<<"version">>, Message) of
    SupportedVersion when SupportedVersion == <<"1.0">> ->
        process_message(Message);
    _ ->
        {error, protocol_version_mismatch}
end.
```

---

## Implementation Checklist

For both runtimes:

- [ ] Message envelope matches structure above
- [ ] All 5 error codes implemented
- [ ] Timeout semantics identical (millisecond precision)
- [ ] Backoff strategy matches exponential curve
- [ ] Unknown field handling consistent
- [ ] Version checking in place
- [ ] Encoding (JSON) matches spec
- [ ] Message ID correlation works bidirectionally
- [ ] Heartbeat mechanism (periodic keep-alive)
- [ ] Connection state tracking matches

---

## Testing Protocol Equivalence

Run same test vector against both runtimes and compare:

1. **Byte-for-byte encoding** (if JSON: normalize whitespace first)
2. **Error responses** (same error codes, messages)
3. **Timing** (timeouts trigger at same duration)
4. **Backoff** (retry delays match)
5. **Unknown fields** (handled identically)

This is the foundation for "good enough" confidence without full emulation.

---

## See Also

- **02-RUNTIME-ARCHITECTURE.md** - How protocol layer fits in overall system
- **07-INTEGRATION-TESTING.md** - Testing protocol equivalence
- **08-PROTOCOL-REFERENCE.md** - Complete message type reference
