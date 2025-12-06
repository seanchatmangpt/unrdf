# Backend API Production Patterns

This document describes the production-ready patterns implemented in the UNRDF knowledge-engine API.

## Table of Contents

1. [Architecture](#architecture)
2. [Middleware Stack](#middleware-stack)
3. [Request/Response Flow](#requestresponse-flow)
4. [Error Handling](#error-handling)
5. [Observability](#observability)
6. [Health Checks](#health-checks)
7. [Validation](#validation)
8. [Examples](#examples)

## Architecture

The knowledge-engine uses Nitro's file-based routing with H3 event handlers:

```
knowledge-engine/server/
├── middleware/          # Global middleware (order: 00, 01, 02, ...)
├── api/                 # API endpoints (auto-routed)
├── utils/               # Shared utilities
└── plugins/             # Runtime plugins
```

## Middleware Stack

Middleware executes in numerical order (00, 01, 02, ...):

### 1. `00.request-id.mjs` - Request ID Tracking
- Generates unique UUID for each request
- Accepts `X-Request-ID` from client
- Propagates to OTEL spans
- Adds to response headers

### 2. `01.telemetry.mjs` - OpenTelemetry
- Creates root span for request
- Adds HTTP attributes
- Tracks request duration

### 3. `02.error-handler.mjs` - Error Handling
- Catches unhandled errors
- Formats OpenAPI-compliant errors
- Records exceptions in OTEL
- Includes trace context

### 4. `04.request-validation.mjs` - Validation Setup
- Adds validation helper to event context
- Provides Zod schema validation

## Request/Response Flow

### Success Response (OpenAPI-compliant)
```json
{
  "success": true,
  "data": { ... },
  "requestId": "uuid-v4",
  "timestamp": "2025-10-02T01:00:00.000Z"
}
```

### Error Response (OpenAPI-compliant)
```json
{
  "success": false,
  "error": {
    "code": "VALIDATION_ERROR",
    "message": "Request validation failed",
    "requestId": "uuid-v4",
    "traceId": "otel-trace-id-32-chars",
    "spanId": "otel-span-id-16-chars",
    "timestamp": "2025-10-02T01:00:00.000Z",
    "path": "/api/hooks/evaluate",
    "issues": [
      {
        "path": "hookId",
        "message": "Hook ID is required",
        "code": "invalid_type"
      }
    ]
  }
}
```

## Error Handling

### Error Types
- **ValidationError** (400) - Zod validation failures
- **UnauthorizedError** (401) - Authentication required
- **ForbiddenError** (403) - Insufficient permissions
- **NotFoundError** (404) - Resource not found
- **InternalError** (500) - Server errors
- **ServiceUnavailable** (503) - Circuit breaker open

### Circuit Breaker Pattern
```javascript
import { circuitBreakerRegistry } from '../utils/circuit-breaker.mjs'

const breaker = circuitBreakerRegistry.get('service-name', {
  failureThreshold: 5,    // Open after 5 failures
  timeout: 60000,         // Wait 60s before retry
  volumeThreshold: 10     // Min requests before calculation
})

const result = await breaker.execute(async () => {
  // Your external call here
})
```

## Observability

### OTEL Trace Context Propagation
All responses include trace correlation:
```javascript
{
  "requestId": "uuid",
  "traceId": "4bf92f3577b34da6a3ce929d0e0e4736",
  "spanId": "00f067aa0ba902b7"
}
```

### Metrics Endpoint
```bash
GET /api/metrics
Content-Type: text/plain; version=0.0.4

# HELP unrdf_circuit_breaker_state Circuit breaker state (0=CLOSED, 1=HALF_OPEN, 2=OPEN)
# TYPE unrdf_circuit_breaker_state gauge
unrdf_circuit_breaker_state{circuit="hook-evaluation"} 0

# HELP unrdf_process_memory_bytes Process memory usage in bytes
# TYPE unrdf_process_memory_bytes gauge
unrdf_process_memory_bytes{type="rss"} 52428800
```

### Traces Endpoint
```bash
GET /api/traces?limit=10&service=unrdf-knowledge-engine
```

## Health Checks

### Liveness Probe (K8s)
```bash
GET /api/health/live
```
Returns 200 if process is alive, 503 if should restart.

Checks:
- Event loop responsive
- Memory not exhausted (&lt;95% heap)

### Readiness Probe (K8s)
```bash
GET /api/health/ready
```
Returns 200 if ready for traffic, 503 if not ready.

Checks:
- Managers initialized
- Circuit breakers healthy
- External dependencies accessible

### Basic Health Check
```bash
GET /api/health
```
Returns comprehensive status including version, uptime, checks.

## Validation

### Zod Schema Validation
```javascript
import { z } from 'zod'
import { sendValidationError } from '../utils/response.mjs'

const MySchema = z.object({
  id: z.string().min(1),
  value: z.number().positive()
})

export default defineEventHandler(async (event) => {
  try {
    const body = await readBody(event)
    const validated = MySchema.parse(body)

    // Use validated data
    return sendSuccess(event, validated)

  } catch (error) {
    if (error.name === 'ZodError') {
      return sendValidationError(event, error)
    }
    throw error
  }
})
```

## Examples

### Complete Endpoint with All Patterns
See `/knowledge-engine/server/api/hooks/evaluate.post.mjs` for a complete example demonstrating:
1. Request ID tracking
2. Zod validation
3. Circuit breaker protection
4. OTEL context propagation
5. OpenAPI-compliant responses

### Testing Endpoints
```bash
# Health check
curl http://localhost:3000/api/health | jq

# Readiness probe
curl http://localhost:3000/api/health/ready | jq

# Liveness probe
curl http://localhost:3000/api/health/live | jq

# Metrics (Prometheus format)
curl http://localhost:3000/api/metrics

# Traces
curl http://localhost:3000/api/traces?limit=5 | jq

# Test validation (should return 400)
curl -X POST http://localhost:3000/api/hooks/evaluate \
  -H "Content-Type: application/json" \
  -d '{"invalid": "data"}' | jq

# Valid request
curl -X POST http://localhost:3000/api/hooks/evaluate \
  -H "Content-Type: application/json" \
  -H "X-Request-ID: test-request-123" \
  -d '{"hookId": "ex:TestHook"}' | jq
```

## Best Practices Checklist

- [x] Request ID tracking on all endpoints
- [x] Zod validation on all POST/PUT endpoints
- [x] Circuit breakers for external calls
- [x] OpenAPI-compliant error responses
- [x] OTEL trace context propagation
- [x] Health/readiness/liveness probes
- [x] Prometheus metrics export
- [x] Structured error logging
- [x] Request/response timestamps
- [x] HTTP status codes per RFC 7231

## Performance Targets

- **P50 latency**: &lt; 50ms (health checks)
- **P50 latency**: &lt; 200ms (hook evaluation)
- **P99 latency**: &lt; 1000ms (all endpoints)
- **Error rate**: &lt; 1% (excluding validation errors)
- **Availability**: &gt; 99.5% (with circuit breakers)

## Security Considerations

1. **Input Validation**: All inputs validated with Zod
2. **Rate Limiting**: Implemented in `03.rate-limit.mjs`
3. **Circuit Breakers**: Prevent cascading failures
4. **MTLS**: Optional mutual TLS validation
5. **RBAC**: Role-based access control via policies
