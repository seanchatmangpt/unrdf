# Backend API Quick Reference

## New Endpoints

### Health & Observability

| Endpoint | Method | Purpose | K8s Probe |
|----------|--------|---------|-----------|
| `/api/health` | GET | Basic health check | - |
| `/api/health/ready` | GET | Readiness status | readinessProbe |
| `/api/health/live` | GET | Liveness status | livenessProbe |
| `/api/metrics` | GET | Prometheus metrics | - |
| `/api/traces` | GET | OTEL trace query | - |

### Example Health Check (K8s YAML)
```yaml
apiVersion: v1
kind: Pod
metadata:
  name: unrdf-knowledge-engine
spec:
  containers:
  - name: knowledge-engine
    image: unrdf/knowledge-engine:latest
    ports:
    - containerPort: 3000
    livenessProbe:
      httpGet:
        path: /api/health/live
        port: 3000
      initialDelaySeconds: 30
      periodSeconds: 10
      timeoutSeconds: 5
      failureThreshold: 3
    readinessProbe:
      httpGet:
        path: /api/health/ready
        port: 3000
      initialDelaySeconds: 10
      periodSeconds: 5
      timeoutSeconds: 3
      failureThreshold: 3
```

## New Middleware

1. **00.request-id.mjs** - Request ID tracking
2. **04.request-validation.mjs** - Zod validation infrastructure

## Response Formats

### Success (All endpoints now include)
```json
{
  "success": true,
  "data": { ... },
  "requestId": "uuid-v4",
  "timestamp": "ISO8601"
}
```

### Error (OpenAPI-compliant)
```json
{
  "success": false,
  "error": {
    "code": "ERROR_CODE",
    "message": "Description",
    "requestId": "uuid-v4",
    "traceId": "otel-trace-id",
    "spanId": "otel-span-id",
    "timestamp": "ISO8601",
    "path": "/api/endpoint"
  }
}
```

## Validation Pattern

```javascript
import { z } from 'zod'
import { validateRequest } from '../../middleware/04.request-validation.mjs'

const MySchema = z.object({
  id: z.string().min(1),
  count: z.number().int().positive()
})

export default defineEventHandler(async (event) => {
  const validated = await event.context.validate(MySchema, 'body')
  // Use validated data
})
```

## Circuit Breaker Pattern

```javascript
import { circuitBreakerRegistry } from '../../utils/circuit-breaker.mjs'

const breaker = circuitBreakerRegistry.get('my-service')
const result = await breaker.execute(async () => {
  // External call here
})
```

## Testing Commands

```bash
# Basic health
curl http://localhost:3000/api/health | jq

# Readiness (for K8s)
curl http://localhost:3000/api/health/ready | jq

# Liveness (for K8s)
curl http://localhost:3000/api/health/live | jq

# Metrics (Prometheus)
curl http://localhost:3000/api/metrics

# Traces (with request ID)
curl -H "X-Request-ID: test-123" \
  http://localhost:3000/api/traces | jq

# Example POST with validation
curl -X POST http://localhost:3000/api/hooks/evaluate \
  -H "Content-Type: application/json" \
  -H "X-Request-ID: demo-request" \
  -d '{
    "hookId": "ex:MyHook",
    "data": "@prefix ex: <http://example.org/> ."
  }' | jq
```

## Performance Targets

- Health checks: &lt; 50ms P50
- Hook evaluation: &lt; 200ms P50
- Error rate: &lt; 1%
- Circuit breaker availability: &gt; 99.5%

## Key Files

### Middleware (Execution Order)
```
00.request-id.mjs       → Request tracking
00.auth.mjs             → Authentication
01.telemetry.mjs        → OTEL spans
02.error-handler.mjs    → Error handling
04.request-validation.mjs → Validation setup
```

### API Endpoints
```
api/health.get.mjs           → Basic health
api/health/ready.get.mjs     → K8s readiness
api/health/live.get.mjs      → K8s liveness
api/metrics.get.mjs          → Prometheus metrics
api/traces.get.mjs           → OTEL traces
api/hooks/evaluate.post.mjs  → Example endpoint
```

### Utilities
```
utils/response.mjs              → OpenAPI responses
utils/circuit-breaker.mjs       → Circuit breaker
utils/otel-context-propagation.mjs → Trace context
```

## 80/20 Principle Applied

**20% of features handling 80% of production needs:**

1. **Health checks** → K8s orchestration
2. **Request IDs** → Distributed tracing
3. **Validation** → Input safety
4. **Circuit breakers** → Resilience
5. **Metrics** → Observability
