# Service Health Monitoring Hook

This example demonstrates a Knowledge Hook for monitoring service health metrics including error rates and latency.

## Hook Definition

```json
{
  "id": "ex:ServiceHealthMonitor",
  "name": "Critical Service Health Monitor",
  "description": "Monitors service error rates and latency for critical services",
  "select": "SELECT ?service ?errorRate ?latency ?critical WHERE { ?service ex:errorRate ?errorRate ; ex:latency ?latency . OPTIONAL { ?service ex:critical ?critical } }",
  "predicates": [
    {
      "kind": "THRESHOLD",
      "spec": {
        "var": "errorRate",
        "op": ">",
        "value": 0.02
      }
    },
    {
      "kind": "THRESHOLD",
      "spec": {
        "var": "latency",
        "op": ">",
        "value": 2000
      }
    },
    {
      "kind": "ASK",
      "spec": {
        "query": "ASK WHERE { ?service ex:critical true }",
        "expected": true
      }
    }
  ],
  "combine": "OR",
  "output": {
    "schema": {
      "type": "object",
      "properties": {
        "service": { "type": "string" },
        "alert": { "type": "string" },
        "severity": {
          "type": "string",
          "enum": ["warning", "critical"]
        },
        "metrics": {
          "type": "object",
          "properties": {
            "errorRate": { "type": "number" },
            "latency": { "type": "number" }
          }
        }
      },
      "required": ["service", "alert", "severity"]
    },
    "format": "json",
    "destination": "webhook",
    "webhook": {
      "url": "https://api.example.com/alerts",
      "method": "POST",
      "headers": {
        "Authorization": "Bearer your-token",
        "Content-Type": "application/json"
      }
    }
  }
}
```

## Sample Data

```turtle
@prefix ex: <http://example.org/> .
@prefix xsd: <http://www.w3.org/2001/XMLSchema#> .

# Normal service
ex:service1 a ex:Service ;
  ex:errorRate 0.01 ;
  ex:latency 1500 .

# Service with high error rate
ex:service2 a ex:Service ;
  ex:errorRate 0.05 ;
  ex:latency 1800 ;
  ex:critical true .

# Service with high latency
ex:service3 a ex:Service ;
  ex:errorRate 0.01 ;
  ex:latency 2500 .

# Critical service with issues
ex:service4 a ex:Service ;
  ex:errorRate 0.08 ;
  ex:latency 3000 ;
  ex:critical true .
```

## CLI Usage

### Create the hook
```bash
unrdf hook create --template threshold --output hooks/service-health.json
```

### Edit the hook to match the definition above
```bash
# Edit hooks/service-health.json to include the full definition
```

### Load sample data
```bash
cat > sample-data.ttl << 'EOF'
@prefix ex: <http://example.org/> .
@prefix xsd: <http://www.w3.org/2001/XMLSchema#> .

ex:service1 a ex:Service ;
  ex:errorRate 0.01 ;
  ex:latency 1500 .

ex:service2 a ex:Service ;
  ex:errorRate 0.05 ;
  ex:latency 1800 ;
  ex:critical true .

ex:service3 a ex:Service ;
  ex:errorRate 0.01 ;
  ex:latency 2500 .

ex:service4 a ex:Service ;
  ex:errorRate 0.08 ;
  ex:latency 3000 ;
  ex:critical true .
EOF
```

### Evaluate the hook
```bash
unrdf hook eval --hook hooks/service-health.json --data sample-data.ttl
```

## Expected Output

```json
{
  "id": "ex:ServiceHealthMonitor",
  "fired": true,
  "predicates": [
    {
      "kind": "THRESHOLD",
      "ok": true,
      "meta": {
        "matched": 2,
        "threshold": 0.02,
        "maxValue": 0.08
      },
      "duration": 12
    },
    {
      "kind": "THRESHOLD",
      "ok": true,
      "meta": {
        "matched": 2,
        "threshold": 2000,
        "maxValue": 3000
      },
      "duration": 8
    },
    {
      "kind": "ASK",
      "ok": true,
      "meta": {
        "matched": 2
      },
      "duration": 4
    }
  ],
  "durations": {
    "totalMs": 28,
    "queryMs": 6,
    "predicateMs": 18,
    "canonicalizationMs": 4
  },
  "provenance": {
    "hookHash": "sha256:abc123...",
    "queryHash": "sha256:def456...",
    "graphHash": "sha256:ghi789...",
    "receiptHash": "sha256:jkl012..."
  },
  "at": "2024-01-01T10:00:00.000Z",
  "input": {
    "bindings": 4,
    "variables": ["service", "errorRate", "latency", "critical"]
  }
}
```

## Explanation

This hook will fire when:
1. Any service has an error rate > 2% (service2, service4)
2. Any service has latency > 2000ms (service3, service4)
3. Any critical service has issues (service2, service4)

The hook fires because service2, service3, and service4 all have issues, and the OR combination logic means any of these conditions will trigger the hook.

## Webhook Payload

If the hook fires, it will send a webhook payload like:

```json
{
  "service": "ex:service4",
  "alert": "Critical service with high error rate and latency",
  "severity": "critical",
  "metrics": {
    "errorRate": 0.08,
    "latency": 3000
  }
}
```

## Performance Considerations

- **Query Optimization**: Uses specific variable selection for efficiency
- **Predicate Efficiency**: ASK predicate is fastest for existence checks
- **Result Limiting**: No LIMIT needed since we want to monitor all services
- **Indexing**: Consider indexing on errorRate and latency for large datasets

## Monitoring Strategy

This hook provides a comprehensive service health monitoring strategy:

1. **Proactive Detection**: Identifies issues before they become critical
2. **Prioritized Alerts**: Focuses on critical services that need immediate attention
3. **Comprehensive Coverage**: Monitors both error rates and latency
4. **Cryptographic Proof**: All evaluations are cryptographically signed for audit trails

## Customization

To customize this hook for your environment:

1. **Adjust Thresholds**: Modify error rate and latency thresholds based on your SLAs
2. **Add More Metrics**: Include additional metrics like CPU usage, memory usage
3. **Change Criticality Logic**: Modify the critical service detection logic
4. **Update Output Schema**: Add or modify the output schema to match your alerting system
