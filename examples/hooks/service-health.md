---
hook:
  id: 'ex:ServiceHealthMonitor'
  name: 'Service Health Monitor'
  description: 'Monitors service error rates and latency thresholds'
  select: 'SELECT ?service ?errorRate ?latency WHERE { ?service <http://example.org/errorRate> ?errorRate ; <http://example.org/latency> ?latency }'
  predicates:
    - kind: 'THRESHOLD'
      spec:
        var: 'errorRate'
        op: '>'
        value: 0.02
    - kind: 'THRESHOLD'
      spec:
        var: 'latency'
        op: '>'
        value: 1000
  combine: 'OR'
  effect: |
    async ({ rows, receipt, store, engine }) => {
      console.log('🚨 HEALTH ALERT: Service performance degraded!')
      rows.forEach(row => {
        const service = row.get('service')?.value
        const errorRate = row.get('errorRate')?.value
        const latency = row.get('latency')?.value
        console.log(`  Service: ${service}`)
        console.log(`  Error Rate: ${errorRate} (threshold: 0.02)`)
        console.log(`  Latency: ${latency}ms (threshold: 1000ms)`)
      })
    }
---

# Service Health Monitor Hook

This Knowledge Hook monitors service health metrics and triggers alerts when performance degrades.

## Configuration

- **Error Rate Threshold**: 2% (0.02)
- **Latency Threshold**: 1000ms
- **Logic**: OR (triggers if either threshold is exceeded)
- **Schedule**: Every 5 minutes

## Predicates

### THRESHOLD Predicates

1. **Error Rate Check**: Monitors `ex:errorRate` property
   - Operator: `>` (greater than)
   - Value: `0.02` (2%)

2. **Latency Check**: Monitors `ex:latency` property
   - Operator: `>` (greater than)
   - Value: `1000` (1000ms)

## Effect Function

When the hook fires, it will:

- Log a health alert message
- Display detailed metrics for each affected service
- Show current values vs. thresholds

## Usage

```bash
# Load and evaluate the hook
unrdf hook eval --hook service-health.md --graph data/

# Or use programmatically
const hook = await loadFrontmatterHook('service-health.md')
const receipt = await evaluateHook(hook, { store, engine })
```

## Sample Data

The hook expects RDF data in this format:

```turtle
@prefix ex: <http://example.org/> .

ex:service1 a ex:Service ;
  ex:errorRate 0.01 ;
  ex:latency 500 .

ex:service2 a ex:Service ;
  ex:errorRate 0.03 ;
  ex:latency 1200 .
```
