# Daemon Monitoring Dashboard Guide

## Overview

The UNRDF Daemon Monitoring Dashboard is a real-time Vue 3 + Tailwind CSS application for visualizing daemon performance, cluster status, and operation execution metrics. It provides comprehensive monitoring capabilities for production daemon deployments.

**Features:**
- Real-time operation metrics (success rates, error rates)
- Raft cluster status visualization
- Operation history timeline with filtering
- Error rate sparklines and alerts
- Task queue depth gauging
- Latency percentile charts (P50/P95/P99)
- Detailed operation inspection and debugging

## Installation

### Prerequisites

- Node.js >= 18.0.0
- pnpm >= 7.0.0
- Vue 3.x
- Tailwind CSS 3.x

### Setup Steps

```bash
# Install dependencies
pnpm install

# Add dashboard dependencies to your project
pnpm add -D vue tailwindcss@latest postcss autoprefixer

# Generate Tailwind configuration
npx tailwindcss init -p
```

### Configure Tailwind

Update your `tailwind.config.js`:

```javascript
export default {
  content: [
    './packages/daemon/ui/components/**/*.vue',
  ],
  theme: {
    extend: {
      colors: {
        slate: {
          // Extend with custom slate tones
        },
      },
    },
  },
  plugins: [],
}
```

### Import Components

```javascript
import DaemonDashboard from '@unrdf/daemon/ui/components/DaemonDashboard.vue';
import OperationDetail from '@unrdf/daemon/ui/components/OperationDetail.vue';

export default {
  components: {
    DaemonDashboard,
    OperationDetail,
  },
};
```

## Components

### DaemonDashboard.vue

The main dashboard component providing comprehensive daemon monitoring.

#### Props

```typescript
{
  // Daemon instance with getMetrics() and getHealth() methods
  daemon: Object (required),

  // Configuration including concurrency settings
  config: Object (default: { concurrency: 10 })
}
```

#### Events

- `@operation-selected(operation)` - Emitted when user clicks an operation in history

#### Usage

```vue
<template>
  <DaemonDashboard
    :daemon="daemonInstance"
    :config="daemonConfig"
    @operation-selected="handleOperationSelected"
  />
</template>

<script>
import { ref } from 'vue';
import DaemonDashboard from '@unrdf/daemon/ui/components/DaemonDashboard.vue';
import { Daemon } from '@unrdf/daemon';

export default {
  components: { DaemonDashboard },
  setup() {
    const daemonInstance = new Daemon({ daemonId: 'prod-daemon' });

    return {
      daemonInstance,
      daemonConfig: { concurrency: 20 },
      handleOperationSelected: (op) => console.log('Selected:', op),
    };
  },
};
</script>
```

#### Key Metrics Displayed

1. **Active Operations** - Currently running tasks with progress gauge
2. **Success Rate** - Percentage of successful operations
3. **Average Latency** - P50, P95, P99 percentiles
4. **Total Operations** - All-time operation count with failure count
5. **Latency Percentiles** - Visual comparison of P50/P95/P99
6. **Error Rate Gauge** - Real-time error percentage with alerts
7. **Cluster Status** - Node status and leadership
8. **Queue Depth** - Task queue utilization gauge

#### Health Status Colors

- **Green (Healthy)** - Error rate < 5%, queue normal
- **Yellow (Degraded)** - Error rate 5-10%, elevated queue
- **Red (Unhealthy)** - Error rate > 10%, queue critical

### OperationDetail.vue

Detailed inspection view for individual operations.

#### Props

```typescript
{
  // Complete operation object
  operation: Object (required, default: empty operation)
}
```

#### Events

- `@close` - Emitted when user clicks back button

#### Usage

```vue
<template>
  <div class="grid grid-cols-3">
    <DaemonDashboard
      :daemon="daemon"
      @operation-selected="selectedOp = $event"
    />
    <div v-if="selectedOp" class="col-span-2">
      <OperationDetail
        :operation="selectedOp"
        @close="selectedOp = null"
      />
    </div>
  </div>
</template>

<script>
import { ref } from 'vue';
import DaemonDashboard from '@unrdf/daemon/ui/components/DaemonDashboard.vue';
import OperationDetail from '@unrdf/daemon/ui/components/OperationDetail.vue';

export default {
  components: { DaemonDashboard, OperationDetail },
  setup() {
    const selectedOp = ref(null);
    return { selectedOp };
  },
};
</script>
```

#### Tabs

1. **Metadata** - User-defined operation metadata key-value pairs
2. **Payload** - Original operation input and configuration
3. **Result** - Operation execution output (JSON)
4. **Error Details** - Error code, message, and full stack trace
5. **Execution History** - Retry attempts with timestamps and durations
6. **Proof** - Cryptographic Merkle hash, timestamp, and signature

#### Export Options

- **Copy Details** - Copies full operation JSON to clipboard
- **Export JSON** - Downloads operation as JSON file

## Metric Interpretation

### Success Rate

The percentage of operations that completed successfully without errors.

- **90-100%** - Excellent reliability
- **80-90%** - Good, monitor for degradation
- **70-80%** - Degraded, investigate root cause
- **<70%** - Critical, immediate action needed

### Latency Percentiles

Operation execution time at different percentile thresholds.

```
P50 (Median):    50% of operations complete within this time
P95 (Tail):      95% complete within this time (SLA baseline)
P99 (Extreme):   99% complete within this time (outlier bound)
```

#### Interpretation

```
Healthy pattern:        P50=50ms, P95=100ms, P99=200ms
Degradation:            P50=50ms, P95=500ms, P99=2000ms
Queue backup:           All percentiles increase together
Memory leak:            Percentiles increase over time
```

### Error Rate Alerts

- **<1%** - Excellent
- **1-5%** - Good (monitor)
- **5-10%** - Warning (investigate)
- **>10%** - Critical (immediate action)

### Queue Depth

Measures number of operations waiting to execute.

```
Healthy:      < 10 tasks
Warning:      10-50 tasks (concurrency may be insufficient)
Critical:     > 50 tasks (backpressure detected)
```

**Root causes:**
- Insufficient concurrency limit
- Downstream service degradation
- Resource exhaustion (CPU, memory, I/O)

### Cluster Status

Raft consensus cluster health.

- **Leader** (👑) - Current cluster leader
- **Follower** (✓) - Healthy replica
- **Unhealthy** (◇) - Node disconnected or unresponsive

## Troubleshooting

### High Error Rate (>10%)

**Symptoms:**
- Dashboard shows red health status
- Error rate alert appears
- Failed operation count increasing

**Investigation Steps:**

1. Click a failed operation in history
2. Go to "Error Details" tab
3. Review error code and message
4. Check stack trace for root cause
5. Verify error is consistent or intermittent

**Common causes:**
- Downstream service unavailable
- Invalid input data
- Resource constraints
- Configuration error

**Resolution:**
```javascript
// Check daemon health
const health = daemon.getHealth();
console.log('Active ops:', health.activeOperations);
console.log('Queued ops:', health.queuedOperations);

// Reduce concurrency if overwhelmed
daemon.config.concurrency = 5;
```

### High Latency (P99 > 1000ms)

**Symptoms:**
- Orange/red latency percentile chart
- Operations taking longer to complete
- Possible timeout failures

**Investigation Steps:**

1. Monitor individual operation duration in history
2. Check if latency is increasing over time (memory leak?)
3. Review system resources (CPU, memory, I/O)
4. Analyze database query performance

**Common causes:**
- High load on downstream services
- Memory pressure (GC pauses)
- Disk I/O bottleneck
- Network latency

**Resolution:**
```javascript
// Profile operation duration
daemon.on('operation:success', ({ duration }) => {
  if (duration > 500) {
    console.warn('Slow operation:', duration, 'ms');
  }
});
```

### Queue Backed Up (>50 tasks)

**Symptoms:**
- Queue depth gauge shows red
- Active operations capped at concurrency limit
- Operations waiting indefinitely

**Investigation Steps:**

1. Check cluster status - any unhealthy nodes?
2. Monitor downstream service health
3. Review operation payload sizes
4. Check system resource usage

**Common causes:**
- Concurrency limit too low
- Downstream service slow/unavailable
- Operation processing time increasing
- Memory or file descriptor limits

**Resolution:**
```javascript
// Increase concurrency if resources available
const config = {
  ...daemon.config,
  concurrency: 20,  // From 10 to 20
};

// Or batch smaller operations
daemon.config.batchSize = 5;
```

### Cluster Node Unhealthy

**Symptoms:**
- Cluster status shows unhealthy node (◇)
- Node disconnects and reconnects frequently
- Operations may fail over

**Investigation Steps:**

1. Check node logs for network errors
2. Verify network connectivity to other nodes
3. Check system resources on unhealthy node
4. Review Raft consensus logs

**Common causes:**
- Network partition
- Node process crash
- Resource exhaustion
- DNS resolution issues

**Resolution:**
```javascript
// Force leader election if needed
await daemon.triggerElection();

// Or remove unhealthy node
daemon.removeClusterMember('node-id');
```

## Performance Tuning

### Dashboard Refresh Interval

Default: 5 seconds

```javascript
// Adjust in component
const refreshInterval = setInterval(refreshMetrics, 3000); // 3s
```

### Metrics Retention

Control how long metrics history is kept.

```javascript
const config = {
  metricsRetentionMs: 3600000, // 1 hour
  // Adjust based on storage constraints
};
```

### Queue Monitoring Thresholds

```javascript
const isQueueHealthy = (queuedTasks) => {
  return queuedTasks < 20; // Adjust based on SLA
};
```

## Best Practices

### 1. Set Up Alerting

```javascript
// Monitor error rate
daemon.on('metrics:updated', ({ errorRate }) => {
  if (errorRate > 5) {
    sendAlert('High error rate: ' + errorRate);
  }
});

// Monitor queue depth
daemon.on('health:updated', ({ queuedOperations }) => {
  if (queuedOperations > 50) {
    sendAlert('Queue backing up: ' + queuedOperations);
  }
});
```

### 2. Regular Health Checks

```bash
# Every 5 minutes
*/5 * * * * curl http://localhost:8080/health

# Export metrics
curl http://localhost:8080/metrics > metrics.json
```

### 3. Log Failed Operations

```javascript
daemon.on('operation:failure', ({ operationId, error }) => {
  logger.error('Operation failed', { operationId, error });
  // Store for post-mortem analysis
});
```

### 4. Monitor Cluster Stability

```javascript
// Track leader changes
daemon.on('cluster:leader-changed', ({ newLeader }) => {
  logger.info('Leader election:', newLeader);
});

// Alert on member changes
daemon.on('cluster:member-left', ({ nodeId }) => {
  logger.warn('Cluster member left:', nodeId);
});
```

## Integration with Monitoring Systems

### Prometheus Metrics

Export metrics for Prometheus scraping:

```javascript
app.get('/metrics', (req, res) => {
  const metrics = daemon.getMetrics();
  const health = daemon.getHealth();

  res.set('Content-Type', 'text/plain');
  res.send(`
    daemon_operations_total ${metrics.totalOperations}
    daemon_operations_success ${metrics.successfulOperations}
    daemon_operations_failed ${metrics.failedOperations}
    daemon_latency_p50 ${metrics.latency.p50}
    daemon_latency_p95 ${metrics.latency.p95}
    daemon_latency_p99 ${metrics.latency.p99}
    daemon_queue_depth ${health.queuedOperations}
  `);
});
```

### Grafana Dashboards

Import dashboard JSON for Grafana:

```json
{
  "dashboard": {
    "title": "UNRDF Daemon",
    "panels": [
      {
        "title": "Error Rate",
        "targets": [
          { "expr": "rate(daemon_operations_failed[5m])" }
        ]
      },
      {
        "title": "Latency P95",
        "targets": [
          { "expr": "daemon_latency_p95" }
        ]
      }
    ]
  }
}
```

### ELK Stack Integration

```javascript
// Ship logs to Elasticsearch
const winston = require('winston');
const ElasticsearchTransport = require('winston-elasticsearch');

const logger = winston.createLogger({
  transports: [
    new ElasticsearchTransport({
      level: 'info',
      clientOpts: { node: 'http://elasticsearch:9200' },
      index: 'daemon-logs',
    }),
  ],
});
```

## Security Considerations

### 1. Authentication

Protect dashboard access with authentication:

```javascript
app.use('/dashboard', requireAuth);
```

### 2. Data Sanitization

Never expose sensitive data in operation payloads:

```javascript
// Bad
payload: { apiKey: 'secret-key', data: {...} }

// Good
payload: { dataId: 'abc-123', data: {...} }
// Store apiKey in secure vault
```

### 3. Operation History Retention

Limit history retention to comply with data retention policies:

```javascript
const config = {
  maxHistoryEntries: 10000,
  retentionDays: 30,
};
```

## Conclusion

The Daemon Monitoring Dashboard provides comprehensive visibility into daemon operation and cluster health. By understanding metrics, interpreting status indicators, and following troubleshooting procedures, you can maintain reliable, high-performance daemon deployments.

For more information:
- [UNRDF Documentation](https://unrdf.org)
- [Daemon API Reference](./daemon-api.md)
- [Performance Tuning Guide](./performance-tuning.md)
