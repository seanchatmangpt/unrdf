# How-To: YAWL-Nitro Tasks

**Time Required**: 5-10 minutes per task
**Difficulty**: Intermediate
**Prerequisites**: Completed [YAWL-Nitro Integration Tutorial](/docs/diataxis/tutorials/yawl-nitro-integration.md)

Quick recipes for common YAWL-Nitro integration tasks.

---

## Table of Contents

1. [Schedule a Recurring Workflow](#schedule-a-recurring-workflow)
2. [Enforce Task Timeouts](#enforce-task-timeouts)
3. [Add Automatic Retry Logic](#add-automatic-retry-logic)
4. [Transform Payloads Between Systems](#transform-payloads-between-systems)
5. [Monitor Task Execution](#monitor-task-execution)
6. [Handle Task Failures](#handle-task-failures)
7. [Validate Tasks Before Execution](#validate-tasks-before-execution)
8. [Execute Tasks Manually via API](#execute-tasks-manually-via-api)
9. [Export Metrics to Prometheus](#export-metrics-to-prometheus)
10. [Deploy with Docker](#deploy-with-docker)

---

## Schedule a Recurring Workflow

**Goal**: Run a workflow case every hour

### Step 1: Create Workflow

```javascript
import { createWorkflow } from '@unrdf/yawl';
import { createStore } from '@unrdf/oxigraph';

const store = createStore();
await createWorkflow(store, {
  id: 'hourly-sync',
  name: 'Hourly Data Sync',
  tasks: [
    { id: 'fetch', name: 'Fetch Data', kind: 'atomic' },
    { id: 'process', name: 'Process Data', kind: 'atomic' },
    { id: 'store', name: 'Store Results', kind: 'atomic' },
  ],
  flow: [
    { from: 'fetch', to: 'process' },
    { from: 'process', to: 'store' },
  ],
});
```

### Step 2: Register Operation

```javascript
import { Daemon } from '@unrdf/daemon';

const daemon = new Daemon({ daemonId: crypto.randomUUID(), name: 'sync-daemon' });
const opId = crypto.randomUUID();

daemon.schedule({
  id: opId,
  name: 'hourly-sync-operation',
  handler: async (context = {}) => {
    // Your sync logic here
    return { synced: true, timestamp: new Date() };
  },
});
```

### Step 3: Register as Nitro Task with Cron

```javascript
import { createNitroTaskExecutor } from '@unrdf/daemon/integrations/nitro-tasks';

const executor = createNitroTaskExecutor(daemon, { autoStart: true });

executor.registerOperationAsTask(opId, 'sync:hourly', {
  description: 'Hourly data synchronization',
  cronExpression: '0 * * * *', // Every hour
  priority: 'high',
  tags: ['sync', 'recurring'],
});
```

**Cron Examples:**
- `'*/5 * * * *'` - Every 5 minutes
- `'0 */6 * * *'` - Every 6 hours
- `'0 2 * * *'` - Daily at 2 AM
- `'0 0 * * 0'` - Weekly on Sunday midnight

**Result**: Task executes automatically every hour.

---

## Enforce Task Timeouts

**Goal**: Cancel tasks that run longer than 30 seconds

### Method 1: Global Timeout (All Tasks)

```javascript
const executor = createNitroTaskExecutor(daemon, {
  timeout: 30000, // 30 seconds for ALL tasks
  maxRetries: 3,
});
```

### Method 2: Per-Task Timeout

```javascript
// Configure operation with timeout metadata
daemon.schedule({
  id: opId,
  name: 'api-call',
  handler: async (context = {}) => {
    const controller = new AbortController();
    const timeout = setTimeout(() => controller.abort(), 30000);

    try {
      const response = await fetch('https://api.example.com/data', {
        signal: controller.signal,
      });
      clearTimeout(timeout);
      return await response.json();
    } catch (error) {
      clearTimeout(timeout);
      if (error.name === 'AbortError') {
        throw new Error('Task timeout after 30s');
      }
      throw error;
    }
  },
  metadata: { timeout: 30000 },
});
```

### Method 3: Executor Timeout Wrapper

```javascript
// Custom timeout wrapper
async function executeWithTimeout(taskId, payload, timeoutMs = 30000) {
  const timeoutPromise = new Promise((_, reject) =>
    setTimeout(() => reject(new Error(`Task timeout: ${timeoutMs}ms`)), timeoutMs)
  );

  const taskPromise = executor.runTask(taskId, payload);

  try {
    return await Promise.race([taskPromise, timeoutPromise]);
  } catch (error) {
    console.error(`Task ${taskId} failed:`, error.message);
    throw error;
  }
}

// Usage
await executeWithTimeout('daemon:my-task', { data: 'test' }, 15000);
```

**Result**: Tasks automatically cancelled if exceeding time limit.

---

## Add Automatic Retry Logic

**Goal**: Retry failed tasks 3 times with exponential backoff

### Step 1: Enable Retry in Executor

```javascript
const executor = createNitroTaskExecutor(daemon, {
  maxRetries: 3, // Retry up to 3 times
  timeout: 30000,
});
```

### Step 2: Configure Retry Policy

```javascript
const executor = createNitroTaskExecutor(daemon, {
  maxRetries: 5,
  retryPolicy: {
    backoffMs: 1000, // Start at 1s
    backoffMultiplier: 2, // Double each time: 1s, 2s, 4s, 8s, 16s
    maxBackoffMs: 30000, // Cap at 30s
    jitterFactor: 0.1, // Add 10% randomness
  },
});
```

### Step 3: Listen for Retry Events

```javascript
executor.on('task:failed', (event) => {
  console.log(`Task ${event.nitroTaskId} failed (attempt ${event.attempt || 1})`);
  console.log(`Error: ${event.error}`);
  console.log(`Next retry in: ${event.nextRetryIn || 'N/A'}ms`);
});

executor.on('task:retry-exhausted', (event) => {
  console.error(`Task ${event.nitroTaskId} exhausted retries (${event.attempts} attempts)`);
  // Implement fallback or alert
});
```

### Step 4: Implement Retry-Safe Handler

```javascript
daemon.schedule({
  id: opId,
  name: 'retryable-operation',
  handler: async (context = {}) => {
    const { attempt = 1 } = context;

    console.log(`Attempt ${attempt}: Processing...`);

    // Idempotent operation (safe to retry)
    const result = await database.upsert({
      id: context.recordId,
      data: context.data,
      updatedAt: new Date(),
    });

    return { success: true, attempt, result };
  },
});
```

**Result**: Failed tasks automatically retry with backoff.

---

## Transform Payloads Between Systems

**Goal**: Convert Nitro payload format to YAWL workflow format

### Step 1: Define Transformation Schemas

```javascript
import { z } from 'zod';

// Nitro task payload schema
const NitroPayloadSchema = z.object({
  task_id: z.string(),
  user_id: z.string(),
  parameters: z.record(z.string(), z.any()),
  scheduled_at: z.string().datetime(),
});

// YAWL workflow context schema
const YawlContextSchema = z.object({
  caseId: z.string(),
  taskId: z.string(),
  userId: z.string(),
  data: z.record(z.string(), z.any()),
  timestamp: z.date(),
});
```

### Step 2: Create Transformation Functions

```javascript
/**
 * Transform Nitro payload to YAWL context
 * @param {Object} nitroPayload - Nitro task payload
 * @returns {Object} YAWL workflow context
 */
function nitroToYawl(nitroPayload) {
  const validated = NitroPayloadSchema.parse(nitroPayload);

  return {
    caseId: `case-${validated.task_id}`,
    taskId: validated.task_id,
    userId: validated.user_id,
    data: validated.parameters,
    timestamp: new Date(validated.scheduled_at),
  };
}

/**
 * Transform YAWL result to Nitro response
 * @param {Object} yawlResult - YAWL task result
 * @returns {Object} Nitro task response
 */
function yawlToNitro(yawlResult) {
  return {
    success: yawlResult.status === 'completed',
    task_id: yawlResult.taskId,
    result: yawlResult.outputData,
    completed_at: yawlResult.completedAt?.toISOString(),
    duration_ms: yawlResult.duration,
  };
}
```

### Step 3: Use Transformations in Handler

```javascript
daemon.schedule({
  id: opId,
  name: 'transform-operation',
  handler: async (nitroPayload) => {
    // Transform incoming Nitro payload
    const yawlContext = nitroToYawl(nitroPayload);

    // Process with YAWL workflow
    const yawlResult = await processWorkflow(yawlContext);

    // Transform result back to Nitro format
    return yawlToNitro(yawlResult);
  },
});
```

**Result**: Seamless payload transformation between systems.

---

## Monitor Task Execution

**Goal**: Track task performance and failures in real-time

### Step 1: Set Up Event Listeners

```javascript
// Track all task events
executor.on('task:started', (event) => {
  console.log(`[${new Date().toISOString()}] START: ${event.nitroTaskId}`);
});

executor.on('task:succeeded', (event) => {
  console.log(`[${new Date().toISOString()}] SUCCESS: ${event.nitroTaskId} (${event.duration}ms)`);
});

executor.on('task:failed', (event) => {
  console.error(`[${new Date().toISOString()}] FAILED: ${event.nitroTaskId}`);
  console.error(`  Error: ${event.error}`);
});
```

### Step 2: Generate Metrics Report

```javascript
function printMetrics() {
  const metrics = executor.getMetrics();

  console.log('\n📊 Execution Metrics:');
  console.log(`  Total Executed:  ${metrics.tasksExecuted}`);
  console.log(`  Succeeded:       ${metrics.tasksSucceeded}`);
  console.log(`  Failed:          ${metrics.tasksFailed}`);
  console.log(`  Success Rate:    ${((metrics.tasksSucceeded / metrics.tasksExecuted) * 100 || 0).toFixed(1)}%`);
  console.log(`  Avg Duration:    ${metrics.averageDuration.toFixed(0)}ms`);
  console.log(`  Total Duration:  ${metrics.totalDuration}ms\n`);
}

// Print every 60 seconds
setInterval(printMetrics, 60000);
```

### Step 3: Export Execution History

```javascript
function exportHistory() {
  const history = executor.getExecutionHistory();

  const csv = [
    'Timestamp,Task ID,Success,Duration (ms),Error',
    ...history.map((entry) =>
      [
        entry.timestamp.toISOString(),
        entry.taskId,
        entry.success,
        entry.duration || 0,
        entry.error || '',
      ].join(',')
    ),
  ].join('\n');

  // Write to file or send to analytics
  return csv;
}
```

**Result**: Real-time visibility into task execution.

---

## Handle Task Failures

**Goal**: Implement graceful error handling and recovery

### Step 1: Categorize Errors

```javascript
class RetryableError extends Error {
  constructor(message) {
    super(message);
    this.name = 'RetryableError';
    this.retryable = true;
  }
}

class NonRetryableError extends Error {
  constructor(message) {
    super(message);
    this.name = 'NonRetryableError';
    this.retryable = false;
  }
}
```

### Step 2: Implement Error Handler

```javascript
daemon.schedule({
  id: opId,
  name: 'resilient-operation',
  handler: async (context = {}) => {
    try {
      // Attempt operation
      const result = await riskyOperation(context);
      return { success: true, result };
    } catch (error) {
      // Categorize error
      if (error.code === 'ECONNREFUSED' || error.code === 'ETIMEDOUT') {
        // Network errors - retryable
        throw new RetryableError(`Network error: ${error.message}`);
      } else if (error.code === 'INVALID_INPUT') {
        // Bad input - not retryable
        throw new NonRetryableError(`Invalid input: ${error.message}`);
      } else if (error.code === 'PERMISSION_DENIED') {
        // Access denied - not retryable
        return { success: false, reason: 'permission-denied', error: error.message };
      } else {
        // Unknown error - log and retry
        console.error('Unknown error:', error);
        throw new RetryableError(`Unexpected error: ${error.message}`);
      }
    }
  },
});
```

### Step 3: Set Up Failure Recovery

```javascript
executor.on('task:failed', async (event) => {
  const { nitroTaskId, error, daemonOperationId } = event;

  // Check if retryable
  if (error.includes('RetryableError')) {
    console.log(`Retrying ${nitroTaskId}...`);
    // Executor will retry automatically based on maxRetries config
    return;
  }

  // Non-retryable - implement recovery
  console.error(`Permanent failure for ${nitroTaskId}`);

  // Recovery strategies:
  // 1. Alert operations team
  await sendAlert({
    severity: 'critical',
    message: `Task ${nitroTaskId} failed permanently`,
    error,
  });

  // 2. Log to database
  await database.logFailure({
    taskId: nitroTaskId,
    operationId: daemonOperationId,
    error,
    timestamp: new Date(),
  });

  // 3. Execute fallback operation
  await executeFallback(daemonOperationId);
});
```

**Result**: Robust error handling with automatic recovery.

---

## Validate Tasks Before Execution

**Goal**: Pre-validate task inputs and system state

### Step 1: Implement Validation Function

```javascript
import { z } from 'zod';

const TaskPayloadSchema = z.object({
  documentId: z.string().uuid(),
  action: z.enum(['submit', 'review', 'approve']),
  userId: z.string().email(),
  data: z.record(z.string(), z.any()).optional(),
});

function validateTaskPayload(payload) {
  const result = TaskPayloadSchema.safeParse(payload);

  if (!result.success) {
    return {
      valid: false,
      errors: result.error.format(),
    };
  }

  return { valid: true, data: result.data };
}
```

### Step 2: Use Executor Validation

```javascript
const validation = executor.validateTask('daemon:my-task');

if (!validation.valid) {
  console.error('Task validation failed:', validation.reason);
  throw new Error(`Cannot execute: ${validation.reason}`);
}
```

### Step 3: Pre-Flight Checks in Handler

```javascript
daemon.schedule({
  id: opId,
  name: 'validated-operation',
  handler: async (context = {}) => {
    // Validate payload
    const validation = validateTaskPayload(context);
    if (!validation.valid) {
      throw new Error(`Validation failed: ${JSON.stringify(validation.errors)}`);
    }

    // Check system prerequisites
    const daemonHealthy = daemon.getHealth().isRunning;
    const sufficientMemory = process.memoryUsage().heapUsed < 500 * 1024 * 1024; // 500MB

    if (!daemonHealthy) {
      throw new Error('Daemon not healthy');
    }

    if (!sufficientMemory) {
      throw new Error('Insufficient memory');
    }

    // Proceed with operation
    return await performOperation(validation.data);
  },
});
```

**Result**: Tasks only execute with valid inputs and healthy system state.

---

## Execute Tasks Manually via API

**Goal**: Trigger tasks on-demand via HTTP endpoint

### Step 1: Create API Handler (Express)

```javascript
import express from 'express';

const app = express();
app.use(express.json());

app.post('/api/tasks/:taskId/execute', async (req, res) => {
  const { taskId } = req.params;
  const payload = req.body;

  try {
    // Validate task exists
    const validation = executor.validateTask(taskId);
    if (!validation.valid) {
      return res.status(404).json({
        error: 'Task not found',
        reason: validation.reason,
      });
    }

    // Execute task
    const result = await executor.runTask(taskId, payload);

    res.status(200).json({
      success: true,
      taskId,
      result: result.result,
      duration: result.duration,
    });
  } catch (error) {
    res.status(500).json({
      success: false,
      error: error.message,
    });
  }
});

app.listen(3000, () => console.log('API listening on :3000'));
```

### Step 2: Create API Handler (h3/Nitro)

```javascript
// server/api/tasks/[taskId]/execute.post.js
export default defineEventHandler(async (event) => {
  const taskId = getRouterParam(event, 'taskId');
  const payload = await readBody(event);

  // Validate
  const validation = executor.validateTask(taskId);
  if (!validation.valid) {
    throw createError({
      statusCode: 404,
      message: `Task not found: ${validation.reason}`,
    });
  }

  try {
    const result = await executor.runTask(taskId, payload);
    return {
      success: true,
      taskId,
      result: result.result,
      duration: result.duration,
    };
  } catch (error) {
    throw createError({
      statusCode: 500,
      message: `Task execution failed: ${error.message}`,
    });
  }
});
```

### Step 3: Test API

```bash
# Execute task via cURL
curl -X POST http://localhost:3000/api/tasks/daemon:my-task/execute \
  -H "Content-Type: application/json" \
  -d '{"documentId": "DOC-123", "action": "submit"}'
```

**Result**: Tasks executable via HTTP API.

---

## Export Metrics to Prometheus

**Goal**: Send executor metrics to Prometheus for monitoring

### Step 1: Install Prometheus Client

```bash
pnpm add prom-client
```

### Step 2: Create Metrics Exporter

```javascript
import { Registry, Counter, Histogram, Gauge } from 'prom-client';

const register = new Registry();

// Create metrics
const tasksExecutedCounter = new Counter({
  name: 'nitro_tasks_executed_total',
  help: 'Total number of tasks executed',
  labelNames: ['task_id', 'status'],
  registers: [register],
});

const taskDurationHistogram = new Histogram({
  name: 'nitro_task_duration_seconds',
  help: 'Task execution duration',
  labelNames: ['task_id'],
  buckets: [0.01, 0.05, 0.1, 0.5, 1, 5, 10],
  registers: [register],
});

const activeTasksGauge = new Gauge({
  name: 'nitro_active_tasks',
  help: 'Number of currently active tasks',
  registers: [register],
});
```

### Step 3: Update Metrics from Events

```javascript
executor.on('task:started', (event) => {
  activeTasksGauge.inc();
});

executor.on('task:succeeded', (event) => {
  tasksExecutedCounter.inc({ task_id: event.nitroTaskId, status: 'success' });
  taskDurationHistogram.observe({ task_id: event.nitroTaskId }, event.duration / 1000);
  activeTasksGauge.dec();
});

executor.on('task:failed', (event) => {
  tasksExecutedCounter.inc({ task_id: event.nitroTaskId, status: 'failed' });
  activeTasksGauge.dec();
});
```

### Step 4: Expose Metrics Endpoint

```javascript
app.get('/metrics', async (req, res) => {
  res.set('Content-Type', register.contentType);
  res.send(await register.metrics());
});
```

**Result**: Metrics exported to Prometheus at `/metrics`.

---

## Deploy with Docker

**Goal**: Containerize YAWL-Nitro application

### Step 1: Create Dockerfile

```dockerfile
# Dockerfile
FROM node:20-alpine

# Install pnpm
RUN corepack enable && corepack prepare pnpm@latest --activate

WORKDIR /app

# Copy package files
COPY package.json pnpm-lock.yaml ./

# Install dependencies
RUN pnpm install --frozen-lockfile --prod

# Copy application code
COPY src ./src

# Expose ports
EXPOSE 3000 8000

# Health check
HEALTHCHECK --interval=30s --timeout=5s --start-period=10s \
  CMD node -e "require('http').get('http://localhost:3000/health', (r) => process.exit(r.statusCode === 200 ? 0 : 1))"

# Start application
CMD ["node", "src/index.mjs"]
```

### Step 2: Create docker-compose.yml

```yaml
# docker-compose.yml
version: '3.8'

services:
  yawl-nitro:
    build: .
    ports:
      - '3000:3000'
      - '8000:8000'
    environment:
      - NODE_ENV=production
      - DAEMON_CONCURRENCY=10
      - TASK_TIMEOUT=30000
    volumes:
      - ./data:/app/data
    restart: unless-stopped
    healthcheck:
      test: ['CMD', 'curl', '-f', 'http://localhost:3000/health']
      interval: 30s
      timeout: 5s
      retries: 3

  prometheus:
    image: prom/prometheus:latest
    ports:
      - '9090:9090'
    volumes:
      - ./prometheus.yml:/etc/prometheus/prometheus.yml
      - prometheus-data:/prometheus
    command:
      - '--config.file=/etc/prometheus/prometheus.yml'

volumes:
  prometheus-data:
```

### Step 3: Build and Run

```bash
# Build image
docker-compose build

# Start services
docker-compose up -d

# View logs
docker-compose logs -f yawl-nitro

# Stop services
docker-compose down
```

**Result**: Production-ready containerized deployment.

---

## Quick Reference

| Task | Command/Pattern | Time |
|------|----------------|------|
| Schedule recurring | `cronExpression: '0 * * * *'` | 2 min |
| Set timeout | `timeout: 30000` in config | 1 min |
| Enable retry | `maxRetries: 3` in config | 1 min |
| Transform payload | `nitroToYawl(payload)` | 3 min |
| Monitor tasks | `executor.on('task:succeeded', ...)` | 2 min |
| Handle errors | Try/catch with error categorization | 4 min |
| Validate task | `executor.validateTask(taskId)` | 2 min |
| Execute via API | POST `/api/tasks/:taskId/execute` | 5 min |
| Export metrics | `prom-client` + event listeners | 6 min |
| Deploy Docker | `docker-compose up -d` | 4 min |

---

## See Also

- **[Tutorial: YAWL-Nitro Integration](/docs/diataxis/tutorials/yawl-nitro-integration.md)** - Complete learning guide
- **[API Reference: YAWL-Nitro](/docs/diataxis/reference/yawl-nitro-api.md)** - API documentation
- **[Explanation: YAWL-Nitro Architecture](/docs/diataxis/explanation/yawl-nitro-architecture.md)** - Architecture deep-dive
- **[Example: YAWL-Nitro Workflow](/examples/yawl-nitro-workflow.mjs)** - Production example

**Version**: 1.0.0
**Last Updated**: 2026-01-11
