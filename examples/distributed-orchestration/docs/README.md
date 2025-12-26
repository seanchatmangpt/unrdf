# Distributed Workflow Orchestration

Production-grade distributed workflow orchestration using YAWL + Federation + Streaming.

## Features

- **Multi-node workflow execution**: Distribute tasks across worker nodes
- **Automatic failover and recovery**: Handle worker failures gracefully
- **Real-time progress monitoring**: WebSocket-based monitoring dashboard
- **OpenTelemetry instrumentation**: Full observability

## Quick Start

### Prerequisites

- Node.js 18+
- pnpm 7+

### Installation

```bash
pnpm install
```

### Running with Docker

```bash
docker-compose up
```

This starts:
- Orchestrator (port 8080)
- 2 Worker nodes
- Monitoring dashboard (port 3000)

### Manual Setup

#### 1. Start Orchestrator

```bash
ORCHESTRATOR_PORT=8080 node src/orchestrator.mjs
```

#### 2. Start Worker Nodes

```bash
# Worker 1
NODE_ID=worker-1 ORCHESTRATOR_URL=http://localhost:8080 node src/worker-node.mjs

# Worker 2
NODE_ID=worker-2 ORCHESTRATOR_URL=http://localhost:8080 node src/worker-node.mjs
```

#### 3. Start Monitoring Dashboard

```bash
DASHBOARD_PORT=3000 node src/monitoring-dashboard.mjs
```

## Usage

### Submit a Workflow

```javascript
import { DistributedOrchestrator } from './src/orchestrator.mjs';

const orchestrator = new DistributedOrchestrator({ port: 8080 });
await orchestrator.initialize();

const workflow = {
  id: 'data-processing',
  tasks: [
    { id: 'fetch', type: 'io' },
    { id: 'transform', type: 'transform' },
    { id: 'store', type: 'io' },
  ],
};

const workflowId = await orchestrator.submitWorkflow(workflow, {
  source: 'https://api.example.com/data',
});

console.log('Workflow submitted:', workflowId);
```

### Monitor Progress

Connect to WebSocket dashboard:

```javascript
const ws = new WebSocket('ws://localhost:3000');

ws.on('message', (data) => {
  const event = JSON.parse(data);

  if (event.type === 'metrics_update') {
    console.log('Active workflows:', event.metrics.workflows);
    console.log('Active tasks:', event.metrics.tasks.active);
  }
});
```

## Configuration

See `.env.example` for all configuration options.

## Testing

```bash
# Run all tests
pnpm test

# Run with coverage
pnpm test:coverage

# Watch mode
pnpm test:watch
```

## Performance

Expected performance (10 workers, 100 tasks):
- Throughput: 500-1000 tasks/second
- Latency: <100ms per task
- Failover time: <5 seconds

## Architecture

See [ARCHITECTURE.md](ARCHITECTURE.md) for detailed architecture documentation.

## API

See [API.md](API.md) for full API documentation.
