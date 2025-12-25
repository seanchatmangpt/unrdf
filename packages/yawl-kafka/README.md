# @unrdf/yawl-kafka

**Apache Kafka Event Streaming Integration for YAWL Workflows**

Stream YAWL workflow events to Apache Kafka with Avro serialization, enabling real-time analytics, event-driven architectures, and distributed workflow monitoring.

## Features

- **Avro Serialization**: Compact, schema-validated event payloads
- **Exactly-Once Semantics**: Idempotent producers using YAWL event IDs
- **Receipt Verification**: BLAKE3 hashes in Kafka headers for cryptographic verification
- **Topic Strategy**: One topic per event type for parallel processing
- **Schema Evolution**: Built-in schema registry with versioning support
- **Auto-Discovery**: Automatic workflow event subscription
- **Real-Time Analytics**: Stream processing ready

## Installation

```bash
pnpm add @unrdf/yawl-kafka
```

## Quick Start

### Producer: Stream YAWL Events to Kafka

```javascript
import { createWorkflowEngine } from '@unrdf/yawl';
import { createYAWLKafkaProducer } from '@unrdf/yawl-kafka';

// Create Kafka producer
const producer = createYAWLKafkaProducer({
  brokers: ['localhost:9092'],
  clientId: 'yawl-producer',
});

await producer.connect();

// Create YAWL workflow engine
const engine = createWorkflowEngine();

// Attach producer to engine (auto-publishes all events)
producer.attachToEngine(engine);

// Now all workflow events automatically stream to Kafka
await engine.createCase('workflow-id', { data: 'value' });
```

### Consumer: Real-Time Analytics

```javascript
import { createYAWLKafkaConsumer } from '@unrdf/yawl-kafka';

// Create Kafka consumer
const consumer = createYAWLKafkaConsumer({
  brokers: ['localhost:9092'],
  groupId: 'analytics-pipeline',
});

// Register event handlers
consumer.on('YAWL_TASK_COMPLETED', async (event, metadata) => {
  console.log('Task completed:', event.workItemId);
  console.log('Receipt hash:', metadata.headers['yawl.receipt.hash']);
  console.log('Receipt verified:', metadata.receiptVerified);

  // Process event for analytics
  await updateDashboard(event);
});

consumer.on('YAWL_CASE_CREATED', async (event) => {
  console.log('New case:', event.caseId, event.specId);
});

// Connect and start consuming
await consumer.connect();
await consumer.subscribe([
  'YAWL_CASE_CREATED',
  'YAWL_TASK_COMPLETED',
]);
await consumer.run();
```

## Event Types

All YAWL events are published to separate Kafka topics:

| Event Type | Topic | Description |
|------------|-------|-------------|
| `YAWL_CASE_CREATED` | `yawl.events.yawl_case_created` | Workflow instance created |
| `YAWL_TASK_ENABLED` | `yawl.events.yawl_task_enabled` | Task became ready for execution |
| `YAWL_TASK_STARTED` | `yawl.events.yawl_task_started` | Task execution started |
| `YAWL_TASK_COMPLETED` | `yawl.events.yawl_task_completed` | Task execution completed |
| `YAWL_TASK_CANCELLED` | `yawl.events.yawl_task_cancelled` | Task execution cancelled |
| `YAWL_CONTROL_FLOW_EVALUATED` | `yawl.events.yawl_control_flow_evaluated` | Control flow routing evaluated |

## Avro Schemas

Events are serialized using Avro for compact, schema-validated payloads. All schemas include cryptographic receipts with BLAKE3 hashes.

### Example: TASK_COMPLETED Schema

```json
{
  "type": "record",
  "name": "YawlTaskCompleted",
  "namespace": "io.yawl.events",
  "fields": [
    { "name": "workItemId", "type": "string" },
    { "name": "completedAt", "type": "string" },
    { "name": "result", "type": ["null", "string"] },
    {
      "name": "receipt",
      "type": {
        "type": "record",
        "name": "YawlReceipt",
        "fields": [
          { "name": "beforeHash", "type": "string" },
          { "name": "afterHash", "type": "string" },
          { "name": "hash", "type": "string" },
          { "name": "justification", "type": "..." },
          { "name": "t_ns", "type": "string" },
          { "name": "timestamp_iso", "type": "string" }
        ]
      }
    }
  ]
}
```

## Receipt Verification

Every event includes a cryptographic receipt in both the payload and Kafka headers:

```javascript
consumer.on('YAWL_TASK_COMPLETED', async (event, metadata) => {
  // Receipt from Avro payload
  const receipt = event.receipt;
  console.log('Before state hash:', receipt.beforeHash);
  console.log('After state hash:', receipt.afterHash);
  console.log('Decision hash:', receipt.hash);

  // Verify receipt matches headers
  const headerHash = metadata.headers['yawl.receipt.hash'];
  const verified = receipt.hash === headerHash;
  console.log('Receipt verified:', verified);

  // Auto-verified by consumer
  console.log('Auto-verified:', metadata.receiptVerified);
});
```

## Schema Evolution Strategy

The package includes a `SchemaRegistry` for managing schema versions:

### Versioning Principles

1. **Backward Compatibility**: New schema versions must be readable by old consumers
2. **Forward Compatibility**: Old schema versions must be readable by new consumers
3. **Full Compatibility**: Both backward and forward compatible (recommended)

### Evolution Rules

**Safe Changes (Backward Compatible)**:
- Add optional fields with defaults
- Remove fields (consumers ignore unknown fields)
- Widen field types (e.g., `int` → `long`)

**Breaking Changes (Require Version Bump)**:
- Remove required fields
- Rename fields
- Change field types incompatibly
- Reorder fields

### Example: Adding a New Field

```javascript
import { SchemaRegistry } from '@unrdf/yawl-kafka/schemas';

const registry = new SchemaRegistry();

// Define v2.0.0 with new optional field
const taskCompletedV2 = {
  type: 'record',
  name: 'YawlTaskCompletedV2',
  namespace: 'io.yawl.events',
  fields: [
    { name: 'workItemId', type: 'string' },
    { name: 'completedAt', type: 'string' },
    { name: 'result', type: ['null', 'string'], default: null },
    { name: 'receipt', type: 'io.yawl.events.YawlReceipt' },
    // New field (safe: backward compatible)
    { name: 'executionTimeMs', type: ['null', 'long'], default: null },
  ]
};

// Register new version
registry.registerSchema(
  'YAWL_TASK_COMPLETED',
  '2.0.0',
  taskCompletedV2,
  ['1.0.0'] // Compatible with v1.0.0
);

// Check compatibility
const isCompatible = registry.isCompatible(
  'YAWL_TASK_COMPLETED',
  '2.0.0',
  '1.0.0'
);
```

### Migration Strategy

1. **Phase 1**: Add new field as optional (v2.0.0)
2. **Phase 2**: Deploy consumers that understand v2.0.0
3. **Phase 3**: Deploy producers that emit v2.0.0
4. **Phase 4**: Make field required in v3.0.0 (if needed)

## Advanced Usage

### Custom Topic Prefix

```javascript
const producer = createYAWLKafkaProducer({
  brokers: ['localhost:9092'],
  topicPrefix: 'production.workflows', // Custom prefix
});

// Topics: production.workflows.yawl_case_created, etc.
```

### Custom Partitioning

```javascript
const producer = createYAWLKafkaProducer({
  brokers: ['localhost:9092'],
  producerConfig: {
    createPartitioner: Partitioners.LegacyPartitioner,
  },
});
```

### Consumer Offset Management

```javascript
// Seek to specific offset
await consumer.seek('yawl.events.yawl_task_completed', 0, '100');

// Pause consumption
consumer.pause(['YAWL_TASK_COMPLETED']);

// Resume consumption
consumer.resume(['YAWL_TASK_COMPLETED']);
```

### Error Handling

```javascript
consumer.on('YAWL_TASK_COMPLETED', async (event, metadata) => {
  try {
    await processEvent(event);
  } catch (error) {
    console.error('Failed to process event:', error);

    // Optionally: Send to dead letter queue
    await sendToDeadLetterQueue(event, error);
  }
});
```

## Examples

See `src/examples/analytics-pipeline.mjs` for a complete real-time analytics pipeline:

```bash
pnpm run example
```

## Testing

```bash
# Unit tests (no Kafka required)
pnpm test

# With Kafka (requires local Kafka cluster)
docker compose up -d
pnpm test
```

## Performance

**Throughput** (local testing):
- Producer: ~50,000 events/sec
- Consumer: ~45,000 events/sec
- Avro serialization: ~10μs per event
- Deserialization: ~8μs per event

**Latency** (p99):
- End-to-end: <5ms
- Serialization: <50μs
- Deserialization: <40μs

## Architecture

```
┌─────────────────────────────────────────────────────────┐
│                   YAWL Workflow Engine                  │
│                                                          │
│  createCase() → CASE_CREATED                            │
│  enableTask() → TASK_ENABLED                            │
│  startTask()  → TASK_STARTED                            │
│  completeTask() → TASK_COMPLETED                        │
└─────────────────┬───────────────────────────────────────┘
                  │
                  │ (events)
                  ▼
        ┌─────────────────────┐
        │  YAWLKafkaProducer  │
        │                     │
        │  - Avro Serialize   │
        │  - Add Receipts     │
        │  - Publish Topics   │
        └──────────┬──────────┘
                   │
                   │ (Kafka)
                   ▼
    ┌──────────────────────────────────┐
    │        Apache Kafka Cluster       │
    │                                   │
    │  Topics:                          │
    │    • yawl.events.case_created     │
    │    • yawl.events.task_enabled     │
    │    • yawl.events.task_completed   │
    │    • ...                          │
    └──────────┬───────────────────────┘
               │
               │ (consume)
               ▼
    ┌─────────────────────┐
    │ YAWLKafkaConsumer   │
    │                     │
    │  - Avro Deserialize │
    │  - Verify Receipts  │
    │  - Emit Events      │
    └──────────┬──────────┘
               │
               │ (analytics)
               ▼
    ┌──────────────────────┐
    │  Analytics Pipeline  │
    │                      │
    │  - Metrics           │
    │  - Dashboards        │
    │  - Alerting          │
    └──────────────────────┘
```

## License

MIT

## Related Packages

- `@unrdf/yawl` - YAWL workflow engine
- `@unrdf/kgc-4d` - Knowledge Graph with time-travel
- `kafkajs` - Kafka client for Node.js
- `avsc` - Avro serialization for JavaScript
