/**
 * @file YAWL-Kafka Integration - Event Streaming for YAWL Workflows
 * @module @unrdf/yawl-kafka
 *
 * @description
 * Apache Kafka integration for YAWL workflow events with Avro serialization.
 * Enables real-time analytics, event-driven architectures, and workflow monitoring.
 */

// Producer
export {
  YAWLKafkaProducer,
  createYAWLKafkaProducer,
} from './producer.mjs';

// Consumer
export {
  YAWLKafkaConsumer,
  createYAWLKafkaConsumer,
} from './consumer.mjs';

// Schemas
export {
  // Avro Schemas
  ReceiptAvroSchema,
  CaseCreatedAvroSchema,
  TaskEnabledAvroSchema,
  TaskStartedAvroSchema,
  TaskCompletedAvroSchema,
  TaskCancelledAvroSchema,
  ControlFlowEvaluatedAvroSchema,

  // Schema Registry
  EVENT_SCHEMA_MAP,
  SchemaRegistry,

  // Utility Functions
  getAvroType,
  getAvroSchema,
  serializeEvent,
  deserializeEvent,
} from './schemas.mjs';
