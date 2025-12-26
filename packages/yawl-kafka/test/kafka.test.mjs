/**
 * @file YAWL-Kafka Integration Tests
 * @description
 * Tests for Kafka producer, consumer, and Avro serialization.
 */

import { describe, it, expect, beforeAll, afterAll } from 'vitest';
import {
  serializeEvent,
  deserializeEvent,
  getAvroType,
  SchemaRegistry,
} from '../src/schemas.mjs';
import { YAWLKafkaProducer } from '../src/producer.mjs';
import { YAWLKafkaConsumer } from '../src/consumer.mjs';

// =============================================================================
// Avro Schema Tests
// =============================================================================

describe('Avro Serialization', () => {
  it('should serialize and deserialize CASE_CREATED event', () => {
    const eventData = {
      caseId: '123e4567-e89b-12d3-a456-426614174000',
      specId: 'expense-approval',
      timestamp: '2024-01-01T00:00:00Z',
      receipt: {
        beforeHash: '0'.repeat(64),
        afterHash: '1'.repeat(64),
        hash: '2'.repeat(64),
        justification: {
          reasoning: 'Test case created',
        },
        t_ns: '1704067200000000000',
        timestamp_iso: '2024-01-01T00:00:00Z',
      },
    };

    const buffer = serializeEvent('YAWL_CASE_CREATED', eventData);
    expect(buffer).toBeInstanceOf(Buffer);

    const deserialized = deserializeEvent('YAWL_CASE_CREATED', buffer);
    expect(deserialized.caseId).toBe(eventData.caseId);
    expect(deserialized.specId).toBe(eventData.specId);
    expect(deserialized.receipt.hash).toBe(eventData.receipt.hash);
  });

  it('should serialize and deserialize TASK_ENABLED event', () => {
    const eventData = {
      taskId: 'submit',
      caseId: '123e4567-e89b-12d3-a456-426614174000',
      workItemId: '987e6543-e21b-12d3-a456-426614174999',
      enabledAt: '2024-01-01T00:00:00Z',
      receipt: {
        beforeHash: 'a'.repeat(64),
        afterHash: 'b'.repeat(64),
        hash: 'c'.repeat(64),
        justification: {},
        t_ns: '1704067200000000000',
        timestamp_iso: '2024-01-01T00:00:00Z',
      },
    };

    const buffer = serializeEvent('YAWL_TASK_ENABLED', eventData);
    const deserialized = deserializeEvent('YAWL_TASK_ENABLED', buffer);

    expect(deserialized.taskId).toBe(eventData.taskId);
    expect(deserialized.workItemId).toBe(eventData.workItemId);
  });

  it('should serialize and deserialize TASK_COMPLETED event with result', () => {
    const eventData = {
      workItemId: '987e6543-e21b-12d3-a456-426614174999',
      completedAt: '2024-01-01T00:01:00Z',
      result: JSON.stringify({ approved: true, amount: 500 }),
      receipt: {
        beforeHash: 'd'.repeat(64),
        afterHash: 'e'.repeat(64),
        hash: 'f'.repeat(64),
        justification: {},
        t_ns: '1704067260000000000',
        timestamp_iso: '2024-01-01T00:01:00Z',
      },
    };

    const buffer = serializeEvent('YAWL_TASK_COMPLETED', eventData);
    const deserialized = deserializeEvent('YAWL_TASK_COMPLETED', buffer);

    expect(deserialized.workItemId).toBe(eventData.workItemId);
    expect(deserialized.result).toBe(eventData.result);
  });

  it('should serialize TASK_COMPLETED with null result', () => {
    const eventData = {
      workItemId: '987e6543-e21b-12d3-a456-426614174999',
      completedAt: '2024-01-01T00:01:00Z',
      result: null,
      receipt: {
        beforeHash: 'd'.repeat(64),
        afterHash: 'e'.repeat(64),
        hash: 'f'.repeat(64),
        justification: {},
        t_ns: '1704067260000000000',
        timestamp_iso: '2024-01-01T00:01:00Z',
      },
    };

    const buffer = serializeEvent('YAWL_TASK_COMPLETED', eventData);
    const deserialized = deserializeEvent('YAWL_TASK_COMPLETED', buffer);

    expect(deserialized.result).toBeNull();
  });

  it('should handle justification with all optional fields', () => {
    const eventData = {
      workItemId: '987e6543-e21b-12d3-a456-426614174999',
      startedAt: '2024-01-01T00:00:30Z',
      receipt: {
        beforeHash: 'g'.repeat(64),
        afterHash: 'h'.repeat(64),
        hash: 'i'.repeat(64),
        justification: {
          hookValidated: 'pre-start-hook',
          conditionChecked: 'has_permission',
          sparqlQuery: 'SELECT * WHERE { ?s ?p ?o }',
          reasoning: 'All checks passed',
        },
        gitRef: 'abc123',
        t_ns: '1704067230000000000',
        timestamp_iso: '2024-01-01T00:00:30Z',
      },
    };

    const buffer = serializeEvent('YAWL_TASK_STARTED', eventData);
    const deserialized = deserializeEvent('YAWL_TASK_STARTED', buffer);

    expect(deserialized.receipt.justification.hookValidated).toBe('pre-start-hook');
    expect(deserialized.receipt.gitRef).toBe('abc123');
  });
});

// =============================================================================
// Schema Registry Tests
// =============================================================================

describe('SchemaRegistry', () => {
  it('should initialize with default schemas', () => {
    const registry = new SchemaRegistry();

    const schema = registry.getSchema('YAWL_CASE_CREATED', '1.0.0');
    expect(schema).toBeDefined();
    expect(schema.name).toBe('YawlCaseCreated');
  });

  it('should get latest version', () => {
    const registry = new SchemaRegistry();

    const latest = registry.getLatestVersion('YAWL_TASK_ENABLED');
    expect(latest.version).toBe('1.0.0');
    expect(latest.eventType).toBe('YAWL_TASK_ENABLED');
  });

  it('should register new schema version', () => {
    const registry = new SchemaRegistry();

    const newSchema = {
      type: 'record',
      name: 'YawlCaseCreatedV2',
      fields: [
        { name: 'caseId', type: 'string' },
        { name: 'specId', type: 'string' },
        { name: 'newField', type: 'string' },
      ],
    };

    registry.registerSchema('YAWL_CASE_CREATED', '2.0.0', newSchema, ['1.0.0']);

    const schema = registry.getSchema('YAWL_CASE_CREATED', '2.0.0');
    expect(schema).toBeDefined();
    expect(schema.fields.length).toBe(3);
  });

  it('should check compatibility between versions', () => {
    const registry = new SchemaRegistry();

    registry.registerSchema('YAWL_CASE_CREATED', '2.0.0', {}, ['1.0.0']);

    const compatible = registry.isCompatible('YAWL_CASE_CREATED', '2.0.0', '1.0.0');
    expect(compatible).toBe(true);

    const incompatible = registry.isCompatible('YAWL_CASE_CREATED', '1.0.0', '2.0.0');
    expect(incompatible).toBe(false);
  });
});

// =============================================================================
// Producer Tests
// =============================================================================

describe('YAWLKafkaProducer', () => {
  it('should create producer with default config', () => {
    const producer = new YAWLKafkaProducer({
      brokers: ['localhost:9092'],
    });

    expect(producer.config.clientId).toBe('yawl-kafka-producer');
    expect(producer.config.topicPrefix).toBe('yawl.events');
    expect(producer.connected).toBe(false);
  });

  it('should track statistics', () => {
    const producer = new YAWLKafkaProducer({
      brokers: ['localhost:9092'],
    });

    const stats = producer.getStats();
    expect(stats.connected).toBe(false);
    expect(stats.totalPublished).toBe(0);
  });

  // Note: Integration tests requiring actual Kafka cluster are skipped in unit tests
  it.skip('should connect to Kafka and create topics', async () => {
    const producer = new YAWLKafkaProducer({
      brokers: ['localhost:9092'],
      createTopics: true,
    });

    await producer.connect();
    expect(producer.connected).toBe(true);

    await producer.disconnect();
    expect(producer.connected).toBe(false);
  });
});

// =============================================================================
// Consumer Tests
// =============================================================================

describe('YAWLKafkaConsumer', () => {
  it('should create consumer with default config', () => {
    const consumer = new YAWLKafkaConsumer({
      brokers: ['localhost:9092'],
      groupId: 'test-group',
    });

    expect(consumer.config.clientId).toBe('yawl-kafka-consumer');
    expect(consumer.config.groupId).toBe('test-group');
    expect(consumer.connected).toBe(false);
  });

  it('should register event handlers', () => {
    const consumer = new YAWLKafkaConsumer({
      brokers: ['localhost:9092'],
      groupId: 'test-group',
    });

    const handler = async (event) => {
      console.log(event);
    };

    const unsubscribe = consumer.on('YAWL_CASE_CREATED', handler);
    expect(consumer.handlers.get('YAWL_CASE_CREATED').size).toBe(1);

    unsubscribe();
    expect(consumer.handlers.get('YAWL_CASE_CREATED').size).toBe(0);
  });

  it('should track statistics', () => {
    const consumer = new YAWLKafkaConsumer({
      brokers: ['localhost:9092'],
      groupId: 'test-group',
    });

    const stats = consumer.getStats();
    expect(stats.connected).toBe(false);
    expect(stats.running).toBe(false);
    expect(stats.totalConsumed).toBe(0);
  });

  // Note: Integration tests requiring actual Kafka cluster are skipped
  it.skip('should connect and subscribe to topics', async () => {
    const consumer = new YAWLKafkaConsumer({
      brokers: ['localhost:9092'],
      groupId: 'test-group',
    });

    await consumer.connect();
    await consumer.subscribe(['YAWL_CASE_CREATED', 'YAWL_TASK_COMPLETED']);

    expect(consumer.connected).toBe(true);

    await consumer.disconnect();
  });
});

// =============================================================================
// Avro Type Tests
// =============================================================================

describe('Avro Type System', () => {
  it('should get Avro type for event', () => {
    const type = getAvroType('YAWL_CASE_CREATED');
    expect(type).toBeDefined();
    expect(type.name).toBe('YawlCaseCreated');
  });

  it('should throw error for unknown event type', () => {
    expect(() => getAvroType('UNKNOWN_EVENT')).toThrow('No Avro schema found');
  });

  it('should validate receipt schema fields', () => {
    const type = getAvroType('YAWL_TASK_STARTED');
    const fields = type.fields;

    const receiptField = fields.find(f => f.name === 'receipt');
    expect(receiptField).toBeDefined();
    expect(receiptField.type.name).toBe('YawlReceipt');

    const receiptType = receiptField.type;
    const receiptFields = receiptType.fields.map(f => f.name);

    expect(receiptFields).toContain('beforeHash');
    expect(receiptFields).toContain('afterHash');
    expect(receiptFields).toContain('hash');
    expect(receiptFields).toContain('justification');
  });
});

// =============================================================================
// End-to-End Schema Test
// =============================================================================

describe('Schema Round-Trip', () => {
  it('should serialize and deserialize all event types', () => {
    const testEvents = {
      YAWL_CASE_CREATED: {
        caseId: '00000000-0000-0000-0000-000000000001',
        specId: 'test-spec',
        timestamp: '2024-01-01T00:00:00Z',
        receipt: createTestReceipt(),
      },
      YAWL_TASK_ENABLED: {
        taskId: 'task1',
        caseId: '00000000-0000-0000-0000-000000000001',
        workItemId: '00000000-0000-0000-0000-000000000002',
        enabledAt: '2024-01-01T00:00:00Z',
        receipt: createTestReceipt(),
      },
      YAWL_TASK_STARTED: {
        workItemId: '00000000-0000-0000-0000-000000000002',
        startedAt: '2024-01-01T00:00:01Z',
        receipt: createTestReceipt(),
      },
      YAWL_TASK_COMPLETED: {
        workItemId: '00000000-0000-0000-0000-000000000002',
        completedAt: '2024-01-01T00:00:02Z',
        result: '{"success": true}',
        receipt: createTestReceipt(),
      },
      YAWL_TASK_CANCELLED: {
        workItemId: '00000000-0000-0000-0000-000000000002',
        cancelledAt: '2024-01-01T00:00:03Z',
        reason: 'User requested cancellation',
        receipt: createTestReceipt(),
      },
      YAWL_CONTROL_FLOW_EVALUATED: {
        caseId: '00000000-0000-0000-0000-000000000001',
        taskId: 'task1',
        result: true,
        timestamp: '2024-01-01T00:00:04Z',
        sparqlQuery: 'SELECT * WHERE { ?s ?p ?o }',
        receipt: createTestReceipt(),
      },
    };

    for (const [eventType, eventData] of Object.entries(testEvents)) {
      const buffer = serializeEvent(eventType, eventData);
      expect(buffer).toBeInstanceOf(Buffer);

      const deserialized = deserializeEvent(eventType, buffer);
      expect(deserialized).toBeDefined();

      // Verify key fields
      if (eventData.caseId) expect(deserialized.caseId).toBe(eventData.caseId);
      if (eventData.workItemId) expect(deserialized.workItemId).toBe(eventData.workItemId);
      if (eventData.taskId) expect(deserialized.taskId).toBe(eventData.taskId);
    }
  });
});

// =============================================================================
// Helper Functions
// =============================================================================

function createTestReceipt() {
  return {
    beforeHash: 'a'.repeat(64),
    afterHash: 'b'.repeat(64),
    hash: 'c'.repeat(64),
    justification: {
      reasoning: 'Test event',
    },
    t_ns: '1704067200000000000',
    timestamp_iso: '2024-01-01T00:00:00Z',
  };
}
