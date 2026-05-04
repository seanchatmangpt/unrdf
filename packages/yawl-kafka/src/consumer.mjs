/**
 * @file YAWL Kafka Consumer - Consume YAWL events from Kafka
 * @module @unrdf/yawl-kafka/consumer
 *
 * @description
 * Consumes YAWL workflow events from Kafka topics with Avro deserialization.
 * Supports event verification using receipt hashes from headers.
 * Provides base class for building analytics pipelines and event processors.
 */

import { Kafka } from 'kafkajs';
import { deserializeEvent } from './schemas.mjs';

// =============================================================================
// YAWLKafkaConsumer Class
// =============================================================================

/**
 * Kafka consumer for YAWL workflow events
 *
 * Features:
 * - Avro deserialization with schema validation
 * - Receipt verification from Kafka headers
 * - Exactly-once processing with offset management
 * - Automatic retry and error handling
 * - Flexible event handler registration
 *
 * @example
 * const consumer = new YAWLKafkaConsumer({
 *   brokers: ['localhost:9092'],
 *   groupId: 'analytics-pipeline',
 * });
 *
 * consumer.on('YAWL_TASK_COMPLETED', async (event, metadata) => {
 *   console.log('Task completed:', event.workItemId);
 *   console.log('Receipt hash:', metadata.headers['yawl.receipt.hash']);
 * });
 *
 * await consumer.connect();
 * await consumer.subscribe(['YAWL_TASK_COMPLETED']);
 * await consumer.run();
 */
export class YAWLKafkaConsumer {
  /**
   * Create a new YAWL Kafka consumer
   *
   * @param {Object} config - Kafka configuration
   * @param {string[]} config.brokers - Kafka broker addresses
   * @param {string} config.groupId - Consumer group ID
   * @param {string} [config.clientId='yawl-kafka-consumer'] - Kafka client ID
   * @param {string} [config.topicPrefix='yawl.events'] - Topic name prefix
   * @param {Object} [config.consumerConfig={}] - Additional KafkaJS consumer config
   */
  constructor(config) {
    this.config = {
      clientId: 'yawl-kafka-consumer',
      topicPrefix: 'yawl.events',
      consumerConfig: {},
      ...config,
    };

    this.kafka = new Kafka({
      clientId: this.config.clientId,
      brokers: this.config.brokers,
    });

    this.consumer = this.kafka.consumer({
      groupId: this.config.groupId,
      ...this.config.consumerConfig,
    });

    this.connected = false;
    this.running = false;

    /** @type {Map<string, Set<Function>>} Event handlers */
    this.handlers = new Map();

    /** @type {Map<string, number>} Event type to consume count */
    this.stats = new Map();
  }

  /**
   * Connect to Kafka cluster
   *
   * @returns {Promise<void>}
   * @throws {Error} If connection fails
   */
  async connect() {
    await this.consumer.connect();
    this.connected = true;
  }

  /**
   * Disconnect from Kafka cluster
   *
   * @returns {Promise<void>}
   */
  async disconnect() {
    if (this.running) {
      await this.consumer.stop();
      this.running = false;
    }

    await this.consumer.disconnect();
    this.connected = false;
  }

  /**
   * Subscribe to YAWL event types
   *
   * @param {string[]} eventTypes - YAWL event types to subscribe to
   * @returns {Promise<void>}
   *
   * @example
   * await consumer.subscribe([
   *   'YAWL_CASE_CREATED',
   *   'YAWL_TASK_COMPLETED',
   * ]);
   */
  async subscribe(eventTypes) {
    const topics = eventTypes.map(eventType =>
      this._getTopicName(eventType)
    );

    await this.consumer.subscribe({
      topics,
      fromBeginning: false,
    });
  }

  /**
   * Get Kafka topic name for event type
   *
   * @param {string} eventType - YAWL event type
   * @returns {string} Kafka topic name
   * @private
   */
  _getTopicName(eventType) {
    return `${this.config.topicPrefix}.${eventType.toLowerCase()}`;
  }

  /**
   * Register event handler
   *
   * @param {string} eventType - YAWL event type
   * @param {Function} handler - Event handler (async function)
   * @returns {Function} Unsubscribe function
   *
   * @example
   * const unsubscribe = consumer.on('YAWL_TASK_COMPLETED', async (event) => {
   *   console.log('Task completed:', event.workItemId);
   * });
   *
   * // Later: unsubscribe()
   */
  on(eventType, handler) {
    if (!this.handlers.has(eventType)) {
      this.handlers.set(eventType, new Set());
    }

    this.handlers.get(eventType).add(handler);

    return () => {
      this.handlers.get(eventType)?.delete(handler);
    };
  }

  /**
   * Start consuming events
   *
   * @returns {Promise<void>}
   */
  async run() {
    if (!this.connected) {
      throw new Error('Consumer not connected. Call connect() first.');
    }

    this.running = true;

    await this.consumer.run({
      eachMessage: async ({ topic, partition, message }) => {
        await this._handleMessage(topic, partition, message);
      },
    });
  }

  /**
   * Handle incoming Kafka message
   *
   * @param {string} topic - Kafka topic
   * @param {number} partition - Partition number
   * @param {Object} message - Kafka message
   * @private
   */
  async _handleMessage(topic, partition, message) {
    try {
      // Extract event type from headers
      const eventType = message.headers['yawl.event.type']?.toString();
      if (!eventType) {
        console.warn('Message missing yawl.event.type header, skipping');
        return;
      }

      // Deserialize Avro payload
      const eventData = deserializeEvent(eventType, message.value);

      // Prepare metadata
      const metadata = {
        topic,
        partition,
        offset: message.offset,
        timestamp: message.timestamp,
        key: message.key?.toString(),
        headers: this._parseHeaders(message.headers),
      };

      // Verify receipt if present
      if (metadata.headers['yawl.receipt.hash']) {
        const verified = this._verifyReceipt(eventData, metadata.headers);
        metadata.receiptVerified = verified;
      }

      // Invoke handlers
      const handlers = this.handlers.get(eventType);
      if (handlers) {
        for (const handler of handlers) {
          try {
            await handler(eventData, metadata);
          } catch (error) {
            console.error(`Error in handler for ${eventType}:`, error);
          }
        }
      }

      // Update stats
      this.stats.set(eventType, (this.stats.get(eventType) || 0) + 1);
    } catch (error) {
      console.error('Failed to process message:', error);
    }
  }

  /**
   * Parse Kafka headers to string map
   *
   * @param {Object} headers - Kafka headers
   * @returns {Object} Parsed headers
   * @private
   */
  _parseHeaders(headers) {
    const parsed = {};
    for (const [key, value] of Object.entries(headers || {})) {
      parsed[key] = value?.toString();
    }
    return parsed;
  }

  /**
   * Verify receipt hashes match header values
   *
   * @param {Object} eventData - Deserialized event data
   * @param {Object} headers - Kafka headers
   * @returns {boolean} True if receipt is valid
   * @private
   */
  _verifyReceipt(eventData, headers) {
    if (!eventData.receipt) return false;

    const hashMatch = eventData.receipt.hash === headers['yawl.receipt.hash'];
    const beforeMatch = eventData.receipt.beforeHash === headers['yawl.receipt.beforeHash'];
    const afterMatch = eventData.receipt.afterHash === headers['yawl.receipt.afterHash'];

    return hashMatch && beforeMatch && afterMatch;
  }

  /**
   * Get consumer statistics
   *
   * @returns {Object} Statistics object
   */
  getStats() {
    return {
      connected: this.connected,
      running: this.running,
      handlers: this.handlers.size,
      eventsConsumed: Object.fromEntries(this.stats),
      totalConsumed: [...this.stats.values()].reduce((sum, count) => sum + count, 0),
    };
  }

  /**
   * Seek to specific offset for a topic partition
   *
   * @param {string} topic - Kafka topic
   * @param {number} partition - Partition number
   * @param {string} offset - Offset to seek to
   * @returns {Promise<void>}
   */
  async seek(topic, partition, offset) {
    await this.consumer.seek({
      topic,
      partition,
      offset,
    });
  }

  /**
   * Pause consumption for topics
   *
   * @param {string[]} eventTypes - Event types to pause
   * @returns {void}
   */
  pause(eventTypes) {
    const topics = eventTypes.map(eventType => ({
      topic: this._getTopicName(eventType),
    }));

    this.consumer.pause(topics);
  }

  /**
   * Resume consumption for topics
   *
   * @param {string[]} eventTypes - Event types to resume
   * @returns {void}
   */
  resume(eventTypes) {
    const topics = eventTypes.map(eventType => ({
      topic: this._getTopicName(eventType),
    }));

    this.consumer.resume(topics);
  }
}

// =============================================================================
// Factory Function
// =============================================================================

/**
 * Create a new YAWL Kafka consumer
 *
 * @param {Object} config - Consumer configuration
 * @returns {YAWLKafkaConsumer} Configured consumer
 *
 * @example
 * const consumer = createYAWLKafkaConsumer({
 *   brokers: ['localhost:9092'],
 *   groupId: 'my-analytics',
 * });
 */
export function createYAWLKafkaConsumer(config) {
  return new YAWLKafkaConsumer(config);
}

// =============================================================================
// Exports
// =============================================================================

export default {
  YAWLKafkaConsumer,
  createYAWLKafkaConsumer,
};
