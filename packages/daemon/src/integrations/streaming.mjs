/**
 * @file Daemon Streaming Integration - Reactive Change Feed Subscriptions
 * @module @unrdf/daemon/integrations/streaming
 * @description Integrates daemon with streaming change feeds for reactive operation execution
 * with backpressure handling, debouncing/throttling, and memory efficiency
 */

import { z } from 'zod';


const ReactiveTriggerSchema = z.object({
  pattern: z.union([z.string(), z.object({ type: z.enum(['add', 'remove', 'update']).optional(), subject: z.any().optional(), predicate: z.any().optional(), object: z.any().optional(), graph: z.any().optional() })]),
  operationId: z.string().min(1),
  metadata: z.record(z.any()).optional(),
}).passthrough();

const StreamingConfigSchema = z.object({
  maxBackpressure: z.number().int().positive().default(1000),
  maxQueueSize: z.number().int().positive().default(5000),
  batchSize: z.number().int().positive().default(100),
  drainThreshold: z.number().int().positive().default(500),
  backpressureDrainMs: z.number().int().positive().default(100),
  maxDrainWaitMs: z.number().int().positive().default(5000),
  memoryWarningThresholdBytes: z.number().int().positive().default(52428800),
  logger: z.any().optional(),
});

const ChangeEventSchema = z.object({
  type: z.enum(['add', 'remove', 'update']),
  quad: z.object({ subject: z.any(), predicate: z.any(), object: z.any(), graph: z.any().optional() }),
  timestamp: z.number(),
  metadata: z.record(z.any()).optional(),
});

const FeedSchema = z.object({
  subscribe: z.any(),
  unsubscribe: z.any().optional(),
}).passthrough();

const DaemonSchema = z.object({
  execute: z.function().optional(),
  schedule: z.function().optional(),
  operations: z.instanceof(Map).optional(),
}).passthrough();

const PatternSchema = z.union([
  z.string(),
  z.object({
    type: z.enum(['add', 'remove', 'update']).optional(),
    subject: z.any().optional(),
    predicate: z.any().optional(),
    object: z.any().optional(),
    graph: z.any().optional(),
  })
]);

/**
 * Manages reactive subscriptions and pattern matching for daemon operations
 * with backpressure handling, debouncing, and memory tracking
 */
export class ReactiveSubscriptionManager {
  /**
   * Creates a new reactive subscription manager
   * @param {UnrdfDaemon} daemon - The daemon instance to execute operations on
   * @param {Object} config - Configuration options
   */
  constructor(daemon, config = {}) {
    // Validate daemon structure without parsing (to preserve method binding)
    DaemonSchema.parse(daemon);
    const validated = StreamingConfigSchema.parse(config);
    this.daemon = daemon;  // Use original daemon, not parsed copy
    this.config = validated;
    this.logger = validated.logger || console;
    this.triggers = new Map();
    this.activeCount = 0;
    this.eventQueue = [];
    this.subscriptions = new Map();
    this.debounceTimers = new Map();
    this.throttleTimestamps = new Map();
    this.memoryUsage = 0;
    this.totalProcessed = 0;
    this.dropped = 0;
  }

  /**
   * Register a reactive trigger pattern
   * @param {string | Object} pattern - SPARQL query or event pattern
   * @param {string} operationId - Operation to execute on pattern match
   * @param {Object} [metadata] - Optional trigger metadata with debounce/throttle
   * @returns {string} Trigger ID
   */
  registerTrigger(pattern, operationId, metadata = {}) {
    const validatedPattern = PatternSchema.parse(pattern);
    const validatedOperationId = z.string().min(1).parse(operationId);
    const validatedMetadata = z.record(z.any()).optional().parse(metadata) || {};

    const trigger = ReactiveTriggerSchema.parse({ pattern: validatedPattern, operationId: validatedOperationId, metadata: validatedMetadata });

    const triggerId = `trigger_${Date.now()}_${Math.random().toString(36).slice(2, 9)}`;
    this.triggers.set(triggerId, {
      ...trigger,
      debounceMs: validatedMetadata.debounceMs || 0,
      throttleMs: validatedMetadata.throttleMs || 0,
    });
    this.logger.debug(`[ReactiveSubscriptionManager] Trigger registered: ${triggerId}`);
    return triggerId;
  }

  /**
   * Unregister a reactive trigger
   * @param {string} triggerId - Trigger ID to unregister
   * @returns {boolean} Whether trigger was found and removed
   */
  unregisterTrigger(triggerId) {
    const validated = z.string().min(1).parse(triggerId);
    const removed = this.triggers.delete(validated);
    if (removed) {
      this.debounceTimers.delete(validated);
      this.throttleTimestamps.delete(validated);
      this.logger.debug(`[ReactiveSubscriptionManager] Trigger unregistered: ${validated}`);
    }
    return removed;
  }

  /**
   * Check if a change event matches a pattern
   * @param {Object} change - Change event from feed
   * @param {string | Object} pattern - Pattern to match against
   * @returns {boolean} Whether change matches pattern
   * @private
   */
  matchesPattern(change, pattern) {
    if (typeof pattern === 'string') return change.type === pattern;
    if (pattern.type && pattern.type !== change.type) return false;
    const q = change.quad;
    const getValue = (obj) => obj?.value !== undefined ? obj?.value : obj;
    if (pattern.subject && getValue(q.subject) !== getValue(pattern.subject)) return false;
    if (pattern.predicate && getValue(q.predicate) !== getValue(pattern.predicate)) return false;
    if (pattern.object && getValue(q.object) !== getValue(pattern.object)) return false;
    if (pattern.graph && getValue(q.graph) !== getValue(pattern.graph)) return false;
    return true;
  }

  /**
   * Check if trigger should execute based on debounce/throttle settings
   * @param {string} triggerId - Trigger ID
   * @param {Object} trigger - Trigger configuration
   * @returns {boolean} Whether trigger should execute now
   * @private
   */
  shouldExecuteTrigger(triggerId, trigger) {
    const now = Date.now();

    if (trigger.throttleMs && trigger.throttleMs > 0) {
      const lastExecution = this.throttleTimestamps.get(triggerId) || 0;
      if (now - lastExecution < trigger.throttleMs) {
        return false;
      }
      this.throttleTimestamps.set(triggerId, now);
    }

    return true;
  }

  /**
   * Schedule debounced trigger execution
   * @param {string} triggerId - Trigger ID
   * @param {Array} operationIds - Operations to execute
   * @param {Object} trigger - Trigger configuration
   * @private
   */
  scheduleDebouncedExecution(triggerId, operationIds, trigger) {
    if (this.debounceTimers.has(triggerId)) {
      clearTimeout(this.debounceTimers.get(triggerId));
    }

    const timer = setTimeout(() => {
      this.executeOperations(operationIds).catch(error => {
        this.logger.error('[ReactiveSubscriptionManager] Error in debounced execution', { error });
      });
      this.debounceTimers.delete(triggerId);
    }, trigger.debounceMs);

    this.debounceTimers.set(triggerId, timer);
  }

  /**
   * Execute operations with error handling
   * @param {Array} operationIds - Operation IDs to execute
   * @returns {Promise<Array>} Results from executed operations
   * @private
   */
  async executeOperations(operationIds) {
    const results = [];
    for (const operationId of operationIds) {
      try {
        const result = await this.daemon.execute(operationId);
        results.push({ operationId, status: 'success', result });
      } catch (error) {
        this.logger.error(`[ReactiveSubscriptionManager] Operation ${operationId} failed: ${error.message}`);
        results.push({ operationId, status: 'error', error: error.message });
      }
    }
    return results;
  }

  /**
   * Handle a change event and execute matching operations
   * @param {Object} change - Change event from feed
   * @returns {Promise<Array>} Results from executed operations
   * @private
   */
  async handleChange(change) {
    const validated = ChangeEventSchema.parse(change);
    this.totalProcessed += 1;

    if (this.eventQueue.length >= this.config.maxQueueSize) {
      this.dropped += 1;
      this.logger.warn(`[ReactiveSubscriptionManager] Event queue full, dropping event (total dropped: ${this.dropped})`);
      return [];
    }

    const matched = [];

    for (const [triggerId, trigger] of this.triggers) {
      if (this.matchesPattern(validated, trigger.pattern)) {
        if (this.shouldExecuteTrigger(triggerId, trigger)) {
          matched.push({ triggerId, trigger, operationId: trigger.operationId });
        }
      }
    }

    if (matched.length === 0) return [];

    this.activeCount += 1;
    try {
      const results = [];

      for (const { triggerId, trigger, operationId } of matched) {
        if (trigger.debounceMs && trigger.debounceMs > 0) {
          this.scheduleDebouncedExecution(triggerId, [operationId], trigger);
          results.push({ operationId, status: 'debounced' });
        } else {
          const opResults = await this.executeOperations([operationId]);
          results.push(...opResults);
        }
      }

      if (this.activeCount >= this.config.drainThreshold) {
        await this.drain();
      }

      return results;
    } finally {
      this.activeCount -= 1;
    }
  }

  /**
   * Drain backpressure by waiting for active operations to complete
   * @returns {Promise<void>}
   */
  async drain() {
    if (this.activeCount < this.config.maxBackpressure) return;

    this.logger.warn(`[ReactiveSubscriptionManager] Backpressure threshold reached (${this.activeCount}/${this.config.maxBackpressure})`);

    const startTime = Date.now();

    while (this.activeCount >= this.config.maxBackpressure) {
      if (Date.now() - startTime > this.config.maxDrainWaitMs) {
        this.logger.error('[ReactiveSubscriptionManager] Backpressure drain timeout');
        break;
      }
      await new Promise(resolve => setTimeout(resolve, this.config.backpressureDrainMs));
    }
  }

  /**
   * Subscribe to a change feed with automatic cleanup tracking
   * @param {Object} feed - Change feed to subscribe to
   * @returns {Function} Unsubscribe function
   */
  subscribe(feed) {
    const validatedFeed = FeedSchema.parse(feed);

    const handler = (change) => {
      this.handleChange(change).catch(error => {
        this.logger.error('[ReactiveSubscriptionManager] Error handling change', { error });
      });
    };

    validatedFeed.subscribe(handler);

    const subscriptionId = `sub_${Date.now()}_${Math.random().toString(36).slice(2, 9)}`;
    this.subscriptions.set(subscriptionId, { feed: validatedFeed, handler });
    this.logger.debug(`[ReactiveSubscriptionManager] Subscription created: ${subscriptionId}`);

    return () => this.unsubscribe(subscriptionId);
  }

  /**
   * Unsubscribe from a change feed
   * @param {string} subscriptionId - Subscription ID to remove
   * @returns {boolean} Whether subscription was found and removed
   */
  unsubscribe(subscriptionId) {
    const validated = z.string().min(1).parse(subscriptionId);
    const subscription = this.subscriptions.get(validated);
    if (!subscription) return false;

    try {
      if (subscription.feed && typeof subscription.feed.unsubscribe === 'function') {
        subscription.feed.unsubscribe(subscription.handler);
      }
    } catch (error) {
      this.logger.warn(`[ReactiveSubscriptionManager] Error unsubscribing: ${error.message}`);
    }

    this.subscriptions.delete(validated);
    this.logger.debug(`[ReactiveSubscriptionManager] Subscription removed: ${validated}`);
    return true;
  }

  /**
   * Cleanup all subscriptions and timers
   * @returns {Promise<void>}
   */
  async cleanup() {
    for (const subscriptionId of this.subscriptions.keys()) {
      this.unsubscribe(subscriptionId);
    }

    for (const timer of this.debounceTimers.values()) {
      clearTimeout(timer);
    }

    this.debounceTimers.clear();
    this.throttleTimestamps.clear();
    this.triggers.clear();

    this.logger.info('[ReactiveSubscriptionManager] Cleanup complete');
  }

  /**
   * Get memory usage statistics
   * @returns {Object} Memory usage information
   */
  getMemoryStats() {
    const estimatedMemory = JSON.stringify({
      triggers: Array.from(this.triggers.values()),
      subscriptions: this.subscriptions.size,
      eventQueue: this.eventQueue.length,
    }).length;

    return {
      estimatedMemoryBytes: estimatedMemory,
      triggersCount: this.triggers.size,
      subscriptionsCount: this.subscriptions.size,
      eventQueueSize: this.eventQueue.length,
      totalProcessed: this.totalProcessed,
      droppedEvents: this.dropped,
    };
  }

  /**
   * Get statistics about trigger execution
   * @returns {Object} Trigger statistics
   */
  getStats() {
    return {
      activeCount: this.activeCount,
      triggersCount: this.triggers.size,
      subscriptionsCount: this.subscriptions.size,
      totalProcessed: this.totalProcessed,
      droppedEvents: this.dropped,
      memoryStats: this.getMemoryStats(),
    };
  }

  /**
   * List all registered triggers
   * @returns {Array} Array of trigger information
   */
  listTriggers() {
    return Array.from(this.triggers.entries()).map(([id, trigger]) => ({
      id,
      pattern: trigger.pattern,
      operationId: trigger.operationId,
      metadata: trigger.metadata,
      debounceMs: trigger.debounceMs,
      throttleMs: trigger.throttleMs,
    }));
  }
}

/**
 * Subscribe daemon to change feeds with reactive execution
 * @param {UnrdfDaemon} daemon - The daemon instance
 * @param {Array | Object} feeds - Change feed(s) to subscribe to
 * @param {Object} [config] - Configuration options
 * @returns {ReactiveSubscriptionManager} Subscription manager instance
 * @example
 * const daemon = new Daemon({ daemonId: uuid(), name: 'worker-1' });
 * const feed = createChangeFeed();
 * const manager = subscribeToChangeFeeds(daemon, feed);
 */
export function subscribeToChangeFeeds(daemon, feeds, config = {}) {
  // Validate daemon structure without parsing (to preserve method binding)
  DaemonSchema.parse(daemon);
  const validatedConfig = StreamingConfigSchema.parse(config);
  const feedArray = Array.isArray(feeds) ? feeds : [feeds];
  const manager = new ReactiveSubscriptionManager(daemon, validatedConfig);  // Use original daemon

  for (const feed of feedArray) {
    FeedSchema.parse(feed);
    manager.subscribe(feed);
  }

  return manager;
}

/**
 * Register a reactive trigger that executes daemon operations on pattern match
 * @param {UnrdfDaemon} daemon - The daemon instance
 * @param {string | Object} pattern - SPARQL query or event pattern to match
 * @param {string} operationId - Daemon operation to execute on match
 * @param {Object} [metadata] - Optional metadata about the trigger
 * @returns {string} Trigger ID for future removal
 * @example
 * const triggerId = registerReactiveTrigger(
 *   daemon,
 *   { type: 'add', subject: 'http://example.org/person/1' },
 *   'sync-cache'
 * );
 */
export function registerReactiveTrigger(daemon, pattern, operationId, metadata = {}) {
  DaemonSchema.parse(daemon);
  const validatedPattern = PatternSchema.parse(pattern);
  const validatedOperationId = z.string().min(1).parse(operationId);
  const validatedMetadata = z.record(z.any()).optional().parse(metadata) || {};

  if (!daemon._reactiveManager) {
    throw new Error('Daemon must be subscribed to change feeds first');
  }
  return daemon._reactiveManager.registerTrigger(validatedPattern, validatedOperationId, validatedMetadata);
}

/**
 * Create a daemon with streaming integration pre-configured
 * @param {UnrdfDaemon} daemon - Daemon instance to integrate with feeds
 * @param {Array | Object} feeds - Change feed(s) to subscribe to
 * @param {Object} [config] - Configuration options
 * @returns {ReactiveSubscriptionManager} Subscription manager instance
 * @example
 * import { Daemon } from '../daemon.mjs';
 * import { createChangeFeed } from '@unrdf/streaming/streaming/change-feed.mjs';
 *
 * const daemon = new Daemon({ daemonId: uuid(), name: 'reactive-worker-1' });
 * const feed = createChangeFeed();
 * const manager = createDaemonFromChangeFeeds(daemon, feed);
 * daemon.schedule({ id: 'sync', handler: () => syncData() });
 * manager.registerTrigger('add', 'sync');
 */
export function createDaemonFromChangeFeeds(daemon, feeds, config = {}) {
  // Validate daemon structure without parsing (to preserve method binding)
  DaemonSchema.parse(daemon);
  const validatedConfig = StreamingConfigSchema.parse(config);
  const manager = subscribeToChangeFeeds(daemon, feeds, validatedConfig);  // Use original daemon
  daemon._reactiveManager = manager;  // Attach to original daemon
  return manager;
}
