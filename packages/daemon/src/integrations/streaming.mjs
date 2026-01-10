/**
 * @file Daemon Streaming Integration - Reactive Change Feed Subscriptions
 * @module @unrdf/daemon/integrations/streaming
 * @description Integrates daemon with streaming change feeds for reactive operation execution
 */

import { z } from 'zod';

const ReactiveTriggerSchema = z.object({
  pattern: z.union([z.string(), z.object({ type: z.enum(['add', 'remove', 'update']).optional(), subject: z.any().optional(), predicate: z.any().optional(), object: z.any().optional(), graph: z.any().optional() })]),
  operationId: z.string().min(1),
  metadata: z.record(z.any()).optional(),
});

const StreamingConfigSchema = z.object({
  maxBackpressure: z.number().int().positive().default(1000),
  batchSize: z.number().int().positive().default(100),
  drainThreshold: z.number().int().positive().default(500),
  logger: z.any().optional(),
});

const ChangeEventSchema = z.object({
  type: z.enum(['add', 'remove', 'update']),
  quad: z.object({ subject: z.any(), predicate: z.any(), object: z.any(), graph: z.any().optional() }),
  timestamp: z.number(),
  metadata: z.record(z.any()).optional(),
});

/**
 * Manages reactive subscriptions and pattern matching for daemon operations
 */
export class ReactiveSubscriptionManager {
  /**
   * Creates a new reactive subscription manager
   * @param {UnrdfDaemon} daemon - The daemon instance to execute operations on
   * @param {Object} config - Configuration options
   */
  constructor(daemon, config = {}) {
    const validated = StreamingConfigSchema.parse(config);
    this.daemon = daemon;
    this.config = validated;
    this.logger = validated.logger || console;
    this.triggers = new Map();
    this.activeCount = 0;
  }

  /**
   * Register a reactive trigger pattern
   * @param {string | Object} pattern - SPARQL query or event pattern
   * @param {string} operationId - Operation to execute on pattern match
   * @param {Object} [metadata] - Optional trigger metadata
   * @returns {string} Trigger ID
   */
  registerTrigger(pattern, operationId, metadata = {}) {
    const trigger = ReactiveTriggerSchema.parse({ pattern, operationId, metadata });
    const triggerId = `trigger_${Date.now()}_${Math.random().toString(36).slice(2, 9)}`;
    this.triggers.set(triggerId, trigger);
    this.logger.debug(`[ReactiveSubscriptionManager] Trigger registered: ${triggerId}`);
    return triggerId;
  }

  /**
   * Unregister a reactive trigger
   * @param {string} triggerId - Trigger ID to unregister
   * @returns {boolean} Whether trigger was found and removed
   */
  unregisterTrigger(triggerId) {
    const removed = this.triggers.delete(triggerId);
    if (removed) {
      this.logger.debug(`[ReactiveSubscriptionManager] Trigger unregistered: ${triggerId}`);
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
    if (typeof pattern === 'string') {
      return change.type === pattern;
    }
    if (pattern.type && pattern.type !== change.type) return false;
    const q = change.quad;
    if (pattern.subject && q.subject?.value !== pattern.subject?.value) return false;
    if (pattern.predicate && q.predicate?.value !== pattern.predicate?.value) return false;
    if (pattern.object && q.object?.value !== pattern.object?.value) return false;
    if (pattern.graph && q.graph?.value !== pattern.graph?.value) return false;
    return true;
  }

  /**
   * Handle a change event and execute matching operations
   * @param {Object} change - Change event from feed
   * @returns {Promise<Array>} Results from executed operations
   * @private
   */
  async handleChange(change) {
    const validated = ChangeEventSchema.parse(change);
    const matched = [];

    for (const [, trigger] of this.triggers) {
      if (this.matchesPattern(validated, trigger.pattern)) {
        matched.push(trigger.operationId);
      }
    }

    if (matched.length === 0) return [];

    this.activeCount += 1;
    try {
      const results = [];
      for (const operationId of matched) {
        try {
          const result = await this.daemon.execute(operationId);
          results.push({ operationId, status: 'success', result });
        } catch (error) {
          results.push({ operationId, status: 'error', error: error.message });
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

    this.logger.warn(`[ReactiveSubscriptionManager] Backpressure threshold reached (${this.activeCount})`);

    const maxWait = 5000;
    const startTime = Date.now();

    while (this.activeCount >= this.config.maxBackpressure) {
      if (Date.now() - startTime > maxWait) {
        this.logger.error('[ReactiveSubscriptionManager] Backpressure drain timeout');
        break;
      }
      await new Promise(resolve => setTimeout(resolve, 10));
    }
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
 * const daemon = new UnrdfDaemon({ id: 'worker-1' });
 * const feed = createChangeFeed();
 * const manager = subscribeToChangeFeeds(daemon, feed);
 */
export function subscribeToChangeFeeds(daemon, feeds, config = {}) {
  const feedArray = Array.isArray(feeds) ? feeds : [feeds];
  const manager = new ReactiveSubscriptionManager(daemon, config);

  for (const feed of feedArray) {
    feed.subscribe(change => {
      manager.handleChange(change).catch(error => {
        manager.logger.error('[subscribeToChangeFeeds] Error handling change', { error });
      });
    });
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
  if (!daemon._reactiveManager) {
    throw new Error('Daemon must be subscribed to change feeds first');
  }
  return daemon._reactiveManager.registerTrigger(pattern, operationId, metadata);
}

/**
 * Create a daemon with streaming integration pre-configured
 * @param {UnrdfDaemon} daemon - Daemon instance to integrate with feeds
 * @param {Array | Object} feeds - Change feed(s) to subscribe to
 * @param {Object} [config] - Configuration options
 * @returns {ReactiveSubscriptionManager} Subscription manager instance
 * @example
 * import { UnrdfDaemon } from '../daemon.mjs';
 * import { createChangeFeed } from '@unrdf/streaming/streaming/change-feed.mjs';
 *
 * const daemon = new UnrdfDaemon({ id: 'reactive-worker-1' });
 * const feed = createChangeFeed();
 * const manager = createDaemonFromChangeFeeds(daemon, feed);
 * daemon.schedule({ id: 'sync', handler: () => syncData() });
 * manager.registerTrigger('add', 'sync');
 */
export function createDaemonFromChangeFeeds(daemon, feeds, config = {}) {
  const manager = subscribeToChangeFeeds(daemon, feeds, config);
  daemon._reactiveManager = manager;
  return manager;
}
