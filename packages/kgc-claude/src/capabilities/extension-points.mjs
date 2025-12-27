/**
 * @fileoverview Extension Points Framework for KGC Claude
 *
 * Provides a hyper-advanced extensibility system with:
 * - Named extension slots with schema validation
 * - Multi-provider slots with priority ordering
 * - Async extension execution with timeout control
 * - Event-based plugin communication (pub/sub)
 * - Extension pipelines with data transformation
 * - Conditional extension activation
 * - Extension point composition
 *
 * Enables plugins to extend core functionality at well-defined points.
 *
 * @module extension-points
 */

import { z } from 'zod';
import { randomUUID } from 'crypto';

/**
 * Extension point schema
 */
const ExtensionPointSchema = z.object({
  id: z.string().describe('Unique extension point identifier'),
  name: z.string().describe('Human-readable name'),
  description: z.string().optional(),
  inputSchema: z.any().describe('Zod schema for input validation'),
  outputSchema: z.any().optional().describe('Zod schema for output validation'),
  execution: z.enum(['sync', 'async', 'parallel', 'pipeline']).default('async'),
  timeout: z.number().int().positive().default(5000).describe('Timeout in ms'),
  maxProviders: z.number().int().positive().default(10),
  required: z.boolean().default(false).describe('Must have at least one provider'),
  composition: z.enum(['first', 'all', 'merge', 'reduce']).default('all')
});

/**
 * Extension provider schema
 */
const ExtensionProviderSchema = z.object({
  id: z.string().describe('Unique provider identifier'),
  extensionPointId: z.string().describe('Extension point this provides'),
  pluginId: z.string().describe('Plugin that provides this extension'),
  priority: z.number().int().default(100).describe('Execution priority (lower = earlier)'),
  handler: z.function().describe('Extension handler function'),
  condition: z.function().optional().describe('Conditional activation function'),
  metadata: z.record(z.any()).default({})
});

/**
 * Event schema for pub/sub
 */
const EventSchema = z.object({
  id: z.string().describe('Event identifier'),
  type: z.string().describe('Event type'),
  source: z.string().describe('Event source (plugin ID)'),
  data: z.any(),
  timestamp: z.date().default(() => new Date()),
  metadata: z.record(z.any()).default({})
});

/**
 * Extension Points Manager
 *
 * Central registry for extension points and their providers.
 */
export class ExtensionPointsManager {
  /**
   * @param {Object} options - Manager configuration
   */
  constructor(options = {}) {
    this.managerId = options.managerId || randomUUID();

    /** @type {Map<string, Object>} Extension points by ID */
    this.extensionPoints = new Map();

    /** @type {Map<string, Array<Object>>} Providers by extension point ID */
    this.providers = new Map();

    /** @type {Map<string, Set<Function>>} Event subscribers */
    this.eventBus = new Map();

    /** @type {Array<Object>} Event history */
    this.eventHistory = [];

    /** @type {Map<string, Object>} Pipelines */
    this.pipelines = new Map();

    /** @type {Object} Execution statistics */
    this.stats = {
      totalExecutions: 0,
      totalErrors: 0,
      totalTimeouts: 0,
      executionsByPoint: {}
    };
  }

  /**
   * Register a new extension point.
   *
   * @param {Object} extensionPoint - Extension point definition
   * @returns {{success: boolean, pointId: string}}
   */
  registerExtensionPoint(extensionPoint) {
    const validation = ExtensionPointSchema.safeParse(extensionPoint);
    if (!validation.success) {
      throw new Error(
        `Invalid extension point: ${validation.error.message}`
      );
    }

    const point = validation.data;

    if (this.extensionPoints.has(point.id)) {
      throw new Error(`Extension point ${point.id} already registered`);
    }

    this.extensionPoints.set(point.id, point);
    this.providers.set(point.id, []);
    this.stats.executionsByPoint[point.id] = {
      count: 0,
      errors: 0,
      totalDuration: 0,
      averageDuration: 0
    };

    return { success: true, pointId: point.id };
  }

  /**
   * Register a provider for an extension point.
   *
   * @param {Object} provider - Provider definition
   * @returns {{success: boolean, providerId: string}}
   */
  registerProvider(provider) {
    const validation = ExtensionProviderSchema.safeParse(provider);
    if (!validation.success) {
      throw new Error(
        `Invalid extension provider: ${validation.error.message}`
      );
    }

    const prov = validation.data;
    const point = this.extensionPoints.get(prov.extensionPointId);

    if (!point) {
      throw new Error(
        `Extension point ${prov.extensionPointId} not found`
      );
    }

    const providers = this.providers.get(prov.extensionPointId);
    if (providers.length >= point.maxProviders) {
      throw new Error(
        `Maximum providers (${point.maxProviders}) reached for ${prov.extensionPointId}`
      );
    }

    providers.push(prov);

    // Sort by priority (lower = earlier)
    providers.sort((a, b) => a.priority - b.priority);

    return { success: true, providerId: prov.id };
  }

  /**
   * Execute an extension point with all registered providers.
   *
   * @param {string} extensionPointId - Extension point identifier
   * @param {any} input - Input data
   * @param {Object} context - Execution context
   * @returns {Promise<any>} Extension execution result
   */
  async executeExtensionPoint(extensionPointId, input, context = {}) {
    const point = this.extensionPoints.get(extensionPointId);
    if (!point) {
      throw new Error(`Extension point ${extensionPointId} not found`);
    }

    const startTime = Date.now();

    try {
      // Validate input
      if (point.inputSchema) {
        const validation = point.inputSchema.safeParse(input);
        if (!validation.success) {
          throw new Error(
            `Invalid input for ${extensionPointId}: ${validation.error.message}`
          );
        }
      }

      const providers = this.providers.get(extensionPointId) || [];

      // Check if required and no providers
      if (point.required && providers.length === 0) {
        throw new Error(
          `Extension point ${extensionPointId} requires at least one provider`
        );
      }

      // Filter providers by condition
      const activeProviders = await this._filterProviders(
        providers,
        input,
        context
      );

      // Execute based on execution mode
      let result;
      switch (point.execution) {
        case 'sync':
          result = this._executeSync(point, activeProviders, input, context);
          break;
        case 'async':
          result = await this._executeAsync(point, activeProviders, input, context);
          break;
        case 'parallel':
          result = await this._executeParallel(point, activeProviders, input, context);
          break;
        case 'pipeline':
          result = await this._executePipeline(point, activeProviders, input, context);
          break;
        default:
          throw new Error(`Unknown execution mode: ${point.execution}`);
      }

      // Validate output
      if (point.outputSchema) {
        const validation = point.outputSchema.safeParse(result);
        if (!validation.success) {
          throw new Error(
            `Invalid output from ${extensionPointId}: ${validation.error.message}`
          );
        }
      }

      // Compose results based on composition strategy
      const composedResult = this._composeResults(point, result);

      // Update stats
      const duration = Date.now() - startTime;
      this._updateStats(extensionPointId, duration, false);

      this.stats.totalExecutions++;

      return composedResult;
    } catch (error) {
      this.stats.totalErrors++;
      this._updateStats(extensionPointId, Date.now() - startTime, true);
      throw error;
    }
  }

  /**
   * Create a pipeline of extension points.
   *
   * @param {string} pipelineId - Pipeline identifier
   * @param {Array<string>} extensionPointIds - Ordered extension points
   * @returns {{success: boolean, pipelineId: string}}
   */
  createPipeline(pipelineId, extensionPointIds) {
    // Validate all points exist
    for (const pointId of extensionPointIds) {
      if (!this.extensionPoints.has(pointId)) {
        throw new Error(`Extension point ${pointId} not found`);
      }
    }

    this.pipelines.set(pipelineId, {
      id: pipelineId,
      points: extensionPointIds,
      createdAt: new Date()
    });

    return { success: true, pipelineId };
  }

  /**
   * Execute a pipeline.
   *
   * @param {string} pipelineId - Pipeline identifier
   * @param {any} initialInput - Initial input
   * @param {Object} context - Execution context
   * @returns {Promise<any>} Final result
   */
  async executePipeline(pipelineId, initialInput, context = {}) {
    const pipeline = this.pipelines.get(pipelineId);
    if (!pipeline) {
      throw new Error(`Pipeline ${pipelineId} not found`);
    }

    let result = initialInput;

    for (const pointId of pipeline.points) {
      result = await this.executeExtensionPoint(pointId, result, context);
    }

    return result;
  }

  /**
   * Subscribe to events on the event bus.
   *
   * @param {string} eventType - Event type to subscribe to
   * @param {Function} handler - Event handler function
   * @returns {{success: boolean, subscriptionId: string}}
   */
  subscribe(eventType, handler) {
    if (!this.eventBus.has(eventType)) {
      this.eventBus.set(eventType, new Set());
    }

    const subscribers = this.eventBus.get(eventType);
    subscribers.add(handler);

    const subscriptionId = randomUUID();

    return { success: true, subscriptionId };
  }

  /**
   * Unsubscribe from events.
   *
   * @param {string} eventType - Event type
   * @param {Function} handler - Event handler to remove
   * @returns {{success: boolean}}
   */
  unsubscribe(eventType, handler) {
    const subscribers = this.eventBus.get(eventType);
    if (subscribers) {
      subscribers.delete(handler);
    }

    return { success: true };
  }

  /**
   * Publish an event to subscribers.
   *
   * @param {Object} event - Event to publish
   * @returns {Promise<{success: boolean, handlerCount: number}>}
   */
  async publish(event) {
    const validation = EventSchema.safeParse(event);
    if (!validation.success) {
      throw new Error(`Invalid event: ${validation.error.message}`);
    }

    const evt = validation.data;

    // Store in history
    this.eventHistory.push(evt);
    if (this.eventHistory.length > 1000) {
      this.eventHistory.shift();
    }

    const subscribers = this.eventBus.get(evt.type) || new Set();
    const wildcardSubscribers = this.eventBus.get('*') || new Set();

    const allSubscribers = new Set([...subscribers, ...wildcardSubscribers]);

    // Execute all handlers
    const promises = Array.from(allSubscribers).map(handler =>
      Promise.resolve().then(() => handler(evt)).catch(e => {
        console.error(`Event handler error for ${evt.type}:`, e);
      })
    );

    await Promise.all(promises);

    return { success: true, handlerCount: allSubscribers.size };
  }

  /**
   * Get extension point info.
   *
   * @param {string} extensionPointId - Extension point identifier
   * @returns {Object} Extension point details
   */
  getExtensionPoint(extensionPointId) {
    const point = this.extensionPoints.get(extensionPointId);
    if (!point) {
      return null;
    }

    const providers = this.providers.get(extensionPointId) || [];
    const stats = this.stats.executionsByPoint[extensionPointId];

    return {
      ...point,
      providerCount: providers.length,
      providers: providers.map(p => ({
        id: p.id,
        pluginId: p.pluginId,
        priority: p.priority,
        metadata: p.metadata
      })),
      stats
    };
  }

  /**
   * List all extension points.
   *
   * @returns {Array<Object>} Extension points
   */
  listExtensionPoints() {
    return Array.from(this.extensionPoints.keys()).map(id =>
      this.getExtensionPoint(id)
    );
  }

  /**
   * Get event history.
   *
   * @param {Object} filter - Filter criteria
   * @returns {Array<Object>} Events
   */
  getEventHistory(filter = {}) {
    let events = [...this.eventHistory];

    if (filter.type) {
      events = events.filter(e => e.type === filter.type);
    }

    if (filter.source) {
      events = events.filter(e => e.source === filter.source);
    }

    if (filter.since) {
      events = events.filter(e => e.timestamp >= filter.since);
    }

    return events;
  }

  /**
   * Get execution statistics.
   *
   * @returns {Object} Statistics
   */
  getStats() {
    return { ...this.stats };
  }

  /**
   * Filter providers by condition.
   * @private
   */
  async _filterProviders(providers, input, context) {
    const filtered = [];

    for (const provider of providers) {
      if (!provider.condition) {
        filtered.push(provider);
        continue;
      }

      try {
        const shouldActivate = await provider.condition(input, context);
        if (shouldActivate) {
          filtered.push(provider);
        }
      } catch (e) {
        console.warn(
          `Provider ${provider.id} condition failed: ${e.message}`
        );
      }
    }

    return filtered;
  }

  /**
   * Execute synchronously.
   * @private
   */
  _executeSync(point, providers, input, context) {
    const results = [];

    for (const provider of providers) {
      try {
        const result = provider.handler(input, context);
        results.push({ providerId: provider.id, result, error: null });
      } catch (error) {
        results.push({
          providerId: provider.id,
          result: null,
          error: error.message
        });
      }
    }

    return results;
  }

  /**
   * Execute asynchronously (sequential).
   * @private
   */
  async _executeAsync(point, providers, input, context) {
    const results = [];

    for (const provider of providers) {
      try {
        const result = await this._executeWithTimeout(
          provider.handler(input, context),
          point.timeout
        );
        results.push({ providerId: provider.id, result, error: null });
      } catch (error) {
        results.push({
          providerId: provider.id,
          result: null,
          error: error.message
        });
      }
    }

    return results;
  }

  /**
   * Execute in parallel.
   * @private
   */
  async _executeParallel(point, providers, input, context) {
    const promises = providers.map(provider =>
      this._executeWithTimeout(
        provider.handler(input, context),
        point.timeout
      )
        .then(result => ({ providerId: provider.id, result, error: null }))
        .catch(error => ({
          providerId: provider.id,
          result: null,
          error: error.message
        }))
    );

    return await Promise.all(promises);
  }

  /**
   * Execute as pipeline (output feeds next input).
   * @private
   */
  async _executePipeline(point, providers, input, context) {
    let currentInput = input;
    const results = [];

    for (const provider of providers) {
      try {
        const result = await this._executeWithTimeout(
          provider.handler(currentInput, context),
          point.timeout
        );
        results.push({ providerId: provider.id, result, error: null });
        currentInput = result; // Feed to next stage
      } catch (error) {
        results.push({
          providerId: provider.id,
          result: null,
          error: error.message
        });
        break; // Stop pipeline on error
      }
    }

    return results;
  }

  /**
   * Compose results based on strategy.
   * @private
   */
  _composeResults(point, results) {
    switch (point.composition) {
      case 'first':
        // Return first successful result
        return results.find(r => !r.error)?.result;

      case 'all':
        // Return all results
        return results;

      case 'merge':
        // Merge all successful results (assumes objects)
        return results
          .filter(r => !r.error)
          .reduce((acc, r) => ({ ...acc, ...r.result }), {});

      case 'reduce':
        // Reduce all results to single value
        return results
          .filter(r => !r.error)
          .map(r => r.result);

      default:
        return results;
    }
  }

  /**
   * Execute with timeout.
   * @private
   */
  async _executeWithTimeout(promise, timeout) {
    return Promise.race([
      promise,
      new Promise((_, reject) =>
        setTimeout(() => {
          this.stats.totalTimeouts++;
          reject(new Error(`Extension execution timeout (${timeout}ms)`));
        }, timeout)
      )
    ]);
  }

  /**
   * Update execution statistics.
   * @private
   */
  _updateStats(pointId, duration, isError) {
    const stats = this.stats.executionsByPoint[pointId];
    stats.count++;
    stats.totalDuration += duration;
    stats.averageDuration = stats.totalDuration / stats.count;

    if (isError) {
      stats.errors++;
    }
  }
}

export {
  ExtensionPointSchema,
  ExtensionProviderSchema,
  EventSchema
};
