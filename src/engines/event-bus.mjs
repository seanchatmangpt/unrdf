/**
 * @fileoverview Uniform Event Bus for Knowledge Hooks System
 * 
 * Provides a centralized event system shared by ObservableStore and RdfEngine.
 * Supports before/after hooks, veto/transform capabilities, and deterministic hook management.
 * 
 * @version 1.0.0
 * @author GitVan Team
 * @license MIT
 */

import { DataFactory } from "n3";

const { namedNode, literal, blankNode, quad, defaultGraph } = DataFactory;

/**
 * Hook identity for deterministic registration/unregistration
 * @typedef {string|symbol} HookId
 */

/**
 * Event handler function signature
 * @typedef {Function} EventHandler
 * @param {EventPayload} payload - Event payload
 * @returns {boolean|void|Promise<boolean|void>} - For before hooks: true to allow, false to veto
 */

/**
 * Event payload structure
 * @typedef {Object} EventPayload
 * @property {string} event - Event name
 * @property {Quad|Array<Quad>|null} quad - The quad(s) involved
 * @property {EventContext} context - Event context
 * @property {Store} store - Current store state
 * @property {RdfEngine} engine - Engine instance
 */

/**
 * Event context with metadata
 * @typedef {Object} EventContext
 * @property {Date} timestamp - When the event occurred
 * @property {string} source - Source of the operation ('manual' | 'import' | 'reasoning' | 'update' | 'batch')
 * @property {Object} metadata - Additional metadata
 * @property {string} [operationId] - Unique operation identifier
 */

/**
 * Hook registration options
 * @typedef {Object} HookOptions
 * @property {boolean} [async=true] - Execute hook asynchronously
 * @property {boolean} [batch=false] - Process in batches
 * @property {Object} [filter] - Quad pattern filter
 * @property {Function} [callback] - User callback function
 * @property {Function} [onError] - Error handler
 */

/**
 * Hook registration result
 * @typedef {Object} HookRegistration
 * @property {HookId} id - Hook identifier
 * @property {Function} unregister - Function to unregister the hook
 */

/**
 * Event bus statistics
 * @typedef {Object} EventBusStats
 * @property {number} totalEvents - Total events emitted
 * @property {number} totalHooks - Total registered hooks
 * @property {number} errors - Total hook errors
 * @property {Object} eventCounts - Count per event type
 * @property {number} avgExecutionTime - Average hook execution time
 */

/**
 * Uniform Event Bus for Knowledge Hooks System
 * 
 * Provides centralized event management with:
 * - Before/after hook support
 * - Veto/transform capabilities
 * - Deterministic hook identity
 * - Error handling
 * - Performance monitoring
 */
export class EventBus {
  constructor(options = {}) {
    this.hooks = new Map(); // event -> Map<HookId, HookRegistration>
    this.stats = {
      totalEvents: 0,
      totalHooks: 0,
      errors: 0,
      eventCounts: new Map(),
      executionTimes: []
    };
    this.enabled = options.enabled !== false;
    this.batchMode = false;
    this.batchEvents = [];
    this.onError = options.onError || null;
    this.maxExecutionTime = options.maxExecutionTime || 1000; // 1 second
  }

  /**
   * Register a hook for an event
   * @param {string} event - Event name
   * @param {EventHandler} handler - Event handler function
   * @param {HookOptions} [options={}] - Hook options
   * @returns {HookRegistration} Registration result
   */
  on(event, handler, options = {}) {
    if (!this.enabled) {
      return { id: Symbol('disabled'), unregister: () => {} };
    }

    if (typeof event !== 'string' || !event.trim()) {
      throw new TypeError('Event name must be a non-empty string');
    }

    if (typeof handler !== 'function') {
      throw new TypeError('Handler must be a function');
    }

    const hookId = options.id || Symbol(`hook_${Date.now()}_${Math.random()}`);
    
    if (!this.hooks.has(event)) {
      this.hooks.set(event, new Map());
    }

    const eventHooks = this.hooks.get(event);
    
    if (eventHooks.has(hookId)) {
      throw new Error(`Hook with id ${String(hookId)} already registered for event ${event}`);
    }

    const registration = {
      id: hookId,
      handler,
      options: {
        async: true,
        batch: false,
        ...options
      },
      registeredAt: Date.now()
    };

    eventHooks.set(hookId, registration);
    this.stats.totalHooks++;

    return {
      id: hookId,
      unregister: () => this.off(event, hookId)
    };
  }

  /**
   * Unregister a hook
   * @param {string} event - Event name
   * @param {HookId} hookId - Hook identifier
   * @returns {boolean} True if hook was removed
   */
  off(event, hookId) {
    const eventHooks = this.hooks.get(event);
    if (!eventHooks) return false;

    const removed = eventHooks.delete(hookId);
    if (removed) {
      this.stats.totalHooks--;
    }

    if (eventHooks.size === 0) {
      this.hooks.delete(event);
    }

    return removed;
  }

  /**
   * Unregister all hooks for an event
   * @param {string} event - Event name
   * @returns {number} Number of hooks removed
   */
  offAll(event) {
    const eventHooks = this.hooks.get(event);
    if (!eventHooks) return 0;

    const count = eventHooks.size;
    this.hooks.delete(event);
    this.stats.totalHooks -= count;
    return count;
  }

  /**
   * Emit an event to all registered hooks
   * @param {string} event - Event name
   * @param {EventPayload} payload - Event payload
   * @returns {Promise<boolean>} True if all before hooks allowed the operation
   */
  async emit(event, payload) {
    if (!this.enabled) return true;

    const startTime = Date.now();
    this.stats.totalEvents++;
    this.stats.eventCounts.set(event, (this.stats.eventCounts.get(event) || 0) + 1);

    if (this.batchMode) {
      this.batchEvents.push({ event, payload, timestamp: startTime });
      return true;
    }

    const eventHooks = this.hooks.get(event);
    if (!eventHooks || eventHooks.size === 0) {
      return true;
    }

    // Separate before and after hooks
    const beforeHooks = [];
    const afterHooks = [];

    for (const [hookId, registration] of eventHooks) {
      if (event.startsWith('before')) {
        beforeHooks.push(registration);
      } else {
        afterHooks.push(registration);
      }
    }

    // Execute before hooks synchronously and check for vetoes
    for (const registration of beforeHooks) {
      try {
        const result = await this._executeHook(registration, payload);
        if (result === false) {
          // Hook vetoed the operation
          return false;
        }
      } catch (error) {
        this._handleHookError(error, registration, event, payload);
        // Re-throw errors from before hooks so they can veto operations
        throw error;
      }
    }

    // Execute after hooks asynchronously (fire-and-forget)
    for (const registration of afterHooks) {
      if (registration.options.async) {
        // Fire and forget
        this._executeHook(registration, payload).catch(error => {
          this._handleHookError(error, registration, event, payload);
        });
      } else {
        try {
          await this._executeHook(registration, payload);
        } catch (error) {
          this._handleHookError(error, registration, event, payload);
        }
      }
    }

    const executionTime = Date.now() - startTime;
    this.stats.executionTimes.push(executionTime);
    
    // Keep only last 100 execution times for rolling average
    if (this.stats.executionTimes.length > 100) {
      this.stats.executionTimes.shift();
    }

    return true;
  }

  /**
   * Execute a single hook
   * @private
   */
  async _executeHook(registration, payload) {
    const { handler, options } = registration;
    
    // Apply filter if specified
    if (options.filter && !this._matchesFilter(payload.quad, options.filter)) {
      return true;
    }

    // Set execution timeout
    const timeoutPromise = new Promise((_, reject) => {
      setTimeout(() => reject(new Error('Hook execution timeout')), this.maxExecutionTime);
    });

    const executionPromise = handler(payload);
    
    return Promise.race([executionPromise, timeoutPromise]);
  }

  /**
   * Check if quad matches filter pattern
   * @private
   */
  _matchesFilter(quad, filter) {
    if (!quad) return true;
    
    if (Array.isArray(quad)) {
      return quad.some(q => this._matchesSingleQuad(q, filter));
    }
    
    return this._matchesSingleQuad(quad, filter);
  }

  /**
   * Check if single quad matches filter pattern
   * @private
   */
  _matchesSingleQuad(quad, filter) {
    if (!quad || !filter) return true;

    if (filter.subject && quad.subject.value !== filter.subject) return false;
    if (filter.predicate && quad.predicate.value !== filter.predicate) return false;
    if (filter.object && quad.object.value !== filter.object) return false;
    if (filter.graph && quad.graph.value !== filter.graph) return false;

    return true;
  }

  /**
   * Handle hook execution errors
   * @private
   */
  _handleHookError(error, registration, event, payload) {
    this.stats.errors++;
    
    if (this.onError) {
      this.onError(error, {
        hookId: registration.id,
        event,
        payload,
        registration
      });
    } else {
      console.error(`Hook error in ${event}:`, error);
    }
  }

  /**
   * Start batch mode - events will be queued
   */
  startBatch() {
    this.batchMode = true;
    this.batchEvents = [];
  }

  /**
   * End batch mode and process all queued events
   * @returns {Promise<boolean>} True if all operations were allowed
   */
  async endBatch() {
    if (!this.batchMode) return true;

    this.batchMode = false;
    const events = [...this.batchEvents];
    this.batchEvents = [];

    // Process all batch events
    for (const { event, payload } of events) {
      await this.emit(event, payload);
    }

    return true;
  }

  /**
   * Create event context
   * @param {string} source - Event source
   * @param {Object} [metadata={}] - Additional metadata
   * @returns {EventContext} Event context
   */
  createContext(source, metadata = {}) {
    return {
      timestamp: new Date(),
      source,
      metadata,
      operationId: `op_${Date.now()}_${Math.random().toString(36).substr(2, 9)}`
    };
  }

  /**
   * Get event bus statistics
   * @returns {EventBusStats} Statistics
   */
  getStats() {
    const avgExecutionTime = this.stats.executionTimes.length > 0
      ? this.stats.executionTimes.reduce((sum, time) => sum + time, 0) / this.stats.executionTimes.length
      : 0;

    return {
      totalEvents: this.stats.totalEvents,
      totalHooks: this.stats.totalHooks,
      errors: this.stats.errors,
      eventCounts: Object.fromEntries(this.stats.eventCounts),
      avgExecutionTime: Math.round(avgExecutionTime * 100) / 100
    };
  }

  /**
   * Clear all hooks and reset statistics
   */
  clear() {
    this.hooks.clear();
    this.stats = {
      totalEvents: 0,
      totalHooks: 0,
      errors: 0,
      eventCounts: new Map(),
      executionTimes: []
    };
  }

  /**
   * Enable or disable the event bus
   * @param {boolean} enabled - Whether to enable the event bus
   */
  setEnabled(enabled) {
    this.enabled = enabled;
  }
}

/**
 * Event constants for all graph operations
 */
export const EVENTS = {
  // Store-level events
  BEFORE_ADD_QUAD: 'beforeAddQuad',
  AFTER_ADD_QUAD: 'afterAddQuad',
  BEFORE_REMOVE_QUAD: 'beforeRemoveQuad',
  AFTER_REMOVE_QUAD: 'afterRemoveQuad',
  BEFORE_ADD_QUADS: 'beforeAddQuads',
  AFTER_ADD_QUADS: 'afterAddQuads',
  BEFORE_REMOVE_QUADS: 'beforeRemoveQuads',
  AFTER_REMOVE_QUADS: 'afterRemoveQuads',
  BEFORE_CLEAR: 'beforeClear',
  AFTER_CLEAR: 'afterClear',

  // Engine-level events
  BEFORE_IMPORT: 'beforeImport',
  AFTER_IMPORT: 'afterImport',
  BEFORE_SERIALIZE: 'beforeSerialize',
  AFTER_SERIALIZE: 'afterSerialize',
  BEFORE_REASON: 'beforeReason',
  AFTER_REASON: 'afterReason',
  BEFORE_VALIDATE: 'beforeValidate',
  AFTER_VALIDATE: 'afterValidate',
  BEFORE_CANONICALIZE: 'beforeCanonicalize',
  AFTER_CANONICALIZE: 'afterCanonicalize',
  BEFORE_SKOLEMIZE: 'beforeSkolemize',
  AFTER_SKOLEMIZE: 'afterSkolemize',

  // SPARQL events
  BEFORE_QUERY: 'beforeQuery',
  AFTER_QUERY: 'afterQuery',
  BEFORE_UPDATE: 'beforeUpdate',
  AFTER_UPDATE: 'afterUpdate'
};

/**
 * Create a new event bus instance
 * @param {Object} [options] - Event bus options
 * @returns {EventBus} New event bus instance
 */
export function createEventBus(options = {}) {
  return new EventBus(options);
}
