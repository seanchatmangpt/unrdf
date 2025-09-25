/**
 * @fileoverview ObservableStore - Event-emitting N3.Store wrapper
 * 
 * Extends N3.Store with event emission on all mutation operations.
 * Supports before/after hooks with veto/transform capabilities.
 * 
 * @version 1.0.0
 * @author GitVan Team
 * @license MIT
 */

import { Store, DataFactory } from "n3";
import { EventBus, EVENTS } from "./event-bus.mjs";

const { namedNode, literal, blankNode, quad, defaultGraph } = DataFactory;

/**
 * ObservableStore extends N3.Store with event emission
 * 
 * Emits events on all mutation operations:
 * - beforeAddQuad / afterAddQuad
 * - beforeRemoveQuad / afterRemoveQuad
 * - beforeAddQuads / afterAddQuads
 * - beforeRemoveQuads / afterRemoveQuads
 * - beforeClear / afterClear
 * 
 * Before hooks can veto operations by returning false.
 * After hooks execute asynchronously after operations complete.
 */
export class ObservableStore extends Store {
  constructor(quads = [], options = {}) {
    super(quads);
    
    this.eventBus = options.eventBus || new EventBus(options.eventBusOptions);
    this.enabled = options.enabled !== false;
    this.batchMode = false;
    this.batchOperations = [];
    
    // Track operation context
    this.currentOperation = null;
    this.operationStack = [];
  }

  /**
   * Register a hook for store events
   * @param {string} event - Event name
   * @param {Function} handler - Event handler
   * @param {Object} [options] - Hook options
   * @returns {Object} Registration result with unregister function
   */
  on(event, handler, options = {}) {
    return this.eventBus.on(event, handler, options);
  }

  /**
   * Unregister a hook
   * @param {string} event - Event name
   * @param {string|symbol} hookId - Hook identifier
   * @returns {boolean} True if hook was removed
   */
  off(event, hookId) {
    return this.eventBus.off(event, hookId);
  }

  /**
   * Start batch mode - operations will be queued
   */
  startBatch() {
    this.batchMode = true;
    this.batchOperations = [];
    this.eventBus.startBatch();
  }

  /**
   * End batch mode and process all queued operations
   * @returns {Promise<boolean>} True if all operations were allowed
   */
  async endBatch() {
    if (!this.batchMode) return true;

    this.batchMode = false;
    const operations = [...this.batchOperations];
    this.batchOperations = [];

    // Process all batch operations
    for (const operation of operations) {
      await operation();
    }

    return this.eventBus.endBatch();
  }

  /**
   * Add a single quad with event emission
   * @param {Quad} quad - Quad to add
   * @returns {Quad} The added quad
   */
  addQuad(s, p, o, g) {
    if (!this.enabled) {
      return super.addQuad(s, p, o, g);
    }

    const quad = this._createQuad(s, p, o, g);
    
    if (this.batchMode) {
      this.batchOperations.push(() => this._addQuadWithEvents(quad));
      return quad;
    }

    return this._addQuadWithEvents(quad);
  }

  /**
   * Add multiple quads with event emission
   * @param {Array<Quad>} quads - Quads to add
   * @returns {Store} This store
   */
  addQuads(quads) {
    if (!this.enabled) {
      return super.addQuads(quads);
    }

    if (this.batchMode) {
      this.batchOperations.push(() => this._addQuadsWithEvents(quads));
      return this;
    }

    return this._addQuadsWithEvents(quads);
  }

  /**
   * Remove a single quad with event emission
   * @param {Quad} quad - Quad to remove
   * @returns {Quad} The removed quad
   */
  removeQuad(s, p, o, g) {
    if (!this.enabled) {
      return super.removeQuad(s, p, o, g);
    }

    const quad = this._createQuad(s, p, o, g);
    
    if (this.batchMode) {
      this.batchOperations.push(() => this._removeQuadWithEvents(quad));
      return quad;
    }

    return this._removeQuadWithEvents(quad);
  }

  /**
   * Remove multiple quads with event emission
   * @param {Array<Quad>} quads - Quads to remove
   * @returns {Store} This store
   */
  removeQuads(quads) {
    if (!this.enabled) {
      return super.removeQuads(quads);
    }

    if (this.batchMode) {
      this.batchOperations.push(() => this._removeQuadsWithEvents(quads));
      return this;
    }

    return this._removeQuadsWithEvents(quads);
  }

  /**
   * Clear all quads with event emission
   * @returns {Store} This store
   */
  clear() {
    if (!this.enabled) {
      return super.clear();
    }

    if (this.batchMode) {
      this.batchOperations.push(() => this._clearWithEvents());
      return this;
    }

    return this._clearWithEvents();
  }

  /**
   * Add quad with before/after event emission
   * @private
   */
  async _addQuadWithEvents(quad) {
    const context = this.eventBus.createContext('manual', {
      operation: 'addQuad',
      quadCount: 1
    });

    const payload = {
      event: EVENTS.BEFORE_ADD_QUAD,
      quad,
      context,
      store: this,
      engine: this.engine
    };

    // Emit before event and check for veto
    const allowed = await this.eventBus.emit(EVENTS.BEFORE_ADD_QUAD, payload);
    if (!allowed) {
      return quad; // Operation vetoed
    }

    // Perform the operation
    const result = super.addQuad(quad);

    // Emit after event
    const afterPayload = {
      event: EVENTS.AFTER_ADD_QUAD,
      quad,
      context: { ...context, completed: true },
      store: this,
      engine: this.engine
    };

    this.eventBus.emit(EVENTS.AFTER_ADD_QUAD, afterPayload);

    return result;
  }

  /**
   * Add quads with before/after event emission
   * @private
   */
  async _addQuadsWithEvents(quads) {
    const context = this.eventBus.createContext('manual', {
      operation: 'addQuads',
      quadCount: quads.length
    });

    const payload = {
      event: EVENTS.BEFORE_ADD_QUADS,
      quad: quads,
      context,
      store: this,
      engine: this.engine
    };

    // Emit before event and check for veto
    const allowed = await this.eventBus.emit(EVENTS.BEFORE_ADD_QUADS, payload);
    if (!allowed) {
      return this; // Operation vetoed
    }

    // Perform the operation
    const result = super.addQuads(quads);

    // Emit after event
    const afterPayload = {
      event: EVENTS.AFTER_ADD_QUADS,
      quad: quads,
      context: { ...context, completed: true },
      store: this,
      engine: this.engine
    };

    this.eventBus.emit(EVENTS.AFTER_ADD_QUADS, afterPayload);

    return result;
  }

  /**
   * Remove quad with before/after event emission
   * @private
   */
  async _removeQuadWithEvents(quad) {
    const context = this.eventBus.createContext('manual', {
      operation: 'removeQuad',
      quadCount: 1
    });

    const payload = {
      event: EVENTS.BEFORE_REMOVE_QUAD,
      quad,
      context,
      store: this,
      engine: this.engine
    };

    // Emit before event and check for veto
    const allowed = await this.eventBus.emit(EVENTS.BEFORE_REMOVE_QUAD, payload);
    if (!allowed) {
      return quad; // Operation vetoed
    }

    // Perform the operation
    const result = super.removeQuad(quad);

    // Emit after event
    const afterPayload = {
      event: EVENTS.AFTER_REMOVE_QUAD,
      quad,
      context: { ...context, completed: true },
      store: this,
      engine: this.engine
    };

    this.eventBus.emit(EVENTS.AFTER_REMOVE_QUAD, afterPayload);

    return result;
  }

  /**
   * Remove quads with before/after event emission
   * @private
   */
  async _removeQuadsWithEvents(quads) {
    const context = this.eventBus.createContext('manual', {
      operation: 'removeQuads',
      quadCount: quads.length
    });

    const payload = {
      event: EVENTS.BEFORE_REMOVE_QUADS,
      quad: quads,
      context,
      store: this,
      engine: this.engine
    };

    // Emit before event and check for veto
    const allowed = await this.eventBus.emit(EVENTS.BEFORE_REMOVE_QUADS, payload);
    if (!allowed) {
      return this; // Operation vetoed
    }

    // Perform the operation
    const result = super.removeQuads(quads);

    // Emit after event
    const afterPayload = {
      event: EVENTS.AFTER_REMOVE_QUADS,
      quad: quads,
      context: { ...context, completed: true },
      store: this,
      engine: this.engine
    };

    this.eventBus.emit(EVENTS.AFTER_REMOVE_QUADS, afterPayload);

    return result;
  }

  /**
   * Clear store with before/after event emission
   * @private
   */
  async _clearWithEvents() {
    const quads = [...this]; // Get all quads before clearing
    const context = this.eventBus.createContext('manual', {
      operation: 'clear',
      quadCount: quads.length
    });

    const payload = {
      event: EVENTS.BEFORE_CLEAR,
      quad: quads,
      context,
      store: this,
      engine: this.engine
    };

    // Emit before event and check for veto
    const allowed = await this.eventBus.emit(EVENTS.BEFORE_CLEAR, payload);
    if (!allowed) {
      return this; // Operation vetoed
    }

    // Perform the operation
    const result = super.clear();

    // Emit after event
    const afterPayload = {
      event: EVENTS.AFTER_CLEAR,
      quad: quads,
      context: { ...context, completed: true },
      store: this,
      engine: this.engine
    };

    this.eventBus.emit(EVENTS.AFTER_CLEAR, afterPayload);

    return result;
  }

  /**
   * Create a quad from components
   * @private
   */
  _createQuad(s, p, o, g) {
    if (s && typeof s === 'object' && s.termType) {
      // Already a quad
      return s;
    }
    return quad(s, p, o, g || defaultGraph());
  }

  /**
   * Set the engine reference for event payloads
   * @param {RdfEngine} engine - RDF engine instance
   */
  setEngine(engine) {
    this.engine = engine;
  }

  /**
   * Get event bus statistics
   * @returns {Object} Event bus statistics
   */
  getEventStats() {
    return this.eventBus.getStats();
  }

  /**
   * Enable or disable event emission
   * @param {boolean} enabled - Whether to enable events
   */
  setEventEnabled(enabled) {
    this.enabled = enabled;
    this.eventBus.setEnabled(enabled);
  }

  /**
   * Clear all hooks and reset statistics
   */
  clearHooks() {
    this.eventBus.clear();
  }
}
