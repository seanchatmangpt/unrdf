/**
 * @fileoverview Context Manager - Manages CLI contexts for different sidecar configurations
 *
 * @description
 * ContextManager handles creating, retrieving, updating, and switching between
 * different CLI contexts. Each context represents a configuration for connecting
 * to a specific sidecar endpoint.
 *
 * @module cli/core/context
 * @version 2.4.0
 * @license MIT
 */

import { mkdir, readFile, writeFile } from 'node:fs/promises';
import { join } from 'node:path';
import { homedir } from 'node:os';
import { existsSync } from 'node:fs';
import { trace, metrics } from '@opentelemetry/api';

const tracer = trace.getTracer('unrdf-cli-context', '2.4.0');
const meter = metrics.getMeter('unrdf-cli-context', '2.4.0');

const contextOperationCounter = meter.createCounter('context.operations', {
  description: 'Number of context operations',
});

const contextErrorCounter = meter.createCounter('context.errors', {
  description: 'Number of context operation errors',
});

/**
 * @typedef {Object} SidecarConfig
 * @property {string} endpoint - Sidecar endpoint URL
 */

/**
 * @typedef {Object} Context
 * @property {string} name - Context name
 * @property {SidecarConfig} sidecar - Sidecar configuration
 * @property {string} createdAt - ISO 8601 creation timestamp
 * @property {string} [updatedAt] - ISO 8601 update timestamp
 */

/**
 * Context Manager for CLI context operations
 */
export class ContextManager {
  /**
   * Create a new ContextManager instance
   */
  constructor() {
    /** @type {Map<string, Context>} */
    this.contexts = new Map();

    /** @type {string|null} */
    this.currentContext = null;

    /** @type {string} */
    this.configDir = join(homedir(), '.unrdf');

    /** @type {string} */
    this.configFile = join(this.configDir, 'contexts.json');
  }

  /**
   * Initialize the context manager by loading existing contexts
   *
   * @returns {Promise<void>}
   */
  async init() {
    return tracer.startActiveSpan('context.init', async (span) => {
      try {
        // Ensure config directory exists
        if (!existsSync(this.configDir)) {
          await mkdir(this.configDir, { recursive: true });
          span.setAttribute('context.dir.created', true);
        }

        // Load existing contexts if file exists
        if (existsSync(this.configFile)) {
          const data = await readFile(this.configFile, 'utf-8');
          const config = JSON.parse(data);

          // Restore contexts
          if (config.contexts) {
            this.contexts = new Map(Object.entries(config.contexts));
            span.setAttribute('context.count', this.contexts.size);
          }

          // Restore current context
          if (config.currentContext) {
            this.currentContext = config.currentContext;
            span.setAttribute('context.current', this.currentContext);
          }
        } else {
          // Create empty config file
          await this.saveContexts();
          span.setAttribute('context.file.created', true);
        }

        contextOperationCounter.add(1, { operation: 'init', status: 'success' });
        span.setStatus({ code: 1 }); // OK
      } catch (error) {
        contextErrorCounter.add(1, { operation: 'init' });
        span.recordException(error);
        span.setStatus({ code: 2, message: error.message }); // ERROR
        throw error;
      } finally {
        span.end();
      }
    });
  }

  /**
   * Save contexts to disk
   *
   * @returns {Promise<void>}
   * @private
   */
  async saveContexts() {
    return tracer.startActiveSpan('context.save', async (span) => {
      try {
        const config = {
          contexts: Object.fromEntries(this.contexts),
          currentContext: this.currentContext,
        };

        await writeFile(this.configFile, JSON.stringify(config, null, 2), 'utf-8');

        span.setAttribute('context.count', this.contexts.size);
        span.setAttribute('context.current', this.currentContext || 'none');
        contextOperationCounter.add(1, { operation: 'save', status: 'success' });
        span.setStatus({ code: 1 }); // OK
      } catch (error) {
        contextErrorCounter.add(1, { operation: 'save' });
        span.recordException(error);
        span.setStatus({ code: 2, message: error.message }); // ERROR
        throw error;
      } finally {
        span.end();
      }
    });
  }

  /**
   * Create a new context
   *
   * @param {string} name - Context name
   * @param {Object} config - Context configuration
   * @param {SidecarConfig} config.sidecar - Sidecar configuration
   * @returns {Promise<Context>}
   */
  async createContext(name, config) {
    return tracer.startActiveSpan('context.create', async (span) => {
      try {
        span.setAttribute('context.name', name);

        if (this.contexts.has(name)) {
          const error = new Error(`Context "${name}" already exists`);
          span.recordException(error);
          contextErrorCounter.add(1, { operation: 'create', reason: 'already_exists' });
          throw error;
        }

        /** @type {Context} */
        const context = {
          name,
          sidecar: config.sidecar,
          createdAt: new Date().toISOString(),
        };

        this.contexts.set(name, context);
        await this.saveContexts();

        span.setAttribute('context.sidecar.endpoint', config.sidecar.endpoint);
        contextOperationCounter.add(1, { operation: 'create', status: 'success' });
        span.setStatus({ code: 1 }); // OK

        return context;
      } catch (error) {
        contextErrorCounter.add(1, { operation: 'create' });
        span.recordException(error);
        span.setStatus({ code: 2, message: error.message }); // ERROR
        throw error;
      } finally {
        span.end();
      }
    });
  }

  /**
   * Get a context by name
   *
   * @param {string} name - Context name
   * @returns {Context|null}
   */
  getContext(name) {
    return tracer.startActiveSpan('context.get', (span) => {
      try {
        span.setAttribute('context.name', name);
        const context = this.contexts.get(name);

        if (context) {
          span.setAttribute('context.found', true);
          span.setAttribute('context.sidecar.endpoint', context.sidecar.endpoint);
          contextOperationCounter.add(1, { operation: 'get', status: 'success' });
        } else {
          span.setAttribute('context.found', false);
          contextOperationCounter.add(1, { operation: 'get', status: 'not_found' });
        }

        span.setStatus({ code: 1 }); // OK
        return context || null;
      } catch (error) {
        contextErrorCounter.add(1, { operation: 'get' });
        span.recordException(error);
        span.setStatus({ code: 2, message: error.message }); // ERROR
        throw error;
      } finally {
        span.end();
      }
    });
  }

  /**
   * List all contexts
   *
   * @returns {Context[]}
   */
  listContexts() {
    return tracer.startActiveSpan('context.list', (span) => {
      try {
        const contexts = Array.from(this.contexts.values());
        span.setAttribute('context.count', contexts.length);
        contextOperationCounter.add(1, { operation: 'list', status: 'success' });
        span.setStatus({ code: 1 }); // OK
        return contexts;
      } catch (error) {
        contextErrorCounter.add(1, { operation: 'list' });
        span.recordException(error);
        span.setStatus({ code: 2, message: error.message }); // ERROR
        throw error;
      } finally {
        span.end();
      }
    });
  }

  /**
   * Switch to a different context
   *
   * @param {string} name - Context name
   * @returns {Promise<void>}
   */
  async useContext(name) {
    return tracer.startActiveSpan('context.use', async (span) => {
      try {
        span.setAttribute('context.name', name);

        if (!this.contexts.has(name)) {
          const error = new Error(`Context "${name}" does not exist`);
          span.recordException(error);
          contextErrorCounter.add(1, { operation: 'use', reason: 'not_found' });
          throw error;
        }

        this.currentContext = name;
        await this.saveContexts();

        span.setAttribute('context.switched', true);
        contextOperationCounter.add(1, { operation: 'use', status: 'success' });
        span.setStatus({ code: 1 }); // OK
      } catch (error) {
        contextErrorCounter.add(1, { operation: 'use' });
        span.recordException(error);
        span.setStatus({ code: 2, message: error.message }); // ERROR
        throw error;
      } finally {
        span.end();
      }
    });
  }

  /**
   * Delete a context
   *
   * @param {string} name - Context name
   * @returns {Promise<void>}
   */
  async deleteContext(name) {
    return tracer.startActiveSpan('context.delete', async (span) => {
      try {
        span.setAttribute('context.name', name);

        if (!this.contexts.has(name)) {
          const error = new Error(`Context "${name}" does not exist`);
          span.recordException(error);
          contextErrorCounter.add(1, { operation: 'delete', reason: 'not_found' });
          throw error;
        }

        if (this.currentContext === name) {
          const error = new Error('Cannot delete current context');
          span.recordException(error);
          contextErrorCounter.add(1, { operation: 'delete', reason: 'current_context' });
          throw error;
        }

        this.contexts.delete(name);
        await this.saveContexts();

        span.setAttribute('context.deleted', true);
        contextOperationCounter.add(1, { operation: 'delete', status: 'success' });
        span.setStatus({ code: 1 }); // OK
      } catch (error) {
        contextErrorCounter.add(1, { operation: 'delete' });
        span.recordException(error);
        span.setStatus({ code: 2, message: error.message }); // ERROR
        throw error;
      } finally {
        span.end();
      }
    });
  }

  /**
   * Get the current context
   *
   * @returns {Context|null}
   */
  getCurrentContext() {
    return tracer.startActiveSpan('context.getCurrent', (span) => {
      try {
        if (!this.currentContext) {
          span.setAttribute('context.current', 'none');
          contextOperationCounter.add(1, { operation: 'getCurrent', status: 'none' });
          span.setStatus({ code: 1 }); // OK
          return null;
        }

        const context = this.contexts.get(this.currentContext);
        span.setAttribute('context.current', this.currentContext);

        if (context) {
          span.setAttribute('context.found', true);
          contextOperationCounter.add(1, { operation: 'getCurrent', status: 'success' });
        } else {
          span.setAttribute('context.found', false);
          contextOperationCounter.add(1, { operation: 'getCurrent', status: 'not_found' });
        }

        span.setStatus({ code: 1 }); // OK
        return context || null;
      } catch (error) {
        contextErrorCounter.add(1, { operation: 'getCurrent' });
        span.recordException(error);
        span.setStatus({ code: 2, message: error.message }); // ERROR
        throw error;
      } finally {
        span.end();
      }
    });
  }

  /**
   * Update an existing context
   *
   * @param {string} name - Context name
   * @param {Object} updates - Updates to apply
   * @param {SidecarConfig} [updates.sidecar] - Updated sidecar configuration
   * @returns {Promise<Context>}
   */
  async updateContext(name, updates) {
    return tracer.startActiveSpan('context.update', async (span) => {
      try {
        span.setAttribute('context.name', name);

        if (!this.contexts.has(name)) {
          const error = new Error(`Context "${name}" does not exist`);
          span.recordException(error);
          contextErrorCounter.add(1, { operation: 'update', reason: 'not_found' });
          throw error;
        }

        const context = this.contexts.get(name);

        // Apply updates
        if (updates.sidecar) {
          context.sidecar = { ...context.sidecar, ...updates.sidecar };
          span.setAttribute('context.sidecar.updated', true);
        }

        context.updatedAt = new Date().toISOString();

        this.contexts.set(name, context);
        await this.saveContexts();

        contextOperationCounter.add(1, { operation: 'update', status: 'success' });
        span.setStatus({ code: 1 }); // OK

        return context;
      } catch (error) {
        contextErrorCounter.add(1, { operation: 'update' });
        span.recordException(error);
        span.setStatus({ code: 2, message: error.message }); // ERROR
        throw error;
      } finally {
        span.end();
      }
    });
  }
}
