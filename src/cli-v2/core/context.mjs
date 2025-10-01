/**
 * @file CLI Context Manager (like kubeconfig)
 * @module cli-v2/core/context
 *
 * @description
 * Manages CLI contexts for different environments (dev, staging, production).
 */

import { readFile, writeFile, access, mkdir } from 'node:fs/promises';
import { join, dirname } from 'node:path';
import { homedir } from 'node:os';

const CONTEXT_DIR = join(homedir(), '.unrdf');
const CONTEXT_FILE = join(CONTEXT_DIR, 'contexts.json');
const CURRENT_CONTEXT_FILE = join(CONTEXT_DIR, 'current-context');

/**
 * Context Manager for managing CLI contexts
 */
export class ContextManager {
  constructor(config = {}) {
    this.config = config;
    this.contexts = new Map();
    this.currentContext = null;
  }

  /**
   * Initialize context manager
   */
  async init() {
    await mkdir(CONTEXT_DIR, { recursive: true });
    await this.loadContexts();
    await this.loadCurrentContext();
  }

  /**
   * Load contexts from file
   */
  async loadContexts() {
    try {
      await access(CONTEXT_FILE);
      const data = await readFile(CONTEXT_FILE, 'utf-8');
      const contexts = JSON.parse(data);

      for (const [name, context] of Object.entries(contexts)) {
        this.contexts.set(name, context);
      }
    } catch {
      // File doesn't exist, start with empty contexts
      this.contexts = new Map();
    }
  }

  /**
   * Save contexts to file
   */
  async saveContexts() {
    const contexts = Object.fromEntries(this.contexts);
    await writeFile(CONTEXT_FILE, JSON.stringify(contexts, null, 2));
  }

  /**
   * Load current context
   */
  async loadCurrentContext() {
    try {
      await access(CURRENT_CONTEXT_FILE);
      this.currentContext = await readFile(CURRENT_CONTEXT_FILE, 'utf-8');
    } catch {
      this.currentContext = null;
    }
  }

  /**
   * Save current context
   */
  async saveCurrentContext() {
    if (this.currentContext) {
      await writeFile(CURRENT_CONTEXT_FILE, this.currentContext);
    }
  }

  /**
   * Create a new context
   * @param {string} name - Context name
   * @param {Object} config - Context configuration
   */
  async createContext(name, config) {
    this.contexts.set(name, {
      name,
      ...config,
      createdAt: new Date().toISOString()
    });
    await this.saveContexts();
  }

  /**
   * Get a context by name
   * @param {string} name - Context name
   * @returns {Object|null} Context configuration
   */
  getContext(name) {
    return this.contexts.get(name) || null;
  }

  /**
   * Get current context
   * @returns {Object|null} Current context configuration
   */
  getCurrentContext() {
    if (!this.currentContext) {
      return null;
    }
    return this.contexts.get(this.currentContext) || null;
  }

  /**
   * Set current context
   * @param {string} name - Context name
   */
  async useContext(name) {
    if (!this.contexts.has(name)) {
      throw new Error(`Context "${name}" does not exist`);
    }

    this.currentContext = name;
    await this.saveCurrentContext();
  }

  /**
   * List all contexts
   * @returns {Array} List of contexts
   */
  listContexts() {
    return Array.from(this.contexts.values()).map(ctx => ({
      ...ctx,
      current: ctx.name === this.currentContext
    }));
  }

  /**
   * Delete a context
   * @param {string} name - Context name
   */
  async deleteContext(name) {
    if (name === this.currentContext) {
      throw new Error('Cannot delete current context. Switch to another context first.');
    }

    this.contexts.delete(name);
    await this.saveContexts();
  }

  /**
   * Update a context
   * @param {string} name - Context name
   * @param {Object} updates - Context updates
   */
  async updateContext(name, updates) {
    const context = this.contexts.get(name);
    if (!context) {
      throw new Error(`Context "${name}" does not exist`);
    }

    this.contexts.set(name, {
      ...context,
      ...updates,
      updatedAt: new Date().toISOString()
    });

    await this.saveContexts();
  }
}
