/**
 * @file CLI Context Manager (like kubeconfig)
 * @module cli-v2/core/context
 *
 * @description
 * Manages CLI contexts for different environments (dev, staging, production).
 * Includes OpenTelemetry instrumentation for observability.
 */

import { readFile, writeFile, access, mkdir } from "node:fs/promises";
import { join, dirname } from "node:path";
import { homedir } from "node:os";
import { trace, SpanStatusCode } from "@opentelemetry/api";

const CONTEXT_DIR = join(homedir(), ".unrdf");
const CONTEXT_FILE = join(CONTEXT_DIR, "contexts.json");
const CURRENT_CONTEXT_FILE = join(CONTEXT_DIR, "current-context");

const tracer = trace.getTracer("unrdf-context-manager");

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
    return await tracer.startActiveSpan(
      "context.manager.init",
      async (span) => {
        try {
          span.setAttributes({
            "context.dir": CONTEXT_DIR,
            "context.file": CONTEXT_FILE,
            "current.context.file": CURRENT_CONTEXT_FILE,
          });

          await mkdir(CONTEXT_DIR, { recursive: true });
          await this.loadContexts();
          await this.loadCurrentContext();

          span.setAttributes({
            "context.count": this.contexts.size,
            "current.context": this.currentContext || "none",
          });

          span.setStatus({ code: SpanStatusCode.OK });
        } catch (error) {
          span.recordException(error);
          span.setStatus({
            code: SpanStatusCode.ERROR,
            message: error.message,
          });
          throw error;
        } finally {
          span.end();
        }
      },
    );
  }

  /**
   * Load contexts from file
   */
  async loadContexts() {
    return await tracer.startActiveSpan(
      "context.manager.load",
      async (span) => {
        try {
          span.setAttribute("context.file", CONTEXT_FILE);

          await access(CONTEXT_FILE);
          const data = await readFile(CONTEXT_FILE, "utf-8");
          const contexts = JSON.parse(data);

          for (const [name, context] of Object.entries(contexts)) {
            this.contexts.set(name, context);
          }

          span.setAttributes({
            "context.loaded": true,
            "context.count": this.contexts.size,
          });

          span.setStatus({ code: SpanStatusCode.OK });
        } catch (error) {
          // File doesn't exist, start with empty contexts
          this.contexts = new Map();

          span.setAttributes({
            "context.loaded": false,
            "context.count": 0,
            "error.type": "file_not_found",
          });

          span.setStatus({ code: SpanStatusCode.OK });
        } finally {
          span.end();
        }
      },
    );
  }

  /**
   * Save contexts to file
   */
  async saveContexts() {
    return await tracer.startActiveSpan(
      "context.manager.save",
      async (span) => {
        try {
          span.setAttributes({
            "context.file": CONTEXT_FILE,
            "context.count": this.contexts.size,
          });

          const contexts = Object.fromEntries(this.contexts);
          await writeFile(CONTEXT_FILE, JSON.stringify(contexts, null, 2));

          span.setStatus({ code: SpanStatusCode.OK });
        } catch (error) {
          span.recordException(error);
          span.setStatus({
            code: SpanStatusCode.ERROR,
            message: error.message,
          });
          throw error;
        } finally {
          span.end();
        }
      },
    );
  }

  /**
   * Load current context
   */
  async loadCurrentContext() {
    return await tracer.startActiveSpan(
      "context.manager.load.current",
      async (span) => {
        try {
          span.setAttribute("current.context.file", CURRENT_CONTEXT_FILE);

          await access(CURRENT_CONTEXT_FILE);
          this.currentContext = await readFile(CURRENT_CONTEXT_FILE, "utf-8");

          span.setAttributes({
            "current.context.loaded": true,
            "current.context": this.currentContext,
          });

          span.setStatus({ code: SpanStatusCode.OK });
        } catch (error) {
          this.currentContext = null;

          span.setAttributes({
            "current.context.loaded": false,
            "current.context": "none",
            "error.type": "file_not_found",
          });

          span.setStatus({ code: SpanStatusCode.OK });
        } finally {
          span.end();
        }
      },
    );
  }

  /**
   * Save current context
   */
  async saveCurrentContext() {
    return await tracer.startActiveSpan(
      "context.manager.save.current",
      async (span) => {
        try {
          span.setAttributes({
            "current.context.file": CURRENT_CONTEXT_FILE,
            "current.context": this.currentContext || "none",
          });

          if (this.currentContext) {
            await writeFile(CURRENT_CONTEXT_FILE, this.currentContext);
          }

          span.setStatus({ code: SpanStatusCode.OK });
        } catch (error) {
          span.recordException(error);
          span.setStatus({
            code: SpanStatusCode.ERROR,
            message: error.message,
          });
          throw error;
        } finally {
          span.end();
        }
      },
    );
  }

  /**
   * Create a new context
   * @param {string} name - Context name
   * @param {Object} config - Context configuration
   */
  async createContext(name, config) {
    return await tracer.startActiveSpan(
      "context.manager.create",
      async (span) => {
        try {
          span.setAttributes({
            "context.name": name,
            "context.sidecar.endpoint": config.sidecar?.endpoint || "none",
          });

          this.contexts.set(name, {
            name,
            ...config,
            createdAt: new Date().toISOString(),
          });
          await this.saveContexts();

          span.setAttributes({
            "context.created": true,
            "context.count": this.contexts.size,
          });

          span.setStatus({ code: SpanStatusCode.OK });
        } catch (error) {
          span.recordException(error);
          span.setStatus({
            code: SpanStatusCode.ERROR,
            message: error.message,
          });
          throw error;
        } finally {
          span.end();
        }
      },
    );
  }

  /**
   * Get a context by name
   * @param {string} name - Context name
   * @returns {Object|null} Context configuration
   */
  getContext(name) {
    return tracer.startActiveSpan("context.manager.get", (span) => {
      try {
        span.setAttribute("context.name", name);

        const context = this.contexts.get(name) || null;

        span.setAttributes({
          "context.found": context !== null,
          "context.sidecar.endpoint": context?.sidecar?.endpoint || "none",
        });

        span.setStatus({ code: SpanStatusCode.OK });
        return context;
      } catch (error) {
        span.recordException(error);
        span.setStatus({ code: SpanStatusCode.ERROR, message: error.message });
        throw error;
      } finally {
        span.end();
      }
    });
  }

  /**
   * Get current context
   * @returns {Object|null} Current context configuration
   */
  getCurrentContext() {
    return tracer.startActiveSpan("context.manager.get.current", (span) => {
      try {
        span.setAttribute("current.context", this.currentContext || "none");

        if (!this.currentContext) {
          span.setAttributes({
            "current.context.found": false,
          });
          span.setStatus({ code: SpanStatusCode.OK });
          return null;
        }

        const context = this.contexts.get(this.currentContext) || null;

        span.setAttributes({
          "current.context.found": context !== null,
          "current.context.sidecar.endpoint":
            context?.sidecar?.endpoint || "none",
        });

        span.setStatus({ code: SpanStatusCode.OK });
        return context;
      } catch (error) {
        span.recordException(error);
        span.setStatus({ code: SpanStatusCode.ERROR, message: error.message });
        throw error;
      } finally {
        span.end();
      }
    });
  }

  /**
   * Set current context
   * @param {string} name - Context name
   */
  async useContext(name) {
    return await tracer.startActiveSpan("context.manager.use", async (span) => {
      try {
        span.setAttribute("context.name", name);

        if (!this.contexts.has(name)) {
          span.setAttributes({
            "context.exists": false,
            "error.type": "context_not_found",
          });
          span.setStatus({
            code: SpanStatusCode.ERROR,
            message: `Context "${name}" does not exist`,
          });
          throw new Error(`Context "${name}" does not exist`);
        }

        this.currentContext = name;
        await this.saveCurrentContext();

        span.setAttributes({
          "context.exists": true,
          "current.context": name,
          "context.switched": true,
        });

        span.setStatus({ code: SpanStatusCode.OK });
      } catch (error) {
        span.recordException(error);
        span.setStatus({ code: SpanStatusCode.ERROR, message: error.message });
        throw error;
      } finally {
        span.end();
      }
    });
  }

  /**
   * List all contexts
   * @returns {Array} List of contexts
   */
  listContexts() {
    return tracer.startActiveSpan("context.manager.list", (span) => {
      try {
        const contexts = Array.from(this.contexts.values()).map((ctx) => ({
          ...ctx,
          current: ctx.name === this.currentContext,
        }));

        span.setAttributes({
          "context.count": contexts.length,
          "current.context": this.currentContext || "none",
        });

        span.setStatus({ code: SpanStatusCode.OK });
        return contexts;
      } catch (error) {
        span.recordException(error);
        span.setStatus({ code: SpanStatusCode.ERROR, message: error.message });
        throw error;
      } finally {
        span.end();
      }
    });
  }

  /**
   * Delete a context
   * @param {string} name - Context name
   */
  async deleteContext(name) {
    return await tracer.startActiveSpan(
      "context.manager.delete",
      async (span) => {
        try {
          span.setAttribute("context.name", name);

          // Check if context exists
          if (!this.contexts.has(name)) {
            span.setAttributes({
              "context.exists": false,
              "error.type": "context_not_found",
            });
            span.setStatus({
              code: SpanStatusCode.ERROR,
              message: "Context does not exist",
            });
            throw new Error(`Context "${name}" does not exist`);
          }

          if (name === this.currentContext) {
            span.setAttributes({
              "context.is.current": true,
              "error.type": "cannot_delete_current",
            });
            span.setStatus({
              code: SpanStatusCode.ERROR,
              message: "Cannot delete current context",
            });
            throw new Error(
              "Cannot delete current context. Switch to another context first.",
            );
          }

          this.contexts.delete(name);
          await this.saveContexts();

          span.setAttributes({
            "context.exists": true,
            "context.is.current": false,
            "context.deleted": true,
            "context.count": this.contexts.size,
          });

          span.setStatus({ code: SpanStatusCode.OK });
        } catch (error) {
          span.recordException(error);
          span.setStatus({
            code: SpanStatusCode.ERROR,
            message: error.message,
          });
          throw error;
        } finally {
          span.end();
        }
      },
    );
  }

  /**
   * Update a context
   * @param {string} name - Context name
   * @param {Object} updates - Context updates
   */
  async updateContext(name, updates) {
    return await tracer.startActiveSpan(
      "context.manager.update",
      async (span) => {
        try {
          span.setAttribute("context.name", name);

          const context = this.contexts.get(name);
          if (!context) {
            span.setAttributes({
              "context.exists": false,
              "error.type": "context_not_found",
            });
            span.setStatus({
              code: SpanStatusCode.ERROR,
              message: `Context "${name}" does not exist`,
            });
            throw new Error(`Context "${name}" does not exist`);
          }

          this.contexts.set(name, {
            ...context,
            ...updates,
            updatedAt: new Date().toISOString(),
          });

          await this.saveContexts();

          span.setAttributes({
            "context.exists": true,
            "context.updated": true,
            "context.count": this.contexts.size,
          });

          span.setStatus({ code: SpanStatusCode.OK });
        } catch (error) {
          span.recordException(error);
          span.setStatus({
            code: SpanStatusCode.ERROR,
            message: error.message,
          });
          throw error;
        } finally {
          span.end();
        }
      },
    );
  }
}
