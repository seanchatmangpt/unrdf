/**
 * Cross-Package Execution Environment
 * Coordinate execution across the monorepo
 */

export class ExecutionEnvironment {
  constructor(registry, projectRoot) {
    this.registry = registry;
    this.projectRoot = projectRoot;
    this.context = new Map();
    this.hooks = new Map();
    this.middleware = [];
  }

  /**
   * Execute function with context
   */
  async execute(fn, context = {}) {
    const ctx = new Map(this.context);
    Object.entries(context).forEach(([k, v]) => ctx.set(k, v));

    // Run middleware
    for (const mw of this.middleware) {
      await mw({ context: ctx, environment: this });
    }

    return fn({ context: ctx, environment: this });
  }

  /**
   * Register hook
   */
  registerHook(name, fn) {
    if (!this.hooks.has(name)) {
      this.hooks.set(name, []);
    }
    this.hooks.get(name).push(fn);
  }

  /**
   * Emit hook
   */
  async emit(hookName, data) {
    const handlers = this.hooks.get(hookName) || [];
    const results = [];

    for (const handler of handlers) {
      results.push(await handler(data));
    }

    return results;
  }

  /**
   * Register middleware
   */
  use(fn) {
    this.middleware.push(fn);
  }

  /**
   * Set context value
   */
  set(key, value) {
    this.context.set(key, value);
  }

  /**
   * Get context value
   */
  get(key) {
    return this.context.get(key);
  }

  /**
   * Get all context
   */
  getContext() {
    return Object.fromEntries(this.context);
  }
}

export default ExecutionEnvironment;
