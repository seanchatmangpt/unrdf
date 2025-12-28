/**
 * API Adapters for v5 → v6 Migration
 *
 * Maps deprecated v5 APIs to v6 equivalents with:
 * - Deprecation warnings
 * - Automatic receipt generation
 * - Migration hints in console
 *
 * @module @unrdf/v6-compat/adapters
 */

import { createStore as createStoreV6 } from '@unrdf/oxigraph';
import { z } from 'zod';

/**
 * Emit deprecation warning
 *
 * @param {string} oldAPI - Deprecated API name
 * @param {string} newAPI - Replacement API name
 * @param {string} [hint] - Migration hint
 */
function deprecationWarning(oldAPI, newAPI, hint = '') {
  const message = `
⚠️  DEPRECATION WARNING: ${oldAPI} is deprecated in UNRDF v6
→  Use: ${newAPI}
${hint ? `💡 Hint: ${hint}` : ''}
📖 Migration guide: https://github.com/unrdf/unrdf/blob/main/docs/v6/MIGRATION_PLAN.md
  `.trim();

  if (process.env.NODE_ENV !== 'test') {
    console.warn(message);
  }

  // Emit event for tracking
  if (typeof process?.emit === 'function') {
    process.emit('deprecation', {
      oldAPI,
      newAPI,
      timestamp: Date.now(),
      stack: new Error().stack
    });
  }
}

/**
 * v5 Store adapter
 *
 * @deprecated Use createStore() from @unrdf/oxigraph
 * @returns {Promise<Store>} Store instance
 *
 * @example
 * // v5 (deprecated)
 * import { Store } from 'n3';
 * const store = new Store();
 *
 * // v6 (use this)
 * import { createStore } from '@unrdf/oxigraph';
 * const store = await createStore();
 *
 * // Migration bridge (temporary)
 * import { createStore } from '@unrdf/v6-compat/adapters';
 * const store = await createStore(); // Warns + uses v6 API
 */
export async function createStore(options = {}) {
  deprecationWarning(
    'new Store() from n3',
    'createStore() from @unrdf/oxigraph',
    'Oxigraph provides 10x faster SPARQL execution'
  );

  return createStoreV6(options);
}

/**
 * Wrap workflow execution with receipt generation
 *
 * @param {Object} workflow - Workflow instance
 * @returns {Object} Wrapped workflow with receipt support
 *
 * @example
 * // v5 (no receipts)
 * const result = await workflow.run(task);
 *
 * // v6 (receipts required)
 * import { wrapWorkflow } from '@unrdf/v6-compat/adapters';
 * const wrapped = wrapWorkflow(workflow);
 * const { result, receipt } = await wrapped.execute(task);
 */
export function wrapWorkflow(workflow) {
  if (!workflow || typeof workflow !== 'object') {
    throw new Error('wrapWorkflow requires a workflow object');
  }

  const wrapped = Object.create(workflow);

  // Wrap run() → execute() with receipt
  if (workflow.run) {
    wrapped.execute = async function execute(task) {
      deprecationWarning(
        'workflow.run(task)',
        'workflow.execute(task) with receipt',
        'Receipts enable replay and verification'
      );

      const startTime = performance.now();
      const result = await workflow.run(task);
      const endTime = performance.now();

      // Generate receipt
      const receipt = {
        version: '6.0.0-alpha.1',
        operation: 'workflow.execute',
        task: task?.id || 'unknown',
        timestamp: Date.now(),
        duration: endTime - startTime,
        result: typeof result === 'object' ? JSON.stringify(result) : String(result)
      };

      return { result, receipt };
    };

    // Keep run() for backward compat but warn
    wrapped.run = async function run(task) {
      deprecationWarning(
        'workflow.run(task)',
        'workflow.execute(task)',
        'Use execute() to get receipts'
      );
      return workflow.run(task);
    };
  }

  return wrapped;
}

/**
 * SPARQL query adapter
 *
 * Wraps string queries in template literal with timeout
 *
 * @param {Object} federation - Federation instance
 * @returns {Object} Wrapped federation with typed queries
 *
 * @example
 * // v5 (string queries)
 * const results = await federation.query('SELECT * WHERE { ?s ?p ?o }');
 *
 * // v6 (typed queries)
 * import { wrapFederation } from '@unrdf/v6-compat/adapters';
 * const wrapped = wrapFederation(federation);
 * const results = await wrapped.querySparql('SELECT * WHERE { ?s ?p ?o }');
 */
export function wrapFederation(federation) {
  if (!federation || typeof federation !== 'object') {
    throw new Error('wrapFederation requires a federation object');
  }

  const wrapped = Object.create(federation);

  if (federation.query) {
    wrapped.querySparql = async function querySparql(queryString, options = {}) {
      deprecationWarning(
        'federation.query(string)',
        'federation.query(sparql`...`)',
        'Template literals prevent injection attacks'
      );

      const timeout = options.timeout || 5000;
      const timeoutPromise = new Promise((_, reject) => {
        setTimeout(() => reject(new Error(`Query timeout after ${timeout}ms`)), timeout);
      });

      const queryPromise = federation.query(queryString, options);

      return Promise.race([queryPromise, timeoutPromise]);
    };

    // Keep original for compat
    wrapped.query = federation.query;
  }

  return wrapped;
}

/**
 * Stream adapter (EventEmitter → AsyncIterator)
 *
 * @param {EventEmitter} stream - v5 stream
 * @returns {AsyncIterableIterator} v6 async iterator
 *
 * @example
 * // v5 (EventEmitter)
 * stream.on('data', (quad) => console.log(quad));
 * stream.on('error', (err) => console.error(err));
 *
 * // v6 (AsyncIterator)
 * import { streamToAsync } from '@unrdf/v6-compat/adapters';
 * for await (const quad of streamToAsync(stream)) {
 *   console.log(quad);
 * }
 */
export async function* streamToAsync(stream) {
  deprecationWarning(
    'stream.on("data", ...)',
    'for await (const x of stream)',
    'Async iterators provide backpressure handling'
  );

  const queue = [];
  let done = false;
  let error = null;

  stream.on('data', (data) => queue.push(data));
  stream.on('end', () => { done = true; });
  stream.on('error', (err) => { error = err; done = true; });

  while (!done || queue.length > 0) {
    if (error) throw error;
    if (queue.length > 0) {
      yield queue.shift();
    } else {
      // Wait for next data or end
      await new Promise((resolve) => setTimeout(resolve, 10));
    }
  }
}

/**
 * Receipt generation wrapper for arbitrary functions
 *
 * @param {Function} fn - Function to wrap
 * @param {Object} options - Receipt options
 * @returns {Function} Wrapped function with receipt
 *
 * @example
 * import { withReceipt } from '@unrdf/v6-compat/adapters';
 *
 * const processData = withReceipt(
 *   (data) => data.map(x => x * 2),
 *   { operation: 'processData' }
 * );
 *
 * const { result, receipt } = await processData([1, 2, 3]);
 * // receipt = { operation: 'processData', timestamp: ..., result: ... }
 */
export function withReceipt(fn, options = {}) {
  if (typeof fn !== 'function') {
    throw new Error('withReceipt requires a function');
  }

  return async function wrappedWithReceipt(...args) {
    const startTime = performance.now();
    const result = await fn(...args);
    const endTime = performance.now();

    const receipt = {
      version: '6.0.0-alpha.1',
      operation: options.operation || fn.name || 'anonymous',
      timestamp: Date.now(),
      duration: endTime - startTime,
      args: JSON.stringify(args),
      result: typeof result === 'object' ? JSON.stringify(result) : String(result)
    };

    return { result, receipt };
  };
}

/**
 * Zod schema validator adapter
 *
 * @param {z.ZodSchema} schema - Zod schema
 * @returns {Function} Validation function
 *
 * @example
 * import { validateSchema } from '@unrdf/v6-compat/adapters';
 *
 * const UserSchema = z.object({ id: z.string(), name: z.string() });
 * const validate = validateSchema(UserSchema);
 *
 * const user = validate({ id: '123', name: 'Alice' }); // OK
 * validate({ id: 123 }); // Throws ZodError
 */
export function validateSchema(schema) {
  if (!(schema instanceof z.ZodType)) {
    throw new Error('validateSchema requires a Zod schema');
  }

  return function validate(data) {
    try {
      return schema.parse(data);
    } catch (error) {
      if (error instanceof z.ZodError) {
        const readable = error.errors.map((e) => {
          return `${e.path.join('.')}: ${e.message}`;
        }).join(', ');

        throw new Error(`Validation failed: ${readable}`);
      }
      throw error;
    }
  };
}

/**
 * Migration status tracker
 */
export class MigrationTracker {
  /**
   * Create a new Migration Tracker
   */
  constructor() {
    this.warnings = [];
    this.start = Date.now();
  }

  /**
   * Track an API migration
   * @param {string} oldAPI - Old API name
   * @param {string} newAPI - New API name
   */
  track(oldAPI, newAPI) {
    this.warnings.push({
      oldAPI,
      newAPI,
      timestamp: Date.now()
    });
  }

  /**
   * Generate migration report
   * @returns {Object} Migration report
   */
  report() {
    const unique = [...new Set(this.warnings.map((w) => w.oldAPI))];
    const elapsed = Date.now() - this.start;

    return {
      totalWarnings: this.warnings.length,
      uniqueAPIs: unique.length,
      elapsed,
      warnings: this.warnings
    };
  }

  /**
   * Print migration summary
   */
  summary() {
    const report = this.report();
    console.log(`
📊 Migration Status Report
━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
Total deprecation warnings: ${report.totalWarnings}
Unique deprecated APIs: ${report.uniqueAPIs}
Elapsed time: ${report.elapsed}ms

Top deprecated APIs:
${Object.entries(
  report.warnings.reduce((acc, w) => {
    acc[w.oldAPI] = (acc[w.oldAPI] || 0) + 1;
    return acc;
  }, {})
)
  .sort((a, b) => b[1] - a[1])
  .slice(0, 5)
  .map(([api, count]) => `  ${count}x ${api}`)
  .join('\n')}

📖 Migration guide: /docs/v6/MIGRATION_PLAN.md
    `);
  }
}

/**
 * Global migration tracker instance
 */
export const migrationTracker = new MigrationTracker();

// Track deprecations globally
if (typeof process?.on === 'function') {
  process.on('deprecation', ({ oldAPI, newAPI }) => {
    migrationTracker.track(oldAPI, newAPI);
  });
}
