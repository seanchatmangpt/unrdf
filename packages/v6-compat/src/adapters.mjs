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
import {
  createStoreParamsSchema,
  wrapWorkflowParamsSchema,
  wrapFederationParamsSchema,
  withReceiptParamsSchema,
  validateSchemaParamsSchema,
  streamToAsyncParamsSchema,
} from './adapters.schema.mjs';

/**
 * Emit deprecation warning
 *
 * @param {string} oldAPI - Deprecated API name
 * @param {string} newAPI - Replacement API name
 * @param {string} [hint] - Migration hint
 * @param {Object} [context] - Context with temporal sources (determinism)
 */
function deprecationWarning(oldAPI, newAPI, hint = '', context = {}) {
  const {
    t_ns = BigInt(Date.now()) * 1_000_000n
  } = context;

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
      timestamp: Number(t_ns / 1_000_000n),
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
  const [validOptions] = createStoreParamsSchema.parse([options]);

  deprecationWarning(
    'new Store() from n3',
    'createStore() from @unrdf/oxigraph',
    'Oxigraph provides 10x faster SPARQL execution'
  );

  return createStoreV6(validOptions);
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
  const [validWorkflow] = wrapWorkflowParamsSchema.parse([workflow]);

  if (!validWorkflow || typeof validWorkflow !== 'object') {
    throw new Error('wrapWorkflow requires a workflow object');
  }

  const wrapped = Object.create(validWorkflow);

  // Wrap run() → execute() with receipt
  if (validWorkflow.run) {
    wrapped.execute = async function execute(task, options = {}) {
      const {
        context = {},
        startTime = performance.now(),
        endTime
      } = options;

      const {
        t_ns = BigInt(Date.now()) * 1_000_000n
      } = context;

      deprecationWarning(
        'workflow.run(task)',
        'workflow.execute(task) with receipt',
        'Receipts enable replay and verification',
        context
      );

      const result = await validWorkflow.run(task);
      const actualEndTime = endTime ?? performance.now();
      const timestamp = Number(t_ns / 1_000_000n);

      // Generate receipt
      const receipt = {
        version: '6.0.0-alpha.1',
        operation: 'workflow.execute',
        task: task?.id || 'unknown',
        timestamp,
        duration: actualEndTime - startTime,
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
      return validWorkflow.run(task);
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
  const [validFederation] = wrapFederationParamsSchema.parse([federation]);

  if (!validFederation || typeof validFederation !== 'object') {
    throw new Error('wrapFederation requires a federation object');
  }

  const wrapped = Object.create(validFederation);

  if (validFederation.query) {
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

      const queryPromise = validFederation.query(queryString, options);

      return Promise.race([queryPromise, timeoutPromise]);
    };

    // Keep original for compat
    wrapped.query = validFederation.query;
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
  const [validStream] = streamToAsyncParamsSchema.parse([stream]);

  deprecationWarning(
    'stream.on("data", ...)',
    'for await (const x of stream)',
    'Async iterators provide backpressure handling'
  );

  const queue = [];
  let done = false;
  let error = null;

  validStream.on('data', (data) => queue.push(data));
  validStream.on('end', () => { done = true; });
  validStream.on('error', (err) => { error = err; done = true; });

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
  const [validFn, validOptions] = withReceiptParamsSchema.parse([fn, options]);

  if (typeof validFn !== 'function') {
    throw new Error('withReceipt requires a function');
  }

  return async function wrappedWithReceipt(...args) {
    const context = validOptions.context || {};
    const {
      t_ns = BigInt(Date.now()) * 1_000_000n
    } = context;

    const startTime = validOptions.startTime ?? performance.now();
    const result = await validFn(...args);
    const endTime = validOptions.endTime ?? performance.now();
    const timestamp = Number(t_ns / 1_000_000n);

    const receipt = {
      version: '6.0.0-alpha.1',
      operation: validOptions.operation || validFn.name || 'anonymous',
      timestamp,
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
  const [validSchema] = validateSchemaParamsSchema.parse([schema]);

  if (!(validSchema instanceof z.ZodType)) {
    throw new Error('validateSchema requires a Zod schema');
  }

  return function validate(data) {
    try {
      return validSchema.parse(data);
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
 *
 * **P0-003 Enhancement**: Static analysis counters
 */
export class MigrationTracker {
  constructor(options = {}) {
    const context = options.context || {};
    const {
      t_ns = BigInt(Date.now()) * 1_000_000n
    } = context;

    this.warnings = [];
    this.start = options.startTime ?? Number(t_ns / 1_000_000n);
    this.getNow = options.getNow ?? (() => Number(BigInt(Date.now()) * 1_000_000n / 1_000_000n));
    this.context = context;
    this.staticAnalysis = {
      n3Imports: 0,
      dateNowCalls: 0,
      mathRandomCalls: 0,
      workflowRunCalls: 0,
      filesScanned: 0,
    };
  }

  track(oldAPI, newAPI, timestamp) {
    this.warnings.push({
      oldAPI,
      newAPI,
      timestamp: timestamp ?? this.getNow()
    });
  }

  /**
   * Analyze source file for migration issues
   *
   * **P0-003**: Static analysis for N3 imports, Date.now(), Math.random()
   *
   * @param {string} source - Source code
   * @param {string} [filePath] - File path for reporting
   * @returns {Object} Analysis result
   */
  analyzeSource(source, filePath = 'unknown') {
    this.staticAnalysis.filesScanned++;

    const issues = {
      file: filePath,
      n3Imports: 0,
      dateNowCalls: 0,
      mathRandomCalls: 0,
      workflowRunCalls: 0,
    };

    // Count N3 imports
    const n3ImportMatches = source.match(/from\s+['"]n3['"]/g);
    if (n3ImportMatches) {
      issues.n3Imports = n3ImportMatches.length;
      this.staticAnalysis.n3Imports += n3ImportMatches.length;
    }

    // Count Date.now() calls
    const dateNowMatches = source.match(/Date\.now\(\)/g);
    if (dateNowMatches) {
      issues.dateNowCalls = dateNowMatches.length;
      this.staticAnalysis.dateNowCalls += dateNowMatches.length;
    }

    // Count Math.random() calls
    const mathRandomMatches = source.match(/Math\.random\(\)/g);
    if (mathRandomMatches) {
      issues.mathRandomCalls = mathRandomMatches.length;
      this.staticAnalysis.mathRandomCalls += mathRandomMatches.length;
    }

    // Count workflow.run() calls
    const workflowRunMatches = source.match(/workflow\.run\(/g);
    if (workflowRunMatches) {
      issues.workflowRunCalls = workflowRunMatches.length;
      this.staticAnalysis.workflowRunCalls += workflowRunMatches.length;
    }

    return issues;
  }

  /**
   * Scan directory for migration issues
   *
   * @param {string} pattern - Glob pattern
   * @returns {Promise<Array<Object>>} Issues found
   */
  async scanDirectory(pattern) {
    const { glob } = await import('glob');
    const { readFile } = await import('fs/promises');

    const files = await glob(pattern, { absolute: true });
    const results = [];

    for (const file of files) {
      try {
        const source = await readFile(file, 'utf-8');
        const issues = this.analyzeSource(source, file);

        if (
          issues.n3Imports > 0 ||
          issues.dateNowCalls > 0 ||
          issues.mathRandomCalls > 0 ||
          issues.workflowRunCalls > 0
        ) {
          results.push(issues);
        }
      } catch (error) {
        console.warn(`Failed to scan ${file}:`, error.message);
      }
    }

    return results;
  }

  report() {
    const unique = [...new Set(this.warnings.map((w) => w.oldAPI))];
    const elapsed = this.getNow() - this.start;

    return {
      totalWarnings: this.warnings.length,
      uniqueAPIs: unique.length,
      elapsed,
      warnings: this.warnings,
      staticAnalysis: this.staticAnalysis,
    };
  }

  summary() {
    const report = this.report();
    console.log(`
📊 Migration Status Report
━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
Total deprecation warnings: ${report.totalWarnings}
Unique deprecated APIs: ${report.uniqueAPIs}
Elapsed time: ${report.elapsed}ms

Static Analysis (P0-003):
━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
Files scanned: ${report.staticAnalysis.filesScanned}
N3 imports: ${report.staticAnalysis.n3Imports}
Date.now() calls: ${report.staticAnalysis.dateNowCalls}
Math.random() calls: ${report.staticAnalysis.mathRandomCalls}
workflow.run() calls: ${report.staticAnalysis.workflowRunCalls}

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
