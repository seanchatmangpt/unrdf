/**
 * @file Enhanced Test Helpers for UNRDF
 * @module test-utils/helpers
 * @description
 * Core testing utilities focused on reducing test writing time and improving
 * feedback speed. Following 80/20 principle - common patterns that handle
 * 80% of test scenarios.
 */

import { createStore, dataFactory } from '@unrdf/oxigraph';
import { trace, context, SpanStatusCode } from '@opentelemetry/api';

/**
 * Create a preconfigured test store with sample data
 * @param {Object} [options] - Store configuration
 * @param {Array} [options.quads] - Initial quads to add
 * @param {boolean} [options.enableMetrics=false] - Enable performance metrics
 * @returns {Object} Configured store with helper methods
 *
 * @example
 * const store = createTestStore({
 *   quads: [
 *     createQuad('http://example.org/subject', 'http://example.org/predicate', 'http://example.org/object')
 *   ]
 * });
 */
export function createTestStore(options = {}) {
  const store = createStore();
  const metrics = options.enableMetrics ? {
    addCount: 0,
    deleteCount: 0,
    matchCount: 0,
  } : null;

  // Add initial quads if provided
  if (options.quads && Array.isArray(options.quads)) {
    for (const quad of options.quads) {
      store.add(quad);
    }
  }

  // Wrap store methods to track metrics if enabled
  if (metrics) {
    const originalAdd = store.add.bind(store);
    const originalDelete = store.delete.bind(store);
    const originalMatch = store.match.bind(store);

    store.add = (quad) => {
      metrics.addCount++;
      return originalAdd(quad);
    };

    store.delete = (quad) => {
      metrics.deleteCount++;
      return originalDelete(quad);
    };

    store.match = (...args) => {
      metrics.matchCount++;
      return originalMatch(...args);
    };

    store.getMetrics = () => ({ ...metrics });
    store.resetMetrics = () => {
      metrics.addCount = 0;
      metrics.deleteCount = 0;
      metrics.matchCount = 0;
    };
  }

  return store;
}

/**
 * Create a sample workflow for testing
 * @param {Object} [options] - Workflow configuration
 * @param {string} [options.id='test-workflow'] - Workflow ID
 * @param {string} [options.name='Test Workflow'] - Workflow name
 * @param {Array} [options.tasks] - Custom tasks
 * @param {Array} [options.flows] - Custom flows
 * @returns {Object} Workflow specification
 *
 * @example
 * const workflow = createTestWorkflow({
 *   id: 'my-test-workflow',
 *   tasks: [
 *     { id: 'task1', type: 'atomic', name: 'First Task' }
 *   ]
 * });
 */
export function createTestWorkflow(options = {}) {
  const defaultTasks = [
    {
      id: 'start',
      type: 'atomic',
      name: 'Start Task',
      description: 'Initial workflow task',
    },
    {
      id: 'process',
      type: 'atomic',
      name: 'Process Task',
      description: 'Main processing task',
    },
    {
      id: 'complete',
      type: 'atomic',
      name: 'Complete Task',
      description: 'Final workflow task',
    },
  ];

  const defaultFlows = [
    { from: 'start', to: 'process' },
    { from: 'process', to: 'complete' },
  ];

  return {
    id: options.id || 'test-workflow',
    name: options.name || 'Test Workflow',
    description: options.description || 'Sample workflow for testing',
    version: options.version || '1.0.0',
    tasks: options.tasks || defaultTasks,
    flows: options.flows || defaultFlows,
    metadata: options.metadata || {},
  };
}

/**
 * Mock OTEL tracer for faster tests (no actual spans created)
 * @param {Object} [options] - Mock configuration
 * @param {boolean} [options.capture=true] - Capture span calls for inspection
 * @returns {Object} Mock tracer with inspection methods
 *
 * @example
 * const otel = mockOTEL({ capture: true });
 * const tracer = otel.getTracer('test');
 * // ... use tracer ...
 * expect(otel.getSpans()).toHaveLength(5);
 */
export function mockOTEL(options = {}) {
  const capturedSpans = options.capture ? [] : null;
  const capturedAttributes = options.capture ? [] : null;

  const mockSpan = {
    setAttribute(key, value) {
      if (capturedAttributes) {
        capturedAttributes.push({ key, value });
      }
      return this;
    },
    setAttributes(attributes) {
      if (capturedAttributes) {
        Object.entries(attributes).forEach(([key, value]) => {
          capturedAttributes.push({ key, value });
        });
      }
      return this;
    },
    setStatus(status) {
      this._status = status;
      return this;
    },
    recordException(exception) {
      this._exception = exception;
      return this;
    },
    end() {
      this._ended = true;
    },
    isRecording() {
      return !this._ended;
    },
  };

  const mockTracer = {
    startSpan(name, options) {
      if (capturedSpans) {
        capturedSpans.push({ name, options, span: mockSpan });
      }
      return mockSpan;
    },
    startActiveSpan(name, options, fn) {
      if (typeof options === 'function') {
        fn = options;
        options = {};
      }
      const span = this.startSpan(name, options);
      try {
        return fn(span);
      } finally {
        span.end();
      }
    },
  };

  return {
    getTracer: () => mockTracer,
    getSpans: () => (capturedSpans ? [...capturedSpans] : []),
    getAttributes: () => (capturedAttributes ? [...capturedAttributes] : []),
    reset: () => {
      if (capturedSpans) capturedSpans.length = 0;
      if (capturedAttributes) capturedAttributes.length = 0;
    },
  };
}

/**
 * Wait for a condition to be true with timeout
 * @param {Function} condition - Function that returns true when condition is met
 * @param {Object} [options] - Wait configuration
 * @param {number} [options.timeout=5000] - Maximum wait time in ms
 * @param {number} [options.interval=100] - Polling interval in ms
 * @param {string} [options.message] - Custom error message
 * @returns {Promise<boolean>} Resolves when condition is true
 * @throws {Error} If timeout is reached
 *
 * @example
 * await waitForCondition(
 *   () => store.size > 0,
 *   { timeout: 1000, message: 'Store should have quads' }
 * );
 */
export async function waitForCondition(condition, options = {}) {
  const timeout = options.timeout || 5000;
  const interval = options.interval || 100;
  const message = options.message || 'Condition not met within timeout';

  const startTime = Date.now();

  while (Date.now() - startTime < timeout) {
    try {
      const result = await condition();
      if (result) {
        return true;
      }
    } catch (error) {
      // Condition threw error, keep waiting
    }

    await new Promise(resolve => setTimeout(resolve, interval));
  }

  throw new Error(`${message} (timeout: ${timeout}ms)`);
}

/**
 * Create a simple quad using dataFactory
 * @param {string} subject - Subject URI
 * @param {string} predicate - Predicate URI
 * @param {string} object - Object URI or literal value
 * @param {Object} [options] - Quad options
 * @param {string} [options.graph] - Named graph URI
 * @param {boolean} [options.literal=false] - Treat object as literal
 * @returns {Object} RDF Quad
 */
export function createQuad(subject, predicate, object, options = {}) {
  const s = dataFactory.namedNode(subject);
  const p = dataFactory.namedNode(predicate);
  const o = options.literal
    ? dataFactory.literal(object)
    : dataFactory.namedNode(object);
  const g = options.graph
    ? dataFactory.namedNode(options.graph)
    : dataFactory.defaultGraph();

  return dataFactory.quad(s, p, o, g);
}

/**
 * Measure test execution time
 * @param {Function} fn - Function to measure
 * @returns {Promise<Object>} Result with duration in ms
 *
 * @example
 * const { result, duration } = await measureTime(async () => {
 *   return await someExpensiveOperation();
 * });
 * expect(duration).toBeLessThan(100); // Should complete in <100ms
 */
export async function measureTime(fn) {
  const start = performance.now();
  const result = await fn();
  const duration = performance.now() - start;
  return { result, duration };
}

/**
 * Create a test batch for multiple operations
 * Useful for testing concurrent operations
 * @param {Array<Function>} operations - Array of async functions
 * @param {Object} [options] - Batch options
 * @param {boolean} [options.parallel=true] - Run in parallel
 * @param {number} [options.timeout=5000] - Timeout per operation
 * @returns {Promise<Array>} Results array
 *
 * @example
 * const results = await testBatch([
 *   () => store.add(quad1),
 *   () => store.add(quad2),
 *   () => store.add(quad3),
 * ], { parallel: true });
 */
export async function testBatch(operations, options = {}) {
  const parallel = options.parallel !== false;
  const timeout = options.timeout || 5000;

  const wrappedOps = operations.map(op => {
    return Promise.race([
      op(),
      new Promise((_, reject) =>
        setTimeout(() => reject(new Error('Operation timeout')), timeout)
      ),
    ]);
  });

  if (parallel) {
    return Promise.all(wrappedOps);
  } else {
    const results = [];
    for (const op of wrappedOps) {
      results.push(await op);
    }
    return results;
  }
}

/**
 * Snapshot store state for comparison
 * @param {Object} store - RDF store
 * @returns {Object} Store snapshot
 */
export function snapshotStore(store) {
  return {
    size: store.size,
    quads: Array.from(store.match()),
    timestamp: Date.now(),
  };
}

/**
 * Assert two store snapshots are equal
 * @param {Object} snapshot1 - First snapshot
 * @param {Object} snapshot2 - Second snapshot
 * @throws {Error} If snapshots differ
 */
export function assertSnapshotsEqual(snapshot1, snapshot2) {
  if (snapshot1.size !== snapshot2.size) {
    throw new Error(
      `Store sizes differ: ${snapshot1.size} vs ${snapshot2.size}`
    );
  }

  const quads1 = new Set(snapshot1.quads.map(q => q.toString()));
  const quads2 = new Set(snapshot2.quads.map(q => q.toString()));

  for (const quad of quads1) {
    if (!quads2.has(quad)) {
      throw new Error(`Quad in snapshot1 but not snapshot2: ${quad}`);
    }
  }

  for (const quad of quads2) {
    if (!quads1.has(quad)) {
      throw new Error(`Quad in snapshot2 but not snapshot1: ${quad}`);
    }
  }
}
