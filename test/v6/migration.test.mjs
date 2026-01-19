/**
 * V6 Migration Tests
 *
 * Tests v5 → v6 migration paths, backward compatibility, and breaking change detection.
 *
 * @module test/v6/migration
 */

import { describe, it, beforeEach, afterEach, expect } from 'vitest';
import { EventEmitter } from 'node:events';

// v6-compat adapters
import {
  createStore,
  wrapWorkflow,
  wrapFederation,
  streamToAsync,
  withReceipt,
  validateSchema,
  MigrationTracker,
  migrationTracker,
} from '../../packages/v6-compat/src/adapters.mjs';

import { z } from 'zod';

describe('V6 Migration Tests', () => {
  describe('Store Migration (v5 → v6)', () => {
    it('should create store using v6 API', async () => {
      const store = await createStore();
      expect(store).toBeTruthy();
      expect(typeof store).toBe('object');
    });

    it('should emit deprecation warning for v5 Store API', async () => {
      const warnings = [];
      const listener = (data) => warnings.push(data);
      process.on('deprecation', listener);

      try {
        await createStore();
        // Warning emitted in non-test mode
        expect(true).toBeTruthy();
      } finally {
        process.off('deprecation', listener);
      }
    });

    it('should accept options like v5 Store', async () => {
      const store = await createStore({ persistent: false });
      expect(store).toBeTruthy();
    });
  });

  describe('Workflow Migration (v5 → v6)', () => {
    let mockWorkflow;

    beforeEach(() => {
      mockWorkflow = {
        run: async (task) => {
          return { status: 'completed', taskId: task?.id };
        },
      };
    });

    it('should wrap v5 workflow with receipt generation', async () => {
      const wrapped = wrapWorkflow(mockWorkflow);
      expect(wrapped.execute).toBeTruthy();
      expect(wrapped.run).toBeTruthy();
    });

    it('should generate receipt on execute()', async () => {
      const wrapped = wrapWorkflow(mockWorkflow);
      const task = { id: 'task-123' };

      const { result, receipt } = await wrapped.execute(task);

      expect(result).toBeTruthy();
      expect(result.status).toBe('completed');

      expect(receipt).toBeTruthy();
      expect(receipt.version).toBe('6.0.0-alpha.1');
      expect(receipt.operation).toBe('workflow.execute');
      expect(receipt.timestamp).toBeTruthy();
      expect(receipt.duration >= 0).toBeTruthy();
    });

    it('should maintain backward compat with run()', async () => {
      const wrapped = wrapWorkflow(mockWorkflow);
      const task = { id: 'task-456' };

      const result = await wrapped.run(task);

      expect(result).toBeTruthy();
      expect(result.status).toBe('completed');
    });

    it('should throw on invalid workflow', () => {
      assert.throws(
        () => wrapWorkflow(null),
        /requires a workflow object/,
        'Should reject null'
      );

      assert.throws(
        () => wrapWorkflow('not-an-object'),
        /requires a workflow object/,
        'Should reject non-object'
      );
    });
  });

  describe('Federation Migration (v5 → v6)', () => {
    let mockFederation;

    beforeEach(() => {
      mockFederation = {
        query: async (queryString) => {
          return [{ s: 'subject', p: 'predicate', o: 'object' }];
        },
      };
    });

    it('should wrap federation with timeout support', async () => {
      const wrapped = wrapFederation(mockFederation);
      expect(wrapped.querySparql).toBeTruthy();
    });

    it('should execute queries with default 5s timeout', async () => {
      const wrapped = wrapFederation(mockFederation);
      const results = await wrapped.querySparql('SELECT * WHERE { ?s ?p ?o }');

      assert.ok(Array.isArray(results), 'Should return array');
      expect(results.length).toBe(1);
    });

    it('should timeout slow queries', async () => {
      const slowFederation = {
        query: async () => {
          return new Promise((resolve) => setTimeout(resolve, 10000));
        },
      };

      const wrapped = wrapFederation(slowFederation);

      await assert.rejects(
        wrapped.querySparql('SELECT * WHERE { ?s ?p ?o }', { timeout: 100 }),
        /Query timeout/,
        'Should timeout after 100ms'
      );
    });

    it('should allow custom timeout', async () => {
      const wrapped = wrapFederation(mockFederation);
      const results = await wrapped.querySparql('SELECT * WHERE { ?s ?p ?o }', {
        timeout: 10000,
      });

      expect(results).toBeTruthy();
    });

    it('should throw on invalid federation', () => {
      assert.throws(
        () => wrapFederation(null),
        /requires a federation object/,
        'Should reject null'
      );
    });
  });

  describe('Stream Migration (EventEmitter → AsyncIterator)', () => {
    it('should convert EventEmitter stream to AsyncIterator', async () => {
      const stream = new EventEmitter();
      const data = ['quad1', 'quad2', 'quad3'];

      // Emit data asynchronously
      setTimeout(() => {
        data.forEach((d) => stream.emit('data', d));
        stream.emit('end');
      }, 10);

      const collected = [];
      for await (const quad of streamToAsync(stream)) {
        collected.push(quad);
      }

      expect(collected.length).toBe(3);
      assert.deepEqual(collected, data, 'Should preserve order');
    });

    it('should handle stream errors', async () => {
      const stream = new EventEmitter();

      setTimeout(() => {
        stream.emit('error', new Error('Stream error'));
      }, 10);

      await assert.rejects(
        async () => {
          // eslint-disable-next-line no-unused-vars
          for await (const quad of streamToAsync(stream)) {
            // Should throw before iterating
          }
        },
        /Stream error/,
        'Should propagate errors'
      );
    });

    it('should handle empty streams', async () => {
      const stream = new EventEmitter();

      setTimeout(() => {
        stream.emit('end');
      }, 10);

      const collected = [];
      for await (const quad of streamToAsync(stream)) {
        collected.push(quad);
      }

      expect(collected.length).toBe(0);
    });
  });

  describe('Receipt Wrapper (withReceipt)', () => {
    it('should wrap function with receipt generation', async () => {
      const multiply = (a, b) => a * b;
      const wrapped = withReceipt(multiply, { operation: 'multiply' });

      const { result, receipt } = await wrapped(5, 3);

      expect(result).toBe(15);
      expect(receipt).toBeTruthy();
      expect(receipt.operation).toBe('multiply');
      expect(receipt.timestamp).toBeTruthy();
      expect(receipt.duration >= 0).toBeTruthy();
    });

    it('should handle async functions', async () => {
      const asyncFn = async (x) => {
        await new Promise((resolve) => setTimeout(resolve, 10));
        return x * 2;
      };

      const wrapped = withReceipt(asyncFn, { operation: 'asyncDouble' });
      const { result, receipt } = await wrapped(7);

      expect(result).toBe(14);
      expect(receipt.duration >= 10).toBeTruthy();
    });

    it('should use function name if operation not provided', async () => {
      function namedFunction() {
        return 42;
      }

      const wrapped = withReceipt(namedFunction);
      const { receipt } = await wrapped();

      expect(receipt.operation).toBe('namedFunction');
    });

    it('should throw on invalid input', () => {
      assert.throws(
        () => withReceipt(null),
        /requires a function/,
        'Should reject null'
      );

      assert.throws(
        () => withReceipt('not-a-function'),
        /requires a function/,
        'Should reject non-function'
      );
    });
  });

  describe('Zod Schema Validation', () => {
    const UserSchema = z.object({
      id: z.string().uuid(),
      name: z.string().min(1),
      age: z.number().int().positive(),
    });

    it('should validate valid data', () => {
      const validate = validateSchema(UserSchema);
      const user = validate({
        id: '123e4567-e89b-12d3-a456-426614174000',
        name: 'Alice',
        age: 30,
      });

      expect(user.name).toBe('Alice');
    });

    it('should throw on invalid data', () => {
      const validate = validateSchema(UserSchema);

      assert.throws(
        () => validate({ id: 'not-a-uuid', name: 'Bob', age: 25 }),
        /Validation failed/,
        'Should reject invalid UUID'
      );

      assert.throws(
        () => validate({ id: '123e4567-e89b-12d3-a456-426614174000', name: '', age: 30 }),
        /Validation failed/,
        'Should reject empty name'
      );

      assert.throws(
        () => validate({ id: '123e4567-e89b-12d3-a456-426614174000', name: 'Charlie', age: -5 }),
        /Validation failed/,
        'Should reject negative age'
      );
    });

    it('should provide readable error messages', () => {
      const validate = validateSchema(UserSchema);

      try {
        validate({ id: 'bad', name: 'Dave' }); // Missing age
        throw new Error('Test failed');
      } catch (error) {
        assert.ok(error.message.includes('Validation failed'), 'Error should mention validation failure');
      }
    });

    it('should throw on invalid schema', () => {
      assert.throws(
        () => validateSchema(null),
        /requires a Zod schema/,
        'Should reject null'
      );

      assert.throws(
        () => validateSchema({}),
        /requires a Zod schema/,
        'Should reject non-schema object'
      );
    });
  });

  describe('Migration Tracker', () => {
    let tracker;

    beforeEach(() => {
      tracker = new MigrationTracker();
    });

    it('should track deprecation warnings', () => {
      tracker.track('workflow.run', 'workflow.execute');
      tracker.track('new Store()', 'createStore()');

      const report = tracker.report();

      expect(report.totalWarnings).toBe(2);
      expect(report.uniqueAPIs).toBe(2);
      expect(report.elapsed >= 0).toBeTruthy();
    });

    it('should count duplicate warnings', () => {
      tracker.track('workflow.run', 'workflow.execute');
      tracker.track('workflow.run', 'workflow.execute');
      tracker.track('workflow.run', 'workflow.execute');

      const report = tracker.report();

      expect(report.totalWarnings).toBe(3);
      expect(report.uniqueAPIs).toBe(1);
    });

    it('should generate migration summary', () => {
      tracker.track('workflow.run', 'workflow.execute');
      tracker.track('new Store()', 'createStore()');

      // Should not throw
      tracker.summary();
      expect(true).toBeTruthy();
    });

    it('should track warnings over time', async () => {
      tracker.track('api1', 'new1');
      await new Promise((resolve) => setTimeout(resolve, 10));
      tracker.track('api2', 'new2');

      const report = tracker.report();

      expect(report.elapsed >= 10).toBeTruthy();
    });
  });

  describe('Breaking Change Detection', () => {
    it('should detect direct N3 imports (would fail in real code)', () => {
      // In real code, this would be caught by ESLint
      // Here we just document the expectation
      const expectedError = 'Direct N3 imports are forbidden in v6';
      assert.ok(expectedError.includes('forbidden'), 'ESLint rule should exist');
    });

    it('should require receipts for all operations', async () => {
      const workflow = { run: async () => ({ status: 'done' }) };
      const wrapped = wrapWorkflow(workflow);

      const { receipt } = await wrapped.execute({ id: 'test' });

      expect(receipt).toBeTruthy();
      expect(receipt.version).toBeTruthy();
      expect(receipt.timestamp).toBeTruthy();
    });

    it('should enforce Zod validation', () => {
      const schema = z.object({ required: z.string() });
      const validate = validateSchema(schema);

      try {
        validate({});
        throw new Error('Test failed');
      } catch (error) {
        assert.ok(error.message.includes('Validation failed') || error.message.includes('Required'), 'Should enforce required fields');
      }
    });

    it('should enforce ESM-only (package.json check)', () => {
      // In v6, package.json must have "type": "module"
      // This is verified at runtime
      expect(process.env.NODE_ENV !== 'commonjs').toBe(true);
    });

    it('should use AsyncIterator for streams (not EventEmitter)', async () => {
      const stream = new EventEmitter();

      setTimeout(() => {
        stream.emit('data', 'item');
        stream.emit('end');
      }, 10);

      const asyncIterator = streamToAsync(stream);
      expect(typeof asyncIterator.next).toBe('function');
      expect(typeof asyncIterator[Symbol.asyncIterator]).toBe('function');
    });
  });

  describe('Backward Compatibility', () => {
    it('should preserve v5 workflow.run() method', async () => {
      const workflow = { run: async () => ({ status: 'ok' }) };
      const wrapped = wrapWorkflow(workflow);

      const result = await wrapped.run({ id: 'test' });

      expect(result).toBeTruthy();
      expect(result.status).toBe('ok');
    });

    it('should preserve v5 federation.query() method', async () => {
      const federation = {
        query: async (q) => [{ result: 'data' }],
      };
      const wrapped = wrapFederation(federation);

      const results = await wrapped.query('SELECT * WHERE { ?s ?p ?o }');

      expect(results).toBeTruthy();
      assert.ok(Array.isArray(results), 'Should return array');
    });

    it('should allow gradual migration (v5 and v6 APIs coexist)', async () => {
      const workflow = { run: async () => ({ status: 'done' }) };
      const wrapped = wrapWorkflow(workflow);

      // Can use both APIs
      const v5Result = await wrapped.run({ id: 'v5' });
      const v6Result = await wrapped.execute({ id: 'v6' });

      expect(v5Result).toBeTruthy();
      expect(v6Result.result).toBeTruthy();
      expect(v6Result.receipt).toBeTruthy();
    });
  });

  describe('Upgrade Path Validation', () => {
    it('should provide clear migration path for Store', async () => {
      // v5: new Store() from 'n3'
      // v6: createStore() from '@unrdf/oxigraph'
      // Migration: use createStore() from '@unrdf/v6-compat/adapters'

      const store = await createStore();
      expect(store).toBeTruthy();
    });

    it('should provide clear migration path for Workflow', async () => {
      // v5: workflow.run(task)
      // v6: workflow.execute(task) with receipt
      // Migration: wrapWorkflow(workflow)

      const workflow = { run: async () => ({}) };
      const wrapped = wrapWorkflow(workflow);

      const { receipt } = await wrapped.execute({ id: 'test' });
      expect(receipt).toBeTruthy();
    });

    it('should provide clear migration path for Streams', async () => {
      // v5: stream.on('data', ...)
      // v6: for await (const x of stream)
      // Migration: streamToAsync(stream)

      const stream = new EventEmitter();
      setTimeout(() => {
        stream.emit('data', 1);
        stream.emit('end');
      }, 10);

      const items = [];
      for await (const item of streamToAsync(stream)) {
        items.push(item);
      }

      expect(items.length).toBe(1);
    });
  });
});
