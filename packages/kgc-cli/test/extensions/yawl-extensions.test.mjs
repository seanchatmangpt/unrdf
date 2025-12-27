/**
 * @file YAWL Extensions Smoke Tests
 * @description Validates that all YAWL CLI extensions load correctly and have proper structure
 */

import { describe, it } from 'node:test';
import assert from 'node:assert/strict';

const YAWL_EXTENSIONS = [
  { id: '@unrdf/yawl-viz', priority: 32, path: '../../src/extensions/yawl-viz.mjs' },
  { id: '@unrdf/yawl-api', priority: 33, path: '../../src/extensions/yawl-api.mjs' },
  { id: '@unrdf/yawl-queue', priority: 34, path: '../../src/extensions/yawl-queue.mjs' },
  { id: '@unrdf/yawl-durable', priority: 35, path: '../../src/extensions/yawl-durable.mjs' },
  { id: '@unrdf/yawl-langchain', priority: 36, path: '../../src/extensions/yawl-langchain.mjs' },
  { id: '@unrdf/yawl-realtime', priority: 37, path: '../../src/extensions/yawl-realtime.mjs' },
  { id: '@unrdf/yawl-kafka', priority: 38, path: '../../src/extensions/yawl-kafka.mjs' },
  { id: '@unrdf/yawl-ai', priority: 39, path: '../../src/extensions/yawl-ai.mjs' },
];

describe('YAWL CLI Extensions', () => {
  for (const { id, priority, path } of YAWL_EXTENSIONS) {
    describe(id, () => {
      let extension;

      it('should load without errors', async () => {
        const module = await import(path);
        extension = module.default;
        assert.ok(extension, `Extension ${id} should export default object`);
      });

      it('should have correct id', async () => {
        const module = await import(path);
        extension = module.default;
        assert.strictEqual(extension.id, id, `Extension should have id ${id}`);
      });

      it('should have correct priority', async () => {
        const module = await import(path);
        extension = module.default;
        assert.strictEqual(extension.priority, priority, `Extension should have priority ${priority}`);
      });

      it('should have description', async () => {
        const module = await import(path);
        extension = module.default;
        assert.ok(extension.description, 'Extension should have description');
        assert.strictEqual(typeof extension.description, 'string');
      });

      it('should have nouns object', async () => {
        const module = await import(path);
        extension = module.default;
        assert.ok(extension.nouns, 'Extension should have nouns object');
        assert.strictEqual(typeof extension.nouns, 'object');
        assert.ok(Object.keys(extension.nouns).length > 0, 'Extension should have at least one noun');
      });

      it('should have valid verb handlers', async () => {
        const module = await import(path);
        extension = module.default;

        for (const [nounName, noun] of Object.entries(extension.nouns)) {
          assert.ok(noun.verbs, `Noun ${nounName} should have verbs`);

          for (const [verbName, verb] of Object.entries(noun.verbs)) {
            assert.ok(verb.description, `Verb ${verbName} should have description`);
            assert.ok(verb.argsSchema, `Verb ${verbName} should have argsSchema`);
            assert.ok(verb.handler, `Verb ${verbName} should have handler`);
            assert.strictEqual(typeof verb.handler, 'function', `Verb ${verbName} handler should be function`);
          }
        }
      });

      it('should have async handlers that return JSON-serializable results', async () => {
        const module = await import(path);
        extension = module.default;

        for (const noun of Object.values(extension.nouns)) {
          for (const verb of Object.values(noun.verbs)) {
            // Test handler is async
            const result = verb.handler({});
            assert.ok(result instanceof Promise, 'Handler should return Promise');

            // Test result is JSON-serializable
            const resolvedResult = await result;
            const jsonString = JSON.stringify(resolvedResult);
            assert.ok(jsonString, 'Handler result should be JSON-serializable');

            // Test result has success field
            assert.ok('success' in resolvedResult, 'Handler result should have success field');
          }
        }
      });
    });
  }

  it('should have unique priorities across all extensions', async () => {
    const priorities = YAWL_EXTENSIONS.map(e => e.priority);
    const uniquePriorities = new Set(priorities);
    assert.strictEqual(priorities.length, uniquePriorities.size, 'All extensions should have unique priorities');
  });

  it('should have priorities in correct order (32-39)', async () => {
    const priorities = YAWL_EXTENSIONS.map(e => e.priority).sort((a, b) => a - b);
    assert.deepStrictEqual(priorities, [32, 33, 34, 35, 36, 37, 38, 39], 'Priorities should be 32-39');
  });
});
