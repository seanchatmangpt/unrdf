/**
 * @file YAWL Extensions Smoke Tests
 * @description Validates that all YAWL CLI extensions load correctly and have proper structure
 */

import { describe, it, expect } from 'vitest';

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
        expect(extension).toBeDefined();
      });

      it('should have correct id', async () => {
        const module = await import(path);
        extension = module.default;
        expect(extension.id).toBe(id);
      });

      it('should have correct priority', async () => {
        const module = await import(path);
        extension = module.default;
        expect(extension.priority).toBe(priority);
      });

      it('should have description', async () => {
        const module = await import(path);
        extension = module.default;
        expect(extension.description).toBeDefined();
        expect(typeof extension.description).toBe('string');
      });

      it('should have nouns object', async () => {
        const module = await import(path);
        extension = module.default;
        expect(extension.nouns).toBeDefined();
        expect(typeof extension.nouns).toBe('object');
        expect(Object.keys(extension.nouns).length).toBeGreaterThan(0);
      });

      it('should have valid verb handlers', async () => {
        const module = await import(path);
        extension = module.default;

        for (const [nounName, noun] of Object.entries(extension.nouns)) {
          expect(noun.verbs).toBeDefined();

          for (const [verbName, verb] of Object.entries(noun.verbs)) {
            expect(verb.description).toBeDefined();
            expect(verb.argsSchema).toBeDefined();
            expect(verb.handler).toBeDefined();
            expect(typeof verb.handler).toBe('function');
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
            expect(result).toBeInstanceOf(Promise);

            // Test result is JSON-serializable
            const resolvedResult = await result;
            const jsonString = JSON.stringify(resolvedResult);
            expect(jsonString).toBeDefined();

            // Test result has success field
            expect('success' in resolvedResult).toBe(true);
          }
        }
      });
    });
  }

  it('should have unique priorities across all extensions', async () => {
    const priorities = YAWL_EXTENSIONS.map(e => e.priority);
    const uniquePriorities = new Set(priorities);
    expect(priorities.length).toBe(uniquePriorities.size);
  });

  it('should have priorities in correct order (32-39)', async () => {
    const priorities = YAWL_EXTENSIONS.map(e => e.priority).sort((a, b) => a - b);
    expect(priorities).toEqual([32, 33, 34, 35, 36, 37, 38, 39]);
  });
});
