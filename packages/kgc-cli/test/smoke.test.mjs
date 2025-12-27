/**
 * @fileoverview Smoke tests - end-to-end command execution.
 *
 * Validates:
 * - Commands can be executed
 * - JSON envelope format is correct
 * - Handlers return proper results
 */

import { describe, it, expect } from 'vitest';
import { Registry, createEnvelope } from '../src/lib/registry.mjs';
import { z } from 'zod';

describe('Smoke Tests', () => {
  describe('JSON envelope', () => {
    it('should create success envelope', () => {
      const envelope = createEnvelope(true, { message: 'Hello' }, { source: 'test' });

      expect(envelope.ok).toBe(true);
      expect(envelope.data).toEqual({ message: 'Hello' });
      expect(envelope.meta.source).toBe('test');
      expect(envelope.meta.timestamp).toBeDefined();
    });

    it('should create error envelope', () => {
      const envelope = createEnvelope(false, {
        code: 'TEST_ERROR',
        message: 'Test error message'
      });

      expect(envelope.ok).toBe(false);
      expect(envelope.code).toBe('TEST_ERROR');
      expect(envelope.message).toBe('Test error message');
      expect(envelope.meta.timestamp).toBeDefined();
    });

    it('should include optional fields in error envelope', () => {
      const envelope = createEnvelope(false, {
        code: 'VALIDATION_ERROR',
        message: 'Bad input',
        details: { field: 'age' },
        hint: 'Age must be positive'
      });

      expect(envelope.details).toEqual({ field: 'age' });
      expect(envelope.hint).toBe('Age must be positive');
    });
  });

  describe('Simple command execution', () => {
    let registry;

    beforeEach(() => {
      registry = new Registry();
    });

    it('should execute basic handler', async () => {
      const ext = {
        id: '@test/smoke',
        nouns: {
          test: {
            verbs: {
              echo: {
                description: 'Echo test command',
                handler: async (args) => {
                  return { echo: args.message || 'Hello' };
                },
                argsSchema: z.object({
                  message: z.string().optional()
                })
              }
            }
          }
        }
      };

      registry.registerExtension(ext, 10);
      const tree = registry.buildCommandTree();

      const handler = tree.nouns.test.verbs.echo.handler;
      const result = await handler({ message: 'World' });

      expect(result.echo).toBe('World');
    });

    it('should validate args against schema', async () => {
      const ext = {
        id: '@test/validate',
        nouns: {
          math: {
            verbs: {
              add: {
                description: 'Add two numbers',
                handler: async (args) => {
                  return { result: args.a + args.b };
                },
                argsSchema: z.object({
                  a: z.number(),
                  b: z.number()
                })
              }
            }
          }
        }
      };

      registry.registerExtension(ext, 10);
      const tree = registry.buildCommandTree();

      const schema = tree.nouns.math.verbs.add.argsSchema;
      expect(() => {
        schema.parse({ a: 1, b: 2 });
      }).not.toThrow();

      expect(() => {
        schema.parse({ a: 'one', b: 2 });
      }).toThrow();
    });

    it('should pass context to handler', async () => {
      const ext = {
        id: '@test/context',
        nouns: {
          info: {
            verbs: {
              get: {
                description: 'Get context info',
                handler: async (args, ctx) => {
                  return { contextProvided: !!ctx };
                }
              }
            }
          }
        }
      };

      registry.registerExtension(ext, 10);
      const tree = registry.buildCommandTree();

      const handler = tree.nouns.info.verbs.get.handler;
      const result = await handler({}, { test: true });

      expect(result.contextProvided).toBe(true);
    });
  });

  describe('Multi-extension scenarios', () => {
    let registry;

    beforeEach(() => {
      registry = new Registry({ failOnCollision: false });
    });

    it('should execute commands from different extensions', async () => {
      const ext1 = {
        id: '@test/ext1',
        nouns: {
          cat: {
            verbs: {
              meow: {
                description: 'Meow',
                handler: async () => ({ sound: 'meow' })
              }
            }
          }
        }
      };

      const ext2 = {
        id: '@test/ext2',
        nouns: {
          dog: {
            verbs: {
              bark: {
                description: 'Bark',
                handler: async () => ({ sound: 'bark' })
              }
            }
          }
        }
      };

      registry.registerExtension(ext1, 10);
      registry.registerExtension(ext2, 20);

      const tree = registry.buildCommandTree();

      const meow = await tree.nouns.cat.verbs.meow.handler({});
      const bark = await tree.nouns.dog.verbs.bark.handler({});

      expect(meow.sound).toBe('meow');
      expect(bark.sound).toBe('bark');
    });

    it('should track command sources', async () => {
      const ext = {
        id: '@test/source',
        nouns: {
          noun: {
            verbs: {
              verb: {
                description: 'Test',
                handler: async () => ({})
              }
            }
          }
        }
      };

      registry.registerExtension(ext, 10);
      const tree = registry.buildCommandTree();

      expect(tree.nouns.noun.verbs.verb._source).toBe('@test/source');
    });
  });

  describe('Error handling', () => {
    it('should handle handler errors gracefully', async () => {
      const ext = {
        id: '@test/error',
        nouns: {
          fail: {
            verbs: {
              now: {
                description: 'Always fails',
                handler: async () => {
                  throw new Error('Intentional failure');
                }
              }
            }
          }
        }
      };

      const registry = new Registry();
      registry.registerExtension(ext, 10);
      const tree = registry.buildCommandTree();

      const handler = tree.nouns.fail.verbs.now.handler;

      await expect(handler({})).rejects.toThrow('Intentional failure');
    });

    it('should handle async handlers', async () => {
      const ext = {
        id: '@test/async',
        nouns: {
          async: {
            verbs: {
              wait: {
                description: 'Wait a bit',
                handler: async (args) => {
                  const delay = args.ms || 1;
                  return new Promise(resolve => {
                    setTimeout(() => resolve({ waited: true }), delay);
                  });
                },
                argsSchema: z.object({
                  ms: z.number().optional()
                })
              }
            }
          }
        }
      };

      const registry = new Registry();
      registry.registerExtension(ext, 10);
      const tree = registry.buildCommandTree();

      const handler = tree.nouns.async.verbs.wait.handler;
      const result = await handler({ ms: 10 });

      expect(result.waited).toBe(true);
    });
  });
});
