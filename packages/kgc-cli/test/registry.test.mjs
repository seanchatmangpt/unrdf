/**
 * @fileoverview Registry contract tests.
 *
 * Validates:
 * - Extension contract (Zod schema compliance)
 * - Collision detection
 * - Deterministic command tree generation
 * - Override rule application
 */

import { describe, it, expect, beforeEach } from 'vitest';
import { Registry, ExtensionSchema } from '../src/lib/registry.mjs';

describe('Registry', () => {
  let registry;

  beforeEach(() => {
    registry = new Registry();
  });

  describe('Extension registration', () => {
    it('should register valid extensions', () => {
      const ext = {
        id: '@test/ext1',
        nouns: {
          test: {
            verbs: {
              run: {
                description: 'Test command',
                handler: async () => ({ ok: true })
              }
            }
          }
        }
      };

      registry.registerExtension(ext);
      expect(registry.extensions.has('@test/ext1')).toBe(true);
    });

    it('should reject invalid extensions', () => {
      const invalid = {
        id: '@test/bad',
        nouns: {
          test: {
            verbs: {
              run: {
                description: 'Missing handler',
                // handler missing
              }
            }
          }
        }
      };

      expect(() => {
        registry.registerExtension(invalid);
      }).toThrow();
    });

    it('should validate extension contract', () => {
      const ext = {
        id: '@test/ext1',
        nouns: {
          noun1: {
            description: 'Test noun',
            verbs: {
              verb1: {
                description: 'Test verb',
                handler: async () => ({}),
                argsSchema: undefined
              }
            }
          }
        }
      };

      const validation = ExtensionSchema.safeParse(ext);
      expect(validation.success).toBe(true);
    });
  });

  describe('Collision detection', () => {
    it('should detect noun:verb collisions', () => {
      const ext1 = {
        id: '@test/ext1',
        priority: 10,
        nouns: {
          test: {
            verbs: {
              run: {
                description: 'First handler',
                handler: async () => ({ from: 'ext1' })
              }
            }
          }
        }
      };

      const ext2 = {
        id: '@test/ext2',
        priority: 10,
        nouns: {
          test: {
            verbs: {
              run: {
                description: 'Second handler',
                handler: async () => ({ from: 'ext2' })
              }
            }
          }
        }
      };

      registry.registerExtension(ext1, 10);
      expect(() => {
        registry.registerExtension(ext2, 11);
      }).toThrow(/Collision/);
    });

    it('should respect load order for collision resolution', () => {
      const registry2 = new Registry({ failOnCollision: false });

      const ext1 = {
        id: '@test/ext1',
        nouns: {
          test: {
            verbs: {
              run: {
                description: 'First',
                handler: async () => ({ from: 'ext1' })
              }
            }
          }
        }
      };

      const ext2 = {
        id: '@test/ext2',
        nouns: {
          test: {
            verbs: {
              run: {
                description: 'Second',
                handler: async () => ({ from: 'ext2' })
              }
            }
          }
        }
      };

      registry2.registerExtension(ext1, 10); // Lower = earlier
      registry2.registerExtension(ext2, 20);

      // Earlier registration (ext1) should win
      expect(registry2.getCommandSource('test', 'run')).toBe('@test/ext1');
    });
  });

  describe('Command tree building', () => {
    it('should build command tree from registered extensions', () => {
      const ext = {
        id: '@test/ext1',
        nouns: {
          noun1: {
            description: 'Test noun 1',
            verbs: {
              verb1: {
                description: 'Test verb 1',
                handler: async () => ({})
              }
            }
          }
        }
      };

      registry.registerExtension(ext, 10);
      const tree = registry.buildCommandTree();

      expect(tree.nouns).toBeDefined();
      expect(tree.nouns.noun1).toBeDefined();
      expect(tree.nouns.noun1.verbs.verb1).toBeDefined();
      expect(tree.nouns.noun1.verbs.verb1._source).toBe('@test/ext1');
    });

    it('should merge multiple extensions into single tree', () => {
      const ext1 = {
        id: '@test/ext1',
        nouns: {
          noun1: {
            verbs: {
              verb1: {
                description: 'V1',
                handler: async () => ({})
              }
            }
          }
        }
      };

      const ext2 = {
        id: '@test/ext2',
        nouns: {
          noun2: {
            verbs: {
              verb2: {
                description: 'V2',
                handler: async () => ({})
              }
            }
          }
        }
      };

      registry.registerExtension(ext1, 10);
      registry.registerExtension(ext2, 20);

      const tree = registry.buildCommandTree();
      expect(Object.keys(tree.nouns).length).toBe(2);
      expect(tree.nouns.noun1).toBeDefined();
      expect(tree.nouns.noun2).toBeDefined();
    });

    it('should maintain deterministic ordering', () => {
      const exts = Array.from({ length: 5 }, (_, i) => ({
        id: `@test/ext${i}`,
        nouns: {
          [`noun${i}`]: {
            verbs: {
              [`verb${i}`]: {
                description: `Verb ${i}`,
                handler: async () => ({})
              }
            }
          }
        }
      }));

      exts.forEach((ext, i) => {
        registry.registerExtension(ext, i);
      });

      const commands1 = registry.listCommands();
      const commands2 = registry.listCommands();

      // Same order both times
      expect(commands1).toEqual(commands2);
    });
  });

  describe('Contract validation', () => {
    it('should validate all commands have handlers', () => {
      const badExt = {
        id: '@test/bad',
        nouns: {
          test: {
            verbs: {
              run: {
                description: 'Missing handler',
                handler: async () => ({})
              },
              fail: {
                description: 'Also missing handler',
                // handler missing
              }
            }
          }
        }
      };

      // This should fail at registration
      expect(() => {
        registry.registerExtension(badExt);
      }).toThrow();
    });

    it('should detect missing descriptions', () => {
      const ext = {
        id: '@test/ext',
        nouns: {
          test: {
            verbs: {
              run: {
                // description missing
                handler: async () => ({})
              }
            }
          }
        }
      };

      expect(() => {
        registry.registerExtension(ext);
      }).toThrow();
    });

    it('should validate Zod schemas when present', async () => {
      const { z } = await import('zod');

      const ext = {
        id: '@test/ext',
        nouns: {
          test: {
            verbs: {
              run: {
                description: 'Test',
                handler: async () => ({}),
                argsSchema: z.object({ name: z.string() })
              }
            }
          }
        }
      };

      registry.registerExtension(ext);
      const errors = registry.validateContracts();
      expect(errors.filter(e => e.source === '@test/ext')).toHaveLength(0);
    });
  });

  describe('listCommands', () => {
    it('should list all registered commands in sorted order', () => {
      const ext = {
        id: '@test/ext',
        nouns: {
          zzz: {
            verbs: {
              aaa: { description: 'A', handler: async () => ({}) },
              bbb: { description: 'B', handler: async () => ({}) }
            }
          },
          aaa: {
            verbs: {
              zzz: { description: 'Z', handler: async () => ({}) }
            }
          }
        }
      };

      registry.registerExtension(ext, 10);
      const commands = registry.listCommands();

      // Should be sorted
      expect(commands).toEqual([
        'aaa:zzz',
        'zzz:aaa',
        'zzz:bbb'
      ]);
    });
  });

  describe('getCommandSource', () => {
    it('should return extension that provides command', () => {
      const ext = {
        id: '@test/ext',
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
      expect(registry.getCommandSource('noun', 'verb')).toBe('@test/ext');
      expect(registry.getCommandSource('noun', 'missing')).toBeUndefined();
    });
  });
});
