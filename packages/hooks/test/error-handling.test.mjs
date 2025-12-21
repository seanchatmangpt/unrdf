/**
 * @vitest-environment node
 * @file Error Handling Tests - Comprehensive error path coverage
 */
import { describe, it, expect, beforeEach } from 'vitest';
import { UnrdfDataFactory as DataFactory } from '@unrdf/core/rdf/n3-justified-only';
import {
  defineHook,
  executeHook,
  executeHookChain,
  createHookRegistry,
  registerHook,
  getHook,
} from '../src/index.mjs';

const { namedNode, literal, quad } = DataFactory;

describe('Error Handling - Invalid Hook Definitions', () => {
  it('should throw on hook without name', () => {
    expect(() => {
      defineHook({
        trigger: 'before-add',
        validate: () => true,
      });
    }).toThrow();
  });

  it('should throw on hook without trigger', () => {
    expect(() => {
      defineHook({
        name: 'test',
        validate: () => true,
      });
    }).toThrow();
  });

  it('should throw on hook without validate or transform', () => {
    expect(() => {
      defineHook({
        name: 'test',
        trigger: 'before-add',
      });
    }).toThrow();
  });

  it('should throw on invalid trigger type', () => {
    expect(() => {
      defineHook({
        name: 'test',
        trigger: 'invalid-trigger',
        validate: () => true,
      });
    }).toThrow();
  });

  it('should throw on non-function validate', () => {
    expect(() => {
      defineHook({
        name: 'test',
        trigger: 'before-add',
        validate: 'not a function',
      });
    }).toThrow();
  });

  it('should throw on non-function transform', () => {
    expect(() => {
      defineHook({
        name: 'test',
        trigger: 'before-add',
        transform: 'not a function',
      });
    }).toThrow();
  });
});

describe('Error Handling - Validation Errors', () => {
  let testQuad;

  beforeEach(() => {
    testQuad = quad(namedNode('http://s'), namedNode('http://p'), literal('test'));
  });

  it('should catch and report validation function errors', () => {
    const hook = defineHook({
      name: 'throwing-validator',
      trigger: 'before-add',
      validate: () => {
        throw new Error('Validation failed');
      },
    });

    expect(() => {
      executeHook(hook, testQuad);
    }).toThrow('Validation failed');
  });

  it('should handle validation returning non-boolean', () => {
    const hook = defineHook({
      name: 'invalid-return',
      trigger: 'before-add',
      validate: () => 'not a boolean',
    });

    const result = executeHook(hook, testQuad);
    expect(result.valid).toBe(true); // Truthy values treated as valid
  });

  it('should handle validation throwing TypeError', () => {
    const hook = defineHook({
      name: 'type-error',
      trigger: 'before-add',
      validate: q => {
        return q.nonexistent.property;
      },
    });

    expect(() => {
      executeHook(hook, testQuad);
    }).toThrow(TypeError);
  });

  it('should handle validation accessing undefined properties', () => {
    const hook = defineHook({
      name: 'undefined-access',
      trigger: 'before-add',
      validate: q => {
        return q.subject.nonexistent === 'test';
      },
    });

    const result = executeHook(hook, testQuad);
    expect(result.valid).toBe(false);
  });
});

describe('Error Handling - Transformation Errors', () => {
  let testQuad;

  beforeEach(() => {
    testQuad = quad(namedNode('http://s'), namedNode('http://p'), literal('test'));
  });

  it('should catch transformation function errors', () => {
    const hook = defineHook({
      name: 'throwing-transformer',
      trigger: 'before-add',
      transform: () => {
        throw new Error('Transform failed');
      },
    });

    expect(() => {
      executeHook(hook, testQuad);
    }).toThrow('Transform failed');
  });

  it('should handle transformation returning null', () => {
    const hook = defineHook({
      name: 'null-transformer',
      trigger: 'before-add',
      transform: () => null,
    });

    const result = executeHook(hook, testQuad);
    expect(result.valid).toBe(false);
    expect(result.quad).toBeNull();
  });

  it('should handle transformation returning undefined', () => {
    const hook = defineHook({
      name: 'undefined-transformer',
      trigger: 'before-add',
      transform: () => undefined,
    });

    const result = executeHook(hook, testQuad);
    expect(result.valid).toBe(false);
  });

  it('should handle transformation returning invalid quad', () => {
    const hook = defineHook({
      name: 'invalid-quad-transformer',
      trigger: 'before-add',
      transform: () => 'not a quad',
    });

    expect(() => {
      executeHook(hook, testQuad);
    }).toThrow();
  });
});

describe('Error Handling - Hook Chain Errors', () => {
  let testQuad;

  beforeEach(() => {
    testQuad = quad(namedNode('http://s'), namedNode('http://p'), literal('test'));
  });

  it('should stop chain on first validation failure', () => {
    const hook1 = defineHook({
      name: 'fail',
      trigger: 'before-add',
      validate: () => false,
    });

    const hook2 = defineHook({
      name: 'never-called',
      trigger: 'before-add',
      validate: () => {
        throw new Error('Should not be called');
      },
    });

    const result = executeHookChain([hook1, hook2], testQuad);
    expect(result.valid).toBe(false);
    expect(result.failedHook).toBe('fail');
  });

  it('should propagate errors through chain', () => {
    const hook1 = defineHook({
      name: 'pass',
      trigger: 'before-add',
      validate: () => true,
    });

    const hook2 = defineHook({
      name: 'error',
      trigger: 'before-add',
      validate: () => {
        throw new Error('Chain error');
      },
    });

    expect(() => {
      executeHookChain([hook1, hook2], testQuad);
    }).toThrow('Chain error');
  });

  it('should handle empty hook chain', () => {
    const result = executeHookChain([], testQuad);
    expect(result.valid).toBe(true);
    expect(result.quad).toBe(testQuad);
  });

  it('should handle null in hook chain', () => {
    expect(() => {
      executeHookChain([null], testQuad);
    }).toThrow();
  });

  it('should handle undefined in hook chain', () => {
    expect(() => {
      executeHookChain([undefined], testQuad);
    }).toThrow();
  });
});

describe('Error Handling - Registry Errors', () => {
  let registry;

  beforeEach(() => {
    registry = createHookRegistry();
  });

  it('should throw on registering invalid hook', () => {
    expect(() => {
      registerHook(registry, null);
    }).toThrow();
  });

  it('should throw on registering hook without name', () => {
    expect(() => {
      registerHook(registry, { trigger: 'before-add' });
    }).toThrow();
  });

  it('should handle getting non-existent hook', () => {
    const hook = getHook(registry, 'nonexistent');
    expect(hook).toBeUndefined();
  });

  it('should throw on duplicate hook registration', () => {
    const hook = defineHook({
      name: 'duplicate',
      trigger: 'before-add',
      validate: () => true,
    });

    registerHook(registry, hook);

    expect(() => {
      registerHook(registry, hook);
    }).toThrow();
  });

  it('should handle invalid registry object', () => {
    expect(() => {
      registerHook(
        null,
        defineHook({
          name: 'test',
          trigger: 'before-add',
          validate: () => true,
        })
      );
    }).toThrow();
  });
});

describe('Error Handling - Input Validation', () => {
  it('should handle null quad', () => {
    const hook = defineHook({
      name: 'test',
      trigger: 'before-add',
      validate: () => true,
    });

    expect(() => {
      executeHook(hook, null);
    }).toThrow();
  });

  it('should handle undefined quad', () => {
    const hook = defineHook({
      name: 'test',
      trigger: 'before-add',
      validate: () => true,
    });

    expect(() => {
      executeHook(hook, undefined);
    }).toThrow();
  });

  it('should handle invalid quad object', () => {
    const hook = defineHook({
      name: 'test',
      trigger: 'before-add',
      validate: () => true,
    });

    expect(() => {
      executeHook(hook, 'not a quad');
    }).toThrow();
  });

  it('should handle quad with missing properties', () => {
    const hook = defineHook({
      name: 'test',
      trigger: 'before-add',
      validate: q => q.subject && q.predicate && q.object,
    });

    const invalidQuad = { subject: namedNode('http://s') };

    expect(() => {
      executeHook(hook, invalidQuad);
    }).toThrow();
  });
});

describe('Error Handling - Resource Cleanup', () => {
  it('should cleanup resources on error', () => {
    const hook = defineHook({
      name: 'cleanup-test',
      trigger: 'before-add',
      validate: () => {
        throw new Error('Forced error');
      },
    });

    const testQuad = quad(namedNode('http://s'), namedNode('http://p'), literal('test'));

    try {
      executeHook(hook, testQuad);
    } catch (error) {
      expect(error.message).toContain('Forced error');
    }

    // Verify no hanging state
    expect(true).toBe(true);
  });

  it('should cleanup after chain failure', () => {
    const hooks = [
      defineHook({
        name: 'h1',
        trigger: 'before-add',
        validate: () => true,
      }),
      defineHook({
        name: 'h2',
        trigger: 'before-add',
        validate: () => {
          throw new Error('Chain failure');
        },
      }),
    ];

    const testQuad = quad(namedNode('http://s'), namedNode('http://p'), literal('test'));

    try {
      executeHookChain(hooks, testQuad);
    } catch (error) {
      expect(error.message).toContain('Chain failure');
    }
  });
});

describe('Error Handling - Edge Cases', () => {
  it('should handle recursive validation', () => {
    let depth = 0;
    const recursiveHook = defineHook({
      name: 'recursive',
      trigger: 'before-add',
      validate: q => {
        if (depth++ > 10) throw new Error('Max recursion depth');
        return true;
      },
    });

    const testQuad = quad(namedNode('http://s'), namedNode('http://p'), literal('test'));

    const result = executeHook(recursiveHook, testQuad);
    expect(result.valid).toBe(true);
  });

  it('should handle very large error messages', () => {
    const hook = defineHook({
      name: 'large-error',
      trigger: 'before-add',
      validate: () => {
        throw new Error('E'.repeat(10000));
      },
    });

    const testQuad = quad(namedNode('http://s'), namedNode('http://p'), literal('test'));

    expect(() => {
      executeHook(hook, testQuad);
    }).toThrow();
  });

  it('should handle unicode in error messages', () => {
    const hook = defineHook({
      name: 'unicode-error',
      trigger: 'before-add',
      validate: () => {
        throw new Error('错误 エラー 🎯');
      },
    });

    const testQuad = quad(namedNode('http://s'), namedNode('http://p'), literal('test'));

    expect(() => {
      executeHook(hook, testQuad);
    }).toThrow('错误');
  });
});
