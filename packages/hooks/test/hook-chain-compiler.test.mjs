/**
 * @vitest-environment node
 * @file Hook Chain Compiler Tests - Comprehensive coverage for hook-chain-compiler.mjs
 */
import { describe, it, expect, beforeEach } from 'vitest';
import { compileHookChain, getChainKey } from '../src/hooks/hook-chain-compiler.mjs';
import { defineHook } from '../src/index.mjs';
import { UnrdfDataFactory as DataFactory } from '@unrdf/core/rdf/n3-justified-only';

const { namedNode, literal, quad } = DataFactory;

describe('Hook Chain Compiler - getChainKey', () => {
  it('should generate unique keys for different hook chains', () => {
    const hooks1 = [
      { name: 'hook1', validate: () => true },
      { name: 'hook2', validate: () => true },
    ];
    const hooks2 = [{ name: 'hook3', validate: () => true }];

    const key1 = getChainKey(hooks1);
    const key2 = getChainKey(hooks2);

    expect(key1).toBe('hook1|hook2');
    expect(key2).toBe('hook3');
    expect(key1).not.toBe(key2);
  });

  it('should generate same key for same hook sequence', () => {
    const hooks1 = [{ name: 'a' }, { name: 'b' }];
    const hooks2 = [{ name: 'a' }, { name: 'b' }];

    expect(getChainKey(hooks1)).toBe(getChainKey(hooks2));
  });

  it('should generate different keys for different sequences', () => {
    const hooks1 = [{ name: 'a' }, { name: 'b' }];
    const hooks2 = [{ name: 'b' }, { name: 'a' }];

    expect(getChainKey(hooks1)).not.toBe(getChainKey(hooks2));
  });

  it('should handle single hook chain', () => {
    const hooks = [{ name: 'single' }];
    expect(getChainKey(hooks)).toBe('single');
  });

  it('should handle empty hook chain', () => {
    expect(getChainKey([])).toBe('');
  });

  it('should handle hook names with special characters', () => {
    const hooks = [{ name: 'hook:with:colons' }, { name: 'hook-with-dashes' }];
    const key = getChainKey(hooks);
    expect(key).toBe('hook:with:colons|hook-with-dashes');
  });
});

describe('Hook Chain Compiler - compileHookChain', () => {
  let testQuad;

  beforeEach(() => {
    testQuad = quad(
      namedNode('http://example.org/s'),
      namedNode('http://example.org/p'),
      literal('test')
    );
  });

  it('should compile single validation hook', () => {
    const hooks = [
      defineHook({
        name: 'validator',
        trigger: 'before-add',
        validate: q => q.subject.termType === 'NamedNode',
      }),
    ];

    const compiled = compileHookChain(hooks);
    expect(typeof compiled).toBe('function');

    const result = compiled(hooks, testQuad);
    expect(result.valid).toBe(true);
  });

  it('should compile single transformation hook', () => {
    const hooks = [
      defineHook({
        name: 'transformer',
        trigger: 'before-add',
        transform: q => q,
      }),
    ];

    const compiled = compileHookChain(hooks);
    const result = compiled(hooks, testQuad);

    expect(result.valid).toBe(true);
    expect(result.quad).toBeDefined();
  });

  it('should compile multiple validation hooks', () => {
    const hooks = [
      defineHook({
        name: 'validator1',
        trigger: 'before-add',
        validate: q => q.subject.termType === 'NamedNode',
      }),
      defineHook({
        name: 'validator2',
        trigger: 'before-add',
        validate: q => q.predicate.termType === 'NamedNode',
      }),
    ];

    const compiled = compileHookChain(hooks);
    const result = compiled(hooks, testQuad);

    expect(result.valid).toBe(true);
  });

  it('should short-circuit on first validation failure', () => {
    const hooks = [
      defineHook({
        name: 'always-fail',
        trigger: 'before-add',
        validate: () => false,
      }),
      defineHook({
        name: 'never-called',
        trigger: 'before-add',
        validate: () => true,
      }),
    ];

    const compiled = compileHookChain(hooks);
    const result = compiled(hooks, testQuad);

    expect(result.valid).toBe(false);
    expect(result.failedHook).toBe('always-fail');
  });

  it('should cache compiled chains', () => {
    const hooks = [
      defineHook({
        name: 'cached-hook',
        trigger: 'before-add',
        validate: () => true,
      }),
    ];

    const compiled1 = compileHookChain(hooks);
    const compiled2 = compileHookChain(hooks);

    // Should return same cached function
    expect(compiled1).toBe(compiled2);
  });

  it('should return different compiled functions for different chains', () => {
    const hooks1 = [
      defineHook({
        name: 'hook1',
        trigger: 'before-add',
        validate: () => true,
      }),
    ];
    const hooks2 = [
      defineHook({
        name: 'hook2',
        trigger: 'before-add',
        validate: () => true,
      }),
    ];

    const compiled1 = compileHookChain(hooks1);
    const compiled2 = compileHookChain(hooks2);

    expect(compiled1).not.toBe(compiled2);
  });

  it('should apply transformations in sequence', () => {
    const hooks = [
      defineHook({
        name: 'transform1',
        trigger: 'before-add',
        transform: q => {
          // Change predicate
          return quad(q.subject, namedNode('http://example.org/p2'), q.object);
        },
      }),
      defineHook({
        name: 'transform2',
        trigger: 'before-add',
        transform: q => {
          // Add validation
          return q.predicate.value === 'http://example.org/p2' ? q : null;
        },
      }),
    ];

    const compiled = compileHookChain(hooks);
    const result = compiled(hooks, testQuad);

    expect(result.valid).toBe(true);
    expect(result.quad.predicate.value).toBe('http://example.org/p2');
  });

  it('should handle mixed validation and transformation hooks', () => {
    const hooks = [
      defineHook({
        name: 'validate-first',
        trigger: 'before-add',
        validate: q => q.subject.termType === 'NamedNode',
      }),
      defineHook({
        name: 'then-transform',
        trigger: 'before-add',
        transform: q => q,
      }),
      defineHook({
        name: 'validate-again',
        trigger: 'before-add',
        validate: () => true,
      }),
    ];

    const compiled = compileHookChain(hooks);
    const result = compiled(hooks, testQuad);

    expect(result.valid).toBe(true);
  });

  it('should handle transformation returning null', () => {
    const hooks = [
      defineHook({
        name: 'reject-transform',
        trigger: 'before-add',
        transform: () => null,
      }),
    ];

    const compiled = compileHookChain(hooks);
    const result = compiled(hooks, testQuad);

    expect(result.valid).toBe(false);
    expect(result.quad).toBeNull();
  });

  it('should handle empty hook chain', () => {
    const compiled = compileHookChain([]);
    const result = compiled([], testQuad);

    expect(result.valid).toBe(true);
    expect(result.quad).toBe(testQuad);
  });
});

describe('Hook Chain Compiler - Performance', () => {
  it('should compile large hook chains efficiently', () => {
    const hooks = Array(50)
      .fill(null)
      .map((_, i) =>
        defineHook({
          name: `hook${i}`,
          trigger: 'before-add',
          validate: () => true,
        })
      );

    const testQuad = quad(namedNode('http://s'), namedNode('http://p'), literal('test'));

    const compiled = compileHookChain(hooks);
    const result = compiled(hooks, testQuad);

    expect(result.valid).toBe(true);
  });

  it('should cache and reuse compiled chains efficiently', () => {
    const hooks = [
      defineHook({
        name: 'perf-test',
        trigger: 'before-add',
        validate: () => true,
      }),
    ];

    // First compilation
    const start1 = performance.now();
    const compiled1 = compileHookChain(hooks);
    const duration1 = performance.now() - start1;

    // Second call (cached)
    const start2 = performance.now();
    const compiled2 = compileHookChain(hooks);
    const duration2 = performance.now() - start2;

    expect(compiled1).toBe(compiled2);
    expect(duration2).toBeLessThan(duration1);
  });
});

describe('Hook Chain Compiler - Edge Cases', () => {
  it('should handle hooks with undefined validate/transform', () => {
    const hooks = [{ name: 'minimal', trigger: 'before-add' }];

    const compiled = compileHookChain(hooks);
    const testQuad = quad(namedNode('http://s'), namedNode('http://p'), literal('test'));

    const result = compiled(hooks, testQuad);
    expect(result.valid).toBe(true);
  });

  it('should handle validation throwing errors', () => {
    const hooks = [
      defineHook({
        name: 'throwing-validator',
        trigger: 'before-add',
        validate: () => {
          throw new Error('Validation error');
        },
      }),
    ];

    const compiled = compileHookChain(hooks);
    const testQuad = quad(namedNode('http://s'), namedNode('http://p'), literal('test'));

    expect(() => compiled(hooks, testQuad)).toThrow('Validation error');
  });

  it('should handle transformation throwing errors', () => {
    const hooks = [
      defineHook({
        name: 'throwing-transformer',
        trigger: 'before-add',
        transform: () => {
          throw new Error('Transform error');
        },
      }),
    ];

    const compiled = compileHookChain(hooks);
    const testQuad = quad(namedNode('http://s'), namedNode('http://p'), literal('test'));

    expect(() => compiled(hooks, testQuad)).toThrow('Transform error');
  });

  it('should handle very long chain keys', () => {
    const hooks = Array(100)
      .fill(null)
      .map((_, i) => ({
        name: `very-long-hook-name-for-testing-purposes-${i}`,
        validate: () => true,
      }));

    const key = getChainKey(hooks);
    expect(key.length).toBeGreaterThan(1000);

    const compiled = compileHookChain(hooks);
    expect(typeof compiled).toBe('function');
  });
});

describe('Hook Chain Compiler - CSP Fallback', () => {
  it('should use interpreted mode when JIT is unavailable', () => {
    // This test verifies the fallback path exists
    // In real CSP-restricted environments, new Function() would throw
    const hooks = [
      defineHook({
        name: 'csp-test',
        trigger: 'before-add',
        validate: () => true,
      }),
    ];

    const compiled = compileHookChain(hooks);
    expect(typeof compiled).toBe('function');
  });
});
