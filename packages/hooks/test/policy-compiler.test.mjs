/**
 * Policy Compiler Tests
 * Tests for JIT compilation, caching, and optimized execution
 */
import { describe, it, expect, beforeEach } from 'vitest';
import {
  PolicyPatterns,
  compilePolicy,
  compileHook,
  compileHooks,
  executeCompiledHook,
  executeCompiledChain,
  batchValidateCompiled,
  precompilePolicies,
  precompileHooks,
  clearPolicyCache,
  getCompilerStats,
  resetCompilerStats,
} from '../src/policy-compiler.mjs';

describe('Policy Compilation', () => {
  beforeEach(() => {
    clearPolicyCache();
    resetCompilerStats();
  });

  describe('Basic Policy Patterns', () => {
    it('should compile ALLOW_ALL policy', () => {
      const policy = { type: PolicyPatterns.ALLOW_ALL };
      const compiled = compilePolicy(policy);

      const quad = { subject: { value: 'test' } };
      expect(compiled(quad)).toBe(true);
    });

    it('should compile DENY_ALL policy', () => {
      const policy = { type: PolicyPatterns.DENY_ALL };
      const compiled = compilePolicy(policy);

      const quad = { subject: { value: 'test' } };
      expect(compiled(quad)).toBe(false);
    });

    it('should compile SUBJECT_PATTERN with regex', () => {
      const policy = {
        type: PolicyPatterns.SUBJECT_PATTERN,
        config: { pattern: /^http:\/\/example\.org/ },
      };
      const compiled = compilePolicy(policy);

      const quad1 = { subject: { value: 'http://example.org/test' } };
      const quad2 = { subject: { value: 'http://other.org/test' } };

      expect(compiled(quad1)).toBe(true);
      expect(compiled(quad2)).toBe(false);
    });

    it('should compile SUBJECT_PATTERN with string', () => {
      const policy = {
        type: PolicyPatterns.SUBJECT_PATTERN,
        config: { pattern: '^http://example\\.org' },
      };
      const compiled = compilePolicy(policy);

      const quad1 = { subject: { value: 'http://example.org/test' } };
      const quad2 = { subject: { value: 'http://other.org/test' } };

      expect(compiled(quad1)).toBe(true);
      expect(compiled(quad2)).toBe(false);
    });

    it('should compile PREDICATE_PATTERN', () => {
      const policy = {
        type: PolicyPatterns.PREDICATE_PATTERN,
        config: { pattern: /rdf:type/ },
      };
      const compiled = compilePolicy(policy);

      const quad1 = { predicate: { value: 'rdf:type' } };
      const quad2 = { predicate: { value: 'rdfs:label' } };

      expect(compiled(quad1)).toBe(true);
      expect(compiled(quad2)).toBe(false);
    });

    it('should compile OBJECT_PATTERN', () => {
      const policy = {
        type: PolicyPatterns.OBJECT_PATTERN,
        config: { pattern: /^literal/ },
      };
      const compiled = compilePolicy(policy);

      const quad1 = { object: { value: 'literal_value' } };
      const quad2 = { object: { value: 'other_value' } };

      expect(compiled(quad1)).toBe(true);
      expect(compiled(quad2)).toBe(false);
    });

    it('should compile NAMESPACE pattern', () => {
      const policy = {
        type: PolicyPatterns.NAMESPACE,
        config: { namespace: 'http://example.org/' },
      };
      const compiled = compilePolicy(policy);

      const quad1 = { subject: { value: 'http://example.org/s' }, predicate: { value: 'p' } };
      const quad2 = { subject: { value: 'http://other.org/s' }, predicate: { value: 'http://example.org/p' } };
      const quad3 = { subject: { value: 'http://other.org/s' }, predicate: { value: 'http://other.org/p' } };

      expect(compiled(quad1)).toBe(true); // Subject matches
      expect(compiled(quad2)).toBe(true); // Predicate matches
      expect(compiled(quad3)).toBe(false); // Neither matches
    });

    it('should compile CUSTOM policy with function', () => {
      const customFn = (quad) => quad.subject.value.length > 10;
      const policy = {
        type: PolicyPatterns.CUSTOM,
        evaluate: customFn,
      };
      const compiled = compilePolicy(policy);

      const quad1 = { subject: { value: 'short' } };
      const quad2 = { subject: { value: 'very_long_subject_value' } };

      expect(compiled(quad1)).toBe(false);
      expect(compiled(quad2)).toBe(true);
    });

    it('should default to allow for unknown patterns', () => {
      const policy = { type: 'UNKNOWN_PATTERN' };
      const compiled = compilePolicy(policy);

      const quad = { subject: { value: 'test' } };
      expect(compiled(quad)).toBe(true);
    });
  });

  describe('Policy Caching', () => {
    it('should cache compiled policies', () => {
      const policy = {
        type: PolicyPatterns.SUBJECT_PATTERN,
        config: { pattern: /test/ },
      };

      resetCompilerStats();

      compilePolicy(policy);
      const stats1 = getCompilerStats();
      expect(stats1.cacheMisses).toBe(1);

      compilePolicy(policy);
      const stats2 = getCompilerStats();
      expect(stats2.cacheHits).toBe(1);
    });

    it('should use different cache keys for different configs', () => {
      const policy1 = {
        type: PolicyPatterns.SUBJECT_PATTERN,
        config: { pattern: /test1/ },
      };
      const policy2 = {
        type: PolicyPatterns.SUBJECT_PATTERN,
        config: { pattern: /test2/ },
      };

      resetCompilerStats();

      compilePolicy(policy1);
      compilePolicy(policy2);

      const stats = getCompilerStats();
      expect(stats.cacheMisses).toBe(2); // Both are cache misses (different patterns)
    });

    it('should clear pattern cache', () => {
      const policy = { type: PolicyPatterns.ALLOW_ALL };

      compilePolicy(policy);
      clearPolicyCache();

      resetCompilerStats();
      compilePolicy(policy);

      const stats = getCompilerStats();
      expect(stats.cacheMisses).toBe(1); // Cache was cleared
    });
  });

  describe('Precompilation', () => {
    it('should precompile multiple policies', () => {
      const policies = [
        { type: PolicyPatterns.ALLOW_ALL },
        { type: PolicyPatterns.DENY_ALL },
        { type: PolicyPatterns.SUBJECT_PATTERN, config: { pattern: /test/ } },
      ];

      const result = precompilePolicies(policies);

      expect(result.compiled).toBe(3);
      expect(result.errors).toHaveLength(0);
    });

    it('should handle precompilation errors gracefully', () => {
      const policies = [
        { type: PolicyPatterns.ALLOW_ALL },
        null, // Will cause error
        { type: PolicyPatterns.DENY_ALL },
      ];

      const result = precompilePolicies(policies);

      expect(result.compiled).toBeGreaterThan(0);
      expect(result.errors.length).toBeGreaterThan(0);
    });
  });
});

describe('Hook Compilation', () => {
  beforeEach(() => {
    clearPolicyCache();
    resetCompilerStats();
  });

  describe('Basic Hook Compilation', () => {
    it('should compile hook with validation', () => {
      const hook = {
        name: 'test-hook',
        validate: (quad) => quad.subject.value.startsWith('http://'),
      };

      const compiled = compileHook(hook);

      expect(compiled._compiled).toBe(true);
      expect(compiled.validate).toBeDefined();
    });

    it('should compile hook with policy', () => {
      const hook = {
        name: 'test-hook',
        policy: {
          type: PolicyPatterns.SUBJECT_PATTERN,
          config: { pattern: /test/ },
        },
        validate: () => true,
      };

      const compiled = compileHook(hook);

      expect(compiled._compiled).toBe(true);
      expect(compiled.validate).toBeDefined();
    });

    it('should compile hook with transform', () => {
      const hook = {
        name: 'test-hook',
        transform: (quad) => ({ ...quad, transformed: true }),
      };

      const compiled = compileHook(hook);

      expect(compiled._compiled).toBe(true);
      expect(compiled.transform).toBeDefined();
    });

    it('should cache compiled hooks in WeakMap', () => {
      const hook = {
        name: 'test-hook',
        validate: () => true,
      };

      resetCompilerStats();

      compileHook(hook);
      const stats1 = getCompilerStats();
      expect(stats1.cacheMisses).toBe(1);

      compileHook(hook); // Same object - should hit cache
      const stats2 = getCompilerStats();
      expect(stats2.cacheHits).toBe(1);
    });
  });

  describe('Batch Hook Compilation', () => {
    it('should compile multiple hooks', () => {
      const hooks = [
        { name: 'hook1', validate: () => true },
        { name: 'hook2', validate: () => true },
        { name: 'hook3', transform: (q) => q },
      ];

      const compiled = compileHooks(hooks);

      expect(compiled).toHaveLength(3);
      expect(compiled.every(h => h._compiled)).toBe(true);
    });
  });

  describe('Precompilation', () => {
    it('should precompile hooks', () => {
      const hooks = [
        { name: 'hook1', validate: () => true },
        { name: 'hook2', policy: { type: PolicyPatterns.ALLOW_ALL } },
      ];

      const result = precompileHooks(hooks);

      expect(result.compiled).toBe(2);
      expect(result.errors).toHaveLength(0);
    });

    it('should handle errors in precompilation', () => {
      const hooks = [
        { name: 'hook1', validate: () => true },
        null, // Will cause error
      ];

      const result = precompileHooks(hooks);

      expect(result.errors.length).toBeGreaterThan(0);
    });
  });
});

describe('Compiled Execution', () => {
  beforeEach(() => {
    resetCompilerStats();
  });

  describe('Single Hook Execution', () => {
    it('should execute compiled hook with validation', () => {
      const hook = {
        name: 'test-hook',
        validate: (quad) => quad.subject.value === 'valid',
      };

      const quad1 = { subject: { value: 'valid' } };
      const quad2 = { subject: { value: 'invalid' } };

      const result1 = executeCompiledHook(hook, quad1);
      const result2 = executeCompiledHook(hook, quad2);

      expect(result1.valid).toBe(true);
      expect(result2.valid).toBe(false);
    });

    it('should execute hook with transformation', () => {
      const hook = {
        name: 'test-hook',
        transform: (quad) => ({ ...quad, transformed: true }),
      };

      const quad = { subject: { value: 'test' } };
      const result = executeCompiledHook(hook, quad);

      expect(result.valid).toBe(true);
      expect(result.quad.transformed).toBe(true);
    });

    it('should handle validation errors', () => {
      const hook = {
        name: 'test-hook',
        validate: () => { throw new Error('Validation error'); },
      };

      const quad = { subject: { value: 'test' } };
      const result = executeCompiledHook(hook, quad);

      expect(result.valid).toBe(false);
      expect(result.error).toContain('Validation error');
    });

    it('should track execution statistics', () => {
      const hook = { name: 'test-hook', validate: () => true };
      const quad = { subject: { value: 'test' } };

      resetCompilerStats();

      executeCompiledHook(hook, quad);

      const stats = getCompilerStats();
      expect(stats.evaluations).toBe(1);
    });
  });

  describe('Hook Chain Execution', () => {
    it('should execute hooks in chain', () => {
      const hooks = [
        {
          name: 'hook1',
          validate: (quad) => quad.subject.value.length > 0,
        },
        {
          name: 'hook2',
          transform: (quad) => ({ ...quad, processed: true }),
        },
        {
          name: 'hook3',
          validate: (quad) => quad.processed === true,
        },
      ];

      const quad = { subject: { value: 'test' } };
      const result = executeCompiledChain(hooks, quad);

      expect(result.valid).toBe(true);
      expect(result.quad.processed).toBe(true);
      expect(result.results).toHaveLength(3);
    });

    it('should stop chain on validation failure', () => {
      const hooks = [
        { name: 'hook1', validate: () => true },
        { name: 'hook2', validate: () => false },
        { name: 'hook3', validate: () => true },
      ];

      const quad = { subject: { value: 'test' } };
      const result = executeCompiledChain(hooks, quad);

      expect(result.valid).toBe(false);
      expect(result.results).toHaveLength(2); // Stopped at hook2
      expect(result.error).toBeDefined();
    });

    it('should precompile all hooks in chain', () => {
      const hooks = [
        { name: 'hook1', validate: () => true },
        { name: 'hook2', validate: () => true },
      ];

      const quad = { subject: { value: 'test' } };

      resetCompilerStats();
      executeCompiledChain(hooks, quad);

      const stats = getCompilerStats();
      expect(stats.compiled).toBeGreaterThan(0);
    });
  });

  describe('Batch Validation', () => {
    it('should validate multiple quads', () => {
      const hooks = [
        {
          name: 'hook1',
          validate: (quad) => quad.subject.value.startsWith('valid'),
        },
      ];

      const quads = [
        { subject: { value: 'valid1' } },
        { subject: { value: 'invalid' } },
        { subject: { value: 'valid2' } },
      ];

      const bitmap = batchValidateCompiled(hooks, quads);

      expect(bitmap[0]).toBe(1); // valid
      expect(bitmap[1]).toBe(0); // invalid
      expect(bitmap[2]).toBe(1); // valid
    });

    it('should handle empty hooks array', () => {
      const quads = [
        { subject: { value: 'test' } },
      ];

      const bitmap = batchValidateCompiled([], quads);

      expect(bitmap[0]).toBe(1); // All valid when no hooks
    });

    it('should handle validation errors gracefully', () => {
      const hooks = [
        {
          name: 'hook1',
          validate: () => { throw new Error('Error'); },
        },
      ];

      const quads = [
        { subject: { value: 'test' } },
      ];

      const bitmap = batchValidateCompiled(hooks, quads);

      expect(bitmap[0]).toBe(0); // Invalid due to error
    });

    it('should compile hooks before batch validation', () => {
      const hooks = [
        { name: 'hook1', validate: () => true },
      ];

      const quads = Array(100).fill({ subject: { value: 'test' } });

      resetCompilerStats();
      batchValidateCompiled(hooks, quads);

      const stats = getCompilerStats();
      expect(stats.compiled).toBeGreaterThan(0);
    });
  });
});

describe('Statistics and Monitoring', () => {
  beforeEach(() => {
    resetCompilerStats();
    clearPolicyCache();
  });

  describe('Compiler Statistics', () => {
    it('should track compilation count', () => {
      const policy1 = { type: PolicyPatterns.ALLOW_ALL };
      const policy2 = { type: PolicyPatterns.DENY_ALL };

      compilePolicy(policy1);
      compilePolicy(policy2);

      const stats = getCompilerStats();
      expect(stats.compiled).toBe(2);
    });

    it('should track cache hits and misses', () => {
      const policy = { type: PolicyPatterns.ALLOW_ALL };

      compilePolicy(policy); // Miss
      compilePolicy(policy); // Hit
      compilePolicy(policy); // Hit

      const stats = getCompilerStats();
      expect(stats.cacheMisses).toBe(1);
      expect(stats.cacheHits).toBe(2);
    });

    it('should calculate cache hit rate', () => {
      const policy = { type: PolicyPatterns.ALLOW_ALL };

      compilePolicy(policy); // Miss
      compilePolicy(policy); // Hit

      const stats = getCompilerStats();
      expect(stats.cacheHitRate).toBe(0.5);
    });

    it('should track evaluation count', () => {
      const hook = { name: 'hook1', validate: () => true };
      const quad = { subject: { value: 'test' } };

      executeCompiledHook(hook, quad);
      executeCompiledHook(hook, quad);

      const stats = getCompilerStats();
      expect(stats.evaluations).toBe(2);
    });

    it('should track compilation time', () => {
      const policy = { type: PolicyPatterns.ALLOW_ALL };

      compilePolicy(policy);

      const stats = getCompilerStats();
      expect(stats.totalCompileTimeMs).toBeGreaterThan(0);
      expect(stats.avgCompileTimeMs).toBeGreaterThan(0);
    });

    it('should track evaluation time', () => {
      const hook = { name: 'hook1', validate: () => true };
      const quad = { subject: { value: 'test' } };

      executeCompiledHook(hook, quad);

      const stats = getCompilerStats();
      expect(stats.totalEvalTimeMs).toBeGreaterThan(0);
      expect(stats.avgEvalTimeUs).toBeGreaterThan(0);
    });

    it('should reset statistics', () => {
      const policy = { type: PolicyPatterns.ALLOW_ALL };
      compilePolicy(policy);

      resetCompilerStats();

      const stats = getCompilerStats();
      expect(stats.compiled).toBe(0);
      expect(stats.cacheHits).toBe(0);
      expect(stats.cacheMisses).toBe(0);
    });
  });
});

describe('Performance Benchmarks', () => {
  it('should compile policies quickly', () => {
    const policies = Array(1000).fill({
      type: PolicyPatterns.SUBJECT_PATTERN,
      config: { pattern: /test/ },
    });

    const start = performance.now();
    policies.forEach(p => compilePolicy(p));
    const elapsed = performance.now() - start;

    // Should compile 1000 policies in < 10ms (most will be cached)
    expect(elapsed).toBeLessThan(10);
  });

  it('should execute compiled hooks quickly', () => {
    const hook = {
      name: 'test-hook',
      validate: (quad) => quad.subject.value.length > 0,
    };
    const compiled = compileHook(hook);

    const quad = { subject: { value: 'test' } };

    const start = performance.now();
    for (let i = 0; i < 10000; i++) {
      executeCompiledHook(compiled, quad);
    }
    const elapsed = performance.now() - start;

    // Should execute 10K hooks in < 100ms (< 10us each)
    expect(elapsed).toBeLessThan(100);
  });

  it('should achieve sub-microsecond policy evaluation', () => {
    const policy = { type: PolicyPatterns.ALLOW_ALL };
    const compiled = compilePolicy(policy);

    const quad = { subject: { value: 'test' } };

    const start = performance.now();
    for (let i = 0; i < 100000; i++) {
      compiled(quad);
    }
    const elapsed = performance.now() - start;

    // 100K evaluations in < 10ms = < 0.1us each
    expect(elapsed).toBeLessThan(10);
  });

  it('should validate batches efficiently', () => {
    const hooks = [
      { name: 'hook1', validate: (quad) => quad.subject.value.length > 0 },
      { name: 'hook2', validate: (quad) => quad.subject.value.length < 1000 },
    ];

    const quads = Array(1000).fill({ subject: { value: 'test' } });

    const start = performance.now();
    batchValidateCompiled(hooks, quads);
    const elapsed = performance.now() - start;

    // Should validate 1000 quads in < 10ms
    expect(elapsed).toBeLessThan(10);
  });
});

describe('Edge Cases', () => {
  it('should handle null quad gracefully', () => {
    const hook = { name: 'hook1', validate: () => true };

    expect(() => executeCompiledHook(hook, null)).not.toThrow();
  });

  it('should handle hook without validation or transform', () => {
    const hook = { name: 'hook1' };
    const quad = { subject: { value: 'test' } };

    const result = executeCompiledHook(hook, quad);

    expect(result.valid).toBe(true);
    expect(result.quad).toEqual(quad);
  });

  it('should handle regex serialization in cache key', () => {
    const policy1 = {
      type: PolicyPatterns.SUBJECT_PATTERN,
      config: { pattern: /test/ },
    };
    const policy2 = {
      type: PolicyPatterns.SUBJECT_PATTERN,
      config: { pattern: /test/ },
    };

    resetCompilerStats();

    compilePolicy(policy1);
    compilePolicy(policy2);

    const stats = getCompilerStats();
    // Should recognize as same pattern
    expect(stats.cacheHits).toBeGreaterThan(0);
  });
});
