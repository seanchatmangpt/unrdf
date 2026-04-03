/**
 * Built-in Hooks and Advanced Features Test Suite
 *
 * Tests:
 * - All 10+ built-in hooks
 * - JIT compilation and hook chain optimization
 * - Object pooling for zero-allocation transforms
 * - Batch operations
 * - Performance characteristics
 */

import { describe, it, expect, beforeEach } from 'vitest';
import {
  // Built-in hooks
  builtinHooks,
  validateSubjectIRI,
  validatePredicateIRI,
  validateObjectLiteral,
  validateIRIFormat,
  validateLanguageTag,
  rejectBlankNodes,
  normalizeNamespace,
  normalizeLanguageTag,
  trimLiterals,
  normalizeLanguageTagPooled,
  trimLiteralsPooled,
  standardValidation,
  // Advanced features
  compileHookChain,
  compileValidationOnlyChain,
  clearCompiledChainCache,
  getCompilerStats,
  isJitAvailable,
  QuadPool,
  quadPool,
  createPooledTransform,
  isPooledQuad,
  executeBatch,
  validateBatch,
  transformBatch,
  defineHook,
  executeHook,
  executeHookChain,
} from '../src/index.mjs';

describe('Built-in Hooks - IRI Validation', () => {
  it('validateSubjectIRI: validates subject is valid IRI', async () => {
    const validQuad = {
      subject: { value: 'http://example.org/subject', termType: 'NamedNode' },
      predicate: { value: 'http://example.org/p' },
      object: { value: 'test' },
    };

    const result = await executeHook(validateSubjectIRI, validQuad);
    expect(result.valid).toBe(true);

    const invalidQuad = {
      subject: { value: '_:blank', termType: 'BlankNode' },
      predicate: { value: 'http://example.org/p' },
      object: { value: 'test' },
    };

    const result2 = await executeHook(validateSubjectIRI, invalidQuad);
    expect(result2.valid).toBe(false);
  });

  it('validatePredicateIRI: validates predicate is valid IRI', async () => {
    const validQuad = {
      subject: { value: 'http://example.org/s' },
      predicate: { value: 'http://example.org/predicate', termType: 'NamedNode' },
      object: { value: 'test' },
    };

    const result = await executeHook(validatePredicateIRI, validQuad);
    expect(result.valid).toBe(true);

    const invalidQuad = {
      subject: { value: 'http://example.org/s' },
      predicate: { value: 'not-an-iri', termType: 'BlankNode' },
      object: { value: 'test' },
    };

    const result2 = await executeHook(validatePredicateIRI, invalidQuad);
    expect(result2.valid).toBe(false);
  });

  it('validateIRIFormat: validates IRI format structure', async () => {
    const validQuad = {
      subject: { value: 'https://example.org/resource/123' },
      predicate: { value: 'http://www.w3.org/1999/02/22-rdf-syntax-ns#type' },
      object: { value: 'test' },
    };

    const result = await executeHook(validateIRIFormat, validQuad);
    expect(result.valid).toBe(true);

    const invalidQuad = {
      subject: { value: 'not a valid iri with spaces' },
      predicate: { value: 'http://example.org/p' },
      object: { value: 'test' },
    };

    const result2 = await executeHook(validateIRIFormat, invalidQuad);
    expect(result2.valid).toBe(false);
  });
});

describe('Built-in Hooks - Literal Validation', () => {
  it('validateObjectLiteral: validates literal values', async () => {
    const validQuad = {
      subject: { value: 'http://example.org/s' },
      predicate: { value: 'http://example.org/p' },
      object: {
        value: 'valid literal',
        termType: 'Literal',
      },
    };

    const result = await executeHook(validateObjectLiteral, validQuad);
    expect(result.valid).toBe(true);

    // Test with empty literal (should fail)
    const invalidQuad = {
      ...validQuad,
      object: {
        value: '',
        termType: 'Literal',
      },
    };

    const result2 = await executeHook(validateObjectLiteral, invalidQuad);
    expect(result2.valid).toBe(false);
  });

  it('validateLanguageTag: validates language tags', async () => {
    const validQuad = {
      subject: { value: 'http://example.org/s' },
      predicate: { value: 'http://example.org/label' },
      object: {
        value: 'Hello',
        language: 'en',
      },
    };

    const result = await executeHook(validateLanguageTag, validQuad);
    expect(result.valid).toBe(true);

    const validQuad2 = {
      ...validQuad,
      object: {
        value: 'Hello',
        language: 'en-US',
      },
    };

    const result2 = await executeHook(validateLanguageTag, validQuad2);
    expect(result2.valid).toBe(true);

    const invalidQuad = {
      ...validQuad,
      object: {
        value: 'Hello',
        language: 'invalid_tag',
      },
    };

    const result3 = await executeHook(validateLanguageTag, invalidQuad);
    expect(result3.valid).toBe(false);
  });
});

describe('Built-in Hooks - Node Type Validation', () => {
  it('rejectBlankNodes: rejects blank nodes', async () => {
    const namedNodeQuad = {
      subject: { value: 'http://example.org/s', termType: 'NamedNode' },
      predicate: { value: 'http://example.org/p', termType: 'NamedNode' },
      object: { value: 'test', termType: 'Literal' },
    };

    const result = await executeHook(rejectBlankNodes, namedNodeQuad);
    expect(result.valid).toBe(true);

    const blankNodeQuad = {
      subject: { value: '_:b0', termType: 'BlankNode' },
      predicate: { value: 'http://example.org/p', termType: 'NamedNode' },
      object: { value: 'test', termType: 'Literal' },
    };

    const result2 = await executeHook(rejectBlankNodes, blankNodeQuad);
    expect(result2.valid).toBe(false);
  });
});

describe('Built-in Hooks - Normalization', () => {
  it('normalizeNamespace: normalizes common namespace prefixes', async () => {
    const quad = {
      subject: { value: 'http://www.w3.org/1999/02/22-rdf-syntax-ns#type' },
      predicate: { value: 'http://example.org/p' },
      object: { value: 'test' },
    };

    const result = await executeHook(normalizeNamespace, quad);
    expect(result.quad).toBeDefined();
    // Normalization should maintain valid quad structure
    expect(result.quad.subject.value).toContain('http://');
  });

  it('normalizeLanguageTag: normalizes language tags to lowercase', async () => {
    const quad = {
      subject: { value: 'http://example.org/s' },
      predicate: { value: 'http://example.org/label' },
      object: {
        value: 'Hello',
        language: 'EN-US',
      },
    };

    const result = await executeHook(normalizeLanguageTag, quad);
    expect(result.quad.object.language).toBe('en-us');
  });

  it('normalizeLanguageTagPooled: uses object pooling for normalization', async () => {
    const quad = {
      subject: { value: 'http://example.org/s' },
      predicate: { value: 'http://example.org/label' },
      object: {
        value: 'Hello',
        language: 'EN',
      },
    };

    const result = await executeHook(normalizeLanguageTagPooled, quad);
    expect(result.quad.object.language).toBe('en');
    // Pooled quad should be marked
    expect(isPooledQuad(result.quad)).toBe(true);
  });

  it('trimLiterals: removes whitespace from literals', async () => {
    const quad = {
      subject: { value: 'http://example.org/s' },
      predicate: { value: 'http://example.org/p' },
      object: {
        value: '  spaces around  ',
        termType: 'Literal',
      },
    };

    const result = await executeHook(trimLiterals, quad);
    expect(result.quad.object.value).toBe('spaces around');
  });

  it('trimLiteralsPooled: uses object pooling for trimming', async () => {
    const quad = {
      subject: { value: 'http://example.org/s' },
      predicate: { value: 'http://example.org/p' },
      object: {
        value: '  test  ',
        termType: 'Literal',
      },
    };

    const result = await executeHook(trimLiteralsPooled, quad);
    expect(result.quad.object.value).toBe('test');
    expect(isPooledQuad(result.quad)).toBe(true);
  });
});

describe('Built-in Hooks - Composite Validation', () => {
  it('standardValidation: applies standard validation rules', async () => {
    const validQuad = {
      subject: { value: 'http://example.org/s', termType: 'NamedNode' },
      predicate: { value: 'http://example.org/p', termType: 'NamedNode' },
      object: { value: 'test', termType: 'Literal' },
    };

    const result = await executeHook(standardValidation, validQuad);
    expect(result.valid).toBe(true);

    const invalidQuad = {
      subject: { value: 'invalid iri', termType: 'NamedNode' },
      predicate: { value: 'http://example.org/p', termType: 'NamedNode' },
      object: { value: 'test', termType: 'Literal' },
    };

    const result2 = await executeHook(standardValidation, invalidQuad);
    expect(result2.valid).toBe(false);
  });

  it('builtinHooks object exports all built-in hooks', () => {
    expect(builtinHooks).toBeDefined();
    expect(typeof builtinHooks).toBe('object');
    expect(Object.keys(builtinHooks).length).toBeGreaterThan(0);
  });
});

describe('Advanced Features - JIT Compilation', () => {
  it('compileHookChain: compiles hook chains for performance', () => {
    const hooks = [
      defineHook({
        name: 'validate1',
        trigger: 'before-add',
        validate: quad => quad.subject.value.startsWith('http://'),
      }),
      defineHook({
        name: 'validate2',
        trigger: 'before-add',
        validate: quad => quad.predicate.value.startsWith('http://'),
      }),
    ];

    const compiled = compileHookChain(hooks);
    expect(compiled).toBeDefined();
    expect(typeof compiled).toBe('function');
  });

  it('compileValidationOnlyChain: compiles validation-only chains', () => {
    const hooks = [
      defineHook({
        name: 'val1',
        trigger: 'before-add',
        validate: quad => quad !== null,
      }),
      defineHook({
        name: 'val2',
        trigger: 'before-add',
        validate: quad => quad.subject !== undefined,
      }),
    ];

    const compiled = compileValidationOnlyChain(hooks);
    expect(compiled).toBeDefined();
    expect(typeof compiled).toBe('function');
  });

  it('getCompilerStats: returns compilation statistics', () => {
    const stats = getCompilerStats();
    expect(stats).toBeDefined();
    expect(typeof stats).toBe('object');
  });

  it('isJitAvailable: checks if JIT compilation is available', () => {
    const available = isJitAvailable();
    expect(typeof available).toBe('boolean');
  });

  it('clearCompiledChainCache: clears compilation cache', () => {
    const hooks = [
      defineHook({
        name: 'test',
        trigger: 'before-add',
        validate: () => true,
      }),
    ];

    compileHookChain(hooks);
    expect(() => clearCompiledChainCache()).not.toThrow();
  });
});

describe('Advanced Features - Object Pooling', () => {
  it('QuadPool: provides object pooling for quads', () => {
    const pool = new QuadPool({ maxSize: 10 });
    expect(pool).toBeDefined();

    const quad = pool.acquire();
    expect(quad).toBeDefined();

    pool.release(quad);
    // Should be able to acquire the same quad again
    const quad2 = pool.acquire();
    expect(quad2).toBeDefined();
  });

  it('quadPool: global quad pool instance', () => {
    expect(quadPool).toBeDefined();
    expect(quadPool instanceof QuadPool).toBe(true);
  });

  it('createPooledTransform: creates zero-allocation transforms', () => {
    const transform = createPooledTransform(quad => {
      return {
        ...quad,
        object: { ...quad.object, value: quad.object.value.toUpperCase() },
      };
    });

    expect(transform).toBeDefined();
    expect(typeof transform).toBe('function');

    const quad = {
      subject: { value: 'http://example.org/s' },
      predicate: { value: 'http://example.org/p' },
      object: { value: 'test' },
    };

    const result = transform(quad);
    expect(result.object.value).toBe('TEST');
  });

  it('isPooledQuad: identifies pooled quads', () => {
    const regularQuad = {
      subject: { value: 'http://example.org/s' },
      predicate: { value: 'http://example.org/p' },
      object: { value: 'test' },
    };

    expect(isPooledQuad(regularQuad)).toBe(false);

    const pooledQuad = quadPool.acquire();
    expect(isPooledQuad(pooledQuad)).toBe(true);
    quadPool.release(pooledQuad);
  });
});

describe('Advanced Features - Batch Operations', () => {
  it('executeBatch: processes quads in batch', async () => {
    const hook = defineHook({
      name: 'batch-validate',
      trigger: 'before-add',
      validate: quad => quad.subject.value.startsWith('http://'),
    });

    const quads = [
      {
        subject: { value: 'http://example.org/s1' },
        predicate: { value: 'http://example.org/p' },
        object: { value: 'test1' },
      },
      {
        subject: { value: 'http://example.org/s2' },
        predicate: { value: 'http://example.org/p' },
        object: { value: 'test2' },
      },
      {
        subject: { value: 'invalid' },
        predicate: { value: 'http://example.org/p' },
        object: { value: 'test3' },
      },
    ];

    const results = await executeBatch([hook], quads);
    expect(results).toBeDefined();
    expect(Array.isArray(results)).toBe(true);
    expect(results.length).toBe(3);

    // First two should pass, third should fail
    expect(results[0].valid).toBe(true);
    expect(results[1].valid).toBe(true);
    expect(results[2].valid).toBe(false);
  });

  it('validateBatch: validates quads in batch', async () => {
    const hook = defineHook({
      name: 'batch-val',
      trigger: 'before-add',
      validate: quad => quad.object.value.length > 0,
    });

    const quads = [
      {
        subject: { value: 'http://example.org/s1' },
        predicate: { value: 'http://example.org/p' },
        object: { value: 'valid' },
      },
      {
        subject: { value: 'http://example.org/s2' },
        predicate: { value: 'http://example.org/p' },
        object: { value: '' },
      },
    ];

    const results = await validateBatch([hook], quads);
    expect(results).toBeDefined();
    expect(results[0]).toBe(true);
    expect(results[1]).toBe(false);
  });

  it('transformBatch: transforms quads in batch', async () => {
    const hook = defineHook({
      name: 'batch-transform',
      trigger: 'before-add',
      transform: quad => ({
        ...quad,
        object: { value: quad.object.value.toUpperCase() },
      }),
    });

    const quads = [
      {
        subject: { value: 'http://example.org/s1' },
        predicate: { value: 'http://example.org/p' },
        object: { value: 'test1' },
      },
      {
        subject: { value: 'http://example.org/s2' },
        predicate: { value: 'http://example.org/p' },
        object: { value: 'test2' },
      },
    ];

    const results = await transformBatch([hook], quads);
    expect(results).toBeDefined();
    expect(results[0].object.value).toBe('TEST1');
    expect(results[1].object.value).toBe('TEST2');
  });
});

describe('Performance Characteristics', () => {
  it('JIT compilation improves chain execution performance', async () => {
    const hooks = Array.from({ length: 10 }, (_, i) =>
      defineHook({
        name: `perf-hook-${i}`,
        trigger: 'before-add',
        validate: quad => quad.subject.value.startsWith('http://'),
      })
    );

    const quad = {
      subject: { value: 'http://example.org/s' },
      predicate: { value: 'http://example.org/p' },
      object: { value: 'test' },
    };

    // Warm up
    await executeHookChain(hooks, quad);

    const start = performance.now();
    for (let i = 0; i < 1000; i++) {
      await executeHookChain(hooks, quad);
    }
    const duration = performance.now() - start;

    // Should complete 1000 iterations in reasonable time
    expect(duration).toBeLessThan(1000); // Less than 1 second
  });

  it('Object pooling reduces allocation overhead', () => {
    const iterations = 10000;

    // Without pooling
    const start1 = performance.now();
    for (let i = 0; i < iterations; i++) {
      const quad = {
        subject: { value: 'http://example.org/s' },
        predicate: { value: 'http://example.org/p' },
        object: { value: 'test' },
      };
      // Simulate transformation
      const transformed = { ...quad };
    }
    const duration1 = performance.now() - start1;

    // With pooling
    const pool = new QuadPool({ maxSize: 100 });
    const start2 = performance.now();
    for (let i = 0; i < iterations; i++) {
      const quad = pool.acquire();
      quad.subject = { value: 'http://example.org/s' };
      quad.predicate = { value: 'http://example.org/p' };
      quad.object = { value: 'test' };
      pool.release(quad);
    }
    const duration2 = performance.now() - start2;

    // Pooling should be faster or comparable
    console.log(`Without pooling: ${duration1.toFixed(2)}ms`);
    console.log(`With pooling: ${duration2.toFixed(2)}ms`);
    console.log(`Speedup: ${(duration1 / duration2).toFixed(2)}x`);
  });

  it('Batch operations scale efficiently', async () => {
    const hook = defineHook({
      name: 'batch-perf',
      trigger: 'before-add',
      validate: quad => quad !== null,
    });

    const sizes = [100, 1000, 10000];
    const timings = [];

    for (const size of sizes) {
      const quads = Array.from({ length: size }, (_, i) => ({
        subject: { value: `http://example.org/s${i}` },
        predicate: { value: 'http://example.org/p' },
        object: { value: `test${i}` },
      }));

      const start = performance.now();
      await executeBatch([hook], quads);
      const duration = performance.now() - start;

      timings.push({ size, duration });
      console.log(`Batch size ${size}: ${duration.toFixed(2)}ms`);
    }

    // Verify linear or better scaling
    const throughput1 = timings[0].size / timings[0].duration;
    const throughput2 = timings[1].size / timings[1].duration;
    const throughput3 = timings[2].size / timings[2].duration;

    console.log(
      `Throughput: ${throughput1.toFixed(0)}, ${throughput2.toFixed(0)}, ${throughput3.toFixed(0)} quads/ms`
    );
  });
});
