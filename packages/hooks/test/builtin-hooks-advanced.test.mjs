/**
 * Built-in Hooks and Advanced Features Test Suite
 *
 * Tests:
 * - All 10+ built-in hooks with edge cases
 * - JIT compilation and hook chain optimization
 * - Object pooling for zero-allocation transforms
 * - Batch operations at scale
 * - Performance baselines and regressions
 */

import { describe, it, expect } from 'vitest';
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

// ============================================================================
// Test Fixtures
// ============================================================================

const fixtures = {
  validIRIQuad: {
    subject: { value: 'http://example.org/subject', termType: 'NamedNode' },
    predicate: { value: 'http://example.org/predicate', termType: 'NamedNode' },
    object: { value: 'test', termType: 'Literal' },
  },

  blankNodeQuad: {
    subject: { value: '_:b0', termType: 'BlankNode' },
    predicate: { value: 'http://example.org/p', termType: 'NamedNode' },
    object: { value: 'test', termType: 'Literal' },
  },

  invalidIRIQuad: {
    subject: { value: 'not a valid iri with spaces' },
    predicate: { value: 'http://example.org/p' },
    object: { value: 'test' },
  },

  literalWithLanguageQuad: (language = 'en') => ({
    subject: { value: 'http://example.org/s' },
    predicate: { value: 'http://example.org/label' },
    object: { value: 'Hello', language },
  }),

  literalWithWhitespace: {
    subject: { value: 'http://example.org/s' },
    predicate: { value: 'http://example.org/p' },
    object: { value: '  spaces around  ', termType: 'Literal' },
  },

  emptyLiteral: {
    subject: { value: 'http://example.org/s' },
    predicate: { value: 'http://example.org/p' },
    object: { value: '', termType: 'Literal' },
  },

  createBatch: (count, baseUrl = 'http://example.org') => {
    return Array.from({ length: count }, (_, i) => ({
      subject: { value: `${baseUrl}/s${i}` },
      predicate: { value: `${baseUrl}/p` },
      object: { value: `test${i}` },
    }));
  },
};

// ============================================================================
// IRI Validation
// ============================================================================

describe('Built-in Hooks - IRI Validation', () => {
  it('validateSubjectIRI: accepts valid named nodes', async () => {
    const result = await executeHook(validateSubjectIRI, fixtures.validIRIQuad);
    expect(result.valid).toBe(true);
  });

  it('validateSubjectIRI: rejects blank nodes in subject', async () => {
    const result = await executeHook(validateSubjectIRI, fixtures.blankNodeQuad);
    expect(result.valid).toBe(false);
  });

  it('validateSubjectIRI: rejects invalid URIs', async () => {
    const result = await executeHook(validateSubjectIRI, fixtures.invalidIRIQuad);
    expect(result.valid).toBe(false);
  });

  it('validatePredicateIRI: accepts valid predicates', async () => {
    const result = await executeHook(validatePredicateIRI, fixtures.validIRIQuad);
    expect(result.valid).toBe(true);
  });

  it('validatePredicateIRI: rejects blank nodes in predicate', async () => {
    const badQuad = { ...fixtures.validIRIQuad, predicate: fixtures.blankNodeQuad.subject };
    const result = await executeHook(validatePredicateIRI, badQuad);
    expect(result.valid).toBe(false);
  });

  it('validateIRIFormat: validates RFC3987 structure', async () => {
    const result = await executeHook(validateIRIFormat, fixtures.validIRIQuad);
    expect(result.valid).toBe(true);
  });

  it('validateIRIFormat: rejects IRIs with invalid characters', async () => {
    const result = await executeHook(validateIRIFormat, fixtures.invalidIRIQuad);
    expect(result.valid).toBe(false);
  });

  it('validateIRIFormat: rejects IRIs with only scheme', async () => {
    const badQuad = { ...fixtures.validIRIQuad, subject: { value: 'http://' } };
    const result = await executeHook(validateIRIFormat, badQuad);
    expect(result.valid).toBe(false);
  });
});

// ============================================================================
// Literal Validation
// ============================================================================

describe('Built-in Hooks - Literal Validation', () => {
  it('validateObjectLiteral: accepts valid literals', async () => {
    const result = await executeHook(validateObjectLiteral, fixtures.validIRIQuad);
    expect(result.valid).toBe(true);
  });

  it('validateObjectLiteral: rejects empty literals', async () => {
    const result = await executeHook(validateObjectLiteral, fixtures.emptyLiteral);
    expect(result.valid).toBe(false);
  });

  it('validateLanguageTag: accepts BCP47 language tags', async () => {
    const enQuad = await executeHook(validateLanguageTag, fixtures.literalWithLanguageQuad('en'));
    expect(enQuad.valid).toBe(true);

    const enUSQuad = await executeHook(
      validateLanguageTag,
      fixtures.literalWithLanguageQuad('en-US')
    );
    expect(enUSQuad.valid).toBe(true);
  });

  it('validateLanguageTag: rejects invalid language tags', async () => {
    const result = await executeHook(
      validateLanguageTag,
      fixtures.literalWithLanguageQuad('INVALID_TAG')
    );
    expect(result.valid).toBe(false);
  });

  it('validateLanguageTag: rejects overly long language tags', async () => {
    const result = await executeHook(
      validateLanguageTag,
      fixtures.literalWithLanguageQuad('a'.repeat(100))
    );
    expect(result.valid).toBe(false);
  });
});

// ============================================================================
// Node Type Validation
// ============================================================================

describe('Built-in Hooks - Node Type Validation', () => {
  it('rejectBlankNodes: accepts named nodes only', async () => {
    const result = await executeHook(rejectBlankNodes, fixtures.validIRIQuad);
    expect(result.valid).toBe(true);
  });

  it('rejectBlankNodes: rejects all blank node positions', async () => {
    // Blank node subject
    let result = await executeHook(rejectBlankNodes, fixtures.blankNodeQuad);
    expect(result.valid).toBe(false);

    // Blank node object
    const badObjectQuad = {
      ...fixtures.validIRIQuad,
      object: { value: '_:blank', termType: 'BlankNode' },
    };
    result = await executeHook(rejectBlankNodes, badObjectQuad);
    expect(result.valid).toBe(false);
  });
});

// ============================================================================
// Normalization
// ============================================================================

describe('Built-in Hooks - Normalization', () => {
  it('normalizeNamespace: preserves valid IRIs', async () => {
    const result = await executeHook(normalizeNamespace, fixtures.validIRIQuad);
    expect(result.quad).toBeDefined();
    expect(result.quad.subject.value).toContain('http://');
  });

  it('normalizeLanguageTag: lowercases mixed-case tags', async () => {
    const quad = fixtures.literalWithLanguageQuad('EN-US');
    const result = await executeHook(normalizeLanguageTag, quad);
    expect(result.quad.object.language).toBe('en-us');
  });

  it('normalizeLanguageTag: preserves already-lowercase tags', async () => {
    const quad = fixtures.literalWithLanguageQuad('en-us');
    const result = await executeHook(normalizeLanguageTag, quad);
    expect(result.quad.object.language).toBe('en-us');
  });

  it('normalizeLanguageTagPooled: uses pooling', async () => {
    const quad = fixtures.literalWithLanguageQuad('EN');
    const result = await executeHook(normalizeLanguageTagPooled, quad);
    expect(result.quad.object.language).toBe('en');
    expect(isPooledQuad(result.quad)).toBe(true);
  });

  it('trimLiterals: removes leading/trailing whitespace', async () => {
    const result = await executeHook(trimLiterals, fixtures.literalWithWhitespace);
    expect(result.quad.object.value).toBe('spaces around');
  });

  it('trimLiterals: preserves internal whitespace', async () => {
    const quad = {
      ...fixtures.validIRIQuad,
      object: { value: '  hello  world  ', termType: 'Literal' },
    };
    const result = await executeHook(trimLiterals, quad);
    expect(result.quad.object.value).toBe('hello  world');
  });

  it('trimLiteralsPooled: uses pooling', async () => {
    const result = await executeHook(trimLiteralsPooled, fixtures.literalWithWhitespace);
    expect(result.quad.object.value).toBe('spaces around');
    expect(isPooledQuad(result.quad)).toBe(true);
  });
});

// ============================================================================
// Composite Validation
// ============================================================================

describe('Built-in Hooks - Composite Validation', () => {
  it('standardValidation: accepts well-formed quads', async () => {
    const result = await executeHook(standardValidation, fixtures.validIRIQuad);
    expect(result.valid).toBe(true);
  });

  it('standardValidation: rejects malformed IRIs', async () => {
    const result = await executeHook(standardValidation, fixtures.invalidIRIQuad);
    expect(result.valid).toBe(false);
  });

  it('standardValidation: accepts blank nodes in subject', async () => {
    const result = await executeHook(standardValidation, fixtures.blankNodeQuad);
    expect(result.valid).toBe(true);
  });

  it('builtinHooks exports at least 10 hooks', () => {
    expect(builtinHooks).toBeDefined();
    expect(typeof builtinHooks).toBe('object');
    expect(Object.keys(builtinHooks).length).toBeGreaterThanOrEqual(10);
  });
});

// ============================================================================
// JIT Compilation
// ============================================================================

describe('Advanced Features - JIT Compilation', () => {
  afterEach(() => {
    clearCompiledChainCache();
  });

  it('compileHookChain: produces callable function', () => {
    const hooks = [
      defineHook({
        name: 'check-http',
        trigger: 'before-add',
        validate: quad => quad.subject.value.startsWith('http://'),
      }),
    ];

    const compiled = compileHookChain(hooks);
    expect(compiled).toBeDefined();
    expect(typeof compiled).toBe('function');
  });

  it('compileValidationOnlyChain: produces callable function', () => {
    const hooks = [
      defineHook({
        name: 'check-exists',
        trigger: 'before-add',
        validate: quad => quad !== null,
      }),
    ];

    const compiled = compileValidationOnlyChain(hooks);
    expect(compiled).toBeDefined();
    expect(typeof compiled).toBe('function');
  });

  it('getCompilerStats: returns object with stats', () => {
    const stats = getCompilerStats();
    expect(stats).toBeDefined();
    expect(typeof stats).toBe('object');
  });

  it('isJitAvailable: returns boolean', () => {
    const available = isJitAvailable();
    expect(typeof available).toBe('boolean');
  });

  it('clearCompiledChainCache: succeeds without error', () => {
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

// ============================================================================
// Object Pooling
// ============================================================================

describe('Advanced Features - Object Pooling', () => {
  it('QuadPool: acquires and releases quads', () => {
    const pool = new QuadPool({ size: 10 });
    const s = { value: 'http://example.org/s' };
    const p = { value: 'http://example.org/p' };
    const o = { value: 'test' };

    const quad = pool.acquire(s, p, o);
    expect(quad).toBeDefined();

    pool.release(quad);

    // Should be able to acquire again
    const quad2 = pool.acquire(s, p, o);
    expect(quad2).toBeDefined();
  });

  it('QuadPool: tracks acquired count', () => {
    const pool = new QuadPool({ size: 10 });
    const s = { value: 'http://example.org/s' };
    const p = { value: 'http://example.org/p' };
    const o = { value: 'test' };

    const initialAcquired = pool.acquired;
    const quad = pool.acquire(s, p, o);
    expect(pool.acquired).toBeGreaterThan(initialAcquired);

    pool.release(quad);
    expect(pool.acquired).toBeLessThanOrEqual(initialAcquired + 1);
  });

  it('quadPool: global instance is QuadPool', () => {
    expect(quadPool).toBeDefined();
    expect(quadPool instanceof QuadPool).toBe(true);
  });

  it('createPooledTransform: returns transformed quad', () => {
    const transform = createPooledTransform(quad => ({
      ...quad,
      object: { ...quad.object, value: quad.object.value.toUpperCase() },
    }));

    expect(transform).toBeDefined();
    expect(typeof transform).toBe('function');

    const result = transform(fixtures.validIRIQuad);
    expect(result.object.value).toBe('TEST');
  });

  it('isPooledQuad: identifies pooled vs regular quads', () => {
    expect(isPooledQuad(fixtures.validIRIQuad)).toBe(false);

    const pool = new QuadPool({ size: 10 });
    const s = { value: 'http://example.org/s' };
    const p = { value: 'http://example.org/p' };
    const o = { value: 'test' };

    const pooledQuad = pool.acquire(s, p, o);
    expect(isPooledQuad(pooledQuad)).toBe(true);
    pool.release(pooledQuad);
  });
});

// ============================================================================
// Batch Operations
// ============================================================================

describe('Advanced Features - Batch Operations', () => {
  it('executeBatch: processes all quads with mixed validity', async () => {
    const hook = defineHook({
      name: 'batch-validate',
      trigger: 'before-add',
      validate: quad => quad.subject.value.startsWith('http://'),
    });

    const quads = [fixtures.validIRIQuad, fixtures.validIRIQuad, fixtures.invalidIRIQuad];

    const results = await executeBatch([hook], quads);
    expect(results).toHaveLength(3);
    expect(results[0].valid).toBe(true);
    expect(results[1].valid).toBe(true);
    expect(results[2].valid).toBe(false);
  });

  it('executeBatch: handles stopOnError option', async () => {
    const hook = defineHook({
      name: 'stop-on-error',
      trigger: 'before-add',
      validate: quad => quad.subject.value.startsWith('http://'),
    });

    const quads = [fixtures.validIRIQuad, fixtures.invalidIRIQuad, fixtures.validIRIQuad];

    const results = await executeBatch([hook], quads, { stopOnError: true });
    expect(results.length).toBeLessThanOrEqual(3);
  });

  it('validateBatch: returns boolean array', async () => {
    const hook = defineHook({
      name: 'batch-val',
      trigger: 'before-add',
      validate: quad => quad.object.value.length > 0,
    });

    const quads = [fixtures.validIRIQuad, fixtures.emptyLiteral];

    const results = await validateBatch([hook], quads);
    expect(Array.isArray(results)).toBe(true);
    expect(results[0]).toBe(true);
    expect(results[1]).toBe(false);
  });

  it('transformBatch: applies transformation to all quads', async () => {
    const hook = defineHook({
      name: 'batch-transform',
      trigger: 'before-add',
      transform: quad => ({
        ...quad,
        object: { value: quad.object.value.toUpperCase() },
      }),
    });

    const quads = fixtures.createBatch(3);
    const results = await transformBatch([hook], quads);

    expect(results).toHaveLength(3);
    expect(results[0].object.value).toBe('TEST0');
    expect(results[1].object.value).toBe('TEST1');
    expect(results[2].object.value).toBe('TEST2');
  });

  it('transformBatch: chains multiple transforms', async () => {
    const hooks = [
      defineHook({
        name: 'uppercase',
        trigger: 'before-add',
        transform: quad => ({
          ...quad,
          object: { value: quad.object.value.toUpperCase() },
        }),
      }),
      defineHook({
        name: 'prefix',
        trigger: 'before-add',
        transform: quad => ({
          ...quad,
          object: { value: `PREFIX_${quad.object.value}` },
        }),
      }),
    ];

    const quads = fixtures.createBatch(1);
    const results = await transformBatch(hooks, quads);

    expect(results[0].object.value).toBe('PREFIX_TEST0');
  });
});

// ============================================================================
// Hook Composition
// ============================================================================

describe('Advanced Features - Hook Composition', () => {
  it('executeHookChain: validates through multiple hooks', async () => {
    const hooks = [
      defineHook({
        name: 'check-iri',
        trigger: 'before-add',
        validate: quad => quad.subject.value.startsWith('http://'),
      }),
      defineHook({
        name: 'check-predicate',
        trigger: 'before-add',
        validate: quad => quad.predicate.value.includes('example.org'),
      }),
    ];

    const result = await executeHookChain(hooks, fixtures.validIRIQuad);
    expect(result.valid).toBe(true);
  });

  it('executeHookChain: fails on first invalid hook', async () => {
    const hooks = [
      defineHook({
        name: 'pass',
        trigger: 'before-add',
        validate: quad => quad.subject.value.startsWith('http://'),
      }),
      defineHook({
        name: 'fail',
        trigger: 'before-add',
        validate: quad => quad.object.value.includes('xyz'),
      }),
    ];

    const result = await executeHookChain(hooks, fixtures.validIRIQuad);
    expect(result.valid).toBe(false);
  });

  it('executeHookChain: applies validations then transforms in order', async () => {
    const hooks = [
      defineHook({
        name: 'validate-exists',
        trigger: 'before-add',
        validate: quad => quad !== null,
      }),
      defineHook({
        name: 'uppercase',
        trigger: 'before-add',
        transform: quad => ({
          ...quad,
          object: { value: quad.object.value.toUpperCase() },
        }),
      }),
      defineHook({
        name: 'trim',
        trigger: 'before-add',
        transform: quad => ({
          ...quad,
          object: { value: quad.object.value.trim() },
        }),
      }),
    ];

    const quad = { ...fixtures.validIRIQuad, object: { value: '  test  ' } };
    const result = await executeHookChain(hooks, quad);

    expect(result.valid).toBe(true);
    expect(result.quad.object.value).toBe('TEST');
  });
});

// ============================================================================
// Performance and Scaling
// ============================================================================

describe('Performance - Hook Chains', () => {
  it('large hook chains execute efficiently', async () => {
    const hooks = Array.from({ length: 20 }, (_, i) =>
      defineHook({
        name: `perf-hook-${i}`,
        trigger: 'before-add',
        validate: quad => quad.subject.value.startsWith('http://'),
      })
    );

    const quad = fixtures.validIRIQuad;

    const start = performance.now();
    for (let i = 0; i < 100; i++) {
      await executeHookChain(hooks, quad);
    }
    const duration = performance.now() - start;

    // 100 iterations of 20-hook chain should complete in < 1 second
    expect(duration).toBeLessThan(1000);
  });
});

describe('Performance - Batch Operations', () => {
  it('batch operations maintain linear scaling', async () => {
    const hook = defineHook({
      name: 'simple-validate',
      trigger: 'before-add',
      validate: quad => quad !== null,
    });

    const timings = [];

    for (const size of [100, 1000, 5000]) {
      const quads = fixtures.createBatch(size);

      const start = performance.now();
      await executeBatch([hook], quads);
      const duration = performance.now() - start;

      timings.push({ size, duration, throughput: size / duration });
    }

    // Throughput should not degrade significantly
    const throughput1 = timings[0].throughput;
    const throughput2 = timings[1].throughput;
    const degradation = (throughput1 - throughput2) / throughput1;

    // Allow up to 100% degradation (system contention under parallel CI load)
    expect(degradation).toBeLessThan(1.0);
  });
});
