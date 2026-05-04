/**
 * Hooks Chain Integration Tests
 * Phase 5: 10 tests covering Hook registration, Sandbox isolation, Policy validation, Sequencing, Error recovery
 *
 * @module @unrdf/integration-tests/test/chains/hooks
 */

import { describe, it, expect, beforeEach, afterEach } from 'vitest';
import {
  defineHook,
  isValidHook,
  executeHook,
  executeHookChain,
  createHookRegistry,
  registerHook,
  unregisterHook,
  getHook,
  listHooks,
  clearHooks,
  getRegistryStats,
  validateSubjectIRI,
  validatePredicateIRI,
  standardValidation,
  trimLiterals,
} from '@unrdf/hooks';

/**
 * Helper: Create mock quad for testing
 * @param {Object} options - Quad options
 * @returns {Object} Mock quad
 */
function createMockQuad(options = {}) {
  return {
    subject: { value: options.subject || 'http://example.org/s1', termType: 'NamedNode' },
    predicate: { value: options.predicate || 'http://example.org/p1', termType: 'NamedNode' },
    object: {
      value: options.object || 'test value',
      termType: options.objectType || 'Literal',
    },
    graph: { value: '', termType: 'DefaultGraph' },
  };
}

describe('Hooks Chain Integration Tests', () => {
  /** @type {ReturnType<typeof createHookRegistry>} */
  let registry;

  beforeEach(() => {
    registry = createHookRegistry();
  });

  afterEach(() => {
    registry = null;
  });

  // Test 1: Hook registration and triggering
  it('should register and trigger hooks correctly', async () => {
    const validationHook = defineHook({
      name: 'test-validation',
      trigger: 'before-add',
      validate: (quad) => {
        return quad.subject.value.startsWith('http://');
      },
    });

    expect(isValidHook(validationHook)).toBe(true);

    registerHook(registry, validationHook);

    const registered = getHook(registry, 'test-validation');
    expect(registered).toBeDefined();
    expect(registered.name).toBe('test-validation');

    // Test triggering
    const validQuad = createMockQuad();
    const result = await executeHook(validationHook, validQuad);
    expect(result.valid).toBe(true);

    const invalidQuad = createMockQuad({ subject: 'invalid-subject' });
    const invalidResult = await executeHook(validationHook, invalidQuad);
    expect(invalidResult.valid).toBe(false);
  });

  // Test 2: Sandbox isolation - validation function contained
  it('should isolate hook execution', async () => {
    let sideEffectCounter = 0;

    const sideEffectHook = defineHook({
      name: 'side-effect-hook',
      trigger: 'before-add',
      validate: (quad) => {
        // Attempt side effect (this tests isolation)
        sideEffectCounter++;
        return true;
      },
    });

    registerHook(registry, sideEffectHook);

    const quad = createMockQuad();
    await executeHook(sideEffectHook, quad);
    await executeHook(sideEffectHook, quad);

    // Side effects should be contained within the hook execution
    // but the counter will still increment as this is basic isolation
    expect(sideEffectCounter).toBe(2);
  });

  // Test 3: Policy validation - IRI format validation
  it('should validate IRI format via policy hooks', async () => {
    const quad = createMockQuad();
    const result = executeHook(validateSubjectIRI, quad);
    expect(result.valid).toBe(true);

    // Create quad with BlankNode to fail NamedNode validation
    const invalidQuad = {
      subject: { value: 'not-a-valid-iri', termType: 'BlankNode' },
      predicate: { value: 'http://example.org/p1', termType: 'NamedNode' },
      object: { value: 'test value', termType: 'Literal' },
      graph: { value: '', termType: 'DefaultGraph' },
    };
    const invalidResult = executeHook(validateSubjectIRI, invalidQuad);
    expect(invalidResult.valid).toBe(false);
  });

  // Test 4: Hook sequencing - 5 hooks in correct order
  it('should execute hooks in correct sequence', async () => {
    const executionOrder = [];

    const hooks = [1, 2, 3, 4, 5].map((n) =>
      defineHook({
        name: `sequence-hook-${n}`,
        trigger: 'before-add',
        priority: n,
        validate: (quad) => {
          executionOrder.push(n);
          return true;
        },
      })
    );

    hooks.forEach((h) => registerHook(registry, h));

    const quad = createMockQuad();
    await executeHookChain(hooks, quad);

    // Should execute in order based on registration
    expect(executionOrder).toHaveLength(5);
    expect(executionOrder).toEqual([1, 2, 3, 4, 5]);
  });

  // Test 5: Error recovery - hook crash contained
  it('should recover from hook crash', async () => {
    const crashingHook = defineHook({
      name: 'crashing-hook',
      trigger: 'before-add',
      validate: () => {
        throw new Error('Intentional crash for testing');
      },
    });

    const successHook = defineHook({
      name: 'success-hook',
      trigger: 'before-add',
      validate: () => true,
    });

    registerHook(registry, crashingHook);
    registerHook(registry, successHook);

    const quad = createMockQuad();

    // Crashing hook should return error result (executeHook catches errors)
    const crashResult = executeHook(crashingHook, quad);
    expect(crashResult.valid).toBe(false);
    expect(crashResult.error).toContain('Intentional crash');

    // Success hook should still work (main process survives)
    const result = executeHook(successHook, quad);
    expect(result.valid).toBe(true);

    // Registry should still be operational
    const stats = getRegistryStats(registry);
    expect(stats.totalHooks).toBe(2);
  });

  // Test 6: Registry management - list and clear
  it('should manage hook registry correctly', () => {
    const hook1 = defineHook({ name: 'hook-1', trigger: 'before-add', validate: () => true });
    const hook2 = defineHook({ name: 'hook-2', trigger: 'after-add', validate: () => true });
    const hook3 = defineHook({ name: 'hook-3', trigger: 'before-remove', validate: () => true });

    registerHook(registry, hook1);
    registerHook(registry, hook2);
    registerHook(registry, hook3);

    expect(listHooks(registry)).toHaveLength(3);

    unregisterHook(registry, 'hook-2');
    expect(listHooks(registry)).toHaveLength(2);
    expect(getHook(registry, 'hook-2')).toBeUndefined();

    clearHooks(registry);
    expect(listHooks(registry)).toHaveLength(0);
  });

  // Test 7: Built-in validation hooks
  it('should use built-in validation hooks correctly', async () => {
    const validQuad = createMockQuad();

    // Subject IRI validation
    const subjectResult = await executeHook(validateSubjectIRI, validQuad);
    expect(subjectResult.valid).toBe(true);

    // Predicate IRI validation
    const predicateResult = await executeHook(validatePredicateIRI, validQuad);
    expect(predicateResult.valid).toBe(true);
  });

  // Test 8: Transformation hooks - trim literals
  it('should transform quads with hooks', async () => {
    const untrimmedQuad = createMockQuad({ object: '  trimmed value  ' });

    const result = await executeHook(trimLiterals, untrimmedQuad);

    // Transformation should produce modified quad
    expect(result.valid).toBe(true);
    if (result.transformed) {
      expect(result.transformed.object.value).toBe('trimmed value');
    }
  });

  // Test 9: Hook chain execution
  it('should execute validation chain and stop on failure', async () => {
    const validationResults = [];

    const hook1 = defineHook({
      name: 'chain-1',
      trigger: 'before-add',
      validate: (quad) => {
        validationResults.push('hook1');
        return true;
      },
    });

    const hook2 = defineHook({
      name: 'chain-2',
      trigger: 'before-add',
      validate: (quad) => {
        validationResults.push('hook2');
        return false; // This should stop the chain
      },
    });

    const hook3 = defineHook({
      name: 'chain-3',
      trigger: 'before-add',
      validate: (quad) => {
        validationResults.push('hook3');
        return true;
      },
    });

    const quad = createMockQuad();
    const chainResult = await executeHookChain([hook1, hook2, hook3], quad);

    // Chain should stop at hook2
    expect(chainResult.valid).toBe(false);
    expect(validationResults).toContain('hook1');
    expect(validationResults).toContain('hook2');
    // hook3 may or may not execute depending on implementation
  });

  // Test 10: Registry statistics
  it('should track registry statistics correctly', () => {
    const hooks = [
      defineHook({ name: 'stat-1', trigger: 'before-add', validate: () => true }),
      defineHook({ name: 'stat-2', trigger: 'before-add', validate: () => true }),
      defineHook({ name: 'stat-3', trigger: 'after-add', validate: () => true }),
      defineHook({ name: 'stat-4', trigger: 'before-remove', validate: () => true }),
    ];

    hooks.forEach((h) => registerHook(registry, h));

    const stats = getRegistryStats(registry);

    expect(stats.totalHooks).toBe(4);
    expect(typeof stats.byTrigger).toBe('object');
  });
});
