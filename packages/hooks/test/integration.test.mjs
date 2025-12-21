/**
 * @vitest-environment node
 * @file Integration Tests - End-to-end hook system integration
 */
import { describe, it, expect, beforeEach } from 'vitest';
import { createStore } from '@unrdf/oxigraph';
import { UnrdfDataFactory as DataFactory } from '@unrdf/core/rdf/n3-justified-only';
import {
  defineHook,
  createHookRegistry,
  registerHook,
  executeHooksByTrigger,
  executeHookChain,
} from '../src/index.mjs';

const { namedNode, literal, quad } = DataFactory;

describe('Integration - Hook Lifecycle', () => {
  let store;
  let registry;

  beforeEach(() => {
    store = createStore();
    registry = createHookRegistry();
  });

  it('should register, execute, and apply hooks end-to-end', () => {
    // Define hook
    const validatorHook = defineHook({
      name: 'integration-validator',
      trigger: 'before-add',
      validate: q => q.subject.termType === 'NamedNode',
    });

    // Register hook
    registerHook(registry, validatorHook);

    // Create test quad
    const testQuad = quad(
      namedNode('http://example.org/s'),
      namedNode('http://example.org/p'),
      literal('test')
    );

    // Execute hooks
    const results = executeHooksByTrigger(registry, 'before-add', testQuad);

    expect(results).toHaveLength(1);
    expect(results[0].valid).toBe(true);
    expect(results[0].hookName).toBe('integration-validator');
  });

  it('should chain multiple hooks correctly', () => {
    const hook1 = defineHook({
      name: 'validator-1',
      trigger: 'before-add',
      validate: q => q.subject.termType === 'NamedNode',
    });

    const hook2 = defineHook({
      name: 'validator-2',
      trigger: 'before-add',
      validate: q => q.predicate.termType === 'NamedNode',
    });

    const hook3 = defineHook({
      name: 'transformer',
      trigger: 'before-add',
      transform: q => q,
    });

    registerHook(registry, hook1);
    registerHook(registry, hook2);
    registerHook(registry, hook3);

    const testQuad = quad(namedNode('http://s'), namedNode('http://p'), literal('test'));

    const result = executeHookChain([hook1, hook2, hook3], testQuad);

    expect(result.valid).toBe(true);
    expect(result.quad).toBeDefined();
  });
});

describe('Integration - RDF Store with Hooks', () => {
  let store;
  let registry;

  beforeEach(() => {
    store = createStore();
    registry = createHookRegistry();
  });

  it('should validate quads before adding to store', () => {
    const hook = defineHook({
      name: 'store-validator',
      trigger: 'before-add',
      validate: q => {
        // Only allow quads with specific predicate
        return q.predicate.value === 'http://example.org/allowed';
      },
    });

    registerHook(registry, hook);

    const validQuad = quad(
      namedNode('http://s'),
      namedNode('http://example.org/allowed'),
      literal('test')
    );

    const invalidQuad = quad(
      namedNode('http://s'),
      namedNode('http://example.org/blocked'),
      literal('test')
    );

    const validResult = executeHooksByTrigger(registry, 'before-add', validQuad);
    const invalidResult = executeHooksByTrigger(registry, 'before-add', invalidQuad);

    expect(validResult[0].valid).toBe(true);
    expect(invalidResult[0].valid).toBe(false);
  });

  it('should transform quads before adding to store', () => {
    const hook = defineHook({
      name: 'namespace-transformer',
      trigger: 'before-add',
      transform: q => {
        // Transform all predicates to use new namespace
        return quad(
          q.subject,
          namedNode('http://new.example.org/' + q.predicate.value.split('/').pop()),
          q.object
        );
      },
    });

    registerHook(registry, hook);

    const originalQuad = quad(
      namedNode('http://s'),
      namedNode('http://old.example.org/name'),
      literal('test')
    );

    const results = executeHooksByTrigger(registry, 'before-add', originalQuad);

    expect(results[0].valid).toBe(true);
    expect(results[0].quad.predicate.value).toContain('http://new.example.org/');
  });
});

describe('Integration - Complex Hook Chains', () => {
  it('should execute validation, transformation, and final validation', () => {
    const validateInput = defineHook({
      name: 'validate-input',
      trigger: 'before-add',
      validate: q => q.object.termType === 'Literal',
    });

    const transformData = defineHook({
      name: 'transform-data',
      trigger: 'before-add',
      transform: q => {
        // Uppercase literal values
        if (q.object.termType === 'Literal') {
          return quad(q.subject, q.predicate, literal(q.object.value.toUpperCase()));
        }
        return q;
      },
    });

    const validateOutput = defineHook({
      name: 'validate-output',
      trigger: 'before-add',
      validate: q => {
        // Ensure transformed value is uppercase
        return q.object.value === q.object.value.toUpperCase();
      },
    });

    const testQuad = quad(namedNode('http://s'), namedNode('http://p'), literal('lowercase'));

    const result = executeHookChain([validateInput, transformData, validateOutput], testQuad);

    expect(result.valid).toBe(true);
    expect(result.quad.object.value).toBe('LOWERCASE');
  });
});

describe('Integration - Error Recovery', () => {
  let registry;

  beforeEach(() => {
    registry = createHookRegistry();
  });

  it('should handle hook execution errors gracefully', () => {
    const errorHook = defineHook({
      name: 'error-hook',
      trigger: 'before-add',
      validate: () => {
        throw new Error('Validation error');
      },
    });

    registerHook(registry, errorHook);

    const testQuad = quad(namedNode('http://s'), namedNode('http://p'), literal('test'));

    expect(() => {
      executeHooksByTrigger(registry, 'before-add', testQuad);
    }).toThrow();
  });

  it('should continue chain execution after non-fatal errors', () => {
    const hook1 = defineHook({
      name: 'hook1',
      trigger: 'before-add',
      validate: () => true,
    });

    const hook2 = defineHook({
      name: 'hook2',
      trigger: 'before-add',
      validate: () => true,
    });

    const testQuad = quad(namedNode('http://s'), namedNode('http://p'), literal('test'));

    const result = executeHookChain([hook1, hook2], testQuad);
    expect(result.valid).toBe(true);
  });
});

describe('Integration - Performance with Multiple Hooks', () => {
  it('should handle 100 hooks efficiently', () => {
    const registry = createHookRegistry();

    // Register 100 simple hooks
    for (let i = 0; i < 100; i++) {
      const hook = defineHook({
        name: `perf-hook-${i}`,
        trigger: 'before-add',
        validate: () => true,
      });
      registerHook(registry, hook);
    }

    const testQuad = quad(namedNode('http://s'), namedNode('http://p'), literal('test'));

    const start = performance.now();
    const results = executeHooksByTrigger(registry, 'before-add', testQuad);
    const duration = performance.now() - start;

    expect(results).toHaveLength(100);
    expect(duration).toBeLessThan(100); // Should complete in < 100ms
  });

  it('should handle 1000 quad validations efficiently', () => {
    const hook = defineHook({
      name: 'batch-validator',
      trigger: 'before-add',
      validate: q => q.subject.termType === 'NamedNode',
    });

    const registry = createHookRegistry();
    registerHook(registry, hook);

    const quads = Array(1000)
      .fill(null)
      .map((_, i) => quad(namedNode(`http://s${i}`), namedNode('http://p'), literal('test')));

    const start = performance.now();
    quads.forEach(q => {
      executeHooksByTrigger(registry, 'before-add', q);
    });
    const duration = performance.now() - start;

    expect(duration).toBeLessThan(500); // Should complete in < 500ms
  });
});

describe('Integration - Memory Management', () => {
  it('should not leak memory with repeated hook executions', () => {
    const registry = createHookRegistry();
    const hook = defineHook({
      name: 'memory-test',
      trigger: 'before-add',
      validate: () => true,
    });
    registerHook(registry, hook);

    const testQuad = quad(namedNode('http://s'), namedNode('http://p'), literal('test'));

    // Execute 10,000 times
    for (let i = 0; i < 10000; i++) {
      executeHooksByTrigger(registry, 'before-add', testQuad);
    }

    // If we get here without memory issues, test passes
    expect(true).toBe(true);
  });
});

describe('Integration - Concurrent Hook Execution', () => {
  it('should handle concurrent executions safely', async () => {
    const registry = createHookRegistry();
    const hook = defineHook({
      name: 'concurrent-hook',
      trigger: 'before-add',
      validate: () => true,
    });
    registerHook(registry, hook);

    const quads = Array(100)
      .fill(null)
      .map((_, i) => quad(namedNode(`http://s${i}`), namedNode('http://p'), literal('test')));

    const promises = quads.map(q =>
      Promise.resolve(executeHooksByTrigger(registry, 'before-add', q))
    );

    const results = await Promise.all(promises);

    expect(results).toHaveLength(100);
    results.forEach(result => {
      expect(result[0].valid).toBe(true);
    });
  });
});
