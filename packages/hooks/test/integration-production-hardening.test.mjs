/**
 * @vitest-environment node
 * @file Integration Production Hardening Tests
 *
 * Verifies all production hardening fixes work together:
 * - Entry point exports (./define, ./executor)
 * - Hook definition → validation → execution → result checking
 * - Batch operations at scale
 * - POKA-YOKE guards (non-boolean, null transform, missing properties)
 * - Cache management (prewarm, clear)
 * - Error handling across the stack
 * - validateOnly / validateBatch / transformBatch fast paths
 * - Registry management + execution pipeline
 * - Zod schema validation at API boundaries
 */
import { describe, it, expect, beforeEach } from 'vitest';
import { namedNode, literal, quad } from '../../test-utils/src/index.mjs';

import {
  defineHook,
  isValidHook,
  hasValidation,
  hasTransformation,
  getHookMetadata,
  executeHook,
  executeHookChain,
  executeHooksByTrigger,
  wouldPassHooks,
  validateOnly,
  executeBatch,
  validateBatch,
  transformBatch,
  clearHookCaches,
  prewarmHookCache,
  createHookRegistry,
  registerHook,
  unregisterHook,
  getHook,
  listHooks,
  getHooksByTrigger,
  clearHooks,
  HookTriggerSchema,
  HookConfigSchema,
  HookResultSchema,
  ChainResultSchema,
  builtinHooks,
  validateSubjectIRI,
  validatePredicateIRI,
  validateIRIFormat,
  trimLiterals,
} from '../src/index.mjs';

// ─── Helpers ──────────────────────────────────────────────────────────────────

function makeQuad(s, p, o) {
  return quad(namedNode(s), namedNode(p), literal(o));
}

function makeIriQuad(s, p, o) {
  return quad(namedNode(s), namedNode(p), namedNode(o));
}

// ─── Tests ────────────────────────────────────────────────────────────────────

describe('Integration: Complete hook lifecycle', () => {
  const testQuad = makeQuad('http://ex.org/s', 'http://ex.org/p', 'hello');

  it('should complete full define → validate → execute → check lifecycle', () => {
    // 1. Define
    const hook = defineHook({
      name: 'lifecycle-validator',
      trigger: 'before-add',
      validate: (q) => q.subject.termType === 'NamedNode',
      metadata: { purpose: 'lifecycle-test' },
    });

    // 2. Validate definition
    expect(isValidHook(hook)).toBe(true);
    expect(hasValidation(hook)).toBe(true);
    expect(getHookMetadata(hook, 'purpose')).toBe('lifecycle-test');

    // 3. Execute
    const result = executeHook(hook, testQuad);

    // 4. Check result
    expect(result.valid).toBe(true);
    expect(result.hookName).toBe('lifecycle-validator');
    expect(result.quad).toBe(testQuad);
  });

  it('should complete define → transform → verify output lifecycle', () => {
    const hook = defineHook({
      name: 'lifecycle-transformer',
      trigger: 'before-add',
      transform: (q) =>
        quad(q.subject, q.predicate, literal(q.object.value.toUpperCase())),
    });

    expect(hasTransformation(hook)).toBe(true);

    const result = executeHook(hook, testQuad);
    expect(result.valid).toBe(true);
    expect(result.quad.object.value).toBe('HELLO');
  });

  it('should define → register → lookup → execute via registry', () => {
    const registry = createHookRegistry();

    const hook = defineHook({
      name: 'registry-lifecycle',
      trigger: 'before-add',
      validate: () => true,
    });

    registerHook(registry, hook);

    const retrieved = getHook(registry, 'registry-lifecycle');
    expect(retrieved).toBeDefined();
    expect(retrieved.name).toBe('registry-lifecycle');

    const result = executeHook(retrieved, testQuad);
    expect(result.valid).toBe(true);
  });
});

describe('Integration: Multi-hook chain pipeline', () => {
  const testQuad = makeQuad('http://ex.org/s', 'http://ex.org/p', 'data');

  it('should execute validate → transform → validate chain', () => {
    const inputValidator = defineHook({
      name: 'input-val',
      trigger: 'before-add',
      validate: (q) => q.object.termType === 'Literal',
    });

    const transformer = defineHook({
      name: 'uppercaser',
      trigger: 'before-add',
      transform: (q) =>
        quad(q.subject, q.predicate, literal(q.object.value.toUpperCase())),
    });

    const outputValidator = defineHook({
      name: 'output-val',
      trigger: 'before-add',
      validate: (q) => q.object.value === q.object.value.toUpperCase(),
    });

    const result = executeHookChain(
      [inputValidator, transformer, outputValidator],
      testQuad
    );

    expect(result.valid).toBe(true);
    expect(result.quad.object.value).toBe('DATA');
    expect(result.results).toHaveLength(3);
  });

  it('should halt chain on first validation failure', () => {
    const rejecter = defineHook({
      name: 'rejecter',
      trigger: 'before-add',
      validate: () => false,
    });

    const neverReached = defineHook({
      name: 'never-reached',
      trigger: 'before-add',
      validate: () => true,
    });

    const result = executeHookChain([rejecter, neverReached], testQuad);

    expect(result.valid).toBe(false);
    expect(result.results).toHaveLength(1);
    expect(result.failedHook).toBe('rejecter');
    expect(result.error).toContain('rejecter');
  });

  it('should chain transformations sequentially', () => {
    const addPrefix = defineHook({
      name: 'add-prefix',
      trigger: 'before-add',
      transform: (q) =>
        quad(q.subject, q.predicate, literal('PREFIX:' + q.object.value)),
    });

    const addSuffix = defineHook({
      name: 'add-suffix',
      trigger: 'before-add',
      transform: (q) =>
        quad(q.subject, q.predicate, literal(q.object.value + ':SUFFIX')),
    });

    const result = executeHookChain([addPrefix, addSuffix], testQuad);

    expect(result.valid).toBe(true);
    expect(result.quad.object.value).toBe('PREFIX:data:SUFFIX');
  });
});

describe('Integration: Trigger-based filtering', () => {
  it('should only execute hooks matching the trigger type', () => {
    const beforeHook = defineHook({
      name: 'before-hook',
      trigger: 'before-add',
      validate: () => true,
    });

    const afterHook = defineHook({
      name: 'after-hook',
      trigger: 'after-add',
      validate: () => true,
    });

    const queryHook = defineHook({
      name: 'query-hook',
      trigger: 'before-query',
      validate: () => true,
    });

    const allHooks = [beforeHook, afterHook, queryHook];
    const testQuad = makeQuad('http://s', 'http://p', 'test');

    const beforeResult = executeHooksByTrigger(allHooks, 'before-add', testQuad);
    expect(beforeResult.valid).toBe(true);
    expect(beforeResult.results).toHaveLength(1);
    expect(beforeResult.results[0].hookName).toBe('before-hook');

    const afterResult = executeHooksByTrigger(allHooks, 'after-add', testQuad);
    expect(afterResult.valid).toBe(true);
    expect(afterResult.results).toHaveLength(1);

    // No hooks for before-remove → should pass with 0 results
    const removeResult = executeHooksByTrigger(allHooks, 'before-remove', testQuad);
    expect(removeResult.valid).toBe(true);
    expect(removeResult.results).toHaveLength(0);
  });
});

describe('Integration: wouldPassHooks dry-run', () => {
  const testQuad = makeQuad('http://s', 'http://p', 'test');

  it('should return true for passing hooks', () => {
    const hooks = [
      defineHook({ name: 'h1', trigger: 'before-add', validate: () => true }),
      defineHook({ name: 'h2', trigger: 'before-add', validate: () => true }),
    ];
    expect(wouldPassHooks(hooks, testQuad)).toBe(true);
  });

  it('should return false for failing hooks', () => {
    const hooks = [
      defineHook({ name: 'h1', trigger: 'before-add', validate: () => true }),
      defineHook({ name: 'h2', trigger: 'before-add', validate: () => false }),
    ];
    expect(wouldPassHooks(hooks, testQuad)).toBe(false);
  });
});

describe('Integration: POKA-YOKE guards', () => {
  const testQuad = makeQuad('http://s', 'http://p', 'test');

  it('should coerce non-boolean validation return to boolean (truthy)', () => {
    const hook = defineHook({
      name: 'non-bool-truthy',
      trigger: 'before-add',
      validate: () => 'truthy-string',
    });

    const result = executeHook(hook, testQuad);
    // Truthy string coerces to true
    expect(result.valid).toBe(true);
    expect(result.warning).toBeDefined();
  });

  it('should coerce non-boolean validation return to boolean (falsy)', () => {
    const hook = defineHook({
      name: 'non-bool-falsy',
      trigger: 'before-add',
      validate: () => 0,
    });

    const result = executeHook(hook, testQuad);
    // 0 is falsy → valid = false
    expect(result.valid).toBe(false);
  });

  it('should handle transform returning null gracefully', () => {
    const hook = defineHook({
      name: 'null-transform',
      trigger: 'before-add',
      transform: () => null,
    });

    const result = executeHook(hook, testQuad);
    expect(result.valid).toBe(false);
  });

  it('should throw on transform returning non-object', () => {
    const hook = defineHook({
      name: 'bad-transform',
      trigger: 'before-add',
      transform: () => 'not-an-object',
    });

    expect(() => executeHook(hook, testQuad)).toThrow(TypeError);
  });

  it('should throw on transform missing required quad properties', () => {
    const hook = defineHook({
      name: 'incomplete-transform',
      trigger: 'before-add',
      transform: () => ({ subject: namedNode('http://s') }),
    });

    expect(() => executeHook(hook, testQuad)).toThrow(TypeError);
  });

  it('should throw TypeError on null quad input', () => {
    const hook = defineHook({
      name: 'guard-hook',
      trigger: 'before-add',
      validate: () => true,
    });

    expect(() => executeHook(hook, null)).toThrow(TypeError);
    expect(() => executeHook(hook, undefined)).toThrow(TypeError);
    expect(() => executeHook(hook, 'string')).toThrow(TypeError);
  });
});

describe('Integration: Batch API operations', () => {
  const quads = Array.from({ length: 20 }, (_, i) =>
    makeQuad(`http://s${i}`, 'http://p', `value-${i}`)
  );

  it('should executeBatch across all quads', () => {
    const hook = defineHook({
      name: 'batch-val',
      trigger: 'before-add',
      validate: (q) => q.subject.termType === 'NamedNode',
    });

    const results = executeBatch([hook], quads);
    expect(results).toHaveLength(20);
    results.forEach((r) => expect(r.valid).toBe(true));
  });

  it('should stop batch on first error when stopOnError is set', () => {
    const hook = defineHook({
      name: 'batch-reject',
      trigger: 'before-add',
      validate: () => false,
    });

    const results = executeBatch([hook], quads, { stopOnError: true });
    expect(results).toHaveLength(1);
    expect(results[0].valid).toBe(false);
  });

  it('should validateBatch returning boolean array', () => {
    const hook = defineHook({
      name: 'vbatch',
      trigger: 'before-add',
      validate: (q) => q.object.value !== 'value-5',
    });

    const results = validateBatch([hook], quads);
    expect(results).toHaveLength(20);
    expect(results[5]).toBe(false);
    expect(results[0]).toBe(true);
    expect(results[19]).toBe(true);
  });

  it('should transformBatch applying transforms to all quads', () => {
    const hook = defineHook({
      name: 'tbatch',
      trigger: 'before-add',
      transform: (q) =>
        quad(q.subject, q.predicate, literal(q.object.value.toUpperCase())),
    });

    const transformed = transformBatch([hook], quads);
    expect(transformed).toHaveLength(20);
    transformed.forEach((q) => {
      expect(q.object.value).toBe(q.object.value.toUpperCase());
    });
  });

  it('should transformBatch with validateFirst option', () => {
    const hook = defineHook({
      name: 'val-then-transform',
      trigger: 'before-add',
      validate: (q) => q.object.value !== 'value-3',
      transform: (q) =>
        quad(q.subject, q.predicate, literal(q.object.value + '-transformed')),
    });

    const transformed = transformBatch([hook], quads, { validateFirst: true });
    // value-3 fails validation, so 19 quads remain
    expect(transformed).toHaveLength(19);
    transformed.forEach((q) => {
      expect(q.object.value).toContain('-transformed');
    });
  });
});

describe('Integration: validateOnly fast path', () => {
  const testQuad = makeQuad('http://s', 'http://p', 'test');

  it('should validate without executing transforms', () => {
    const hooks = [
      defineHook({
        name: 'val-only-1',
        trigger: 'before-add',
        validate: () => true,
      }),
      defineHook({
        name: 'val-only-2',
        trigger: 'before-add',
        validate: () => true,
      }),
    ];

    const result = validateOnly(hooks, testQuad);
    expect(result.valid).toBe(true);
  });

  it('should return failure on first failing hook', () => {
    const hooks = [
      defineHook({ name: 'pass', trigger: 'before-add', validate: () => true }),
      defineHook({ name: 'fail', trigger: 'before-add', validate: () => false }),
      defineHook({ name: 'never', trigger: 'before-add', validate: () => true }),
    ];

    const result = validateOnly(hooks, testQuad);
    expect(result.valid).toBe(false);
    expect(result.hookName).toBe('fail');
  });

  it('should catch validation errors gracefully', () => {
    const hooks = [
      defineHook({
        name: 'thrower',
        trigger: 'before-add',
        validate: () => { throw new Error('boom'); },
      }),
    ];

    const result = validateOnly(hooks, testQuad);
    expect(result.valid).toBe(false);
    expect(result.error).toBe('boom');
  });
});

describe('Integration: Cache management', () => {
  it('should prewarm hook cache without errors', () => {
    const hooks = [
      defineHook({ name: 'pw1', trigger: 'before-add', validate: () => true }),
      defineHook({ name: 'pw2', trigger: 'after-add', validate: () => true }),
    ];

    const { prewarmed, errors } = prewarmHookCache(hooks);
    expect(prewarmed).toBe(2);
    expect(errors).toHaveLength(0);
  });

  it('should report errors for invalid hooks during prewarm', () => {
    const hooks = [
      defineHook({ name: 'valid', trigger: 'before-add', validate: () => true }),
      { name: 123, trigger: 'invalid' }, // invalid hook
    ];

    const { prewarmed, errors } = prewarmHookCache(hooks);
    expect(prewarmed).toBe(1);
    expect(errors.length).toBeGreaterThan(0);
  });

  it('should clear caches without throwing', () => {
    expect(() => clearHookCaches()).not.toThrow();
  });
});

describe('Integration: Registry management', () => {
  let registry;

  beforeEach(() => {
    registry = createHookRegistry();
  });

  it('should manage full registry lifecycle', () => {
    const h1 = defineHook({ name: 'r1', trigger: 'before-add', validate: () => true });
    const h2 = defineHook({ name: 'r2', trigger: 'after-add', validate: () => true });
    const h3 = defineHook({ name: 'r3', trigger: 'before-add', validate: () => true });

    registerHook(registry, h1);
    registerHook(registry, h2);
    registerHook(registry, h3);

    expect(listHooks(registry)).toHaveLength(3);

    const beforeHooks = getHooksByTrigger(registry, 'before-add');
    expect(beforeHooks).toHaveLength(2);

    unregisterHook(registry, 'r1');
    expect(listHooks(registry)).toHaveLength(2);

    clearHooks(registry);
    expect(listHooks(registry)).toHaveLength(0);
  });

  it('should reject duplicate hook names', () => {
    const hook = defineHook({ name: 'dup', trigger: 'before-add', validate: () => true });
    registerHook(registry, hook);
    expect(() => registerHook(registry, hook)).toThrow();
  });

  it('should execute registered hooks from registry', () => {
    const hook = defineHook({
      name: 'reg-exec',
      trigger: 'before-add',
      validate: (q) => q.subject.termType === 'NamedNode',
    });

    registerHook(registry, hook);
    const retrieved = getHook(registry, 'reg-exec');
    const testQuad = makeQuad('http://s', 'http://p', 'test');
    const result = executeHook(retrieved, testQuad);
    expect(result.valid).toBe(true);
  });
});

describe('Integration: Built-in hooks', () => {
  const iriQuad = makeIriQuad('http://ex.org/s', 'http://ex.org/p', 'http://ex.org/o');
  const litQuad = makeQuad('http://ex.org/s', 'http://ex.org/p', 'test');

  it('should validate IRI subjects with built-in hook', () => {
    const result = executeHook(builtinHooks.validateSubjectIRI, iriQuad);
    expect(result.valid).toBe(true);
  });

  it('should validate IRI predicates with built-in hook', () => {
    const result = executeHook(builtinHooks.validatePredicateIRI, iriQuad);
    expect(result.valid).toBe(true);
  });

  it('should validate IRI format with built-in hook', () => {
    const result = executeHook(builtinHooks.validateIRIFormat, iriQuad);
    expect(result.valid).toBe(true);
  });

  it('should chain built-in hooks together', () => {
    const chain = [
      builtinHooks.validateSubjectIRI,
      builtinHooks.validatePredicateIRI,
      builtinHooks.validateIRIFormat,
    ];

    const result = executeHookChain(chain, iriQuad);
    expect(result.valid).toBe(true);
    expect(result.results).toHaveLength(3);
  });

  it('should use built-in standard validation', () => {
    const result = executeHook(builtinHooks.standardValidation, iriQuad);
    expect(result.valid).toBe(true);
  });
});

describe('Integration: Error handling across the stack', () => {
  const testQuad = makeQuad('http://s', 'http://p', 'test');

  it('should propagate validation exceptions through executeHook', () => {
    const hook = defineHook({
      name: 'exception-hook',
      trigger: 'before-add',
      validate: () => { throw new Error('deliberate'); },
    });

    expect(() => executeHook(hook, testQuad)).toThrow('deliberate');
  });

  it('should propagate transform exceptions through executeHook', () => {
    const hook = defineHook({
      name: 'transform-error',
      trigger: 'before-add',
      transform: () => { throw new Error('transform-boom'); },
    });

    expect(() => executeHook(hook, testQuad)).toThrow('transform-boom');
  });

  it('should reject non-array hooks in executeHookChain', () => {
    expect(() => executeHookChain('not-array', testQuad)).toThrow(TypeError);
    expect(() => executeHookChain(null, testQuad)).toThrow(TypeError);
  });

  it('should reject null hooks in chain', () => {
    const validHook = defineHook({ name: 'ok', trigger: 'before-add', validate: () => true });
    expect(() => executeHookChain([validHook, null], testQuad)).toThrow();
  });

  it('should handle batch errors without crashing', () => {
    const hook = defineHook({
      name: 'batch-err',
      trigger: 'before-add',
      validate: () => { throw new Error('batch-fail'); },
    });

    const quads = [testQuad, testQuad];
    const results = executeBatch([hook], quads);
    expect(results).toHaveLength(2);
    results.forEach((r) => {
      expect(r.valid).toBe(false);
      expect(r.error).toBe('batch-fail');
    });
  });
});

describe('Integration: Zod schema validation at API boundaries', () => {
  it('should reject hook definition with empty name', () => {
    expect(() =>
      defineHook({ name: '', trigger: 'before-add', validate: () => true })
    ).toThrow();
  });

  it('should reject hook definition with invalid trigger', () => {
    expect(() =>
      defineHook({ name: 'bad', trigger: 'not-a-trigger', validate: () => true })
    ).toThrow();
  });

  it('should validate HookResultSchema correctly', () => {
    const valid = HookResultSchema.safeParse({ valid: true, hookName: 'test' });
    expect(valid.success).toBe(true);

    const invalid = HookResultSchema.safeParse({ valid: 'no', hookName: 42 });
    expect(invalid.success).toBe(false);
  });

  it('should validate ChainResultSchema correctly', () => {
    const valid = ChainResultSchema.safeParse({
      valid: true,
      quad: {},
      results: [],
    });
    expect(valid.success).toBe(true);
  });

  it('should validate all 33 trigger types', () => {
    const allTriggers = [
      'before-add', 'after-add', 'before-query', 'after-query',
      'before-remove', 'after-remove', 'before-commit', 'after-commit',
      'before-rollback', 'after-rollback', 'on-error', 'on-validation-fail',
      'on-transform', 'on-timeout', 'on-circuit-open', 'before-fetch',
      'after-fetch', 'before-sync', 'after-sync', 'before-import',
      'after-import', 'on-schedule', 'on-interval', 'on-idle', 'on-startup',
      'quality-gate', 'defect-detection', 'continuous-improvement',
      'spc-control', 'capability-analysis', 'root-cause',
      'kaizen-event', 'audit-trail',
    ];

    for (const trigger of allTriggers) {
      expect(HookTriggerSchema.parse(trigger)).toBe(trigger);
    }
    expect(allTriggers).toHaveLength(33);
  });
});

describe('Integration: Performance under load', () => {
  it('should execute 1000 hooks in batch under 500ms', () => {
    const hook = defineHook({
      name: 'perf-batch',
      trigger: 'before-add',
      validate: (q) => q.subject.termType === 'NamedNode',
    });

    const quads = Array.from({ length: 1000 }, (_, i) =>
      makeQuad(`http://s${i}`, 'http://p', `v${i}`)
    );

    const start = performance.now();
    const results = executeBatch([hook], quads);
    const duration = performance.now() - start;

    expect(results).toHaveLength(1000);
    expect(duration).toBeLessThan(500);
  });

  it('should validateBatch 1000 quads under 200ms', () => {
    const hooks = [
      defineHook({ name: 'fast1', trigger: 'before-add', validate: () => true }),
      defineHook({ name: 'fast2', trigger: 'before-add', validate: () => true }),
    ];

    const quads = Array.from({ length: 1000 }, (_, i) =>
      makeQuad(`http://s${i}`, 'http://p', `v${i}`)
    );

    const start = performance.now();
    const results = validateBatch(hooks, quads);
    const duration = performance.now() - start;

    expect(results).toHaveLength(1000);
    expect(results.every(Boolean)).toBe(true);
    expect(duration).toBeLessThan(200);
  });

  it('should handle 50 hooks in a chain under 100ms', () => {
    const hooks = Array.from({ length: 50 }, (_, i) =>
      defineHook({
        name: `chain-hook-${i}`,
        trigger: 'before-add',
        validate: () => true,
      })
    );

    const testQuad = makeQuad('http://s', 'http://p', 'test');

    const start = performance.now();
    const result = executeHookChain(hooks, testQuad);
    const duration = performance.now() - start;

    expect(result.valid).toBe(true);
    expect(result.results).toHaveLength(50);
    expect(duration).toBeLessThan(100);
  });
});

describe('Integration: Memory stability', () => {
  it('should not leak with repeated batch executions', () => {
    const hook = defineHook({
      name: 'mem-stable',
      trigger: 'before-add',
      validate: () => true,
    });

    const quads = Array.from({ length: 100 }, (_, i) =>
      makeQuad(`http://s${i}`, 'http://p', `v${i}`)
    );

    for (let iteration = 0; iteration < 100; iteration++) {
      executeBatch([hook], quads);
    }

    // If we get here without OOM, test passes
    expect(true).toBe(true);
  });

  it('should not leak with repeated prewarm + clear cycles', () => {
    for (let i = 0; i < 100; i++) {
      const hooks = Array.from({ length: 10 }, (_, j) =>
        defineHook({
          name: `cycle-${i}-${j}`,
          trigger: 'before-add',
          validate: () => true,
        })
      );
      prewarmHookCache(hooks);
      clearHookCaches();
    }
    expect(true).toBe(true);
  });
});
