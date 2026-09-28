/**
 * @vitest-environment node
 * @file Export Entry Points Tests - Verify ./define and ./executor entry points
 *
 * Tests that all exported functions exist, have correct signatures,
 * and work correctly when imported from their dedicated entry points.
 */
import { describe, it, expect } from 'vitest';
import { UnrdfDataFactory as DataFactory } from '@unrdf/core/rdf/n3-justified-only';

// Import from ./define entry point
import {
  defineHook,
  isValidHook,
  getHookMetadata,
  hasValidation,
  hasTransformation,
  HookTriggerSchema,
  HookConfigSchema,
  HookSchema,
  KnowledgeHookSchema,
  HookMetaSchema,
  HookConditionSchema,
  HookEffectSchema,
  createKnowledgeHook,
  validateKnowledgeHook,
  ConditionKind,
  SPARQL_ASK,
  SPARQL_SELECT,
  SHACL,
  DELTA,
  THRESHOLD,
  COUNT,
  WINDOW,
  N3,
  defineValidationHook,
  defineTransformHook,
  defineValidateAndTransformHook,
} from '../src/define.mjs';

// Import from ./executor entry point
import {
  HookExecutor,
  executeHook,
  batchExecute,
  HookResultSchema,
  ChainResultSchema,
} from '../src/executor.mjs';

const { namedNode, literal, quad } = DataFactory;

/* ═══════════════════════════════════════════════════════════════════════════ */
/* ./define entry point                                                      */
/* ═══════════════════════════════════════════════════════════════════════════ */

describe('./define entry point - Core exports', () => {
  it('should export defineHook as a function', () => {
    expect(typeof defineHook).toBe('function');
  });

  it('should export isValidHook as a function', () => {
    expect(typeof isValidHook).toBe('function');
  });

  it('should export getHookMetadata as a function', () => {
    expect(typeof getHookMetadata).toBe('function');
  });

  it('should export hasValidation as a function', () => {
    expect(typeof hasValidation).toBe('function');
  });

  it('should export hasTransformation as a function', () => {
    expect(typeof hasTransformation).toBe('function');
  });

  it('should export HookTriggerSchema as a Zod schema', () => {
    expect(HookTriggerSchema).toBeDefined();
    expect(typeof HookTriggerSchema.parse).toBe('function');
  });

  it('should export HookConfigSchema as a Zod schema', () => {
    expect(HookConfigSchema).toBeDefined();
    expect(typeof HookConfigSchema.parse).toBe('function');
  });

  it('should export HookSchema as a Zod schema', () => {
    expect(HookSchema).toBeDefined();
    expect(typeof HookSchema.parse).toBe('function');
  });
});

describe('./define entry point - defineHook works correctly', () => {
  it('should define a validation hook via entry point', () => {
    const hook = defineHook({
      name: 'ep-validator',
      trigger: 'before-add',
      validate: (q) => q.subject.termType === 'NamedNode',
    });

    expect(hook.name).toBe('ep-validator');
    expect(hook.trigger).toBe('before-add');
    expect(typeof hook.validate).toBe('function');
    expect(hook._validated).toBe(true);
  });

  it('should define a transformation hook via entry point', () => {
    const hook = defineHook({
      name: 'ep-transformer',
      trigger: 'before-add',
      transform: (q) => q,
    });

    expect(hook.name).toBe('ep-transformer');
    expect(typeof hook.transform).toBe('function');
  });

  it('should define a hook with both validate and transform', () => {
    const hook = defineHook({
      name: 'ep-dual',
      trigger: 'before-add',
      validate: () => true,
      transform: (q) => q,
    });

    expect(hasValidation(hook)).toBe(true);
    expect(hasTransformation(hook)).toBe(true);
  });

  it('should throw on missing name', () => {
    expect(() =>
      defineHook({ trigger: 'before-add', validate: () => true })
    ).toThrow();
  });

  it('should throw on missing validate/transform/run', () => {
    expect(() =>
      defineHook({ name: 'bad', trigger: 'before-add' })
    ).toThrow();
  });
});

describe('./define entry point - isValidHook and metadata', () => {
  it('should validate a proper hook', () => {
    const hook = defineHook({
      name: 'valid-check',
      trigger: 'before-add',
      validate: () => true,
    });
    expect(isValidHook(hook)).toBe(true);
  });

  it('should reject an invalid object', () => {
    expect(isValidHook({})).toBe(false);
    expect(isValidHook(null)).toBe(false);
    expect(isValidHook('string')).toBe(false);
  });

  it('should retrieve metadata by key', () => {
    const hook = defineHook({
      name: 'meta-hook',
      trigger: 'before-add',
      validate: () => true,
      metadata: { author: 'test', version: '1.0' },
    });

    expect(getHookMetadata(hook, 'author')).toBe('test');
    expect(getHookMetadata(hook, 'version')).toBe('1.0');
    expect(getHookMetadata(hook, 'missing')).toBeUndefined();
  });
});

describe('./define entry point - Zod schemas', () => {
  it('should validate trigger types with HookTriggerSchema', () => {
    expect(HookTriggerSchema.parse('before-add')).toBe('before-add');
    expect(HookTriggerSchema.parse('after-add')).toBe('after-add');
    expect(HookTriggerSchema.parse('quality-gate')).toBe('quality-gate');
    expect(() => HookTriggerSchema.parse('invalid-trigger')).toThrow();
  });

  it('should validate all Lean Six Sigma trigger types', () => {
    const leanTriggers = [
      'quality-gate', 'defect-detection', 'continuous-improvement',
      'spc-control', 'capability-analysis', 'root-cause',
      'kaizen-event', 'audit-trail',
    ];
    for (const trigger of leanTriggers) {
      expect(HookTriggerSchema.parse(trigger)).toBe(trigger);
    }
  });

  it('should validate hook config with HookConfigSchema', () => {
    const config = {
      name: 'schema-test',
      trigger: 'before-add',
      validate: () => true,
    };
    const parsed = HookConfigSchema.parse(config);
    expect(parsed.name).toBe('schema-test');
    expect(parsed.trigger).toBe('before-add');
  });
});

describe('./define entry point - Knowledge hook schemas', () => {
  it('should export KnowledgeHookSchema', () => {
    expect(KnowledgeHookSchema).toBeDefined();
    expect(typeof KnowledgeHookSchema.parse).toBe('function');
  });

  it('should export HookMetaSchema', () => {
    expect(HookMetaSchema).toBeDefined();
    const meta = HookMetaSchema.parse({ name: 'test', version: '1.0.0' });
    expect(meta.name).toBe('test');
  });

  it('should export HookConditionSchema', () => {
    expect(HookConditionSchema).toBeDefined();
    const condition = HookConditionSchema.parse({ kind: 'sparql-ask' });
    expect(condition.kind).toBe('sparql-ask');
  });

  it('should export HookEffectSchema', () => {
    expect(HookEffectSchema).toBeDefined();
  });

  it('should validate a knowledge hook via createKnowledgeHook', () => {
    const hookDef = {
      id: '550e8400-e29b-41d4-a716-446655440000',
      meta: { name: 'test-kh', version: '1.0.0' },
      condition: { kind: 'sparql-ask' },
      effect: { ref: { uri: 'file:///hook.mjs' }, timeout: 5000, retries: 1 },
      run: () => {},
    };
    const created = createKnowledgeHook(hookDef);
    expect(created.id).toBe('550e8400-e29b-41d4-a716-446655440000');
    expect(created.meta.name).toBe('test-kh');
  });

  it('should validate a valid knowledge hook with validateKnowledgeHook', () => {
    const hookDef = {
      id: '550e8400-e29b-41d4-a716-446655440001',
      meta: { name: 'vkh-test', version: '1.0.0' },
      condition: { kind: 'sparql-ask' },
      effect: { ref: { uri: 'file:///hook.mjs' }, timeout: 5000, retries: 1 },
      run: () => {},
    };
    const result = validateKnowledgeHook(hookDef);
    expect(result.success).toBe(true);
    expect(result.data).toBeDefined();
    expect(result.data.meta.name).toBe('vkh-test');
  });
});

describe('./define entry point - Condition kind constants', () => {
  it('should export all condition kind constants', () => {
    expect(SPARQL_ASK).toBe('sparql-ask');
    expect(SPARQL_SELECT).toBe('sparql-select');
    expect(SHACL).toBe('shacl');
    expect(DELTA).toBe('delta');
    expect(THRESHOLD).toBe('threshold');
    expect(COUNT).toBe('count');
    expect(WINDOW).toBe('window');
    expect(N3).toBe('n3');
  });

  it('should export ConditionKind as a frozen object', () => {
    expect(ConditionKind).toBeDefined();
    expect(Object.isFrozen(ConditionKind)).toBe(true);
    expect(ConditionKind.SPARQL_ASK).toBe('sparql-ask');
    expect(ConditionKind.SHACL).toBe('shacl');
    expect(ConditionKind.N3).toBe('n3');
  });

  it('should not allow mutation of ConditionKind', () => {
    expect(() => { ConditionKind.NEW_KIND = 'new'; }).toThrow();
  });
});

describe('./define entry point - Builder helpers', () => {
  it('should define a validation-only hook with defineValidationHook', () => {
    const hook = defineValidationHook('val-only', 'before-add', () => true);
    expect(hook.name).toBe('val-only');
    expect(hook.trigger).toBe('before-add');
    expect(hasValidation(hook)).toBe(true);
    expect(hasTransformation(hook)).toBe(false);
  });

  it('should define a transform-only hook with defineTransformHook', () => {
    const hook = defineTransformHook('txn-only', 'before-add', (q) => q);
    expect(hook.name).toBe('txn-only');
    expect(hasValidation(hook)).toBe(false);
    expect(hasTransformation(hook)).toBe(true);
  });

  it('should define a validate+transform hook with defineValidateAndTransformHook', () => {
    const hook = defineValidateAndTransformHook(
      'dual-hook', 'before-add',
      () => true,
      (q) => q,
      { tier: 'gold' }
    );
    expect(hook.name).toBe('dual-hook');
    expect(hasValidation(hook)).toBe(true);
    expect(hasTransformation(hook)).toBe(true);
    expect(hook.metadata.tier).toBe('gold');
  });

  it('should pass metadata through builder helpers', () => {
    const hook = defineValidationHook(
      'meta-val', 'after-add',
      () => true,
      { environment: 'test' }
    );
    expect(hook.metadata.environment).toBe('test');
  });
});

/* ═══════════════════════════════════════════════════════════════════════════ */
/* ./executor entry point                                                    */
/* ═══════════════════════════════════════════════════════════════════════════ */

describe('./executor entry point - Exports exist', () => {
  it('should export HookExecutor as a class', () => {
    expect(HookExecutor).toBeDefined();
    expect(typeof HookExecutor).toBe('function'); // class constructor
  });

  it('should export executeHook as a function', () => {
    expect(typeof executeHook).toBe('function');
  });

  it('should export batchExecute as a function', () => {
    expect(typeof batchExecute).toBe('function');
  });

  it('should export HookResultSchema as a Zod schema', () => {
    expect(HookResultSchema).toBeDefined();
    expect(typeof HookResultSchema.parse).toBe('function');
  });

  it('should export ChainResultSchema as a Zod schema', () => {
    expect(ChainResultSchema).toBeDefined();
    expect(typeof ChainResultSchema.parse).toBe('function');
  });
});

describe('./executor entry point - executeHook works', () => {
  const testQuad = quad(
    namedNode('http://example.org/s'),
    namedNode('http://example.org/p'),
    literal('test')
  );

  it('should execute a validation hook from executor entry point', () => {
    const hook = defineHook({
      name: 'exec-validator',
      trigger: 'before-add',
      validate: (q) => q.subject.termType === 'NamedNode',
    });

    const result = executeHook(hook, testQuad);
    expect(result.valid).toBe(true);
    expect(result.hookName).toBe('exec-validator');
  });

  it('should execute a transformation hook from executor entry point', () => {
    const hook = defineHook({
      name: 'exec-transformer',
      trigger: 'before-add',
      transform: (q) =>
        quad(q.subject, q.predicate, literal(q.object.value.toUpperCase())),
    });

    const result = executeHook(hook, testQuad);
    expect(result.valid).toBe(true);
    expect(result.quad.object.value).toBe('TEST');
  });

  it('should return validation failure result', () => {
    const hook = defineHook({
      name: 'exec-rejecter',
      trigger: 'before-add',
      validate: () => false,
    });

    const result = executeHook(hook, testQuad);
    expect(result.valid).toBe(false);
    expect(result.error).toBeDefined();
  });

  it('should throw on null quad', () => {
    const hook = defineHook({
      name: 'null-guard',
      trigger: 'before-add',
      validate: () => true,
    });

    expect(() => executeHook(hook, null)).toThrow(TypeError);
  });
});

describe('./executor entry point - batchExecute works', () => {
  it('should batch execute hooks on multiple quads', () => {
    const quads = Array.from({ length: 5 }, (_, i) =>
      quad(
        namedNode(`http://example.org/s${i}`),
        namedNode('http://example.org/p'),
        literal(`value-${i}`)
      )
    );

    const hook = defineHook({
      name: 'batch-hook',
      trigger: 'before-add',
      validate: (q) => q.subject.termType === 'NamedNode',
    });

    const results = batchExecute([hook], quads);
    expect(results).toHaveLength(5);
    results.forEach((r) => expect(r.valid).toBe(true));
  });

  it('should report failures in batch execution', () => {
    const quads = [
      quad(namedNode('http://s'), namedNode('http://p'), literal('ok')),
      quad(namedNode('http://s'), namedNode('http://p'), literal('ok')),
    ];

    const hook = defineHook({
      name: 'batch-rejecter',
      trigger: 'before-add',
      validate: () => false,
    });

    const results = batchExecute([hook], quads);
    expect(results).toHaveLength(2);
    results.forEach((r) => expect(r.valid).toBe(false));
  });

  it('should stop on error when option is set', () => {
    const quads = Array.from({ length: 10 }, (_, i) =>
      quad(namedNode(`http://s${i}`), namedNode('http://p'), literal(`v${i}`))
    );

    const hook = defineHook({
      name: 'stop-early',
      trigger: 'before-add',
      validate: () => false,
    });

    const results = batchExecute([hook], quads, { stopOnError: true });
    expect(results).toHaveLength(1);
    expect(results[0].valid).toBe(false);
  });
});

describe('./executor entry point - HookResultSchema validation', () => {
  it('should validate a valid hook result', () => {
    const result = HookResultSchema.parse({
      valid: true,
      hookName: 'test',
    });
    expect(result.valid).toBe(true);
    expect(result.hookName).toBe('test');
  });

  it('should validate a failed hook result with error', () => {
    const result = HookResultSchema.parse({
      valid: false,
      hookName: 'test',
      error: 'Validation failed',
    });
    expect(result.valid).toBe(false);
    expect(result.error).toBe('Validation failed');
  });

  it('should reject invalid result schema', () => {
    expect(() => HookResultSchema.parse({ valid: 'not-bool' })).toThrow();
  });
});

describe('./executor entry point - ChainResultSchema validation', () => {
  it('should validate a valid chain result', () => {
    const result = ChainResultSchema.parse({
      valid: true,
      quad: {},
      results: [{ valid: true, hookName: 'h1' }],
    });
    expect(result.valid).toBe(true);
    expect(result.results).toHaveLength(1);
  });

  it('should validate a failed chain result', () => {
    const result = ChainResultSchema.parse({
      valid: false,
      quad: {},
      results: [{ valid: false, hookName: 'h1', error: 'fail' }],
      error: 'Chain failed',
    });
    expect(result.valid).toBe(false);
    expect(result.error).toBe('Chain failed');
  });
});

/* ═══════════════════════════════════════════════════════════════════════════ */
/* Cross-entry-point interoperability                                        */
/* ═══════════════════════════════════════════════════════════════════════════ */

describe('Cross-entry-point interoperability', () => {
  const testQuad = quad(
    namedNode('http://example.org/s'),
    namedNode('http://example.org/p'),
    literal('interop-test')
  );

  it('should define a hook via ./define and execute via ./executor', () => {
    const hook = defineHook({
      name: 'cross-entry',
      trigger: 'before-add',
      validate: (q) => q.subject.termType === 'NamedNode',
    });

    const result = executeHook(hook, testQuad);
    expect(result.valid).toBe(true);
  });

  it('should use builder helper from ./define with executeHook from ./executor', () => {
    const hook = defineValidationHook(
      'builder-cross',
      'before-add',
      (q) => q.object.value.length > 0
    );

    const result = executeHook(hook, testQuad);
    expect(result.valid).toBe(true);
  });

  it('should batch execute hooks defined via ./define builders', () => {
    const quads = [testQuad, testQuad, testQuad];
    const hook = defineTransformHook('batch-cross', 'before-add', (q) => q);

    const results = batchExecute([hook], quads);
    expect(results).toHaveLength(3);
    results.forEach((r) => expect(r.valid).toBe(true));
  });

  it('should verify pre-computed flags work across entry points', () => {
    const hook = defineValidateAndTransformHook(
      'flags-cross', 'before-add',
      () => true,
      (q) => q
    );

    // Pre-computed flags from defineHook should be honored by executor
    expect(hook._validated).toBe(true);
    expect(hook._hasValidation).toBe(true);
    expect(hook._hasTransformation).toBe(true);

    const result = executeHook(hook, testQuad);
    expect(result.valid).toBe(true);
  });
});
