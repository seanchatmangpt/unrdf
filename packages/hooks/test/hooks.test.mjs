/**
 * @vitest-environment node
 */
import { describe, it, expect, beforeEach } from 'vitest';
import { DataFactory } from 'n3';

import {
  defineHook,
  isValidHook,
  executeHook,
  executeHookChain,
  executeHooksByTrigger,
  createHookRegistry,
  registerHook,
  unregisterHook,
  getHook,
  listHooks,
  getHooksByTrigger,
  builtinHooks,
} from '../src/index.mjs';

const { namedNode, literal, quad } = DataFactory;

describe('@unrdf/hooks - Hook Definition', () => {
  it('should define a validation hook', () => {
    const hook = defineHook({
      name: 'test-validator',
      trigger: 'before-add',
      validate: q => q.subject.termType === 'NamedNode',
    });

    expect(hook.name).toBe('test-validator');
    expect(hook.trigger).toBe('before-add');
    expect(typeof hook.validate).toBe('function');
  });

  it('should define a transformation hook', () => {
    const hook = defineHook({
      name: 'test-transformer',
      trigger: 'before-add',
      transform: q => q,
    });

    expect(hook.name).toBe('test-transformer');
    expect(typeof hook.transform).toBe('function');
  });

  it('should throw if neither validate nor transform provided', () => {
    expect(() => {
      defineHook({
        name: 'invalid',
        trigger: 'before-add',
      });
    }).toThrow();
  });

  it('should validate hook with isValidHook', () => {
    const valid = defineHook({
      name: 'valid',
      trigger: 'before-add',
      validate: () => true,
    });

    expect(isValidHook(valid)).toBe(true);
    expect(isValidHook({})).toBe(false);
  });
});

describe('@unrdf/hooks - Hook Execution', () => {
  const testQuad = quad(
    namedNode('http://example.org/s'),
    namedNode('http://example.org/p'),
    literal('test')
  );

  it('should execute validation hook', () => {
    const hook = defineHook({
      name: 'validator',
      trigger: 'before-add',
      validate: q => q.subject.termType === 'NamedNode',
    });

    const result = executeHook(hook, testQuad);
    expect(result.valid).toBe(true);
    expect(result.hookName).toBe('validator');
  });

  it('should fail validation for invalid quad', () => {
    const hook = defineHook({
      name: 'validator',
      trigger: 'before-add',
      validate: () => false,
    });

    const result = executeHook(hook, testQuad);
    expect(result.valid).toBe(false);
    expect(result.error).toBeDefined();
  });

  it('should execute transformation hook', () => {
    const hook = defineHook({
      name: 'transformer',
      trigger: 'before-add',
      transform: q => q,
    });

    const result = executeHook(hook, testQuad);
    expect(result.valid).toBe(true);
    expect(result.quad).toBe(testQuad);
  });

  it('should execute hook chain', () => {
    const validator = defineHook({
      name: 'validator',
      trigger: 'before-add',
      validate: q => q.subject.termType === 'NamedNode',
    });

    const transformer = defineHook({
      name: 'transformer',
      trigger: 'before-add',
      transform: q => q,
    });

    const result = executeHookChain([validator, transformer], testQuad);
    expect(result.valid).toBe(true);
    expect(result.results.length).toBe(2);
  });

  it('should stop chain on validation failure', () => {
    const validator = defineHook({
      name: 'validator',
      trigger: 'before-add',
      validate: () => false,
    });

    const transformer = defineHook({
      name: 'transformer',
      trigger: 'before-add',
      transform: q => q,
    });

    const result = executeHookChain([validator, transformer], testQuad);
    expect(result.valid).toBe(false);
    expect(result.results.length).toBe(1);
  });

  it('should execute hooks by trigger type', () => {
    const beforeHook = defineHook({
      name: 'before',
      trigger: 'before-add',
      validate: () => true,
    });

    const afterHook = defineHook({
      name: 'after',
      trigger: 'after-add',
      validate: () => true,
    });

    const result = executeHooksByTrigger([beforeHook, afterHook], 'before-add', testQuad);
    expect(result.valid).toBe(true);
    expect(result.results.length).toBe(1);
    expect(result.results[0].hookName).toBe('before');
  });
});

describe('@unrdf/hooks - Hook Management', () => {
  let registry;

  beforeEach(() => {
    registry = createHookRegistry();
  });

  it('should create empty registry', () => {
    expect(registry.hooks.size).toBe(0);
  });

  it('should register hook', () => {
    const hook = defineHook({
      name: 'test',
      trigger: 'before-add',
      validate: () => true,
    });

    registerHook(registry, hook);
    expect(registry.hooks.size).toBe(1);
  });

  it('should throw on duplicate hook name', () => {
    const hook = defineHook({
      name: 'test',
      trigger: 'before-add',
      validate: () => true,
    });

    registerHook(registry, hook);
    expect(() => registerHook(registry, hook)).toThrow();
  });

  it('should unregister hook', () => {
    const hook = defineHook({
      name: 'test',
      trigger: 'before-add',
      validate: () => true,
    });

    registerHook(registry, hook);
    const removed = unregisterHook(registry, 'test');
    expect(removed).toBe(true);
    expect(registry.hooks.size).toBe(0);
  });

  it('should get hook by name', () => {
    const hook = defineHook({
      name: 'test',
      trigger: 'before-add',
      validate: () => true,
    });

    registerHook(registry, hook);
    const retrieved = getHook(registry, 'test');
    expect(retrieved).toBeDefined();
    expect(retrieved.name).toBe('test');
  });

  it('should list all hooks', () => {
    const hook1 = defineHook({
      name: 'test1',
      trigger: 'before-add',
      validate: () => true,
    });

    const hook2 = defineHook({
      name: 'test2',
      trigger: 'after-add',
      validate: () => true,
    });

    registerHook(registry, hook1);
    registerHook(registry, hook2);

    const hooks = listHooks(registry);
    expect(hooks.length).toBe(2);
  });

  it('should get hooks by trigger', () => {
    const hook1 = defineHook({
      name: 'test1',
      trigger: 'before-add',
      validate: () => true,
    });

    const hook2 = defineHook({
      name: 'test2',
      trigger: 'before-add',
      validate: () => true,
    });

    const hook3 = defineHook({
      name: 'test3',
      trigger: 'after-add',
      validate: () => true,
    });

    registerHook(registry, hook1);
    registerHook(registry, hook2);
    registerHook(registry, hook3);

    const beforeHooks = getHooksByTrigger(registry, 'before-add');
    expect(beforeHooks.length).toBe(2);
  });
});

describe('@unrdf/hooks - Built-in Hooks', () => {
  const iriQuad = quad(
    namedNode('http://example.org/s'),
    namedNode('http://example.org/p'),
    namedNode('http://example.org/o')
  );

  const literalQuad = quad(
    namedNode('http://example.org/s'),
    namedNode('http://example.org/p'),
    literal('test')
  );

  it('should validate subject IRI', () => {
    const result = executeHook(builtinHooks.validateSubjectIRI, iriQuad);
    expect(result.valid).toBe(true);
  });

  it('should validate predicate IRI', () => {
    const result = executeHook(builtinHooks.validatePredicateIRI, iriQuad);
    expect(result.valid).toBe(true);
  });

  it('should validate object literal', () => {
    const result = executeHook(builtinHooks.validateObjectLiteral, literalQuad);
    expect(result.valid).toBe(true);
  });

  it('should validate IRI format', () => {
    const result = executeHook(builtinHooks.validateIRIFormat, iriQuad);
    expect(result.valid).toBe(true);
  });

  it('should apply standard validation', () => {
    const result = executeHook(builtinHooks.standardValidation, iriQuad);
    expect(result.valid).toBe(true);
  });
});
