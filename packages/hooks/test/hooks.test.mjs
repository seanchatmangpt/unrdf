/**
 * @vitest-environment node
 * FAST SUITE: Essential hook tests only
 * Target: <500ms total for all hooks tests
 * Removed: Complex chains, policy compilation, full integration
 */
import { describe, it, expect } from 'vitest';
import { UnrdfDataFactory as DataFactory } from '@unrdf/core/rdf/n3-justified-only';

import {
  defineHook,
  executeHook,
  createHookRegistry,
  registerHook,
} from '../src/index.mjs';

const { namedNode, literal, quad } = DataFactory;

describe('@unrdf/hooks - Fast Suite', () => {
  it('should define and register hook', () => {
    const hook = defineHook({
      name: 'test-validator',
      trigger: 'before-add',
      validate: q => q.subject.termType === 'NamedNode',
    });
    expect(hook.name).toBe('test-validator');
    expect(hook.trigger).toBe('before-add');
  });

  it('should execute validation hook', () => {
    const testQuad = quad(
      namedNode('http://example.org/s'),
      namedNode('http://example.org/p'),
      literal('test')
    );
    const hook = defineHook({
      name: 'validator',
      trigger: 'before-add',
      validate: q => q.subject.termType === 'NamedNode',
    });
    const result = executeHook(hook, testQuad);
    expect(result.valid).toBe(true);
  });

  it('should fail validation correctly', () => {
    const hook = defineHook({
      name: 'validator',
      trigger: 'before-add',
      validate: () => false,
    });
    const testQuad = quad(
      namedNode('http://example.org/s'),
      namedNode('http://example.org/p'),
      literal('test')
    );
    const result = executeHook(hook, testQuad);
    expect(result.valid).toBe(false);
  });

  it('should throw on duplicate hook registration', () => {
    const registry = createHookRegistry();
    const hook = defineHook({
      name: 'test',
      trigger: 'before-add',
      validate: () => true,
    });
    registerHook(registry, hook);
    expect(() => registerHook(registry, hook)).toThrow();
  });
});
