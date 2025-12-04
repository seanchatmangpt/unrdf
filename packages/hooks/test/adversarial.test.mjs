/**
 * @vitest-environment node
 * Adversarial Testing: Test advertised capabilities for @unrdf/hooks
 * Goal: PROVE what doesn't work, not security
 */

import { describe, it, expect } from 'vitest';
import {
  defineHook,
  isValidHook,
  executeHook,
  executeHookChain,
  createHookRegistry,
  registerHook,
  getHooksByTrigger,
  builtinHooks,
  validateSubjectIRI,
  standardValidation,
} from '../src/index.mjs';
import { namedNode, literal, quad } from '@unrdf/core';

describe('@unrdf/hooks Adversarial Tests - Capabilities', () => {
  describe('Hook Definition - Advertised Features', () => {
    it('ADVERTISED: Can define hooks with validation', () => {
      const hook = defineHook({
        name: 'test-hook',
        trigger: 'before-add',
        validate: ({ quad }) => ({ valid: true, quad }),
      });

      expect(isValidHook(hook)).toBe(true);
      expect(hook.name).toBe('test-hook');
      expect(hook.trigger).toBe('before-add');
    });

    it('ADVERTISED: Can define hooks with transformation', () => {
      const hook = defineHook({
        name: 'transform-hook',
        trigger: 'before-add',
        transform: ({ quad }) => ({ quad }),
      });

      expect(isValidHook(hook)).toBe(true);
      expect(hook.transform).toBeDefined();
    });

    it('ADVERTISED: Can define hooks with both validation and transformation', () => {
      const hook = defineHook({
        name: 'combo-hook',
        trigger: 'before-add',
        validate: ({ quad }) => ({ valid: true, quad }),
        transform: ({ quad }) => ({ quad }),
      });

      expect(isValidHook(hook)).toBe(true);
      expect(hook.validate).toBeDefined();
      expect(hook.transform).toBeDefined();
    });
  });

  describe('Hook Execution - Advertised Features', () => {
    it('ADVERTISED: Can execute hooks on quad operations', () => {
      const hook = defineHook({
        name: 'exec-hook',
        trigger: 'before-add',
        validate: q => q.subject.termType === 'NamedNode',
      });

      const testQuad = quad(
        namedNode('http://example.org/s'),
        namedNode('http://example.org/p'),
        literal('o')
      );

      const result = executeHook(hook, testQuad);
      expect(result.valid).toBe(true);
    });

    it('ADVERTISED: Can execute hook chains', () => {
      const hook1 = defineHook({
        name: 'chain-1',
        trigger: 'before-add',
        validate: q => q.subject.termType === 'NamedNode',
      });

      const hook2 = defineHook({
        name: 'chain-2',
        trigger: 'before-add',
        validate: q => q.predicate.termType === 'NamedNode',
      });

      const testQuad = quad(
        namedNode('http://example.org/s'),
        namedNode('http://example.org/p'),
        literal('o')
      );

      const result = executeHookChain([hook1, hook2], testQuad);
      expect(result.valid).toBe(true);
    });
  });

  describe('Hook Registry - Advertised Features', () => {
    it('ADVERTISED: Can create and manage hook registry', () => {
      const registry = createHookRegistry();

      const hook = defineHook({
        name: 'registry-hook',
        trigger: 'before-add',
        validate: ({ quad }) => ({ valid: true, quad }),
      });

      registerHook(registry, hook);

      const hooks = getHooksByTrigger(registry, 'before-add');
      expect(hooks.length).toBeGreaterThan(0);
      expect(hooks[0].name).toBe('registry-hook');
    });

    it('ADVERTISED: Can filter hooks by trigger type', () => {
      const registry = createHookRegistry();

      registerHook(
        registry,
        defineHook({
          name: 'add-hook',
          trigger: 'before-add',
          validate: q => q.subject.termType === 'NamedNode',
        })
      );

      registerHook(
        registry,
        defineHook({
          name: 'delete-hook',
          trigger: 'before-remove',
          validate: q => q.subject.termType === 'NamedNode',
        })
      );

      const addHooks = getHooksByTrigger(registry, 'before-add');
      const deleteHooks = getHooksByTrigger(registry, 'before-remove');

      expect(addHooks.length).toBe(1);
      expect(deleteHooks.length).toBe(1);
      expect(addHooks[0].name).toBe('add-hook');
      expect(deleteHooks[0].name).toBe('delete-hook');
    });
  });

  describe('Built-in Hooks - Advertised Features', () => {
    it('ADVERTISED: Can use built-in IRI validation hooks', () => {
      const validIRI = namedNode('http://example.org/valid');

      const testQuad = quad(validIRI, namedNode('http://example.org/p'), literal('o'));

      const result = executeHook(validateSubjectIRI, testQuad);

      expect(result.valid).toBe(true);
    });

    it('ADVERTISED: Built-in hooks are available and functional', () => {
      expect(builtinHooks).toBeDefined();
      expect(typeof builtinHooks).toBe('object');

      expect(builtinHooks.validateSubjectIRI).toBeDefined();
      expect(isValidHook(builtinHooks.validateSubjectIRI)).toBe(true);
    });

    it('ADVERTISED: Standard validation hook works', () => {
      const testQuad = quad(
        namedNode('http://example.org/s'),
        namedNode('http://example.org/p'),
        literal('o')
      );

      const result = executeHook(standardValidation, testQuad);
      expect(result.valid).toBe(true);
    });
  });
});
