/**
 * @vitest-environment node
 * Adversarial Testing: Hooks - Test Advertised Capabilities
 */

import { describe, it, expect } from 'vitest';
import {
  defineHook,
  executeHook,
  executeHookChain,
  createHookRegistry,
  registerHook,
  unregisterHook,
  getHooksByTrigger,
  getHook,
  validateSubjectIRI,
  validatePredicateIRI,
  normalizeLanguageTag,
} from '../src/index.mjs';

describe('@unrdf/hooks Adversarial Tests - Capabilities', () => {
  describe('Hook Definition - Advertised Features', () => {
    it('ADVERTISED: Can define validation hooks with trigger types', () => {
      const hook = defineHook({
        name: 'test-validation',
        trigger: 'before-add',
        validate: quad => {
          return quad.subject && quad.subject.termType === 'NamedNode';
        },
      });

      expect(hook).toBeDefined();
      expect(hook.name).toBe('test-validation');
    });

    it('ADVERTISED: Can define transformation hooks that modify quads', () => {
      const hook = defineHook({
        name: 'test-transform',
        trigger: 'before-add',
        transform: quad => {
          // Advertised: Should return modified quad
          return {
            ...quad,
            subject: { ...quad.subject, value: quad.subject.value.toUpperCase() },
          };
        },
      });

      expect(hook.transform).toBeDefined();
    });

    it('ADVERTISED: Cannot define hooks without required fields', () => {
      expect(() => {
        defineHook({
          // Missing 'name' - should fail
          trigger: 'before-add',
        });
      }).toThrow();
    });
  });

  describe('Hook Execution - Advertised Features', () => {
    it('ADVERTISED: executeHook validates quads and returns pass/fail', () => {
      const hook = defineHook({
        name: 'validation',
        trigger: 'before-add',
        validate: quad => quad.subject !== null,
      });

      const testQuad = {
        subject: { value: 'http://example.org/s', termType: 'NamedNode' },
        predicate: { value: 'http://example.org/p', termType: 'NamedNode' },
        object: { value: 'object', termType: 'Literal' },
      };

      const result = executeHook(hook, testQuad);
      expect(result.valid).toBe(true);
    });

    it('ADVERTISED: executeHook transformation actually modifies quad', () => {
      const hook = defineHook({
        name: 'uppercase',
        trigger: 'before-add',
        transform: quad => ({
          ...quad,
          object: { ...quad.object, value: quad.object.value.toUpperCase() },
        }),
      });

      const testQuad = {
        subject: { value: 'http://example.org/s', termType: 'NamedNode' },
        predicate: { value: 'http://example.org/p', termType: 'NamedNode' },
        object: { value: 'lowercase text', termType: 'Literal' },
      };

      const result = executeHook(hook, testQuad);
      expect(result.quad.object.value).toBe('LOWERCASE TEXT');
    });

    it('ADVERTISED: executeHookChain runs multiple hooks in sequence', () => {
      const hook1 = defineHook({
        name: 'hook1',
        trigger: 'before-add',
        validate: quad => quad.subject !== null,
      });

      const hook2 = defineHook({
        name: 'hook2',
        trigger: 'before-add',
        transform: quad => quad,
      });

      const testQuad = {
        subject: { value: 'http://example.org/s', termType: 'NamedNode' },
        predicate: { value: 'http://example.org/p', termType: 'NamedNode' },
        object: { value: 'object', termType: 'Literal' },
      };

      const result = executeHookChain([hook1, hook2], testQuad);
      expect(result.valid).toBe(true);
    });

    it('ADVERTISED: Hook chain STOPS at first validation failure', () => {
      const hook1 = defineHook({
        name: 'fail',
        trigger: 'before-add',
        validate: _quad => false, // Always fails
      });

      const hook2 = defineHook({
        name: 'never-runs',
        trigger: 'before-add',
        validate: _quad => {
          throw new Error('Should not be called!');
        },
      });

      const testQuad = {
        subject: { value: 'http://example.org/s', termType: 'NamedNode' },
        predicate: { value: 'http://example.org/p', termType: 'NamedNode' },
        object: { value: 'object', termType: 'Literal' },
      };

      const result = executeHookChain([hook1, hook2], testQuad);
      expect(result.valid).toBe(false);
    });
  });

  describe('Hook Registry - Advertised Features', () => {
    it('ADVERTISED: Can create registry and register/unregister hooks', () => {
      const registry = createHookRegistry();

      const hook = defineHook({
        name: 'test-hook',
        trigger: 'before-add',
        validate: _quad => true,
      });

      registerHook(registry, hook);
      const registered = getHook(registry, 'test-hook');
      expect(registered).toBeDefined();

      unregisterHook(registry, 'test-hook');
      const unregistered = getHook(registry, 'test-hook');
      expect(unregistered).toBeUndefined();
    });

    it('ADVERTISED: Cannot register duplicate hooks', () => {
      const registry = createHookRegistry();

      const hook = defineHook({
        name: 'duplicate',
        trigger: 'before-add',
        validate: _quad => true,
      });

      registerHook(registry, hook);

      // Advertised: Second registration should fail or overwrite
      expect(() => {
        registerHook(registry, hook);
      }).toThrow(); // Or it may silently overwrite - test will reveal
    });

    it('ADVERTISED: Can query hooks by trigger type', () => {
      const registry = createHookRegistry();

      const beforeHook = defineHook({
        name: 'before',
        trigger: 'before-add',
        validate: _quad => true,
      });

      const afterHook = defineHook({
        name: 'after',
        trigger: 'after-add',
        validate: _quad => true,
      });

      registerHook(registry, beforeHook);
      registerHook(registry, afterHook);

      // Advertised: getHooksByTrigger function
      if (getHooksByTrigger) {
        const beforeHooks = getHooksByTrigger(registry, 'before-add');
        expect(beforeHooks.length).toBe(1);
      }
    });
  });

  describe('Built-in Hooks - Advertised Features', () => {
    it('ADVERTISED: Built-in validation hooks exist and work', () => {
      if (!validateSubjectIRI || !validatePredicateIRI) {
        throw new Error('ADVERTISED built-in hooks NOT exported!');
      }

      const validQuad = {
        subject: { value: 'http://example.org/s', termType: 'NamedNode' },
        predicate: { value: 'http://example.org/p', termType: 'NamedNode' },
        object: { value: 'object', termType: 'Literal' },
      };

      const result1 = executeHook(validateSubjectIRI, validQuad);
      expect(result1.valid).toBe(true);

      const result2 = executeHook(validatePredicateIRI, validQuad);
      expect(result2.valid).toBe(true);
    });

    it('ADVERTISED: Built-in transformation hooks normalize data', () => {
      if (!normalizeLanguageTag) {
        throw new Error('ADVERTISED normalizeLanguageTag hook NOT exported!');
      }

      const quad = {
        subject: { value: 'http://example.org/s', termType: 'NamedNode' },
        predicate: { value: 'http://example.org/p', termType: 'NamedNode' },
        object: { value: 'Hello', termType: 'Literal', language: 'EN' }, // Uppercase - should normalize
      };

      const result = executeHook(normalizeLanguageTag, quad);
      expect(result.quad.object.language).toBe('en');
    });
  });

  describe('Hook Composition - Advertised Features', () => {
    it('ADVERTISED: Can compose multiple validation and transformation hooks', () => {
      const validate1 = defineHook({
        name: 'validate-subject',
        trigger: 'before-add',
        validate: quad => quad.subject.termType === 'NamedNode',
      });

      const validate2 = defineHook({
        name: 'validate-predicate',
        trigger: 'before-add',
        validate: quad => quad.predicate.termType === 'NamedNode',
      });

      const transform1 = defineHook({
        name: 'transform-object',
        trigger: 'before-add',
        transform: quad => ({
          ...quad,
          object: { ...quad.object, value: quad.object.value.trim() },
        }),
      });

      const testQuad = {
        subject: { value: 'http://example.org/s', termType: 'NamedNode' },
        predicate: { value: 'http://example.org/p', termType: 'NamedNode' },
        object: { value: '  spaced text  ', termType: 'Literal' },
      };

      const result = executeHookChain([validate1, validate2, transform1], testQuad);
      expect(result.valid).toBe(true);
      expect(result.quad.object.value).toBe('spaced text');
    });
  });
});
