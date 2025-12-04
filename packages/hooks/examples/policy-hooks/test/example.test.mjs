/**
 * @file Tests for Policy Hooks Example
 * @vitest-environment node
 */

import { describe, it, expect } from 'vitest';
import { namedNode, literal, quad, createStore } from '@unrdf/core';
import { executeHook, executeHooksByTrigger } from '@unrdf/hooks';
import {
  aclPolicy,
  dataTypePolicy,
  privacyPolicy,
  provenancePolicy,
  setupPolicyRegistry,
} from '../src/index.mjs';

describe('Policy Hooks Example', () => {
  describe('ACL Policy', () => {
    it('should allow quads from trusted namespaces', () => {
      const store = createStore();
      const q = quad(
        namedNode('http://example.org/alice'),
        namedNode('http://xmlns.com/foaf/0.1/name'),
        literal('Alice')
      );

      const result = executeHook(aclPolicy, q);
      expect(result.valid).toBe(true);
    });

    it('should reject quads from untrusted namespaces', () => {
      const store = createStore();
      const q = quad(
        namedNode('http://untrusted.org/bob'),
        namedNode('http://untrusted.org/property'),
        literal('Bob')
      );

      const result = executeHook(aclPolicy, q);
      expect(result.valid).toBe(false);
    });
  });

  describe('Data Type Policy', () => {
    it('should allow valid age values', () => {
      const store = createStore();
      const q = quad(
        namedNode('http://example.org/alice'),
        namedNode('http://xmlns.com/foaf/0.1/age'),
        literal('30')
      );

      const result = executeHook(dataTypePolicy, q);
      expect(result.valid).toBe(true);
    });

    it('should reject invalid age values', () => {
      const store = createStore();
      const q = quad(
        namedNode('http://example.org/bob'),
        namedNode('http://xmlns.com/foaf/0.1/age'),
        literal('999')
      );

      const result = executeHook(dataTypePolicy, q);
      expect(result.valid).toBe(false);
    });

    it('should reject non-integer age values', () => {
      const store = createStore();
      const q = quad(
        namedNode('http://example.org/charlie'),
        namedNode('http://xmlns.com/foaf/0.1/age'),
        literal('not-a-number')
      );

      const result = executeHook(dataTypePolicy, q);
      expect(result.valid).toBe(false);
    });
  });

  describe('Privacy Policy', () => {
    it('should redact email addresses', () => {
      const store = createStore();
      const q = quad(
        namedNode('http://example.org/alice'),
        namedNode('http://xmlns.com/foaf/0.1/mbox'),
        literal('alice@example.org')
      );

      const result = executeHook(privacyPolicy, q);
      expect(result.valid).toBe(true);
      expect(result.quad.object.value).toBe('[REDACTED]');
    });

    it('should not transform non-email predicates', () => {
      const store = createStore();
      const q = quad(
        namedNode('http://example.org/alice'),
        namedNode('http://xmlns.com/foaf/0.1/name'),
        literal('Alice')
      );

      const result = executeHook(privacyPolicy, q);
      expect(result.valid).toBe(true);
      expect(result.quad.object.value).toBe('Alice');
    });
  });

  describe('Provenance Policy', () => {
    it('should require named graph for provenance', () => {
      const store = createStore();
      const q = quad(
        namedNode('http://example.org/alice'),
        namedNode('http://xmlns.com/foaf/0.1/name'),
        literal('Alice'),
        namedNode('http://example.org/graph1')
      );

      const result = executeHook(provenancePolicy, q);
      expect(result.valid).toBe(true);
    });

    it('should reject quads without provenance', () => {
      const store = createStore();
      const q = quad(
        namedNode('http://example.org/bob'),
        namedNode('http://xmlns.com/foaf/0.1/name'),
        literal('Bob')
      );

      const result = executeHook(provenancePolicy, q);
      expect(result.valid).toBe(false);
    });
  });

  describe('Policy Registry', () => {
    it('should execute all registered policies', () => {
      const registry = setupPolicyRegistry();
      const store = createStore();
      const q = quad(
        namedNode('http://example.org/alice'),
        namedNode('http://xmlns.com/foaf/0.1/name'),
        literal('Alice'),
        namedNode('http://example.org/graph1')
      );

      const hooks = [aclPolicy, dataTypePolicy, privacyPolicy, provenancePolicy];
      const results = executeHooksByTrigger(hooks, 'before-add', q);
      expect(results.results.length).toBe(4);
      expect(results.results.filter(r => r.valid).length).toBe(4);
      expect(results.valid).toBe(true);
    });

    it('should detect policy violations', () => {
      const registry = setupPolicyRegistry();
      const store = createStore();
      const q = quad(
        namedNode('http://untrusted.org/bob'),
        namedNode('http://untrusted.org/property'),
        literal('Bob')
      );

      const hooks = [aclPolicy, dataTypePolicy, privacyPolicy, provenancePolicy];
      const results = executeHooksByTrigger(hooks, 'before-add', q);
      // Chain stops on first failure, so we only get results up to the failed hook
      expect(results.results.length).toBeGreaterThan(0);
      expect(results.valid).toBe(false);
      expect(results.results[0].hookName).toBe('acl-policy');
      expect(results.results[0].valid).toBe(false);
    });
  });
});
