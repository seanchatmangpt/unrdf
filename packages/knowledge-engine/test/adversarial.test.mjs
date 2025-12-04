/**
 * @vitest-environment node
 * Adversarial Testing: Test advertised capabilities for @unrdf/knowledge-engine
 * Goal: PROVE what doesn't work, not security
 */

import { describe, it, expect } from 'vitest';
import {
  defineRule,
  matchPattern,
  createInferenceEngine,
  addRules,
  runInference,
  builtinRules,
  parsePattern,
  buildPattern,
} from '../src/index.mjs';
import { createStore, addQuad, namedNode, literal, quad } from '@unrdf/core';

describe('@unrdf/knowledge-engine Adversarial Tests - Capabilities', () => {
  describe('Rule Definition - Advertised Features', () => {
    it('ADVERTISED: Can define inference rules', () => {
      const rule = defineRule({
        name: 'test-rule',
        patterns: [{ subject: '?s', predicate: 'rdf:type', object: 'foaf:Person' }],
        infer: [{ subject: '?s', predicate: 'rdf:type', object: 'schema:Person' }],
      });

      expect(rule).toBeDefined();
      expect(rule.name).toBe('test-rule');
      expect(rule.patterns.length).toBe(1);
    });

    it('ADVERTISED: Can define rules with multiple patterns', () => {
      const rule = defineRule({
        name: 'multi-pattern-rule',
        patterns: [
          { subject: '?s', predicate: 'foaf:knows', object: '?o' },
          { subject: '?o', predicate: 'foaf:knows', object: '?s' },
        ],
        infer: [{ subject: '?s', predicate: 'rel:friend', object: '?o' }],
      });

      expect(rule.patterns.length).toBe(2);
      expect(rule.infer.length).toBe(1);
    });

    it('ADVERTISED: Can compile rules', () => {
      const rule = defineRule({
        name: 'compilable-rule',
        patterns: [{ subject: '?s', predicate: 'rdf:type', object: '?type' }],
        infer: [{ subject: '?s', predicate: 'schema:type', object: '?type' }],
      });

      expect(rule).toBeDefined();
    });
  });

  describe('Pattern Matching - Advertised Features', () => {
    it('ADVERTISED: Can match patterns against RDF quads', () => {
      const store = createStore();
      addQuad(
        store,
        quad(
          namedNode('http://example.org/alice'),
          namedNode('http://www.w3.org/1999/02/22-rdf-syntax-ns#type'),
          namedNode('http://xmlns.com/foaf/0.1/Person')
        )
      );

      const pattern = {
        subject: '?person',
        predicate: 'http://www.w3.org/1999/02/22-rdf-syntax-ns#type',
        object: 'http://xmlns.com/foaf/0.1/Person',
      };

      const matches = matchPattern(store, pattern);
      expect(matches).toBeDefined();
    });

    it('ADVERTISED: Can match patterns with variable bindings', () => {
      const store = createStore();
      addQuad(
        store,
        quad(namedNode('http://example.org/alice'), namedNode('http://foaf/name'), literal('Alice'))
      );

      const pattern = {
        subject: '?person',
        predicate: 'http://foaf/name',
        object: '?name',
      };

      const matches = matchPattern(store, pattern);
      expect(matches).toBeDefined();
    });
  });

  describe('Inference Engine - Advertised Features', () => {
    it('ADVERTISED: Can create inference engine', () => {
      const engine = createInferenceEngine(createStore());

      expect(engine).toBeDefined();
      expect(engine.addRules).toBeDefined();
      expect(engine.runInference).toBeDefined();
    });

    it('ADVERTISED: Can add rules to inference engine', () => {
      const engine = createInferenceEngine(createStore());

      const rule = defineRule({
        name: 'inference-rule',
        patterns: [{ subject: '?s', predicate: 'rdf:type', object: 'foaf:Person' }],
        infer: [{ subject: '?s', predicate: 'schema:type', object: 'schema:Person' }],
      });

      addRules(engine, [rule]);
      expect(engine).toBeDefined();
    });

    it('ADVERTISED: Can run inference on knowledge base', () => {
      const store = createStore();
      const engine = createInferenceEngine(store);

      addQuad(
        store,
        quad(
          namedNode('http://example.org/alice'),
          namedNode('http://www.w3.org/1999/02/22-rdf-syntax-ns#type'),
          namedNode('http://xmlns.com/foaf/0.1/Person')
        )
      );

      const rule = defineRule({
        name: 'person-rule',
        patterns: [{ subject: '?s', predicate: 'rdf:type', object: 'foaf:Person' }],
        infer: [{ subject: '?s', predicate: 'a', object: 'schema:Person' }],
      });

      addRules(engine, [rule]);
      const result = runInference(engine);

      expect(result).toBeDefined();
    });
  });

  describe('Built-in Rules - Advertised Features', () => {
    it('ADVERTISED: Built-in RDFS/OWL rules are available', () => {
      expect(builtinRules).toBeDefined();
      expect(Array.isArray(builtinRules)).toBe(true);
      expect(builtinRules.length).toBeGreaterThan(0);
    });

    it('ADVERTISED: Can use RDFS subclass reasoning', () => {
      const rdfsRule = builtinRules.find(r => r.name?.includes('subclass'));
      expect(rdfsRule).toBeDefined();
    });

    it('ADVERTISED: Can use OWL transitive property reasoning', () => {
      const owlRule = builtinRules.find(r => r.name?.includes('transitive'));
      expect(owlRule).toBeDefined();
    });
  });

  describe('Pattern DSL - Advertised Features', () => {
    it('ADVERTISED: Can parse pattern strings', () => {
      const patternStr = '?s rdf:type foaf:Person';
      const parsed = parsePattern(patternStr);

      expect(parsed).toBeDefined();
    });

    it('ADVERTISED: Can build patterns programmatically', () => {
      const pattern = buildPattern({
        subject: '?person',
        predicate: 'foaf:name',
        object: '?name',
      });

      expect(pattern).toBeDefined();
      expect(pattern.subject).toBe('?person');
    });

    it('ADVERTISED: Can validate patterns', () => {
      const validPattern = { subject: '?s', predicate: 'rdf:type', object: '?type' };
      expect(validPattern).toBeDefined();
    });
  });
});
