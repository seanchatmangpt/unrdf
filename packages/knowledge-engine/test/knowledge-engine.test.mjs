/**
 * @vitest-environment node
 */
import { describe, it, expect, beforeEach } from 'vitest';
import { createStore, addQuad, quad, namedNode, literal, countQuads } from '@unrdf/core';
import {
  defineRule,
  compileRule,
  getRule,
  getAllRules,
  clearRules,
  matchPattern,
  matchPatternWithBindings,
  hasMatch,
  matchMultiplePatterns,
  createInferenceEngine,
  addRules,
  runInference,
  getInferredQuads,
  rdfsSubClassRule,
  rdfsSubPropertyRule,
  rdfsDomainRule,
  rdfsRangeRule,
  getBuiltinRules,
  parsePattern,
  patternToSparql,
  buildPattern,
  isValidPattern,
} from '../src/index.mjs';

describe('@unrdf/knowledge-engine', () => {
  beforeEach(() => {
    clearRules();
  });

  describe('Rule Definition', () => {
    it('should define a simple rule', () => {
      const rule = defineRule({
        name: 'test-rule',
        pattern: { subject: '?x', predicate: 'rdf:type', object: 'foaf:Person' },
        consequent: { subject: '?x', predicate: 'rdfs:label', object: 'Person' },
        salience: 50,
      });

      expect(rule).toBeDefined();
      expect(rule.name).toBe('test-rule');
      expect(rule.salience).toBe(50);
    });

    it('should retrieve a defined rule', () => {
      defineRule({
        name: 'test-retrieve',
        pattern: { subject: '?x', predicate: 'rdf:type', object: 'foaf:Person' },
        consequent: { subject: '?x', predicate: 'rdfs:label', object: 'Person' },
      });

      const retrieved = getRule('test-retrieve');
      expect(retrieved).toBeDefined();
      expect(retrieved.name).toBe('test-retrieve');
    });

    it('should compile a rule', () => {
      const rule = defineRule({
        name: 'compile-test',
        pattern: { subject: '?x', predicate: 'rdf:type', object: 'foaf:Person' },
        consequent: { subject: '?x', predicate: 'rdfs:label', object: 'Person' },
      });

      const compiled = compileRule(rule);
      expect(compiled.compiled).toBe(true);
    });

    it('should get all rules', () => {
      defineRule({
        name: 'rule1',
        pattern: { subject: '?x', predicate: 'rdf:type', object: 'foaf:Person' },
        consequent: { subject: '?x', predicate: 'rdfs:label', object: 'Person' },
      });

      defineRule({
        name: 'rule2',
        pattern: { subject: '?x', predicate: 'rdf:type', object: 'foaf:Organization' },
        consequent: { subject: '?x', predicate: 'rdfs:label', object: 'Org' },
      });

      const all = getAllRules();
      expect(all.length).toBeGreaterThanOrEqual(2);
    });
  });

  describe('Pattern Matching', () => {
    let store;

    beforeEach(() => {
      store = createStore();
      addQuad(
        store,
        quad(
          namedNode('http://example.org/alice'),
          namedNode('http://www.w3.org/1999/02/22-rdf-syntax-ns#type'),
          namedNode('http://xmlns.com/foaf/0.1/Person')
        )
      );
      addQuad(
        store,
        quad(
          namedNode('http://example.org/alice'),
          namedNode('http://xmlns.com/foaf/0.1/name'),
          literal('Alice')
        )
      );
    });

    it('should match a simple pattern', () => {
      const matches = matchPattern(store, {
        subject: null,
        predicate: 'http://www.w3.org/1999/02/22-rdf-syntax-ns#type',
        object: null,
      });

      expect(matches.length).toBeGreaterThanOrEqual(1);
    });

    it('should match pattern with bindings', () => {
      const bindings = matchPatternWithBindings(store, {
        subject: '?person',
        predicate: 'http://xmlns.com/foaf/0.1/name',
        object: '?name',
      });

      expect(bindings.length).toBeGreaterThanOrEqual(1);
      expect(bindings[0]).toHaveProperty('person');
      expect(bindings[0]).toHaveProperty('name');
    });

    it('should check if pattern has match', () => {
      const hasMatches = hasMatch(store, {
        subject: null,
        predicate: 'http://xmlns.com/foaf/0.1/name',
        object: null,
      });

      expect(hasMatches).toBe(true);
    });

    it('should match multiple patterns', () => {
      const bindings = matchMultiplePatterns(store, [
        {
          subject: '?x',
          predicate: 'http://www.w3.org/1999/02/22-rdf-syntax-ns#type',
          object: null,
        },
        { subject: '?x', predicate: 'http://xmlns.com/foaf/0.1/name', object: '?name' },
      ]);

      expect(bindings.length).toBeGreaterThanOrEqual(1);
    });
  });

  describe('Inference Engine', () => {
    let store;
    let engine;

    beforeEach(() => {
      store = createStore();
      engine = createInferenceEngine(store);
    });

    it('should create an inference engine', () => {
      expect(engine).toBeDefined();
      expect(engine.store).toBe(store);
      expect(engine.rules).toEqual([]);
    });

    it('should add rules to engine', () => {
      const rule = defineRule({
        name: 'test-rule',
        pattern: { subject: '?x', predicate: 'rdf:type', object: 'foaf:Person' },
        consequent: { subject: '?x', predicate: 'rdfs:label', object: 'Person' },
      });

      addRules(engine, [rule]);
      expect(engine.rules.length).toBe(1);
    });

    it('should run inference with RDFS subclass rule', () => {
      addQuad(
        store,
        quad(
          namedNode('http://example.org/Student'),
          namedNode('http://www.w3.org/2000/01/rdf-schema#subClassOf'),
          namedNode('http://xmlns.com/foaf/0.1/Person')
        )
      );

      addQuad(
        store,
        quad(
          namedNode('http://example.org/alice'),
          namedNode('http://www.w3.org/1999/02/22-rdf-syntax-ns#type'),
          namedNode('http://example.org/Student')
        )
      );

      addRules(engine, [rdfsSubClassRule]);

      const initialCount = countQuads(store);
      const results = runInference(engine);

      expect(results.inferredCount).toBeGreaterThanOrEqual(1);
      expect(countQuads(store)).toBeGreaterThan(initialCount);
    });

    it('should detect fixpoint', () => {
      addQuad(
        store,
        quad(
          namedNode('http://example.org/alice'),
          namedNode('http://www.w3.org/1999/02/22-rdf-syntax-ns#type'),
          namedNode('http://xmlns.com/foaf/0.1/Person')
        )
      );

      const rule = defineRule({
        name: 'label-rule',
        pattern: {
          subject: '?x',
          predicate: 'http://www.w3.org/1999/02/22-rdf-syntax-ns#type',
          object: 'http://xmlns.com/foaf/0.1/Person',
        },
        consequent: {
          subject: '?x',
          predicate: 'http://www.w3.org/2000/01/rdf-schema#label',
          object: literal('Person'),
        },
      });

      addRules(engine, [rule]);
      const results = runInference(engine);

      expect(results.fixpointReached).toBe(true);
    });

    it('should get inferred quads', () => {
      addQuad(
        store,
        quad(
          namedNode('http://example.org/Student'),
          namedNode('http://www.w3.org/2000/01/rdf-schema#subClassOf'),
          namedNode('http://xmlns.com/foaf/0.1/Person')
        )
      );

      addQuad(
        store,
        quad(
          namedNode('http://example.org/alice'),
          namedNode('http://www.w3.org/1999/02/22-rdf-syntax-ns#type'),
          namedNode('http://example.org/Student')
        )
      );

      addRules(engine, [rdfsSubClassRule]);
      runInference(engine);

      const inferred = getInferredQuads(engine);
      expect(inferred.length).toBeGreaterThanOrEqual(1);
    });
  });

  describe('Built-in Rules', () => {
    it('should have RDFS subclass rule', () => {
      expect(rdfsSubClassRule).toBeDefined();
      expect(rdfsSubClassRule.name).toBe('rdfs:subClassOf');
    });

    it('should have RDFS subproperty rule', () => {
      expect(rdfsSubPropertyRule).toBeDefined();
      expect(rdfsSubPropertyRule.name).toBe('rdfs:subPropertyOf');
    });

    it('should have RDFS domain rule', () => {
      expect(rdfsDomainRule).toBeDefined();
      expect(rdfsDomainRule.name).toBe('rdfs:domain');
    });

    it('should have RDFS range rule', () => {
      expect(rdfsRangeRule).toBeDefined();
      expect(rdfsRangeRule.name).toBe('rdfs:range');
    });

    it('should get all builtin rules', () => {
      const rules = getBuiltinRules();
      expect(rules.length).toBeGreaterThanOrEqual(7);
    });
  });

  describe('Pattern DSL', () => {
    it('should parse a simple pattern string', () => {
      const pattern = parsePattern('?x rdf:type foaf:Person');
      expect(pattern.subject).toBe('?x');
      expect(pattern.predicate).toBe('rdf:type');
      expect(pattern.object).toBe('foaf:Person');
    });

    it('should convert pattern to SPARQL', () => {
      const sparql = patternToSparql({
        subject: '?x',
        predicate: 'rdf:type',
        object: 'foaf:Person',
      });

      expect(sparql).toContain('?x');
      expect(sparql).toContain('rdf:type');
      expect(sparql).toContain('foaf:Person');
    });

    it('should build a pattern', () => {
      const pattern = buildPattern('?x', 'rdf:type', 'foaf:Person');
      expect(pattern.subject).toBe('?x');
      expect(pattern.predicate).toBe('rdf:type');
      expect(pattern.object).toBe('foaf:Person');
    });

    it('should validate a pattern', () => {
      const valid = isValidPattern({
        subject: '?x',
        predicate: 'rdf:type',
        object: 'foaf:Person',
      });

      expect(valid).toBe(true);
    });

    it('should reject invalid pattern', () => {
      const invalid = isValidPattern({ subject: '?x' });
      expect(invalid).toBe(false);
    });
  });

  describe('Integration Tests', () => {
    it('should perform complete inference workflow', () => {
      const store = createStore();

      addQuad(
        store,
        quad(
          namedNode('http://example.org/Student'),
          namedNode('http://www.w3.org/2000/01/rdf-schema#subClassOf'),
          namedNode('http://xmlns.com/foaf/0.1/Person')
        )
      );

      addQuad(
        store,
        quad(
          namedNode('http://example.org/alice'),
          namedNode('http://www.w3.org/1999/02/22-rdf-syntax-ns#type'),
          namedNode('http://example.org/Student')
        )
      );

      const engine = createInferenceEngine(store);
      addRules(engine, getBuiltinRules());

      const results = runInference(engine);

      expect(results.inferredCount).toBeGreaterThanOrEqual(1);
      expect(results.fixpointReached).toBe(true);
    });

    it('should handle multiple inference iterations', () => {
      const store = createStore();

      addQuad(
        store,
        quad(
          namedNode('http://example.org/Undergraduate'),
          namedNode('http://www.w3.org/2000/01/rdf-schema#subClassOf'),
          namedNode('http://example.org/Student')
        )
      );

      addQuad(
        store,
        quad(
          namedNode('http://example.org/Student'),
          namedNode('http://www.w3.org/2000/01/rdf-schema#subClassOf'),
          namedNode('http://xmlns.com/foaf/0.1/Person')
        )
      );

      addQuad(
        store,
        quad(
          namedNode('http://example.org/bob'),
          namedNode('http://www.w3.org/1999/02/22-rdf-syntax-ns#type'),
          namedNode('http://example.org/Undergraduate')
        )
      );

      const engine = createInferenceEngine(store);
      addRules(engine, [rdfsSubClassRule]);

      const results = runInference(engine);

      expect(results.iterations).toBeGreaterThan(1);
      expect(results.inferredCount).toBeGreaterThanOrEqual(2);
    });
  });
});
