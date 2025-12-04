/**
 * @file Tests for Hook Chains Example
 * @vitest-environment node
 */

import { describe, it, expect } from 'vitest';
import { namedNode, literal, quad, createStore } from '@unrdf/core';
import { executeHookChain } from '@unrdf/hooks';
import {
  validateIRIs,
  normalizeWhitespace,
  validateLiteralLength,
  addProvenance,
  finalValidation,
  dataCleaningChain,
  qualityAssuranceChain,
  completeProcessingChain,
} from '../src/index.mjs';

describe('Hook Chains Example', () => {
  describe('Individual Hooks', () => {
    it('should validate well-formed IRIs', () => {
      const store = createStore();
      const q = quad(
        namedNode('http://example.org/alice'),
        namedNode('http://xmlns.com/foaf/0.1/name'),
        literal('Alice')
      );

      const result = executeHookChain([validateIRIs], q, { collectResults: true });
      expect(result.valid).toBe(true);
      expect(result.results[0].valid).toBe(true);
    });

    it('should normalize whitespace in literals', () => {
      const store = createStore();
      const q = quad(
        namedNode('http://example.org/alice'),
        namedNode('http://xmlns.com/foaf/0.1/name'),
        literal('  Alice   Smith  ')
      );

      const result = executeHookChain([normalizeWhitespace], q);
      expect(result.valid).toBe(true);
      expect(result.quad.object.value).toBe('Alice Smith');
    });

    it('should validate literal length constraints', () => {
      const store = createStore();
      const shortQuad = quad(
        namedNode('http://example.org/alice'),
        namedNode('http://xmlns.com/foaf/0.1/name'),
        literal('Alice')
      );

      const result1 = executeHookChain([validateLiteralLength], shortQuad);
      expect(result1.valid).toBe(true);

      const longQuad = quad(
        namedNode('http://example.org/bob'),
        namedNode('http://xmlns.com/foaf/0.1/description'),
        literal('x'.repeat(1500))
      );

      const result2 = executeHookChain([validateLiteralLength], longQuad);
      expect(result2.valid).toBe(false);
    });

    it('should add provenance metadata', () => {
      const store = createStore();
      const q = quad(
        namedNode('http://example.org/alice'),
        namedNode('http://xmlns.com/foaf/0.1/name'),
        literal('Alice')
      );

      const result = executeHookChain([addProvenance], q);
      expect(result.valid).toBe(true);
      expect(result.quad.graph.termType).toBe('NamedNode');
      expect(result.quad.graph.value).toMatch(/^http:\/\/example\.org\/provenance\/\d+$/);
    });

    it('should perform final validation', () => {
      const store = createStore();
      const validQuad = quad(
        namedNode('http://example.org/alice'),
        namedNode('http://xmlns.com/foaf/0.1/name'),
        literal('Alice')
      );

      const result = executeHookChain([finalValidation], validQuad);
      expect(result.valid).toBe(true);
    });
  });

  describe('Data Cleaning Chain', () => {
    it('should clean and normalize dirty data', () => {
      const store = createStore();
      const dirtyQuad = quad(
        namedNode('http://example.org/alice'),
        namedNode('http://xmlns.com/foaf/0.1/name'),
        literal('  Alice   Smith  ')
      );

      const result = executeHookChain(dataCleaningChain, dirtyQuad, { collectResults: true });
      expect(result.valid).toBe(true);
      expect(result.results.length).toBe(3);
      expect(result.quad.object.value).toBe('Alice Smith');
    });

    it('should reject invalid data after cleaning', () => {
      const store = createStore();
      const invalidQuad = quad(
        namedNode('http://example.org/bob'),
        namedNode('http://xmlns.com/foaf/0.1/description'),
        literal('  ' + 'x'.repeat(1500) + '  ')
      );

      const result = executeHookChain(dataCleaningChain, invalidQuad, { collectResults: true });
      expect(result.valid).toBe(false);
      expect(result.results.some(r => r.hookName === 'validate-literal-length')).toBe(true);
    });
  });

  describe('Quality Assurance Chain', () => {
    it('should validate high-quality data', () => {
      const store = createStore();
      const q = quad(
        namedNode('http://example.org/alice'),
        namedNode('http://xmlns.com/foaf/0.1/name'),
        literal('Alice')
      );

      const result = executeHookChain(qualityAssuranceChain, q, { collectResults: true });
      expect(result.valid).toBe(true);
      expect(result.results.every(r => r.valid)).toBe(true);
    });

    it('should detect quality issues early', () => {
      const store = createStore();
      const invalidQuad = quad(
        namedNode('http://example.org/bob'),
        namedNode('http://xmlns.com/foaf/0.1/description'),
        literal('x'.repeat(1500))
      );

      const result = executeHookChain(qualityAssuranceChain, invalidQuad, { collectResults: true });
      expect(result.valid).toBe(false);
      // Chain executes until failure, so we get results up to and including the failed hook
      expect(result.results.length).toBeLessThanOrEqual(qualityAssuranceChain.length);
      // Verify a validation failed
      expect(result.results.some(r => !r.valid)).toBe(true);
    });
  });

  describe('Complete Processing Chain', () => {
    it('should process quad through all stages', () => {
      const store = createStore();
      const rawQuad = quad(
        namedNode('http://example.org/charlie'),
        namedNode('http://xmlns.com/foaf/0.1/nick'),
        literal('  Chuck  ')
      );

      const result = executeHookChain(completeProcessingChain, rawQuad, { collectResults: true });
      expect(result.valid).toBe(true);
      expect(result.results.length).toBe(5);
      expect(result.quad.object.value).toBe('Chuck');
      expect(result.quad.graph.termType).toBe('NamedNode');
    });

    it('should terminate on first validation failure', () => {
      const store = createStore();
      // Use a literal as subject (which is invalid for RDF) to trigger validation failure
      const invalidQuad = quad(
        namedNode('http://example.org/bob'),
        namedNode('http://xmlns.com/foaf/0.1/description'),
        literal('x'.repeat(1500)) // Exceeds length limit to trigger validation failure
      );

      const result = executeHookChain(completeProcessingChain, invalidQuad, { collectResults: true });
      expect(result.valid).toBe(false);
      expect(result.results.length).toBeLessThan(completeProcessingChain.length);
      // Expect literal length validation to fail
      expect(result.results.some(r => !r.valid)).toBe(true);
    });

    it('should apply all transformations in order', () => {
      const store = createStore();
      const rawQuad = quad(
        namedNode('http://example.org/dana'),
        namedNode('http://xmlns.com/foaf/0.1/name'),
        literal('  Dana   Lee  ')
      );

      const result = executeHookChain(completeProcessingChain, rawQuad);
      expect(result.valid).toBe(true);

      // Check that transformations occurred
      expect(result.quad.object.value).toBe('Dana Lee');
      expect(result.quad.graph.termType).toBe('NamedNode');
    });
  });

  describe('Chain Behavior', () => {
    it('should preserve quad through validation-only hooks', () => {
      const store = createStore();
      const q = quad(
        namedNode('http://example.org/alice'),
        namedNode('http://xmlns.com/foaf/0.1/name'),
        literal('Alice')
      );

      const validationChain = [validateIRIs, validateLiteralLength, finalValidation];
      const result = executeHookChain(validationChain, q);

      expect(result.valid).toBe(true);
      expect(result.quad).toEqual(q);
    });

    it('should accumulate transformations through chain', () => {
      const store = createStore();
      const q = quad(
        namedNode('http://example.org/eve'),
        namedNode('http://xmlns.com/foaf/0.1/name'),
        literal('  Eve   ')
      );

      const transformChain = [normalizeWhitespace, addProvenance];
      const result = executeHookChain(transformChain, q);

      expect(result.valid).toBe(true);
      expect(result.quad.object.value).toBe('Eve');
      expect(result.quad.graph.termType).toBe('NamedNode');
    });

    it('should stop execution on first failure', () => {
      const store = createStore();
      const invalidQuad = quad(
        namedNode('http://example.org/frank'),
        namedNode('http://xmlns.com/foaf/0.1/description'),
        literal('x'.repeat(1500))
      );

      const result = executeHookChain(completeProcessingChain, invalidQuad, { collectResults: true });
      expect(result.valid).toBe(false);

      const firstFailureIndex = result.results.findIndex(r => !r.valid);
      expect(result.results.length).toBe(firstFailureIndex + 1);
    });
  });
});
