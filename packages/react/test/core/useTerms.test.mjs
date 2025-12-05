/**
 * @fileoverview Tests for useTerms hook
 */

import { describe, it, expect } from 'vitest';
import { renderHook } from '@testing-library/react';
import { useTerms } from '../src/core/useTerms.mjs';
import { DataFactory } from 'n3';

const { namedNode, literal, _blankNode } = DataFactory;

describe('useTerms', () => {
  describe('Named Node Creation', () => {
    it('should create named node from IRI', () => {
      const { result } = renderHook(() => useTerms());

      const node = result.current.createNamedNode('http://example.org/test');

      expect(node.termType).toBe('NamedNode');
      expect(node.value).toBe('http://example.org/test');
    });

    it('should throw on non-string IRI', () => {
      const { result } = renderHook(() => useTerms());

      expect(() => {
        result.current.createNamedNode(123);
      }).toThrow(TypeError);
    });

    it('should create multiple different named nodes', () => {
      const { result } = renderHook(() => useTerms());

      const node1 = result.current.createNamedNode('http://ex.org/1');
      const node2 = result.current.createNamedNode('http://ex.org/2');

      expect(node1.value).not.toBe(node2.value);
    });
  });

  describe('Literal Creation', () => {
    it('should create plain literal', () => {
      const { result } = renderHook(() => useTerms());

      const lit = result.current.createLiteral('test value');

      expect(lit.termType).toBe('Literal');
      expect(lit.value).toBe('test value');
    });

    it('should create literal with language tag', () => {
      const { result } = renderHook(() => useTerms());

      const lit = result.current.createLiteral('hello', 'en');

      expect(lit.value).toBe('hello');
      expect(lit.language).toBe('en');
    });

    it('should create literal with datatype', () => {
      const { result } = renderHook(() => useTerms());

      const datatype = namedNode('http://www.w3.org/2001/XMLSchema#integer');
      const lit = result.current.createLiteral('42', datatype);

      expect(lit.value).toBe('42');
    });

    it('should convert non-string values to strings', () => {
      const { result } = renderHook(() => useTerms());

      const lit = result.current.createLiteral(123);

      expect(lit.value).toBe('123');
    });
  });

  describe('Blank Node Creation', () => {
    it('should create blank node', () => {
      const { result } = renderHook(() => useTerms());

      const bn = result.current.createBlankNode();

      expect(bn.termType).toBe('BlankNode');
    });

    it('should create blank node with ID', () => {
      const { result } = renderHook(() => useTerms());

      const bn = result.current.createBlankNode('n1');

      expect(bn.termType).toBe('BlankNode');
      expect(bn.value).toBe('n1');
    });

    it('should create different blank nodes', () => {
      const { result } = renderHook(() => useTerms());

      const bn1 = result.current.createBlankNode();
      const bn2 = result.current.createBlankNode();

      expect(bn1.value).not.toBe(bn2.value);
    });
  });

  describe('Quad Creation', () => {
    it('should create quad from strings', () => {
      const { result } = renderHook(() => useTerms());

      const q = result.current.createQuad('http://s', 'http://p', 'object');

      expect(q.subject.termType).toBe('NamedNode');
      expect(q.predicate.termType).toBe('NamedNode');
      expect(q.object.termType).toBe('Literal');
    });

    it('should create quad from terms', () => {
      const { result } = renderHook(() => useTerms());

      const s = namedNode('http://s');
      const p = namedNode('http://p');
      const o = literal('object');

      const q = result.current.createQuad(s, p, o);

      expect(q.subject).toBe(s);
      expect(q.predicate).toBe(p);
      expect(q.object).toBe(o);
    });

    it('should create quad with named graph', () => {
      const { result } = renderHook(() => useTerms());

      const q = result.current.createQuad('http://s', 'http://p', 'o', 'http://g');

      expect(q.graph.termType).toBe('NamedNode');
      expect(q.graph.value).toBe('http://g');
    });

    it('should use default graph when graph not specified', () => {
      const { result } = renderHook(() => useTerms());

      const q = result.current.createQuad('http://s', 'http://p', 'o');

      expect(q.graph.termType).toBe('DefaultGraph');
    });
  });

  describe('Variable Creation', () => {
    it('should create variable', () => {
      const { result } = renderHook(() => useTerms());

      const v = result.current.createVariable('x');

      expect(v.termType).toBe('Variable');
      expect(v.value).toBe('x');
    });
  });

  describe('Term Type Checking', () => {
    it('should identify named node', () => {
      const { result } = renderHook(() => useTerms());

      const node = result.current.createNamedNode('http://test');

      expect(result.current.isNamedNode(node)).toBe(true);
      expect(result.current.isLiteral(node)).toBe(false);
      expect(result.current.isBlankNode(node)).toBe(false);
    });

    it('should identify literal', () => {
      const { result } = renderHook(() => useTerms());

      const lit = result.current.createLiteral('test');

      expect(result.current.isLiteral(lit)).toBe(true);
      expect(result.current.isNamedNode(lit)).toBe(false);
      expect(result.current.isBlankNode(lit)).toBe(false);
    });

    it('should identify blank node', () => {
      const { result } = renderHook(() => useTerms());

      const bn = result.current.createBlankNode();

      expect(result.current.isBlankNode(bn)).toBe(true);
      expect(result.current.isNamedNode(bn)).toBe(false);
      expect(result.current.isLiteral(bn)).toBe(false);
    });

    it('should handle null/undefined in type checks', () => {
      const { result } = renderHook(() => useTerms());

      expect(result.current.isNamedNode(null)).toBe(false);
      expect(result.current.isLiteral(undefined)).toBe(false);
      expect(result.current.isBlankNode(null)).toBe(false);
    });
  });

  describe('Term to String Conversion', () => {
    it('should convert named node to string', () => {
      const { result } = renderHook(() => useTerms());

      const node = result.current.createNamedNode('http://example.org/test');
      const str = result.current.termToString(node);

      expect(str).toBe('http://example.org/test');
    });

    it('should convert literal to string', () => {
      const { result } = renderHook(() => useTerms());

      const lit = result.current.createLiteral('test value');
      const str = result.current.termToString(lit);

      expect(str).toBe('test value');
    });

    it('should handle null/undefined', () => {
      const { result } = renderHook(() => useTerms());

      expect(result.current.termToString(null)).toBe('');
      expect(result.current.termToString(undefined)).toBe('');
    });
  });

  describe('DataFactory Access', () => {
    it('should provide direct DataFactory access', () => {
      const { result } = renderHook(() => useTerms());

      expect(result.current.DataFactory).toBeDefined();
      expect(typeof result.current.DataFactory.namedNode).toBe('function');
    });
  });

  describe('Stability', () => {
    it('should return stable function references', () => {
      const { result, rerender } = renderHook(() => useTerms());

      const createNamedNode1 = result.current.createNamedNode;
      rerender();
      const createNamedNode2 = result.current.createNamedNode;

      expect(createNamedNode1).toBe(createNamedNode2);
    });
  });

  describe('Performance', () => {
    it('should create many terms efficiently', () => {
      const { result } = renderHook(() => useTerms());

      const start = performance.now();

      for (let i = 0; i < 10000; i++) {
        result.current.createNamedNode(`http://ex.org/${i}`);
      }

      const duration = performance.now() - start;

      expect(duration).toBeLessThan(1000);
    });
  });

  describe('Edge Cases', () => {
    it('should handle empty string IRI', () => {
      const { result } = renderHook(() => useTerms());

      const node = result.current.createNamedNode('');

      expect(node.value).toBe('');
    });

    it('should handle special characters in literal', () => {
      const { result } = renderHook(() => useTerms());

      const lit = result.current.createLiteral('test\n\r\t"\'');

      expect(lit.value).toContain('\n');
      expect(lit.value).toContain('"');
    });
  });
});
