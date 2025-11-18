/**
 * @fileoverview Tests for React hooks core functionality
 */

import { describe, it, expect, beforeEach } from 'vitest';
import { renderHook, act } from '@testing-library/react';
import { useStore } from '../../src/react-hooks/core/useStore.mjs';
import { useTerms } from '../../src/react-hooks/core/useTerms.mjs';
import { DataFactory } from 'n3';

const { namedNode, literal } = DataFactory;

describe('useStore', () => {
  it('should initialize with empty store', () => {
    const { result } = renderHook(() => useStore());
    expect(result.current.size).toBe(0);
    expect(result.current.isEmpty).toBe(true);
  });

  it('should add quad to store', () => {
    const { result } = renderHook(() => {
      const store = useStore();
      const terms = useTerms();
      return { store, terms };
    });

    act(() => {
      const quad = result.current.terms.quad(
        namedNode('http://example.org/s'),
        namedNode('http://example.org/p'),
        literal('value')
      );
      result.current.store.addQuad(quad);
    });

    expect(result.current.store.size).toBe(1);
  });

  it('should remove quad from store', () => {
    const { result } = renderHook(() => {
      const store = useStore();
      const terms = useTerms();
      return { store, terms };
    });

    const quad = result.current.terms.quad(
      namedNode('http://example.org/s'),
      namedNode('http://example.org/p'),
      literal('value')
    );

    act(() => {
      result.current.store.addQuad(quad);
    });

    expect(result.current.store.size).toBe(1);

    act(() => {
      result.current.store.removeQuad(quad);
    });

    expect(result.current.store.size).toBe(0);
  });

  it('should clear store', () => {
    const { result } = renderHook(() => useStore());

    act(() => {
      result.current.addQuad(
        result.current.store.quad(
          namedNode('http://example.org/s'),
          namedNode('http://example.org/p'),
          literal('value')
        )
      );
    });

    expect(result.current.size).toBeGreaterThan(0);

    act(() => {
      result.current.clear();
    });

    expect(result.current.size).toBe(0);
  });
});

describe('useTerms', () => {
  it('should create named node', () => {
    const { result } = renderHook(() => useTerms());

    const node = result.current.namedNode('http://example.org/test');

    expect(node.termType).toBe('NamedNode');
    expect(node.value).toBe('http://example.org/test');
  });

  it('should create literal', () => {
    const { result } = renderHook(() => useTerms());

    const lit = result.current.literal('test', 'en');

    expect(lit.termType).toBe('Literal');
    expect(lit.value).toBe('test');
    expect(lit.language).toBe('en');
  });

  it('should create quad', () => {
    const { result } = renderHook(() => useTerms());

    const quad = result.current.quad(
      result.current.namedNode('http://example.org/s'),
      result.current.namedNode('http://example.org/p'),
      result.current.literal('value')
    );

    expect(quad.subject.value).toBe('http://example.org/s');
    expect(quad.predicate.value).toBe('http://example.org/p');
    expect(quad.object.value).toBe('value');
  });

  it('should expand prefixed names', () => {
    const { result } = renderHook(() =>
      useTerms({
        prefixes: {
          foaf: 'http://xmlns.com/foaf/0.1/'
        }
      })
    );

    const expanded = result.current.expand('foaf:name');
    expect(expanded).toBe('http://xmlns.com/foaf/0.1/name');
  });

  it('should check term types', () => {
    const { result } = renderHook(() => useTerms());

    const node = result.current.namedNode('http://example.org/test');
    const lit = result.current.literal('test');
    const blank = result.current.blankNode();

    expect(result.current.isNamedNode(node)).toBe(true);
    expect(result.current.isLiteral(lit)).toBe(true);
    expect(result.current.isBlankNode(blank)).toBe(true);
  });
});
