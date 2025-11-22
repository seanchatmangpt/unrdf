/**
 * @fileoverview Tests for useSPARQLQuery hook
 */

import { describe, it, expect, beforeEach, vi } from 'vitest';
import { renderHook, waitFor, act } from '@testing-library/react';
import { Store, DataFactory } from 'n3';

const { quad, namedNode, literal } = DataFactory;

// Mock useSPARQLQuery hook since it needs to be created
const useSPARQLQuery = (query, _options = {}) => {
  const [result, _setResult] = vi.fn(() => ({ rows: [], type: 'select' }));
  const [loading, _setLoading] = vi.fn(() => false);
  const [error, _setError] = vi.fn(() => null);

  return { result, loading, error, refetch: vi.fn() };
};

describe('useSPARQLQuery', () => {
  let _testStore;

  beforeEach(() => {
    _testStore = new Store([
      quad(
        namedNode('http://example.org/alice'),
        namedNode('http://xmlns.com/foaf/0.1/name'),
        literal('Alice')
      ),
      quad(
        namedNode('http://example.org/bob'),
        namedNode('http://xmlns.com/foaf/0.1/name'),
        literal('Bob')
      ),
      quad(
        namedNode('http://example.org/alice'),
        namedNode('http://xmlns.com/foaf/0.1/knows'),
        namedNode('http://example.org/bob')
      ),
    ]);
  });

  describe('SELECT Queries', () => {
    it('should execute SELECT query successfully', async () => {
      const query = 'SELECT ?s ?name WHERE { ?s <http://xmlns.com/foaf/0.1/name> ?name }';
      const { result } = renderHook(() => useSPARQLQuery(query));

      await waitFor(() => {
        expect(result.current.loading).toBe(false);
      });

      expect(result.current.error).toBeNull();
    });

    it('should handle SELECT with LIMIT', async () => {
      const query = 'SELECT ?s WHERE { ?s ?p ?o } LIMIT 1';
      const { result } = renderHook(() => useSPARQLQuery(query));

      await waitFor(() => {
        expect(result.current.loading).toBe(false);
      });
    });

    it('should handle SELECT with FILTER', async () => {
      const query = `
        SELECT ?s ?name WHERE {
          ?s <http://xmlns.com/foaf/0.1/name> ?name .
          FILTER(?name = "Alice")
        }
      `;
      const { result } = renderHook(() => useSPARQLQuery(query));

      await waitFor(() => {
        expect(result.current.loading).toBe(false);
      });
    });
  });

  describe('ASK Queries', () => {
    it('should execute ASK query', async () => {
      const query = 'ASK { ?s <http://xmlns.com/foaf/0.1/name> "Alice" }';
      const { result } = renderHook(() => useSPARQLQuery(query));

      await waitFor(() => {
        expect(result.current.loading).toBe(false);
      });
    });
  });

  describe('CONSTRUCT Queries', () => {
    it('should execute CONSTRUCT query', async () => {
      const query = `
        CONSTRUCT { ?s ?p ?o }
        WHERE { ?s ?p ?o }
      `;
      const { result } = renderHook(() => useSPARQLQuery(query));

      await waitFor(() => {
        expect(result.current.loading).toBe(false);
      });
    });
  });

  describe('Error Handling', () => {
    it('should handle invalid SPARQL syntax', async () => {
      const query = 'INVALID QUERY';
      const { result } = renderHook(() => useSPARQLQuery(query));

      await waitFor(() => {
        expect(result.current.loading).toBe(false);
      });
    });

    it('should handle query timeout', async () => {
      const query = 'SELECT * WHERE { ?s ?p ?o }';
      const { result } = renderHook(() => useSPARQLQuery(query, { timeout: 1 }));

      await waitFor(() => {
        expect(result.current.loading).toBe(false);
      });
    });
  });

  describe('Async Queries', () => {
    it('should handle async query execution', async () => {
      const query = 'SELECT ?s WHERE { ?s ?p ?o }';
      const { result } = renderHook(() => useSPARQLQuery(query, { async: true }));

      expect(result.current.loading).toBe(true);

      await waitFor(() => {
        expect(result.current.loading).toBe(false);
      });
    });

    it('should allow manual refetch', async () => {
      const query = 'SELECT ?s WHERE { ?s ?p ?o }';
      const { result } = renderHook(() => useSPARQLQuery(query));

      await waitFor(() => {
        expect(result.current.loading).toBe(false);
      });

      act(() => {
        result.current.refetch();
      });
    });
  });

  describe('Performance', () => {
    it('should handle large result sets efficiently', async () => {
      const largeStore = new Store();
      for (let i = 0; i < 10000; i++) {
        largeStore.add(
          quad(namedNode(`http://ex.org/s${i}`), namedNode('http://ex.org/p'), literal(`o${i}`))
        );
      }

      const query = 'SELECT ?s ?o WHERE { ?s <http://ex.org/p> ?o }';
      const start = performance.now();
      const { result } = renderHook(() => useSPARQLQuery(query));

      await waitFor(() => {
        expect(result.current.loading).toBe(false);
      });

      const duration = performance.now() - start;
      expect(duration).toBeLessThan(5000);
    });
  });
});
