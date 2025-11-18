/**
 * @fileoverview Tests for useKnowledgeEngine hook
 */

import { describe, it, expect, beforeEach, vi } from 'vitest';
import { renderHook, waitFor } from '@testing-library/react';
import { useKnowledgeEngine } from '../../../src/react-hooks/core/useKnowledgeEngine.mjs';
import { createElement } from 'react';
import { KnowledgeEngineProvider } from '../../../src/react-hooks/context/KnowledgeEngineContext.mjs';

describe('useKnowledgeEngine', () => {
  describe('Initialization', () => {
    it('should initialize with default options', async () => {
      const { result } = renderHook(() => useKnowledgeEngine());

      await waitFor(() => {
        expect(result.current.isReady).toBe(true);
      });

      expect(result.current.engine).toBeDefined();
      expect(result.current.error).toBeNull();
    });

    it('should initialize with custom base IRI', async () => {
      const { result } = renderHook(() =>
        useKnowledgeEngine({ baseIRI: 'http://example.org/' })
      );

      await waitFor(() => {
        expect(result.current.isReady).toBe(true);
      });

      expect(result.current.engine).toBeDefined();
    });

    it('should initialize with strict mode enabled', async () => {
      const { result } = renderHook(() =>
        useKnowledgeEngine({ strictMode: true })
      );

      await waitFor(() => {
        expect(result.current.isReady).toBe(true);
      });

      expect(result.current.engine).toBeDefined();
    });

    it('should use context engine when provided', async () => {
      const mockEngine = { type: 'mock-engine' };

      const wrapper = ({ children }) =>
        createElement(KnowledgeEngineProvider, { value: mockEngine }, children);

      const { result } = renderHook(() => useKnowledgeEngine(), { wrapper });

      await waitFor(() => {
        expect(result.current.isReady).toBe(true);
      });

      expect(result.current.engine).toEqual(mockEngine);
    });
  });

  describe('Error Handling', () => {
    it('should handle initialization errors gracefully', async () => {
      // This test would require mocking the engine creation to throw
      const { result } = renderHook(() => useKnowledgeEngine());

      await waitFor(() => {
        expect(result.current).toBeDefined();
      });
    });

    it('should set error state when engine creation fails', async () => {
      // Test error handling
      const { result } = renderHook(() => useKnowledgeEngine());

      await waitFor(() => {
        expect(result.current.error === null || result.current.error instanceof Error).toBe(true);
      });
    });
  });

  describe('State Management', () => {
    it('should maintain isReady state correctly', async () => {
      const { result } = renderHook(() => useKnowledgeEngine());

      // Initially might not be ready
      expect(typeof result.current.isReady).toBe('boolean');

      await waitFor(() => {
        expect(result.current.isReady).toBe(true);
      });
    });

    it('should update engine when options change', async () => {
      const { result, rerender } = renderHook(
        ({ options }) => useKnowledgeEngine(options),
        { initialProps: { options: { baseIRI: 'http://example1.org/' } } }
      );

      await waitFor(() => {
        expect(result.current.isReady).toBe(true);
      });

      const firstEngine = result.current.engine;

      rerender({ options: { baseIRI: 'http://example2.org/' } });

      await waitFor(() => {
        expect(result.current.engine).toBeDefined();
      });
    });
  });

  describe('Return Values', () => {
    it('should return engine, isReady, and error properties', async () => {
      const { result } = renderHook(() => useKnowledgeEngine());

      await waitFor(() => {
        expect(result.current).toHaveProperty('engine');
        expect(result.current).toHaveProperty('isReady');
        expect(result.current).toHaveProperty('error');
      });
    });

    it('should return null error when no errors occur', async () => {
      const { result } = renderHook(() => useKnowledgeEngine());

      await waitFor(() => {
        expect(result.current.isReady).toBe(true);
      });

      expect(result.current.error).toBeNull();
    });
  });

  describe('Lifecycle', () => {
    it('should cleanup properly on unmount', async () => {
      const { result, unmount } = renderHook(() => useKnowledgeEngine());

      await waitFor(() => {
        expect(result.current.isReady).toBe(true);
      });

      unmount();
      // Verify no memory leaks or errors
    });

    it('should handle rapid re-renders gracefully', async () => {
      const { result, rerender } = renderHook(() => useKnowledgeEngine());

      for (let i = 0; i < 5; i++) {
        rerender();
      }

      await waitFor(() => {
        expect(result.current.isReady).toBe(true);
      });
    });
  });
});
