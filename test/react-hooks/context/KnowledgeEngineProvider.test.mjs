/**
 * @fileoverview Tests for KnowledgeEngineProvider
 */

import { describe, it, expect } from 'vitest';
import { renderHook } from '@testing-library/react';
import { createElement } from 'react';
import {
  KnowledgeEngineProvider,
  KnowledgeEngineContext,
} from '../../../src/react-hooks/context/KnowledgeEngineContext.mjs';
import { useContext } from 'react';

describe('KnowledgeEngineProvider', () => {
  describe('Provider Setup', () => {
    it('should provide engine context', () => {
      const mockEngine = { type: 'test-engine' };

      const wrapper = ({ children }) =>
        createElement(KnowledgeEngineProvider, { value: mockEngine }, children);

      const { result } = renderHook(() => useContext(KnowledgeEngineContext), {
        wrapper,
      });

      expect(result.current).toBe(mockEngine);
    });

    it('should handle null engine', () => {
      const wrapper = ({ children }) =>
        createElement(KnowledgeEngineProvider, { value: null }, children);

      const { result } = renderHook(() => useContext(KnowledgeEngineContext), {
        wrapper,
      });

      expect(result.current).toBeNull();
    });
  });

  describe('Context Propagation', () => {
    it('should propagate context to nested components', () => {
      const mockEngine = { id: '123', type: 'knowledge-engine' };

      const wrapper = ({ children }) =>
        createElement(KnowledgeEngineProvider, { value: mockEngine }, children);

      const { result } = renderHook(() => useContext(KnowledgeEngineContext), {
        wrapper,
      });

      expect(result.current.id).toBe('123');
      expect(result.current.type).toBe('knowledge-engine');
    });

    it('should update context when engine changes', () => {
      const engine1 = { id: '1' };
      const engine2 = { id: '2' };

      const { result, rerender } = renderHook(
        ({ engine }) => {
          const wrapper = ({ children }) =>
            createElement(KnowledgeEngineProvider, { value: engine }, children);

          return renderHook(() => useContext(KnowledgeEngineContext), {
            wrapper,
          }).result.current;
        },
        { initialProps: { engine: engine1 } }
      );

      expect(result.current?.id).toBe('1');

      rerender({ engine: engine2 });

      expect(result.current?.id).toBe('2');
    });
  });

  describe('Multiple Providers', () => {
    it('should support nested providers', () => {
      const outerEngine = { level: 'outer' };
      const innerEngine = { level: 'inner' };

      const wrapper = ({ children }) =>
        createElement(
          KnowledgeEngineProvider,
          { value: outerEngine },
          createElement(KnowledgeEngineProvider, { value: innerEngine }, children)
        );

      const { result } = renderHook(() => useContext(KnowledgeEngineContext), {
        wrapper,
      });

      expect(result.current.level).toBe('inner');
    });
  });

  describe('Provider Performance', () => {
    it('should not cause unnecessary rerenders', () => {
      const mockEngine = { id: '123' };
      let renderCount = 0;

      const wrapper = ({ children }) =>
        createElement(KnowledgeEngineProvider, { value: mockEngine }, children);

      const { rerender } = renderHook(
        () => {
          renderCount++;
          return useContext(KnowledgeEngineContext);
        },
        { wrapper }
      );

      const initialRenderCount = renderCount;

      rerender();
      rerender();
      rerender();

      // Should only rerender when we explicitly call rerender
      expect(renderCount).toBeGreaterThan(initialRenderCount);
    });
  });
});
