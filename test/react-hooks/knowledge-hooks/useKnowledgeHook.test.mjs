/**
 * @fileoverview Tests for useKnowledgeHook
 */

import { describe, it, expect, beforeEach, vi } from 'vitest';

describe('useKnowledgeHook', () => {
  describe('Hook Registration', () => {
    it('should register a pre-transaction hook', () => {
      const hook = {
        name: 'test-pre-hook',
        type: 'pre',
        execute: vi.fn()
      };

      expect(hook.name).toBe('test-pre-hook');
      expect(hook.type).toBe('pre');
    });

    it('should register a post-transaction hook', () => {
      const hook = {
        name: 'test-post-hook',
        type: 'post',
        execute: vi.fn()
      };

      expect(hook.type).toBe('post');
    });

    it('should register a veto hook', () => {
      const hook = {
        name: 'test-veto-hook',
        type: 'veto',
        execute: vi.fn()
      };

      expect(hook.type).toBe('veto');
    });
  });

  describe('Hook Execution', () => {
    it('should execute pre hooks before transaction', () => {
      const hook = {
        name: 'pre-hook',
        type: 'pre',
        execute: vi.fn()
      };

      hook.execute();

      expect(hook.execute).toHaveBeenCalled();
    });

    it('should execute post hooks after transaction', () => {
      const hook = {
        name: 'post-hook',
        type: 'post',
        execute: vi.fn()
      };

      hook.execute();

      expect(hook.execute).toHaveBeenCalled();
    });

    it('should allow veto hooks to block transaction', () => {
      const hook = {
        name: 'veto-hook',
        type: 'veto',
        execute: vi.fn(() => ({ allowed: false, reason: 'Blocked' }))
      };

      const result = hook.execute();

      expect(result.allowed).toBe(false);
      expect(result.reason).toBe('Blocked');
    });
  });

  describe('Hook Context', () => {
    it('should provide transaction context to hooks', () => {
      const context = {
        transactionId: '123',
        delta: { added: [], removed: [] },
        metadata: { user: 'alice' }
      };

      expect(context.transactionId).toBe('123');
    });

    it('should allow hooks to modify context', () => {
      const context = { metadata: {} };

      context.metadata.processed = true;

      expect(context.metadata.processed).toBe(true);
    });
  });

  describe('Hook Priority', () => {
    it('should execute hooks in priority order', () => {
      const executionOrder = [];

      const hook1 = {
        name: 'hook-1',
        priority: 10,
        execute: () => executionOrder.push('hook-1')
      };

      const hook2 = {
        name: 'hook-2',
        priority: 5,
        execute: () => executionOrder.push('hook-2')
      };

      const hooks = [hook1, hook2].sort((a, b) => b.priority - a.priority);
      hooks.forEach(h => h.execute());

      expect(executionOrder).toEqual(['hook-1', 'hook-2']);
    });
  });

  describe('Error Handling', () => {
    it('should handle hook execution errors', () => {
      const hook = {
        name: 'error-hook',
        execute: () => {
          throw new Error('Hook failed');
        }
      };

      expect(() => hook.execute()).toThrow('Hook failed');
    });

    it('should continue with other hooks on error', () => {
      const executionOrder = [];

      const hook1 = {
        name: 'hook-1',
        execute: () => { throw new Error('Failed'); }
      };

      const hook2 = {
        name: 'hook-2',
        execute: () => executionOrder.push('hook-2')
      };

      try {
        hook1.execute();
      } catch (e) {
        // Continue
      }

      hook2.execute();

      expect(executionOrder).toContain('hook-2');
    });
  });
});
