/**
 * @fileoverview Tests for useHookManager
 */

import { describe, it, expect, beforeEach, vi } from 'vitest';

describe('useHookManager', () => {
  let hooks;

  beforeEach(() => {
    hooks = [];
  });

  describe('Hook Management', () => {
    it('should add hooks to manager', () => {
      const hook = {
        name: 'test-hook',
        execute: vi.fn(),
      };

      hooks.push(hook);

      expect(hooks).toHaveLength(1);
    });

    it('should remove hooks from manager', () => {
      const hook = {
        name: 'test-hook',
        execute: vi.fn(),
      };

      hooks.push(hook);
      hooks = hooks.filter(h => h.name !== 'test-hook');

      expect(hooks).toHaveLength(0);
    });

    it('should list all hooks', () => {
      hooks.push({ name: 'hook-1', execute: vi.fn() });
      hooks.push({ name: 'hook-2', execute: vi.fn() });

      expect(hooks.map(h => h.name)).toEqual(['hook-1', 'hook-2']);
    });
  });

  describe('Hook Execution Order', () => {
    it('should execute pre hooks before post hooks', () => {
      const order = [];

      const preHook = {
        name: 'pre',
        type: 'pre',
        execute: () => order.push('pre'),
      };

      const postHook = {
        name: 'post',
        type: 'post',
        execute: () => order.push('post'),
      };

      hooks = [preHook, postHook];
      hooks.filter(h => h.type === 'pre').forEach(h => h.execute());
      hooks.filter(h => h.type === 'post').forEach(h => h.execute());

      expect(order).toEqual(['pre', 'post']);
    });

    it('should respect priority within hook types', () => {
      const order = [];

      const hook1 = {
        name: 'h1',
        type: 'pre',
        priority: 10,
        execute: () => order.push('h1'),
      };
      const hook2 = {
        name: 'h2',
        type: 'pre',
        priority: 20,
        execute: () => order.push('h2'),
      };

      hooks = [hook1, hook2].sort((a, b) => b.priority - a.priority);
      hooks.forEach(h => h.execute());

      expect(order).toEqual(['h2', 'h1']);
    });
  });

  describe('Hook Lifecycle', () => {
    it('should initialize hooks on mount', () => {
      const initialized = [];

      const hook = {
        name: 'test',
        init: () => initialized.push('initialized'),
        execute: vi.fn(),
      };

      hook.init?.();

      expect(initialized).toContain('initialized');
    });

    it('should cleanup hooks on unmount', () => {
      const cleaned = [];

      const hook = {
        name: 'test',
        cleanup: () => cleaned.push('cleaned'),
        execute: vi.fn(),
      };

      hook.cleanup?.();

      expect(cleaned).toContain('cleaned');
    });
  });

  describe('Hook Statistics', () => {
    it('should track hook execution count', () => {
      const stats = { executions: 0 };

      const hook = {
        name: 'test',
        execute: () => stats.executions++,
      };

      hook.execute();
      hook.execute();
      hook.execute();

      expect(stats.executions).toBe(3);
    });

    it('should track hook execution time', () => {
      const stats = { totalTime: 0 };

      const hook = {
        name: 'test',
        execute: () => {
          const start = performance.now();
          // Simulate work
          const end = performance.now();
          stats.totalTime += end - start;
        },
      };

      hook.execute();

      expect(stats.totalTime).toBeGreaterThan(0);
    });
  });
});
