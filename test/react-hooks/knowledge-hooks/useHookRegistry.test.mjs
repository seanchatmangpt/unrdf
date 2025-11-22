/**
 * @fileoverview Tests for useHookRegistry
 */

import { describe, it, expect, beforeEach, vi } from 'vitest';

describe('useHookRegistry', () => {
  let registry;

  beforeEach(() => {
    registry = new Map();
  });

  describe('Registry Operations', () => {
    it('should register hook', () => {
      const hook = { name: 'test-hook', execute: vi.fn() };

      registry.set(hook.name, hook);

      expect(registry.has('test-hook')).toBe(true);
    });

    it('should unregister hook', () => {
      const hook = { name: 'test-hook', execute: vi.fn() };

      registry.set(hook.name, hook);
      registry.delete('test-hook');

      expect(registry.has('test-hook')).toBe(false);
    });

    it('should get hook by name', () => {
      const hook = { name: 'test-hook', execute: vi.fn() };

      registry.set(hook.name, hook);
      const retrieved = registry.get('test-hook');

      expect(retrieved).toBe(hook);
    });

    it('should list all registered hooks', () => {
      registry.set('hook1', { name: 'hook1', execute: vi.fn() });
      registry.set('hook2', { name: 'hook2', execute: vi.fn() });

      expect([...registry.keys()]).toEqual(['hook1', 'hook2']);
    });
  });

  describe('Global Registry', () => {
    it('should share registry across components', () => {
      const global = new Map();

      global.set('shared-hook', { name: 'shared-hook', execute: vi.fn() });

      expect(global.has('shared-hook')).toBe(true);
    });

    it('should allow hook lookup by name', () => {
      const global = new Map();
      const hook = { name: 'lookup-test', execute: vi.fn() };

      global.set('lookup-test', hook);

      expect(global.get('lookup-test')).toBe(hook);
    });
  });

  describe('Hook Metadata', () => {
    it('should store hook metadata', () => {
      const hook = {
        name: 'meta-hook',
        version: '1.0.0',
        author: 'test',
        description: 'Test hook',
        execute: vi.fn(),
      };

      registry.set(hook.name, hook);
      const stored = registry.get(hook.name);

      expect(stored.version).toBe('1.0.0');
      expect(stored.author).toBe('test');
    });
  });

  describe('Hook Discovery', () => {
    it('should find hooks by type', () => {
      registry.set('pre1', { name: 'pre1', type: 'pre', execute: vi.fn() });
      registry.set('post1', { name: 'post1', type: 'post', execute: vi.fn() });
      registry.set('pre2', { name: 'pre2', type: 'pre', execute: vi.fn() });

      const preHooks = [...registry.values()].filter(h => h.type === 'pre');

      expect(preHooks).toHaveLength(2);
    });

    it('should find hooks by tag', () => {
      registry.set('h1', {
        name: 'h1',
        tags: ['validation'],
        execute: vi.fn(),
      });
      registry.set('h2', { name: 'h2', tags: ['audit'], execute: vi.fn() });
      registry.set('h3', {
        name: 'h3',
        tags: ['validation'],
        execute: vi.fn(),
      });

      const validationHooks = [...registry.values()].filter(h => h.tags?.includes('validation'));

      expect(validationHooks).toHaveLength(2);
    });
  });
});
