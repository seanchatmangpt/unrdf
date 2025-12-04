/**
 * @fileoverview Tests for KnowledgeHookManager class
 */

import { describe, it, expect } from 'vitest';
import { DataFactory } from 'n3';
import { KnowledgeHookManager } from '../src/index.mjs';

const { namedNode, literal, quad } = DataFactory;

describe('KnowledgeHookManager', () => {
  it('should create instance', () => {
    const manager = new KnowledgeHookManager();
    expect(manager).toBeInstanceOf(KnowledgeHookManager);
  });

  it('should load built-in hooks when requested', () => {
    const manager = new KnowledgeHookManager({ includeBuiltins: true });
    const hooks = manager.listHooks();
    expect(hooks.length).toBeGreaterThan(0);
  });

  it('should register custom hook', () => {
    const manager = new KnowledgeHookManager();
    const hook = {
      id: 'test-hook',
      name: 'test-hook',
      description: 'Test validation hook',
      version: '1.0.0',
      trigger: 'before-add',
      enabled: true,
      validate: () => ({ valid: true }),
    };

    manager.registerHook(hook);
    expect(manager.hasHook('test-hook')).toBe(true);
  });

  it('should execute hook by trigger', async () => {
    const manager = new KnowledgeHookManager({ includeBuiltins: true });
    const testQuad = quad(
      namedNode('http://example.org/subject'),
      namedNode('http://example.org/predicate'),
      literal('value')
    );

    const result = await manager.executeByTrigger('before-add', testQuad);
    expect(result).toHaveProperty('valid');
  });

  it('should get hooks by trigger', () => {
    const manager = new KnowledgeHookManager({ includeBuiltins: true });
    const hooks = manager.getHooksByTrigger('before-add');
    expect(hooks.length).toBeGreaterThan(0);
  });

  it('should provide static access to built-in hooks', () => {
    const builtins = KnowledgeHookManager.getBuiltinHooks();
    expect(Array.isArray(builtins)).toBe(true);
    expect(builtins.length).toBeGreaterThan(0);
  });

  it('should clear all hooks', () => {
    const manager = new KnowledgeHookManager({ includeBuiltins: true });
    expect(manager.listHooks().length).toBeGreaterThan(0);

    manager.clearHooks();
    expect(manager.listHooks().length).toBe(0);
  });

  it('should unregister specific hook', () => {
    const manager = new KnowledgeHookManager();
    const hook = {
      id: 'removable-hook',
      name: 'removable-hook',
      description: 'Test hook',
      version: '1.0.0',
      trigger: 'before-add',
      enabled: true,
      validate: () => ({ valid: true }),
    };

    manager.registerHook(hook);
    expect(manager.hasHook('removable-hook')).toBe(true);

    manager.unregisterHook('removable-hook');
    expect(manager.hasHook('removable-hook')).toBe(false);
  });

  it('should check if data would pass hooks', async () => {
    const manager = new KnowledgeHookManager({ includeBuiltins: true });
    const validQuad = quad(
      namedNode('http://example.org/subject'),
      namedNode('http://example.org/predicate'),
      literal('value')
    );

    const wouldPass = await manager.wouldPass('before-add', validQuad);
    expect(typeof wouldPass).toBe('boolean');
  });

  it('should get statistics', () => {
    const manager = new KnowledgeHookManager({ includeBuiltins: true });
    const stats = manager.getStats();
    expect(stats).toHaveProperty('byTrigger');
    expect(stats).toHaveProperty('totalHooks');
    expect(stats.totalHooks).toBeGreaterThan(0);
  });
});
