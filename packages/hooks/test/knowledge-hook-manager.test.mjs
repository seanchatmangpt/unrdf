/**
 * @fileoverview Fast tests for KnowledgeHookManager
 * Focus: Registration and basic execution only
 * Removed: Statistics, unregister, complex operations
 */
import { describe, it, expect } from 'vitest';
import { UnrdfDataFactory as DataFactory } from '@unrdf/core/rdf/n3-justified-only';
import { KnowledgeHookManager } from '../src/index.mjs';

const { namedNode, literal, quad } = DataFactory;

describe('KnowledgeHookManager - Fast Suite', () => {
  it('should register and retrieve hook', () => {
    const manager = new KnowledgeHookManager({ includeBuiltins: false });
    const hook = {
      id: 'test-hook',
      name: 'test-hook',
      trigger: 'before-add',
      enabled: true,
      validate: () => ({ valid: true }),
    };
    manager.registerHook(hook);
    expect(manager.hasHook('test-hook')).toBe(true);
  });

  it('should execute hook by trigger', async () => {
    const manager = new KnowledgeHookManager({ includeBuiltins: false });
    const testQuad = quad(
      namedNode('http://example.org/s'),
      namedNode('http://example.org/p'),
      literal('test')
    );
    const hook = {
      id: 'test',
      name: 'test',
      trigger: 'before-add',
      enabled: true,
      validate: () => ({ valid: true }),
    };
    manager.registerHook(hook);
    const result = await manager.executeByTrigger('before-add', testQuad);
    expect(result).toHaveProperty('valid');
  });

  it('should handle unregister', () => {
    const manager = new KnowledgeHookManager({ includeBuiltins: false });
    const hook = {
      id: 'removable',
      name: 'removable',
      trigger: 'before-add',
      enabled: true,
      validate: () => ({ valid: true }),
    };
    manager.registerHook(hook);
    manager.unregisterHook('removable');
    expect(manager.hasHook('removable')).toBe(false);
  });
});
