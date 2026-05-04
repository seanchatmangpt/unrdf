/**
 * @fileoverview HooksBridge Behavioral Tests
 * @description Contract-based testing for BiDirectional JS/Erlang bridge.
 */

import { describe, it, expect, beforeEach, afterEach, vi } from 'vitest';
import { HooksBridge, HOOKS_BRIDGE_OPERATIONS } from '../src/index.mjs';

describe('HooksBridge', () => {
  let bridge;
  let store;

  beforeEach(async () => {
    store = { queryAsync: vi.fn().mockResolvedValue({ boolean: true }) };
    bridge = new HooksBridge(store, {
      nodeId: 'test-node',
      enableReceiptChaining: true,
      enableJIT: true,
    });
  });

  afterEach(() => {
    bridge.clear();
  });

  describe('Registration & Lifecycle', () => {
    it('should register valid hook specification', async () => {
      const hookSpec = {
        hook_name: 'test_hook',
        hook_type: 'validation',
        condition: { type: 'sparql-ask', spec: { query: 'ASK { ?s ?p ?o }' } },
        effects: [],
      };

      const result = await bridge.registerHook(hookSpec);
      expect(result.registered).toBe(true);
      expect(result.hookId).toBeDefined();
    });

    it('should enforce max hooks capacity', async () => {
      const smallBridge = new HooksBridge(store, { maxHooks: 1 });
      await smallBridge.registerHook({
        hook_name: 'h1',
        hook_type: 'validation',
        condition: { type: 'sparql-ask', spec: {} },
        effects: [],
      });

      await expect(
        smallBridge.registerHook({
          hook_name: 'h2',
          hook_type: 'validation',
          condition: { type: 'sparql-ask', spec: {} },
          effects: [],
        })
      ).rejects.toThrow('Maximum hooks limit');
    });
  });

  describe('Conditional Evaluation', () => {
    it('should evaluate supported logic types correctly', async () => {
      const types = ['sparql-ask', 'regex', 'datalog'];
      for (const type of types) {
        const res = await bridge.evaluateCondition({ type, spec: {} });
        expect(res.metadata.evaluationType).toBe(type);
      }
    });
  });

  describe('Effect Execution', () => {
    it('should trigger defined effect handlers', async () => {
      const res = await bridge.executeEffect({
        type: 'side-effect',
        config: { log: 'ok' },
      });
      expect(res.executed).toBe(true);
    });
  });

  describe('Operations Contract', () => {
    it('should expose defined operational constants', () => {
      expect(HOOKS_BRIDGE_OPERATIONS.REGISTER_HOOK).toBeDefined();
      expect(HOOKS_BRIDGE_OPERATIONS.EXECUTE_EFFECT).toBeDefined();
    });
  });
});
