/**
 * @file HooksBridge Tests
 * @module test-hooks-bridge
 * @description Tests for BiDirectional JS/Erlang hooks bridge
 */

import { describe, it, expect, beforeEach, afterEach } from 'vitest';

/**
 * Mock store for testing
 */
class MockStore {
  async queryAsync(query) {
    // Simulate SPARQL query response
    return { boolean: true };
  }
}

describe('HooksBridge', () => {
  let bridge;
  let store;
  let HooksBridge;
  let HOOKS_BRIDGE_OPERATIONS;

  beforeEach(async () => {
    // Lazy import to avoid vite resolution issues
    const mod = await import('../src/index.mjs');
    HooksBridge = mod.HooksBridge;
    HOOKS_BRIDGE_OPERATIONS = mod.HOOKS_BRIDGE_OPERATIONS;

    store = new MockStore();
    bridge = new HooksBridge(store, {
      nodeId: 'test-node',
      enableReceiptChaining: true,
      enableJIT: true,
    });
  });

  afterEach(() => {
    bridge.clear();
  });

  describe('Initialization', () => {
    it('should create bridge instance', () => {
      expect(bridge).toBeDefined();
      expect(bridge.store).toBe(store);
      expect(bridge.hooks).toBeDefined();
      expect(bridge.receipts).toBeDefined();
      expect(bridge.options.nodeId).toBe('test-node');
    });

    it('should have default options', () => {
      const testBridge = new HooksBridge(store);
      expect(testBridge.options.maxHooks).toBe(1000);
      expect(testBridge.options.enableReceiptChaining).toBe(true);
      expect(testBridge.options.enableJIT).toBe(true);
    });
  });

  describe('Hook Registration', () => {
    it('should register a hook', async () => {
      const hookSpec = {
        hook_name: 'test_hook',
        hook_type: 'validation',
        condition: {
          type: 'sparql-ask',
          spec: { query: 'ASK { ?s ?p ?o }' },
        },
        effects: [
          {
            type: 'side-effect',
            config: { log: 'Hook executed' },
          },
        ],
      };

      const result = await bridge.registerHook(hookSpec);

      expect(result.registered).toBe(true);
      expect(result.hookId).toBeDefined();
      expect(result.receipt).toBeDefined();
      expect(bridge.hooks.size).toBe(1);
    });

    it('should validate hook specification', async () => {
      const invalidHookSpec = {
        // Missing hook_name
        hook_type: 'validation',
        condition: { type: 'sparql-ask', spec: {} },
        effects: [],
      };

      await expect(bridge.registerHook(invalidHookSpec)).rejects.toThrow();
    });

    it('should enforce max hooks limit', async () => {
      const smallBridge = new HooksBridge(store, { maxHooks: 2 });

      // Register first hook
      await smallBridge.registerHook({
        hook_name: 'hook1',
        hook_type: 'validation',
        condition: { type: 'sparql-ask', spec: {} },
        effects: [],
      });

      // Register second hook
      await smallBridge.registerHook({
        hook_name: 'hook2',
        hook_type: 'validation',
        condition: { type: 'sparql-ask', spec: {} },
        effects: [],
      });

      // Third hook should fail
      await expect(
        smallBridge.registerHook({
          hook_name: 'hook3',
          hook_type: 'validation',
          condition: { type: 'sparql-ask', spec: {} },
          effects: [],
        })
      ).rejects.toThrow('Maximum hooks limit');
    });
  });

  describe('Condition Evaluation', () => {
    it('should evaluate SPARQL ASK condition', async () => {
      const result = await bridge.evaluateCondition({
        type: 'sparql-ask',
        spec: { query: 'ASK { ?s ?p ?o }' },
      });

      expect(result.result).toBe(true);
      expect(result.metadata.evaluationType).toBe('sparql-ask');
      expect(result.receipt).toBeDefined();
    });

    it('should evaluate regex condition', async () => {
      const result = await bridge.evaluateCondition({
        type: 'regex',
        spec: { pattern: '^test', text: 'test123' },
      });

      expect(result.result).toBe(true);
      expect(result.metadata.evaluationType).toBe('regex');
    });

    it('should evaluate datalog condition', async () => {
      const result = await bridge.evaluateCondition({
        type: 'datalog',
        spec: { facts: ['fact1', 'fact2'] },
      });

      expect(result.result).toBe(true);
      expect(result.metadata.evaluationType).toBe('datalog');
    });

    it('should handle unknown condition type', async () => {
      await expect(
        bridge.evaluateCondition({
          type: 'unknown-type',
          spec: {},
        })
      ).rejects.toThrow();
    });
  });

  describe('Effect Execution', () => {
    it('should execute side-effect', async () => {
      const result = await bridge.executeEffect({
        type: 'side-effect',
        config: { log: 'Test side effect' },
      });

      expect(result.executed).toBe(true);
      expect(result.receipt).toBeDefined();
    });

    it('should execute JS function effect', async () => {
      const result = await bridge.executeEffect({
        type: 'js-function',
        config: {
          fn: async () => ({ result: 'success' }),
        },
      });

      expect(result.executed).toBe(true);
    });

    it('should execute SPARQL CONSTRUCT effect', async () => {
      const result = await bridge.executeEffect({
        type: 'sparql-construct',
        config: { query: 'CONSTRUCT { ?s ?p ?o } WHERE { ?s ?p ?o }' },
      });

      expect(result.executed).toBe(true);
    });

    it('should execute receipt-chain effect', async () => {
      const result = await bridge.executeEffect({
        type: 'receipt-chain',
        config: { chain: true },
      });

      expect(result.executed).toBe(true);
      expect(result.result.chain_length).toBe(1); // One effect receipt
    });
  });

  describe('Receipt Chain', () => {
    it('should generate receipt on hook registration', async () => {
      const initialCount = bridge.receipts.length;

      await bridge.registerHook({
        hook_name: 'test_hook',
        hook_type: 'validation',
        condition: { type: 'sparql-ask', spec: { query: 'ASK { ?s ?p ?o }' } },
        effects: [],
      });

      expect(bridge.receipts.length).toBe(initialCount + 1);
    });

    it('should chain receipts via BLAKE3 hashing', async () => {
      // Perform multiple operations
      await bridge.registerHook({
        hook_name: 'hook1',
        hook_type: 'validation',
        condition: { type: 'sparql-ask', spec: {} },
        effects: [],
      });

      await bridge.evaluateCondition({
        type: 'sparql-ask',
        spec: { query: 'ASK { ?s ?p ?o }' },
      });

      const chain = bridge.getReceiptChain();

      expect(chain.length).toBe(2);
      expect(chain[0].position).toBe(0);
      expect(chain[0].is_genesis).toBe(true);
      expect(chain[0].previous_exists).toBe(false);

      expect(chain[1].position).toBe(1);
      expect(chain[1].previous_exists).toBe(true);
    });

    it('should return full receipt chain with metadata', async () => {
      // Register multiple hooks
      await bridge.registerHook({
        hook_name: 'hook1',
        hook_type: 'validation',
        condition: { type: 'sparql-ask', spec: {} },
        effects: [],
      });

      await bridge.registerHook({
        hook_name: 'hook2',
        hook_type: 'inference',
        condition: { type: 'datalog', spec: {} },
        effects: [],
      });

      const chain = bridge.getReceiptChain();

      expect(chain.length).toBe(2);
      chain.forEach((receipt, index) => {
        expect(receipt.position).toBe(index);
        expect(receipt.total_in_chain).toBe(2);
      });
    });
  });

  describe('Hook Listing and Clearing', () => {
    it('should list all registered hooks', async () => {
      await bridge.registerHook({
        hook_name: 'hook1',
        hook_type: 'validation',
        condition: { type: 'sparql-ask', spec: {} },
        effects: [],
      });

      await bridge.registerHook({
        hook_name: 'hook2',
        hook_type: 'inference',
        condition: { type: 'datalog', spec: {} },
        effects: [],
      });

      const hooks = bridge.listHooks();

      expect(hooks.length).toBe(2);
      expect(hooks[0].name).toBe('hook1');
      expect(hooks[1].name).toBe('hook2');
    });

    it('should clear all hooks and receipts', async () => {
      await bridge.registerHook({
        hook_name: 'hook1',
        hook_type: 'validation',
        condition: { type: 'sparql-ask', spec: {} },
        effects: [],
      });

      expect(bridge.hooks.size).toBe(1);
      expect(bridge.receipts.length).toBeGreaterThan(0);

      bridge.clear();

      expect(bridge.hooks.size).toBe(0);
      expect(bridge.receipts.length).toBe(0);
    });
  });

  describe('Operations Enum', () => {
    it('should export operation types', () => {
      expect(HOOKS_BRIDGE_OPERATIONS.REGISTER_HOOK).toBe('hooks_bridge.register_hook');
      expect(HOOKS_BRIDGE_OPERATIONS.EVALUATE_CONDITION).toBe(
        'hooks_bridge.evaluate_condition'
      );
      expect(HOOKS_BRIDGE_OPERATIONS.EXECUTE_EFFECT).toBe('hooks_bridge.execute_effect');
      expect(HOOKS_BRIDGE_OPERATIONS.EXECUTE_HOOKS).toBe('hooks_bridge.execute_hooks');
      expect(HOOKS_BRIDGE_OPERATIONS.GET_RECEIPT_CHAIN).toBe(
        'hooks_bridge.get_receipt_chain'
      );
    });
  });
});
