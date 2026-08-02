import { describe, it, expect, beforeEach, afterEach } from 'vitest';
import { Store } from '@unrdf/oxigraph';
import { HooksBridge, HOOKS_BRIDGE_OPERATIONS } from '../src/index.mjs';

describe('HooksBridge — real store and receipt state', () => {
  let bridge;

  beforeEach(() => {
    bridge = new HooksBridge(new Store(), {
      nodeId: 'chicago-node',
      enableReceiptChaining: true,
      enableJIT: true,
    });
  });

  afterEach(() => bridge.clear());

  it('registers a real regex hook and records a receipt', async () => {
    const result = await bridge.registerHook({
      hook_name: 'vehicle_identifier',
      hook_type: 'validation',
      condition: {
        type: 'regex',
        spec: { pattern: '^V-[0-9]+$', text: 'V-42' },
      },
      effects: [],
    });

    expect(result.registered).toBe(true);
    expect(bridge.getHook(result.hookId).name).toBe('vehicle_identifier');
    expect(bridge.getReceiptChain()).toHaveLength(1);
  });

  it('evaluates real regex state and appends the execution receipt', async () => {
    const passed = await bridge.evaluateCondition({
      type: 'regex',
      spec: { pattern: '^V-[0-9]+$', text: 'V-1001' },
    });
    const failed = await bridge.evaluateCondition({
      type: 'regex',
      spec: { pattern: '^V-[0-9]+$', text: 'truck' },
    });

    expect(passed.result).toBe(true);
    expect(failed.result).toBe(false);
    expect(bridge.getReceiptChain()).toHaveLength(2);
  });

  it('executes a real side-effect contract and enforces capacity', async () => {
    const effect = await bridge.executeEffect({
      type: 'side-effect',
      config: { log: 'fleet event admitted' },
    });
    expect(effect.executed).toBe(true);
    expect(effect.result).toEqual({ executed: true });

    const bounded = new HooksBridge(new Store(), { maxHooks: 1, nodeId: 'bounded' });
    await bounded.registerHook({
      hook_name: 'first',
      hook_type: 'validation',
      condition: { type: 'regex', spec: { pattern: 'x', text: 'x' } },
      effects: [],
    });
    await expect(bounded.registerHook({
      hook_name: 'second',
      hook_type: 'validation',
      condition: { type: 'regex', spec: { pattern: 'x', text: 'x' } },
      effects: [],
    })).rejects.toThrow(/Maximum hooks limit/);
  });

  it('publishes the operational contract', () => {
    expect(HOOKS_BRIDGE_OPERATIONS.REGISTER_HOOK).toBe('hooks_bridge.register_hook');
    expect(HOOKS_BRIDGE_OPERATIONS.EXECUTE_EFFECT).toBe('hooks_bridge.execute_effect');
  });
});
