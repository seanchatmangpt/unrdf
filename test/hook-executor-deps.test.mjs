import { describe, it, expect } from 'vitest';
import { createHookExecutor } from '../src/knowledge-engine/hook-executor.mjs';

describe('Hook Executor - dependency resolution', () => {
  it('executes hooks respecting meta.dependencies order', async () => {
    const order = [];
    const executor = createHookExecutor({
      strictMode: true,
      enableConditionEvaluation: false,
      enableSandboxing: false,
    });

    const hookA = {
      meta: { name: 'A', dependencies: ['B'] },
      run: async () => {
        order.push('A');
        return { ok: true };
      },
    };

    const hookB = {
      meta: { name: 'B' },
      run: async () => {
        order.push('B');
        return { ok: true };
      },
    };

    const results = await executor.executeWithDependencies([hookA, hookB], {
      payload: {},
      context: {},
    });
    expect(results).toHaveLength(2);
    expect(order).toEqual(['B', 'A']);
    expect(results.every(r => r.success)).toBe(true);
  });

  it('throws on missing dependency in strictMode', async () => {
    const executor = createHookExecutor({
      strictMode: true,
      enableConditionEvaluation: false,
      enableSandboxing: false,
    });
    const hook = {
      meta: { name: 'A', dependencies: ['Missing'] },
      run: async () => ({ ok: true }),
    };
    await expect(
      executor.executeWithDependencies([hook], { payload: {}, context: {} })
    ).rejects.toThrow(/missing dependency/i);
  });
});
