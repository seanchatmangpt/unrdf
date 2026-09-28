/**
 * @file Regression: every named re-export in src/index.mjs must resolve.
 * Previously index.mjs re-exported `TriggerEvaluator` (never defined) and 13 schema
 * names schemas.mjs never exported, so `import '@unrdf/daemon'` threw SyntaxError.
 */
import { describe, it, expect } from 'vitest';

describe('@unrdf/daemon entry', () => {
  it('imports and exposes the trigger evaluator API', async () => {
    const mod = await import('../src/index.mjs');
    for (const name of [
      'evaluateTrigger',
      'shouldExecuteIdle',
      'calculateNextExecutionTime',
      'isValidTrigger',
    ]) {
      expect(typeof mod[name]).toBe('function');
      expect(mod.TriggerEvaluator[name]).toBe(mod[name]);
    }
    const r = mod.TriggerEvaluator.evaluateTrigger(
      { type: 'interval', value: 5000 },
      Date.now() - 6000
    );
    expect(r).toEqual({ shouldExecute: true, nextExecutionTime: 0 });
  });

  it('exposes the real daemon schemas', async () => {
    const mod = await import('../src/index.mjs');
    for (const name of [
      'TriggerSchema',
      'ScheduledOperationSchema',
      'DaemonConfigSchema',
      'OperationReceiptSchema',
    ]) {
      expect(typeof mod[name]?.parse, name).toBe('function');
    }
  });
});
