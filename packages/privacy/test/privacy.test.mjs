import { describe, it, expect } from 'vitest';
import DifferentialPrivacySPARQL, {
  PrivacyBudgetManager,
  laplace,
  allocateEpsilon,
  estimateNoise,
} from '../src/differential-privacy-sparql.mjs';

describe('PrivacyBudgetManager', () => {
  it('tracks spend and remaining budget', () => {
    const m = new PrivacyBudgetManager(2.0);
    m.spend(0.5, 'q1');
    expect(m.remaining()).toBeCloseTo(1.5);
    expect(m.canExecute(1.5)).toBe(true);
    expect(m.canExecute(1.6)).toBe(false);
  });

  it('refuses to exceed the budget', () => {
    const m = new PrivacyBudgetManager(1.0);
    m.spend(1.0, 'q1');
    expect(() => m.spend(0.1, 'q2')).toThrow(/Privacy budget exhausted/);
  });

  it('issues a 64-hex BLAKE3 receipt', async () => {
    const m = new PrivacyBudgetManager(3.0);
    m.spend(1.0, 'q1');
    const receipt = await m.generateReceipt();
    expect(receipt.receiptHash).toMatch(/^[0-9a-f]{64}$/);
    expect(receipt.spent).toBe(1.0);
    expect(receipt.queryCount).toBe(1);
  });
});

describe('mechanisms', () => {
  it('laplace noise is finite and roughly centred', () => {
    let sum = 0;
    const n = 5000;
    for (let i = 0; i < n; i++) {
      const x = laplace(0, 1);
      expect(Number.isFinite(x)).toBe(true);
      sum += x;
    }
    expect(Math.abs(sum / n)).toBeLessThan(0.2);
  });

  it('allocateEpsilon splits budgets equally and by weight', () => {
    expect(allocateEpsilon(10, 5)).toEqual([2, 2, 2, 2, 2]);
    expect(allocateEpsilon(10, 2, [1, 3])).toEqual([2.5, 7.5]);
    expect(() => allocateEpsilon(10, 3, [1, 2])).toThrow(/must match/);
  });

  it('estimateNoise: laplace scale = sensitivity / epsilon', () => {
    const s = estimateNoise(0.5, 1);
    expect(s.scale).toBe(2);
    expect(() => estimateNoise(1, 1, 'nope')).toThrow(/Unknown mechanism/);
  });
});

describe('DifferentialPrivacySPARQL', () => {
  const store = { match: async () => new Array(100).fill({}) };

  it('executeCOUNT returns a noisy count and spends budget', async () => {
    const engine = new DifferentialPrivacySPARQL({ totalBudget: 5.0 });
    const result = await engine.executeCOUNT(store, '?s a Patient', 1.0);
    expect(result.trueValue).toBe(100);
    expect(result.mechanism).toBe('laplace');
    expect(engine.getRemainingBudget()).toBeCloseTo(4.0);
  });

  it('rejects epsilon above the schema maximum', async () => {
    const engine = new DifferentialPrivacySPARQL();
    await expect(engine.executeCOUNT(store, '?s a Patient', 11)).rejects.toThrow();
  });

  it('errors when the store has no match()', async () => {
    const engine = new DifferentialPrivacySPARQL();
    await expect(engine.executeCOUNT({}, '?s a Patient', 1)).rejects.toThrow(/match\(\)/);
  });
});
