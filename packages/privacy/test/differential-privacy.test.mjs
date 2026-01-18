/**
 * @file Differential Privacy SPARQL Tests
 * @module @unrdf/privacy/test
 */

import { describe, it, expect, beforeEach } from 'vitest';
import {
  DifferentialPrivacySPARQL,
  PrivacyBudgetManager,
  laplace,
  gaussian,
  exponentialMechanism,
  allocateEpsilon,
  estimateNoise,
} from '../src/differential-privacy-sparql.mjs';

describe('PrivacyBudgetManager', () => {
  let manager;

  beforeEach(() => {
    manager = new PrivacyBudgetManager(10.0);
  });

  it('should initialize with correct total budget', () => {
    expect(manager.totalBudget).toBe(10.0);
    expect(manager.spent).toBe(0.0);
    expect(manager.remaining()).toBe(10.0);
  });

  it('should allow spending within budget', () => {
    expect(manager.canExecute(5.0)).toBe(true);
    manager.spend(5.0, 'query-1');
    expect(manager.remaining()).toBe(5.0);
    expect(manager.queries).toHaveLength(1);
  });

  it('should prevent spending beyond budget', () => {
    manager.spend(8.0, 'query-1');
    expect(manager.canExecute(3.0)).toBe(false);
    expect(() => manager.spend(3.0, 'query-2')).toThrow('Privacy budget exhausted');
  });

  it('should track multiple queries', () => {
    manager.spend(2.0, 'query-1', 'laplace');
    manager.spend(3.0, 'query-2', 'gaussian');

    expect(manager.queries).toHaveLength(2);
    expect(manager.spent).toBe(5.0);
    expect(manager.queries[0].epsilon).toBe(2.0);
    expect(manager.queries[1].mechanism).toBe('gaussian');
  });

  it('should generate cryptographic receipt', async () => {
    manager.spend(1.0, 'query-1');
    manager.spend(2.0, 'query-2');

    const receipt = await manager.generateReceipt();

    expect(receipt).toHaveProperty('id');
    expect(receipt).toHaveProperty('receiptHash');
    expect(receipt.totalBudget).toBe(10.0);
    expect(receipt.spent).toBe(3.0);
    expect(receipt.queryCount).toBe(2);
  });

  it('should reset budget', () => {
    manager.spend(5.0, 'query-1');
    manager.reset();

    expect(manager.spent).toBe(0.0);
    expect(manager.remaining()).toBe(10.0);
    expect(manager.queries).toHaveLength(0);
  });
});

describe('Noise Mechanisms', () => {
  describe('laplace', () => {
    it('should generate values with correct distribution', () => {
      const samples = Array(1000).fill(0).map(() => laplace(0, 1.0));

      const mean = samples.reduce((a, b) => a + b, 0) / samples.length;
      expect(Math.abs(mean)).toBeLessThan(0.2); // Mean should be near 0
    });

    it('should scale with parameter b', () => {
      const smallScale = Array(100).fill(0).map(() => laplace(0, 0.1));
      const largeScale = Array(100).fill(0).map(() => laplace(0, 10.0));

      const smallStd = Math.sqrt(smallScale.reduce((sum, x) => sum + x * x, 0) / 100);
      const largeStd = Math.sqrt(largeScale.reduce((sum, x) => sum + x * x, 0) / 100);

      expect(largeStd).toBeGreaterThan(smallStd);
    });
  });

  describe('gaussian', () => {
    it('should generate normally distributed values', () => {
      const samples = Array(1000).fill(0).map(() => gaussian(0, 1.0));

      const mean = samples.reduce((a, b) => a + b, 0) / samples.length;
      expect(Math.abs(mean)).toBeLessThan(0.2);
    });
  });

  describe('exponentialMechanism', () => {
    it('should select from candidates', () => {
      const candidates = [
        { value: 'A', score: 10 },
        { value: 'B', score: 5 },
        { value: 'C', score: 1 }
      ];

      const selected = exponentialMechanism(candidates, 1.0, 1);
      expect(['A', 'B', 'C']).toContain(selected);
    });

    it('should favor higher scores', () => {
      const candidates = [
        { value: 'high', score: 100 },
        { value: 'low', score: 1 }
      ];

      const selections = Array(100).fill(0).map(() =>
        exponentialMechanism(candidates, 10.0, 1)
      );

      const highCount = selections.filter(s => s === 'high').length;
      expect(highCount).toBeGreaterThan(50); // Should select 'high' more often
    });
  });
});

describe('DifferentialPrivacySPARQL', () => {
  let engine;
  let mockStore;

  beforeEach(() => {
    engine = new DifferentialPrivacySPARQL({ totalBudget: 10.0 });

    // Mock store with match method
    mockStore = {
      match: async (pattern) => {
        // Return mock results based on pattern
        if (pattern.includes('Patient')) {
          return Array(42).fill({ subject: 'patient', predicate: 'type', object: 'Patient' });
        }
        return [];
      }
    };
  });

  describe('executeCOUNT', () => {
    it('should execute private COUNT query', async () => {
      const result = await engine.executeCOUNT(mockStore, '?s a Patient', 1.0);

      expect(result).toHaveProperty('trueValue');
      expect(result).toHaveProperty('noisyValue');
      expect(result).toHaveProperty('epsilon');
      expect(result.epsilon).toBe(1.0);
      expect(result.mechanism).toBe('laplace');
      expect(result.trueValue).toBe(42);
      expect(result.noisyValue).toBeGreaterThanOrEqual(0);
    });

    it('should add calibrated noise', async () => {
      const results = await Promise.all(
        Array(10).fill(0).map(() => engine.executeCOUNT(mockStore, '?s a Patient', 1.0))
      );

      const noisyValues = results.map(r => r.noisyValue);
      const variance = noisyValues.some(v => v !== noisyValues[0]);

      expect(variance).toBe(true); // Values should vary due to noise
    });

    it('should spend privacy budget', async () => {
      const initialBudget = engine.getRemainingBudget();
      await engine.executeCOUNT(mockStore, '?s a Patient', 2.0);

      expect(engine.getRemainingBudget()).toBe(initialBudget - 2.0);
    });

    it('should enforce budget limits', async () => {
      const smallEngine = new DifferentialPrivacySPARQL({ totalBudget: 1.0 });

      await smallEngine.executeCOUNT(mockStore, '?s a Patient', 0.5);
      await expect(
        smallEngine.executeCOUNT(mockStore, '?s a Patient', 1.0)
      ).rejects.toThrow('Privacy budget exhausted');
    });
  });

  describe('executeSUM', () => {
    beforeEach(() => {
      mockStore.match = async (pattern) => {
        if (pattern.includes('hasAge')) {
          return [
            { age: 25 },
            { age: 30 },
            { age: 45 }
          ];
        }
        return [];
      };
    });

    it('should execute private SUM query', async () => {
      const result = await engine.executeSUM(
        mockStore,
        '?p hasAge ?age',
        'age',
        1.0,
        0,
        120
      );

      expect(result.trueValue).toBe(100); // 25 + 30 + 45
      expect(result.mechanism).toBe('laplace');
      expect(result.noisyValue).toBeGreaterThan(0);
    });
  });

  describe('executeSELECT', () => {
    beforeEach(() => {
      mockStore.match = async (_pattern) => {
        return [
          { disease: 'flu' },
          { disease: 'flu' },
          { disease: 'cold' }
        ];
      };
    });

    it('should use exponential mechanism', async () => {
      const result = await engine.executeSELECT(
        mockStore,
        'disease',
        '?p hasDiagnosis ?disease',
        1.0
      );

      expect(result.noisyValue).toBeDefined();
      expect(result.mechanism).toBe('exponential');
      expect(['flu', 'cold']).toContain(result.noisyValue);
    });
  });

  describe('executeSparseVector', () => {
    it('should test multiple patterns against threshold', async () => {
      const results = await engine.executeSparseVector(
        mockStore,
        ['?p hasFever true', '?p hasCough true'],
        30,
        1.0
      );

      expect(results).toHaveLength(2);
      expect(results[0]).toHaveProperty('pattern');
      expect(results[0]).toHaveProperty('aboveThreshold');
      expect(typeof results[0].aboveThreshold).toBe('boolean');
    });
  });

  describe('getBudgetReceipt', () => {
    it('should return cryptographic receipt', async () => {
      await engine.executeCOUNT(mockStore, '?s a Patient', 1.0);

      const receipt = await engine.getBudgetReceipt();

      expect(receipt).toHaveProperty('id');
      expect(receipt).toHaveProperty('receiptHash');
      expect(receipt).toHaveProperty('queries');
      expect(receipt.queries).toHaveLength(1);
    });
  });
});

describe('Utility Functions', () => {
  describe('allocateEpsilon', () => {
    it('should allocate epsilon equally', () => {
      const allocations = allocateEpsilon(10.0, 5);

      expect(allocations).toHaveLength(5);
      expect(allocations.every(a => a === 2.0)).toBe(true);
    });

    it('should allocate with priorities', () => {
      const allocations = allocateEpsilon(10.0, 3, [2, 1, 1]);

      expect(allocations).toHaveLength(3);
      expect(allocations[0]).toBe(5.0); // 2/4 * 10
      expect(allocations[1]).toBe(2.5); // 1/4 * 10
      expect(allocations[2]).toBe(2.5);
    });

    it('should throw on mismatched priority length', () => {
      expect(() =>
        allocateEpsilon(10.0, 3, [1, 2])
      ).toThrow('Priority array length must match query count');
    });
  });

  describe('estimateNoise', () => {
    it('should estimate Laplace noise', () => {
      const stats = estimateNoise(1.0, 1, 'laplace');

      expect(stats.mechanism).toBe('laplace');
      expect(stats.mean).toBe(0);
      expect(stats.scale).toBe(1.0);
      expect(stats.stdDev).toBeGreaterThan(0);
      expect(stats.confidence95).toBeGreaterThan(0);
    });

    it('should estimate Gaussian noise', () => {
      const stats = estimateNoise(1.0, 1, 'gaussian');

      expect(stats.mechanism).toBe('gaussian');
      expect(stats.mean).toBe(0);
      expect(stats.sigma).toBeGreaterThan(0);
    });

    it('should scale with epsilon', () => {
      const low = estimateNoise(0.1, 1, 'laplace');
      const high = estimateNoise(10.0, 1, 'laplace');

      expect(low.scale).toBeGreaterThan(high.scale);
    });

    it('should throw on unknown mechanism', () => {
      expect(() =>
        estimateNoise(1.0, 1, 'unknown')
      ).toThrow('Unknown mechanism');
    });
  });
});
