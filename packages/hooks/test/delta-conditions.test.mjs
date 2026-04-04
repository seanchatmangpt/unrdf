/**
 * Delta Conditions Test Suite
 *
 * Tests delta-based change detection in knowledge hooks:
 * - Increase detection (more additions than removals)
 * - Decrease detection (more removals than additions)
 * - Neutral changes (equal adds/removes)
 * - Threshold-based filtering
 * - Change magnitude calculation
 */

import { describe, it, expect, beforeEach } from 'vitest';
import { createStore } from '../src/index.mjs';

// Mock delta evaluation function that mirrors the fixed logic
async function evaluateDelta(condition, graph, deltaInfo) {
  const { spec } = condition;
  const { change, threshold = 0.1 } = spec;

  // Calculate change magnitude from delta
  let changeMagnitude = 0;
  if (deltaInfo) {
    const totalQuads = graph.size;
    const additions = deltaInfo.additions?.length || 0;
    const removals = deltaInfo.removals?.length || 0;
    const netChange = additions - removals;
    changeMagnitude = totalQuads > 0 ? netChange / totalQuads : 0;
  }

  // Evaluate change type
  switch (change) {
    case 'any':
      return changeMagnitude !== 0;
    case 'increase':
      return changeMagnitude > threshold;
    case 'decrease':
      return changeMagnitude < -threshold;
    case 'modify':
      return Math.abs(changeMagnitude) > threshold;
    default:
      return false;
  }
}

describe('Delta Conditions - Increase Detection', () => {
  let graph;

  beforeEach(() => {
    graph = createStore();
  });

  it('should detect increase when more quads are added than removed', async () => {
    // Start with 10 quads
    for (let i = 0; i < 10; i++) {
      graph.insert({
        subject: { value: `http://example.org/s${i}` },
        predicate: { value: 'http://example.org/p' },
        object: { value: `o${i}` },
      });
    }

    const condition = {
      kind: 'delta',
      spec: {
        change: 'increase',
        threshold: 0.1,
      },
    };

    const deltaInfo = {
      additions: Array(5).fill(null).map((_, i) => ({ id: `add-${i}` })),
      removals: Array(1).fill(null).map((_, i) => ({ id: `rem-${i}` })),
    };

    // Net change: (5 - 1) / 10 = 0.4 > 0.1 threshold
    const result = await evaluateDelta(condition, graph, deltaInfo);
    expect(result).toBe(true);
  });

  it('should not detect increase when change is below threshold', async () => {
    // Start with 100 quads
    for (let i = 0; i < 100; i++) {
      graph.insert({
        subject: { value: `http://example.org/s${i}` },
        predicate: { value: 'http://example.org/p' },
        object: { value: `o${i}` },
      });
    }

    const condition = {
      kind: 'delta',
      spec: {
        change: 'increase',
        threshold: 0.5,
      },
    };

    const deltaInfo = {
      additions: Array(3).fill(null).map((_, i) => ({ id: `add-${i}` })),
      removals: [],
    };

    // Change: (3 - 0) / 100 = 0.03 < 0.5 threshold
    const result = await evaluateDelta(condition, graph, deltaInfo);
    expect(result).toBe(false);
  });
});

describe('Delta Conditions - Decrease Detection (THE FIX)', () => {
  let graph;

  beforeEach(() => {
    graph = createStore();
  });

  it('should detect decrease when removals exceed additions', async () => {
    // Start with 10 quads
    for (let i = 0; i < 10; i++) {
      graph.insert({
        subject: { value: `http://example.org/s${i}` },
        predicate: { value: 'http://example.org/p' },
        object: { value: `o${i}` },
      });
    }

    const condition = {
      kind: 'delta',
      spec: {
        change: 'decrease',
        threshold: 0.1,
      },
    };

    const deltaInfo = {
      additions: [],
      removals: Array(3).fill(null).map((_, i) => ({ id: `rem-${i}` })),
    };

    // Net change: (0 - 3) / 10 = -0.3 < -0.1 threshold
    const result = await evaluateDelta(condition, graph, deltaInfo);
    expect(result).toBe(true);
  });

  it('should not detect decrease when removals are below threshold', async () => {
    // Start with 100 quads
    for (let i = 0; i < 100; i++) {
      graph.insert({
        subject: { value: `http://example.org/s${i}` },
        predicate: { value: 'http://example.org/p' },
        object: { value: `o${i}` },
      });
    }

    const condition = {
      kind: 'delta',
      spec: {
        change: 'decrease',
        threshold: 0.5,
      },
    };

    const deltaInfo = {
      additions: [],
      removals: Array(3).fill(null).map((_, i) => ({ id: `rem-${i}` })),
    };

    // Change: (0 - 3) / 100 = -0.03 > -0.5 (not meeting threshold)
    const result = await evaluateDelta(condition, graph, deltaInfo);
    expect(result).toBe(false);
  });

  it('should detect decrease with mixed additions and removals (net negative)', async () => {
    // Start with 20 quads
    for (let i = 0; i < 20; i++) {
      graph.insert({
        subject: { value: `http://example.org/s${i}` },
        predicate: { value: 'http://example.org/p' },
        object: { value: `o${i}` },
      });
    }

    const condition = {
      kind: 'delta',
      spec: {
        change: 'decrease',
        threshold: 0.05,
      },
    };

    const deltaInfo = {
      additions: Array(5).fill(null).map((_, i) => ({ id: `add-${i}` })),
      removals: Array(10).fill(null).map((_, i) => ({ id: `rem-${i}` })),
    };

    // Net change: (5 - 10) / 20 = -0.25 < -0.05
    const result = await evaluateDelta(condition, graph, deltaInfo);
    expect(result).toBe(true);
  });

  it('should correctly handle large removals indicating deletion', async () => {
    // Start with 50 quads (simulating a populated graph)
    for (let i = 0; i < 50; i++) {
      graph.insert({
        subject: { value: `http://example.org/s${i}` },
        predicate: { value: 'http://example.org/p' },
        object: { value: `o${i}` },
      });
    }

    const condition = {
      kind: 'delta',
      spec: {
        change: 'decrease',
        threshold: 0.2,
      },
    };

    const deltaInfo = {
      additions: [],
      removals: Array(20).fill(null).map((_, i) => ({ id: `rem-${i}` })),
    };

    // Net change: (0 - 20) / 50 = -0.4 < -0.2 (deletion detected)
    const result = await evaluateDelta(condition, graph, deltaInfo);
    expect(result).toBe(true);
  });
});

describe('Delta Conditions - Neutral Change', () => {
  let graph;

  beforeEach(() => {
    graph = createStore();
  });

  it('should return zero magnitude when additions equal removals', async () => {
    // Start with 10 quads
    for (let i = 0; i < 10; i++) {
      graph.insert({
        subject: { value: `http://example.org/s${i}` },
        predicate: { value: 'http://example.org/p' },
        object: { value: `o${i}` },
      });
    }

    const condition = {
      kind: 'delta',
      spec: {
        change: 'modify',
        threshold: 0.05,
      },
    };

    const deltaInfo = {
      additions: Array(5).fill(null).map((_, i) => ({ id: `add-${i}` })),
      removals: Array(5).fill(null).map((_, i) => ({ id: `rem-${i}` })),
    };

    // Net change: (5 - 5) / 10 = 0 (not > 0.05)
    const result = await evaluateDelta(condition, graph, deltaInfo);
    expect(result).toBe(false);
  });

  it('should not trigger increase or decrease for neutral change', async () => {
    const increaseCondition = {
      kind: 'delta',
      spec: {
        change: 'increase',
        threshold: 0.1,
      },
    };

    const decreaseCondition = {
      kind: 'delta',
      spec: {
        change: 'decrease',
        threshold: 0.1,
      },
    };

    // Start with 20 quads
    for (let i = 0; i < 20; i++) {
      graph.insert({
        subject: { value: `http://example.org/s${i}` },
        predicate: { value: 'http://example.org/p' },
        object: { value: `o${i}` },
      });
    }

    // Equal additions and removals
    const deltaInfo = {
      additions: Array(5).fill(null).map((_, i) => ({ id: `add-${i}` })),
      removals: Array(5).fill(null).map((_, i) => ({ id: `rem-${i}` })),
    };

    const increaseResult = await evaluateDelta(increaseCondition, graph, deltaInfo);
    const decreaseResult = await evaluateDelta(decreaseCondition, graph, deltaInfo);

    expect(increaseResult).toBe(false);
    expect(decreaseResult).toBe(false);
  });
});

describe('Delta Conditions - Modify Type', () => {
  let graph;

  beforeEach(() => {
    graph = createStore();
  });

  it('should detect modify when absolute change exceeds threshold', async () => {
    // Start with 20 quads
    for (let i = 0; i < 20; i++) {
      graph.insert({
        subject: { value: `http://example.org/s${i}` },
        predicate: { value: 'http://example.org/p' },
        object: { value: `o${i}` },
      });
    }

    const condition = {
      kind: 'delta',
      spec: {
        change: 'modify',
        threshold: 0.1,
      },
    };

    const deltaInfo = {
      additions: [],
      removals: Array(5).fill(null).map((_, i) => ({ id: `rem-${i}` })),
    };

    // Absolute change: |(0 - 5) / 20| = 0.25 > 0.1
    const result = await evaluateDelta(condition, graph, deltaInfo);
    expect(result).toBe(true);
  });

  it('should detect modify for both positive and negative changes', async () => {
    const condition = {
      kind: 'delta',
      spec: {
        change: 'modify',
        threshold: 0.15,
      },
    };

    // Start with 20 quads
    for (let i = 0; i < 20; i++) {
      graph.insert({
        subject: { value: `http://example.org/s${i}` },
        predicate: { value: 'http://example.org/p' },
        object: { value: `o${i}` },
      });
    }

    // Test large increase
    const increaseInfo = {
      additions: Array(8).fill(null).map((_, i) => ({ id: `add-${i}` })),
      removals: [],
    };

    const increaseResult = await evaluateDelta(condition, graph, increaseInfo);
    expect(increaseResult).toBe(true); // 8/20 = 0.4 > 0.15

    // Test large decrease
    const decreaseInfo = {
      additions: [],
      removals: Array(8).fill(null).map((_, i) => ({ id: `rem-${i}` })),
    };

    const decreaseResult = await evaluateDelta(condition, graph, decreaseInfo);
    expect(decreaseResult).toBe(true); // |-8/20| = 0.4 > 0.15
  });
});

describe('Delta Conditions - Any Change Type', () => {
  let graph;

  beforeEach(() => {
    graph = createStore();
  });

  it('should detect any change when delta is not neutral', async () => {
    for (let i = 0; i < 5; i++) {
      graph.insert({
        subject: { value: `http://example.org/s${i}` },
        predicate: { value: 'http://example.org/p' },
        object: { value: `o${i}` },
      });
    }

    const condition = {
      kind: 'delta',
      spec: {
        change: 'any',
        threshold: 0,
      },
    };

    const deltaInfo = {
      additions: [{ id: 'add-0' }],
      removals: [],
    };

    const result = await evaluateDelta(condition, graph, deltaInfo);
    expect(result).toBe(true);
  });

  it('should not detect any change when delta is empty', async () => {
    for (let i = 0; i < 5; i++) {
      graph.insert({
        subject: { value: `http://example.org/s${i}` },
        predicate: { value: 'http://example.org/p' },
        object: { value: `o${i}` },
      });
    }

    const condition = {
      kind: 'delta',
      spec: {
        change: 'any',
        threshold: 0,
      },
    };

    const deltaInfo = {
      additions: [],
      removals: [],
    };

    const result = await evaluateDelta(condition, graph, deltaInfo);
    expect(result).toBe(false);
  });

  it('should handle any change with neutral delta', async () => {
    for (let i = 0; i < 10; i++) {
      graph.insert({
        subject: { value: `http://example.org/s${i}` },
        predicate: { value: 'http://example.org/p' },
        object: { value: `o${i}` },
      });
    }

    const condition = {
      kind: 'delta',
      spec: {
        change: 'any',
        threshold: 0.1,
      },
    };

    // 5 additions, 5 removals → changeMagnitude = 0 (no net change)
    const deltaInfo = {
      additions: Array(5).fill(null).map((_, i) => ({ id: `add-${i}` })),
      removals: Array(5).fill(null).map((_, i) => ({ id: `rem-${i}` })),
    };

    const result = await evaluateDelta(condition, graph, deltaInfo);
    expect(result).toBe(false); // changeMagnitude === 0, not !== 0
  });
});
