/**
 * Resource & Intervention Benchmarks
 * Perspective: "What should we do?" — Van der Aalst
 *
 * Algorithms:
 *   estimate_queue_delay  — M/M/1 queueing model
 *   rank_interventions    — greedy UCB-like heuristic
 *   select_intervention   — UCB1 multi-armed bandit
 */

import { describe, it, expect, beforeAll, afterAll } from 'vitest';
import { ms, printTable, BenchRow } from './bench-helpers.js';

let wasm: any;
const rows: BenchRow[] = [];

beforeAll(async () => {
  wasm = await import('../../pkg/wasm4pm.js');
  wasm.init();
});

afterAll(() => printTable(rows));

// ─── estimate_queue_delay (M/M/1) ────────────────────────────────────────────

describe('estimate_queue_delay', () => {
  it('stable queue λ=0.5 μ=1.0 → W=2.0s', () => {
    const t = performance.now();
    const result = JSON.parse(wasm.estimate_queue_delay(0.5, 1.0));
    const dur = ms(performance.now() - t);
    expect(result.wait_time).toBeCloseTo(2.0, 1);
    expect(result.utilization).toBeCloseTo(0.5, 2);
    expect(result.is_stable).toBe(true);
    rows.push({
      algorithm: 'estimate_queue_delay(stable)',
      dataset: 'n/a',
      traces: 0,
      durationMs: dur,
      note: `W=${result.wait_time}s ρ=${result.utilization}`,
    });
  });

  it('high-utilization queue λ=0.9 μ=1.0 → W=10.0s', () => {
    const result = JSON.parse(wasm.estimate_queue_delay(0.9, 1.0));
    expect(result.wait_time).toBeCloseTo(10.0, 0);
    expect(result.is_stable).toBe(true);
  });

  it('unstable queue λ≥μ → Infinity', () => {
    const result = JSON.parse(wasm.estimate_queue_delay(1.0, 1.0));
    expect(result.is_stable).toBe(false);
  });

  it('1 000 000 evaluations — nanosecond latency', () => {
    const t = performance.now();
    for (let i = 0; i < 1_000_000; i++) {
      wasm.estimate_queue_delay(0.5, 1.0);
    }
    const perCall = Number(((performance.now() - t) / 1_000_000).toFixed(6));
    rows.push({
      algorithm: 'estimate_queue_delay (1M)',
      dataset: 'n/a',
      traces: 0,
      durationMs: perCall,
      note: 'ms/call (O(1))',
    });
    expect(perCall).toBeLessThan(0.1); // should be sub-100µs
  });
});

// ─── rank_interventions ───────────────────────────────────────────────────────

describe('rank_interventions', () => {
  const interventions = [
    { name: 'escalate', utility: 0.9 },
    { name: 'reassign', utility: 0.6 },
    { name: 'notify', utility: 0.4 },
    { name: 'wait', utility: 0.2 },
  ];

  it('exploit-dominant (w=0.9) — top is highest utility', () => {
    const t = performance.now();
    const result = JSON.parse(wasm.rank_interventions(JSON.stringify(interventions), 0.9));
    const dur = ms(performance.now() - t);
    expect(Array.isArray(result)).toBe(true);
    expect(result[0].name).toBe('escalate');
    rows.push({
      algorithm: 'rank_interventions(w=0.9)',
      dataset: 'n/a',
      traces: 0,
      durationMs: dur,
      note: `top=${result[0].name}`,
    });
  });

  it('balanced (w=0.5) — stable ordering', () => {
    const result = JSON.parse(wasm.rank_interventions(JSON.stringify(interventions), 0.5));
    expect(result).toHaveLength(interventions.length);
    // All items present
    const names = result.map((r: any) => r.name);
    for (const i of interventions) expect(names).toContain(i.name);
  });

  it('ranks include score and rank fields', () => {
    const result = JSON.parse(wasm.rank_interventions(JSON.stringify(interventions), 0.7));
    for (let i = 0; i < result.length; i++) {
      expect(result[i]).toHaveProperty('name');
      expect(result[i]).toHaveProperty('score');
      expect(result[i]).toHaveProperty('rank');
      expect(result[i].rank).toBe(i + 1);
    }
  });

  it('100 000 rankings — latency', () => {
    const t = performance.now();
    for (let i = 0; i < 100_000; i++) {
      wasm.rank_interventions(JSON.stringify(interventions), 0.7);
    }
    const perCall = Number(((performance.now() - t) / 100_000).toFixed(5));
    rows.push({
      algorithm: 'rank_interventions (100k)',
      dataset: 'n/a',
      traces: 0,
      durationMs: perCall,
      note: 'ms/call',
    });
    expect(perCall).toBeLessThan(1);
  });
});

// ─── select_intervention (UCB1) ───────────────────────────────────────────────

describe('select_intervention (UCB1)', () => {
  it('zero-pull arm is always selected first (forced exploration)', () => {
    const bandit = {
      arms: [
        { name: 'A', total_reward: 8.0, pull_count: 10 },
        { name: 'B', total_reward: 3.0, pull_count: 5 },
        { name: 'C', total_reward: 0.0, pull_count: 0 },
      ],
      total_pulls: 15,
    };
    const t = performance.now();
    const result = JSON.parse(wasm.select_intervention(JSON.stringify(bandit), Math.SQRT2));
    const dur = ms(performance.now() - t);
    expect(result.selected).toBe('C');
    rows.push({
      algorithm: 'select_intervention(UCB1)',
      dataset: 'n/a',
      traces: 0,
      durationMs: dur,
      note: `selected=${result.selected}`,
    });
  });

  it('high-performing arm dominates after enough pulls', () => {
    const bandit = {
      arms: [
        { name: 'A', total_reward: 90.0, pull_count: 100 }, // mean=0.9
        { name: 'B', total_reward: 20.0, pull_count: 100 }, // mean=0.2
      ],
      total_pulls: 200,
    };
    const result = JSON.parse(wasm.select_intervention(JSON.stringify(bandit), 0.01)); // minimal exploration
    expect(result.selected).toBe('A');
  });

  it('returns all required fields', () => {
    const bandit = {
      arms: [{ name: 'X', total_reward: 5.0, pull_count: 10 }],
      total_pulls: 10,
    };
    const result = JSON.parse(wasm.select_intervention(JSON.stringify(bandit), Math.SQRT2));
    expect(result).toHaveProperty('selected');
    expect(result).toHaveProperty('arm_index');
    expect(result).toHaveProperty('ucb_score');
    expect(result).toHaveProperty('mean_reward');
    expect(result).toHaveProperty('exploration_bonus');
  });

  it('1 000 000 UCB1 selections — nanosecond latency', () => {
    const bandit = {
      arms: [
        { name: 'A', total_reward: 8.0, pull_count: 10 },
        { name: 'B', total_reward: 5.0, pull_count: 10 },
        { name: 'C', total_reward: 2.0, pull_count: 10 },
      ],
      total_pulls: 30,
    };
    const s = JSON.stringify(bandit);
    const t = performance.now();
    for (let i = 0; i < 1_000_000; i++) {
      wasm.select_intervention(s, Math.SQRT2);
    }
    const perCall = Number(((performance.now() - t) / 1_000_000).toFixed(6));
    rows.push({
      algorithm: 'select_intervention (1M)',
      dataset: 'n/a',
      traces: 0,
      durationMs: perCall,
      note: 'ms/call (O(k))',
    });
    expect(perCall).toBeLessThan(0.5);
  });
});
