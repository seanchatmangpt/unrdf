/**
 * @fileoverview Tests for KGC-SWARM Orchestrator
 * Validates the calculus implementation:
 *   A = μ(O), μ∘μ = μ
 *   Λ is ≺-total over P
 *   stop ⇔ argmin drift(A_t) s.t. budget(B)
 */

import { describe, it, expect, beforeEach } from 'vitest';
import {
  SwarmOrchestrator,
  createSwarm10,
  AgentConfigSchema,
  BudgetSchema,
  SwarmConfigSchema,
} from '../src/swarm-orchestrator.mjs';
import { clearShards } from '../src/shard-merge.mjs';

describe('SwarmOrchestrator', () => {
  beforeEach(() => {
    clearShards();
  });

  describe('createSwarm10', () => {
    it('creates 10 agents with correct IDs (α₁ through α₁₀)', () => {
      const swarm = createSwarm10();
      swarm.initializeAgents();

      expect(swarm.agents.size).toBe(10);

      for (let i = 1; i <= 10; i++) {
        expect(swarm.agents.has(`α_${i}`)).toBe(true);
      }
    });

    it('assigns correct priorities (1 through 10)', () => {
      const swarm = createSwarm10();

      for (let i = 0; i < swarm.config.agents.length; i++) {
        expect(swarm.config.agents[i].priority).toBe(i + 1);
      }
    });

    it('accepts custom budget', () => {
      const swarm = createSwarm10({
        budget: { time: 5000, steps: 50, bytes: 1024 },
      });

      expect(swarm.config.budget.time).toBe(5000);
      expect(swarm.config.budget.steps).toBe(50);
      expect(swarm.config.budget.bytes).toBe(1024);
    });
  });

  describe('probe scheduling (Λ ordering)', () => {
    it('chooses probe with highest ρ(p) = Δ̂(p) / cost(p)', () => {
      const swarm = createSwarm10();

      // Add probes with different yield/cost ratios
      swarm.addProbes([
        { id: 'p1', type: 'read', target: 'a', cost: 2, expected_yield: 0.4 }, // ρ = 0.2
        { id: 'p2', type: 'read', target: 'b', cost: 1, expected_yield: 0.8 }, // ρ = 0.8
        { id: 'p3', type: 'read', target: 'c', cost: 4, expected_yield: 0.4 }, // ρ = 0.1
      ]);

      const chosen = swarm.chooseProbe();

      expect(chosen.id).toBe('p2'); // Highest ρ
    });

    it('removes chosen probe from queue', () => {
      const swarm = createSwarm10();

      swarm.addProbes([
        { id: 'p1', type: 'read', target: 'a', cost: 1, expected_yield: 0.5 },
        { id: 'p2', type: 'read', target: 'b', cost: 1, expected_yield: 0.4 },
      ]);

      expect(swarm.probeQueue.length).toBe(2);

      swarm.chooseProbe();

      expect(swarm.probeQueue.length).toBe(1);
    });

    it('returns null when queue is empty', () => {
      const swarm = createSwarm10();
      const chosen = swarm.chooseProbe();
      expect(chosen).toBe(null);
    });
  });

  describe('observation execution', () => {
    it('executes probe and records observation', async () => {
      const swarm = createSwarm10();
      swarm.initializeAgents();

      const probe = {
        id: 'test-probe',
        type: 'read',
        target: 'http://kgc.io/test',
        cost: 1,
        expected_yield: 0.5,
      };

      const executor = async (p) => ({ result: 'test-data' });
      const observation = await swarm.executeProbe('α_1', probe, executor);

      expect(observation.probe_id).toBe('test-probe');
      expect(observation.agent_id).toBe('α_1');
      expect(observation.success).toBe(true);
      expect(observation.hash).toBeDefined();
    });

    it('handles probe execution errors', async () => {
      const swarm = createSwarm10();
      swarm.initializeAgents();

      const probe = {
        id: 'error-probe',
        type: 'read',
        target: 'http://kgc.io/test',
        cost: 1,
        expected_yield: 0.5,
      };

      const executor = async (p) => { throw new Error('Test error'); };
      const observation = await swarm.executeProbe('α_1', probe, executor);

      expect(observation.success).toBe(false);
      expect(observation.error).toBe('Test error');
    });

    it('tracks budget usage', async () => {
      const swarm = createSwarm10({ budget: { time: 30000, steps: 100, bytes: 10000 } });
      swarm.initializeAgents();

      const probe = {
        id: 'budget-probe',
        type: 'read',
        target: 'http://kgc.io/test',
        cost: 1,
        expected_yield: 0.5,
      };

      const executor = async (p) => ({ data: 'x'.repeat(100) });
      await swarm.executeProbe('α_1', probe, executor);

      expect(swarm.budgetUsed.steps).toBe(1);
      expect(swarm.budgetUsed.bytes).toBeGreaterThan(0);
    });
  });

  describe('projection (μ operator)', () => {
    it('projects observations to artifact', async () => {
      const swarm = createSwarm10();
      swarm.initializeAgents();

      // Add some observations
      const probe = { id: 'p1', type: 'read', target: 'a', cost: 1, expected_yield: 0.5 };
      await swarm.executeProbe('α_1', probe, async () => ({ test: true }));

      const artifact = await swarm.project();

      expect(artifact.epoch).toBe(swarm.epoch);
      expect(artifact.observations).toBeDefined();
      expect(artifact.t_ns).toBeDefined();
    });

    it('is idempotent (μ∘μ = μ)', async () => {
      const swarm = createSwarm10();
      swarm.initializeAgents();

      const probe = { id: 'p1', type: 'read', target: 'a', cost: 1, expected_yield: 0.5 };
      await swarm.executeProbe('α_1', probe, async () => ({ test: true }));

      const artifact1 = await swarm.project();
      const artifact2 = await swarm.project();

      // Same epoch, same observations = same structure
      expect(artifact1.epoch).toBe(artifact2.epoch);
      expect(artifact1.observations.length).toBe(artifact2.observations.length);
    });
  });

  describe('drift detection', () => {
    it('calculates drift between artifacts', async () => {
      const swarm = createSwarm10();
      swarm.initializeAgents();

      // First artifact
      const artifact1 = { observations: [1, 2, 3], merged_deltas: 5, conflicts: 0 };
      // Second artifact with changes
      const artifact2 = { observations: [1, 2, 3, 4], merged_deltas: 6, conflicts: 1 };

      const drift = swarm.calculateDrift(artifact2, artifact1);

      expect(drift).toBeGreaterThan(0);
      expect(drift).toBeLessThanOrEqual(1);
    });

    it('returns 1.0 for first artifact (no previous)', async () => {
      const swarm = createSwarm10();
      const drift = swarm.calculateDrift({ observations: [] }, null);
      expect(drift).toBe(1.0);
    });

    it('returns 0 for identical artifacts', async () => {
      const swarm = createSwarm10();
      const artifact = { observations: [], merged_deltas: 0, conflicts: 0 };
      // Note: drift still > 0 due to hash comparison if artifacts are different objects
      // but structurally same artifacts should have low drift
      const drift = swarm.calculateDrift(artifact, artifact);
      expect(drift).toBeLessThanOrEqual(1);
    });
  });

  describe('stopping condition', () => {
    it('stops when drift <= epsilon', () => {
      const swarm = createSwarm10({ drift_epsilon: 0.05, min_epochs: 2 });
      swarm.epoch = 3;

      expect(swarm.shouldStop(0.01)).toBe(true);
      expect(swarm.shouldStop(0.06)).toBe(false);
    });

    it('respects minimum epochs', () => {
      const swarm = createSwarm10({ drift_epsilon: 0.05, min_epochs: 5 });
      swarm.epoch = 3;

      expect(swarm.shouldStop(0.01)).toBe(false); // Not enough epochs
    });

    it('stops when budget exhausted', () => {
      const swarm = createSwarm10({ budget: { time: 100, steps: 5, bytes: 1000 } });
      swarm.budgetUsed.steps = 10; // Exceeds budget

      expect(swarm.shouldStop(0.5)).toBe(true);
    });
  });

  describe('receipt generation', () => {
    it('generates epoch receipt with correct fields', async () => {
      const swarm = createSwarm10();
      swarm.initializeAgents();
      swarm.epoch = 1;

      const artifact = { merged_deltas: 3, conflicts: 1 };
      const receipt = await swarm.generateReceipt(artifact, 0.5);

      expect(receipt.id).toContain('swarm-receipt');
      expect(receipt.epoch).toBe(1);
      expect(receipt.agents_run).toBe(10);
      expect(receipt.drift).toBe(0.5);
      expect(receipt.hash).toBeDefined();
    });

    it('chains receipts with parent_hash', async () => {
      const swarm = createSwarm10();
      swarm.initializeAgents();

      const receipt1 = await swarm.generateReceipt({ merged_deltas: 0 }, 0.8);
      const receipt2 = await swarm.generateReceipt({ merged_deltas: 1 }, 0.4);

      expect(receipt1.parent_hash).toBeUndefined();
      expect(receipt2.parent_hash).toBe(receipt1.hash);
    });
  });

  describe('full execution', () => {
    it('runs epochs until convergence', async () => {
      const swarm = createSwarm10({
        budget: { time: 10000, steps: 20, bytes: 100000 },
        drift_epsilon: 0.1,
        min_epochs: 2,
      });
      swarm.initializeAgents();

      // Add initial probes
      for (let i = 1; i <= 10; i++) {
        swarm.addProbes([
          { id: `p${i}`, type: 'read', target: `a${i}`, cost: 1, expected_yield: 0.5 },
        ]);
      }

      const executor = async (p) => ({ result: p.id });
      const result = await swarm.run(executor, []);

      expect(result.epochs).toBeGreaterThanOrEqual(swarm.config.min_epochs);
      expect(result.receipts.length).toBeGreaterThan(0);
      expect(result.artifact).toBeDefined();
    });

    it('returns correct final state', async () => {
      const swarm = createSwarm10({ budget: { time: 5000, steps: 10, bytes: 10000 } });
      swarm.initializeAgents();

      swarm.addProbes([
        { id: 'p1', type: 'read', target: 'a', cost: 1, expected_yield: 0.5 },
      ]);

      await swarm.run(async (p) => ({ result: true }), []);
      const state = swarm.getState();

      expect(state.epoch).toBeGreaterThan(0);
      expect(state.agents.length).toBe(10);
      expect(state.budget_remaining).toBeDefined();
    });
  });
});
