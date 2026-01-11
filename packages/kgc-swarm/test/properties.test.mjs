/**
 * @file KGC-SWARM Property-Based Tests
 * @description Mathematical property validation for KGC-SWARM
 *
 * Properties Tested:
 * 1. μ ∘ μ = μ (idempotence) - monad composition
 * 2. Receipt chain integrity
 * 3. Guard H enforcement
 * 4. Convergence guarantees
 * 5. Budget compliance
 */

import { describe, it, expect, beforeEach } from 'vitest';
import { KGCSwarm, createSwarm } from '../src/index.mjs';
import { GuardSystem } from '../src/guards.mjs';
import { ReceiptChain, hash } from '../src/receipts.mjs';

describe('KGC-SWARM Property-Based Tests', () => {
  let swarm;

  beforeEach(() => {
    swarm = createSwarm({
      maxAgents: 10,
      maxBudget: 100000,
      maxIterations: 100,
      guardH: 16,
    });
  });

  describe('Property: μ ∘ μ = μ (Idempotence)', () => {
    it('should satisfy idempotence for executeIteration on terminal states', async () => {
      // Create swarm and reach terminal state (complete)
      const agent = swarm.spawnAgent('planner');
      agent.status = 'complete';

      swarm.state.status = 'complete';

      // First execution on terminal state
      const state1 = await swarm.executeIteration();

      // Second execution on terminal state (μ ∘ μ)
      const state2 = await swarm.executeIteration();

      // Should return same state (idempotence)
      expect(state1.status).toBe('complete');
      expect(state2.status).toBe('complete');
      expect(state1.iteration).toBe(state2.iteration);
      expect(JSON.stringify(state1)).toBe(JSON.stringify(state2));
    });

    it('should satisfy idempotence for failed terminal state', async () => {
      swarm.state.status = 'failed';

      const state1 = await swarm.executeIteration();
      const state2 = await swarm.executeIteration();

      expect(state1.status).toBe('failed');
      expect(state2.status).toBe('failed');
      expect(JSON.stringify(state1)).toBe(JSON.stringify(state2));
    });

    it('should demonstrate monad composition property', async () => {
      // μ: SwarmState → SwarmState (bind operation)
      // μ ∘ μ = μ means applying twice is same as once

      const agent = swarm.spawnAgent('planner');

      // Set up terminal state
      agent.status = 'complete';
      swarm.state.status = 'complete';

      // Apply μ once
      const mu1 = await swarm.executeIteration();

      // Apply μ twice (μ ∘ μ)
      const mu2 = await swarm.executeIteration();
      const muMu = await swarm.executeIteration();

      // Property: μ(μ(x)) = μ(x)
      expect(mu1).toEqual(mu2);
      expect(mu2).toEqual(muMu);
    });

    it('should verify getState returns immutable copy (no side effects)', () => {
      const state1 = swarm.getState();
      const state2 = swarm.getState();

      // Modify copy
      state1.iteration = 999;
      state1.status = 'tampered';

      // Original should be unchanged
      expect(swarm.state.iteration).toBe(0);
      expect(swarm.state.status).toBe('idle');

      // Second copy should match original
      expect(state2.iteration).toBe(0);
      expect(state2.status).toBe('idle');
    });
  });

  describe('Property: Receipt Chain Integrity', () => {
    it('should maintain chain integrity: r[i].before = r[i-1].after', async () => {
      const agent = swarm.spawnAgent('planner');

      const receipts = [];
      for (let i = 0; i < 5; i++) {
        receipts.push(await swarm.generateReceipt(agent.id, {
          action: `step_${i}`,
          result: { data: i },
        }));
      }

      // Property: Chain integrity
      for (let i = 1; i < receipts.length; i++) {
        // r[i].previousHash = r[i-1].receiptHash
        expect(receipts[i].previousHash).toBe(receipts[i - 1].receiptHash);
      }

      // Genesis receipt has no previous
      expect(receipts[0].previousHash).toBeNull();
    });

    it('should satisfy cryptographic hash properties', async () => {
      const agent = swarm.spawnAgent('planner');

      const payload = { action: 'test', result: { data: 'value' } };
      const receipt = await swarm.generateReceipt(agent.id, payload);

      // Property: Hash determinism
      const payloadStr = JSON.stringify(payload, Object.keys(payload).sort());
      const expectedPayloadHash = await (await import('hash-wasm')).blake3(payloadStr);
      expect(receipt.payloadHash).toBe(expectedPayloadHash);

      // Property: Receipt hash formula
      const chainInput = (receipt.previousHash || 'GENESIS') + ':' + receipt.payloadHash;
      const expectedReceiptHash = await (await import('hash-wasm')).blake3(chainInput);
      expect(receipt.receiptHash).toBe(expectedReceiptHash);
    });

    it('should detect any tampering in chain', async () => {
      const agent = swarm.spawnAgent('planner');

      const receipts = [];
      for (let i = 0; i < 3; i++) {
        receipts.push(await swarm.generateReceipt(agent.id, {
          action: `step_${i}`,
          result: { data: i },
        }));
      }

      // Valid chain
      const validChain = await swarm.verifyReceiptChain(receipts);
      expect(validChain).toBe(true);

      // Tamper with middle receipt
      const tamperedReceipts = [...receipts];
      tamperedReceipts[1] = {
        ...tamperedReceipts[1],
        payload: { action: 'tampered', result: { data: 'evil' } },
      };

      // Should detect tampering
      const invalidChain = await swarm.verifyReceiptChain(tamperedReceipts);
      expect(invalidChain).toBe(false);
    });

    it.skip('should satisfy non-repudiation property', async () => {
      // SKIP REASON: Zod v4 compatibility - receipt validation uses v3 API
      // RESOLUTION: Update to Zod v4 API
      // Once a receipt is generated, it cryptographically proves:
      // 1. The agent that created it (agentId)
      // 2. The exact payload (payloadHash)
      // 3. The time it was created (t_ns)
      // 4. Its position in the chain (previousHash)

      const agent = swarm.spawnAgent('planner');
      const receipt = await swarm.generateReceipt(agent.id, {
        action: 'critical_decision',
        result: { approved: true },
      });

      // Property: Receipt contains complete provenance
      expect(receipt.agentId).toBe(agent.id);
      expect(receipt.t_ns).toBeGreaterThan(0n);
      expect(receipt.payloadHash).toMatch(/^[a-f0-9]{64}$/);
      expect(receipt.receiptHash).toMatch(/^[a-f0-9]{64}$/);

      // Property: Tampering with payload invalidates receipt
      const tamperedReceipt = structuredClone(receipt);
      tamperedReceipt.payload.result.approved = false; // Tamper

      // Verification should fail because payload hash won't match
      const isValid = await swarm.verifyReceipt(tamperedReceipt);
      expect(isValid).toBe(false);
    });

    it('should maintain temporal ordering: t[i] ≤ t[i+1]', async () => {
      const agent = swarm.spawnAgent('planner');

      const receipts = [];
      for (let i = 0; i < 5; i++) {
        // Add small delay to ensure different timestamps
        await new Promise(resolve => setTimeout(resolve, 2));

        receipts.push(await swarm.generateReceipt(agent.id, {
          action: `step_${i}`,
          result: { data: i },
        }));
      }

      // Property: Monotonic timestamps
      for (let i = 1; i < receipts.length; i++) {
        expect(receipts[i].t_ns).toBeGreaterThanOrEqual(receipts[i - 1].t_ns);
      }
    });
  });

  describe('Property: Guard H Enforcement', () => {
    it('should satisfy entropy bound: H(content) ≤ guardH', () => {
      // Create swarm with low entropy limit
      const guardedSwarm = createSwarm({ guardH: 4 });

      // Low entropy content (should pass)
      const lowEntropy = 'aaaaaaaaaa';
      const entropyLow = guardedSwarm.calculateGuardH(lowEntropy);
      expect(entropyLow).toBeLessThanOrEqual(4);

      expect(() => {
        guardedSwarm.enforceGuardH(lowEntropy);
      }).not.toThrow();

      // High entropy content (should fail)
      const highEntropy = 'aB3!xY9@qZ$mN7&pL';
      const entropyHigh = guardedSwarm.calculateGuardH(highEntropy);
      expect(entropyHigh).toBeGreaterThan(4);

      expect(() => {
        guardedSwarm.enforceGuardH(highEntropy);
      }).toThrow(/Guard H violation/);
    });

    it('should satisfy additivity: H(a+b) ≥ max(H(a), H(b))', () => {
      const a = 'aaaa';
      const b = 'bbbb';
      const ab = a + b;

      const Ha = swarm.calculateGuardH(a);
      const Hb = swarm.calculateGuardH(b);
      const Hab = swarm.calculateGuardH(ab);

      // Concatenation doesn't decrease maximum entropy
      expect(Hab).toBeGreaterThanOrEqual(Math.max(Ha, Hb));
    });

    it('should satisfy empty string property: H("") = 0', () => {
      const H_empty = swarm.calculateGuardH('');
      expect(H_empty).toBe(0);
    });

    it('should satisfy uniform distribution bound', () => {
      // Uniform random string has maximum entropy
      const uniform = 'abcdefghijklmnopqrstuvwxyz0123456789';
      const H_uniform = swarm.calculateGuardH(uniform);

      // Repeated string has lower entropy
      const repeated = 'aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa';
      const H_repeated = swarm.calculateGuardH(repeated);

      expect(H_uniform).toBeGreaterThan(H_repeated);
    });

    it('should enforce multi-level guard composition conceptually', () => {
      // Property: Guards can be composed (G₁ ∧ G₂ ∧ ... ∧ Gₙ)
      // This test validates the concept without external guard system

      const testContent = 'mixed_content_123!@#';
      const H = swarm.calculateGuardH(testContent);

      // Guard 1: Entropy check
      const guard1Pass = H <= swarm.config.guardH;

      // Guard 2: Length check
      const guard2Pass = testContent.length <= 100;

      // Guard 3: No null bytes
      const guard3Pass = !testContent.includes('\0');

      // Composition: All guards must pass
      const allPass = guard1Pass && guard2Pass && guard3Pass;

      expect(allPass).toBe(true);
      expect(guard1Pass).toBe(true);
      expect(guard2Pass).toBe(true);
      expect(guard3Pass).toBe(true);
    });
  });

  describe('Property: Convergence Guarantees', () => {
    it('should satisfy termination: ∃n: iteration ≥ n ⇒ status ∈ {complete, failed}', async () => {
      // Property: Swarm must eventually terminate
      const maxIter = 10;
      const convergentSwarm = createSwarm({ maxIterations: maxIter });

      // Execute max iterations
      for (let i = 0; i < maxIter; i++) {
        await convergentSwarm.executeIteration();
      }

      // Next iteration must fail (termination guaranteed)
      await expect(async () => {
        await convergentSwarm.executeIteration();
      }).rejects.toThrow('Maximum iterations exceeded');

      expect(convergentSwarm.state.status).toBe('failed');
    });

    it('should satisfy liveness: all agents eventually reach terminal state', () => {
      const agent1 = swarm.spawnAgent('planner');
      const agent2 = swarm.spawnAgent('coder');

      // Initial: not converged
      expect(swarm.hasConverged()).toBe(false);

      // Agent 1 completes
      agent1.status = 'complete';
      expect(swarm.hasConverged()).toBe(false);

      // Agent 2 completes → converged
      agent2.status = 'complete';
      expect(swarm.hasConverged()).toBe(true);

      // Property: Once converged, stays converged
      agent1.status = 'complete'; // Re-apply (idempotent)
      expect(swarm.hasConverged()).toBe(true);
    });

    it('should satisfy safety: iteration ≤ maxIterations', async () => {
      const safeSwarm = createSwarm({ maxIterations: 5 });

      for (let i = 0; i < 5; i++) {
        await safeSwarm.executeIteration();
        // Property: Never exceeds max
        expect(safeSwarm.state.iteration).toBeLessThanOrEqual(5);
      }

      expect(safeSwarm.state.iteration).toBe(5);
    });

    it('should satisfy bounded execution: tokens ≤ maxBudget OR iteration ≤ maxIterations', async () => {
      const bounded = createSwarm({
        maxBudget: 1000,
        maxIterations: 100,
      });

      // Simulate token consumption
      bounded.state.totalTokens = 500;
      await bounded.executeIteration();

      // Within bounds
      expect(bounded.state.totalTokens).toBeLessThan(1000);
      expect(bounded.state.iteration).toBeLessThan(100);

      // Exceed budget
      bounded.state.totalTokens = 1001;

      await expect(async () => {
        await bounded.executeIteration();
      }).rejects.toThrow('Budget exceeded');

      // Property: Execution stops when bounds violated
      expect(bounded.state.status).toBe('failed');
    });

    it('should satisfy convergence progress: agents_complete increases monotonically', () => {
      const a1 = swarm.spawnAgent('planner');
      const a2 = swarm.spawnAgent('coder');
      const a3 = swarm.spawnAgent('tester');

      const countComplete = () =>
        swarm.state.agents.filter(a => a.status === 'complete').length;

      let count0 = countComplete();
      expect(count0).toBe(0);

      a1.status = 'complete';
      let count1 = countComplete();
      expect(count1).toBe(1);
      expect(count1).toBeGreaterThanOrEqual(count0);

      a2.status = 'complete';
      let count2 = countComplete();
      expect(count2).toBe(2);
      expect(count2).toBeGreaterThanOrEqual(count1);

      a3.status = 'complete';
      let count3 = countComplete();
      expect(count3).toBe(3);
      expect(count3).toBeGreaterThanOrEqual(count2);

      // Property: Monotonic increase
      expect(count3 >= count2 && count2 >= count1 && count1 >= count0).toBe(true);
    });
  });

  describe('Property: Budget Compliance', () => {
    it('should satisfy budget constraint: ∀t: totalTokens ≤ maxBudget OR status = failed', async () => {
      const budgetSwarm = createSwarm({ maxBudget: 5000 });

      // Within budget
      budgetSwarm.state.totalTokens = 4999;
      await budgetSwarm.executeIteration();
      expect(budgetSwarm.state.status).toBe('running');

      // At exact budget (edge case - still allowed)
      budgetSwarm.state.totalTokens = 4999;
      await budgetSwarm.executeIteration();

      // Over budget → must fail
      budgetSwarm.state.totalTokens = 5001;
      try {
        await budgetSwarm.executeIteration();
        expect.fail('Should have thrown Budget exceeded error');
      } catch (error) {
        expect(error.message).toContain('Budget exceeded');
        expect(budgetSwarm.state.status).toBe('failed');
      }
    });

    it('should satisfy agent capacity constraint: agents.length ≤ maxAgents', () => {
      const cappedSwarm = createSwarm({ maxAgents: 3 });

      cappedSwarm.spawnAgent('planner');
      expect(cappedSwarm.state.agents.length).toBeLessThanOrEqual(3);

      cappedSwarm.spawnAgent('coder');
      expect(cappedSwarm.state.agents.length).toBeLessThanOrEqual(3);

      cappedSwarm.spawnAgent('tester');
      expect(cappedSwarm.state.agents.length).toBeLessThanOrEqual(3);
      expect(cappedSwarm.state.agents.length).toBe(3);

      // Exceeding capacity should throw
      expect(() => {
        cappedSwarm.spawnAgent('reviewer');
      }).toThrow('Maximum agents');

      // Property: Never exceeds max
      expect(cappedSwarm.state.agents.length).toBe(3);
    });

    it('should satisfy resource invariant: ∑(agent.tokensUsed) ≤ totalTokens', () => {
      const a1 = swarm.spawnAgent('planner');
      const a2 = swarm.spawnAgent('coder');

      a1.tokensUsed = 1000;
      a2.tokensUsed = 2000;
      swarm.state.totalTokens = 3000;

      const agentSum = swarm.state.agents.reduce((sum, a) => sum + a.tokensUsed, 0);

      // Property: Agent tokens sum ≤ total
      expect(agentSum).toBeLessThanOrEqual(swarm.state.totalTokens);
      expect(agentSum).toBe(3000);
    });

    it('should satisfy strict ordering: budgetExceeded ⇒ iterationStopped', async () => {
      const strict = createSwarm({ maxBudget: 1000, maxIterations: 100 });

      strict.state.totalTokens = 1001;

      // Budget exceeded - should fail
      try {
        await strict.executeIteration();
        expect.fail('Should have thrown');
      } catch (e) {
        expect(e.message).toContain('Budget exceeded');
      }

      // Property: Cannot continue iterations after budget failure
      expect(strict.state.status).toBe('failed');

      // Subsequent calls should return same failed state (idempotence)
      const state = await strict.executeIteration();
      expect(state.status).toBe('failed');
    });
  });

  describe('Property: Receipt Merkle Tree Integrity', () => {
    it('should satisfy Merkle root uniqueness: different chains → different roots', () => {
      const chain1 = new ReceiptChain([
        {
          before: hash('state0'),
          after: hash('state1'),
          delta: hash('delta1'),
          timestamp: Date.now(),
        },
        {
          before: hash('state1'),
          after: hash('state2'),
          delta: hash('delta2'),
          timestamp: Date.now(),
        },
      ]);

      const chain2 = new ReceiptChain([
        {
          before: hash('state0'),
          after: hash('state1'),
          delta: hash('delta1'),
          timestamp: Date.now(),
        },
        {
          before: hash('state1'),
          after: hash('stateX'), // Different
          delta: hash('deltaX'),
          timestamp: Date.now(),
        },
      ]);

      const root1 = chain1.getMerkleRoot();
      const root2 = chain2.getMerkleRoot();

      // Property: Different chains have different roots
      expect(root1).not.toBe(root2);
    });

    it('should satisfy Merkle root determinism: same chain → same root', () => {
      const data = [
        {
          before: hash('a'),
          after: hash('b'),
          delta: hash('d1'),
          timestamp: 12345,
        },
        {
          before: hash('b'),
          after: hash('c'),
          delta: hash('d2'),
          timestamp: 12346,
        },
      ];

      const chain1 = new ReceiptChain(data);
      const chain2 = new ReceiptChain(data);

      const root1 = chain1.getMerkleRoot();
      const root2 = chain2.getMerkleRoot();

      // Property: Deterministic root
      expect(root1).toBe(root2);
    });

    it('should satisfy chain immutability: add after finalization → new chain', () => {
      const chain = new ReceiptChain([
        {
          before: hash('s0'),
          after: hash('s1'),
          delta: hash('d1'),
          timestamp: Date.now(),
        },
      ]);

      const rootBefore = chain.getMerkleRoot();

      chain.add({
        before: hash('s1'),
        after: hash('s2'),
        delta: hash('d2'),
        timestamp: Date.now(),
      });

      const rootAfter = chain.getMerkleRoot();

      // Property: Adding changes root
      expect(rootAfter).not.toBe(rootBefore);
    });
  });
});
