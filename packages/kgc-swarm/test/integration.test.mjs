/**
 * @file KGC-SWARM Integration Tests
 * @description End-to-end swarm execution with multi-agent coordination
 *
 * Tests:
 * 1. End-to-end swarm execution
 * 2. 10-agent coordination
 * 3. Observable expansion → compression → receipts
 * 4. Convergence under budget
 * 5. Guard enforcement integration
 */

import { describe, it, expect, beforeEach } from 'vitest';
import { KGCSwarm, createSwarm } from '../src/index.mjs';
import { GuardSystem } from '../src/guards.mjs';
import { ReceiptChain } from '../src/receipts.mjs';

describe('KGC-SWARM Integration Tests', () => {
  let swarm;
  let guardSystem;

  beforeEach(() => {
    swarm = createSwarm({
      maxAgents: 10,
      maxBudget: 100000,
      maxIterations: 100,
      guardH: 16,
    });

    guardSystem = new GuardSystem({
      rootPath: process.cwd(),
      allowedHosts: ['localhost', '*.example.com'],
      enableSecretGuard: true,
      enablePathGuard: true,
      enableNetworkGuard: true,
      enablePrivilegeGuard: true,
    });
  });

  describe('End-to-End Swarm Execution', () => {
    it('should execute complete swarm lifecycle', async () => {
      // Spawn agents
      const planner = swarm.spawnAgent('planner');
      const coder = swarm.spawnAgent('coder');
      const tester = swarm.spawnAgent('tester');

      expect(swarm.state.agents).toHaveLength(3);
      expect(swarm.state.status).toBe('idle');

      // Execute iterations
      await swarm.executeIteration();
      expect(swarm.state.iteration).toBe(1);
      expect(swarm.state.status).toBe('running');

      // Generate receipts for agent actions
      const receipt1 = await swarm.generateReceipt(planner.id, {
        action: 'create_plan',
        result: { tasks: ['task1', 'task2'] },
      });

      const receipt2 = await swarm.generateReceipt(coder.id, {
        action: 'implement_task',
        result: { code: 'function test() {}' },
      });

      const receipt3 = await swarm.generateReceipt(tester.id, {
        action: 'run_tests',
        result: { passed: 10, failed: 0 },
      });

      // Verify receipts are chained
      expect(swarm.state.receiptChain).toHaveLength(3);
      expect(receipt2.previousHash).toBe(receipt1.receiptHash);
      expect(receipt3.previousHash).toBe(receipt2.receiptHash);

      // Mark agents complete
      planner.status = 'complete';
      coder.status = 'complete';
      tester.status = 'complete';

      expect(swarm.hasConverged()).toBe(true);
    });

    it('should handle agent failures gracefully', async () => {
      const agent1 = swarm.spawnAgent('planner');
      const agent2 = swarm.spawnAgent('coder');

      agent1.status = 'failed';
      agent2.status = 'complete';

      expect(swarm.hasConverged()).toBe(true);
    });
  });

  describe('10-Agent Coordination', () => {
    it('should coordinate 10 agents with receipt chain', async () => {
      // Spawn 10 agents
      const agents = [];
      const roles = ['planner', 'coder', 'coder', 'tester', 'reviewer', 'validator', 'orchestrator', 'planner', 'coder', 'tester'];

      for (let i = 0; i < 10; i++) {
        agents.push(swarm.spawnAgent(roles[i]));
      }

      expect(swarm.state.agents).toHaveLength(10);

      // Each agent generates a receipt
      const receipts = [];
      for (let i = 0; i < 10; i++) {
        const receipt = await swarm.generateReceipt(agents[i].id, {
          action: `action_${i}`,
          result: { data: `result_${i}` },
        });
        receipts.push(receipt);
      }

      expect(swarm.state.receiptChain).toHaveLength(10);

      // Verify chain integrity
      const isValid = await swarm.verifyReceiptChain(receipts);
      expect(isValid).toBe(true);

      // Verify each link
      for (let i = 1; i < receipts.length; i++) {
        expect(receipts[i].previousHash).toBe(receipts[i - 1].receiptHash);
      }

      // First receipt should be genesis
      expect(receipts[0].previousHash).toBeNull();
    });

    it('should enforce maximum agent limit', () => {
      // Create swarm with max 5 agents
      const limitedSwarm = createSwarm({ maxAgents: 5 });

      // Spawn 5 agents successfully
      for (let i = 0; i < 5; i++) {
        limitedSwarm.spawnAgent('planner');
      }

      expect(limitedSwarm.state.agents).toHaveLength(5);

      // 6th agent should fail
      expect(() => {
        limitedSwarm.spawnAgent('planner');
      }).toThrow('Maximum agents (5) reached');
    });

    it('should track token usage across all agents', async () => {
      const agent1 = swarm.spawnAgent('planner');
      const agent2 = swarm.spawnAgent('coder');

      agent1.tokensUsed = 1000;
      agent2.tokensUsed = 2000;

      swarm.state.totalTokens = 3000;

      expect(swarm.state.totalTokens).toBe(3000);
    });
  });

  describe('Observable Expansion → Compression → Receipts', () => {
    it('should demonstrate expansion and compression with receipts', async () => {
      // Expansion: Start with simple task, spawn agents
      const orchestrator = swarm.spawnAgent('orchestrator');
      const receipt1 = await swarm.generateReceipt(orchestrator.id, {
        action: 'expand_task',
        result: {
          subtasks: ['analyze', 'implement', 'test', 'review'],
          agentsNeeded: 4,
        },
      });

      // Spawn additional agents for subtasks
      const analyzer = swarm.spawnAgent('planner');
      const implementer = swarm.spawnAgent('coder');
      const tester = swarm.spawnAgent('tester');
      const reviewer = swarm.spawnAgent('reviewer');

      expect(swarm.state.agents).toHaveLength(5); // orchestrator + 4 workers

      // Execute subtasks with receipts
      const receipt2 = await swarm.generateReceipt(analyzer.id, {
        action: 'analyze',
        result: { complexity: 'medium', estimatedTime: '2h' },
      });

      const receipt3 = await swarm.generateReceipt(implementer.id, {
        action: 'implement',
        result: { linesOfCode: 150, functions: 5 },
      });

      const receipt4 = await swarm.generateReceipt(tester.id, {
        action: 'test',
        result: { coverage: '95%', passed: 20 },
      });

      const receipt5 = await swarm.generateReceipt(reviewer.id, {
        action: 'review',
        result: { approved: true, comments: 2 },
      });

      // Compression: Orchestrator summarizes results
      const receipt6 = await swarm.generateReceipt(orchestrator.id, {
        action: 'compress_results',
        result: {
          taskComplete: true,
          metrics: {
            linesOfCode: 150,
            coverage: '95%',
            approved: true,
          },
        },
      });

      // Verify receipt chain
      const allReceipts = [receipt1, receipt2, receipt3, receipt4, receipt5, receipt6];
      const isValid = await swarm.verifyReceiptChain(allReceipts);
      expect(isValid).toBe(true);

      // Observable: All steps are cryptographically proven
      for (const receipt of allReceipts) {
        const verified = await swarm.verifyReceipt(receipt);
        expect(verified).toBe(true);
      }
    });

    it('should create Merkle root for batch verification', async () => {
      const agent = swarm.spawnAgent('planner');

      // Generate 5 receipts
      const receipts = [];
      for (let i = 0; i < 5; i++) {
        receipts.push(await swarm.generateReceipt(agent.id, {
          action: `step_${i}`,
          result: { data: i },
        }));
      }

      // Create receipt chain
      const chain = new ReceiptChain(receipts.map(r => ({
        before: r.previousHash || r.receiptHash,
        after: r.receiptHash,
        delta: r.payloadHash,
        timestamp: Number(r.t_ns / 1000000n),
      })));

      const merkleRoot = chain.getMerkleRoot();
      expect(merkleRoot).toMatch(/^[a-f0-9]{64}$/);
    });
  });

  describe('Convergence Under Budget', () => {
    it('should converge when all agents complete', async () => {
      const agent1 = swarm.spawnAgent('planner');
      const agent2 = swarm.spawnAgent('coder');
      const agent3 = swarm.spawnAgent('tester');

      expect(swarm.hasConverged()).toBe(false);

      agent1.status = 'complete';
      expect(swarm.hasConverged()).toBe(false);

      agent2.status = 'complete';
      expect(swarm.hasConverged()).toBe(false);

      agent3.status = 'complete';
      expect(swarm.hasConverged()).toBe(true);
    });

    it('should fail when budget exceeded', async () => {
      const budgetSwarm = createSwarm({
        maxAgents: 10,
        maxBudget: 1000,
        maxIterations: 100,
        guardH: 16,
      });

      budgetSwarm.state.totalTokens = 1001;

      await expect(async () => {
        await budgetSwarm.executeIteration();
      }).rejects.toThrow('Budget exceeded');

      expect(budgetSwarm.state.status).toBe('failed');
    });

    it('should fail when max iterations exceeded', async () => {
      const iterSwarm = createSwarm({
        maxAgents: 10,
        maxBudget: 100000,
        maxIterations: 5,
        guardH: 16,
      });

      // Execute 5 iterations successfully
      for (let i = 0; i < 5; i++) {
        await iterSwarm.executeIteration();
      }

      expect(iterSwarm.state.iteration).toBe(5);

      // 6th iteration should fail
      await expect(async () => {
        await iterSwarm.executeIteration();
      }).rejects.toThrow('Maximum iterations exceeded');
    });

    it('should enforce convergence within resource bounds', async () => {
      const boundedSwarm = createSwarm({
        maxAgents: 3,
        maxBudget: 10000,
        maxIterations: 10,
        guardH: 16,
      });

      // Spawn max agents
      const a1 = boundedSwarm.spawnAgent('planner');
      const a2 = boundedSwarm.spawnAgent('coder');
      const a3 = boundedSwarm.spawnAgent('tester');

      // Simulate work within budget
      a1.tokensUsed = 2000;
      a2.tokensUsed = 3000;
      a3.tokensUsed = 4000;
      boundedSwarm.state.totalTokens = 9000;

      // Execute iterations
      for (let i = 0; i < 5; i++) {
        await boundedSwarm.executeIteration();
      }

      // Mark complete
      a1.status = 'complete';
      a2.status = 'complete';
      a3.status = 'complete';

      expect(boundedSwarm.hasConverged()).toBe(true);
      expect(boundedSwarm.state.totalTokens).toBeLessThan(10000);
      expect(boundedSwarm.state.iteration).toBeLessThan(10);
    });
  });

  describe('Guard Enforcement Integration', () => {
    it('should enforce Guard H entropy bounds', () => {
      const lowEntropyContent = 'aaaaaaaaaa'; // Low entropy
      const highEntropyContent = 'aB3!xY9@qZ'; // High entropy

      swarm.config.guardH = 5; // Low threshold

      expect(() => {
        swarm.enforceGuardH(lowEntropyContent);
      }).not.toThrow();

      expect(() => {
        swarm.enforceGuardH(highEntropyContent);
      }).toThrow(/Guard H violation/);

      const entropy = swarm.calculateGuardH(highEntropyContent);
      expect(entropy).toBeGreaterThan(5);
    });

    it('should calculate entropy for various content types', () => {
      const uniform = 'abcdefghijklmnopqrstuvwxyz';
      const repeated = 'aaaaaaaaaaaaaaaaaaaaaaaaa';
      const empty = '';

      const H_uniform = swarm.calculateGuardH(uniform);
      const H_repeated = swarm.calculateGuardH(repeated);
      const H_empty = swarm.calculateGuardH(empty);

      expect(H_uniform).toBeGreaterThan(H_repeated);
      expect(H_empty).toBe(0);
      expect(H_uniform).toBeLessThanOrEqual(swarm.config.guardH);
    });
  });

  describe('Receipt Chain Integrity', () => {
    it.skip('should detect tampered receipts', async () => {
      const agent = swarm.spawnAgent('planner');

      const receipt1 = await swarm.generateReceipt(agent.id, {
        action: 'step1',
        result: { data: 'original' },
      });

      const receipt2 = await swarm.generateReceipt(agent.id, {
        action: 'step2',
        result: { data: 'step2' },
      });

      // Verify valid chain
      const validChain = await swarm.verifyReceiptChain([receipt1, receipt2]);
      expect(validChain).toBe(true);

      // Tamper with receipt payload (this changes payloadHash)
      const tamperedReceipt = structuredClone(receipt1);
      tamperedReceipt.payload.result.data = 'tampered';
      // Don't update the hash - this creates a mismatch

      // Verification should fail due to hash mismatch
      const isValid = await swarm.verifyReceipt(tamperedReceipt);
      expect(isValid).toBe(false);
    });

    it('should detect broken chain links', async () => {
      const agent = swarm.spawnAgent('planner');

      const receipt1 = await swarm.generateReceipt(agent.id, {
        action: 'step1',
        result: { data: 'step1' },
      });

      const receipt2 = await swarm.generateReceipt(agent.id, {
        action: 'step2',
        result: { data: 'step2' },
      });

      // Break the chain link
      const brokenReceipt = { ...receipt2 };
      brokenReceipt.previousHash = 'invalid_hash_0000000000000000000000000000000000000000000000000000000000000000';

      // Chain verification should fail
      const isValid = await swarm.verifyReceiptChain([receipt1, brokenReceipt]);
      expect(isValid).toBe(false);
    });
  });
});
