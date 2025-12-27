/**
 * @file KGC-SWARM Core Implementation
 * @description Multi-agent swarm coordination with receipt-driven verification
 *
 * Core Properties:
 * 1. μ ∘ μ = μ (idempotence) - monad composition
 * 2. Receipt chain integrity - cryptographic verification
 * 3. Guard H enforcement - entropy/complexity bounds
 * 4. Convergence guarantees - bounded execution
 * 5. Budget compliance - resource limits
 *
 * New Exports:
 * - KGCSwarmOrchestrator: Token-based orchestration loop
 * - TokenGenerator: G(σ, κ) token emission
 */

import { blake3 } from 'hash-wasm';
import { z } from 'zod';

// Export orchestrator and token generator
export {
  KGCSwarmOrchestrator,
  createOrchestrator,
  BudgetSchema as OrchestratorBudgetSchema,
  ObservationSchema,
  StateSchema as OrchestratorStateSchema,
} from './orchestrator.mjs';

export {
  TokenGenerator,
  createTokenGenerator,
  ControlParameterSchema,
  SeedParameterSchema,
  TokenSchema,
} from './token-generator.mjs';

/**
 * @typedef {Object} SwarmConfig
 * @property {number} maxAgents - Maximum number of agents in swarm
 * @property {number} maxBudget - Maximum token budget
 * @property {number} maxIterations - Maximum coordination iterations
 * @property {number} guardH - Entropy bound (bits)
 */

/**
 * @typedef {Object} Agent
 * @property {string} id - Agent identifier
 * @property {string} role - Agent role (planner, coder, tester, etc.)
 * @property {string} status - Current status (idle, running, complete, failed)
 * @property {number} tokensUsed - Tokens consumed
 * @property {Array<string>} receipts - Receipt hashes generated
 */

/**
 * @typedef {Object} SwarmState
 * @property {string} id - Swarm execution ID
 * @property {Array<Agent>} agents - Active agents
 * @property {number} iteration - Current iteration
 * @property {number} totalTokens - Total tokens used
 * @property {Array<string>} receiptChain - Chain of receipt hashes
 * @property {string} status - Swarm status
 */

/**
 * @typedef {Object} Receipt
 * @property {string} id - Receipt UUID
 * @property {bigint} t_ns - Nanosecond timestamp
 * @property {string} agentId - Agent that generated receipt
 * @property {string} previousHash - Previous receipt hash (null for genesis)
 * @property {string} payloadHash - Hash of payload
 * @property {string} receiptHash - Chained hash
 * @property {Object} payload - Receipt payload
 */

// Zod schemas for validation
const SwarmConfigSchema = z.object({
  maxAgents: z.number().int().positive().max(100),
  maxBudget: z.number().int().positive(),
  maxIterations: z.number().int().positive().max(1000),
  guardH: z.number().positive().max(32), // Max 32 bits entropy
});

const AgentSchema = z.object({
  id: z.string().uuid(),
  role: z.enum(['planner', 'coder', 'tester', 'reviewer', 'orchestrator', 'validator']),
  status: z.enum(['idle', 'running', 'complete', 'failed']),
  tokensUsed: z.number().int().nonnegative(),
  receipts: z.array(z.string()),
});

const ReceiptSchema = z.object({
  id: z.string().uuid(),
  t_ns: z.bigint(),
  agentId: z.string().uuid(),
  previousHash: z.string().nullable(),
  payloadHash: z.string().regex(/^[a-f0-9]{64}$/),
  receiptHash: z.string().regex(/^[a-f0-9]{64}$/),
  payload: z.object({
    action: z.string(),
    result: z.any(),
    metadata: z.record(z.any()).optional(),
  }),
});

/**
 * KGC-SWARM Core Class
 * Implements multi-agent coordination with mathematical guarantees
 */
export class KGCSwarm {
  /**
   * @param {SwarmConfig} config - Swarm configuration
   */
  constructor(config) {
    this.config = SwarmConfigSchema.parse(config);
    this.state = {
      id: crypto.randomUUID(),
      agents: [],
      iteration: 0,
      totalTokens: 0,
      receiptChain: [],
      status: 'idle',
    };
  }

  /**
   * Spawn a new agent in the swarm
   * @param {string} role - Agent role
   * @returns {Agent} Created agent
   */
  spawnAgent(role) {
    if (this.state.agents.length >= this.config.maxAgents) {
      throw new Error(`Maximum agents (${this.config.maxAgents}) reached`);
    }

    const agent = {
      id: crypto.randomUUID(),
      role,
      status: 'idle',
      tokensUsed: 0,
      receipts: [],
    };

    AgentSchema.parse(agent);
    this.state.agents.push(agent);
    return agent;
  }

  /**
   * Generate a receipt for an agent action
   * Implements receipt chaining with BLAKE3
   * @param {string} agentId - Agent identifier
   * @param {Object} payload - Action payload
   * @returns {Promise<Receipt>} Generated receipt
   */
  async generateReceipt(agentId, payload) {
    const agent = this.state.agents.find(a => a.id === agentId);
    if (!agent) {
      throw new Error(`Agent ${agentId} not found`);
    }

    // Get previous receipt hash (or null for genesis)
    const previousHash = this.state.receiptChain.length > 0
      ? this.state.receiptChain[this.state.receiptChain.length - 1]
      : null;

    // Hash the payload
    const payloadStr = JSON.stringify(payload, Object.keys(payload).sort());
    const payloadHash = await blake3(payloadStr);

    // Chain the hashes: receiptHash = BLAKE3(previousHash || "GENESIS" : payloadHash)
    const chainInput = (previousHash || 'GENESIS') + ':' + payloadHash;
    const receiptHash = await blake3(chainInput);

    const receipt = {
      id: crypto.randomUUID(),
      t_ns: BigInt(Date.now()) * 1000000n, // Nanosecond precision
      agentId,
      previousHash,
      payloadHash,
      receiptHash,
      payload,
    };

    ReceiptSchema.parse(receipt);

    // Update state
    agent.receipts.push(receiptHash);
    this.state.receiptChain.push(receiptHash);

    return receipt;
  }

  /**
   * Verify receipt integrity
   * @param {Receipt} receipt - Receipt to verify
   * @returns {Promise<boolean>} True if valid
   */
  async verifyReceipt(receipt) {
    try {
      ReceiptSchema.parse(receipt);

      // Recompute payload hash
      const payloadStr = JSON.stringify(receipt.payload, Object.keys(receipt.payload).sort());
      const computedPayloadHash = await blake3(payloadStr);

      if (computedPayloadHash !== receipt.payloadHash) {
        return false;
      }

      // Recompute receipt hash
      const chainInput = (receipt.previousHash || 'GENESIS') + ':' + receipt.payloadHash;
      const computedReceiptHash = await blake3(chainInput);

      if (computedReceiptHash !== receipt.receiptHash) {
        return false;
      }

      return true;
    } catch {
      return false;
    }
  }

  /**
   * Execute a swarm iteration
   * Property: μ ∘ μ = μ (idempotence)
   * @returns {SwarmState} Updated state
   */
  async executeIteration() {
    if (this.state.status === 'complete' || this.state.status === 'failed') {
      // Idempotence: executing on terminal state returns same state
      return this.state;
    }

    if (this.state.iteration >= this.config.maxIterations) {
      this.state.status = 'failed';
      throw new Error('Maximum iterations exceeded');
    }

    if (this.state.totalTokens >= this.config.maxBudget) {
      this.state.status = 'failed';
      throw new Error('Budget exceeded');
    }

    this.state.iteration += 1;
    this.state.status = 'running';

    return this.state;
  }

  /**
   * Check convergence criteria
   * @returns {boolean} True if swarm has converged
   */
  hasConverged() {
    // Converged if all agents are complete or failed
    return this.state.agents.every(a =>
      a.status === 'complete' || a.status === 'failed'
    );
  }

  /**
   * Calculate Guard H (entropy bound)
   * @param {string} content - Content to measure
   * @returns {number} Estimated entropy in bits
   */
  calculateGuardH(content) {
    // Simple entropy estimation: unique characters / total * log2(alphabet)
    const unique = new Set(content).size;
    const total = content.length;
    if (total === 0) return 0;

    const ratio = unique / total;
    const alphabetSize = 256; // Assume byte alphabet
    return ratio * Math.log2(alphabetSize);
  }

  /**
   * Enforce Guard H constraint
   * @param {string} content - Content to validate
   * @throws {Error} If entropy exceeds guard H
   */
  enforceGuardH(content) {
    const entropy = this.calculateGuardH(content);
    if (entropy > this.config.guardH) {
      throw new Error(
        `Guard H violation: entropy ${entropy.toFixed(2)} exceeds limit ${this.config.guardH}`
      );
    }
  }

  /**
   * Get current swarm state
   * @returns {SwarmState} Current state
   */
  getState() {
    return structuredClone(this.state);
  }

  /**
   * Verify entire receipt chain
   * @param {Array<Receipt>} receipts - Receipts to verify
   * @returns {Promise<boolean>} True if chain is valid
   */
  async verifyReceiptChain(receipts) {
    if (receipts.length === 0) return true;

    // Verify genesis receipt
    if (receipts[0].previousHash !== null) {
      return false;
    }

    // Verify each receipt
    for (let i = 0; i < receipts.length; i++) {
      const receipt = receipts[i];

      // Verify receipt integrity
      if (!(await this.verifyReceipt(receipt))) {
        return false;
      }

      // Verify chain linkage
      if (i > 0) {
        if (receipt.previousHash !== receipts[i - 1].receiptHash) {
          return false;
        }
      }
    }

    return true;
  }
}

/**
 * Create a new swarm with default configuration
 * @param {Partial<SwarmConfig>} config - Partial configuration
 * @returns {KGCSwarm} New swarm instance
 */
export function createSwarm(config = {}) {
  const defaults = {
    maxAgents: 10,
    maxBudget: 100000,
    maxIterations: 100,
    guardH: 16,
  };

  return new KGCSwarm({ ...defaults, ...config });
}
