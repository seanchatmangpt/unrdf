/**
 * @file Resolution Layer for Multi-Agent Coordination
 * @module resolution-layer
 *
 * @description
 * Implements multi-agent coordination and Delta resolution for swarm behavior.
 * Handles competing proposals from multiple agents and resolves them into
 * a single, consolidated Delta.
 */

import { createStore } from '@unrdf/oxigraph'; // Oxigraph Store
import { randomUUID } from 'crypto';
import { z } from 'zod';

/**
 * Schema for agent proposal
 */
const AgentProposalSchema = z.object({
  id: z.string().uuid(),
  agentId: z.string().min(1),
  delta: z.object({
    additions: z.array(z.any()),
    removals: z.array(z.any()),
    metadata: z.record(z.any()).optional(),
  }),
  confidence: z.number().min(0).max(1),
  priority: z.number().int().min(0).max(100).default(50),
  timestamp: z.number(),
  metadata: z.record(z.any()).optional(),
  dependencies: z.array(z.string()).optional(),
  conflicts: z.array(z.string()).optional(),
});

/**
 * Schema for resolution strategy
 */
const ResolutionStrategySchema = z.object({
  type: z.enum(['voting', 'merging', 'crdt', 'consensus', 'priority', 'random']),
  parameters: z.record(z.any()).optional(),
  timeout: z.number().int().positive().max(300000).default(30000),
  quorum: z.number().min(0).max(1).default(0.5),
  maxRetries: z.number().int().nonnegative().max(10).default(3),
});

/**
 * Schema for resolution result
 */
const ResolutionResultSchema = z.object({
  id: z.string().uuid(),
  strategy: z.string(),
  proposals: z.array(AgentProposalSchema),
  resolvedDelta: z.object({
    additions: z.array(z.any()),
    removals: z.array(z.any()),
    metadata: z.record(z.any()).optional(),
  }),
  confidence: z.number().min(0).max(1),
  consensus: z.boolean(),
  conflicts: z
    .array(
      z.object({
        type: z.enum(['addition', 'removal', 'metadata']),
        proposals: z.array(z.string()),
        resolution: z.string(),
      })
    )
    .optional(),
  timestamp: z.number(),
  duration: z.number().nonnegative(),
});

/**
 * Resolution Layer for multi-agent coordination
 */
export class ResolutionLayer {
  /**
   * Create a new resolution layer
   * @param {Object} [config] - Configuration
   */
  constructor(config = {}) {
    this.config = {
      defaultStrategy: config.defaultStrategy || 'voting',
      maxProposals: config.maxProposals || 100,
      enableConflictDetection: config.enableConflictDetection !== false,
      enableConsensus: config.enableConsensus !== false,
      timeout: config.timeout || 30000,
      ...config,
    };

    this.proposals = new Map();
    this.resolutionHistory = [];
    this.agents = new Map();
    this.strategies = new Map();

    // Register default strategies
    this._registerDefaultStrategies();
  }

  /**
   * Register an agent
   * @param {string} agentId - Agent identifier
   * @param {Object} [metadata] - Agent metadata
   */
  registerAgent(agentId, metadata = {}) {
    this.agents.set(agentId, {
      id: agentId,
      metadata,
      registeredAt: Date.now(),
      proposalCount: 0,
      lastActivity: Date.now(),
    });
  }

  /**
   * Submit a proposal from an agent
   * @param {string} agentId - Agent identifier
   * @param {Object} delta - Proposed delta
   * @param {Object} [options] - Proposal options
   * @returns {Promise<string>} Proposal ID
   */
  async submitProposal(agentId, delta, options = {}) {
    // Validate agent
    if (!this.agents.has(agentId)) {
      this.registerAgent(agentId);
    }

    const proposal = {
      id: randomUUID(),
      agentId,
      delta,
      confidence: options.confidence || 0.5,
      priority: options.priority || 50,
      timestamp: Date.now(),
      metadata: options.metadata || {},
      dependencies: options.dependencies || [],
      conflicts: options.conflicts || [],
    };

    // Validate proposal
    const validatedProposal = AgentProposalSchema.parse(proposal);

    // Store proposal
    this.proposals.set(validatedProposal.id, validatedProposal);

    // Update agent stats
    const agent = this.agents.get(agentId);
    agent.proposalCount++;
    agent.lastActivity = Date.now();

    return validatedProposal.id;
  }

  /**
   * Resolve proposals using specified strategy
   * @param {Array<string>} proposalIds - Proposal IDs to resolve
   * @param {Object} [strategy] - Resolution strategy
   * @returns {Promise<Object>} Resolution result
   */
  async resolveProposals(proposalIds, strategy = {}) {
    const startTime = Date.now();
    const resolutionId = randomUUID();

    try {
      // Get proposals
      const proposals = proposalIds.map(id => this.proposals.get(id)).filter(p => p !== undefined);

      if (proposals.length === 0) {
        throw new Error('No valid proposals found');
      }

      // Validate strategy
      const validatedStrategy = ResolutionStrategySchema.parse({
        type: strategy.type || this.config.defaultStrategy,
        parameters: strategy.parameters || {},
        timeout: strategy.timeout || this.config.timeout,
        quorum: strategy.quorum || 0.5,
        maxRetries: strategy.maxRetries || 3,
      });

      // Get resolution function
      const resolutionFn = this.strategies.get(validatedStrategy.type);
      if (!resolutionFn) {
        throw new Error(`Unknown resolution strategy: ${validatedStrategy.type}`);
      }

      // Resolve proposals
      const resolvedDelta = await resolutionFn(proposals, validatedStrategy);

      // Calculate consensus
      const consensus = this._calculateConsensus(proposals, resolvedDelta);

      // Detect conflicts
      const conflicts = this.config.enableConflictDetection
        ? this._detectConflicts(proposals, resolvedDelta)
        : [];

      const result = {
        id: resolutionId,
        strategy: validatedStrategy.type,
        proposals,
        resolvedDelta,
        confidence: this._calculateConfidence(proposals, resolvedDelta),
        consensus,
        conflicts,
        timestamp: Date.now(),
        duration: Date.now() - startTime,
      };

      // Validate result
      const validatedResult = ResolutionResultSchema.parse(result);

      // Store in history
      this.resolutionHistory.push(validatedResult);

      return validatedResult;
    } catch (error) {
      throw new Error(`Resolution failed: ${error.message}`);
    }
  }

  /**
   * Get all proposals for an agent
   * @param {string} agentId - Agent identifier
   * @returns {Array} Agent proposals
   */
  getAgentProposals(agentId) {
    return Array.from(this.proposals.values()).filter(p => p.agentId === agentId);
  }

  /**
   * Get all active proposals
   * @returns {Array} All proposals
   */
  getAllProposals() {
    return Array.from(this.proposals.values());
  }

  /**
   * Get resolution history
   * @param {number} [limit] - Limit number of results
   * @returns {Array} Resolution history
   */
  getResolutionHistory(limit) {
    const history = [...this.resolutionHistory].reverse();
    return limit ? history.slice(0, limit) : history;
  }

  /**
   * Get layer statistics
   * @returns {Object} Statistics
   */
  getStats() {
    const proposals = Array.from(this.proposals.values());
    const agents = Array.from(this.agents.values());

    return {
      config: this.config,
      proposals: {
        total: proposals.length,
        byAgent: agents.reduce((acc, agent) => {
          acc[agent.id] = agent.proposalCount;
          return acc;
        }, {}),
      },
      agents: {
        total: agents.length,
        active: agents.filter(a => Date.now() - a.lastActivity < 300000).length, // 5 minutes
      },
      resolutions: {
        total: this.resolutionHistory.length,
        strategies: this.resolutionHistory.reduce((acc, r) => {
          acc[r.strategy] = (acc[r.strategy] || 0) + 1;
          return acc;
        }, {}),
      },
    };
  }

  /**
   * Register default resolution strategies
   * @private
   */
  _registerDefaultStrategies() {
    // Voting strategy
    this.strategies.set('voting', async (proposals, _strategy) => {
      const votes = new Map();

      for (const proposal of proposals) {
        const weight = proposal.confidence * (proposal.priority / 100);
        const key = JSON.stringify(proposal.delta);
        votes.set(key, (votes.get(key) || 0) + weight);
      }

      // Find highest voted delta
      let bestDelta = null;
      let bestWeight = 0;

      for (const [deltaKey, weight] of votes) {
        if (weight > bestWeight) {
          bestWeight = weight;
          bestDelta = JSON.parse(deltaKey);
        }
      }

      return bestDelta;
    });

    // Merging strategy
    this.strategies.set('merging', async (proposals, _strategy) => {
      const merged = { additions: [], removals: [], metadata: {} };

      for (const proposal of proposals) {
        merged.additions.push(...proposal.delta.additions);
        merged.removals.push(...proposal.delta.removals);
        Object.assign(merged.metadata, proposal.delta.metadata || {});
      }

      // Remove duplicates
      merged.additions = this._deduplicateQuads(merged.additions);
      merged.removals = this._deduplicateQuads(merged.removals);

      return merged;
    });

    // Priority strategy
    this.strategies.set('priority', async (proposals, _strategy) => {
      const sorted = proposals.sort((a, b) => b.priority - a.priority);
      return sorted[0].delta;
    });

    // Random strategy
    this.strategies.set('random', async (proposals, _strategy) => {
      const randomIndex = Math.floor(Math.random() * proposals.length);
      return proposals[randomIndex].delta;
    });

    // CRDT strategy (simplified)
    this.strategies.set('crdt', async (proposals, _strategy) => {
      const crdt = { additions: new Map(), removals: new Map() };

      for (const proposal of proposals) {
        // Add additions with timestamps
        for (const quad of proposal.delta.additions) {
          const key = this._quadToKey(quad);
          const existing = crdt.additions.get(key);
          if (!existing || proposal.timestamp > existing.timestamp) {
            crdt.additions.set(key, {
              quad,
              timestamp: proposal.timestamp,
              agent: proposal.agentId,
            });
          }
        }

        // Add removals with timestamps
        for (const quad of proposal.delta.removals) {
          const key = this._quadToKey(quad);
          const existing = crdt.removals.get(key);
          if (!existing || proposal.timestamp > existing.timestamp) {
            crdt.removals.set(key, {
              quad,
              timestamp: proposal.timestamp,
              agent: proposal.agentId,
            });
          }
        }
      }

      return {
        additions: Array.from(crdt.additions.values()).map(v => v.quad),
        removals: Array.from(crdt.removals.values()).map(v => v.quad),
        metadata: { strategy: 'crdt', timestamp: Date.now() },
      };
    });
  }

  /**
   * Calculate consensus
   * @param {Array} proposals - Proposals
   * @param {Object} resolvedDelta - Resolved delta
   * @returns {boolean} Consensus achieved
   * @private
   */
  _calculateConsensus(proposals, resolvedDelta) {
    if (proposals.length === 0) return false;

    const resolvedKey = JSON.stringify(resolvedDelta);
    let consensusCount = 0;

    for (const proposal of proposals) {
      const proposalKey = JSON.stringify(proposal.delta);
      if (proposalKey === resolvedKey) {
        consensusCount++;
      }
    }

    const consensusRatio = consensusCount / proposals.length;
    return consensusRatio >= 0.5; // 50% consensus threshold
  }

  /**
   * Detect conflicts between proposals
   * @param {Array} proposals - Proposals
   * @param {Object} resolvedDelta - Resolved delta
   * @returns {Array} Conflicts
   * @private
   */
  _detectConflicts(proposals, resolvedDelta) {
    const conflicts = [];

    // Check for conflicting additions/removals
    const resolvedAdditions = new Set(resolvedDelta.additions.map(q => this._quadToKey(q)));
    const resolvedRemovals = new Set(resolvedDelta.removals.map(q => this._quadToKey(q)));

    for (const proposal of proposals) {
      const proposalAdditions = new Set(proposal.delta.additions.map(q => this._quadToKey(q)));
      const proposalRemovals = new Set(proposal.delta.removals.map(q => this._quadToKey(q)));

      // Check for conflicts
      for (const key of proposalAdditions) {
        if (resolvedRemovals.has(key)) {
          conflicts.push({
            type: 'addition',
            proposals: [proposal.id],
            resolution: 'conflict detected',
          });
        }
      }

      for (const key of proposalRemovals) {
        if (resolvedAdditions.has(key)) {
          conflicts.push({
            type: 'removal',
            proposals: [proposal.id],
            resolution: 'conflict detected',
          });
        }
      }
    }

    return conflicts;
  }

  /**
   * Calculate confidence score
   * @param {Array} proposals - Proposals
   * @param {Object} resolvedDelta - Resolved delta
   * @returns {number} Confidence score
   * @private
   */
  _calculateConfidence(proposals, _resolvedDelta) {
    if (proposals.length === 0) return 0;

    let totalConfidence = 0;
    let weightSum = 0;

    for (const proposal of proposals) {
      const weight = proposal.priority / 100;
      totalConfidence += proposal.confidence * weight;
      weightSum += weight;
    }

    return weightSum > 0 ? totalConfidence / weightSum : 0;
  }

  /**
   * Deduplicate quads
   * @param {Array} quads - Quads array
   * @returns {Array} Deduplicated quads
   * @private
   */
  _deduplicateQuads(quads) {
    const seen = new Set();
    return quads.filter(quad => {
      const key = this._quadToKey(quad);
      if (seen.has(key)) {
        return false;
      }
      seen.add(key);
      return true;
    });
  }

  /**
   * Convert quad to key
   * @param {Object} quad - RDF quad
   * @returns {string} Quad key
   * @private
   */
  _quadToKey(quad) {
    return `${quad.subject?.value || ''}:${quad.predicate?.value || ''}:${quad.object?.value || ''}:${quad.graph?.value || ''}`;
  }
}

/**
 * Create a resolution layer instance
 * @param {Object} [config] - Configuration
 * @returns {ResolutionLayer} Resolution layer
 */
export function createResolutionLayer(config = {}) {
  return new ResolutionLayer(config);
}

/**
 * Default resolution layer instance
 */
export const defaultResolutionLayer = createResolutionLayer();
