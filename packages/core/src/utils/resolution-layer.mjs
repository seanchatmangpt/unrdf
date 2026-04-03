/**
 * @file Resolution layer for multi-agent conflict resolution
 * @module utils/resolution-layer
 */

import { z } from 'zod';

/**
 * Resolution config schema
 */
const ResolutionConfigSchema = z.object({
  defaultStrategy: z.enum(['voting', 'priority', 'consensus']).default('voting'),
  maxProposals: z.number().default(100),
  enableConflictDetection: z.boolean().default(true),
  enableConsensus: z.boolean().default(true),
  timeout: z.number().default(30000),
});

/**
 * Proposal schema
 */
const ProposalSchema = z.object({
  id: z.string(),
  agentId: z.string(),
  delta: z.any(),
  timestamp: z.number(),
  priority: z.number().default(0),
  votes: z.number().default(0),
  status: z.enum(['pending', 'accepted', 'rejected']).default('pending'),
});

/**
 * Create a resolution layer for managing proposals and conflicts
 *
 * @param {Object} config - Resolution configuration
 * @returns {Object} Resolution layer instance
 */
export function createResolutionLayer(config = {}) {
  const validatedConfig = ResolutionConfigSchema.parse(config);

  /** @type {Map<string, any>} */
  const proposals = new Map();

  /** @type {Map<string, any>} */
  const conflicts = new Map();

  let proposalCounter = 0;

  return {
    /**
     * Initialize the resolution layer
     * @returns {Promise<void>}
     */
    async init() {
      proposals.clear();
      conflicts.clear();
      proposalCounter = 0;
    },

    /**
     * Submit a proposal for resolution
     *
     * @param {string} agentId - Agent identifier
     * @param {Object} delta - Proposed delta
     * @param {Object} [options] - Proposal options
     * @returns {Promise<string>} Proposal ID
     */
    async submitProposal(agentId, delta, options = {}) {
      if (proposals.size >= validatedConfig.maxProposals) {
        throw new Error(`Maximum proposals (${validatedConfig.maxProposals}) exceeded`);
      }

      const proposalId = `proposal_${++proposalCounter}`;
      const proposal = ProposalSchema.parse({
        id: proposalId,
        agentId,
        delta,
        timestamp: Date.now(),
        priority: options.priority || 0,
        votes: 0,
        status: 'pending',
      });

      proposals.set(proposalId, proposal);

      // Detect conflicts if enabled
      if (validatedConfig.enableConflictDetection) {
        await this._detectConflicts(proposalId, delta);
      }

      return proposalId;
    },

    /**
     * Resolve proposals using the specified strategy
     *
     * @param {Array<string>} proposalIds - Proposal IDs to resolve
     * @param {Object} [strategy] - Resolution strategy
     * @returns {Promise<Object>} Resolution result
     */
    async resolveProposals(proposalIds, strategy = {}) {
      const strategyType = strategy.type || validatedConfig.defaultStrategy;
      const targetProposals = proposalIds
        .map(id => proposals.get(id))
        .filter(p => p && p.status === 'pending');

      if (targetProposals.length === 0) {
        return { accepted: [], rejected: [] };
      }

      let accepted = [];
      let rejected = [];

      switch (strategyType) {
        case 'voting':
          // Accept proposals with most votes
          targetProposals.sort((a, b) => b.votes - a.votes);
          accepted = [targetProposals[0]];
          rejected = targetProposals.slice(1);
          break;

        case 'priority':
          // Accept proposals with highest priority
          targetProposals.sort((a, b) => b.priority - a.priority);
          accepted = [targetProposals[0]];
          rejected = targetProposals.slice(1);
          break;

        case 'consensus':
          // Accept if majority agree (simple implementation)
          const threshold = Math.ceil(targetProposals.length / 2);
          accepted = targetProposals.filter(p => p.votes >= threshold);
          rejected = targetProposals.filter(p => p.votes < threshold);
          break;

        default:
          throw new Error(`Unknown resolution strategy: ${strategyType}`);
      }

      // Update proposal statuses
      for (const proposal of accepted) {
        proposal.status = 'accepted';
      }

      for (const proposal of rejected) {
        proposal.status = 'rejected';
      }

      return {
        accepted: accepted.map(p => p.id),
        rejected: rejected.map(p => p.id),
      };
    },

    /**
     * Vote on a proposal
     *
     * @param {string} proposalId - Proposal ID
     * @param {number} [weight=1] - Vote weight
     * @returns {boolean} True if vote was recorded
     */
    vote(proposalId, weight = 1) {
      const proposal = proposals.get(proposalId);
      if (!proposal || proposal.status !== 'pending') {
        return false;
      }

      proposal.votes += weight;
      return true;
    },

    /**
     * Get proposal by ID
     *
     * @param {string} proposalId - Proposal ID
     * @returns {Object | null} Proposal or null
     */
    getProposal(proposalId) {
      return proposals.get(proposalId) || null;
    },

    /**
     * List all proposals
     *
     * @param {Object} [filter] - Filter options
     * @returns {Array<Object>} Array of proposals
     */
    listProposals(filter = {}) {
      let result = Array.from(proposals.values());

      if (filter.status) {
        result = result.filter(p => p.status === filter.status);
      }

      if (filter.agentId) {
        result = result.filter(p => p.agentId === filter.agentId);
      }

      return result;
    },

    /**
     * Get statistics
     *
     * @returns {Object} Statistics
     */
    getStats() {
      const byStatus = {
        pending: 0,
        accepted: 0,
        rejected: 0,
      };

      for (const proposal of proposals.values()) {
        byStatus[proposal.status]++;
      }

      return {
        totalProposals: proposals.size,
        maxProposals: validatedConfig.maxProposals,
        byStatus,
        conflictCount: conflicts.size,
        strategy: validatedConfig.defaultStrategy,
      };
    },

    /**
     * Detect conflicts between proposals
     * @private
     */
    async _detectConflicts(proposalId, _delta) {
      // Simple conflict detection: check for overlapping changes
      // This is a placeholder - real implementation would check quad overlaps
      const conflictId = `conflict_${proposalId}`;
      conflicts.set(conflictId, {
        id: conflictId,
        proposals: [proposalId],
        timestamp: Date.now(),
      });
    },

    /**
     * Cleanup resources
     */
    async cleanup() {
      proposals.clear();
      conflicts.clear();
      proposalCounter = 0;
    },
  };
}
