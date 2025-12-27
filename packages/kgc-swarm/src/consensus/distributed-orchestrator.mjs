/**
 * @file distributed-orchestrator.mjs
 * @description Distributed KGC-SWARM Orchestrator with consensus support
 *
 * Extends single-node orchestrator with:
 * - Raft consensus for strongly consistent replication
 * - Byzantine fault tolerance for untrusted environments
 * - CRDT-based eventual consistency for partition tolerance
 * - Node discovery and membership management
 *
 * @module @unrdf/kgc-swarm/consensus/distributed-orchestrator
 */

import { z } from 'zod';
import { EventEmitter } from 'node:events';
import { KGCSwarmOrchestrator } from '../orchestrator.mjs';
import { RaftNode } from './raft.mjs';
import { ByzantineNode } from './byzantine.mjs';
import { LWWElementSet } from './crdt.mjs';
import { MembershipManager } from './membership.mjs';

/**
 * Consensus mode enumeration
 * @enum {string}
 */
export const ConsensusMode = {
  RAFT: 'raft', // Strong consistency, leader-based
  BYZANTINE: 'byzantine', // Byzantine fault tolerance
  CRDT: 'crdt', // Eventual consistency, partition tolerance
  NONE: 'none', // Single-node mode
};

/**
 * Distributed orchestrator configuration
 * @typedef {Object} DistributedConfig
 * @property {string} nodeId - This node's identifier
 * @property {string} host - This node's host
 * @property {number} port - This node's port
 * @property {ConsensusMode} mode - Consensus mode
 * @property {string[]} peers - Peer node identifiers
 * @property {Object} [raftConfig] - Raft-specific configuration
 * @property {Object} [byzantineConfig] - Byzantine-specific configuration
 * @property {Object} [membershipConfig] - Membership-specific configuration
 * @property {Object} [orchestratorConfig] - Base orchestrator configuration
 */
const DistributedConfigSchema = z.object({
  nodeId: z.string(),
  host: z.string(),
  port: z.number().int().positive(),
  mode: z.nativeEnum(ConsensusMode),
  peers: z.array(z.string()).default([]),
  raftConfig: z.object({}).passthrough().optional(),
  byzantineConfig: z.object({}).passthrough().optional(),
  membershipConfig: z.object({}).passthrough().optional(),
  orchestratorConfig: z.object({}).passthrough().optional(),
});

/**
 * DistributedOrchestrator: Multi-node orchestration with consensus
 *
 * Modes:
 * - **Raft**: Strong consistency, requires majority (n/2 + 1) for commits
 * - **Byzantine**: Tolerates f Byzantine failures with 3f+1 nodes
 * - **CRDT**: Eventual consistency, always available (AP in CAP)
 * - **None**: Single-node fallback
 *
 * @class DistributedOrchestrator
 * @extends EventEmitter
 * @example
 * ```javascript
 * // Raft mode - strong consistency
 * const raftOrch = new DistributedOrchestrator({
 *   nodeId: 'node-1',
 *   host: 'localhost',
 *   port: 7001,
 *   mode: 'raft',
 *   peers: ['node-2', 'node-3', 'node-4', 'node-5']
 * });
 *
 * // Byzantine mode - f=1 failures with 4 nodes
 * const bftOrch = new DistributedOrchestrator({
 *   nodeId: 'node-1',
 *   host: 'localhost',
 *   port: 8001,
 *   mode: 'byzantine',
 *   peers: ['node-1', 'node-2', 'node-3', 'node-4'],
 *   byzantineConfig: { f: 1 }
 * });
 *
 * // CRDT mode - partition tolerant
 * const crdtOrch = new DistributedOrchestrator({
 *   nodeId: 'node-1',
 *   host: 'localhost',
 *   port: 9001,
 *   mode: 'crdt',
 *   peers: ['node-2', 'node-3']
 * });
 * ```
 */
export class DistributedOrchestrator extends EventEmitter {
  /**
   * @param {DistributedConfig} config - Distributed configuration
   */
  constructor(config) {
    super();
    this.config = DistributedConfigSchema.parse(config);

    /** @type {string} */
    this.nodeId = this.config.nodeId;

    /** @type {ConsensusMode} */
    this.mode = this.config.mode;

    /** @type {KGCSwarmOrchestrator} - Local orchestrator */
    this.localOrchestrator = new KGCSwarmOrchestrator(this.config.orchestratorConfig);

    /** @type {RaftNode | null} */
    this.raftNode = null;

    /** @type {ByzantineNode | null} */
    this.byzantineNode = null;

    /** @type {LWWElementSet | null} */
    this.crdtSet = null;

    /** @type {MembershipManager} */
    this.membership = new MembershipManager({
      nodeId: this.config.nodeId,
      host: this.config.host,
      port: this.config.port,
      ...this.config.membershipConfig,
    });

    /** @type {boolean} */
    this.running = false;

    // Initialize consensus layer based on mode
    this._initializeConsensus();

    // Setup membership event handlers
    this._setupMembershipHandlers();
  }

  /**
   * Initialize consensus layer
   * @private
   */
  _initializeConsensus() {
    switch (this.mode) {
      case ConsensusMode.RAFT:
        this.raftNode = new RaftNode({
          nodeId: this.config.nodeId,
          peers: this.config.peers,
          ...this.config.raftConfig,
        });

        this.raftNode.on('stateChange', (state, term) => {
          this.emit('consensusStateChange', { mode: 'raft', state, term });
        });

        this.raftNode.on('committed', (entry) => {
          this.emit('consensusCommit', entry);
          this._applyConsensusEntry(entry);
        });

        break;

      case ConsensusMode.BYZANTINE:
        this.byzantineNode = new ByzantineNode({
          nodeId: this.config.nodeId,
          peers: this.config.peers,
          ...this.config.byzantineConfig,
        });

        this.byzantineNode.on('committed', (sequence, request) => {
          this.emit('consensusCommit', { sequence, request });
          this._applyConsensusEntry(request);
        });

        this.byzantineNode.on('viewChange', (view) => {
          this.emit('consensusStateChange', { mode: 'byzantine', view });
        });

        break;

      case ConsensusMode.CRDT:
        this.crdtSet = new LWWElementSet(this.config.nodeId);

        // Setup periodic sync
        setInterval(() => {
          if (this.running) {
            this._syncCRDT();
          }
        }, 5000); // Sync every 5 seconds

        break;

      case ConsensusMode.NONE:
        // Single-node mode - no consensus
        break;

      default:
        throw new Error(`Unknown consensus mode: ${this.mode}`);
    }
  }

  /**
   * Setup membership event handlers
   * @private
   */
  _setupMembershipHandlers() {
    this.membership.on('memberJoined', (member) => {
      this.emit('nodeJoined', member);
      this._handleMembershipChange();
    });

    this.membership.on('memberFailed', (member) => {
      this.emit('nodeFailed', member);
      this._handleMembershipChange();
    });

    this.membership.on('memberLeft', (member) => {
      this.emit('nodeLeft', member);
      this._handleMembershipChange();
    });
  }

  /**
   * Handle membership changes
   * @private
   */
  _handleMembershipChange() {
    const clusterSize = this.membership.getClusterSize();

    // Check if we have quorum for Raft
    if (this.mode === ConsensusMode.RAFT && this.raftNode) {
      const majority = Math.floor(this.config.peers.length / 2) + 1;
      const hasQuorum = clusterSize >= majority;

      this.emit('quorumChange', { hasQuorum, clusterSize, required: majority });
    }

    // Check if we have sufficient nodes for Byzantine
    if (this.mode === ConsensusMode.BYZANTINE && this.byzantineNode) {
      const f = this.byzantineNode.f;
      const required = 3 * f + 1;
      const hasSufficient = clusterSize >= required;

      this.emit('quorumChange', { hasSufficient, clusterSize, required });
    }
  }

  /**
   * Start distributed orchestrator
   * @returns {Promise<void>}
   */
  async start() {
    if (this.running) return;

    this.running = true;

    // Start membership management
    await this.membership.start();

    // Start consensus layer
    switch (this.mode) {
      case ConsensusMode.RAFT:
        await this.raftNode.start();
        break;

      case ConsensusMode.BYZANTINE:
        await this.byzantineNode.start();
        break;

      case ConsensusMode.CRDT:
        // CRDT is always available
        break;

      case ConsensusMode.NONE:
        // No consensus to start
        break;
    }

    this.emit('started', this.nodeId);
  }

  /**
   * Stop distributed orchestrator
   * @returns {Promise<void>}
   */
  async stop() {
    if (!this.running) return;

    this.running = false;

    // Stop consensus layer
    switch (this.mode) {
      case ConsensusMode.RAFT:
        await this.raftNode.stop();
        break;

      case ConsensusMode.BYZANTINE:
        await this.byzantineNode.stop();
        break;

      case ConsensusMode.CRDT:
        // No cleanup needed
        break;

      case ConsensusMode.NONE:
        // No consensus to stop
        break;
    }

    // Stop membership management
    await this.membership.stop();

    this.emit('stopped', this.nodeId);
  }

  /**
   * Run distributed orchestration
   * @param {any} σ - Seed parameter
   * @param {any} κ - Control parameter
   * @param {Object} [options] - Execution options
   * @returns {Promise<any>} Execution result
   */
  async run(σ, κ, options = {}) {
    if (!this.running) {
      throw new Error('Orchestrator not running. Call start() first.');
    }

    // Propose execution via consensus
    const proposal = {
      type: 'run',
      seed: σ,
      control: κ,
      options,
      timestamp: Date.now(),
    };

    switch (this.mode) {
      case ConsensusMode.RAFT:
        if (this.raftNode.state !== 'leader') {
          throw new Error(`Not leader. Current leader: ${this.raftNode.leaderId || 'unknown'}`);
        }
        await this.raftNode.propose(proposal);
        break;

      case ConsensusMode.BYZANTINE:
        if (!this.byzantineNode.isPrimary()) {
          throw new Error(`Not primary. Current primary: ${this.byzantineNode.getPrimary()}`);
        }
        await this.byzantineNode.request(proposal.type, proposal);
        break;

      case ConsensusMode.CRDT:
        // CRDT - always accept locally, sync later
        this.crdtSet.add(proposal);
        await this._applyConsensusEntry(proposal);
        break;

      case ConsensusMode.NONE:
        // Single-node - execute directly
        return this.localOrchestrator.run(σ, κ, options);
    }

    // Wait for consensus and execution
    return new Promise((resolve, reject) => {
      const timeout = setTimeout(() => {
        reject(new Error('Consensus timeout'));
      }, 30000); // 30 second timeout

      this.once('executionComplete', (result) => {
        clearTimeout(timeout);
        resolve(result);
      });
    });
  }

  /**
   * Apply consensus entry to local state
   * @private
   * @param {any} entry - Consensus entry
   */
  async _applyConsensusEntry(entry) {
    if (entry.type === 'run') {
      const result = await this.localOrchestrator.run(
        entry.seed,
        entry.control,
        entry.options
      );

      this.emit('executionComplete', result);
    }
  }

  /**
   * Sync CRDT with peers
   * @private
   */
  async _syncCRDT() {
    if (!this.crdtSet) return;

    const members = this.membership.getMembers();

    for (const member of members) {
      if (member.nodeId !== this.nodeId && member.status === 'alive') {
        try {
          // Request CRDT state from peer
          const peerState = await this._requestPeerCRDT(member.nodeId);

          if (peerState) {
            // Merge peer state
            this.crdtSet.merge(peerState);
            this.emit('crdtMerged', member.nodeId);
          }
        } catch (error) {
          this.emit('syncError', member.nodeId, error);
        }
      }
    }
  }

  /**
   * Request CRDT state from peer
   * @private
   * @param {string} peerId - Peer node ID
   * @returns {Promise<LWWElementSet>}
   */
  async _requestPeerCRDT(peerId) {
    // TODO: Implement network request
    // This would send an RPC to the peer requesting their CRDT state
    throw new Error('Not implemented - requires network layer');
  }

  /**
   * Set network handler for consensus
   * @param {Function} handler - Network handler function
   */
  setNetworkHandler(handler) {
    this.membership.setNetworkHandler(handler);

    if (this.raftNode) {
      this.raftNode.setRPCHandler(handler);
    }

    if (this.byzantineNode) {
      this.byzantineNode.setNetworkHandler(handler);
    }
  }

  /**
   * Get orchestrator state
   * @returns {Object} State information
   */
  getState() {
    const baseState = {
      nodeId: this.nodeId,
      mode: this.mode,
      running: this.running,
      clusterSize: this.membership.getClusterSize(),
      members: this.membership.getMembers(),
    };

    switch (this.mode) {
      case ConsensusMode.RAFT:
        return {
          ...baseState,
          consensus: this.raftNode.getState(),
        };

      case ConsensusMode.BYZANTINE:
        return {
          ...baseState,
          consensus: this.byzantineNode.getState(),
        };

      case ConsensusMode.CRDT:
        return {
          ...baseState,
          consensus: {
            elements: this.crdtSet.size(),
          },
        };

      case ConsensusMode.NONE:
        return {
          ...baseState,
          consensus: null,
        };

      default:
        return baseState;
    }
  }

  /**
   * Get local orchestrator (for direct access)
   * @returns {KGCSwarmOrchestrator} Local orchestrator
   */
  getLocalOrchestrator() {
    return this.localOrchestrator;
  }
}

/**
 * Create a new distributed orchestrator
 * @param {DistributedConfig} config - Configuration
 * @returns {DistributedOrchestrator} Distributed orchestrator instance
 */
export function createDistributedOrchestrator(config) {
  return new DistributedOrchestrator(config);
}

// Export schemas and enums
export { DistributedConfigSchema };
