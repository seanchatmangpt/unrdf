/**
 * @file distributed-orchestrator.mjs
 * @description Distributed KGC-SWARM orchestrator with Raft, Byzantine, CRDT, and local modes.
 */

import { z } from 'zod';
import { EventEmitter } from 'node:events';
import { KGCSwarmOrchestrator } from '../orchestrator.mjs';
import { RaftNode } from './raft.mjs';
import { ByzantineNode } from './byzantine.mjs';
import { LWWElementSet } from './crdt.mjs';
import { MembershipManager } from './membership.mjs';

export const ConsensusMode = {
  RAFT: 'raft',
  BYZANTINE: 'byzantine',
  CRDT: 'crdt',
  NONE: 'none',
};

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

const CRDTTimestampedEntrySchema = z.tuple([
  z.string(),
  z.object({
    element: z.any(),
    timestamp: z.number().int().nonnegative(),
    nodeId: z.string(),
  }),
]);

const CRDTStateSchema = z.object({
  kind: z.literal('lww-element-set'),
  nodeId: z.string(),
  biasAdd: z.boolean(),
  clock: z.object({
    nodeId: z.string(),
    clock: z.record(z.number().int().nonnegative()),
  }),
  added: z.array(CRDTTimestampedEntrySchema),
  removed: z.array(CRDTTimestampedEntrySchema),
});

const CRDTRequestSchema = z.object({
  type: z.literal('crdt-state-request'),
  from: z.string(),
  timestamp: z.number(),
});

const CRDTResponseSchema = z.object({
  type: z.literal('crdt-state-response'),
  from: z.string(),
  to: z.string(),
  timestamp: z.number(),
  state: CRDTStateSchema,
});

export class DistributedOrchestrator extends EventEmitter {
  constructor(config) {
    super();
    this.config = DistributedConfigSchema.parse(config);
    this.nodeId = this.config.nodeId;
    this.mode = this.config.mode;
    this.localOrchestrator = new KGCSwarmOrchestrator(this.config.orchestratorConfig);
    this.raftNode = null;
    this.byzantineNode = null;
    this.crdtSet = null;
    this.running = false;
    this.networkHandler = null;
    this.crdtSyncTimer = null;
    this.membership = new MembershipManager({
      nodeId: this.config.nodeId,
      host: this.config.host,
      port: this.config.port,
      ...this.config.membershipConfig,
    });
    this._initializeConsensus();
    this._setupMembershipHandlers();
  }

  _initializeConsensus() {
    switch (this.mode) {
      case ConsensusMode.RAFT:
        this.raftNode = new RaftNode({
          nodeId: this.nodeId,
          peers: this.config.peers,
          ...this.config.raftConfig,
        });
        this.raftNode.on('stateChange', (state, term) => {
          this.emit('consensusStateChange', { mode: ConsensusMode.RAFT, state, term });
        });
        this.raftNode.on('committed', entry => {
          this.emit('consensusCommit', entry);
          void this._applyConsensusEntry(entry);
        });
        break;

      case ConsensusMode.BYZANTINE:
        this.byzantineNode = new ByzantineNode({
          nodeId: this.nodeId,
          peers: this.config.peers,
          ...this.config.byzantineConfig,
        });
        this.byzantineNode.on('committed', (sequence, request) => {
          this.emit('consensusCommit', { sequence, request });
          void this._applyConsensusEntry(request);
        });
        this.byzantineNode.on('viewChange', view => {
          this.emit('consensusStateChange', { mode: ConsensusMode.BYZANTINE, view });
        });
        break;

      case ConsensusMode.CRDT:
        this.crdtSet = new LWWElementSet(this.nodeId);
        break;

      case ConsensusMode.NONE:
        break;

      default:
        throw new Error(`Unknown consensus mode: ${this.mode}`);
    }
  }

  _setupMembershipHandlers() {
    this.membership.on('memberJoined', member => {
      this.emit('nodeJoined', member);
      this._handleMembershipChange();
    });
    this.membership.on('memberFailed', member => {
      this.emit('nodeFailed', member);
      this._handleMembershipChange();
    });
    this.membership.on('memberLeft', member => {
      this.emit('nodeLeft', member);
      this._handleMembershipChange();
    });
  }

  _handleMembershipChange() {
    const clusterSize = this.membership.getClusterSize();
    if (this.mode === ConsensusMode.RAFT && this.raftNode) {
      const required = Math.floor(this.config.peers.length / 2) + 1;
      this.emit('quorumChange', { hasQuorum: clusterSize >= required, clusterSize, required });
    }
    if (this.mode === ConsensusMode.BYZANTINE && this.byzantineNode) {
      const required = 3 * this.byzantineNode.f + 1;
      this.emit('quorumChange', { hasSufficient: clusterSize >= required, clusterSize, required });
    }
  }

  async start() {
    if (this.running) return;
    this.running = true;
    try {
      await this.membership.start();
      switch (this.mode) {
        case ConsensusMode.RAFT:
          await this.raftNode.start();
          break;
        case ConsensusMode.BYZANTINE:
          await this.byzantineNode.start();
          break;
        case ConsensusMode.CRDT:
          this.crdtSyncTimer = setInterval(() => {
            if (this.running) void this._syncCRDT();
          }, 5_000);
          this.crdtSyncTimer.unref?.();
          break;
        case ConsensusMode.NONE:
          break;
      }
      this.emit('started', this.nodeId);
    } catch (error) {
      this.running = false;
      throw error;
    }
  }

  async stop() {
    if (!this.running) return;
    this.running = false;
    switch (this.mode) {
      case ConsensusMode.RAFT:
        await this.raftNode.stop();
        break;
      case ConsensusMode.BYZANTINE:
        await this.byzantineNode.stop();
        break;
      case ConsensusMode.CRDT:
        if (this.crdtSyncTimer) {
          clearInterval(this.crdtSyncTimer);
          this.crdtSyncTimer = null;
        }
        break;
      case ConsensusMode.NONE:
        break;
    }
    await this.membership.stop();
    this.emit('stopped', this.nodeId);
  }

  async run(seed, control, options = {}) {
    if (!this.running) throw new Error('Orchestrator not running. Call start() first.');
    const { consensusTimeoutMs = 30_000, ...executionOptions } = options;
    const proposal = {
      type: 'run',
      seed,
      control,
      options: executionOptions,
      timestamp: Date.now(),
    };

    switch (this.mode) {
      case ConsensusMode.RAFT:
        if (this.raftNode.state !== 'leader') {
          throw new Error(`Not leader. Current leader: ${this.raftNode.leaderId || 'unknown'}`);
        }
        return this._proposeAndWait(() => this.raftNode.propose(proposal), consensusTimeoutMs);

      case ConsensusMode.BYZANTINE:
        if (!this.byzantineNode.isPrimary()) {
          throw new Error(`Not primary. Current primary: ${this.byzantineNode.getPrimary()}`);
        }
        return this._proposeAndWait(
          () => this.byzantineNode.request(proposal.type, proposal),
          consensusTimeoutMs
        );

      case ConsensusMode.CRDT:
        this.crdtSet.add(proposal);
        return this._applyConsensusEntry(proposal);

      case ConsensusMode.NONE:
        return this.localOrchestrator.run(seed, control, executionOptions);

      default:
        throw new Error(`Unknown consensus mode: ${this.mode}`);
    }
  }

  _proposeAndWait(propose, timeoutMs) {
    return new Promise((resolve, reject) => {
      const onComplete = result => {
        clearTimeout(timer);
        resolve(result);
      };
      const timer = setTimeout(() => {
        this.removeListener('executionComplete', onComplete);
        reject(new Error('Consensus timeout'));
      }, timeoutMs);
      this.once('executionComplete', onComplete);
      Promise.resolve().then(propose).catch(error => {
        clearTimeout(timer);
        this.removeListener('executionComplete', onComplete);
        reject(error);
      });
    });
  }

  async _applyConsensusEntry(entry) {
    if (entry.type !== 'run') return undefined;
    const result = await this.localOrchestrator.run(entry.seed, entry.control, entry.options);
    this.emit('executionComplete', result);
    return result;
  }

  async _syncCRDT() {
    if (!this.crdtSet) return { attempted: [], merged: [], errors: [] };
    const peerIds = new Set(this.config.peers);
    for (const member of this.membership.getMembers()) {
      if (member.status === 'alive') peerIds.add(member.nodeId);
    }
    peerIds.delete(this.nodeId);

    const receipt = { attempted: [...peerIds].sort(), merged: [], errors: [] };
    for (const peerId of receipt.attempted) {
      try {
        const peerState = await this._requestPeerCRDT(peerId);
        this.crdtSet.merge(peerState);
        receipt.merged.push(peerId);
        this.emit('crdtMerged', peerId);
      } catch (error) {
        receipt.errors.push({ peerId, message: error.message });
        this.emit('syncError', peerId, error);
      }
    }
    return receipt;
  }

  async syncCRDT() {
    if (this.mode !== ConsensusMode.CRDT || !this.crdtSet) {
      throw new Error('CRDT synchronization is only available in CRDT mode');
    }
    return this._syncCRDT();
  }

  async _requestPeerCRDT(peerId) {
    if (!this.networkHandler) throw new Error('Network handler not configured');
    const response = await this.networkHandler(peerId, {
      type: 'crdt-state-request',
      from: this.nodeId,
      timestamp: Date.now(),
    });
    const admitted = CRDTResponseSchema.parse(response);
    if (admitted.from !== peerId || admitted.to !== this.nodeId) {
      throw new Error(`CRDT response identity mismatch for ${peerId}`);
    }
    return this._deserializeCRDT(admitted.state);
  }

  async handleNetworkMessage(message) {
    if (message?.type === 'crdt-state-request') {
      if (this.mode !== ConsensusMode.CRDT || !this.crdtSet) {
        throw new Error('CRDT state requested from a non-CRDT node');
      }
      const admitted = CRDTRequestSchema.parse(message);
      return {
        type: 'crdt-state-response',
        from: this.nodeId,
        to: admitted.from,
        timestamp: Date.now(),
        state: this._serializeCRDT(),
      };
    }
    this.membership.handleMessage(message);
    return null;
  }

  _serializeCRDT() {
    return {
      kind: 'lww-element-set',
      nodeId: this.crdtSet.nodeId,
      biasAdd: this.crdtSet.biasAdd,
      clock: this.crdtSet.clock.toJSON(),
      added: [...this.crdtSet.added.entries()],
      removed: [...this.crdtSet.removed.entries()],
    };
  }

  _deserializeCRDT(state) {
    const admitted = CRDTStateSchema.parse(state);
    const peer = new LWWElementSet(admitted.nodeId, { biasAdd: admitted.biasAdd });
    peer.added = new Map(admitted.added);
    peer.removed = new Map(admitted.removed);
    peer.clock.nodeId = admitted.clock.nodeId;
    peer.clock.clock = new Map(Object.entries(admitted.clock.clock));
    return peer;
  }

  setNetworkHandler(handler) {
    if (typeof handler !== 'function') throw new TypeError('network handler must be a function');
    this.networkHandler = handler;
    this.membership.setNetworkHandler(handler);
    if (this.raftNode) this.raftNode.setRPCHandler(handler);
    if (this.byzantineNode) this.byzantineNode.setNetworkHandler(handler);
  }

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
        return { ...baseState, consensus: this.raftNode.getState() };
      case ConsensusMode.BYZANTINE:
        return { ...baseState, consensus: this.byzantineNode.getState() };
      case ConsensusMode.CRDT:
        return { ...baseState, consensus: { elements: this.crdtSet.size() } };
      case ConsensusMode.NONE:
        return { ...baseState, consensus: null };
      default:
        return baseState;
    }
  }

  getLocalOrchestrator() {
    return this.localOrchestrator;
  }
}

export function createDistributedOrchestrator(config) {
  return new DistributedOrchestrator(config);
}

export { DistributedConfigSchema, CRDTStateSchema };
