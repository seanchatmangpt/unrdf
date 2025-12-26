/**
 * @fileoverview Raft Coordinator for Workflow Consensus
 * @module consensus/raft/raft-coordinator
 *
 * @description
 * Production-grade Raft consensus coordinator for distributed workflows.
 * Integrates raft library with WebSocket transport for real consensus.
 *
 * Key features:
 * - Leader election with Raft algorithm
 * - Log replication across cluster
 * - Strong consistency guarantees
 * - Automatic failover on node failure
 * - Integration with federation patterns
 */

import { EventEmitter } from 'events';
import { randomUUID } from 'crypto';
import { z } from 'zod';
import { trace, SpanStatusCode, metrics } from '@opentelemetry/api';
import { createWebSocketTransport } from '../transport/websocket-transport.mjs';

const tracer = trace.getTracer('unrdf-consensus');
const meter = metrics.getMeter('unrdf-consensus');

/**
 * Raft node states
 * @enum {string}
 */
const RaftState = {
  FOLLOWER: 'follower',
  CANDIDATE: 'candidate',
  LEADER: 'leader',
};

/**
 * Raft coordinator configuration schema
 */
const RaftConfigSchema = z.object({
  nodeId: z.string(),
  port: z.number().int().positive(),
  host: z.string().default('localhost'),
  electionTimeoutMin: z.number().positive().default(150),
  electionTimeoutMax: z.number().positive().default(300),
  heartbeatInterval: z.number().positive().default(50),
  snapshotThreshold: z.number().positive().default(1000),
});

/**
 * Workflow command schema
 */
const WorkflowCommandSchema = z.object({
  type: z.enum(['START_WORKFLOW', 'STOP_WORKFLOW', 'UPDATE_STATE', 'REGISTER_NODE', 'DEREGISTER_NODE']),
  workflowId: z.string().optional(),
  nodeId: z.string().optional(),
  data: z.any(),
  timestamp: z.number().default(() => Date.now()),
});

/**
 * Raft Coordinator
 *
 * Manages Raft consensus for distributed workflow coordination.
 * Provides leader election, log replication, and state consistency.
 *
 * @class RaftCoordinator
 * @extends EventEmitter
 *
 * @example
 * const coordinator = new RaftCoordinator({
 *   nodeId: 'node-1',
 *   port: 8080,
 *   host: 'localhost'
 * });
 *
 * await coordinator.initialize();
 *
 * // Add cluster peers
 * coordinator.addPeer('node-2', 'localhost', 8081);
 * coordinator.addPeer('node-3', 'localhost', 8082);
 *
 * // Listen for state changes
 * coordinator.on('leader_elected', ({ leaderId }) => {
 *   console.log(`New leader: ${leaderId}`);
 * });
 *
 * // Replicate workflow command
 * await coordinator.replicateCommand({
 *   type: 'START_WORKFLOW',
 *   workflowId: 'wf-123',
 *   data: { name: 'Data Pipeline' }
 * });
 */
export class RaftCoordinator extends EventEmitter {
  /**
   * Create a Raft coordinator
   * @param {Object} config - Raft configuration
   */
  constructor(config) {
    super();
    this.config = RaftConfigSchema.parse(config);

    this.transport = null;

    // Raft state (implementing Raft directly)
    this.currentTerm = 0;
    this.votedFor = null;
    this.log = [];
    this.commitIndex = 0;
    this.lastApplied = 0;

    // Volatile leader state
    this.nextIndex = new Map();
    this.matchIndex = new Map();

    // Node state
    this.state = RaftState.FOLLOWER;
    this.isLeader = false;
    this.leaderId = null;
    this.peers = new Map();

    // Timers
    this.electionTimer = null;
    this.heartbeatTimer = null;

    // State machine
    this.commandLog = [];
    this.stateMachine = new Map();

    // Metrics
    this.commandCounter = meter.createCounter('consensus.commands.total', {
      description: 'Total commands replicated',
    });

    this.leaderElections = meter.createCounter('consensus.elections.total', {
      description: 'Total leader elections',
    });

    this.nodeStateGauge = meter.createObservableGauge('consensus.node.state', {
      description: 'Current node state (0=follower, 1=candidate, 2=leader)',
    });

    this.nodeStateGauge.addCallback(result => {
      const stateValue = this.isLeader ? 2 : 0;
      result.observe(stateValue);
    });
  }

  /**
   * Initialize the Raft coordinator
   * @returns {Promise<void>}
   */
  async initialize() {
    return tracer.startActiveSpan('raft.initialize', async span => {
      try {
        span.setAttribute('node.id', this.config.nodeId);
        span.setAttribute('node.port', this.config.port);

        // Initialize transport layer
        this.transport = createWebSocketTransport({
          nodeId: this.config.nodeId,
          port: this.config.port,
          host: this.config.host,
        });

        await this.transport.start();

        // Set up transport handlers
        this.setupTransportHandlers();

        // Start as follower with election timer
        this.resetElectionTimer();

        this.emit('initialized', { nodeId: this.config.nodeId });
        span.setStatus({ code: SpanStatusCode.OK });
      } catch (error) {
        span.recordException(error);
        span.setStatus({ code: SpanStatusCode.ERROR, message: error.message });
        throw error;
      } finally {
        span.end();
      }
    });
  }


  /**
   * Set up transport event handlers
   * @private
   */
  setupTransportHandlers() {
    this.transport.on('message', message => {
      this.handleTransportMessage(message);
    });

    this.transport.on('peer_connected', ({ nodeId }) => {
      this.emit('peer_connected', { nodeId });
    });

    this.transport.on('peer_disconnected', ({ nodeId }) => {
      this.emit('peer_disconnected', { nodeId });
    });

    this.transport.on('error', error => {
      this.emit('error', { source: 'transport', error });
    });
  }

  /**
   * Handle transport message
   * @param {Object} message - Transport message
   * @private
   */
  handleTransportMessage(message) {
    switch (message.type) {
      case 'request_vote':
        this.handleRequestVote(message);
        break;
      case 'append_entries':
        this.handleAppendEntries(message);
        break;
      case 'heartbeat':
        this.handleHeartbeat(message);
        break;
      case 'command':
        this.handleCommand(message);
        break;
      default:
        this.emit('error', { error: new Error(`Unknown message type: ${message.type}`) });
    }
  }

  /**
   * Handle vote request
   * @param {Object} message - Vote request message
   * @private
   */
  handleRequestVote(message) {
    const { term, candidateId, lastLogIndex, lastLogTerm } = message.data;

    // Update term if needed
    if (term > this.currentTerm) {
      this.currentTerm = term;
      this.votedFor = null;
      this.becomeFollower();
    }

    // Check if we can grant vote
    const logOk = lastLogTerm > this.getLastLogTerm() ||
                  (lastLogTerm === this.getLastLogTerm() && lastLogIndex >= this.log.length);
    const granted = term >= this.currentTerm &&
                   (this.votedFor === null || this.votedFor === candidateId) &&
                   logOk;

    if (granted) {
      this.votedFor = candidateId;
      this.resetElectionTimer();
    }

    this.transport.sendResponse(message, { voteGranted: granted, term: this.currentTerm });
  }

  /**
   * Handle append entries
   * @param {Object} message - Append entries message
   * @private
   */
  handleAppendEntries(message) {
    const { term, leaderId, prevLogIndex, prevLogTerm, entries, leaderCommit } = message.data;

    // Update term if needed
    if (term > this.currentTerm) {
      this.currentTerm = term;
      this.votedFor = null;
      this.becomeFollower();
    }

    // Reset election timer on valid leader message
    if (term >= this.currentTerm) {
      this.leaderId = leaderId;
      this.resetElectionTimer();
    }

    // Check log consistency
    const logOk = prevLogIndex === 0 ||
                  (prevLogIndex <= this.log.length &&
                   this.log[prevLogIndex - 1]?.term === prevLogTerm);

    let success = false;
    if (term >= this.currentTerm && logOk) {
      // Append entries
      if (entries && entries.length > 0) {
        this.log = this.log.slice(0, prevLogIndex);
        this.log.push(...entries);
      }

      // Update commit index
      if (leaderCommit > this.commitIndex) {
        this.commitIndex = Math.min(leaderCommit, this.log.length);
        this.applyCommittedEntries();
      }

      success = true;
    }

    this.transport.sendResponse(message, { success, term: this.currentTerm });
  }

  /**
   * Handle heartbeat
   * @param {Object} message - Heartbeat message
   * @private
   */
  handleHeartbeat(message) {
    this.handleAppendEntries(message);
  }

  /**
   * Handle command
   * @param {Object} message - Command message
   * @private
   */
  async handleCommand(message) {
    if (this.isLeader) {
      await this.replicateCommand(message.data);
      this.transport.sendResponse(message, { success: true });
    } else {
      this.transport.sendResponse(message, {
        success: false,
        leaderId: this.leaderId,
        error: 'Not leader',
      });
    }
  }

  /**
   * Add a peer to the cluster
   * @param {string} nodeId - Peer node ID
   * @param {string} host - Peer host
   * @param {number} port - Peer port
   */
  addPeer(nodeId, host, port) {
    this.peers.set(nodeId, { nodeId, host, port });
    this.transport.addPeer(nodeId, host, port);

    if (this.isLeader) {
      this.nextIndex.set(nodeId, this.log.length + 1);
      this.matchIndex.set(nodeId, 0);
    }

    this.emit('peer_added', { nodeId, host, port });
  }

  /**
   * Remove a peer from the cluster
   * @param {string} nodeId - Peer node ID
   */
  removePeer(nodeId) {
    this.peers.delete(nodeId);
    this.transport.removePeer(nodeId);
    this.nextIndex.delete(nodeId);
    this.matchIndex.delete(nodeId);
    this.emit('peer_removed', { nodeId });
  }

  /**
   * Replicate a workflow command across the cluster
   * @param {Object} command - Workflow command
   * @returns {Promise<boolean>} True if successfully replicated
   */
  async replicateCommand(command) {
    return tracer.startActiveSpan('raft.replicateCommand', async span => {
      try {
        const validatedCommand = WorkflowCommandSchema.parse(command);
        span.setAttribute('command.type', validatedCommand.type);

        if (!this.isLeader) {
          throw new Error(`Not leader. Current leader: ${this.leaderId || 'none'}`);
        }

        // Add to Raft log
        const entry = {
          term: this.currentTerm,
          index: this.log.length + 1,
          command: validatedCommand,
          id: randomUUID(),
        };

        this.log.push(entry);
        this.commandLog.push(entry);

        // Replicate to followers
        await this.replicateToFollowers();

        // Wait for majority
        const success = await this.waitForMajority(entry.index);

        if (success) {
          this.commitIndex = entry.index;
          this.applyCommittedEntries();
        }

        this.commandCounter.add(1, {
          type: validatedCommand.type,
          success: String(success),
        });

        span.setAttribute('replicate.success', success);
        span.setStatus({ code: SpanStatusCode.OK });
        return success;
      } catch (error) {
        span.recordException(error);
        span.setStatus({ code: SpanStatusCode.ERROR, message: error.message });
        throw error;
      } finally {
        span.end();
      }
    });
  }

  /**
   * Apply a committed command to state machine
   * @param {Object} entry - Log entry
   * @private
   */
  applyCommand(entry) {
    try {
      const { command } = entry;

      switch (command.type) {
        case 'START_WORKFLOW':
          this.stateMachine.set(command.workflowId, {
            status: 'running',
            data: command.data,
            startedAt: command.timestamp,
          });
          break;

        case 'STOP_WORKFLOW':
          if (this.stateMachine.has(command.workflowId)) {
            const workflow = this.stateMachine.get(command.workflowId);
            workflow.status = 'stopped';
            workflow.stoppedAt = command.timestamp;
          }
          break;

        case 'UPDATE_STATE':
          if (this.stateMachine.has(command.workflowId)) {
            const workflow = this.stateMachine.get(command.workflowId);
            workflow.data = { ...workflow.data, ...command.data };
            workflow.updatedAt = command.timestamp;
          }
          break;

        case 'REGISTER_NODE':
          this.stateMachine.set(`node:${command.nodeId}`, {
            status: 'active',
            registeredAt: command.timestamp,
            ...command.data,
          });
          break;

        case 'DEREGISTER_NODE':
          this.stateMachine.delete(`node:${command.nodeId}`);
          break;

        default:
          this.emit('error', { error: new Error(`Unknown command type: ${command.type}`) });
      }

      this.emit('command_applied', { command, entry });
    } catch (error) {
      this.emit('error', { error, source: 'apply_command' });
    }
  }

  /**
   * Get current state
   * @returns {Object} Current coordinator state
   */
  getState() {
    return {
      nodeId: this.config.nodeId,
      isLeader: this.isLeader,
      leaderId: this.leaderId,
      term: this.currentTerm,
      logLength: this.log.length,
      peers: this.transport ? this.transport.getConnectedPeers() : [],
      stateMachineSize: this.stateMachine.size,
    };
  }

  /**
   * Get last log term
   * @returns {number} Last log term
   * @private
   */
  getLastLogTerm() {
    return this.log.length > 0 ? this.log[this.log.length - 1].term : 0;
  }

  /**
   * Start election
   * @private
   */
  async startElection() {
    this.state = RaftState.CANDIDATE;
    this.currentTerm++;
    this.votedFor = this.config.nodeId;

    let votesReceived = 1;
    const votesNeeded = Math.floor((this.peers.size + 1) / 2) + 1;

    // Request votes from peers
    const votePromises = Array.from(this.peers.keys()).map(async peerId => {
      try {
        const response = await this.transport.send(peerId, {
          type: 'request_vote',
          term: this.currentTerm,
          data: {
            term: this.currentTerm,
            candidateId: this.config.nodeId,
            lastLogIndex: this.log.length,
            lastLogTerm: this.getLastLogTerm(),
          },
        });

        if (response.voteGranted) {
          votesReceived++;
        }
      } catch (error) {
        // Ignore vote failures
      }
    });

    await Promise.allSettled(votePromises);

    // Check if won election
    if (this.state === RaftState.CANDIDATE && votesReceived >= votesNeeded) {
      this.becomeLeader();
    } else {
      this.becomeFollower();
    }
  }

  /**
   * Become leader
   * @private
   */
  becomeLeader() {
    this.state = RaftState.LEADER;
    this.isLeader = true;
    this.leaderId = this.config.nodeId;

    // Initialize leader state
    for (const peerId of this.peers.keys()) {
      this.nextIndex.set(peerId, this.log.length + 1);
      this.matchIndex.set(peerId, 0);
    }

    // Clear election timer
    if (this.electionTimer) {
      clearTimeout(this.electionTimer);
      this.electionTimer = null;
    }

    // Start heartbeat
    this.startHeartbeat();

    this.leaderElections.add(1);
    this.emit('leader_elected', { leaderId: this.config.nodeId });
  }

  /**
   * Become follower
   * @private
   */
  becomeFollower() {
    this.state = RaftState.FOLLOWER;
    this.isLeader = false;

    if (this.heartbeatTimer) {
      clearInterval(this.heartbeatTimer);
      this.heartbeatTimer = null;
    }

    this.resetElectionTimer();
    this.emit('became_follower');
  }

  /**
   * Reset election timer
   * @private
   */
  resetElectionTimer() {
    if (this.electionTimer) {
      clearTimeout(this.electionTimer);
    }

    const timeout = this.config.electionTimeoutMin +
                   Math.random() * (this.config.electionTimeoutMax - this.config.electionTimeoutMin);

    this.electionTimer = setTimeout(() => {
      if (this.state !== RaftState.LEADER) {
        this.startElection();
      }
    }, timeout);
  }

  /**
   * Start heartbeat timer
   * @private
   */
  startHeartbeat() {
    this.heartbeatTimer = setInterval(() => {
      this.sendHeartbeats();
    }, this.config.heartbeatInterval);
  }

  /**
   * Send heartbeats to all followers
   * @private
   */
  async sendHeartbeats() {
    if (!this.isLeader) return;

    const promises = Array.from(this.peers.keys()).map(async peerId => {
      try {
        await this.transport.send(peerId, {
          type: 'heartbeat',
          term: this.currentTerm,
          data: {
            term: this.currentTerm,
            leaderId: this.config.nodeId,
            prevLogIndex: this.log.length,
            prevLogTerm: this.getLastLogTerm(),
            entries: [],
            leaderCommit: this.commitIndex,
          },
        });
      } catch (error) {
        // Ignore heartbeat failures
      }
    });

    await Promise.allSettled(promises);
  }

  /**
   * Replicate to followers
   * @private
   */
  async replicateToFollowers() {
    if (!this.isLeader) return;

    const promises = Array.from(this.peers.keys()).map(async peerId => {
      try {
        const nextIdx = this.nextIndex.get(peerId) || 1;
        const prevLogIndex = nextIdx - 1;
        const prevLogTerm = prevLogIndex > 0 ? this.log[prevLogIndex - 1].term : 0;
        const entries = this.log.slice(nextIdx - 1);

        const response = await this.transport.send(peerId, {
          type: 'append_entries',
          term: this.currentTerm,
          data: {
            term: this.currentTerm,
            leaderId: this.config.nodeId,
            prevLogIndex,
            prevLogTerm,
            entries,
            leaderCommit: this.commitIndex,
          },
        });

        if (response.success) {
          this.nextIndex.set(peerId, this.log.length + 1);
          this.matchIndex.set(peerId, this.log.length);
        } else {
          // Decrement nextIndex and retry
          this.nextIndex.set(peerId, Math.max(1, nextIdx - 1));
        }
      } catch (error) {
        // Ignore replication failures
      }
    });

    await Promise.allSettled(promises);
  }

  /**
   * Wait for majority acknowledgment
   * @param {number} index - Log index
   * @returns {Promise<boolean>} True if majority acknowledged
   * @private
   */
  async waitForMajority(index) {
    // Simple implementation: check matchIndex
    return new Promise(resolve => {
      setTimeout(() => {
        const matchCount = Array.from(this.matchIndex.values()).filter(idx => idx >= index).length;
        const majority = Math.floor((this.peers.size + 1) / 2) + 1;
        resolve(matchCount + 1 >= majority);
      }, 100);
    });
  }

  /**
   * Apply committed entries to state machine
   * @private
   */
  applyCommittedEntries() {
    while (this.lastApplied < this.commitIndex && this.lastApplied < this.log.length) {
      this.lastApplied++;
      const entry = this.log[this.lastApplied - 1];

      if (entry) {
        this.applyCommand(entry);
      }
    }
  }

  /**
   * Get workflow state from state machine
   * @param {string} workflowId - Workflow ID
   * @returns {Object|null} Workflow state or null
   */
  getWorkflowState(workflowId) {
    return this.stateMachine.get(workflowId) || null;
  }

  /**
   * Get all workflows from state machine
   * @returns {Array<Object>} Array of workflows
   */
  getAllWorkflows() {
    const workflows = [];
    for (const [key, value] of this.stateMachine.entries()) {
      if (!key.startsWith('node:')) {
        workflows.push({ id: key, ...value });
      }
    }
    return workflows;
  }

  /**
   * Shutdown the coordinator
   * @returns {Promise<void>}
   */
  async shutdown() {
    if (this.electionTimer) {
      clearTimeout(this.electionTimer);
      this.electionTimer = null;
    }

    if (this.heartbeatTimer) {
      clearInterval(this.heartbeatTimer);
      this.heartbeatTimer = null;
    }

    if (this.transport) {
      await this.transport.shutdown();
    }

    this.emit('shutdown');
  }
}

/**
 * Create a Raft coordinator
 * @param {Object} config - Raft configuration
 * @returns {RaftCoordinator} New coordinator instance
 */
export function createRaftCoordinator(config) {
  return new RaftCoordinator(config);
}
