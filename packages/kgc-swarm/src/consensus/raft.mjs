/**
 * @file raft.mjs
 * @description Raft consensus protocol implementation
 *
 * Raft Consensus Algorithm:
 * - Leader election with randomized timeouts
 * - Log replication via AppendEntries RPC
 * - Safety: Leader Completeness, State Machine Safety
 * - Strong consistency (linearizable)
 *
 * Node States: Follower → Candidate → Leader
 * Term-based consensus with majority quorum
 *
 * @module @unrdf/kgc-swarm/consensus/raft
 */

import { z } from 'zod';
import { EventEmitter } from 'node:events';

/**
 * Node state enumeration
 * @enum {string}
 */
export const NodeState = {
  FOLLOWER: 'follower',
  CANDIDATE: 'candidate',
  LEADER: 'leader',
};

/**
 * Log entry schema
 * @typedef {Object} LogEntry
 * @property {number} term - Term when entry was created
 * @property {number} index - Position in log (1-indexed)
 * @property {any} command - State machine command
 * @property {number} timestamp - Creation timestamp
 */
const LogEntrySchema = z.object({
  term: z.number().int().nonnegative(),
  index: z.number().int().positive(),
  command: z.any(),
  timestamp: z.number(),
});

/**
 * Raft configuration schema
 * @typedef {Object} RaftConfig
 * @property {string} nodeId - This node's identifier
 * @property {string[]} peers - Peer node identifiers
 * @property {number} [electionTimeoutMin=150] - Min election timeout (ms)
 * @property {number} [electionTimeoutMax=300] - Max election timeout (ms)
 * @property {number} [heartbeatInterval=50] - Leader heartbeat interval (ms)
 */
const RaftConfigSchema = z.object({
  nodeId: z.string(),
  peers: z.array(z.string()),
  electionTimeoutMin: z.number().positive().default(150),
  electionTimeoutMax: z.number().positive().default(300),
  heartbeatInterval: z.number().positive().default(50),
});

/**
 * RequestVote RPC
 * @typedef {Object} RequestVoteArgs
 * @property {number} term - Candidate's term
 * @property {string} candidateId - Candidate requesting vote
 * @property {number} lastLogIndex - Index of candidate's last log entry
 * @property {number} lastLogTerm - Term of candidate's last log entry
 */
const RequestVoteArgsSchema = z.object({
  term: z.number().int().nonnegative(),
  candidateId: z.string(),
  lastLogIndex: z.number().int().nonnegative(),
  lastLogTerm: z.number().int().nonnegative(),
});

/**
 * RequestVote response
 * @typedef {Object} RequestVoteReply
 * @property {number} term - Current term for candidate to update itself
 * @property {boolean} voteGranted - True means candidate received vote
 */
const RequestVoteReplySchema = z.object({
  term: z.number().int().nonnegative(),
  voteGranted: z.boolean(),
});

/**
 * AppendEntries RPC
 * @typedef {Object} AppendEntriesArgs
 * @property {number} term - Leader's term
 * @property {string} leaderId - Leader identifier
 * @property {number} prevLogIndex - Index of log entry immediately preceding new ones
 * @property {number} prevLogTerm - Term of prevLogIndex entry
 * @property {LogEntry[]} entries - Log entries to store (empty for heartbeat)
 * @property {number} leaderCommit - Leader's commitIndex
 */
const AppendEntriesArgsSchema = z.object({
  term: z.number().int().nonnegative(),
  leaderId: z.string(),
  prevLogIndex: z.number().int().nonnegative(),
  prevLogTerm: z.number().int().nonnegative(),
  entries: z.array(LogEntrySchema),
  leaderCommit: z.number().int().nonnegative(),
});

/**
 * AppendEntries response
 * @typedef {Object} AppendEntriesReply
 * @property {number} term - Current term for leader to update itself
 * @property {boolean} success - True if follower contained entry matching prevLogIndex/prevLogTerm
 * @property {number} [conflictIndex] - Index to help leader backtrack
 * @property {number} [conflictTerm] - Term of conflicting entry
 */
const AppendEntriesReplySchema = z.object({
  term: z.number().int().nonnegative(),
  success: z.boolean(),
  conflictIndex: z.number().int().nonnegative().optional(),
  conflictTerm: z.number().int().nonnegative().optional(),
});

/**
 * RaftNode: Implements Raft consensus algorithm
 *
 * Properties:
 * - Safety: Only one leader per term
 * - Leader Completeness: Log entries committed in previous terms appear in future leaders
 * - State Machine Safety: State machines apply same commands in same order
 *
 * @class RaftNode
 * @extends EventEmitter
 * @example
 * ```javascript
 * const node = new RaftNode({
 *   nodeId: 'node-1',
 *   peers: ['node-2', 'node-3', 'node-4', 'node-5']
 * });
 *
 * node.on('stateChange', (state) => {
 *   console.log(`Node ${node.nodeId} is now ${state}`);
 * });
 *
 * node.on('committed', (entry) => {
 *   console.log(`Committed entry ${entry.index}: ${entry.command}`);
 * });
 *
 * await node.start();
 * await node.propose({ type: 'set', key: 'x', value: 42 });
 * ```
 */
export class RaftNode extends EventEmitter {
  /**
   * @param {RaftConfig} config - Raft configuration
   */
  constructor(config) {
    super();
    this.config = RaftConfigSchema.parse(config);

    /** @type {string} */
    this.nodeId = this.config.nodeId;

    /** @type {string[]} */
    this.peers = this.config.peers;

    // Persistent state (should be persisted to disk in production)
    /** @type {number} - Latest term server has seen */
    this.currentTerm = 0;

    /** @type {string | null} - CandidateId that received vote in current term */
    this.votedFor = null;

    /** @type {LogEntry[]} - Log entries (1-indexed, index 0 is sentinel) */
    this.log = [{ term: 0, index: 0, command: null, timestamp: Date.now() }];

    // Volatile state on all servers
    /** @type {number} - Index of highest log entry known to be committed */
    this.commitIndex = 0;

    /** @type {number} - Index of highest log entry applied to state machine */
    this.lastApplied = 0;

    // Volatile state on leaders (reinitialized after election)
    /** @type {Map<string, number>} - For each peer, index of next log entry to send */
    this.nextIndex = new Map();

    /** @type {Map<string, number>} - For each peer, index of highest log entry known to be replicated */
    this.matchIndex = new Map();

    // Node state
    /** @type {string} - Current state: follower, candidate, leader */
    this.state = NodeState.FOLLOWER;

    /** @type {string | null} - Current leader ID */
    this.leaderId = null;

    // Timers
    /** @type {NodeJS.Timeout | null} */
    this.electionTimer = null;

    /** @type {NodeJS.Timeout | null} */
    this.heartbeatTimer = null;

    /** @type {boolean} */
    this.running = false;

    // RPC handler (set by transport layer)
    /** @type {Function | null} */
    this.rpcHandler = null;
  }

  /**
   * Start the Raft node
   * @returns {Promise<void>}
   */
  async start() {
    if (this.running) return;

    this.running = true;
    this.becomeFollower(this.currentTerm);
    this.emit('started', this.nodeId);
  }

  /**
   * Stop the Raft node
   * @returns {Promise<void>}
   */
  async stop() {
    this.running = false;
    this._clearElectionTimer();
    this._clearHeartbeatTimer();
    this.emit('stopped', this.nodeId);
  }

  /**
   * Transition to follower state
   * @param {number} term - New term
   */
  becomeFollower(term) {
    this.currentTerm = term;
    this.state = NodeState.FOLLOWER;
    this.votedFor = null;
    this.leaderId = null;

    this._clearHeartbeatTimer();
    this._resetElectionTimer();

    this.emit('stateChange', NodeState.FOLLOWER, term);
  }

  /**
   * Transition to candidate state and start election
   */
  becomeCandidate() {
    this.currentTerm += 1;
    this.state = NodeState.CANDIDATE;
    this.votedFor = this.nodeId;
    this.leaderId = null;

    this._resetElectionTimer();
    this.emit('stateChange', NodeState.CANDIDATE, this.currentTerm);

    // Start election
    this._startElection();
  }

  /**
   * Transition to leader state
   */
  becomeLeader() {
    this.state = NodeState.LEADER;
    this.leaderId = this.nodeId;

    // Initialize leader volatile state
    const lastLogIndex = this.log.length - 1;
    for (const peer of this.peers) {
      this.nextIndex.set(peer, lastLogIndex + 1);
      this.matchIndex.set(peer, 0);
    }

    this._clearElectionTimer();
    this._startHeartbeat();

    this.emit('stateChange', NodeState.LEADER, this.currentTerm);

    // Send initial heartbeat
    this._sendHeartbeats();
  }

  /**
   * Start leader election
   * @private
   */
  async _startElection() {
    const lastLogIndex = this.log.length - 1;
    const lastLogTerm = this.log[lastLogIndex].term;

    const args = {
      term: this.currentTerm,
      candidateId: this.nodeId,
      lastLogIndex,
      lastLogTerm,
    };

    let votesReceived = 1; // Vote for self
    const votesNeeded = Math.floor(this.peers.length / 2) + 1;

    this.emit('electionStarted', this.currentTerm);

    // Request votes from all peers
    const promises = this.peers.map(async (peer) => {
      try {
        const reply = await this._sendRequestVote(peer, args);

        if (reply.term > this.currentTerm) {
          this.becomeFollower(reply.term);
          return;
        }

        if (this.state === NodeState.CANDIDATE && reply.voteGranted) {
          votesReceived += 1;

          if (votesReceived >= votesNeeded) {
            this.becomeLeader();
          }
        }
      } catch (error) {
        // RPC failure - peer unavailable
        this.emit('rpcError', peer, error);
      }
    });

    await Promise.allSettled(promises);
  }

  /**
   * Send heartbeats to all peers
   * @private
   */
  async _sendHeartbeats() {
    if (this.state !== NodeState.LEADER) return;

    const promises = this.peers.map((peer) => this._replicateLog(peer));
    await Promise.allSettled(promises);
  }

  /**
   * Replicate log to a peer
   * @private
   * @param {string} peer - Peer node ID
   */
  async _replicateLog(peer) {
    if (this.state !== NodeState.LEADER) return;

    const nextIndex = this.nextIndex.get(peer) || 1;
    const prevLogIndex = nextIndex - 1;
    const prevLogTerm = this.log[prevLogIndex].term;

    const entries = this.log.slice(nextIndex);

    const args = {
      term: this.currentTerm,
      leaderId: this.nodeId,
      prevLogIndex,
      prevLogTerm,
      entries,
      leaderCommit: this.commitIndex,
    };

    try {
      const reply = await this._sendAppendEntries(peer, args);

      if (reply.term > this.currentTerm) {
        this.becomeFollower(reply.term);
        return;
      }

      if (this.state !== NodeState.LEADER) return;

      if (reply.success) {
        // Update nextIndex and matchIndex for follower
        const newMatchIndex = prevLogIndex + entries.length;
        this.matchIndex.set(peer, newMatchIndex);
        this.nextIndex.set(peer, newMatchIndex + 1);

        // Update commitIndex if majority has replicated
        this._updateCommitIndex();
      } else {
        // Log inconsistency - decrement nextIndex and retry
        const newNextIndex = Math.max(1, (this.nextIndex.get(peer) || 1) - 1);
        this.nextIndex.set(peer, newNextIndex);

        // Retry replication
        await this._replicateLog(peer);
      }
    } catch (error) {
      this.emit('rpcError', peer, error);
    }
  }

  /**
   * Update commit index based on majority replication
   * @private
   */
  _updateCommitIndex() {
    if (this.state !== NodeState.LEADER) return;

    // Find highest N such that:
    // 1. N > commitIndex
    // 2. majority of matchIndex[i] >= N
    // 3. log[N].term == currentTerm

    for (let n = this.log.length - 1; n > this.commitIndex; n--) {
      if (this.log[n].term !== this.currentTerm) continue;

      let replicationCount = 1; // Count self
      for (const matchIndex of this.matchIndex.values()) {
        if (matchIndex >= n) replicationCount += 1;
      }

      const majority = Math.floor(this.peers.length / 2) + 1;
      if (replicationCount >= majority) {
        this.commitIndex = n;
        this._applyCommittedEntries();
        break;
      }
    }
  }

  /**
   * Apply committed log entries to state machine
   * @private
   */
  _applyCommittedEntries() {
    while (this.lastApplied < this.commitIndex) {
      this.lastApplied += 1;
      const entry = this.log[this.lastApplied];
      this.emit('committed', entry);
    }
  }

  /**
   * Propose a new command to the cluster
   * @param {any} command - State machine command
   * @returns {Promise<LogEntry>} Committed log entry
   * @throws {Error} If not leader
   */
  async propose(command) {
    if (this.state !== NodeState.LEADER) {
      throw new Error(`Not leader. Current leader: ${this.leaderId || 'unknown'}`);
    }

    const entry = {
      term: this.currentTerm,
      index: this.log.length,
      command,
      timestamp: Date.now(),
    };

    this.log.push(entry);
    this.emit('proposed', entry);

    // Replicate to followers
    await this._sendHeartbeats();

    return entry;
  }

  /**
   * Handle RequestVote RPC
   * @param {RequestVoteArgs} args - Request arguments
   * @returns {RequestVoteReply} Vote reply
   */
  handleRequestVote(args) {
    RequestVoteArgsSchema.parse(args);

    // Reply false if term < currentTerm
    if (args.term < this.currentTerm) {
      return { term: this.currentTerm, voteGranted: false };
    }

    // If RPC term > currentTerm, update and become follower
    if (args.term > this.currentTerm) {
      this.becomeFollower(args.term);
    }

    // Check if we can vote for this candidate
    const canVote = this.votedFor === null || this.votedFor === args.candidateId;

    // Check if candidate's log is at least as up-to-date as ours
    const lastLogIndex = this.log.length - 1;
    const lastLogTerm = this.log[lastLogIndex].term;

    const logUpToDate =
      args.lastLogTerm > lastLogTerm ||
      (args.lastLogTerm === lastLogTerm && args.lastLogIndex >= lastLogIndex);

    const voteGranted = canVote && logUpToDate;

    if (voteGranted) {
      this.votedFor = args.candidateId;
      this._resetElectionTimer();
    }

    return { term: this.currentTerm, voteGranted };
  }

  /**
   * Handle AppendEntries RPC
   * @param {AppendEntriesArgs} args - Request arguments
   * @returns {AppendEntriesReply} Append reply
   */
  handleAppendEntries(args) {
    AppendEntriesArgsSchema.parse(args);

    // Reply false if term < currentTerm
    if (args.term < this.currentTerm) {
      return { term: this.currentTerm, success: false };
    }

    // If RPC term >= currentTerm, update and become follower
    if (args.term >= this.currentTerm) {
      this.becomeFollower(args.term);
      this.leaderId = args.leaderId;
    }

    this._resetElectionTimer();

    // Reply false if log doesn't contain entry at prevLogIndex with prevLogTerm
    if (this.log.length <= args.prevLogIndex) {
      return {
        term: this.currentTerm,
        success: false,
        conflictIndex: this.log.length,
      };
    }

    if (this.log[args.prevLogIndex].term !== args.prevLogTerm) {
      const conflictTerm = this.log[args.prevLogIndex].term;
      let conflictIndex = args.prevLogIndex;

      // Find first index of conflicting term
      while (conflictIndex > 0 && this.log[conflictIndex - 1].term === conflictTerm) {
        conflictIndex -= 1;
      }

      return {
        term: this.currentTerm,
        success: false,
        conflictIndex,
        conflictTerm,
      };
    }

    // Append new entries
    let index = args.prevLogIndex + 1;
    for (const entry of args.entries) {
      if (index < this.log.length) {
        // Delete conflicting entry and all that follow
        if (this.log[index].term !== entry.term) {
          this.log.splice(index);
          this.log.push(entry);
        }
      } else {
        this.log.push(entry);
      }
      index += 1;
    }

    // Update commit index
    if (args.leaderCommit > this.commitIndex) {
      this.commitIndex = Math.min(args.leaderCommit, this.log.length - 1);
      this._applyCommittedEntries();
    }

    return { term: this.currentTerm, success: true };
  }

  /**
   * Reset election timer
   * @private
   */
  _resetElectionTimer() {
    this._clearElectionTimer();

    const timeout = this._randomElectionTimeout();
    this.electionTimer = setTimeout(() => {
      if (this.running && this.state !== NodeState.LEADER) {
        this.becomeCandidate();
      }
    }, timeout);
  }

  /**
   * Clear election timer
   * @private
   */
  _clearElectionTimer() {
    if (this.electionTimer) {
      clearTimeout(this.electionTimer);
      this.electionTimer = null;
    }
  }

  /**
   * Start heartbeat timer
   * @private
   */
  _startHeartbeat() {
    this._clearHeartbeatTimer();

    this.heartbeatTimer = setInterval(() => {
      if (this.state === NodeState.LEADER) {
        this._sendHeartbeats();
      }
    }, this.config.heartbeatInterval);
  }

  /**
   * Clear heartbeat timer
   * @private
   */
  _clearHeartbeatTimer() {
    if (this.heartbeatTimer) {
      clearInterval(this.heartbeatTimer);
      this.heartbeatTimer = null;
    }
  }

  /**
   * Generate random election timeout
   * @private
   * @returns {number} Timeout in milliseconds
   */
  _randomElectionTimeout() {
    const { electionTimeoutMin, electionTimeoutMax } = this.config;
    return Math.floor(
      Math.random() * (electionTimeoutMax - electionTimeoutMin) + electionTimeoutMin
    );
  }

  /**
   * Send RequestVote RPC (override with transport layer)
   * @private
   * @param {string} peer - Peer node ID
   * @param {RequestVoteArgs} args - Request arguments
   * @returns {Promise<RequestVoteReply>}
   */
  async _sendRequestVote(peer, args) {
    if (this.rpcHandler) {
      return this.rpcHandler(peer, 'RequestVote', args);
    }
    throw new Error('RPC handler not configured');
  }

  /**
   * Send AppendEntries RPC (override with transport layer)
   * @private
   * @param {string} peer - Peer node ID
   * @param {AppendEntriesArgs} args - Request arguments
   * @returns {Promise<AppendEntriesReply>}
   */
  async _sendAppendEntries(peer, args) {
    if (this.rpcHandler) {
      return this.rpcHandler(peer, 'AppendEntries', args);
    }
    throw new Error('RPC handler not configured');
  }

  /**
   * Set RPC handler for network transport
   * @param {Function} handler - RPC handler function
   */
  setRPCHandler(handler) {
    this.rpcHandler = handler;
  }

  /**
   * Get current node state
   * @returns {Object} Node state
   */
  getState() {
    return {
      nodeId: this.nodeId,
      state: this.state,
      currentTerm: this.currentTerm,
      votedFor: this.votedFor,
      leaderId: this.leaderId,
      logLength: this.log.length,
      commitIndex: this.commitIndex,
      lastApplied: this.lastApplied,
    };
  }
}

/**
 * Create a new Raft node
 * @param {RaftConfig} config - Configuration
 * @returns {RaftNode} Raft node instance
 */
export function createRaftNode(config) {
  return new RaftNode(config);
}

// Export schemas
export {
  RaftConfigSchema,
  LogEntrySchema,
  RequestVoteArgsSchema,
  RequestVoteReplySchema,
  AppendEntriesArgsSchema,
  AppendEntriesReplySchema,
};
