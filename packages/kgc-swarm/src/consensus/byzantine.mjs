/**
 * @file byzantine.mjs
 * @description Byzantine Fault Tolerant consensus implementation
 *
 * Practical Byzantine Fault Tolerance (PBFT):
 * - Tolerates f Byzantine failures in 3f+1 nodes
 * - Three-phase protocol: Pre-Prepare, Prepare, Commit
 * - Quorum-based consensus: 2f+1 messages required
 * - View changes for leader (primary) replacement
 * - Digital signatures for authentication
 *
 * Safety: Even with f Byzantine nodes, honest nodes agree
 * Liveness: System makes progress if f < n/3
 *
 * @module @unrdf/kgc-swarm/consensus/byzantine
 */

import { z } from 'zod';
import { EventEmitter } from 'node:events';
import { createSign, createVerify, generateKeyPairSync } from 'node:crypto';

/**
 * Node phase in consensus
 * @enum {string}
 */
export const ConsensusPhase = {
  IDLE: 'idle',
  PRE_PREPARE: 'pre-prepare',
  PREPARE: 'prepare',
  COMMIT: 'commit',
  COMMITTED: 'committed',
};

/**
 * Message type enumeration
 * @enum {string}
 */
export const MessageType = {
  REQUEST: 'request',
  PRE_PREPARE: 'pre-prepare',
  PREPARE: 'prepare',
  COMMIT: 'commit',
  VIEW_CHANGE: 'view-change',
  NEW_VIEW: 'new-view',
};

/**
 * Byzantine node configuration
 * @typedef {Object} ByzantineConfig
 * @property {string} nodeId - This node's identifier
 * @property {string[]} peers - All peer node identifiers (including self)
 * @property {number} f - Maximum Byzantine failures tolerated
 * @property {number} [viewChangeTimeout=5000] - View change timeout (ms)
 */
const ByzantineConfigSchema = z.object({
  nodeId: z.string(),
  peers: z.array(z.string()),
  f: z.number().int().nonnegative(),
  viewChangeTimeout: z.number().positive().default(5000),
});

/**
 * Request message schema
 * @typedef {Object} Request
 * @property {string} type - Message type
 * @property {string} clientId - Client identifier
 * @property {number} timestamp - Request timestamp
 * @property {any} operation - Operation to execute
 */
const RequestSchema = z.object({
  type: z.literal(MessageType.REQUEST),
  clientId: z.string(),
  timestamp: z.number(),
  operation: z.any(),
});

/**
 * Pre-Prepare message schema
 * @typedef {Object} PrePrepare
 * @property {string} type - Message type
 * @property {number} view - View number
 * @property {number} sequence - Sequence number
 * @property {string} digest - Request digest
 * @property {Request} request - Original request
 */
const PrePrepareSchema = z.object({
  type: z.literal(MessageType.PRE_PREPARE),
  view: z.number().int().nonnegative(),
  sequence: z.number().int().positive(),
  digest: z.string(),
  request: RequestSchema,
});

/**
 * Prepare message schema
 * @typedef {Object} Prepare
 * @property {string} type - Message type
 * @property {number} view - View number
 * @property {number} sequence - Sequence number
 * @property {string} digest - Request digest
 * @property {string} nodeId - Node identifier
 */
const PrepareSchema = z.object({
  type: z.literal(MessageType.PREPARE),
  view: z.number().int().nonnegative(),
  sequence: z.number().int().positive(),
  digest: z.string(),
  nodeId: z.string(),
});

/**
 * Commit message schema
 * @typedef {Object} Commit
 * @property {string} type - Message type
 * @property {number} view - View number
 * @property {number} sequence - Sequence number
 * @property {string} digest - Request digest
 * @property {string} nodeId - Node identifier
 */
const CommitSchema = z.object({
  type: z.literal(MessageType.COMMIT),
  view: z.number().int().nonnegative(),
  sequence: z.number().int().positive(),
  digest: z.string(),
  nodeId: z.string(),
});

/**
 * Signed message wrapper
 * @typedef {Object} SignedMessage
 * @property {Object} message - The message content
 * @property {string} signature - Digital signature
 * @property {string} senderId - Sender node ID
 */
const SignedMessageSchema = z.object({
  message: z.any(),
  signature: z.string(),
  senderId: z.string(),
});

/**
 * ByzantineNode: Implements PBFT consensus
 *
 * Properties:
 * - Agreement: All honest nodes agree on same sequence of requests
 * - Validity: If honest node commits, others will eventually commit
 * - Integrity: Committed requests are authentic
 *
 * Requires 3f+1 nodes to tolerate f Byzantine failures
 *
 * @class ByzantineNode
 * @extends EventEmitter
 * @example
 * ```javascript
 * const node = new ByzantineNode({
 *   nodeId: 'node-1',
 *   peers: ['node-1', 'node-2', 'node-3', 'node-4'], // 3f+1 = 4, f=1
 *   f: 1
 * });
 *
 * node.on('committed', (sequence, request) => {
 *   console.log(`Committed request ${sequence}:`, request);
 * });
 *
 * await node.start();
 * await node.request({ type: 'transfer', from: 'A', to: 'B', amount: 100 });
 * ```
 */
export class ByzantineNode extends EventEmitter {
  /**
   * @param {ByzantineConfig} config - Byzantine configuration
   */
  constructor(config) {
    super();
    this.config = ByzantineConfigSchema.parse(config);

    // Validate: n = 3f + 1
    const n = this.config.peers.length;
    const f = this.config.f;
    if (n < 3 * f + 1) {
      throw new Error(`Need at least ${3 * f + 1} nodes to tolerate ${f} failures, got ${n}`);
    }

    /** @type {string} */
    this.nodeId = this.config.nodeId;

    /** @type {string[]} */
    this.peers = this.config.peers;

    /** @type {number} - Number of Byzantine failures tolerated */
    this.f = this.config.f;

    /** @type {number} - Current view number */
    this.view = 0;

    /** @type {number} - Next sequence number */
    this.sequence = 0;

    /** @type {Map<number, Request>} - Pending requests by sequence */
    this.pendingRequests = new Map();

    /** @type {Map<number, Set<string>>} - Prepare messages received: sequence -> nodeIds */
    this.prepareMessages = new Map();

    /** @type {Map<number, Set<string>>} - Commit messages received: sequence -> nodeIds */
    this.commitMessages = new Map();

    /** @type {Map<number, string>} - Committed requests: sequence -> digest */
    this.committedRequests = new Map();

    /** @type {Map<number, ConsensusPhase>} - Current phase for each sequence */
    this.phaseState = new Map();

    /** @type {boolean} */
    this.running = false;

    // Cryptographic keys (Ed25519)
    const { publicKey, privateKey } = generateKeyPairSync('ed25519');

    /** @type {KeyObject} */
    this.publicKey = publicKey;

    /** @type {KeyObject} */
    this.privateKey = privateKey;

    /** @type {Map<string, KeyObject>} - Peer public keys */
    this.peerKeys = new Map();

    // View change state
    /** @type {NodeJS.Timeout | null} */
    this.viewChangeTimer = null;

    /** @type {Set<string>} - View change messages received */
    this.viewChangeMessages = new Set();

    // Network handler
    /** @type {Function | null} */
    this.networkHandler = null;
  }

  /**
   * Start the Byzantine node
   * @returns {Promise<void>}
   */
  async start() {
    if (this.running) return;

    this.running = true;
    this._resetViewChangeTimer();
    this.emit('started', this.nodeId);
  }

  /**
   * Stop the Byzantine node
   * @returns {Promise<void>}
   */
  async stop() {
    this.running = false;
    this._clearViewChangeTimer();
    this.emit('stopped', this.nodeId);
  }

  /**
   * Check if this node is the primary for current view
   * @returns {boolean}
   */
  isPrimary() {
    return this.peers[this.view % this.peers.length] === this.nodeId;
  }

  /**
   * Get current primary node ID
   * @returns {string}
   */
  getPrimary() {
    return this.peers[this.view % this.peers.length];
  }

  /**
   * Submit a client request
   * @param {any} operation - Operation to execute
   * @returns {Promise<number>} Sequence number
   */
  async request(operation) {
    const request = {
      type: MessageType.REQUEST,
      clientId: this.nodeId,
      timestamp: Date.now(),
      operation,
    };

    RequestSchema.parse(request);

    if (this.isPrimary()) {
      // We are primary - start consensus
      return this._handleRequest(request);
    } else {
      // Forward to primary
      const primary = this.getPrimary();
      await this._sendMessage(primary, request);

      // Return estimated sequence (may not be final)
      return this.sequence + 1;
    }
  }

  /**
   * Handle incoming request (primary only)
   * @private
   * @param {Request} request - Client request
   * @returns {number} Sequence number
   */
  _handleRequest(request) {
    if (!this.isPrimary()) {
      throw new Error('Only primary can handle requests');
    }

    this.sequence += 1;
    const sequence = this.sequence;

    const digest = this._computeDigest(request);

    const prePrepare = {
      type: MessageType.PRE_PREPARE,
      view: this.view,
      sequence,
      digest,
      request,
    };

    PrePrepareSchema.parse(prePrepare);

    // Store pending request
    this.pendingRequests.set(sequence, request);
    this.phaseState.set(sequence, ConsensusPhase.PRE_PREPARE);

    // Broadcast pre-prepare to all replicas
    this._broadcast(prePrepare);

    // Primary also moves to prepare phase
    this._handlePrePrepare(prePrepare);

    this.emit('prePrepare', sequence, digest);

    return sequence;
  }

  /**
   * Handle Pre-Prepare message (replicas)
   * @private
   * @param {PrePrepare} prePrepare - Pre-prepare message
   */
  _handlePrePrepare(prePrepare) {
    PrePrepareSchema.parse(prePrepare);

    const { view, sequence, digest, request } = prePrepare;

    // Verify: 1) signatures valid, 2) digest matches, 3) sequence in bounds
    if (view !== this.view) {
      this.emit('error', 'View mismatch', { expected: this.view, received: view });
      return;
    }

    const computedDigest = this._computeDigest(request);
    if (computedDigest !== digest) {
      this.emit('error', 'Digest mismatch', { expected: digest, computed: computedDigest });
      return;
    }

    // Accept pre-prepare and move to prepare phase
    this.pendingRequests.set(sequence, request);
    this.phaseState.set(sequence, ConsensusPhase.PREPARE);

    // Send prepare message to all replicas
    const prepare = {
      type: MessageType.PREPARE,
      view,
      sequence,
      digest,
      nodeId: this.nodeId,
    };

    PrepareSchema.parse(prepare);
    this._broadcast(prepare);

    // Add own prepare message
    this._handlePrepare(prepare);

    this.emit('prepare', sequence, digest);
  }

  /**
   * Handle Prepare message
   * @private
   * @param {Prepare} prepare - Prepare message
   */
  _handlePrepare(prepare) {
    PrepareSchema.parse(prepare);

    const { view, sequence, digest, nodeId } = prepare;

    if (view !== this.view) return;

    // Record prepare message
    if (!this.prepareMessages.has(sequence)) {
      this.prepareMessages.set(sequence, new Set());
    }
    this.prepareMessages.get(sequence).add(nodeId);

    // Check if we have 2f prepare messages (including own)
    const prepareCount = this.prepareMessages.get(sequence).size;
    const quorum = 2 * this.f; // Need 2f+1 total (including pre-prepare)

    if (prepareCount >= quorum && this.phaseState.get(sequence) === ConsensusPhase.PREPARE) {
      // Move to commit phase
      this.phaseState.set(sequence, ConsensusPhase.COMMIT);

      const commit = {
        type: MessageType.COMMIT,
        view,
        sequence,
        digest,
        nodeId: this.nodeId,
      };

      CommitSchema.parse(commit);
      this._broadcast(commit);

      // Add own commit message
      this._handleCommit(commit);

      this.emit('commit', sequence, digest);
    }
  }

  /**
   * Handle Commit message
   * @private
   * @param {Commit} commit - Commit message
   */
  _handleCommit(commit) {
    CommitSchema.parse(commit);

    const { view, sequence, digest, nodeId } = commit;

    if (view !== this.view) return;

    // Record commit message
    if (!this.commitMessages.has(sequence)) {
      this.commitMessages.set(sequence, new Set());
    }
    this.commitMessages.get(sequence).add(nodeId);

    // Check if we have 2f+1 commit messages
    const commitCount = this.commitMessages.get(sequence).size;
    const quorum = 2 * this.f + 1;

    if (commitCount >= quorum && this.phaseState.get(sequence) === ConsensusPhase.COMMIT) {
      // Committed!
      this.phaseState.set(sequence, ConsensusPhase.COMMITTED);
      this.committedRequests.set(sequence, digest);

      const request = this.pendingRequests.get(sequence);
      this.emit('committed', sequence, request);

      // Reset view change timer
      this._resetViewChangeTimer();
    }
  }

  /**
   * Compute digest of request
   * @private
   * @param {Request} request - Request to digest
   * @returns {string} Hex digest
   */
  _computeDigest(request) {
    const json = JSON.stringify(request, Object.keys(request).sort());
    const hash = createSign('sha256');
    hash.update(json);
    return hash.sign(this.privateKey, 'hex');
  }

  /**
   * Sign a message
   * @private
   * @param {Object} message - Message to sign
   * @returns {SignedMessage} Signed message
   */
  _signMessage(message) {
    const json = JSON.stringify(message, Object.keys(message).sort());
    const sign = createSign('sha256');
    sign.update(json);
    const signature = sign.sign(this.privateKey, 'hex');

    return {
      message,
      signature,
      senderId: this.nodeId,
    };
  }

  /**
   * Verify a signed message
   * @private
   * @param {SignedMessage} signedMessage - Signed message
   * @returns {boolean} True if valid
   */
  _verifyMessage(signedMessage) {
    SignedMessageSchema.parse(signedMessage);

    const { message, signature, senderId } = signedMessage;

    const peerKey = this.peerKeys.get(senderId);
    if (!peerKey) {
      this.emit('error', 'Unknown sender', senderId);
      return false;
    }

    const json = JSON.stringify(message, Object.keys(message).sort());
    const verify = createVerify('sha256');
    verify.update(json);

    return verify.verify(peerKey, signature, 'hex');
  }

  /**
   * Broadcast message to all peers
   * @private
   * @param {Object} message - Message to broadcast
   */
  _broadcast(message) {
    const signedMessage = this._signMessage(message);

    for (const peer of this.peers) {
      if (peer !== this.nodeId) {
        this._sendMessage(peer, signedMessage);
      }
    }
  }

  /**
   * Send message to specific peer
   * @private
   * @param {string} peer - Peer node ID
   * @param {Object} message - Message to send
   */
  async _sendMessage(peer, message) {
    if (this.networkHandler) {
      try {
        await this.networkHandler(peer, message);
      } catch (error) {
        this.emit('networkError', peer, error);
      }
    } else {
      throw new Error('Network handler not configured');
    }
  }

  /**
   * Handle incoming message
   * @param {SignedMessage} signedMessage - Signed message from peer
   */
  handleMessage(signedMessage) {
    // Verify signature
    if (!this._verifyMessage(signedMessage)) {
      this.emit('error', 'Invalid signature', signedMessage.senderId);
      return;
    }

    const { message } = signedMessage;

    switch (message.type) {
      case MessageType.REQUEST:
        if (this.isPrimary()) {
          this._handleRequest(message);
        }
        break;

      case MessageType.PRE_PREPARE:
        if (!this.isPrimary()) {
          this._handlePrePrepare(message);
        }
        break;

      case MessageType.PREPARE:
        this._handlePrepare(message);
        break;

      case MessageType.COMMIT:
        this._handleCommit(message);
        break;

      case MessageType.VIEW_CHANGE:
        this._handleViewChange(message);
        break;

      case MessageType.NEW_VIEW:
        this._handleNewView(message);
        break;

      default:
        this.emit('error', 'Unknown message type', message.type);
    }
  }

  /**
   * Initiate view change
   * @private
   */
  _initiateViewChange() {
    this.view += 1;

    const viewChange = {
      type: MessageType.VIEW_CHANGE,
      view: this.view,
      nodeId: this.nodeId,
      preparedRequests: Array.from(this.committedRequests.entries()),
    };

    this._broadcast(viewChange);
    this.emit('viewChange', this.view);
  }

  /**
   * Handle view change message
   * @private
   * @param {Object} message - View change message
   */
  _handleViewChange(message) {
    // Simplified view change - in production, need full checkpointing
    this.viewChangeMessages.add(message.nodeId);

    const quorum = 2 * this.f + 1;
    if (this.viewChangeMessages.size >= quorum) {
      // View change successful
      this.viewChangeMessages.clear();
      this._resetViewChangeTimer();
      this.emit('viewChanged', this.view);
    }
  }

  /**
   * Handle new view message
   * @private
   * @param {Object} message - New view message
   */
  _handleNewView(message) {
    // Accept new view from new primary
    this.view = message.view;
    this.emit('viewChanged', this.view);
  }

  /**
   * Reset view change timer
   * @private
   */
  _resetViewChangeTimer() {
    this._clearViewChangeTimer();

    this.viewChangeTimer = setTimeout(() => {
      if (this.running) {
        this._initiateViewChange();
      }
    }, this.config.viewChangeTimeout);
  }

  /**
   * Clear view change timer
   * @private
   */
  _clearViewChangeTimer() {
    if (this.viewChangeTimer) {
      clearTimeout(this.viewChangeTimer);
      this.viewChangeTimer = null;
    }
  }

  /**
   * Register peer public key
   * @param {string} nodeId - Peer node ID
   * @param {KeyObject} publicKey - Peer's public key
   */
  registerPeerKey(nodeId, publicKey) {
    this.peerKeys.set(nodeId, publicKey);
  }

  /**
   * Set network handler
   * @param {Function} handler - Network handler function
   */
  setNetworkHandler(handler) {
    this.networkHandler = handler;
  }

  /**
   * Get node state
   * @returns {Object} Node state
   */
  getState() {
    return {
      nodeId: this.nodeId,
      view: this.view,
      sequence: this.sequence,
      isPrimary: this.isPrimary(),
      primary: this.getPrimary(),
      pendingRequests: this.pendingRequests.size,
      committedRequests: this.committedRequests.size,
    };
  }
}

/**
 * Create a new Byzantine node
 * @param {ByzantineConfig} config - Configuration
 * @returns {ByzantineNode} Byzantine node instance
 */
export function createByzantineNode(config) {
  return new ByzantineNode(config);
}

// Export schemas
export {
  ByzantineConfigSchema,
  RequestSchema,
  PrePrepareSchema,
  PrepareSchema,
  CommitSchema,
  SignedMessageSchema,
};
