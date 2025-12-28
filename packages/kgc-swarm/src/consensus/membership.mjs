/**
 * @file membership.mjs
 * @description Node discovery and membership management for distributed swarm
 *
 * Features:
 * - Gossip-based node discovery
 * - Failure detection with heartbeats
 * - Membership changes (join/leave/fail)
 * - SWIM protocol for scalable monitoring
 *
 * @module @unrdf/kgc-swarm/consensus/membership
 */

import { z } from 'zod';
import { EventEmitter } from 'node:events';

/**
 * Node status enumeration
 * @enum {string}
 */
export const NodeStatus = {
  ALIVE: 'alive',
  SUSPECT: 'suspect',
  DEAD: 'dead',
  LEFT: 'left',
};

/**
 * Membership configuration
 * @typedef {Object} MembershipConfig
 * @property {string} nodeId - This node's identifier
 * @property {string} host - This node's host
 * @property {number} port - This node's port
 * @property {string[]} [seeds] - Seed nodes for bootstrap
 * @property {number} [gossipInterval=1000] - Gossip interval (ms)
 * @property {number} [failureTimeout=5000] - Failure detection timeout (ms)
 * @property {number} [suspectTimeout=3000] - Suspect timeout before marking dead (ms)
 * @property {number} [gossipFanout=3] - Number of nodes to gossip to
 */
const MembershipConfigSchema = z.object({
  nodeId: z.string(),
  host: z.string(),
  port: z.number().int().positive(),
  seeds: z.array(z.string()).default([]),
  gossipInterval: z.number().positive().default(1000),
  failureTimeout: z.number().positive().default(5000),
  suspectTimeout: z.number().positive().default(3000),
  gossipFanout: z.number().int().positive().default(3),
});

/**
 * Member information
 * @typedef {Object} Member
 * @property {string} nodeId - Node identifier
 * @property {string} host - Node host
 * @property {number} port - Node port
 * @property {NodeStatus} status - Node status
 * @property {number} incarnation - Incarnation number (for refuting suspicions)
 * @property {number} lastSeen - Last heartbeat timestamp
 * @property {Object} [metadata] - Optional metadata
 */
const MemberSchema = z.object({
  nodeId: z.string(),
  host: z.string(),
  port: z.number().int().positive(),
  status: z.nativeEnum(NodeStatus),
  incarnation: z.number().int().nonnegative().default(0),
  lastSeen: z.number(),
  metadata: z.record(z.any()).optional(),
});

/**
 * Gossip message types
 * @enum {string}
 */
export const GossipMessageType = {
  PING: 'ping',
  ACK: 'ack',
  PING_REQ: 'ping-req',
  ALIVE: 'alive',
  SUSPECT: 'suspect',
  DEAD: 'dead',
  JOIN: 'join',
  LEAVE: 'leave',
};

/**
 * Gossip message schema
 * @typedef {Object} GossipMessage
 * @property {string} type - Message type
 * @property {string} from - Sender node ID
 * @property {string} [to] - Target node ID (for direct messages)
 * @property {Member[]} [members] - Member updates
 * @property {number} [incarnation] - Incarnation number
 * @property {number} timestamp - Message timestamp
 */
const GossipMessageSchema = z.object({
  type: z.nativeEnum(GossipMessageType),
  from: z.string(),
  to: z.string().optional(),
  members: z.array(MemberSchema).optional(),
  incarnation: z.number().int().nonnegative().optional(),
  timestamp: z.number(),
});

/**
 * MembershipManager: Manages cluster membership using SWIM protocol
 *
 * SWIM (Scalable Weakly-consistent Infection-style Membership Protocol):
 * - Gossip-based dissemination: O(log N) messages per change
 * - Failure detection: Indirect probing via ping-req
 * - Suspicion mechanism: Reduces false positives
 * - Incarnation numbers: Nodes can refute suspicions
 *
 * @class MembershipManager
 * @extends EventEmitter
 * @example
 * ```javascript
 * const membership = new MembershipManager({
 *   nodeId: 'node-1',
 *   host: 'localhost',
 *   port: 7001,
 *   seeds: ['node-2:7002', 'node-3:7003']
 * });
 *
 * membership.on('memberJoined', (member) => {
 *   console.log(`Node ${member.nodeId} joined`);
 * });
 *
 * membership.on('memberFailed', (member) => {
 *   console.log(`Node ${member.nodeId} failed`);
 * });
 *
 * await membership.start();
 * ```
 */
export class MembershipManager extends EventEmitter {
  /**
   * @param {MembershipConfig} config - Membership configuration
   */
  constructor(config) {
    super();
    this.config = MembershipConfigSchema.parse(config);

    /** @type {string} */
    this.nodeId = this.config.nodeId;

    /** @type {Map<string, Member>} - Cluster members */
    this.members = new Map();

    /** @type {number} - Incarnation number for this node */
    this.incarnation = 0;

    /** @type {boolean} */
    this.running = false;

    /** @type {NodeJS.Timeout | null} */
    this.gossipTimer = null;

    /** @type {NodeJS.Timeout | null} */
    this.failureTimer = null;

    /** @type {Function | null} - Network send handler */
    this.networkHandler = null;

    // Add self to members
    this._addMember({
      nodeId: this.nodeId,
      host: this.config.host,
      port: this.config.port,
      status: NodeStatus.ALIVE,
      incarnation: this.incarnation,
      lastSeen: Date.now(),
    });
  }

  /**
   * Start membership management
   * @returns {Promise<void>}
   */
  async start() {
    if (this.running) return;

    this.running = true;

    // Bootstrap from seed nodes
    await this._bootstrap();

    // Start gossip timer
    this._startGossip();

    // Start failure detection timer
    this._startFailureDetection();

    this.emit('started', this.nodeId);
  }

  /**
   * Stop membership management
   * @returns {Promise<void>}
   */
  async stop() {
    if (!this.running) return;

    this.running = false;

    // Announce leave
    await this._broadcastLeave();

    this._clearGossipTimer();
    this._clearFailureTimer();

    this.emit('stopped', this.nodeId);
  }

  /**
   * Bootstrap from seed nodes
   * @private
   */
  async _bootstrap() {
    if (this.config.seeds.length === 0) {
      // No seeds - we are the first node
      return;
    }

    // Send join message to seeds
    for (const seed of this.config.seeds) {
      const [host, port] = seed.split(':');

      const joinMessage = {
        type: GossipMessageType.JOIN,
        from: this.nodeId,
        members: [this._getSelfMember()],
        timestamp: Date.now(),
      };

      try {
        await this._sendMessage(seed, joinMessage);
      } catch (error) {
        this.emit('bootstrapError', seed, error);
      }
    }
  }

  /**
   * Get this node's member info
   * @private
   * @returns {Member}
   */
  _getSelfMember() {
    return this.members.get(this.nodeId);
  }

  /**
   * Add or update member
   * @private
   * @param {Member} member - Member to add/update
   */
  _addMember(member) {
    MemberSchema.parse(member);

    const existing = this.members.get(member.nodeId);

    if (existing) {
      // Update existing member
      if (member.incarnation > existing.incarnation) {
        // Higher incarnation - accept update
        this.members.set(member.nodeId, member);

        if (existing.status !== member.status) {
          this._emitStatusChange(existing, member);
        }
      } else if (member.incarnation === existing.incarnation) {
        // Same incarnation - update lastSeen
        existing.lastSeen = Math.max(existing.lastSeen, member.lastSeen);

        // Status transitions: ALIVE -> SUSPECT -> DEAD
        if (this._shouldUpdateStatus(existing.status, member.status)) {
          existing.status = member.status;
          this._emitStatusChange(existing, member);
        }
      }
    } else {
      // New member
      this.members.set(member.nodeId, member);
      this.emit('memberJoined', member);
    }
  }

  /**
   * Check if status transition is valid
   * @private
   * @param {NodeStatus} currentStatus - Current status
   * @param {NodeStatus} newStatus - New status
   * @returns {boolean}
   */
  _shouldUpdateStatus(currentStatus, newStatus) {
    const transitions = {
      [NodeStatus.ALIVE]: [NodeStatus.SUSPECT, NodeStatus.DEAD, NodeStatus.LEFT],
      [NodeStatus.SUSPECT]: [NodeStatus.ALIVE, NodeStatus.DEAD, NodeStatus.LEFT],
      [NodeStatus.DEAD]: [NodeStatus.LEFT],
      [NodeStatus.LEFT]: [],
    };

    return transitions[currentStatus]?.includes(newStatus) || false;
  }

  /**
   * Emit status change event
   * @private
   * @param {Member} oldMember - Previous member state
   * @param {Member} newMember - New member state
   */
  _emitStatusChange(oldMember, newMember) {
    switch (newMember.status) {
      case NodeStatus.SUSPECT:
        this.emit('memberSuspect', newMember);
        break;
      case NodeStatus.DEAD:
        this.emit('memberFailed', newMember);
        break;
      case NodeStatus.LEFT:
        this.emit('memberLeft', newMember);
        break;
      case NodeStatus.ALIVE:
        if (oldMember.status === NodeStatus.SUSPECT) {
          this.emit('memberAlive', newMember);
        }
        break;
    }
  }

  /**
   * Start gossip timer
   * @private
   */
  _startGossip() {
    this._clearGossipTimer();

    this.gossipTimer = setInterval(() => {
      this._performGossip();
    }, this.config.gossipInterval);
  }

  /**
   * Clear gossip timer
   * @private
   */
  _clearGossipTimer() {
    if (this.gossipTimer) {
      clearInterval(this.gossipTimer);
      this.gossipTimer = null;
    }
  }

  /**
   * Perform gossip round
   * @private
   */
  async _performGossip() {
    if (!this.running) return;

    const aliveMembers = this._getAliveMembers();
    if (aliveMembers.length === 0) return;

    // Select random members to gossip to
    const targets = this._selectRandomMembers(aliveMembers, this.config.gossipFanout);

    // Prepare member updates
    const memberUpdates = Array.from(this.members.values())
      .filter(m => m.status !== NodeStatus.LEFT)
      .slice(0, 10); // Limit gossip payload

    const gossipMessage = {
      type: GossipMessageType.PING,
      from: this.nodeId,
      members: memberUpdates,
      timestamp: Date.now(),
    };

    // Send to selected targets
    for (const target of targets) {
      try {
        await this._sendMessage(target.nodeId, gossipMessage);
      } catch (error) {
        this.emit('gossipError', target.nodeId, error);
      }
    }
  }

  /**
   * Start failure detection timer
   * @private
   */
  _startFailureDetection() {
    this._clearFailureTimer();

    this.failureTimer = setInterval(() => {
      this._detectFailures();
    }, this.config.failureTimeout);
  }

  /**
   * Clear failure timer
   * @private
   */
  _clearFailureTimer() {
    if (this.failureTimer) {
      clearInterval(this.failureTimer);
      this.failureTimer = null;
    }
  }

  /**
   * Detect failed members
   * @private
   */
  _detectFailures() {
    if (!this.running) return;

    const now = Date.now();

    for (const member of this.members.values()) {
      if (member.nodeId === this.nodeId) continue;
      if (member.status === NodeStatus.DEAD || member.status === NodeStatus.LEFT) continue;

      const timeSinceLastSeen = now - member.lastSeen;

      if (member.status === NodeStatus.ALIVE && timeSinceLastSeen > this.config.suspectTimeout) {
        // Mark as suspect
        member.status = NodeStatus.SUSPECT;
        this.emit('memberSuspect', member);

        // Broadcast suspicion
        this._broadcastStatusChange(member, NodeStatus.SUSPECT);
      } else if (
        member.status === NodeStatus.SUSPECT &&
        timeSinceLastSeen > this.config.failureTimeout
      ) {
        // Mark as dead
        member.status = NodeStatus.DEAD;
        this.emit('memberFailed', member);

        // Broadcast failure
        this._broadcastStatusChange(member, NodeStatus.DEAD);
      }
    }
  }

  /**
   * Broadcast status change
   * @private
   * @param {Member} member - Member with status change
   * @param {NodeStatus} status - New status
   */
  async _broadcastStatusChange(member, status) {
    const message = {
      type: GossipMessageType[status.toUpperCase()],
      from: this.nodeId,
      members: [{ ...member, status }],
      timestamp: Date.now(),
    };

    await this._broadcast(message);
  }

  /**
   * Broadcast leave message
   * @private
   */
  async _broadcastLeave() {
    const selfMember = this._getSelfMember();
    selfMember.status = NodeStatus.LEFT;

    const leaveMessage = {
      type: GossipMessageType.LEAVE,
      from: this.nodeId,
      members: [selfMember],
      timestamp: Date.now(),
    };

    await this._broadcast(leaveMessage);
  }

  /**
   * Broadcast message to all members
   * @private
   * @param {GossipMessage} message - Message to broadcast
   */
  async _broadcast(message) {
    const aliveMembers = this._getAliveMembers();

    for (const member of aliveMembers) {
      if (member.nodeId !== this.nodeId) {
        try {
          await this._sendMessage(member.nodeId, message);
        } catch (error) {
          this.emit('broadcastError', member.nodeId, error);
        }
      }
    }
  }

  /**
   * Send message to a member
   * @private
   * @param {string} nodeId - Target node ID
   * @param {GossipMessage} message - Message to send
   */
  async _sendMessage(nodeId, message) {
    GossipMessageSchema.parse(message);

    if (this.networkHandler) {
      await this.networkHandler(nodeId, message);
    } else {
      throw new Error('Network handler not configured');
    }
  }

  /**
   * Handle incoming gossip message
   * @param {GossipMessage} message - Incoming message
   */
  handleMessage(message) {
    GossipMessageSchema.parse(message);

    // Update lastSeen for sender
    const sender = this.members.get(message.from);
    if (sender) {
      sender.lastSeen = Date.now();
    }

    // Process member updates
    if (message.members) {
      for (const member of message.members) {
        if (member.nodeId === this.nodeId) {
          // Message about us - check for suspicion
          if (member.status === NodeStatus.SUSPECT && member.incarnation <= this.incarnation) {
            // Refute suspicion by incrementing incarnation
            this.incarnation += 1;
            const selfMember = this._getSelfMember();
            selfMember.incarnation = this.incarnation;

            // Broadcast alive message
            this._broadcastStatusChange(selfMember, NodeStatus.ALIVE);
          }
        } else {
          this._addMember(member);
        }
      }
    }

    // Send ACK for PING
    if (message.type === GossipMessageType.PING) {
      const ackMessage = {
        type: GossipMessageType.ACK,
        from: this.nodeId,
        to: message.from,
        timestamp: Date.now(),
      };

      this._sendMessage(message.from, ackMessage).catch((error) => {
        this.emit('ackError', message.from, error);
      });
    }
  }

  /**
   * Get alive members
   * @private
   * @returns {Member[]}
   */
  _getAliveMembers() {
    return Array.from(this.members.values()).filter((m) => m.status === NodeStatus.ALIVE);
  }

  /**
   * Select random members
   * @private
   * @param {Member[]} members - Members to select from
   * @param {number} count - Number to select
   * @returns {Member[]}
   */
  _selectRandomMembers(members, count) {
    const shuffled = [...members].sort(() => Math.random() - 0.5);
    return shuffled.slice(0, count);
  }

  /**
   * Set network handler
   * @param {Function} handler - Network handler function
   */
  setNetworkHandler(handler) {
    this.networkHandler = handler;
  }

  /**
   * Get all members
   * @returns {Member[]} Array of members
   */
  getMembers() {
    return Array.from(this.members.values());
  }

  /**
   * Get member by ID
   * @param {string} nodeId - Node identifier
   * @returns {Member | undefined} Member info
   */
  getMember(nodeId) {
    return this.members.get(nodeId);
  }

  /**
   * Get cluster size
   * @returns {number} Number of alive members
   */
  getClusterSize() {
    return this._getAliveMembers().length;
  }
}

/**
 * Create a new membership manager
 * @param {MembershipConfig} config - Configuration
 * @returns {MembershipManager} Membership manager instance
 */
export function createMembershipManager(config) {
  return new MembershipManager(config);
}

// Export schemas
export { MembershipConfigSchema, MemberSchema, GossipMessageSchema };
