/**
 * @file membership.test.mjs
 * @description Tests for membership management and node discovery
 */

import { describe, it, expect, beforeEach, vi } from 'vitest';
import {
  MembershipManager,
  NodeStatus,
  GossipMessageType,
  createMembershipManager,
} from '../../src/consensus/membership.mjs';

describe('Membership Management', () => {
  describe('MembershipManager', () => {
    /** @type {MembershipManager} */
    let manager;

    beforeEach(() => {
      manager = new MembershipManager({
        nodeId: 'node-1',
        host: 'localhost',
        port: 7001,
        gossipInterval: 100,
        failureTimeout: 500,
        suspectTimeout: 300,
      });
    });

    it('should initialize correctly', () => {
      expect(manager.nodeId).toBe('node-1');
      expect(manager.getClusterSize()).toBe(1); // Only self
      expect(manager.running).toBe(false);
    });

    it('should include self as alive member', () => {
      const self = manager.getMember('node-1');

      expect(self).toBeDefined();
      expect(self.nodeId).toBe('node-1');
      expect(self.host).toBe('localhost');
      expect(self.port).toBe(7001);
      expect(self.status).toBe(NodeStatus.ALIVE);
    });

    it('should start and stop correctly', async () => {
      const startedSpy = vi.fn();
      const stoppedSpy = vi.fn();

      manager.on('started', startedSpy);
      manager.on('stopped', stoppedSpy);

      await manager.start();
      expect(manager.running).toBe(true);
      expect(startedSpy).toHaveBeenCalledWith('node-1');

      await manager.stop();
      expect(manager.running).toBe(false);
      expect(stoppedSpy).toHaveBeenCalledWith('node-1');
    });

    it('should handle member join via gossip', () => {
      const joinedSpy = vi.fn();
      manager.on('memberJoined', joinedSpy);

      const newMember = {
        nodeId: 'node-2',
        host: 'localhost',
        port: 7002,
        status: NodeStatus.ALIVE,
        incarnation: 0,
        lastSeen: Date.now(),
      };

      const joinMessage = {
        type: GossipMessageType.JOIN,
        from: 'node-2',
        members: [newMember],
        timestamp: Date.now(),
      };

      manager.handleMessage(joinMessage);

      expect(joinedSpy).toHaveBeenCalledWith(newMember);
      expect(manager.getClusterSize()).toBe(2);

      const member = manager.getMember('node-2');
      expect(member).toBeDefined();
      expect(member.status).toBe(NodeStatus.ALIVE);
    });

    it('should detect failed members', async () => {
      const suspectSpy = vi.fn();
      const failedSpy = vi.fn();

      manager.on('memberSuspect', suspectSpy);
      manager.on('memberFailed', failedSpy);

      // Add a member
      manager._addMember({
        nodeId: 'node-2',
        host: 'localhost',
        port: 7002,
        status: NodeStatus.ALIVE,
        incarnation: 0,
        lastSeen: Date.now() - 400, // Last seen 400ms ago
      });

      await manager.start();

      // Wait for suspect timeout (300ms)
      await new Promise((resolve) => setTimeout(resolve, 350));

      expect(suspectSpy).toHaveBeenCalled();

      const member = manager.getMember('node-2');
      expect(member.status).toBe(NodeStatus.SUSPECT);

      // Wait for failure timeout (500ms total)
      await new Promise((resolve) => setTimeout(resolve, 200));

      expect(failedSpy).toHaveBeenCalled();
      expect(member.status).toBe(NodeStatus.DEAD);

      await manager.stop();
    });

    it('should refute suspicions by incrementing incarnation', () => {
      expect(manager.incarnation).toBe(0);

      // Receive suspicion about self
      const suspicionMessage = {
        type: GossipMessageType.SUSPECT,
        from: 'node-2',
        members: [
          {
            nodeId: 'node-1',
            host: 'localhost',
            port: 7001,
            status: NodeStatus.SUSPECT,
            incarnation: 0,
            lastSeen: Date.now(),
          },
        ],
        timestamp: Date.now(),
      };

      // Mock network handler to capture broadcast
      const broadcasts = [];
      manager.setNetworkHandler(async (peer, message) => {
        broadcasts.push({ peer, message });
      });

      manager.handleMessage(suspicionMessage);

      // Should increment incarnation
      expect(manager.incarnation).toBe(1);

      // Should broadcast alive status with new incarnation
      expect(broadcasts.length).toBeGreaterThan(0);
    });

    it('should gossip member updates periodically', async () => {
      const messages = [];

      manager.setNetworkHandler(async (peer, message) => {
        messages.push({ peer, message });
      });

      // Add some members
      manager._addMember({
        nodeId: 'node-2',
        host: 'localhost',
        port: 7002,
        status: NodeStatus.ALIVE,
        incarnation: 0,
        lastSeen: Date.now(),
      });

      manager._addMember({
        nodeId: 'node-3',
        host: 'localhost',
        port: 7003,
        status: NodeStatus.ALIVE,
        incarnation: 0,
        lastSeen: Date.now(),
      });

      await manager.start();

      // Wait for gossip interval
      await new Promise((resolve) => setTimeout(resolve, 150));

      expect(messages.length).toBeGreaterThan(0);

      // Verify gossip messages contain member updates
      const gossipMessage = messages.find((m) => m.message.type === GossipMessageType.PING);
      expect(gossipMessage).toBeDefined();
      expect(gossipMessage.message.members).toBeDefined();

      await manager.stop();
    });

    it('should handle ping-ack protocol', async () => {
      const ackSpy = vi.fn();

      manager.setNetworkHandler(async (peer, message) => {
        if (message.type === GossipMessageType.ACK) {
          ackSpy(peer, message);
        }
      });

      const pingMessage = {
        type: GossipMessageType.PING,
        from: 'node-2',
        members: [],
        timestamp: Date.now(),
      };

      manager.handleMessage(pingMessage);

      // Should send ACK in response
      await new Promise((resolve) => setTimeout(resolve, 10));

      expect(ackSpy).toHaveBeenCalled();
    });

    it('should broadcast leave message on stop', async () => {
      const messages = [];

      manager.setNetworkHandler(async (peer, message) => {
        messages.push({ peer, message });
      });

      manager._addMember({
        nodeId: 'node-2',
        host: 'localhost',
        port: 7002,
        status: NodeStatus.ALIVE,
        incarnation: 0,
        lastSeen: Date.now(),
      });

      await manager.start();
      await manager.stop();

      // Should have sent LEAVE message
      const leaveMessage = messages.find((m) => m.message.type === GossipMessageType.LEAVE);
      expect(leaveMessage).toBeDefined();
      expect(leaveMessage.message.members[0].status).toBe(NodeStatus.LEFT);
    });

    it('should update lastSeen on message receipt', () => {
      manager._addMember({
        nodeId: 'node-2',
        host: 'localhost',
        port: 7002,
        status: NodeStatus.ALIVE,
        incarnation: 0,
        lastSeen: Date.now() - 1000,
      });

      const beforeLastSeen = manager.getMember('node-2').lastSeen;

      const pingMessage = {
        type: GossipMessageType.PING,
        from: 'node-2',
        members: [],
        timestamp: Date.now(),
      };

      manager.handleMessage(pingMessage);

      const afterLastSeen = manager.getMember('node-2').lastSeen;

      expect(afterLastSeen).toBeGreaterThan(beforeLastSeen);
    });

    it('should select random members for gossip fanout', () => {
      // Add multiple members
      for (let i = 2; i <= 10; i++) {
        manager._addMember({
          nodeId: `node-${i}`,
          host: 'localhost',
          port: 7000 + i,
          status: NodeStatus.ALIVE,
          incarnation: 0,
          lastSeen: Date.now(),
        });
      }

      const aliveMembers = manager._getAliveMembers();
      expect(aliveMembers.length).toBe(10); // 9 peers + self

      const selected = manager._selectRandomMembers(aliveMembers, 3);
      expect(selected.length).toBe(3);

      // Verify randomness - selected members should be different on multiple calls
      const selected2 = manager._selectRandomMembers(aliveMembers, 3);
      const different = selected.some(
        (m, i) => !selected2.find((m2) => m2.nodeId === m.nodeId)
      );
      expect(different).toBe(true);
    });

    it('should handle status transitions correctly', () => {
      manager._addMember({
        nodeId: 'node-2',
        host: 'localhost',
        port: 7002,
        status: NodeStatus.ALIVE,
        incarnation: 0,
        lastSeen: Date.now(),
      });

      const member = manager.getMember('node-2');

      // Valid transition: ALIVE -> SUSPECT
      expect(manager._shouldUpdateStatus(NodeStatus.ALIVE, NodeStatus.SUSPECT)).toBe(true);

      // Valid transition: SUSPECT -> DEAD
      expect(manager._shouldUpdateStatus(NodeStatus.SUSPECT, NodeStatus.DEAD)).toBe(true);

      // Invalid transition: DEAD -> ALIVE (not allowed)
      expect(manager._shouldUpdateStatus(NodeStatus.DEAD, NodeStatus.ALIVE)).toBe(false);

      // Valid transition: SUSPECT -> ALIVE (refutation)
      expect(manager._shouldUpdateStatus(NodeStatus.SUSPECT, NodeStatus.ALIVE)).toBe(true);
    });

    it('should ignore updates from older incarnations', () => {
      manager._addMember({
        nodeId: 'node-2',
        host: 'localhost',
        port: 7002,
        status: NodeStatus.ALIVE,
        incarnation: 5,
        lastSeen: Date.now(),
      });

      const updateMessage = {
        type: GossipMessageType.ALIVE,
        from: 'node-3',
        members: [
          {
            nodeId: 'node-2',
            host: 'localhost',
            port: 7002,
            status: NodeStatus.SUSPECT,
            incarnation: 3, // Older incarnation
            lastSeen: Date.now(),
          },
        ],
        timestamp: Date.now(),
      };

      manager.handleMessage(updateMessage);

      const member = manager.getMember('node-2');
      expect(member.status).toBe(NodeStatus.ALIVE); // Status unchanged
      expect(member.incarnation).toBe(5); // Incarnation unchanged
    });

    it('should accept updates from higher incarnations', () => {
      manager._addMember({
        nodeId: 'node-2',
        host: 'localhost',
        port: 7002,
        status: NodeStatus.ALIVE,
        incarnation: 3,
        lastSeen: Date.now(),
      });

      const updateMessage = {
        type: GossipMessageType.SUSPECT,
        from: 'node-3',
        members: [
          {
            nodeId: 'node-2',
            host: 'localhost',
            port: 7002,
            status: NodeStatus.SUSPECT,
            incarnation: 5, // Higher incarnation
            lastSeen: Date.now(),
          },
        ],
        timestamp: Date.now(),
      };

      manager.handleMessage(updateMessage);

      const member = manager.getMember('node-2');
      expect(member.status).toBe(NodeStatus.SUSPECT);
      expect(member.incarnation).toBe(5);
    });
  });

  describe('createMembershipManager', () => {
    it('should create a new membership manager', () => {
      const manager = createMembershipManager({
        nodeId: 'test-node',
        host: '127.0.0.1',
        port: 8000,
      });

      expect(manager).toBeInstanceOf(MembershipManager);
      expect(manager.nodeId).toBe('test-node');
    });
  });
});
