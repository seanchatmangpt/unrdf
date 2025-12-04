/**
 * @file Peer Discovery Example Tests
 * @vitest-environment node
 */

import { describe, it, expect, beforeEach } from 'vitest';
import { createPeerManager } from '@unrdf/federation';

describe('Peer Discovery Example', () => {
  let peerManager;

  beforeEach(() => {
    peerManager = createPeerManager();
  });

  describe('Peer Registration', () => {
    it('should register a new peer', () => {
      const peer = peerManager.registerPeer(
        'test-peer',
        'http://localhost:8000/sparql',
        { dataset: 'test' }
      );

      expect(peer.id).toBe('test-peer');
      expect(peer.endpoint).toBe('http://localhost:8000/sparql');
      expect(peer.status).toBe('healthy');
      expect(peer.metadata).toEqual({ dataset: 'test' });
    });

    it('should register multiple peers', () => {
      peerManager.registerPeer('peer-1', 'http://localhost:8001/sparql');
      peerManager.registerPeer('peer-2', 'http://localhost:8002/sparql');
      peerManager.registerPeer('peer-3', 'http://localhost:8003/sparql');

      expect(peerManager.size()).toBe(3);
    });

    it('should update existing peer on re-registration', () => {
      const peer1 = peerManager.registerPeer('peer-1', 'http://localhost:8001/sparql', {
        version: '1.0',
      });
      const registeredAt = peer1.registeredAt;

      const peer2 = peerManager.registerPeer('peer-1', 'http://localhost:8001/sparql', {
        version: '2.0',
      });

      expect(peer2.metadata).toEqual({ version: '2.0' });
      expect(peer2.registeredAt).toBe(registeredAt); // Should preserve original registration time
      expect(peerManager.size()).toBe(1); // Should not create duplicate
    });
  });

  describe('Peer Listing', () => {
    beforeEach(() => {
      peerManager.registerPeer('peer-1', 'http://localhost:8001/sparql');
      peerManager.registerPeer('peer-2', 'http://localhost:8002/sparql');
      peerManager.updateStatus('peer-2', 'degraded');
    });

    it('should list all peers', () => {
      const peers = peerManager.listPeers();
      expect(peers).toHaveLength(2);
    });

    it('should filter peers by status', () => {
      const healthyPeers = peerManager.listPeers({ status: 'healthy' });
      expect(healthyPeers).toHaveLength(1);
      expect(healthyPeers[0].id).toBe('peer-1');

      const degradedPeers = peerManager.listPeers({ status: 'degraded' });
      expect(degradedPeers).toHaveLength(1);
      expect(degradedPeers[0].id).toBe('peer-2');
    });
  });

  describe('Peer Information', () => {
    it('should get peer by id', () => {
      peerManager.registerPeer('peer-1', 'http://localhost:8001/sparql', {
        region: 'us-west',
      });

      const peer = peerManager.getPeer('peer-1');
      expect(peer).toBeDefined();
      expect(peer?.id).toBe('peer-1');
      expect(peer?.metadata).toEqual({ region: 'us-west' });
    });

    it('should return null for non-existent peer', () => {
      const peer = peerManager.getPeer('non-existent');
      expect(peer).toBeNull();
    });
  });

  describe('Peer Status Management', () => {
    beforeEach(() => {
      peerManager.registerPeer('peer-1', 'http://localhost:8001/sparql');
    });

    it('should update peer status', () => {
      const updated = peerManager.updateStatus('peer-1', 'degraded');
      expect(updated).toBe(true);

      const peer = peerManager.getPeer('peer-1');
      expect(peer?.status).toBe('degraded');
    });

    it('should return false for non-existent peer', () => {
      const updated = peerManager.updateStatus('non-existent', 'degraded');
      expect(updated).toBe(false);
    });

    it('should update lastSeen timestamp on status update', () => {
      const before = Date.now();
      peerManager.updateStatus('peer-1', 'healthy');
      const after = Date.now();

      const peer = peerManager.getPeer('peer-1');
      expect(peer?.lastSeen).toBeGreaterThanOrEqual(before);
      expect(peer?.lastSeen).toBeLessThanOrEqual(after);
    });
  });

  describe('Peer Health Checks', () => {
    it('should return false for unreachable peer', async () => {
      peerManager.registerPeer('peer-1', 'http://localhost:9999/sparql');

      const isHealthy = await peerManager.ping('peer-1', 1000); // 1 second timeout
      expect(isHealthy).toBe(false);

      const peer = peerManager.getPeer('peer-1');
      expect(peer?.status).toBe('unreachable');
    });

    it('should return false for non-existent peer', async () => {
      const isHealthy = await peerManager.ping('non-existent');
      expect(isHealthy).toBe(false);
    });
  });

  describe('Peer Removal', () => {
    it('should unregister peer', () => {
      peerManager.registerPeer('peer-1', 'http://localhost:8001/sparql');
      expect(peerManager.size()).toBe(1);

      const removed = peerManager.unregisterPeer('peer-1');
      expect(removed).toBe(true);
      expect(peerManager.size()).toBe(0);
    });

    it('should return false for non-existent peer', () => {
      const removed = peerManager.unregisterPeer('non-existent');
      expect(removed).toBe(false);
    });
  });

  describe('Peer Manager Operations', () => {
    it('should clear all peers', () => {
      peerManager.registerPeer('peer-1', 'http://localhost:8001/sparql');
      peerManager.registerPeer('peer-2', 'http://localhost:8002/sparql');
      expect(peerManager.size()).toBe(2);

      peerManager.clear();
      expect(peerManager.size()).toBe(0);
    });

    it('should return correct peer count', () => {
      expect(peerManager.size()).toBe(0);

      peerManager.registerPeer('peer-1', 'http://localhost:8001/sparql');
      expect(peerManager.size()).toBe(1);

      peerManager.registerPeer('peer-2', 'http://localhost:8002/sparql');
      expect(peerManager.size()).toBe(2);
    });
  });
});
