/**
 * @vitest-environment node
 * Adversarial Testing: Test advertised capabilities for @unrdf/federation
 * Goal: PROVE what doesn't work, not security
 */

import { describe, it, expect } from 'vitest';
import {
  createCoordinator,
  createPeerManager,
  executeFederatedQuery,
  aggregateResults,
  routeQuery,
} from '../src/index.mjs';
import { createStore, addQuad, namedNode, literal, quad } from '@unrdf/core';

describe('@unrdf/federation Adversarial Tests - Capabilities', () => {
  describe('Coordinator - Advertised Features', () => {
    it('ADVERTISED: Can create federation coordinator', () => {
      const coordinator = createCoordinator({
        localStore: createStore(),
        peerId: 'test-peer-1',
      });

      expect(coordinator).toBeDefined();
      expect(coordinator.addPeer).toBeDefined();
      expect(coordinator.removePeer).toBeDefined();
    });

    it('ADVERTISED: Coordinator can manage peers', () => {
      const coordinator = createCoordinator({
        localStore: createStore(),
        peerId: 'test-peer-1',
      });

      coordinator.addPeer({ id: 'peer-2', endpoint: 'http://localhost:8002' });
      const peers = coordinator.getPeers();

      expect(peers.length).toBe(1);
      expect(peers[0].id).toBe('peer-2');
    });

    it('ADVERTISED: Can initialize coordinator with multiple peers', () => {
      const coordinator = createCoordinator({
        localStore: createStore(),
        peerId: 'test-peer-1',
        peers: [
          { id: 'peer-2', endpoint: 'http://localhost:8002' },
          { id: 'peer-3', endpoint: 'http://localhost:8003' },
        ],
      });

      const peers = coordinator.getPeers();
      expect(peers.length).toBe(2);
    });
  });

  describe('Peer Manager - Advertised Features', () => {
    it('ADVERTISED: Can create peer manager', () => {
      const peerManager = createPeerManager();

      expect(peerManager).toBeDefined();
      expect(peerManager.addPeer).toBeDefined();
      expect(peerManager.getPeer).toBeDefined();
    });

    it('ADVERTISED: Can add and retrieve peers', () => {
      const peerManager = createPeerManager();

      peerManager.addPeer({ id: 'peer-1', endpoint: 'http://localhost:8001' });
      const peer = peerManager.getPeer('peer-1');

      expect(peer).toBeDefined();
      expect(peer.id).toBe('peer-1');
      expect(peer.endpoint).toBe('http://localhost:8001');
    });

    it('ADVERTISED: Can list all peers', () => {
      const peerManager = createPeerManager();

      peerManager.addPeer({ id: 'peer-1', endpoint: 'http://localhost:8001' });
      peerManager.addPeer({ id: 'peer-2', endpoint: 'http://localhost:8002' });

      const peers = peerManager.listPeers();
      expect(peers.length).toBe(2);
    });

    it('ADVERTISED: Can remove peers', () => {
      const peerManager = createPeerManager();

      peerManager.addPeer({ id: 'peer-1', endpoint: 'http://localhost:8001' });
      peerManager.removePeer('peer-1');

      const peers = peerManager.listPeers();
      expect(peers.length).toBe(0);
    });
  });

  describe('Distributed Query - Advertised Features', () => {
    it('ADVERTISED: Can execute federated queries', async () => {
      const store = createStore();
      addQuad(
        store,
        quad(namedNode('http://example.org/alice'), namedNode('http://foaf/name'), literal('Alice'))
      );

      const result = await executeFederatedQuery({
        query: 'SELECT ?s ?p ?o WHERE { ?s ?p ?o }',
        localStore: store,
        peers: [],
      });

      expect(result).toBeDefined();
    });

    it('ADVERTISED: Can aggregate results from multiple sources', () => {
      const results1 = [{ name: { value: 'Alice' } }];
      const results2 = [{ name: { value: 'Bob' } }];

      const aggregated = aggregateResults([results1, results2]);

      expect(aggregated.length).toBe(2);
      expect(aggregated[0].name.value).toBe('Alice');
      expect(aggregated[1].name.value).toBe('Bob');
    });

    it('ADVERTISED: Can route queries to appropriate peers', () => {
      const query = 'SELECT ?s WHERE { ?s a <http://example.org/Person> }';
      const peers = [
        { id: 'peer-1', endpoint: 'http://localhost:8001', capabilities: ['person-data'] },
        { id: 'peer-2', endpoint: 'http://localhost:8002', capabilities: ['location-data'] },
      ];

      const routes = routeQuery({ query, peers });

      expect(routes).toBeDefined();
      expect(Array.isArray(routes)).toBe(true);
    });
  });
});
