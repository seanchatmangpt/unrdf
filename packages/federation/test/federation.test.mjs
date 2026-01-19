/**
 * @file Federation - Distributed Query Execution Tests
 * @module federation/test/federation
 * @description Fast tests for federation distributed query execution
 * @vitest-environment node
 */

import { describe, it, expect, beforeEach, afterEach, vi } from 'vitest';
import {
  createCoordinator,
  createPeerManager,
  executeFederatedQuery,
} from '../src/index.mjs';

// Mock fetch for testing
const originalFetch = global.fetch;

function createMockFetch(responses = {}) {
  return vi.fn(async (url) => {
    const endpoint = url.toString();
    if (endpoint.includes('/sparql')) {
      const peerId = Object.keys(responses).find(id => endpoint.includes(id));
      const response = responses[peerId] || { ok: true, data: [] };
      return {
        ok: response.ok !== false,
        status: response.status || 200,
        json: async () => response.data,
      };
    }
    return { ok: true, status: 200 };
  });
}

beforeEach(() => {
  vi.useFakeTimers();
});

afterEach(() => {
  global.fetch = originalFetch;
  vi.restoreAllMocks();
  vi.useRealTimers();
});

/* ========================================================================= */
/* Peer Manager - Basic Registration                                       */
/* ========================================================================= */

describe('PeerManager', () => {
  it('should register and list peers', () => {
    const manager = createPeerManager();
    manager.registerPeer('peer1', 'http://peer1.com/sparql');
    manager.registerPeer('peer2', 'http://peer2.com/sparql');

    const peers = manager.listPeers();
    expect(peers).toHaveLength(2);
    expect(peers[0].id).toBe('peer1');
    expect(peers[1].id).toBe('peer2');
  });
});

/* ========================================================================= */
/* Distributed Query - Basic Execution                                     */
/* ========================================================================= */

describe('Distributed Query', () => {
  it('should execute federated query successfully', async () => {
    const mockData = {
      results: {
        bindings: [{ name: { type: 'literal', value: 'Alice' } }],
      },
    };

    global.fetch = createMockFetch({
      peer1: { ok: true, data: mockData },
    });

    const result = await executeFederatedQuery(
      'peer1',
      'http://peer1.com/sparql',
      'SELECT * WHERE { ?s ?p ?o }'
    );

    expect(result.success).toBe(true);
    expect(result.peerId).toBe('peer1');
    expect(result.data).toEqual(mockData);
  });

  it('should execute distributed query across multiple peers', async () => {
    const mockData1 = {
      results: { bindings: [{ x: { type: 'literal', value: '1' } }] },
    };

    const mockData2 = {
      results: { bindings: [{ x: { type: 'literal', value: '2' } }] },
    };

    global.fetch = createMockFetch({
      peer1: { ok: true, data: mockData1 },
      peer2: { ok: true, data: mockData2 },
    });

    const peers = [
      { id: 'peer1', endpoint: 'http://peer1.com/sparql' },
      { id: 'peer2', endpoint: 'http://peer2.com/sparql' },
    ];

    const result = await executeDistributedQuery(peers, 'SELECT * { ?s ?p ?o }');

    expect(result.success).toBe(true);
    expect(result.successCount).toBe(2);
    expect(result.failureCount).toBe(0);
  });
});
