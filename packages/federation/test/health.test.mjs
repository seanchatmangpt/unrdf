/**
 * @vitest-environment node
 */

import { describe, it, expect, beforeEach, afterEach, vi } from 'vitest';
import { createCoordinator } from '../src/federation/coordinator.mjs';
import { createHealthEndpoint } from '../src/federation/health.mjs';

/* ========================================================================= */
/* Mock Setup                                                               */
/* ========================================================================= */

const originalFetch = global.fetch;

function createMockFetch(shouldSucceed = true) {
  return vi.fn(async () => {
    if (!shouldSucceed) {
      throw new Error('Network error');
    }
    return {
      ok: true,
      status: 200,
      statusText: 'OK',
      json: async () => ({ results: { bindings: [] } }),
    };
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
/* Health Endpoint Tests                                                    */
/* ========================================================================= */

describe('Health Endpoint', () => {
  it('should create health endpoint with coordinator', () => {
    const coordinator = createCoordinator();
    const health = createHealthEndpoint(coordinator);

    expect(health).toBeDefined();
    expect(health.check).toBeTypeOf('function');
    expect(health.ready).toBeTypeOf('function');
    expect(health.startup).toBeTypeOf('function');
  });

  it('should return healthy status with no peers', async () => {
    const coordinator = createCoordinator();
    const health = createHealthEndpoint(coordinator);

    const status = await health.check();

    expect(status.status).toBe('healthy');
    expect(status.uptime).toBeGreaterThanOrEqual(0);
    expect(status.version).toBe('5.0.1');
    expect(status.peers.healthy).toBe(0);
    expect(status.peers.total).toBe(0);
    expect(status.queries.total).toBe(0);
    expect(status.queries.errors).toBe(0);
  });

  it('should return healthy status with healthy peers', async () => {
    global.fetch = createMockFetch(true);

    const coordinator = createCoordinator({
      peers: [
        { id: 'peer1', endpoint: 'http://peer1.com/sparql' },
        { id: 'peer2', endpoint: 'http://peer2.com/sparql' },
      ],
    });

    const health = createHealthEndpoint(coordinator);
    const status = await health.check();

    expect(status.status).toBe('healthy');
    expect(status.peers.total).toBe(2);
    expect(status.peers.healthy).toBe(2);
  });

  it('should return unhealthy status with all unreachable peers', async () => {
    global.fetch = createMockFetch(false);

    const coordinator = createCoordinator({
      peers: [{ id: 'peer1', endpoint: 'http://peer1.com/sparql' }],
    });

    // Ping to mark as unreachable
    await coordinator.healthCheck();

    const health = createHealthEndpoint(coordinator);
    const status = await health.check();

    expect(status.status).toBe('unhealthy');
    expect(status.peers.total).toBe(1);
    expect(status.peers.healthy).toBe(0);
  });

  it('should return degraded status with high error rate', async () => {
    global.fetch = createMockFetch(false);

    const coordinator = createCoordinator({
      peers: [{ id: 'peer1', endpoint: 'http://peer1.com/sparql' }],
    });

    // Execute queries to generate errors
    await coordinator.query('SELECT * WHERE { ?s ?p ?o }');
    await coordinator.query('SELECT * WHERE { ?s ?p ?o }');

    const health = createHealthEndpoint(coordinator);
    const status = await health.check();

    expect(status.status).toBe('degraded');
    expect(status.queries.errors).toBeGreaterThan(0);
  });

  it('should track uptime correctly', async () => {
    const coordinator = createCoordinator();
    const health = createHealthEndpoint(coordinator);

    const status1 = await health.check();
    vi.advanceTimersByTime(5000); // 5 seconds
    const status2 = await health.check();

    expect(status2.uptime).toBeGreaterThan(status1.uptime);
  });
});

/* ========================================================================= */
/* Readiness Probe Tests                                                    */
/* ========================================================================= */

describe('Readiness Probe', () => {
  it('should return not ready with no peers', async () => {
    const coordinator = createCoordinator();
    const health = createHealthEndpoint(coordinator);

    const ready = await health.ready();

    expect(ready.ready).toBe(false);
    expect(ready.healthyPeers).toBe(0);
    expect(ready.message).toContain('No healthy peers');
  });

  it('should return ready with healthy peers', async () => {
    global.fetch = createMockFetch(true);

    const coordinator = createCoordinator({
      peers: [{ id: 'peer1', endpoint: 'http://peer1.com/sparql' }],
    });

    const health = createHealthEndpoint(coordinator);
    const ready = await health.ready();

    expect(ready.ready).toBe(true);
    expect(ready.healthyPeers).toBe(1);
    expect(ready.message).toContain('ready');
  });

  it('should return not ready with only unhealthy peers', async () => {
    global.fetch = createMockFetch(false);

    const coordinator = createCoordinator({
      peers: [{ id: 'peer1', endpoint: 'http://peer1.com/sparql' }],
    });

    // Mark peer as unreachable
    await coordinator.healthCheck();

    const health = createHealthEndpoint(coordinator);
    const ready = await health.ready();

    expect(ready.ready).toBe(false);
    expect(ready.healthyPeers).toBe(0);
  });

  it('should return ready with at least one healthy peer', async () => {
    const mockFetchMixed = vi.fn(async url => {
      const endpoint = url.toString();
      if (endpoint.includes('peer1')) {
        return {
          ok: true,
          status: 200,
          statusText: 'OK',
          json: async () => ({ results: { bindings: [] } }),
        };
      }
      throw new Error('Unreachable');
    });

    global.fetch = mockFetchMixed;

    const coordinator = createCoordinator({
      peers: [
        { id: 'peer1', endpoint: 'http://peer1.com/sparql' },
        { id: 'peer2', endpoint: 'http://peer2.com/sparql' },
      ],
    });

    await coordinator.healthCheck();

    const health = createHealthEndpoint(coordinator);
    const ready = await health.ready();

    expect(ready.ready).toBe(true);
    expect(ready.healthyPeers).toBeGreaterThan(0);
  });
});

/* ========================================================================= */
/* Startup Probe Tests                                                      */
/* ========================================================================= */

describe('Startup Probe', () => {
  it('should return started immediately', async () => {
    const coordinator = createCoordinator();
    const health = createHealthEndpoint(coordinator);

    const startup = await health.startup();

    expect(startup.started).toBe(true);
    expect(startup.uptime).toBeGreaterThanOrEqual(0);
  });

  it('should track startup time', async () => {
    const coordinator = createCoordinator();
    const health = createHealthEndpoint(coordinator);

    vi.advanceTimersByTime(3000); // 3 seconds

    const startup = await health.startup();

    expect(startup.started).toBe(true);
    expect(startup.uptime).toBeGreaterThan(0);
  });
});

/* ========================================================================= */
/* Integration Tests                                                        */
/* ========================================================================= */

describe('Health Integration', () => {
  it('should reflect query activity in health status', async () => {
    global.fetch = createMockFetch(true);

    const coordinator = createCoordinator({
      peers: [{ id: 'peer1', endpoint: 'http://peer1.com/sparql' }],
    });

    const health = createHealthEndpoint(coordinator);

    // Initial status
    const status1 = await health.check();
    expect(status1.queries.total).toBe(0);

    // Execute query
    await coordinator.query('SELECT * WHERE { ?s ?p ?o }');

    // Check updated status
    const status2 = await health.check();
    expect(status2.queries.total).toBeGreaterThan(status1.queries.total);
  });

  it('should reflect peer changes in readiness', async () => {
    global.fetch = createMockFetch(true);

    const coordinator = createCoordinator();
    const health = createHealthEndpoint(coordinator);

    // Initially not ready
    const ready1 = await health.ready();
    expect(ready1.ready).toBe(false);

    // Add peer
    await coordinator.addPeer('peer1', 'http://peer1.com/sparql');

    // Now ready
    const ready2 = await health.ready();
    expect(ready2.ready).toBe(true);
    expect(ready2.healthyPeers).toBe(1);
  });

  it('should provide consistent health data across checks', async () => {
    global.fetch = createMockFetch(true);

    const coordinator = createCoordinator({
      peers: [{ id: 'peer1', endpoint: 'http://peer1.com/sparql' }],
    });

    const health = createHealthEndpoint(coordinator);

    const check1 = await health.check();
    const ready1 = await health.ready();
    const startup1 = await health.startup();

    expect(check1.peers.healthy).toBe(ready1.healthyPeers);
    expect(check1.uptime).toBe(startup1.uptime);
  });
});
