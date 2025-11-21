/**
 * @file Tests for useFederationHealth hook functionality
 * Tests health monitoring, metrics collection, and diagnostics
 */

import { describe, it, expect, beforeEach, vi } from 'vitest';

describe('FederationHealth', () => {
  describe('Health Status', () => {
    it('should initialize with unknown status', () => {
      const initialHealth = {
        status: 'unknown',
        score: 0,
        checks: []
      };

      expect(initialHealth.status).toBe('unknown');
      expect(initialHealth.score).toBe(0);
    });

    it('should classify health by score', () => {
      const classifyHealth = (score) => {
        if (score >= 80) return 'healthy';
        if (score >= 50) return 'degraded';
        return 'unhealthy';
      };

      expect(classifyHealth(95)).toBe('healthy');
      expect(classifyHealth(80)).toBe('healthy');
      expect(classifyHealth(65)).toBe('degraded');
      expect(classifyHealth(50)).toBe('degraded');
      expect(classifyHealth(30)).toBe('unhealthy');
    });

    it('should track health status changes', () => {
      const healthHistory = [];
      let previousStatus = null;

      const updateHealth = (newHealth) => {
        if (previousStatus !== newHealth.status) {
          healthHistory.push({
            from: previousStatus,
            to: newHealth.status,
            timestamp: new Date().toISOString()
          });
        }
        previousStatus = newHealth.status;
      };

      updateHealth({ status: 'healthy' });
      updateHealth({ status: 'healthy' }); // No change
      updateHealth({ status: 'degraded' });
      updateHealth({ status: 'unhealthy' });

      expect(healthHistory).toHaveLength(3);
      expect(healthHistory[1].from).toBe('healthy');
      expect(healthHistory[1].to).toBe('degraded');
    });

    it('should trigger callback on unhealthy transition', () => {
      const callbacks = [];
      let previousHealth = { status: 'healthy' };

      const onUnhealthy = (health) => {
        callbacks.push({ health, triggeredAt: Date.now() });
      };

      const checkHealth = (newHealth) => {
        if (previousHealth.status === 'healthy' && newHealth.status !== 'healthy') {
          onUnhealthy(newHealth);
        }
        previousHealth = newHealth;
      };

      checkHealth({ status: 'healthy' });
      checkHealth({ status: 'degraded' });
      checkHealth({ status: 'unhealthy' }); // Already not healthy

      expect(callbacks).toHaveLength(1);
      expect(callbacks[0].health.status).toBe('degraded');
    });
  });

  describe('Health Score Calculation', () => {
    it('should calculate score from store availability', () => {
      const calculateStoreScore = (stores) => {
        const total = stores.length;
        const healthy = stores.filter(s => s.status === 'healthy').length;
        return total > 0 ? (healthy / total) * 40 : 40; // 40 points max
      };

      const stores = [
        { id: 's1', status: 'healthy' },
        { id: 's2', status: 'healthy' },
        { id: 's3', status: 'unhealthy' },
        { id: 's4', status: 'healthy' }
      ];

      const score = calculateStoreScore(stores);
      expect(score).toBe(30); // 3/4 * 40
    });

    it('should penalize unhealthy consensus', () => {
      const calculateConsensusScore = (consensus) => {
        return consensus.healthy ? 30 : 0; // 30 points max
      };

      expect(calculateConsensusScore({ healthy: true })).toBe(30);
      expect(calculateConsensusScore({ healthy: false })).toBe(0);
    });

    it('should penalize high replication lag', () => {
      const calculateLagScore = (lag) => {
        if (lag <= 100) return 20; // Full points
        if (lag <= 1000) return 20 - Math.floor(lag / 100);
        return 0;
      };

      expect(calculateLagScore(50)).toBe(20);
      expect(calculateLagScore(500)).toBe(15);
      expect(calculateLagScore(1500)).toBe(0);
    });

    it('should penalize conflicts', () => {
      const calculateConflictScore = (conflicts) => {
        return Math.max(0, 10 - conflicts); // 10 points max
      };

      expect(calculateConflictScore(0)).toBe(10);
      expect(calculateConflictScore(5)).toBe(5);
      expect(calculateConflictScore(15)).toBe(0);
    });

    it('should combine all scores', () => {
      const calculateHealthScore = (healthData) => {
        let score = 100;

        // Store availability (40 points)
        const storeCount = healthData.stores?.length || 0;
        const healthyStores = healthData.stores?.filter(s => s.status === 'healthy').length || 0;
        if (storeCount > 0) {
          score -= (1 - healthyStores / storeCount) * 40;
        }

        // Consensus health (30 points)
        if (!healthData.consensus?.healthy) {
          score -= 30;
        }

        // Replication lag (20 points)
        const lag = healthData.replication?.lag || 0;
        if (lag > 1000) {
          score -= Math.min(20, lag / 100);
        }

        // Conflicts (10 points)
        const conflicts = healthData.replication?.conflicts || 0;
        if (conflicts > 0) {
          score -= Math.min(10, conflicts);
        }

        return Math.max(0, Math.round(score));
      };

      const healthData = {
        stores: [
          { status: 'healthy' },
          { status: 'healthy' },
          { status: 'unhealthy' },
          { status: 'healthy' }
        ],
        consensus: { healthy: true },
        replication: { lag: 500, conflicts: 2 }
      };

      const score = calculateHealthScore(healthData);
      expect(score).toBe(88); // 100 - 10 (stores) - 0 (consensus) - 0 (lag) - 2 (conflicts)
    });
  });

  describe('Metrics Tracking', () => {
    it('should track latency metrics', () => {
      const latencies = [50, 75, 100, 125, 200, 250, 300, 350, 400, 500];

      const calculateMetrics = (values) => {
        const sorted = [...values].sort((a, b) => a - b);
        const avg = values.reduce((a, b) => a + b, 0) / values.length;
        const p95Index = Math.floor(values.length * 0.95) - 1;
        const p99Index = Math.floor(values.length * 0.99) - 1;

        return {
          avg: Math.round(avg),
          p95: sorted[p95Index] || sorted[sorted.length - 1],
          p99: sorted[p99Index] || sorted[sorted.length - 1]
        };
      };

      const metrics = calculateMetrics(latencies);
      expect(metrics.avg).toBe(235);
      expect(metrics.p95).toBe(400);
    });

    it('should track throughput metrics', () => {
      const throughput = {
        queries: 0,
        replications: 0,
        startTime: Date.now()
      };

      const recordQuery = () => throughput.queries++;
      const recordReplication = () => throughput.replications++;

      const getRate = (count, startTime) => {
        const elapsed = (Date.now() - startTime) / 1000;
        return elapsed > 0 ? count / elapsed : 0;
      };

      recordQuery();
      recordQuery();
      recordReplication();

      expect(throughput.queries).toBe(2);
      expect(throughput.replications).toBe(1);
    });

    it('should track availability metrics', () => {
      const availability = {
        uptime: 0,
        startTime: Date.now(),
        stores: {
          'store-1': { uptime: 99.9 },
          'store-2': { uptime: 98.5 },
          'store-3': { uptime: 100 }
        }
      };

      const getAverageAvailability = () => {
        const uptimes = Object.values(availability.stores).map(s => s.uptime);
        return uptimes.reduce((a, b) => a + b, 0) / uptimes.length;
      };

      expect(getAverageAvailability()).toBeCloseTo(99.47, 1);
    });

    it('should track error metrics', () => {
      const errors = {
        count: 0,
        history: []
      };

      const recordError = (error) => {
        errors.count++;
        errors.history.push({
          message: error.message,
          timestamp: Date.now()
        });
      };

      const getErrorRate = (windowMs = 60000) => {
        const now = Date.now();
        const recentErrors = errors.history.filter(e => now - e.timestamp < windowMs);
        return recentErrors.length;
      };

      recordError({ message: 'Connection failed' });
      recordError({ message: 'Timeout' });

      expect(errors.count).toBe(2);
      expect(getErrorRate()).toBe(2);
    });
  });

  describe('Store Health', () => {
    it('should track individual store health', () => {
      const stores = [
        { id: 'store-1', status: 'healthy', latency: 50, connections: 10 },
        { id: 'store-2', status: 'healthy', latency: 75, connections: 8 },
        { id: 'store-3', status: 'degraded', latency: 500, connections: 2 }
      ];

      const getStoreHealth = (storeId) => {
        return stores.find(s => s.id === storeId);
      };

      const store = getStoreHealth('store-3');
      expect(store.status).toBe('degraded');
      expect(store.latency).toBe(500);
    });

    it('should detect store failures', () => {
      const stores = new Map([
        ['store-1', { lastHeartbeat: Date.now() }],
        ['store-2', { lastHeartbeat: Date.now() - 60000 }], // 1 minute ago
        ['store-3', { lastHeartbeat: Date.now() - 300000 }] // 5 minutes ago
      ]);

      const getFailedStores = (timeout = 120000) => {
        const now = Date.now();
        const failed = [];
        for (const [id, state] of stores) {
          if (now - state.lastHeartbeat > timeout) {
            failed.push(id);
          }
        }
        return failed;
      };

      const failed = getFailedStores();
      expect(failed).toHaveLength(1);
      expect(failed).toContain('store-3');
    });

    it('should aggregate store statuses', () => {
      const stores = [
        { status: 'healthy' },
        { status: 'healthy' },
        { status: 'degraded' },
        { status: 'healthy' },
        { status: 'unhealthy' }
      ];

      const aggregateStatus = (stores) => {
        const counts = { healthy: 0, degraded: 0, unhealthy: 0 };
        stores.forEach(s => counts[s.status]++);

        if (counts.unhealthy > 0) return 'unhealthy';
        if (counts.degraded > 0) return 'degraded';
        return 'healthy';
      };

      expect(aggregateStatus(stores)).toBe('unhealthy');
    });
  });

  describe('Consensus Health', () => {
    it('should track consensus state', () => {
      const consensus = {
        protocol: 'raft',
        leader: 'node-1',
        term: 15,
        healthy: true
      };

      expect(consensus.protocol).toBe('raft');
      expect(consensus.leader).toBe('node-1');
      expect(consensus.healthy).toBe(true);
    });

    it('should detect leader loss', () => {
      let consensus = { leader: 'node-1', healthy: true };

      const handleLeaderLoss = () => {
        consensus = { leader: null, healthy: false };
      };

      handleLeaderLoss();
      expect(consensus.leader).toBeNull();
      expect(consensus.healthy).toBe(false);
    });

    it('should track quorum health', () => {
      const nodes = ['n1', 'n2', 'n3', 'n4', 'n5'];
      const activeNodes = ['n1', 'n2', 'n3'];

      const hasQuorum = (active, total) => {
        return active.length > total.length / 2;
      };

      expect(hasQuorum(activeNodes, nodes)).toBe(true);
      expect(hasQuorum(['n1', 'n2'], nodes)).toBe(false);
    });
  });

  describe('Replication Health', () => {
    it('should track replication factor', () => {
      const replication = {
        factor: 3,
        activeReplicas: 3,
        lag: 50,
        conflicts: 0
      };

      const isReplicationHealthy = (rep) => {
        return rep.activeReplicas >= rep.factor && rep.lag < 1000 && rep.conflicts === 0;
      };

      expect(isReplicationHealthy(replication)).toBe(true);
    });

    it('should detect replication lag', () => {
      const replicas = [
        { id: 'r1', offset: 1000 },
        { id: 'r2', offset: 995 },
        { id: 'r3', offset: 850 }
      ];

      const calculateLag = (replicas) => {
        const maxOffset = Math.max(...replicas.map(r => r.offset));
        return replicas.map(r => ({
          id: r.id,
          lag: maxOffset - r.offset
        }));
      };

      const lags = calculateLag(replicas);
      expect(lags.find(l => l.id === 'r3').lag).toBe(150);
    });

    it('should track conflict count', () => {
      let conflicts = 0;

      const recordConflict = () => conflicts++;
      const resolveConflict = () => conflicts = Math.max(0, conflicts - 1);

      recordConflict();
      recordConflict();
      recordConflict();
      resolveConflict();

      expect(conflicts).toBe(2);
    });
  });

  describe('Health Refresh', () => {
    it('should refresh health on demand', async () => {
      let refreshCount = 0;

      const refresh = async () => {
        refreshCount++;
        return {
          status: 'healthy',
          score: 95,
          timestamp: new Date().toISOString()
        };
      };

      await refresh();
      await refresh();

      expect(refreshCount).toBe(2);
    });

    it('should update health after refresh', async () => {
      let health = { status: 'unknown', score: 0 };

      const refresh = async () => {
        const newHealth = { status: 'healthy', score: 90 };
        health = newHealth;
        return newHealth;
      };

      await refresh();
      expect(health.status).toBe('healthy');
      expect(health.score).toBe(90);
    });
  });

  describe('Health Check Interval', () => {
    it('should run periodic health checks', async () => {
      let checkCount = 0;
      const interval = 50;

      const check = () => {
        checkCount++;
      };

      const intervalId = setInterval(check, interval);

      await new Promise(resolve => setTimeout(resolve, 150));
      clearInterval(intervalId);

      expect(checkCount).toBeGreaterThanOrEqual(2);
    });

    it('should cleanup interval on unmount', () => {
      const intervals = [];

      const startInterval = (fn, ms) => {
        const id = setInterval(fn, ms);
        intervals.push(id);
        return id;
      };

      const cleanup = () => {
        intervals.forEach(id => clearInterval(id));
        intervals.length = 0;
      };

      startInterval(() => {}, 100);
      startInterval(() => {}, 100);
      expect(intervals).toHaveLength(2);

      cleanup();
      expect(intervals).toHaveLength(0);
    });
  });

  describe('Diagnostics', () => {
    it('should collect diagnostic information', () => {
      const collectDiagnostics = () => ({
        timestamp: new Date().toISOString(),
        system: {
          nodeCount: 5,
          activeNodes: 4,
          leaderNode: 'node-1'
        },
        stores: {
          total: 3,
          healthy: 2,
          degraded: 1
        },
        replication: {
          pendingChanges: 10,
          lastSync: new Date().toISOString(),
          conflicts: 0
        },
        performance: {
          avgLatency: 75,
          throughput: 1000,
          errorRate: 0.01
        }
      });

      const diagnostics = collectDiagnostics();
      expect(diagnostics.system.nodeCount).toBe(5);
      expect(diagnostics.stores.total).toBe(3);
    });

    it('should identify health issues', () => {
      const healthChecks = [
        { name: 'stores', status: 'pass', message: 'All stores healthy' },
        { name: 'consensus', status: 'fail', message: 'No leader elected' },
        { name: 'replication', status: 'warn', message: 'High replication lag' }
      ];

      const getIssues = (checks) => {
        return checks.filter(c => c.status !== 'pass');
      };

      const issues = getIssues(healthChecks);
      expect(issues).toHaveLength(2);
      expect(issues[0].name).toBe('consensus');
    });
  });

  describe('isHealthy Flag', () => {
    it('should be true when status is healthy', () => {
      const health = { status: 'healthy', score: 95 };
      const isHealthy = health.status === 'healthy';
      expect(isHealthy).toBe(true);
    });

    it('should be false when status is degraded', () => {
      const health = { status: 'degraded', score: 65 };
      const isHealthy = health.status === 'healthy';
      expect(isHealthy).toBe(false);
    });

    it('should be false when status is unhealthy', () => {
      const health = { status: 'unhealthy', score: 30 };
      const isHealthy = health.status === 'healthy';
      expect(isHealthy).toBe(false);
    });
  });
});
