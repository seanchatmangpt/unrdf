/**
 * @file use-federation-health.mjs
 * @description React hook for monitoring federation system health and metrics
 * @since 3.2.0
 */

import { useState, useCallback, useEffect, useRef } from 'react';
import { useFederatedSystem } from './use-federated-system.mjs';

/**
 * Hook for monitoring federated system health, metrics, and diagnostics
 *
 * @since 3.2.0
 * @param {Object} config - Health monitoring configuration
 * @param {number} [config.interval=5000] - Health check interval (ms)
 * @param {string[]} [config.metrics] - Specific metrics to track: 'latency', 'throughput', 'availability'
 * @param {Function} [config.onUnhealthy] - Callback when system becomes unhealthy
 * @returns {Object} Health state and monitoring operations
 * @throws {Error} When refresh fails due to network or system error
 * @performance Health checks run on interval - increase interval to reduce monitoring overhead.
 *   Score calculation is synchronous but lightweight. Store health checks may add network latency.
 *
 * @example
 * // Continuous health monitoring with alerts
 * const {
 *   health,
 *   metrics,
 *   stores,
 *   consensus,
 *   replication,
 *   isHealthy,
 *   refresh
 * } = useFederationHealth({
 *   interval: 5000,
 *   metrics: ['latency', 'throughput', 'availability'],
 *   onUnhealthy: (health) => {
 *     console.warn('Federation unhealthy:', health);
 *   }
 * });
 *
 * @example
 * // Manual health check with store details
 * const { refresh, getStoreHealth } = useFederationHealth({ interval: 0 });
 * await refresh();
 * const store1Health = getStoreHealth('store-1');
 */
export function useFederationHealth(config = {}) {
  const {
    system,
    health: _federationHealth,
    refreshHealth,
  } = useFederatedSystem(config.federation || {});
  const [health, setHealth] = useState({
    status: 'unknown',
    score: 0,
    checks: [],
  });
  const [metrics, setMetrics] = useState({
    latency: { avg: 0, p95: 0, p99: 0 },
    throughput: { queries: 0, replications: 0 },
    availability: { uptime: 0, stores: {} },
    errors: { count: 0, rate: 0 },
  });
  const [stores, setStores] = useState([]);
  const [consensus, setConsensus] = useState({
    protocol: null,
    leader: null,
    term: 0,
    healthy: false,
  });
  const [replication, setReplication] = useState({
    factor: 0,
    lag: 0,
    conflicts: 0,
  });
  const [loading, setLoading] = useState(true);
  const [error, setError] = useState(null);
  const intervalRef = useRef(null);
  const previousHealthRef = useRef(null);

  // Health check loop
  useEffect(() => {
    if (!system) return;

    let mounted = true;

    async function checkHealth() {
      try {
        // Get federation health
        const healthData = await refreshHealth();

        if (!mounted) return;

        // Calculate health score (0-100)
        const score = calculateHealthScore(healthData);

        // Get detailed metrics
        const stats = await system.getStats?.();

        if (!mounted) return;

        const healthState = {
          status: score >= 80 ? 'healthy' : score >= 50 ? 'degraded' : 'unhealthy',
          score,
          checks: healthData.checks || [],
          timestamp: new Date().toISOString(),
        };

        setHealth(healthState);
        setStores(healthData.stores || []);
        setConsensus(healthData.consensus || {});
        setReplication(healthData.replication || {});

        if (stats) {
          setMetrics({
            latency: stats.latency || { avg: 0, p95: 0, p99: 0 },
            throughput: stats.throughput || { queries: 0, replications: 0 },
            availability: stats.availability || { uptime: 0, stores: {} },
            errors: stats.errors || { count: 0, rate: 0 },
          });
        }

        // Check for health state change
        if (previousHealthRef.current?.status === 'healthy' && healthState.status !== 'healthy') {
          config.onUnhealthy?.(healthState);
        }

        previousHealthRef.current = healthState;
        setLoading(false);
      } catch (err) {
        if (!mounted) return;
        setError(err);
        setHealth({
          status: 'error',
          score: 0,
          checks: [{ name: 'system', status: 'error', error: err.message }],
        });
        setLoading(false);
      }
    }

    // Initial check
    checkHealth();

    // Periodic checks
    const interval = config.interval || 5000;
    intervalRef.current = setInterval(checkHealth, interval);

    return () => {
      mounted = false;
      if (intervalRef.current) {
        clearInterval(intervalRef.current);
      }
    };
  }, [system, config.interval, refreshHealth]);

  // Calculate health score
  function calculateHealthScore(healthData) {
    let score = 100;

    // Check store availability (40 points)
    const storeCount = healthData.stores?.length || 0;
    const healthyStores = healthData.stores?.filter((s) => s.status === 'healthy').length || 0;
    if (storeCount > 0) {
      score -= (1 - healthyStores / storeCount) * 40;
    }

    // Check consensus health (30 points)
    if (!healthData.consensus?.healthy) {
      score -= 30;
    }

    // Check replication lag (20 points)
    const replicationLag = healthData.replication?.lag || 0;
    if (replicationLag > 1000) {
      score -= Math.min(20, replicationLag / 100);
    }

    // Check conflicts (10 points)
    const conflicts = healthData.replication?.conflicts || 0;
    if (conflicts > 0) {
      score -= Math.min(10, conflicts);
    }

    return Math.max(0, Math.round(score));
  }

  // Refresh health immediately
  const refresh = useCallback(async () => {
    if (!system) return;

    try {
      setLoading(true);
      const healthData = await refreshHealth();
      const score = calculateHealthScore(healthData);

      setHealth({
        status: score >= 80 ? 'healthy' : score >= 50 ? 'degraded' : 'unhealthy',
        score,
        checks: healthData.checks || [],
      });

      setLoading(false);
      return healthData;
    } catch (err) {
      setError(err);
      setLoading(false);
      throw err;
    }
  }, [system, refreshHealth]);

  // Get specific store health
  const getStoreHealth = useCallback(
    (storeId) => {
      return stores.find((s) => s.id === storeId);
    },
    [stores]
  );

  // Check if system is healthy
  const isHealthy = health.status === 'healthy';

  return {
    health,
    metrics,
    stores,
    consensus,
    replication,
    loading,
    error,
    isHealthy,
    refresh,
    getStoreHealth,
  };
}
