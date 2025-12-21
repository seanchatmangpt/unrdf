/**
 * @file Health Endpoint - Kubernetes health and readiness probes
 * @module federation/health
 */

/**
 * @typedef {Object} HealthStatus
 * @property {'healthy' | 'degraded' | 'unhealthy'} status - Overall health status
 * @property {number} uptime - Process uptime in seconds
 * @property {string} version - Package version
 * @property {Object} peers - Peer health summary
 * @property {number} peers.healthy - Number of healthy peers
 * @property {number} peers.total - Total number of peers
 * @property {Object} queries - Query statistics
 * @property {number} queries.total - Total queries executed
 * @property {number} queries.errors - Total query errors
 */

/**
 * @typedef {Object} ReadinessStatus
 * @property {boolean} ready - Whether service is ready to accept traffic
 * @property {number} healthyPeers - Number of healthy peers
 * @property {string} [message] - Optional message
 */

/**
 * Create health check endpoint.
 *
 * @param {Object} coordinator - Federation coordinator instance
 * @returns {Object} Health endpoint handlers
 */
export function createHealthEndpoint(coordinator) {
  const startTime = Date.now();

  return {
    /**
     * Liveness probe - Is the service running?
     *
     * @returns {Promise<HealthStatus>} Health status
     */
    async check() {
      const stats = coordinator.getStats();
      const peers = coordinator.listPeers();

      // Determine overall health status
      let status = 'healthy';
      if (stats.healthyPeers === 0 && stats.totalPeers > 0) {
        status = 'unhealthy'; // No healthy peers available
      } else if (stats.errorRate > 0.5) {
        status = 'degraded'; // High error rate
      }

      return {
        status,
        uptime: Math.floor((Date.now() - startTime) / 1000),
        version: '5.0.1',
        peers: {
          healthy: stats.healthyPeers,
          total: stats.totalPeers,
        },
        queries: {
          total: stats.totalQueries,
          errors: stats.totalErrors,
        },
      };
    },

    /**
     * Readiness probe - Is the service ready to accept traffic?
     *
     * @returns {Promise<ReadinessStatus>} Readiness status
     */
    async ready() {
      const stats = coordinator.getStats();
      const healthyCount = stats.healthyPeers;

      // Service is ready if at least one healthy peer exists
      const isReady = healthyCount > 0;

      return {
        ready: isReady,
        healthyPeers: healthyCount,
        message: isReady ? 'Service ready' : 'No healthy peers available',
      };
    },

    /**
     * Startup probe - Has the service completed initialization?
     * For this implementation, startup is immediate after coordinator creation.
     *
     * @returns {Promise<{started: boolean, uptime: number}>} Startup status
     */
    async startup() {
      return {
        started: true,
        uptime: Math.floor((Date.now() - startTime) / 1000),
      };
    },
  };
}
