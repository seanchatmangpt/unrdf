/**
 * Monitors the health of a cluster of containers by polling Docker's `docker ps` command.
 * It checks the health status of each container, tracks uptime, and detects downtime.
 * @param {Array<string>} containers - List of container names or IDs to monitor
 * @param {number} [pollInterval=5000] - Polling interval in milliseconds (default: 5000)
 * @param {number} [maxPolls=60] - Maximum number of polls to perform (default: 60)
 * @returns {Promise<{ total: number, healthy: number, unhealthy: number, downtime: number }>} Health report
 * @throws {Error} If Docker command execution fails or invalid input
 * @example
 * const report = await monitorClusterHealth(['my-app', 'db-container'], 2000, 30)
 * console.log(report)
 * // Output: { total: 2, healthy: 1, unhealthy: 1, downtime: 10 }
 */
export async function monitorClusterHealth(containers, pollInterval = 5000, maxPolls = 60) {
  if (!Array.isArray(containers) || containers.length === 0) {
    throw new Error('Invalid containers list: must be a non-empty array');
  }

  if (typeof pollInterval !== 'number' || pollInterval <= 0) {
    throw new Error('Invalid pollInterval: must be a positive number');
  }

  if (typeof maxPolls !== 'number' || maxPolls <= 0) {
    throw new Error('Invalid maxPolls: must be a positive number');
  }

  const healthReport = {
    total: containers.length,
    healthy: 0,
    unhealthy: 0,
    downtime: 0,
  };

  const containerStatuses = {};

  const poll = async () => {
    try {
      const { stdout } = await exec('docker ps --format "{{.ID}} {{.Status}} {{.Health}}"');
      const lines = stdout.trim().split('\n');

      for (const line of lines) {
        const [id, status, health] = line.split(/\s+/);
        if (containers.includes(id)) {
          containerStatuses[id] = {
            status,
            health,
            uptime: 0,
          };
        }
      }

      const now = Date.now();
      for (const id in containerStatuses) {
        const container = containerStatuses[id];
        const uptime = now - new Date(container.status.match(/Up (\d+) seconds/)?.[1] * 1000 || 0);
        container.uptime = uptime;
      }

      const healthyContainers = Object.values(containerStatuses)
        .filter(c => c.health && c.status.includes('Up'));

      const unhealthyContainers = Object.values(containerStatuses)
        .filter(c => !c.health || !c.status.includes('Up'));

      healthReport.healthy = healthyContainers.length;
      healthReport.unhealthy = unhealthyContainers.length;

      // Track downtime for unhealthy containers
      for (const container of unhealthyContainers) {
        const downtime = now - new Date(container.status.match(/Up (\d+) seconds/)?.[1] * 1000 || 0);
        healthReport.downtime += downtime;
      }

      return healthyContainers.length === containers.length;
    } catch (err) {
      console.error('Failed to poll Docker status:', err.message);
      return false;
    }
  };

  let healthy = false;
  let polls = 0;

  while (polls < maxPolls) {
    healthy = await poll();
    if (healthy) break;
    await new Promise(resolve => setTimeout(resolve, pollInterval));
    polls++;
  }

  return healthReport;
}