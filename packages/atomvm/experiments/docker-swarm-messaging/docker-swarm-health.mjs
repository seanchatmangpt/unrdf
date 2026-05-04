/**
 * Checks the health of a Docker Swarm cluster by executing `docker node ls --format json`
 * and parsing the node status. It counts healthy/unhealthy nodes, verifies quorum, and
 * returns a summary object. If the swarm is unhealthy, it throws an error.
 *
 * @param {string} [dockerPath='/usr/bin/docker'] - Path to the Docker CLI executable
 * @param {number} [quorumSize=2] - Required number of healthy nodes to maintain quorum
 * @throws {Error} If swarm is unhealthy (not enough healthy nodes)
 * @returns {{total: number, healthy: number, unhealthy: number, leaders: number, workers: number}} Summary of swarm health
 * @example
 * const health = await checkSwarmHealth()
 * console.log(health)
 * // { total: 5, healthy: 4, unhealthy: 1, leaders: 2, workers: 3 }
 */
export async function checkSwarmHealth(dockerPath = '/usr/bin/docker', quorumSize = 2) {
  // Validate inputs
  if (typeof dockerPath !== 'string' || dockerPath.trim() === '') {
    throw new Error('Invalid Docker CLI path provided');
  }
  if (typeof quorumSize !== 'number' || quorumSize <= 0) {
    throw new Error('Quorum size must be a positive number');
  }

  // Execute the command
  const command = `${dockerPath} node ls --format json`;
  const { stdout, stderr } = await new Promise((resolve, reject) => {
    const { exec } = require('child_process');
    exec(command, (err, stdout, stderr) => {
      if (err) {
        reject(err);
      } else {
        resolve({ stdout, stderr });
      }
    });
  });

  // Parse the JSON output
  try {
    const nodes = JSON.parse(stdout);
    if (!Array.isArray(nodes)) {
      throw new Error('Unexpected output format from docker node ls');
    }

    // Count healthy, unhealthy, leaders, and workers
    let healthy = 0;
    let unhealthy = 0;
    let leaders = 0;
    let workers = 0;

    for (const node of nodes) {
      const status = node.Status?.Status || 'unknown';
      const role = node.Spec?.Role || 'unknown';

      if (status === 'healthy') {
        healthy++;
      } else {
        unhealthy++;
      }

      if (role === 'leader') {
        leaders++;
      } else if (role === 'worker') {
        workers++;
      }
    }

    const total = healthy + unhealthy;

    // Check quorum
    if (healthy < quorumSize) {
      throw new Error(`Swarm is unhealthy: ${healthy} healthy nodes < required quorum of ${quorumSize}`);
    }

    return {
      total,
      healthy,
      unhealthy,
      leaders,
      workers
    };
  } catch (err) {
    throw new Error(`Failed to parse swarm health: ${err.message}`);
  }
}