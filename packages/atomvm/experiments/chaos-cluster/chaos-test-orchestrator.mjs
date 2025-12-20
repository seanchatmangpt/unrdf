/**
 * Runs a chaos test on a Docker Compose cluster by killing containers and validating recovery.
 * @param {number} nodeCount - Number of nodes in the cluster
 * @param {number} killIterations - Number of containers to kill in the test
 * @param {number} recoveryWaitMs - Time to wait for recovery (in ms)
 * @returns {{totalKills: number, successfulRecoveries: number, failures: number}} Chaos test results
 * @throws {Error} If Docker Compose is not installed or if any step fails
 * @example
 * const result = await runChaosTest(5, 3, 5000)
 * console.log(`Total kills: ${result.totalKills}, Successful recoveries: ${result.successfulRecoveries}, Failures: ${result.failures}`)
 */
export async function runChaosTest(nodeCount, killIterations, recoveryWaitMs) {
  // Validate input
  if (typeof nodeCount !== 'number' || nodeCount <= 0 || !Number.isInteger(nodeCount)) {
    throw new Error('nodeCount must be a positive integer');
  }
  if (typeof killIterations !== 'number' || killIterations <= 0 || !Number.isInteger(killIterations)) {
    throw new Error('killIterations must be a positive integer');
  }
  if (typeof recoveryWaitMs !== 'number' || recoveryWait
    || !Number.isInteger(recoveryWaitMs) || recoveryWaitMs <= 0) {
    throw new Error('recoveryWaitMs must be a positive integer');
  }

  const docker = require('docker-compose');

  // Start Docker Compose cluster
  console.log('Starting Docker Compose cluster...');
  const compose = new docker({
    project: 'chaos-test',
    cwd: './docker-compose'
  });
  await compose.up();
  console.log('Docker Compose cluster started.');

  // Monitor baseline health
  console.log('Monitoring baseline health...');
  const baselineHealth = await checkHealth(nodeCount);
  console.log(`Baseline health: ${baselineHealth} containers healthy`);

  let totalKills = 0;
  let successfulRecoveries = 0;
  let failures = 0;

  // Kill containers in loop
  for (let i = 0; i < killIterations; i++) {
    console.log(`Killing container ${i + 1} of ${killIterations}...`);
    const containers = await compose.list();
    const healthyContainers = containers.filter(c => c.State === 'running');
    if (healthyContainers.length === 0) {
      console.log('No healthy containers to kill. Skipping iteration.');
      continue;
    }
    const randomContainer = healthyContainers[Math.floor(Math.random() * healthyContainers.length)];
    await compose.kill(randomContainer.Id);
    totalKills++;

    // Wait for recovery
    console.log(`Waiting for recovery for ${recoveryWaitMs} ms...`);
    await new Promise(resolve => setTimeout(resolve, recoveryWaitMs));

    // Validate recovery
    console.log('Validating recovery...');
    const recoveryHealth = await checkHealth(nodeCount);
    if (recoveryHealth === nodeCount) {
      successfulRecoveries++;
      console.log('Recovery successful.');
    } else {
      failures++;
      console.log(`Recovery failed. Healthy containers: ${recoveryHealth}`);
    }
  }

  // Stop Docker Compose cluster
  console.log('Stopping Docker Compose cluster...');
  await compose.down();
  console.log('Docker Compose cluster stopped.');

  return { totalKills, successfulRecoveries, failures };
}

/**
 * Checks the health of the Docker Compose cluster.
 * @param {number} expectedHealthy - Expected number of healthy containers
 * @returns {number} Number of healthy containers
 * @throws {Error} If any step fails
 */
async function checkHealth(expectedHealthy) {
  const docker = require('docker-compose');
  const compose = new docker({
    project: 'chaos-test',
    cwd: './docker-compose'
  });

  const containers = await compose.list();
  const healthyContainers = containers.filter(c => c.State === 'running');

  if (healthyContainers.length !== expectedHealthy) {
    throw new Error(`Health check failed: Expected ${expectedHealthy} healthy containers, got ${healthyContainers.length}`);
  }

  return healthyContainers.length;
}