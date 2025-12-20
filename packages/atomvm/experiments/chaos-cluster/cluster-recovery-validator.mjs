/**
 * Validates cluster recovery after a container kill. Compares the state before and after the kill to ensure
 * remaining containers are healthy, the killed container has restarted, and there are no cascading failures.
 *
 * @param {Object} beforeState - State of the cluster before the container kill
 * @param {Object} afterState - State of the cluster after the container kill
 * @param {string[]} killedContainers - List of container IDs or names that were killed
 * @returns {{recovered: boolean, failedNodes: string[], cascadingFailures: boolean}} Validation result
 * @throws {Error} If required parameters are missing or invalid
 * @example
 * const beforeState = {
 *   containers: ['container1', 'container2', 'container3'],
 *   health: ['healthy', 'healthy', 'healthy']
 * };
 * const afterState = {
 *   containers: ['container1', 'container2', 'container3', 'container1'],
 *   health: ['healthy', 'healthy', 'healthy', 'healthy']
 * };
 * const result = validateClusterRecovery(beforeState, afterState, ['container1']);
 * // result.recovered === true, result.failedNodes === [], result.cascadingFailures === false
 */
export function validateClusterRecovery(beforeState, afterState, killedContainers) {
  if (!beforeState || !afterState || !killedContainers) {
    throw new Error('Missing required parameters: beforeState, afterState, and killedContainers');
  }

  if (!Array.isArray(killedContainers)) {
    throw new Error('killedContainers must be an array');
  }

  // Check if all killed containers have restarted
  const restartedContainers = killedContainers.filter(container => {
    return afterState.containers.includes(container) && afterState.health.includes('healthy');
  });

  const failedNodes = killedContainers.filter(container => {
    return !afterState.containers.includes(container) || !afterState.health.includes('healthy');
  });

  // Check if any non-killed container is unhealthy (cascading failure)
  const cascadingFailures = afterState.health.some(status => status !== 'healthy') && !killedContainers.some(container => afterState.health.includes('healthy'));

  const recovered = restartedContainers.length === killedContainers.length && failedNodes.length === 0 && !cascadingFailures;

  return {
    recovered,
    failedNodes,
    cascadenFailures: cascadingFailures
  };
}