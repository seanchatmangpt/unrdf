/**
 * Cleans up a Docker cluster by stopping all containers, removing the network, and verifying cleanup.
 * @param {StartedTestContainer[]} containers - Array of StartedTestContainer objects
 * @param {string} networkName - Optional name of the Docker network to remove
 * @returns {Promise<{success: boolean, message: string}>} Cleanup status
 * @throws {Error} If any container fails to stop or network removal fails
 * @example
 * const containers = [
 *   { id: 'abc123', stop: jest.fn().mockResolvedValue(true) },
 *   { id: 'def456', stop: jest.fn().mockResolvedValue(true) }
 * ];
 * const result = await cleanupDockerCluster(containers, 'test-network');
 * // result.success === true
 */
export async function cleanupDockerCluster(containers, networkName) {
  if (!Array.isArray(containers)) {
    throw new Error('Containers must be an array');
  }

  if (networkName && typeof networkName !== 'string') {
    throw new Error('Network name must be a string or undefined');
  }

  const stopPromises = containers.map(container => {
    if (!container || !container.stop) {
      throw new Error(`Invalid container object: ${JSON.stringify(container)}`);
    }
    return container.stop();
  });

  try {
    await Promise.all(stopPromises);
    console.log('All containers stopped successfully.');
  } catch (error) {
    throw new Error(`Failed to stop containers: ${error.message}`);
  }

  if (networkName) {
    try {
      await removeDockerNetwork(networkName);
      console.log(`Docker network "${networkName}" removed.`);
    } catch (error) {
      throw new Error(`Failed to remove Docker network "${networkName}": ${error.message}`);
    }
  }

  const allStopped = await Promise.all(containers.map(container => {
    return container.isStopped();
  }));

  if (!allStopped.every(status => status)) {
    throw new Error('Not all containers were stopped successfully.');
  }

  return { success: true, message: 'Docker cluster cleaned up successfully.' };
}

/**
 * Removes a Docker network by name.
 * @param {string} networkName - Name of the Docker network
 * @returns {Promise<void>} Promise that resolves when the network is removed
 * @throws {Error} If network removal fails
 * @example
 * await removeDockerNetwork('test-network');
 */
export async function removeDockerNetwork(networkName) {
  if (typeof networkName !== 'string' || networkName.trim() === '') {
    throw new Error('Network name must be a non-empty string');
  }

  // Simulate Docker network removal
  // In real use, this would call a Docker API or CLI command
  console.log(`Removing Docker network "${networkName}"...`);
  // Simulate async operation
  await new Promise(resolve => setTimeout(resolve, 500));
  console.log(`Docker network "${networkName}" removed.`);
}