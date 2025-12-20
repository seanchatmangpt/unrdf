/**
 * Starts a Docker cluster with the specified number of Node.js containers.
 * Each container is based on the `node:18-alpine` image and is named `node1`, `node2`, etc.
 * The containers expose ports 4369 and 9001.
 * @param {number} nodeCount - The number of containers to start.
 * @returns {Promise<StartedTestContainer[]>} An array of StartedTestContainer instances.
 * @throws {Error} If nodeCount is less than 1.
 * @example
 * const nodes = await startDockerCluster(3);
 * console.log(nodes); // Array of StartedTestContainer instances
 */
export async function startDockerCluster(nodeCount) {
  if (nodeCount < 1) {
    throw new Error('nodeCount must be at least 1');
  }

  const containers = [];

  for (let i = 0; i < nodeCount; i++) {
    const containerName = `node${i + 1}`;
    const container = await import('testcontainers').default.GenericContainer.create(
      'node:18-alpine',
      {
        name: containerName,
        ports: [4369, 9001],
      }
    );

    await container.start();
    containers.push(container);
  }

  return containers;
}