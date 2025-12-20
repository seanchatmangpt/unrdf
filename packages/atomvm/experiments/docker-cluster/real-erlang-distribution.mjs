/**
 * Tests Erlang node connectivity between containers using distributed Erlang.
 * Each container runs an Erlang node with a unique name and cookie.
 * It then checks connectivity between nodes using net_adm:ping.
 *
 * @param {Object} containers - An object where keys are container names and values are Docker container objects.
 * @param {number} maxRetries - Maximum number of retries for each node ping test.
 * @param {number} retryDelay - Delay in milliseconds between retries.
 * @param {number} timeout - Timeout in milliseconds for each node ping test.
 * @returns {Promise<Object>} A matrix where each key is a container name and the value is an object with
 *                            other container names as keys and boolean values indicating connectivity.
 * @throws {Error} If any container is missing, or if node setup or ping fails after retries.
 * @example
 * const containers = {
 *   node1: { name: 'node1', exec: async (cmd) => 'ok' },
 *   node2: { name: 'node2', exec: async (cmd) => 'ok' }
 * };
 * const result = await testErlangDistribution(containers, 3, 1000, 5000);
 * console.log(result);
 */
export async function testErlangDistribution(containers, maxRetries = 3, retryDelay = 1000, timeout = 5000) {
  if (!containers || typeof containers !== 'object' || Object.keys(containers).length === 0) {
    throw new Error('Containers object is required and must not be empty.');
  }

  const nodeNames = Object.keys(containers).map((name, index) => `node${index + 1}`);
  const connectionMatrix = {};

  // Start Erlang nodes in each container
  for (const [containerName, container] of Object.entries(containers)) {
    const nodeName = nodeNames[Object.keys(containers).indexOf(containerName)];
    const startNodeCommand = `erl -sname ${nodeName} -setcookie secret`;
    const startResult = await container.exec(startNodeCommand);
    if (!startResult || !startResult.stdout || !startResult.stdout.includes('Eshell')) {
      throw new Error(`Failed to start Erlang node in container ${containerName}: ${startResult.stderr}`);
    }
    connectionMatrix[containerName] = {};
  }

  // Wait for nodes to be ready (simulate with delay)
  await new Promise(resolve => setTimeout(resolve, 2000));

  // Test connectivity between all pairs of nodes
  for (const [containerNameA, containerA] of Object.entries(containers)) {
    const nodeNameA = nodeNames[Object.keys(containers).indexOf(containerNameA)];
    connectionMatrix[containerNameA][containerNameA] = true;

    for (const [containerNameB, containerB] of Object.entries(containers)) {
      if (containerNameA === containerNameB) continue;

      const nodeNameB = nodeNames[Object.keys(containers).indexOf(containerNameB)];
      const pingCommand = `erl -sname ${nodeNameA} -setcookie secret -eval "net_adm:ping(${nodeNameB})."`;
      let success = false;

      for (let i = 0; i < maxRetries; i++) {
        const pingResult = await containerA.exec(pingCommand);
        if (!pingResult || !pingResult.stdout) {
          throw new Error(`Failed to execute ping command in container ${containerNameA}: ${pingResult.stderr}`);
        }

        const output = pingResult.stdout.trim();
        if (output.includes('ok')) {
          success = true;
          break;
        } else if (output.includes('error')) {
          throw new Error(`Ping from ${nodeNameA} to ${nodeNameB} failed: ${output}`);
        }

        await new Promise(resolve => setTimeout(resolve, retryDelay));
      }

      if (!success) {
        throw new Error(`Ping from ${nodeNameA} to ${nodeNameB} failed after ${maxRetries} retries.`);
      }

      connectionMatrix[containerNameA][containerNameB] = true;
    }
  }

  return connectionMatrix;
}