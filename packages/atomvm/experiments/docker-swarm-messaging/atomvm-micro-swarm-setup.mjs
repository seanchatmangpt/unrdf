/**
 * Sets up a micro swarm with specified containers, installs AtomVM runtime, starts Erlang nodes, and registers them in the swarm.
 * @param {Array<string>} containers - List of container IDs or names to use for the swarm nodes.
 * @returns {Promise<Array<string>>} List of node names that were successfully registered in the swarm.
 * @throws {Error} If any container setup fails, or if there's an issue starting the Erlang node or registering the node.
 * @example
 * const swarmNodes = await setupMicroSwarm(['container1', 'container2'])
 * console.log('Swarm nodes registered:', swarmNodes)
 */
export async function setupMicroSwarm(containers) {
  if (!Array.isArray(containers) || containers.length === 0) {
    throw new Error('Containers must be a non-empty array of strings.');
  }

  const swarmNodes = [];

  for (const container of containers) {
    try {
      // Step 1: Install AtomVM runtime in the container
      await installAtomVM(container);

      // Step 2: Start Erlang node with -sname flag
      const nodeName = `node_${Date.now()}`;
      await startErlangNode(container, nodeName);

      // Step 3: Register node in the swarm
      await registerNodeInSwarm(nodeName);

      swarmNodes.push(nodeName);
    } catch (err) {
      console.error(`Failed to setup container ${container}:`, err.message);
      throw new Error(`Setup failed for container ${container}`);
    }
  }

  return swarmNodes;
}

/**
 * Installs AtomVM runtime in a specified container.
 * @param {string} container - Container ID or name.
 * @returns {Promise<void>}
 * @throws {Error} If installation fails.
 */
async function installAtomVM(container) {
  // Simulate installation
  if (!container) {
    throw new Error('Container must be a valid string.');
  }
  console.log(`Installing AtomVM in container: ${container}`);
  // In real scenario, this would be a Docker command or API call
  // For example: await exec(`docker exec ${container} apt-get install -y atomvm`);
}

/**
 * Starts an Erlang node with a specified name in a container.
 * @param {string} container - Container ID or name.
 * @param {string} nodeName - Name of the Erlang node.
 * @returns {Promise<void>}
 * @throws {Error} If node start fails.
 */
async function startErlangNode(container, nodeName) {
  if (!container || !nodeName) {
    throw new Error('Container and node name must be valid strings.');
  }
  console.log(`Starting Erlang node ${nodeName} in container: ${container}`);
  // In real scenario, this would be a command to start an Erlang node
  // For example: await exec(`docker exec ${container} erl -sname ${nodeName}`);
}

/**
 * Registers a node in the micro swarm.
 * @param {string} nodeName - Name of the Erlang node.
 * @returns {Promise<void>}
 * @throws {Error} If registration fails.
 */
async function registerNodeInSwarm(nodeName) {
  if (!nodeName) {
    throw new Error('Node name must be a valid string.');
  }
  console.log(`Registering node ${nodeName} in swarm`);
  // In real scenario, this would be a call to a swarm registration API or command
  // For example: await exec(`swarm register ${nodeName}`);
}