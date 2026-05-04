/**
 * Creates a Docker network and connects containers to it, then verifies network connectivity between containers.
 * @param {Array<string>} containers - Array of container names or IDs to connect to the network
 * @returns {Promise<{networkName: string, containerIPs: Array<string>, pingResults: Array<{container: string, success: boolean}>}>}
 * @throws {Error} If any container is not found, or if network creation or ping execution fails
 * @example
 * const clusterInfo = await createClusterNetwork(['container1', 'container2', 'container3']);
 * console.log(clusterInfo);
 */
export async function createClusterNetwork(containers) {
  if (!Array.isArray(containers) || containers.length < 2) {
    throw new Error('At least two containers are required to verify network connectivity.');
  }

  const networkName = `cluster-network-${Date.now()}`;
  const docker = require('dockerode')();
  const client = docker.getNetwork(networkName);

  try {
    // Create the network
    await client.create({
      Driver: 'bridge',
      Name: networkName
    });

    // Connect containers to the network
    for (const container of containers) {
      const c = docker.getContainer(container);
      await c.modem.getNetworkClient().connect(networkName);
    }

    // Get container IPs
    const containerIPs = [];
    for (const container of containers) {
      const c = docker.getContainer(container);
      const info = await c.inspect();
      containerIPs.push(info.NetworkSettings.Networks[networkName].IPAddress);
    }

    // Ping between containers
    const pingResults = [];
    for (let i = 0; i < containers.length; i++) {
      const container = containers[i];
      const target = containers[(i + 1) % containers.length];
      const c = docker.getContainer(container);

      const exec = await c.exec({
        Cmd: ['ping', '-c', '1', target],
        AttachStdout: true,
        AttachStderr: true
      });

      const result = await exec.start({ stdout: true, stderr: true });
      const output = result.stdout.toString() + result.stderr.toString();
      const success = output.includes('bytes from');
      pingResults.push({ container, success });
    }

    return {
      networkName,
      containerIPs,
      pingResults
    };
  } catch (err) {
    throw new Error(`Failed to create or connect network: ${err.message}`);
  } finally {
    // Clean up network if it was created
    try {
      if (client) await client.remove();
    } catch (e) {
      console.error('Error removing network:', e.message);
    }
  }
}