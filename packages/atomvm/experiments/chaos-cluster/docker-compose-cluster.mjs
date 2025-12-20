/**
 * Generates a docker-compose.yml content string with a specified number of Node.js services.
 * Each service uses the node:18-alpine image, is assigned a unique container name, and
 * is part of a custom bridge network. Each service also includes a healthcheck with a 5-second interval.
 *
 * @param {number} nodeCount - Number of Node.js services to create in the docker-compose.yml
 * @param {string} networkName - Name of the custom bridge network to use
 * @returns {string} A valid docker-compose.yml content string
 * @throws {Error} If nodeCount is less than 1 or networkName is empty
 * @example
 * const composeYaml = createDockerComposeYaml(3, 'my-bridge-network')
 * console.log(composeYaml)
 */
export function createDockerComposeYaml(nodeCount, networkName) {
  if (nodeCount < 1) {
    throw new Error('nodeCount must be at least 1');
  }
  if (!networkName || typeof networkName !== 'string' || networkName.trim() === '') {
    throw new Error('networkName must be a non-empty string');
  }

  const services = [];
  for (let i = 1; i <= nodeCount; i++) {
    const containerName = `atomvm-node${i}`;
    services.push(`  atomvm-node${i}:`);
    services.push(`    image: node:18-alpine`);
    services.push(`    container_name: ${containerName}`);
    services.push(`    networks:`);
    services.push(`      - ${networkName}`);
    services.push(`    healthcheck:`);
    services.push(`      test: ["CMD", "sh", "-c", "curl -f http://localhost:3000 || exit 1"]`);
    services.push(`      interval: 5s`);
    services.push(`      timeout: 3s`);
    services.push(`      retries: 3`);
    services.push('');
  }

  const composeYaml = `version: '3.8'\n\nservices:\n${services.join('\n')}\nnetworks:\n  ${networkName}:\n    driver: bridge`;
  return composeYaml;
}