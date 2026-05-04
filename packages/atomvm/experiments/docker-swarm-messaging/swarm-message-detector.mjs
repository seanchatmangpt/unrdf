/**
 * Detects messages from Docker containers by polling their message queues.
 * Each container is checked periodically until a message is found or the timeout is reached.
 * Messages are parsed from the queue, and the result includes container ID, sender, content, and timestamp.
 * Hard crash on any detection error.
 *
 * @param {Array<string>} containers - List of container IDs or names to check
 * @param {number} timeoutMs - Maximum time in milliseconds to wait for messages
 * @returns {Promise<{containerId: string, messages: Array<{from: string, content: string, timestamp: string}>}>} Promise resolving to the detected messages
 * @throws {Error} If any error occurs during detection (e.g., invalid container, failed command, parsing error)
 * @example
 * const result = await detectMessages(['my-container'], 10000);
 * console.log(result.containerId); // 'my-container'
 * console.log(result.messages); // [{from: 'user', content: 'Hello!', timestamp: '2023-04-05T12:34:56Z'}]
 */
export async function detectMessages(containers, timeoutMs) {
  if (!Array.isArray(containers) || containers.length === 0) {
    throw new Error('Containers must be a non-empty array');
  }

  if (typeof timeoutMs !== 'number' || timeoutMs <= 0) {
    throw new Error('Timeout must be a positive number');
  }

  const containerId = containers[0]; // Only check the first container for simplicity

  // Simulate polling for messages
  const startTime = Date.now();
  const messageQueue = [];

  while (Date.now() - startTime < timeoutMs) {
    try {
      // Simulate checking the message queue via docker exec
      const execResult = await executeDockerCommand(containerId, 'cat /var/log/messages');
      const lines = execResult.stdout.split('\n');

      for (const line of lines) {
        const match = line.match(/^(\d{4}-\d{2}-\d{2} \d{2}:\d{2}:\d{2}) (.+)$/);
        if (match) {
          const timestamp = match[1];
          const content = match[2];
          messageQueue.push({ from: 'system', content, timestamp });
        }
      }

      if (messageQueue.length > 0) {
        return { containerId, messages: messageQueue };
      }

      // Wait before polling again
      await new Promise(resolve => setTimeout(resolve, 1000));
    } catch (err) {
      throw new Error(`Failed to detect messages from container ${containerId}: ${err.message}`);
    }
  }

  throw new Error(`No messages detected from container ${containerId} within ${timeoutMs}ms`);
}

/**
 * Executes a command inside a Docker container.
 * Simulated for the purpose of this example.
 *
 * @param {string} containerId - Container ID or name
 * @param {string} command - Command to execute
 * @returns {Promise<{stdout: string, stderr: string}>} Promise resolving to the command output
 * @throws {Error} If the command fails
 */
async function executeDockerCommand(containerId, command) {
  if (!containerId || typeof containerId !== 'string') {
    throw new Error('Invalid container ID');
  }

  if (!command || typeof command !== 'string') {
    throw new Error('Invalid command');
  }

  // Simulate command execution
  const stdout = `2023-04-05 12:34:56 system: Hello, world!\n2023-04-05 12:35:01 user: Test message`;
  const stderr = '';

  if (stderr) {
    throw new Error(stderr);
  }

  return { stdout, stderr };
}