/**
 * Broadcasts a message to multiple nodes using Erlang-style RPC calls.
 * Sends the message to all nodes in the targetNodes list, tracks acknowledgments,
 * and returns an object with the count of sent, acknowledged, and failed messages.
 * Hard crashes if any node fails to receive the message.
 *
 * @param {string} sourceNode - The identifier of the source node sending the message.
 * @param {any} message - The message to be sent to the target nodes.
 * @param {string[]} targetNodes - List of target node identifiers to which the message will be sent.
 * @returns {{sent: number, acked: number, failed: number}} Object containing the counts of sent, acknowledged, and failed messages.
 * @throws {Error} If any node fails to receive the message, the function will throw an error and crash.
 * @example
 * const result = await broadcastMessage('node1', { data: 'hello' }, ['node2', 'node3', 'node4']);
 * console.log(result); // { sent: 3, acked: 2, failed: 1 }
 */
export async function broadcastMessage(sourceNode, message, targetNodes) {
  if (typeof sourceNode !== 'string') {
    throw new Error('sourceNode must be a string');
  }

  if (typeof message !== 'object' && typeof message !== 'string' && typeof message !== 'number') {
    throw new Error('message must be a string, number, or object');
  }

  if (!Array.isArray(targetNodes)) {
    throw new Error('targetNodes must be an array of strings');
  }

  if (targetNodes.length === 0) {
    throw new Error('targetNodes must not be empty');
  }

  const sent = targetNodes.length;
  const acks = new Set();
  const failures = [];

  for (const targetNode of targetNodes) {
    if (typeof targetNode !== 'string') {
      throw new Error(`Invalid target node: ${targetNode}`);
    }

    try {
      // Simulate an RPC call to the target node
      const response = await simulateRpcCall(targetNode, sourceNode, message);
      if (response.ack) {
        acks.add(targetNode);
      } else {
        failures.push(targetNode);
      }
    } catch (err) {
      failures.push(targetNode);
    }
  }

  if (failures.length > 0) {
    throw new Error(`Failed to send message to nodes: ${failures.join(', ')}`);
  }

  return {
    sent,
    acked: acks.size,
    failed: failures.length,
  };
}

/**
 * Simulates an Erlang-style RPC call to a target node.
 * @param {string} targetNode - The identifier of the target node.
 * @param {string} sourceNode - The identifier of the source node.
 * @param {any} message - The message to be sent.
 * @returns {Promise<{ack: boolean}>} A promise that resolves to an object indicating whether the message was acknowledged.
 */
async function simulateRpcCall(targetNode, sourceNode, message) {
  // Simulate a delay to mimic network latency
  await new Promise(resolve => setTimeout(resolve, Math.random() * 500 + 100));

  // Simulate a possible failure (e.g., node is unreachable)
  const success = Math.random() > 0.2;

  if (success) {
    return { ack: true };
  } else {
    throw new Error(`RPC call to ${targetNode} failed`);
  }
}