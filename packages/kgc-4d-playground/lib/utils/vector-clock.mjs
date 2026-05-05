/**
 * Vector Clock Utilities - Happened-Before Causality Detection
 *
 * Implements Lamport's happened-before relation for distributed event ordering
 *
 * Vector Clock Format:
 * {
 *   nodeId: "node-abc123",
 *   counters: {
 *     "node-abc123": "5",  // BigInt as string
 *     "node-def456": "3"
 *   }
 * }
 */

/**
 * Check if event A happened-before event B using vector clocks
 *
 * A → B iff for all nodes i: A[i] <= B[i] AND exists j: A[j] < B[j]
 *
 * @param {Object} clockA - Vector clock from event A
 * @param {Object} clockB - Vector clock from event B
 * @returns {boolean} true if A happened-before B
 */
export function happenedBefore(clockA, clockB) {
  if (!clockA || !clockB || !clockA.counters || !clockB.counters) {
    return false;
  }

  const counterA = clockA.counters;
  const counterB = clockB.counters;

  // Get all node IDs from both clocks
  const allNodes = new Set([
    ...Object.keys(counterA),
    ...Object.keys(counterB),
  ]);

  let hasStrictlyLess = false;

  for (const nodeId of allNodes) {
    const a = BigInt(counterA[nodeId] || '0');
    const b = BigInt(counterB[nodeId] || '0');

    // Check A[i] <= B[i] for all i
    if (a > b) {
      return false; // Violates happened-before
    }

    // Track if exists j where A[j] < B[j]
    if (a < b) {
      hasStrictlyLess = true;
    }
  }

  // Must have at least one strictly less component
  return hasStrictlyLess;
}

/**
 * Check if two events are concurrent (neither happened-before the other)
 *
 * @param {Object} clockA - Vector clock from event A
 * @param {Object} clockB - Vector clock from event B
 * @returns {boolean} true if A and B are concurrent
 */
export function isConcurrent(clockA, clockB) {
  return !happenedBefore(clockA, clockB) && !happenedBefore(clockB, clockA);
}

/**
 * Compare two vector clocks for ordering
 *
 * @param {Object} clockA
 * @param {Object} clockB
 * @returns {number} -1 if A < B, 1 if A > B, 0 if concurrent
 */
export function compare(clockA, clockB) {
  if (happenedBefore(clockA, clockB)) return -1;
  if (happenedBefore(clockB, clockA)) return 1;
  return 0; // Concurrent
}

/**
 * Merge two vector clocks (take max of each component)
 *
 * @param {Object} clockA
 * @param {Object} clockB
 * @returns {Object} Merged vector clock
 */
export function merge(clockA, clockB) {
  if (!clockA) return clockB;
  if (!clockB) return clockA;

  const allNodes = new Set([
    ...Object.keys(clockA.counters || {}),
    ...Object.keys(clockB.counters || {}),
  ]);

  const merged = {
    nodeId: clockA.nodeId || clockB.nodeId,
    counters: {},
  };

  for (const nodeId of allNodes) {
    const a = BigInt(clockA.counters?.[nodeId] || '0');
    const b = BigInt(clockB.counters?.[nodeId] || '0');
    merged.counters[nodeId] = (a > b ? a : b).toString();
  }

  return merged;
}

/**
 * Increment a vector clock for this node
 *
 * @param {Object} clock - Current vector clock
 * @param {string} nodeId - This node's ID
 * @returns {Object} Incremented vector clock
 */
export function increment(clock, nodeId) {
  const counters = { ...(clock?.counters || {}) };
  const current = BigInt(counters[nodeId] || '0');
  counters[nodeId] = (current + 1n).toString();

  return {
    nodeId,
    counters,
  };
}

/**
 * Format vector clock for display
 *
 * @param {Object} clock
 * @returns {string} Formatted string like "node-abc:5, node-def:3"
 */
export function format(clock) {
  if (!clock || !clock.counters) return '—';

  return Object.entries(clock.counters)
    .map(([nodeId, count]) => {
      const short = nodeId.slice(0, 8);
      return `${short}:${count}`;
    })
    .join(', ');
}
