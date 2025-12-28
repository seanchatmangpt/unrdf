/**
 * @fileoverview Multi-Swarm System - Hierarchical agent coordination
 *
 * **Purpose**: Enable large-scale agent coordination through hierarchical swarm topology
 *
 * **Architecture**:
 * ```
 * Queen Swarm (Meta-orchestrator)
 *     ├── Worker Swarm 1 (Domain: Compression)
 *     │   ├── Agent 1
 *     │   ├── Agent 2
 *     │   └── Agent N
 *     ├── Worker Swarm 2 (Domain: Validation)
 *     │   ├── Agent 1
 *     │   └── Agent N
 *     └── Worker Swarm N (Domain: Custom)
 * ```
 *
 * **Features**:
 * - Hierarchical coordination (queen → workers → agents)
 * - Inter-swarm messaging with message queues
 * - Work stealing for load balancing
 * - Nested receipt chains for audit trails
 * - Fault isolation (failures don't cascade)
 * - Domain-specific swarms
 *
 * **Use Cases**:
 * 1. Large-scale observable processing (partition across swarms)
 * 2. Specialized processing (compression swarm + validation swarm)
 * 3. Fault tolerance (isolate failures to individual swarms)
 * 4. Load balancing (distribute work based on capacity)
 *
 * @module multi-swarm
 */

// Core components
export {
  CoordinationHub,
  MessageQueue,
  WorkDistributor,
  MessageType,
  SwarmStatus,
  createCoordinationHub
} from './coordination.mjs';

export {
  WorkerSwarm,
  Agent,
  AgentStatus,
  WorkItemSchema,
  createWorkerSwarm
} from './worker-swarm.mjs';

export {
  QueenSwarm,
  JobSchema,
  JobStatus,
  createQueenSwarm
} from './queen.mjs';

/**
 * Create a complete multi-swarm system
 *
 * @param {Object} config - System configuration
 * @param {Array<Object>} config.swarms - Swarm configurations
 * @param {Object} [config.queenOptions] - Queen swarm options
 * @returns {Promise<QueenSwarm>} Configured queen swarm
 *
 * @example
 * const system = await createMultiSwarmSystem({
 *   swarms: [
 *     {
 *       id: 'compression-swarm',
 *       domain: 'compression',
 *       capacity: 10,
 *       agents: [
 *         { id: 'agent-1', processor: compressData },
 *         { id: 'agent-2', processor: compressData }
 *       ]
 *     },
 *     {
 *       id: 'validation-swarm',
 *       domain: 'validation',
 *       capacity: 5,
 *       agents: [
 *         { id: 'agent-1', processor: validateData }
 *       ]
 *     }
 *   ],
 *   queenOptions: {
 *     heartbeatInterval: 5000
 *   }
 * });
 *
 * await system.start();
 *
 * const result = await system.submitJob({
 *   type: 'process-batch',
 *   payload: [...data],
 *   partitionStrategy: 'domain'
 * });
 */
export async function createMultiSwarmSystem(config) {
  const { QueenSwarm } = await import('./queen.mjs');
  const { WorkerSwarm } = await import('./worker-swarm.mjs');

  const queen = new QueenSwarm(config.queenOptions || {});

  // Create and configure worker swarms
  for (const swarmConfig of config.swarms) {
    const workerSwarm = new WorkerSwarm(swarmConfig.id, {
      domain: swarmConfig.domain,
      capacity: swarmConfig.capacity,
      ...swarmConfig.options
    });

    // Add agents to swarm
    for (const agentConfig of swarmConfig.agents || []) {
      workerSwarm.addAgent(agentConfig.id, agentConfig.processor);
    }

    queen.addWorkerSwarm(workerSwarm);
  }

  return queen;
}

/**
 * Common partition strategies for jobs
 */
export const PartitionStrategies = {
  /**
   * Partition by domain (one partition per swarm domain)
   */
  DOMAIN: 'domain',

  /**
   * Round-robin distribution across swarms
   */
  ROUND_ROBIN: 'round-robin',

  /**
   * Send to least loaded swarm
   */
  LEAST_LOADED: 'least-loaded'
};

/**
 * Common aggregation strategies for results
 */
export const AggregationStrategies = {
  /**
   * Concatenate array results
   */
  CONCAT: 'concat',

  /**
   * Merge object results
   */
  MERGE: 'merge',

  /**
   * Custom reduce function
   */
  REDUCE: 'reduce'
};
