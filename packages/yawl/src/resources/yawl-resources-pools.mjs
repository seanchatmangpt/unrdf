/**
 * @fileoverview YAWL Resource Pools - Pool Allocation Strategies
 *
 * Resource pool implementations with allocation strategies:
 * - ResourcePool class
 * - Round-robin allocation
 * - Priority-based allocation
 * - Random allocation
 * - Resource factory functions
 *
 * @module @unrdf/yawl/resources/pools
 * @version 1.0.0
 */

import { z } from 'zod';
import {
  ResourceSchema,
  WorkItemSchema,
  PolicyPackSchema,
  ResourceType,
} from './yawl-resources-core.mjs';

/* ========================================================================= */
/* Resource Pool Class                                                       */
/* ========================================================================= */

/**
 * Resource Pool - Bundles resources with allocation strategies
 *
 * @class
 */
export class ResourcePool {
  /**
   * Parent resource manager
   * @private
   * @type {import('./yawl-resources-core.mjs').YawlResourceManager}
   */
  #manager;

  /**
   * Pool configuration
   * @private
   * @type {Object}
   */
  #config;

  /**
   * Round-robin index
   * @private
   * @type {number}
   */
  #roundRobinIndex;

  /**
   * Create a resource pool
   *
   * @param {import('./yawl-resources-core.mjs').YawlResourceManager} manager - Parent manager
   * @param {Object} config - Pool configuration
   */
  constructor(manager, config) {
    this.#manager = manager;
    this.#config = config;
    this.#roundRobinIndex = 0;
  }

  /**
   * Get pool ID
   * @returns {string}
   */
  get id() {
    return this.#config.id;
  }

  /**
   * Get pool name
   * @returns {string|undefined}
   */
  get name() {
    return this.#config.name;
  }

  /**
   * Get pool resources
   * @returns {import('./yawl-resources-core.mjs').Resource[]}
   */
  get resources() {
    return [...this.#config.resources];
  }

  /**
   * Allocate any available resource from pool
   *
   * @param {import('./yawl-resources-core.mjs').WorkItem} workItem - Work item to allocate for
   * @param {Object} [options] - Allocation options
   * @returns {Promise<import('./yawl-resources-core.mjs').AllocationReceipt|null>}
   */
  async allocateAny(workItem, options = {}) {
    const orderedResources = this.#getOrderedResources();

    for (const resource of orderedResources) {
      try {
        const receipt = await this.#manager.allocateResource(workItem, resource, options);
        return receipt;
      } catch {
        continue;
      }
    }

    return null;
  }

  /**
   * Get available resources in pool (with capacity)
   *
   * @returns {import('./yawl-resources-core.mjs').Resource[]}
   */
  getAvailableResources() {
    return this.#config.resources.filter(resource => {
      const status = this.#manager.getCapacityStatus(resource.id);
      return status.max === -1 || status.available > 0;
    });
  }

  /**
   * Get pool availability status
   *
   * @returns {{ available: boolean, availableCount: number, totalCount: number }}
   */
  getAvailability() {
    const available = this.getAvailableResources();
    return {
      available: available.length > 0,
      availableCount: available.length,
      totalCount: this.#config.resources.length,
    };
  }

  /**
   * Get resources ordered by allocation strategy
   * @private
   * @returns {import('./yawl-resources-core.mjs').Resource[]}
   */
  #getOrderedResources() {
    const resources = [...this.#config.resources];

    switch (this.#config.allocationStrategy) {
      case 'round-robin':
        const rotated = [
          ...resources.slice(this.#roundRobinIndex),
          ...resources.slice(0, this.#roundRobinIndex),
        ];
        this.#roundRobinIndex = (this.#roundRobinIndex + 1) % resources.length;
        return rotated;

      case 'random':
        for (let i = resources.length - 1; i > 0; i--) {
          const j = Math.floor(Math.random() * (i + 1));
          [resources[i], resources[j]] = [resources[j], resources[i]];
        }
        return resources;

      case 'priority':
      default:
        return resources;
    }
  }
}

/* ========================================================================= */
/* Pool Factory Function                                                     */
/* ========================================================================= */

/**
 * Create a resource pool for a manager
 *
 * @param {import('./yawl-resources-core.mjs').YawlResourceManager} manager - Resource manager
 * @param {Object} config - Pool configuration
 * @param {string} config.id - Pool identifier
 * @param {string} [config.name] - Pool display name
 * @param {import('./yawl-resources-core.mjs').Resource[]} config.resources - Resources in pool
 * @param {string} [config.allocationStrategy] - 'round-robin' | 'priority' | 'random'
 * @returns {ResourcePool}
 */
export function createPoolForManager(manager, config) {
  const poolConfig = {
    id: z.string().min(1).parse(config.id),
    name: config.name,
    resources: config.resources.map(r => ResourceSchema.parse(r)),
    allocationStrategy: config.allocationStrategy || 'priority',
  };

  const pool = new ResourcePool(manager, poolConfig);
  manager._registerPool(poolConfig.id, pool);
  manager._storeResourcePoolRDF(poolConfig);

  return pool;
}

/* ========================================================================= */
/* Resource Factory Functions                                                */
/* ========================================================================= */

/**
 * Create a Participant resource definition
 *
 * @param {Object} config - Participant configuration
 * @param {string} config.id - Participant identifier
 * @param {string} [config.name] - Participant name
 * @param {number} [config.capacity=1] - Maximum concurrent allocations
 * @param {string} [config.sparql] - Eligibility SPARQL query
 * @returns {import('./yawl-resources-core.mjs').Resource}
 *
 * @example
 * const participant = createParticipant({
 *   id: 'alice',
 *   name: 'Alice Smith',
 *   capacity: 1
 * });
 */
export function createParticipant(config) {
  return ResourceSchema.parse({
    id: config.id,
    type: ResourceType.PARTICIPANT,
    name: config.name,
    capacity: config.capacity ?? 1,
    sparql: config.sparql,
    metadata: config.metadata,
  });
}

/**
 * Create a Tool resource definition
 *
 * @param {Object} config - Tool configuration
 * @param {string} config.id - Tool identifier
 * @param {string} [config.name] - Tool name
 * @param {number} [config.capacity=-1] - Maximum concurrent allocations (-1 = unlimited)
 * @param {string} [config.sparql] - Eligibility SPARQL query
 * @returns {import('./yawl-resources-core.mjs').Resource}
 *
 * @example
 * const tool = createTool({
 *   id: 'email-service',
 *   name: 'Email Service',
 *   capacity: -1
 * });
 */
export function createTool(config) {
  return ResourceSchema.parse({
    id: config.id,
    type: ResourceType.TOOL,
    name: config.name,
    capacity: config.capacity ?? -1,
    sparql: config.sparql,
    metadata: config.metadata,
  });
}

/**
 * Create a Role resource definition
 *
 * @param {Object} config - Role configuration
 * @param {string} config.id - Role identifier
 * @param {string} [config.name] - Role name
 * @param {number} [config.capacity=-1] - Maximum concurrent allocations
 * @param {string} [config.sparql] - Eligibility SPARQL query (for member checking)
 * @returns {import('./yawl-resources-core.mjs').Resource}
 *
 * @example
 * const role = createRole({
 *   id: 'approvers',
 *   name: 'Approval Team',
 *   sparql: 'ASK { ?person foaf:hasRole ?role }'
 * });
 */
export function createRole(config) {
  return ResourceSchema.parse({
    id: config.id,
    type: ResourceType.ROLE,
    name: config.name,
    capacity: config.capacity ?? -1,
    sparql: config.sparql,
    metadata: config.metadata,
  });
}

/**
 * Create a policy pack with resource rules
 *
 * @param {Object} config - Policy pack configuration
 * @param {string} config.id - Policy pack identifier
 * @param {string} [config.name] - Policy pack name
 * @param {import('./yawl-resources-core.mjs').Resource[]} config.resources - Resource definitions
 * @param {number} [config.priority=0] - Priority (higher = more important)
 * @param {boolean} [config.enabled=true] - Whether pack is enabled
 * @returns {import('./yawl-resources-core.mjs').PolicyPack}
 *
 * @example
 * const policyPack = createPolicyPack({
 *   id: 'approval-workflow',
 *   name: 'Approval Workflow Resources',
 *   resources: [
 *     createRole({ id: 'approvers' }),
 *     createParticipant({ id: 'manager', capacity: 1 })
 *   ],
 *   priority: 10
 * });
 */
export function createPolicyPack(config) {
  return PolicyPackSchema.parse({
    id: config.id,
    name: config.name,
    version: config.version,
    resources: config.resources,
    priority: config.priority ?? 0,
    enabled: config.enabled ?? true,
  });
}

/* ========================================================================= */
/* Pool Strategy Helpers                                                     */
/* ========================================================================= */

/**
 * Round-robin allocation - distributes allocations evenly across resources
 *
 * @param {import('./yawl-resources-core.mjs').Resource[]} resources - Available resources
 * @param {number} index - Current round-robin index
 * @returns {{ resource: import('./yawl-resources-core.mjs').Resource, nextIndex: number }}
 */
export function selectRoundRobin(resources, index) {
  if (resources.length === 0) {
    throw new Error('No resources available for round-robin selection');
  }

  const selectedIndex = index % resources.length;
  const nextIndex = (index + 1) % resources.length;

  return {
    resource: resources[selectedIndex],
    nextIndex,
  };
}

/**
 * Priority allocation - selects first available resource by priority order
 *
 * @param {import('./yawl-resources-core.mjs').Resource[]} resources - Available resources (pre-sorted by priority)
 * @returns {import('./yawl-resources-core.mjs').Resource}
 */
export function selectByPriority(resources) {
  if (resources.length === 0) {
    throw new Error('No resources available for priority selection');
  }

  return resources[0];
}

/**
 * Random allocation - selects random resource from available pool
 *
 * @param {import('./yawl-resources-core.mjs').Resource[]} resources - Available resources
 * @returns {import('./yawl-resources-core.mjs').Resource}
 */
export function selectRandom(resources) {
  if (resources.length === 0) {
    throw new Error('No resources available for random selection');
  }

  const randomIndex = Math.floor(Math.random() * resources.length);
  return resources[randomIndex];
}

/**
 * Weighted allocation - selects resource based on capacity weights
 *
 * @param {import('./yawl-resources-core.mjs').Resource[]} resources - Available resources
 * @returns {import('./yawl-resources-core.mjs').Resource}
 */
export function selectWeightedByCapacity(resources) {
  if (resources.length === 0) {
    throw new Error('No resources available for weighted selection');
  }

  const totalCapacity = resources.reduce((sum, r) => {
    return sum + (r.capacity === -1 ? 1000 : r.capacity);
  }, 0);

  let random = Math.random() * totalCapacity;

  for (const resource of resources) {
    const weight = resource.capacity === -1 ? 1000 : resource.capacity;
    random -= weight;
    if (random <= 0) {
      return resource;
    }
  }

  return resources[resources.length - 1];
}
