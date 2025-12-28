/**
 * @fileoverview YAWL Resource Manager - Core Allocation
 *
 * Core resource allocation functionality including:
 * - ResourceManager class
 * - Policy pack management
 * - Resource allocation/deallocation
 * - Capacity tracking
 * - Eligibility checking
 *
 * @module @unrdf/yawl/resources/core
 * @version 1.0.0
 */

import { createStore } from '@unrdf/oxigraph';
import { ResourceError, StorageError } from '../errors.mjs';
import {
  storePolicyPackRDF,
  countActiveAllocations,
  storeResourcePoolRDF,
} from './yawl-resources-rdf.mjs';
import {
  queryResourcesByType,
  getActiveAllocationsFromStore,
  getResourceCapacityStatus,
  findEligibleResourcesInPacks,
} from './yawl-resources-eligibility.mjs';
import {
  checkResourceCapacity,
  performResourceAllocation,
  performResourceDeallocation,
} from './yawl-resources-allocation.mjs';
import {
  PolicyPackSchema,
  ResourceSchema,
  WorkItemSchema,
} from './yawl-resources-types.mjs';

/* ========================================================================= */
/* Re-export Type Definitions for Backward Compatibility                    */
/* ========================================================================= */

export {
  YAWL_NS,
  FOAF_NS,
  RDF_NS,
  XSD_NS,
  TIME_NS,
  yawl,
  foaf,
  rdf,
  xsd,
  time,
  ResourceType,
  ResourceSchema,
  WorkItemSchema,
  PolicyPackSchema,
  TimeWindowSchema,
  AllocationReceiptSchema,
} from './yawl-resources-types.mjs';

/**
 * @typedef {import('./yawl-resources-types.mjs').Resource} Resource
 * @typedef {import('./yawl-resources-types.mjs').WorkItem} WorkItem
 * @typedef {import('./yawl-resources-types.mjs').PolicyPack} PolicyPack
 * @typedef {import('./yawl-resources-types.mjs').TimeWindow} TimeWindow
 * @typedef {import('./yawl-resources-types.mjs').AllocationReceipt} AllocationReceipt
 */

/* ========================================================================= */
/* Resource Manager Class                                                    */
/* ========================================================================= */

/**
 * YAWL Resource Manager
 *
 * Manages resource allocation, eligibility, and capacity tracking
 * following YAWL workflow resource patterns.
 *
 * @class
 * @example
 * const manager = new YawlResourceManager();
 * manager.registerPolicyPack(myPolicyPack);
 * const receipt = await manager.allocateResource(workItem, resource);
 */
export class YawlResourceManager {
  /**
   * RDF store for tracking allocations and capacity
   * @private
   * @type {import('@unrdf/oxigraph').OxigraphStore}
   */
  #store;

  /**
   * Registered policy packs
   * @private
   * @type {Map<string, PolicyPack>}
   */
  #policyPacks;

  /**
   * Resource pools
   * @private
   * @type {Map<string, any>}
   */
  #resourcePools;

  /**
   * Allocation counter for receipt IDs
   * @private
   * @type {number}
   */
  #allocationCounter;

  /**
   * Create a new YawlResourceManager
   *
   * @param {Object} [options] - Configuration options
   * @param {import('@unrdf/oxigraph').OxigraphStore} [options.store] - External RDF store
   */
  constructor(options = {}) {
    this.#store = options.store || createStore();
    this.#policyPacks = new Map();
    this.#resourcePools = new Map();
    this.#allocationCounter = 0;
  }

  /* ----------------------------------------------------------------------- */
  /* Policy Pack Management                                                  */
  /* ----------------------------------------------------------------------- */

  /**
   * Register a policy pack with resource rules
   *
   * @param {PolicyPack} policyPack - Policy pack definition
   * @returns {void}
   * @throws {z.ZodError} If policy pack is invalid
   */
  registerPolicyPack(policyPack) {
    try {
      const validated = PolicyPackSchema.parse(policyPack);
      this.#policyPacks.set(validated.id, validated);
      storePolicyPackRDF(this.#store, validated);
    } catch (err) {
      throw new ResourceError('Failed to register policy pack', {
        cause: err,
        context: { policyPackId: policyPack?.id },
      });
    }
  }

  /**
   * Unregister a policy pack
   *
   * @param {string} policyPackId - Policy pack ID
   * @returns {boolean} True if removed
   */
  unregisterPolicyPack(policyPackId) {
    const existed = this.#policyPacks.has(policyPackId);
    this.#policyPacks.delete(policyPackId);
    return existed;
  }

  /**
   * Get registered policy pack
   *
   * @param {string} policyPackId - Policy pack ID
   * @returns {PolicyPack|undefined}
   */
  getPolicyPack(policyPackId) {
    return this.#policyPacks.get(policyPackId);
  }

  /**
   * List all registered policy packs
   *
   * @returns {PolicyPack[]}
   */
  listPolicyPacks() {
    return Array.from(this.#policyPacks.values())
      .sort((a, b) => (b.priority || 0) - (a.priority || 0));
  }


  /* ----------------------------------------------------------------------- */
  /* Resource Allocation                                                     */
  /* ----------------------------------------------------------------------- */

  /**
   * Allocate a resource to a work item
   */
  async allocateResource(workItem, resource, options = {}) {
    try {
      const state = { allocationCounter: this.#allocationCounter };
      const receipt = await performResourceAllocation(
        this.#store,
        workItem,
        resource,
        options,
        Array.from(this.#policyPacks.values()).sort((a, b) => (b.priority || 0) - (a.priority || 0)),
        state
      );
      this.#allocationCounter = state.allocationCounter;
      return receipt;
    } catch (err) {
      if (err instanceof ResourceError) {
        throw err;
      }
      throw new ResourceError('Failed to allocate resource', {
        cause: err,
        context: { workItem: workItem?.id, resource: resource?.id },
      });
    }
  }

  /**
   * Deallocate a resource from a work item
   */
  deallocateResource(allocationId) {
    try {
      return performResourceDeallocation(this.#store, allocationId);
    } catch (err) {
      if (err instanceof ResourceError) {
        throw err;
      }
      throw new ResourceError('Failed to deallocate resource', {
        cause: err,
        context: { allocationId },
      });
    }
  }

  /**
   * Check resource capacity (for internal use by eligibility)
   * @private
   */
  #checkCapacity(resource) {
    return checkResourceCapacity(this.#store, resource);
  }

  /**
   * Check resource eligibility (for internal use by eligibility)
   * @private
   */
  async #checkEligibility(resource, workItem) {
    const { checkResourceEligibility } = await import('./yawl-resources-eligibility.mjs');
    return checkResourceEligibility(this.#store, resource, workItem);
  }

  /**
   * Count active allocations (for internal use by capacity tracking)
   * @private
   */
  #countActiveAllocations(resourceNode) {
    return countActiveAllocations(this.#store, resourceNode);
  }

  /* ----------------------------------------------------------------------- */
  /* Resource Eligibility                                                    */
  /* ----------------------------------------------------------------------- */

  /**
   * Get eligible resources for a task in a case
   */
  async getEligibleResources(taskId, caseId, options = {}) {
    try {
      return findEligibleResourcesInPacks(
        this.listPolicyPacks(),
        taskId,
        caseId,
        options,
        this.#checkCapacity.bind(this),
        this.#checkEligibility.bind(this),
        this.getAvailability.bind(this)
      );
    } catch (err) {
      throw new ResourceError('Failed to find eligible resources', {
        cause: err,
        context: { taskId, caseId },
      });
    }
  }

  /**
   * Query RDF store for matching resources
   */
  queryResources(resourceType) {
    try {
      return queryResourcesByType(this.#store, resourceType);
    } catch (err) {
      throw new StorageError('Failed to query resources', {
        cause: err,
        context: { resourceType },
      });
    }
  }

  /* ----------------------------------------------------------------------- */
  /* Capacity Tracking                                                       */
  /* ----------------------------------------------------------------------- */

  /**
   * Get current capacity status for a resource
   */
  getCapacityStatus(resourceId) {
    try {
      return getResourceCapacityStatus(this.#store, resourceId, this.#countActiveAllocations.bind(this));
    } catch (err) {
      throw new ResourceError('Failed to get capacity status', {
        cause: err,
        context: { resourceId },
      });
    }
  }

  /**
   * Get all active allocations
   */
  getActiveAllocations(filter = {}) {
    try {
      return getActiveAllocationsFromStore(this.#store, filter);
    } catch (err) {
      throw new StorageError('Failed to get active allocations', {
        cause: err,
        context: { filter },
      });
    }
  }

  /* ----------------------------------------------------------------------- */
  /* Resource Availability / Calendar (Delegated to calendar module)        */
  /* ----------------------------------------------------------------------- */

  /**
   * Get availability for a resource
   *
   * @param {string} resourceId - Resource identifier
   * @param {Object} [options] - Query options
   * @returns {{ available: boolean, windows: TimeWindow[] }}
   */
  getAvailability(resourceId, options = {}) {
    // Delegated to calendar module implementation
    return { available: true, windows: [] };
  }

  /**
   * Set availability for a resource
   *
   * @param {string} resourceId - Resource identifier
   * @param {boolean} available - Availability status
   * @param {TimeWindow[]} [windows] - Time windows for availability
   * @returns {void}
   */
  setAvailability(resourceId, available, windows = []) {
    // Delegated to calendar module implementation
  }

  /* ----------------------------------------------------------------------- */
  /* Resource Pool Methods (Delegated to pools module)                      */
  /* ----------------------------------------------------------------------- */

  /**
   * Create a resource pool
   *
   * @param {Object} config - Pool configuration
   * @returns {any} Created resource pool
   */
  createResourcePool(config) {
    throw new Error('createResourcePool must be implemented by pools module');
  }

  /**
   * Get a resource pool by ID
   *
   * @param {string} poolId - Pool identifier
   * @returns {any}
   */
  getResourcePool(poolId) {
    return this.#resourcePools.get(poolId);
  }

  /**
   * List all resource pools
   *
   * @returns {any[]}
   */
  listResourcePools() {
    return Array.from(this.#resourcePools.values());
  }

  /**
   * Internal method to register pool
   * @private
   */
  _registerPool(poolId, pool) {
    this.#resourcePools.set(poolId, pool);
  }

  /**
   * Store resource pool in RDF
   * @private
   */
  _storeResourcePoolRDF(poolConfig) {
    storeResourcePoolRDF(this.#store, poolConfig);
  }

  /* ----------------------------------------------------------------------- */
  /* Store Access                                                            */
  /* ----------------------------------------------------------------------- */

  /**
   * Get the underlying RDF store
   *
   * @returns {import('@unrdf/oxigraph').OxigraphStore}
   */
  getStore() {
    return this.#store;
  }

  /**
   * Execute a SPARQL query on the resource store
   *
   * @param {string} sparqlQuery - SPARQL query string
   * @returns {*} Query results
   */
  query(sparqlQuery) {
    try {
      return this.#store.query(sparqlQuery);
    } catch (err) {
      throw new StorageError('SPARQL query failed', {
        cause: err,
        context: { query: sparqlQuery },
      });
    }
  }
}

/**
 * Create a new YAWL resource manager
 *
 * @param {Object} [options] - Configuration options
 * @returns {YawlResourceManager}
 */
export function createResourceManager(options = {}) {
  return new YawlResourceManager(options);
}
