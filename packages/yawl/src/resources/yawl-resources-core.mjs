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

import { createStore, dataFactory } from '@unrdf/oxigraph';
import { z } from 'zod';
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

const { namedNode, literal, quad, blankNode, defaultGraph } = dataFactory;

/* ========================================================================= */
/* Namespace Definitions                                                     */
/* ========================================================================= */

/**
 * YAWL Resource namespace
 * @constant {string}
 */
export const YAWL_NS = 'http://yawlfoundation.org/yawlschema#';

/**
 * FOAF namespace for person properties
 * @constant {string}
 */
export const FOAF_NS = 'http://xmlns.com/foaf/0.1/';

/**
 * RDF namespace
 * @constant {string}
 */
export const RDF_NS = 'http://www.w3.org/1999/02/22-rdf-syntax-ns#';

/**
 * XSD namespace for datatypes
 * @constant {string}
 */
export const XSD_NS = 'http://www.w3.org/2001/XMLSchema#';

/**
 * TIME namespace for temporal properties
 * @constant {string}
 */
export const TIME_NS = 'http://www.w3.org/2006/time#';

/**
 * Namespace helper functions
 * @private
 */
export const yawl = (localName) => namedNode(`${YAWL_NS}${localName}`);
export const foaf = (localName) => namedNode(`${FOAF_NS}${localName}`);
export const rdf = (localName) => namedNode(`${RDF_NS}${localName}`);
export const xsd = (localName) => `${XSD_NS}${localName}`;
export const time = (localName) => namedNode(`${TIME_NS}${localName}`);

/* ========================================================================= */
/* Zod Schemas for Validation                                                */
/* ========================================================================= */

/**
 * Resource type enum
 * @constant
 */
export const ResourceType = /** @type {const} */ ({
  PARTICIPANT: 'Participant',
  TOOL: 'Tool',
  ROLE: 'Role',
});

/**
 * Resource definition schema
 */
export const ResourceSchema = z.object({
  id: z.string().min(1),
  type: z.enum([ResourceType.PARTICIPANT, ResourceType.TOOL, ResourceType.ROLE]),
  name: z.string().optional(),
  capacity: z.number().int().min(-1).default(1), // -1 = unlimited
  sparql: z.string().optional(),
  metadata: z.record(z.unknown()).optional(),
});

/**
 * Work item schema for allocation
 */
export const WorkItemSchema = z.object({
  id: z.string().min(1),
  taskId: z.string().min(1),
  caseId: z.string().min(1),
  status: z.string().optional(),
  createdAt: z.string().datetime().optional(),
  data: z.record(z.unknown()).optional(),
});

/**
 * Policy pack resource rules schema
 */
export const PolicyPackSchema = z.object({
  id: z.string().min(1),
  name: z.string().optional(),
  version: z.string().optional(),
  resources: z.array(ResourceSchema),
  priority: z.number().int().min(0).default(0),
  enabled: z.boolean().default(true),
});

/**
 * Time window schema for availability
 */
export const TimeWindowSchema = z.object({
  start: z.string().datetime(),
  end: z.string().datetime(),
  available: z.boolean().default(true),
});

/**
 * Allocation receipt schema
 */
export const AllocationReceiptSchema = z.object({
  id: z.string(),
  workItemId: z.string(),
  resourceId: z.string(),
  resourceType: z.enum([ResourceType.PARTICIPANT, ResourceType.TOOL, ResourceType.ROLE]),
  allocatedAt: z.string().datetime(),
  expiresAt: z.string().datetime().optional(),
  proof: z.object({
    capacityCheck: z.boolean(),
    eligibilityCheck: z.boolean(),
    policyPackId: z.string().optional(),
    sparqlResult: z.boolean().optional(),
  }),
});

/**
 * @typedef {z.infer<typeof ResourceSchema>} Resource
 * @typedef {z.infer<typeof WorkItemSchema>} WorkItem
 * @typedef {z.infer<typeof PolicyPackSchema>} PolicyPack
 * @typedef {z.infer<typeof TimeWindowSchema>} TimeWindow
 * @typedef {z.infer<typeof AllocationReceiptSchema>} AllocationReceipt
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
    const validated = PolicyPackSchema.parse(policyPack);
    this.#policyPacks.set(validated.id, validated);
    storePolicyPackRDF(this.#store, validated);
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
  }

  /**
   * Deallocate a resource from a work item
   */
  deallocateResource(allocationId) {
    return performResourceDeallocation(this.#store, allocationId);
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
    return findEligibleResourcesInPacks(
      this.listPolicyPacks(),
      taskId,
      caseId,
      options,
      this.#checkCapacity.bind(this),
      this.#checkEligibility.bind(this),
      this.getAvailability.bind(this)
    );
  }

  /**
   * Query RDF store for matching resources
   */
  queryResources(resourceType) {
    return queryResourcesByType(this.#store, resourceType);
  }

  /* ----------------------------------------------------------------------- */
  /* Capacity Tracking                                                       */
  /* ----------------------------------------------------------------------- */

  /**
   * Get current capacity status for a resource
   */
  getCapacityStatus(resourceId) {
    return getResourceCapacityStatus(this.#store, resourceId, this.#countActiveAllocations.bind(this));
  }

  /**
   * Get all active allocations
   */
  getActiveAllocations(filter = {}) {
    return getActiveAllocationsFromStore(this.#store, filter);
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
    return this.#store.query(sparqlQuery);
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
