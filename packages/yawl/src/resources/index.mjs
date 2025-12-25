/**
 * @fileoverview YAWL Resource Allocation Semantics
 *
 * Main entry point for YAWL resource management. Implements YAWL-compliant
 * resource allocation with:
 * - Resource types: Participant, Tool, Role
 * - Policy pack integration for resource rules
 * - SPARQL-based eligibility conditions
 * - Capacity tracking in RDF
 * - Resource calendar/availability
 * - Allocation receipts with proofs
 *
 * @module @unrdf/yawl/resources
 * @version 1.0.0
 */

import { createStore, dataFactory } from '@unrdf/oxigraph';
import { z } from 'zod';

// Import from split modules
import {
  PARTICIPANT,
  ParticipantSchema,
  createParticipant,
  createParticipantWithRole,
} from './resource-participants.mjs';

import {
  TOOL,
  ToolSchema,
  createTool,
  createToolWithStatusCheck,
  createApiTool,
} from './resource-tools.mjs';

import {
  ROLE,
  RoleSchema,
  createRole,
  createRoleWithMembership,
  createRoleWithCapability,
  RESOURCE_SPARQL_PREFIXES,
  createRoleMembershipQuery,
  createCapabilityQuery,
} from './resource-roles.mjs';

import {
  TimeWindowSchema,
  CapacityStatusSchema,
  AllocationRecordSchema,
  checkResourceCapacity,
  countActiveAllocations,
  getCapacityStatus,
  getAllActiveAllocations,
  getAllocationHistory,
  getResourceAvailability,
  isAvailableAt,
  calculateWorkloadDistribution,
  findLeastLoadedResource,
} from './resource-capacity.mjs';

const { namedNode, literal, quad, blankNode, defaultGraph } = dataFactory;

/* ========================================================================= */
/* Namespace Definitions                                                     */
/* ========================================================================= */

const YAWL_NS = 'http://yawlfoundation.org/yawlschema#';
const FOAF_NS = 'http://xmlns.com/foaf/0.1/';
const RDF_NS = 'http://www.w3.org/1999/02/22-rdf-syntax-ns#';
const XSD_NS = 'http://www.w3.org/2001/XMLSchema#';
const TIME_NS = 'http://www.w3.org/2006/time#';

const yawl = (localName) => namedNode(`${YAWL_NS}${localName}`);
const foaf = (localName) => namedNode(`${FOAF_NS}${localName}`);
const rdf = (localName) => namedNode(`${RDF_NS}${localName}`);
const xsd = (localName) => `${XSD_NS}${localName}`;
const time = (localName) => namedNode(`${TIME_NS}${localName}`);

/* ========================================================================= */
/* Resource Type Enum                                                        */
/* ========================================================================= */

export const ResourceType = /** @type {const} */ ({
  PARTICIPANT,
  TOOL,
  ROLE,
});

/* ========================================================================= */
/* Unified Schemas                                                           */
/* ========================================================================= */

const ResourceSchema = z.union([ParticipantSchema, ToolSchema, RoleSchema]);

const WorkItemSchema = z.object({
  id: z.string().min(1),
  taskId: z.string().min(1),
  caseId: z.string().min(1),
  status: z.string().optional(),
  createdAt: z.string().datetime().optional(),
  data: z.record(z.unknown()).optional(),
});

const PolicyPackSchema = z.object({
  id: z.string().min(1),
  name: z.string().optional(),
  version: z.string().optional(),
  resources: z.array(ResourceSchema),
  priority: z.number().int().min(0).default(0),
  enabled: z.boolean().default(true),
});

const AllocationReceiptSchema = z.object({
  id: z.string(),
  workItemId: z.string(),
  resourceId: z.string(),
  resourceType: z.enum([PARTICIPANT, TOOL, ROLE]),
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
 * @typedef {z.infer<typeof AllocationReceiptSchema>} AllocationReceipt
 */

/* ========================================================================= */
/* YAWL Resource Manager                                                     */
/* ========================================================================= */

/**
 * YAWL Resource Manager
 *
 * Manages resource allocation, eligibility, and capacity tracking
 * following YAWL workflow resource patterns.
 *
 * @class
 */
export class YawlResourceManager {
  #store;
  #policyPacks;
  #resourcePools;
  #allocationCounter;

  constructor(options = {}) {
    this.#store = options.store || createStore();
    this.#policyPacks = new Map();
    this.#resourcePools = new Map();
    this.#allocationCounter = 0;
  }

  /* ----------------------------------------------------------------------- */
  /* Policy Pack Management                                                  */
  /* ----------------------------------------------------------------------- */

  registerPolicyPack(policyPack) {
    const validated = PolicyPackSchema.parse(policyPack);
    this.#policyPacks.set(validated.id, validated);
    this.#storePolicyPackRDF(validated);
  }

  unregisterPolicyPack(policyPackId) {
    const existed = this.#policyPacks.has(policyPackId);
    this.#policyPacks.delete(policyPackId);
    return existed;
  }

  getPolicyPack(policyPackId) {
    return this.#policyPacks.get(policyPackId);
  }

  listPolicyPacks() {
    return Array.from(this.#policyPacks.values())
      .sort((a, b) => (b.priority || 0) - (a.priority || 0));
  }

  #storePolicyPackRDF(policyPack) {
    const packNode = namedNode(`${YAWL_NS}policypack/${policyPack.id}`);

    this.#store.add(quad(packNode, rdf('type'), yawl('PolicyPack'), defaultGraph()));

    if (policyPack.name) {
      this.#store.add(quad(packNode, foaf('name'), literal(policyPack.name), defaultGraph()));
    }

    this.#store.add(quad(
      packNode,
      yawl('priority'),
      literal(String(policyPack.priority || 0), namedNode(xsd('integer'))),
      defaultGraph()
    ));

    this.#store.add(quad(
      packNode,
      yawl('enabled'),
      literal(String(policyPack.enabled !== false), namedNode(xsd('boolean'))),
      defaultGraph()
    ));

    for (const resource of policyPack.resources) {
      const resourceNode = namedNode(`${YAWL_NS}resource/${resource.id}`);
      this.#store.add(quad(packNode, yawl('hasResource'), resourceNode, defaultGraph()));
      this.#storeResourceRDF(resource, resourceNode);
    }
  }

  #storeResourceRDF(resource, resourceNode) {
    this.#store.add(quad(resourceNode, rdf('type'), yawl(resource.type), defaultGraph()));

    if (resource.name) {
      this.#store.add(quad(resourceNode, foaf('name'), literal(resource.name), defaultGraph()));
    }

    this.#store.add(quad(
      resourceNode,
      yawl('capacity'),
      literal(String(resource.capacity), namedNode(xsd('integer'))),
      defaultGraph()
    ));

    if (resource.sparql) {
      this.#store.add(quad(
        resourceNode,
        yawl('eligibilitySparql'),
        literal(resource.sparql),
        defaultGraph()
      ));
    }
  }

  /* ----------------------------------------------------------------------- */
  /* Resource Allocation                                                     */
  /* ----------------------------------------------------------------------- */

  async allocateResource(workItem, resource, options = {}) {
    const validatedWorkItem = WorkItemSchema.parse(workItem);
    const validatedResource = ResourceSchema.parse(resource);

    // Check capacity
    const resourceNode = namedNode(`${YAWL_NS}resource/${validatedResource.id}`);
    const capacityCheck = checkResourceCapacity(this.#store, resourceNode, validatedResource.capacity);

    if (!capacityCheck.allowed) {
      throw new Error(
        `Capacity exceeded for resource ${validatedResource.id}: ` +
        `${capacityCheck.current}/${capacityCheck.max}`
      );
    }

    // Check eligibility
    const eligibilityCheck = await this.#checkEligibility(validatedResource, validatedWorkItem);
    if (!eligibilityCheck.eligible) {
      throw new Error(`Resource ${validatedResource.id} not eligible: ${eligibilityCheck.reason}`);
    }

    // Get policy pack
    let policyPackId = options.policyPackId;
    if (!policyPackId) {
      const matchingPack = this.#findMatchingPolicyPack(validatedResource);
      policyPackId = matchingPack?.id;
    }

    // Create allocation
    const allocationId = this.#createAllocation(validatedWorkItem, validatedResource, options.duration);

    const now = new Date();
    const receipt = {
      id: allocationId,
      workItemId: validatedWorkItem.id,
      resourceId: validatedResource.id,
      resourceType: validatedResource.type,
      allocatedAt: now.toISOString(),
      expiresAt: options.duration
        ? new Date(now.getTime() + options.duration).toISOString()
        : undefined,
      proof: {
        capacityCheck: true,
        eligibilityCheck: true,
        policyPackId,
        sparqlResult: eligibilityCheck.sparqlResult,
      },
    };

    AllocationReceiptSchema.parse(receipt);
    return receipt;
  }

  deallocateResource(allocationId) {
    const allocationNode = namedNode(`${YAWL_NS}allocation/${allocationId}`);

    const allocations = this.#store.match(allocationNode, rdf('type'), yawl('Allocation'), null);
    if (!Array.from(allocations).length) {
      return false;
    }

    this.#store.add(quad(allocationNode, yawl('status'), literal('deallocated'), defaultGraph()));
    this.#store.add(quad(
      allocationNode,
      yawl('deallocatedAt'),
      literal(new Date().toISOString(), namedNode(xsd('dateTime'))),
      defaultGraph()
    ));

    return true;
  }

  async #checkEligibility(resource, workItem) {
    if (!resource.sparql) {
      return { eligible: true };
    }

    try {
      const sparqlQuery = resource.sparql
        .replace(/\?workItem/g, `<${YAWL_NS}workitem/${workItem.id}>`)
        .replace(/\?resource/g, `<${YAWL_NS}resource/${resource.id}>`)
        .replace(/\?case/g, `<${YAWL_NS}case/${workItem.caseId}>`);

      const result = this.#store.query(sparqlQuery);

      if (typeof result === 'boolean') {
        return {
          eligible: result,
          sparqlResult: result,
          reason: result ? undefined : 'SPARQL eligibility condition not met',
        };
      }

      const bindings = Array.from(result);
      const eligible = bindings.length > 0;

      return {
        eligible,
        sparqlResult: eligible,
        reason: eligible ? undefined : 'SPARQL eligibility query returned no results',
      };
    } catch (error) {
      return {
        eligible: false,
        sparqlResult: false,
        reason: `SPARQL eligibility check failed: ${error.message}`,
      };
    }
  }

  #findMatchingPolicyPack(resource) {
    const packs = this.listPolicyPacks();

    for (const pack of packs) {
      if (!pack.enabled) continue;
      const hasResource = pack.resources.some(r => r.id === resource.id);
      if (hasResource) return pack;
    }

    return undefined;
  }

  #createAllocation(workItem, resource, duration) {
    this.#allocationCounter++;
    const allocationId = `alloc-${Date.now()}-${this.#allocationCounter}`;
    const allocationNode = namedNode(`${YAWL_NS}allocation/${allocationId}`);
    const resourceNode = namedNode(`${YAWL_NS}resource/${resource.id}`);
    const workItemNode = namedNode(`${YAWL_NS}workitem/${workItem.id}`);

    const now = new Date();

    this.#store.add(quad(allocationNode, rdf('type'), yawl('Allocation'), defaultGraph()));
    this.#store.add(quad(allocationNode, yawl('resource'), resourceNode, defaultGraph()));
    this.#store.add(quad(allocationNode, yawl('workItem'), workItemNode, defaultGraph()));
    this.#store.add(quad(
      allocationNode,
      yawl('allocatedAt'),
      literal(now.toISOString(), namedNode(xsd('dateTime'))),
      defaultGraph()
    ));
    this.#store.add(quad(allocationNode, yawl('status'), literal('active'), defaultGraph()));

    if (duration) {
      this.#store.add(quad(
        allocationNode,
        yawl('expiresAt'),
        literal(new Date(now.getTime() + duration).toISOString(), namedNode(xsd('dateTime'))),
        defaultGraph()
      ));
    }

    return allocationId;
  }

  /* ----------------------------------------------------------------------- */
  /* Resource Eligibility & Queries                                          */
  /* ----------------------------------------------------------------------- */

  async getEligibleResources(taskId, caseId, options = {}) {
    const eligibleResources = [];
    const packs = this.listPolicyPacks();

    for (const pack of packs) {
      if (!pack.enabled) continue;

      for (const resource of pack.resources) {
        if (options.resourceType && resource.type !== options.resourceType) {
          continue;
        }

        const resourceNode = namedNode(`${YAWL_NS}resource/${resource.id}`);
        const capacityCheck = checkResourceCapacity(this.#store, resourceNode, resource.capacity);
        if (!capacityCheck.allowed) continue;

        const mockWorkItem = { id: `wi-${taskId}`, taskId, caseId };
        const eligibility = await this.#checkEligibility(resource, mockWorkItem);
        if (!eligibility.eligible) continue;

        if (options.checkAvailability) {
          const availability = this.getAvailability(resource.id);
          if (!availability.available) continue;
        }

        eligibleResources.push({
          ...resource,
          _policyPackId: pack.id,
          _priority: pack.priority || 0,
        });
      }
    }

    return eligibleResources.sort((a, b) => (b._priority || 0) - (a._priority || 0));
  }

  queryResources(resourceType) {
    const query = `
      PREFIX yawl: <${YAWL_NS}>
      PREFIX rdf: <${RDF_NS}>
      PREFIX foaf: <${FOAF_NS}>

      SELECT ?resource ?name ?capacity WHERE {
        ?resource rdf:type yawl:${resourceType} .
        OPTIONAL { ?resource foaf:name ?name }
        OPTIONAL { ?resource yawl:capacity ?capacity }
      }
    `;

    try {
      const results = this.#store.query(query);
      const resources = [];

      for (const binding of results) {
        const resourceUri = binding.get('resource')?.value;
        if (!resourceUri) continue;

        const id = resourceUri.replace(`${YAWL_NS}resource/`, '');
        const name = binding.get('name')?.value;
        const capacity = binding.get('capacity')?.value ? parseInt(binding.get('capacity').value, 10) : 1;

        resources.push({ id, type: resourceType, name, capacity });
      }

      return resources;
    } catch {
      return [];
    }
  }

  /* ----------------------------------------------------------------------- */
  /* Availability / Calendar                                                 */
  /* ----------------------------------------------------------------------- */

  getAvailability(resourceId, options = {}) {
    return getResourceAvailability(this.#store, resourceId, options, dataFactory);
  }

  setAvailability(resourceId, available, windows = []) {
    const resourceNode = namedNode(`${YAWL_NS}resource/${resourceId}`);

    this.#store.add(quad(
      resourceNode,
      foaf('available'),
      literal(String(available), namedNode(xsd('boolean'))),
      defaultGraph()
    ));

    for (const window of windows) {
      const validated = TimeWindowSchema.parse(window);
      const windowNode = blankNode();

      this.#store.add(quad(resourceNode, yawl('hasAvailabilityWindow'), windowNode, defaultGraph()));
      this.#store.add(quad(windowNode, yawl('scheduleStart'), literal(validated.start, namedNode(xsd('dateTime'))), defaultGraph()));
      this.#store.add(quad(windowNode, yawl('scheduleEnd'), literal(validated.end, namedNode(xsd('dateTime'))), defaultGraph()));
      this.#store.add(quad(windowNode, foaf('available'), literal(String(validated.available), namedNode(xsd('boolean'))), defaultGraph()));
    }
  }

  /* ----------------------------------------------------------------------- */
  /* Resource Pools                                                          */
  /* ----------------------------------------------------------------------- */

  createResourcePool(config) {
    const poolConfig = {
      id: z.string().min(1).parse(config.id),
      name: config.name,
      resources: config.resources.map(r => ResourceSchema.parse(r)),
      allocationStrategy: config.allocationStrategy || 'priority',
    };

    const pool = new ResourcePool(this, poolConfig);
    this.#resourcePools.set(poolConfig.id, pool);
    this.#storeResourcePoolRDF(poolConfig);

    return pool;
  }

  getResourcePool(poolId) {
    return this.#resourcePools.get(poolId);
  }

  listResourcePools() {
    return Array.from(this.#resourcePools.values());
  }

  #storeResourcePoolRDF(poolConfig) {
    const poolNode = namedNode(`${YAWL_NS}pool/${poolConfig.id}`);

    this.#store.add(quad(poolNode, rdf('type'), yawl('ResourcePool'), defaultGraph()));

    if (poolConfig.name) {
      this.#store.add(quad(poolNode, foaf('name'), literal(poolConfig.name), defaultGraph()));
    }

    this.#store.add(quad(poolNode, yawl('allocationStrategy'), literal(poolConfig.allocationStrategy), defaultGraph()));

    for (const resource of poolConfig.resources) {
      const resourceNode = namedNode(`${YAWL_NS}resource/${resource.id}`);
      this.#storeResourceRDF(resource, resourceNode);
      this.#store.add(quad(poolNode, yawl('hasResource'), resourceNode, defaultGraph()));
    }
  }

  /* ----------------------------------------------------------------------- */
  /* Capacity Tracking                                                       */
  /* ----------------------------------------------------------------------- */

  getCapacityStatus(resourceId) {
    return getCapacityStatus(this.#store, resourceId, dataFactory);
  }

  getActiveAllocations(filter = {}) {
    return getAllActiveAllocations(this.#store, filter);
  }

  /* ----------------------------------------------------------------------- */
  /* Store Access                                                            */
  /* ----------------------------------------------------------------------- */

  getStore() {
    return this.#store;
  }

  query(sparqlQuery) {
    return this.#store.query(sparqlQuery);
  }
}

/* ========================================================================= */
/* Resource Pool Class                                                       */
/* ========================================================================= */

export class ResourcePool {
  #manager;
  #config;
  #roundRobinIndex;

  constructor(manager, config) {
    this.#manager = manager;
    this.#config = config;
    this.#roundRobinIndex = 0;
  }

  get id() {
    return this.#config.id;
  }

  get name() {
    return this.#config.name;
  }

  get resources() {
    return [...this.#config.resources];
  }

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

  getAvailableResources() {
    return this.#config.resources.filter(resource => {
      const status = this.#manager.getCapacityStatus(resource.id);
      return status.max === -1 || status.available > 0;
    });
  }

  getAvailability() {
    const available = this.getAvailableResources();
    return {
      available: available.length > 0,
      availableCount: available.length,
      totalCount: this.#config.resources.length,
    };
  }

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
/* Factory Functions                                                         */
/* ========================================================================= */

export function createResourceManager(options = {}) {
  return new YawlResourceManager(options);
}

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
/* Re-exports from Modules                                                   */
/* ========================================================================= */

export {
  // Resource types
  PARTICIPANT,
  TOOL,
  ROLE,

  // Schemas
  ParticipantSchema,
  ToolSchema,
  RoleSchema,
  ResourceSchema,
  WorkItemSchema,
  PolicyPackSchema,
  TimeWindowSchema,
  AllocationReceiptSchema,

  // Participant factories & utilities
  createParticipant,
  createParticipantWithRole,

  // Tool factories & utilities
  createTool,
  createToolWithStatusCheck,
  createApiTool,

  // Role factories & utilities
  createRole,
  createRoleWithMembership,
  createRoleWithCapability,

  // SPARQL helpers
  RESOURCE_SPARQL_PREFIXES,
  createRoleMembershipQuery,
  createCapabilityQuery,

  // Capacity utilities
  checkResourceCapacity,
  countActiveAllocations,
  getCapacityStatus,
  getAllActiveAllocations,
  getAllocationHistory,
  getResourceAvailability,
  isAvailableAt,
  calculateWorkloadDistribution,
  findLeastLoadedResource,
};

/* ========================================================================= */
/* Namespace Exports                                                         */
/* ========================================================================= */

export {
  YAWL_NS,
  FOAF_NS,
  RDF_NS,
  XSD_NS,
  TIME_NS,
};
