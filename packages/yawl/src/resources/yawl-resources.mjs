/**
 * @fileoverview YAWL Resource Allocation Semantics
 *
 * Implements YAWL-compliant resource allocation with:
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
import {
  tracer,
  resourcesAllocatedCounter,
  resourceAllocationHistogram,
  resourcesAvailableGauge,
} from '../otel.mjs';

const { namedNode, literal, quad, blankNode, defaultGraph } = dataFactory;

/* ========================================================================= */
/* Namespace Definitions                                                     */
/* ========================================================================= */

/**
 * YAWL Resource namespace
 * @constant {string}
 */
const YAWL_NS = 'http://yawlfoundation.org/yawlschema#';

/**
 * FOAF namespace for person properties
 * @constant {string}
 */
const FOAF_NS = 'http://xmlns.com/foaf/0.1/';

/**
 * RDF namespace
 * @constant {string}
 */
const RDF_NS = 'http://www.w3.org/1999/02/22-rdf-syntax-ns#';

/**
 * XSD namespace for datatypes
 * @constant {string}
 */
const XSD_NS = 'http://www.w3.org/2001/XMLSchema#';

/**
 * TIME namespace for temporal properties
 * @constant {string}
 */
const TIME_NS = 'http://www.w3.org/2006/time#';

/**
 * Namespace helper functions
 * @private
 */
const yawl = (localName) => namedNode(`${YAWL_NS}${localName}`);
const foaf = (localName) => namedNode(`${FOAF_NS}${localName}`);
const rdf = (localName) => namedNode(`${RDF_NS}${localName}`);
const xsd = (localName) => `${XSD_NS}${localName}`;
const time = (localName) => namedNode(`${TIME_NS}${localName}`);

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
const ResourceSchema = z.object({
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
const WorkItemSchema = z.object({
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
const PolicyPackSchema = z.object({
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
const TimeWindowSchema = z.object({
  start: z.string().datetime(),
  end: z.string().datetime(),
  available: z.boolean().default(true),
});

/**
 * Allocation receipt schema
 */
const AllocationReceiptSchema = z.object({
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
   * @type {Map<string, ResourcePool>}
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
   *
   * @example
   * manager.registerPolicyPack({
   *   id: 'approval-policy',
   *   name: 'Approval Workflow Resources',
   *   resources: [
   *     { id: 'approvers', type: 'Role', capacity: 1, sparql: '...' }
   *   ],
   *   priority: 10
   * });
   */
  registerPolicyPack(policyPack) {
    const validated = PolicyPackSchema.parse(policyPack);
    this.#policyPacks.set(validated.id, validated);

    // Store policy pack in RDF for audit
    this.#storePolicyPackRDF(validated);
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

  /**
   * Store policy pack in RDF
   * @private
   * @param {PolicyPack} policyPack
   */
  #storePolicyPackRDF(policyPack) {
    const packNode = namedNode(`${YAWL_NS}policypack/${policyPack.id}`);

    this.#store.add(quad(
      packNode,
      rdf('type'),
      yawl('PolicyPack'),
      defaultGraph()
    ));

    if (policyPack.name) {
      this.#store.add(quad(
        packNode,
        foaf('name'),
        literal(policyPack.name),
        defaultGraph()
      ));
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

    // Store each resource definition
    for (const resource of policyPack.resources) {
      const resourceNode = namedNode(`${YAWL_NS}resource/${resource.id}`);

      this.#store.add(quad(
        packNode,
        yawl('hasResource'),
        resourceNode,
        defaultGraph()
      ));

      this.#storeResourceRDF(resource, resourceNode);
    }
  }

  /**
   * Store resource definition in RDF
   * @private
   * @param {Resource} resource
   * @param {import('@unrdf/oxigraph').NamedNode} resourceNode
   */
  #storeResourceRDF(resource, resourceNode) {
    this.#store.add(quad(
      resourceNode,
      rdf('type'),
      yawl(resource.type),
      defaultGraph()
    ));

    if (resource.name) {
      this.#store.add(quad(
        resourceNode,
        foaf('name'),
        literal(resource.name),
        defaultGraph()
      ));
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

  /**
   * Allocate a resource to a work item
   *
   * Performs:
   * 1. Capacity check (not exceeded)
   * 2. Eligibility check (SPARQL conditions)
   * 3. Policy pack resource rules validation
   * 4. Creates allocation in RDF
   * 5. Returns receipt with allocation proof
   *
   * @param {WorkItem} workItem - Work item to allocate resource for
   * @param {Resource} resource - Resource to allocate
   * @param {Object} [options] - Allocation options
   * @param {string} [options.policyPackId] - Specific policy pack to use
   * @param {number} [options.duration] - Allocation duration in milliseconds
   * @returns {Promise<AllocationReceipt>} Allocation receipt with proof
   * @throws {Error} If capacity exceeded or eligibility check fails
   *
   * @example
   * const receipt = await manager.allocateResource(
   *   { id: 'wi-001', taskId: 'approval', caseId: 'case-123' },
   *   { id: 'john', type: 'Participant', capacity: 1 }
   * );
   * console.log(receipt.proof.capacityCheck); // true
   */
  async allocateResource(workItem, resource, options = {}) {
    const span = tracer.startSpan('resource.allocate');
    const startTime = Date.now();

    try {
      // Validate inputs
      const validatedWorkItem = WorkItemSchema.parse(workItem);
      const validatedResource = ResourceSchema.parse(resource);

      span.setAttributes({
        'resource.id': validatedResource.id,
        'resource.type': validatedResource.type,
        'workItem.id': validatedWorkItem.id,
        'resource.capacity': validatedResource.capacity,
      });

      // Step 1: Check capacity
      const capacityCheck = this.#checkCapacity(validatedResource);
      if (!capacityCheck.allowed) {
        throw new Error(
          `Capacity exceeded for resource ${validatedResource.id}: ` +
          `${capacityCheck.current}/${capacityCheck.max}`
        );
      }

      // Step 2: Check eligibility (SPARQL conditions)
      const eligibilityCheck = await this.#checkEligibility(
        validatedResource,
        validatedWorkItem
      );
      if (!eligibilityCheck.eligible) {
        throw new Error(
          `Resource ${validatedResource.id} not eligible: ${eligibilityCheck.reason}`
        );
      }

      // Step 3: Get policy pack if specified
      let policyPackId = options.policyPackId;
      if (!policyPackId) {
        // Find first matching policy pack by priority
        const matchingPack = this.#findMatchingPolicyPack(validatedResource);
        policyPackId = matchingPack?.id;
      }

      // Step 4: Create allocation in RDF
      const allocationId = this.#createAllocation(
        validatedWorkItem,
        validatedResource,
        options.duration
      );

      // Step 5: Create and return receipt
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

      // Validate receipt
      AllocationReceiptSchema.parse(receipt);

      // Record metrics
      const duration = Date.now() - startTime;
      resourcesAllocatedCounter.add(1, { 'resource.id': validatedResource.id, 'resource.type': validatedResource.type });
      resourceAllocationHistogram.record(duration, { 'resource.id': validatedResource.id });
      resourcesAvailableGauge.add(-1, { 'resource.type': validatedResource.type });

      span.setAttributes({
        'allocation.id': allocationId,
        'allocation.duration_ms': duration,
        'allocation.policyPackId': policyPackId || 'none',
      });
      span.setStatus({ code: 1 }); // OK
      return receipt;
    } catch (error) {
      span.recordException(error);
      span.setStatus({ code: 2, message: error.message }); // ERROR
      throw error;
    } finally {
      span.end();
    }
  }

  /**
   * Deallocate a resource from a work item
   *
   * @param {string} allocationId - Allocation receipt ID
   * @returns {boolean} True if deallocation successful
   *
   * @example
   * const success = manager.deallocateResource('alloc-001');
   */
  deallocateResource(allocationId) {
    const allocationNode = namedNode(`${YAWL_NS}allocation/${allocationId}`);

    // Check if allocation exists
    const allocations = this.#store.match(
      allocationNode,
      rdf('type'),
      yawl('Allocation'),
      null
    );

    if (!Array.from(allocations).length) {
      return false;
    }

    // Mark allocation as deallocated
    this.#store.add(quad(
      allocationNode,
      yawl('status'),
      literal('deallocated'),
      defaultGraph()
    ));

    this.#store.add(quad(
      allocationNode,
      yawl('deallocatedAt'),
      literal(new Date().toISOString(), namedNode(xsd('dateTime'))),
      defaultGraph()
    ));

    return true;
  }

  /**
   * Check resource capacity
   * @private
   * @param {Resource} resource
   * @returns {{ allowed: boolean, current: number, max: number }}
   */
  #checkCapacity(resource) {
    // Unlimited capacity
    if (resource.capacity === -1) {
      return { allowed: true, current: 0, max: -1 };
    }

    // Count current allocations
    const resourceNode = namedNode(`${YAWL_NS}resource/${resource.id}`);
    const activeAllocations = this.#countActiveAllocations(resourceNode);

    return {
      allowed: activeAllocations < resource.capacity,
      current: activeAllocations,
      max: resource.capacity,
    };
  }

  /**
   * Count active allocations for a resource
   * @private
   * @param {import('@unrdf/oxigraph').NamedNode} resourceNode
   * @returns {number}
   */
  #countActiveAllocations(resourceNode) {
    // Query for allocations that are not deallocated
    const query = `
      PREFIX yawl: <${YAWL_NS}>
      PREFIX rdf: <${RDF_NS}>

      SELECT (COUNT(?allocation) as ?count) WHERE {
        ?allocation rdf:type yawl:Allocation ;
                    yawl:resource <${resourceNode.value}> .
        FILTER NOT EXISTS {
          ?allocation yawl:status "deallocated" .
        }
      }
    `;

    try {
      const results = this.#store.query(query);
      const bindings = Array.from(results);
      if (bindings.length > 0 && bindings[0].get('count')) {
        return parseInt(bindings[0].get('count').value, 10);
      }
    } catch {
      // Fallback: count via match
      const allocations = this.#store.match(null, yawl('resource'), resourceNode, null);
      return Array.from(allocations).length;
    }

    return 0;
  }

  /**
   * Check resource eligibility via SPARQL
   * @private
   * @param {Resource} resource
   * @param {WorkItem} workItem
   * @returns {Promise<{ eligible: boolean, reason?: string, sparqlResult?: boolean }>}
   */
  async #checkEligibility(resource, workItem) {
    // No SPARQL condition = eligible by default
    if (!resource.sparql) {
      return { eligible: true };
    }

    try {
      // Execute ASK query
      const sparqlQuery = resource.sparql
        .replace(/\?workItem/g, `<${YAWL_NS}workitem/${workItem.id}>`)
        .replace(/\?resource/g, `<${YAWL_NS}resource/${resource.id}>`)
        .replace(/\?case/g, `<${YAWL_NS}case/${workItem.caseId}>`);

      const result = this.#store.query(sparqlQuery);

      // ASK queries return boolean
      if (typeof result === 'boolean') {
        return {
          eligible: result,
          sparqlResult: result,
          reason: result ? undefined : 'SPARQL eligibility condition not met',
        };
      }

      // SELECT queries - check if any results
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

  /**
   * Find matching policy pack for resource
   * @private
   * @param {Resource} resource
   * @returns {PolicyPack|undefined}
   */
  #findMatchingPolicyPack(resource) {
    const packs = this.listPolicyPacks();

    for (const pack of packs) {
      if (!pack.enabled) continue;

      const hasResource = pack.resources.some(r => r.id === resource.id);
      if (hasResource) {
        return pack;
      }
    }

    return undefined;
  }

  /**
   * Create allocation in RDF
   * @private
   * @param {WorkItem} workItem
   * @param {Resource} resource
   * @param {number} [duration]
   * @returns {string} Allocation ID
   */
  #createAllocation(workItem, resource, duration) {
    this.#allocationCounter++;
    const allocationId = `alloc-${Date.now()}-${this.#allocationCounter}`;
    const allocationNode = namedNode(`${YAWL_NS}allocation/${allocationId}`);
    const resourceNode = namedNode(`${YAWL_NS}resource/${resource.id}`);
    const workItemNode = namedNode(`${YAWL_NS}workitem/${workItem.id}`);

    const now = new Date();

    // Type
    this.#store.add(quad(
      allocationNode,
      rdf('type'),
      yawl('Allocation'),
      defaultGraph()
    ));

    // Resource reference
    this.#store.add(quad(
      allocationNode,
      yawl('resource'),
      resourceNode,
      defaultGraph()
    ));

    // Work item reference
    this.#store.add(quad(
      allocationNode,
      yawl('workItem'),
      workItemNode,
      defaultGraph()
    ));

    // Allocated timestamp
    this.#store.add(quad(
      allocationNode,
      yawl('allocatedAt'),
      literal(now.toISOString(), namedNode(xsd('dateTime'))),
      defaultGraph()
    ));

    // Status
    this.#store.add(quad(
      allocationNode,
      yawl('status'),
      literal('active'),
      defaultGraph()
    ));

    // Expiration if duration specified
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
  /* Resource Eligibility                                                    */
  /* ----------------------------------------------------------------------- */

  /**
   * Get eligible resources for a task in a case
   *
   * @param {string} taskId - Task identifier
   * @param {string} caseId - Case identifier
   * @param {Object} [options] - Query options
   * @param {string} [options.resourceType] - Filter by resource type
   * @param {boolean} [options.checkAvailability] - Also check availability calendar
   * @returns {Promise<Resource[]>} Ordered list of eligible resources (by policy priority)
   *
   * @example
   * const eligible = await manager.getEligibleResources('approval-task', 'case-123');
   * console.log(`Found ${eligible.length} eligible resources`);
   */
  async getEligibleResources(taskId, caseId, options = {}) {
    const eligibleResources = [];

    // Collect all resources from policy packs (ordered by priority)
    const packs = this.listPolicyPacks();

    for (const pack of packs) {
      if (!pack.enabled) continue;

      for (const resource of pack.resources) {
        // Filter by type if specified
        if (options.resourceType && resource.type !== options.resourceType) {
          continue;
        }

        // Check capacity
        const capacityCheck = this.#checkCapacity(resource);
        if (!capacityCheck.allowed) {
          continue;
        }

        // Check eligibility via SPARQL
        const mockWorkItem = { id: `wi-${taskId}`, taskId, caseId };
        const eligibility = await this.#checkEligibility(resource, mockWorkItem);

        if (!eligibility.eligible) {
          continue;
        }

        // Check availability if requested
        if (options.checkAvailability) {
          const availability = this.getAvailability(resource.id);
          if (!availability.available) {
            continue;
          }
        }

        eligibleResources.push({
          ...resource,
          _policyPackId: pack.id,
          _priority: pack.priority || 0,
        });
      }
    }

    // Sort by priority (already sorted by pack, but ensure stability)
    return eligibleResources.sort((a, b) => (b._priority || 0) - (a._priority || 0));
  }

  /**
   * Query RDF store for matching resources
   *
   * @param {string} resourceType - Resource type to query
   * @returns {Resource[]} Matching resources from RDF store
   */
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
        const capacity = binding.get('capacity')?.value
          ? parseInt(binding.get('capacity').value, 10)
          : 1;

        resources.push({
          id,
          type: resourceType,
          name,
          capacity,
        });
      }

      return resources;
    } catch {
      return [];
    }
  }

  /* ----------------------------------------------------------------------- */
  /* Resource Availability / Calendar                                        */
  /* ----------------------------------------------------------------------- */

  /**
   * Get availability for a resource
   *
   * Checks participant schedule (foaf namespace) and returns
   * available time windows.
   *
   * @param {string} resourceId - Resource identifier
   * @param {Object} [options] - Query options
   * @param {string} [options.from] - Start of time range (ISO datetime)
   * @param {string} [options.to] - End of time range (ISO datetime)
   * @returns {{ available: boolean, windows: TimeWindow[], schedule?: Object }}
   *
   * @example
   * const availability = manager.getAvailability('john', {
   *   from: '2024-01-15T09:00:00Z',
   *   to: '2024-01-15T17:00:00Z'
   * });
   * console.log(availability.available); // true/false
   * console.log(availability.windows); // [{ start, end, available }]
   */
  getAvailability(resourceId, options = {}) {
    const resourceNode = namedNode(`${YAWL_NS}resource/${resourceId}`);
    const now = new Date();

    // Query for availability schedule with windows stored on blank nodes
    const query = `
      PREFIX yawl: <${YAWL_NS}>
      PREFIX foaf: <${FOAF_NS}>
      PREFIX time: <${TIME_NS}>
      PREFIX xsd: <${XSD_NS}>

      SELECT ?available ?startTime ?endTime ?windowAvailable WHERE {
        <${resourceNode.value}> foaf:available ?available .
        OPTIONAL {
          <${resourceNode.value}> yawl:hasAvailabilityWindow ?window .
          ?window yawl:scheduleStart ?startTime .
          ?window yawl:scheduleEnd ?endTime .
          ?window foaf:available ?windowAvailable .
        }
      }
    `;

    try {
      const results = this.#store.query(query);
      const bindings = Array.from(results);

      if (bindings.length === 0) {
        // No availability info = assume available (for tools especially)
        return {
          available: true,
          windows: [{
            start: (options.from || now.toISOString()),
            end: (options.to || new Date(now.getTime() + 86400000).toISOString()),
            available: true,
          }],
        };
      }

      const windows = [];
      let isCurrentlyAvailable = true;

      for (const binding of bindings) {
        const available = binding.get('available')?.value;
        const startTime = binding.get('startTime')?.value;
        const endTime = binding.get('endTime')?.value;
        const windowAvailable = binding.get('windowAvailable')?.value;

        // Parse overall availability
        if (available === 'false' || available === '0') {
          isCurrentlyAvailable = false;
        }

        // Add window if present
        if (startTime && endTime) {
          windows.push({
            start: startTime,
            end: endTime,
            available: windowAvailable !== 'false' && windowAvailable !== '0',
          });
        }
      }

      // Filter windows by requested time range
      const filteredWindows = windows.filter(w => {
        if (options.from && new Date(w.end) < new Date(options.from)) return false;
        if (options.to && new Date(w.start) > new Date(options.to)) return false;
        return true;
      });

      // Determine final availability: use overall status if we have windows, or check if any window is available
      const finalAvailable = filteredWindows.length > 0
        ? isCurrentlyAvailable
        : isCurrentlyAvailable;

      return {
        available: finalAvailable,
        windows: filteredWindows.length > 0 ? filteredWindows : [{
          start: options.from || now.toISOString(),
          end: options.to || new Date(now.getTime() + 86400000).toISOString(),
          available: isCurrentlyAvailable,
        }],
      };
    } catch {
      // Fallback: assume available
      return {
        available: true,
        windows: [{
          start: options.from || now.toISOString(),
          end: options.to || new Date(now.getTime() + 86400000).toISOString(),
          available: true,
        }],
      };
    }
  }

  /**
   * Set availability for a resource
   *
   * @param {string} resourceId - Resource identifier
   * @param {boolean} available - Availability status
   * @param {TimeWindow[]} [windows] - Time windows for availability
   * @returns {void}
   *
   * @example
   * manager.setAvailability('john', true, [
   *   { start: '2024-01-15T09:00:00Z', end: '2024-01-15T17:00:00Z', available: true }
   * ]);
   */
  setAvailability(resourceId, available, windows = []) {
    const resourceNode = namedNode(`${YAWL_NS}resource/${resourceId}`);

    // Set overall availability
    this.#store.add(quad(
      resourceNode,
      foaf('available'),
      literal(String(available), namedNode(xsd('boolean'))),
      defaultGraph()
    ));

    // Store time windows
    for (const window of windows) {
      const validated = TimeWindowSchema.parse(window);
      const windowNode = blankNode();

      this.#store.add(quad(
        resourceNode,
        yawl('hasAvailabilityWindow'),
        windowNode,
        defaultGraph()
      ));

      this.#store.add(quad(
        windowNode,
        yawl('scheduleStart'),
        literal(validated.start, namedNode(xsd('dateTime'))),
        defaultGraph()
      ));

      this.#store.add(quad(
        windowNode,
        yawl('scheduleEnd'),
        literal(validated.end, namedNode(xsd('dateTime'))),
        defaultGraph()
      ));

      this.#store.add(quad(
        windowNode,
        foaf('available'),
        literal(String(validated.available), namedNode(xsd('boolean'))),
        defaultGraph()
      ));
    }
  }

  /* ----------------------------------------------------------------------- */
  /* Resource Pools                                                          */
  /* ----------------------------------------------------------------------- */

  /**
   * Create a resource pool
   *
   * Bundles resources for workflow with time-window tracking
   * and "allocate any from pool" semantics.
   *
   * @param {Object} config - Pool configuration
   * @param {string} config.id - Pool identifier
   * @param {string} [config.name] - Pool display name
   * @param {Resource[]} config.resources - Resources in pool
   * @param {string} [config.allocationStrategy] - 'round-robin' | 'priority' | 'random'
   * @returns {ResourcePool} Created resource pool
   *
   * @example
   * const pool = manager.createResourcePool({
   *   id: 'approvers-pool',
   *   name: 'Approval Team',
   *   resources: [
   *     { id: 'alice', type: 'Participant', capacity: 1 },
   *     { id: 'bob', type: 'Participant', capacity: 1 }
   *   ],
   *   allocationStrategy: 'round-robin'
   * });
   *
   * const resource = await pool.allocateAny(workItem);
   */
  createResourcePool(config) {
    const poolConfig = {
      id: z.string().min(1).parse(config.id),
      name: config.name,
      resources: config.resources.map(r => ResourceSchema.parse(r)),
      allocationStrategy: config.allocationStrategy || 'priority',
    };

    const pool = new ResourcePool(this, poolConfig);
    this.#resourcePools.set(poolConfig.id, pool);

    // Store pool in RDF
    this.#storeResourcePoolRDF(poolConfig);

    return pool;
  }

  /**
   * Get a resource pool by ID
   *
   * @param {string} poolId - Pool identifier
   * @returns {ResourcePool|undefined}
   */
  getResourcePool(poolId) {
    return this.#resourcePools.get(poolId);
  }

  /**
   * List all resource pools
   *
   * @returns {ResourcePool[]}
   */
  listResourcePools() {
    return Array.from(this.#resourcePools.values());
  }

  /**
   * Store resource pool in RDF
   * @private
   * @param {Object} poolConfig
   */
  #storeResourcePoolRDF(poolConfig) {
    const poolNode = namedNode(`${YAWL_NS}pool/${poolConfig.id}`);

    this.#store.add(quad(
      poolNode,
      rdf('type'),
      yawl('ResourcePool'),
      defaultGraph()
    ));

    if (poolConfig.name) {
      this.#store.add(quad(
        poolNode,
        foaf('name'),
        literal(poolConfig.name),
        defaultGraph()
      ));
    }

    this.#store.add(quad(
      poolNode,
      yawl('allocationStrategy'),
      literal(poolConfig.allocationStrategy),
      defaultGraph()
    ));

    for (const resource of poolConfig.resources) {
      const resourceNode = namedNode(`${YAWL_NS}resource/${resource.id}`);
      this.#storeResourceRDF(resource, resourceNode);

      this.#store.add(quad(
        poolNode,
        yawl('hasResource'),
        resourceNode,
        defaultGraph()
      ));
    }
  }

  /* ----------------------------------------------------------------------- */
  /* Capacity Tracking                                                       */
  /* ----------------------------------------------------------------------- */

  /**
   * Get current capacity status for a resource
   *
   * @param {string} resourceId - Resource identifier
   * @returns {{ current: number, max: number, available: number, utilizationPercent: number }}
   *
   * @example
   * const status = manager.getCapacityStatus('john');
   * console.log(`${status.current}/${status.max} allocated (${status.utilizationPercent}%)`);
   */
  getCapacityStatus(resourceId) {
    const resourceNode = namedNode(`${YAWL_NS}resource/${resourceId}`);

    // Get max capacity
    const capacityQuads = this.#store.match(resourceNode, yawl('capacity'), null, null);
    const capacityArr = Array.from(capacityQuads);
    let maxCapacity = 1;

    if (capacityArr.length > 0) {
      maxCapacity = parseInt(capacityArr[0].object.value, 10);
    }

    // Count active allocations
    const currentAllocations = this.#countActiveAllocations(resourceNode);

    // Calculate available and utilization
    const available = maxCapacity === -1 ? Infinity : Math.max(0, maxCapacity - currentAllocations);
    const utilizationPercent = maxCapacity === -1 ? 0 : Math.round((currentAllocations / maxCapacity) * 100);

    return {
      current: currentAllocations,
      max: maxCapacity,
      available: maxCapacity === -1 ? -1 : available,
      utilizationPercent,
    };
  }

  /**
   * Get all active allocations
   *
   * @param {Object} [filter] - Filter options
   * @param {string} [filter.resourceId] - Filter by resource
   * @param {string} [filter.workItemId] - Filter by work item
   * @returns {Array<{ allocationId: string, resourceId: string, workItemId: string, allocatedAt: string }>}
   */
  getActiveAllocations(filter = {}) {
    let query = `
      PREFIX yawl: <${YAWL_NS}>
      PREFIX rdf: <${RDF_NS}>

      SELECT ?allocation ?resource ?workItem ?allocatedAt WHERE {
        ?allocation rdf:type yawl:Allocation ;
                    yawl:resource ?resource ;
                    yawl:workItem ?workItem ;
                    yawl:allocatedAt ?allocatedAt .
        FILTER NOT EXISTS {
          ?allocation yawl:status "deallocated" .
        }
    `;

    if (filter.resourceId) {
      query += `\n        FILTER(?resource = <${YAWL_NS}resource/${filter.resourceId}>)`;
    }

    if (filter.workItemId) {
      query += `\n        FILTER(?workItem = <${YAWL_NS}workitem/${filter.workItemId}>)`;
    }

    query += '\n      }';

    try {
      const results = this.#store.query(query);
      const allocations = [];

      for (const binding of results) {
        allocations.push({
          allocationId: binding.get('allocation')?.value.replace(`${YAWL_NS}allocation/`, ''),
          resourceId: binding.get('resource')?.value.replace(`${YAWL_NS}resource/`, ''),
          workItemId: binding.get('workItem')?.value.replace(`${YAWL_NS}workitem/`, ''),
          allocatedAt: binding.get('allocatedAt')?.value,
        });
      }

      return allocations;
    } catch {
      return [];
    }
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
   * @type {YawlResourceManager}
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
   * @param {YawlResourceManager} manager - Parent manager
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
   * @returns {Resource[]}
   */
  get resources() {
    return [...this.#config.resources];
  }

  /**
   * Allocate any available resource from pool
   *
   * @param {WorkItem} workItem - Work item to allocate for
   * @param {Object} [options] - Allocation options
   * @returns {Promise<AllocationReceipt|null>} Allocation receipt or null if none available
   *
   * @example
   * const receipt = await pool.allocateAny(workItem);
   * if (receipt) {
   *   console.log(`Allocated ${receipt.resourceId}`);
   * }
   */
  async allocateAny(workItem, options = {}) {
    const orderedResources = this.#getOrderedResources();

    for (const resource of orderedResources) {
      try {
        const receipt = await this.#manager.allocateResource(workItem, resource, options);
        return receipt;
      } catch {
        // Resource not available, try next
        continue;
      }
    }

    return null;
  }

  /**
   * Get available resources in pool (with capacity)
   *
   * @returns {Resource[]}
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
   * @returns {Resource[]}
   */
  #getOrderedResources() {
    const resources = [...this.#config.resources];

    switch (this.#config.allocationStrategy) {
      case 'round-robin':
        // Rotate starting position
        const rotated = [
          ...resources.slice(this.#roundRobinIndex),
          ...resources.slice(0, this.#roundRobinIndex),
        ];
        this.#roundRobinIndex = (this.#roundRobinIndex + 1) % resources.length;
        return rotated;

      case 'random':
        // Shuffle using Fisher-Yates
        for (let i = resources.length - 1; i > 0; i--) {
          const j = Math.floor(Math.random() * (i + 1));
          [resources[i], resources[j]] = [resources[j], resources[i]];
        }
        return resources;

      case 'priority':
      default:
        // Already ordered by priority from policy pack
        return resources;
    }
  }
}

/* ========================================================================= */
/* Factory Functions                                                         */
/* ========================================================================= */

/**
 * Create a new YAWL resource manager
 *
 * @param {Object} [options] - Configuration options
 * @returns {YawlResourceManager}
 *
 * @example
 * const manager = createResourceManager();
 * manager.registerPolicyPack(myPolicyPack);
 */
export function createResourceManager(options = {}) {
  return new YawlResourceManager(options);
}

/**
 * Create a Participant resource definition
 *
 * @param {Object} config - Participant configuration
 * @param {string} config.id - Participant identifier
 * @param {string} [config.name] - Participant name
 * @param {number} [config.capacity=1] - Maximum concurrent allocations
 * @param {string} [config.sparql] - Eligibility SPARQL query
 * @returns {Resource}
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
 * @returns {Resource}
 *
 * @example
 * const tool = createTool({
 *   id: 'email-service',
 *   name: 'Email Service',
 *   capacity: -1 // unlimited
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
 * @returns {Resource}
 *
 * @example
 * const role = createRole({
 *   id: 'approvers',
 *   name: 'Approval Team',
 *   sparql: `
 *     PREFIX foaf: <http://xmlns.com/foaf/0.1/>
 *     ASK {
 *       ?person foaf:hasRole <http://example.org/roles/approvers> .
 *     }
 *   `
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
 * @param {Resource[]} config.resources - Resource definitions
 * @param {number} [config.priority=0] - Priority (higher = more important)
 * @param {boolean} [config.enabled=true] - Whether pack is enabled
 * @returns {PolicyPack}
 *
 * @example
 * const policyPack = createPolicyPack({
 *   id: 'approval-workflow',
 *   name: 'Approval Workflow Resources',
 *   resources: [
 *     createRole({ id: 'approvers', sparql: '...' }),
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
/* SPARQL Query Helpers                                                      */
/* ========================================================================= */

/**
 * Common SPARQL prefixes for resource queries
 * @constant {string}
 */
export const RESOURCE_SPARQL_PREFIXES = `
PREFIX yawl: <${YAWL_NS}>
PREFIX foaf: <${FOAF_NS}>
PREFIX rdf: <${RDF_NS}>
PREFIX xsd: <${XSD_NS}>
PREFIX time: <${TIME_NS}>
`;

/**
 * Create an eligibility SPARQL ASK query for role membership
 *
 * @param {string} roleUri - Role URI to check membership
 * @returns {string} SPARQL ASK query
 *
 * @example
 * const sparql = createRoleMembershipQuery('http://example.org/roles/approvers');
 */
export function createRoleMembershipQuery(roleUri) {
  return `
    ${RESOURCE_SPARQL_PREFIXES}
    ASK {
      ?person foaf:hasRole <${roleUri}> ;
              foaf:available true .
    }
  `;
}

/**
 * Create an eligibility SPARQL ASK query for capability check
 *
 * @param {string} capabilityUri - Required capability URI
 * @returns {string} SPARQL ASK query
 *
 * @example
 * const sparql = createCapabilityQuery('http://example.org/capabilities/sign-documents');
 */
export function createCapabilityQuery(capabilityUri) {
  return `
    ${RESOURCE_SPARQL_PREFIXES}
    ASK {
      ?resource yawl:hasCapability <${capabilityUri}> .
    }
  `;
}

/* ========================================================================= */
/* Exports                                                                   */
/* ========================================================================= */

export {
  ResourceSchema,
  WorkItemSchema,
  PolicyPackSchema,
  TimeWindowSchema,
  AllocationReceiptSchema,
  YAWL_NS,
  FOAF_NS,
  RDF_NS,
  XSD_NS,
  TIME_NS,
};
