/**
 * @fileoverview YAWL Resource Allocation Logic
 *
 * Core allocation/deallocation implementation
 *
 * @module @unrdf/yawl/resources/allocation
 * @version 1.0.0
 */

import { dataFactory } from '@unrdf/oxigraph';
import {
  YAWL_NS,
  WorkItemSchema,
  ResourceSchema,
  AllocationReceiptSchema,
  yawl,
  rdf,
  xsd,
} from './yawl-resources-types.mjs';
import { createAllocationRDF, countActiveAllocations } from './yawl-resources-rdf.mjs';
import { checkResourceEligibility } from './yawl-resources-eligibility.mjs';

const { namedNode, literal, quad, defaultGraph } = dataFactory;

/**
 * Check resource capacity
 *
 * @param {import('@unrdf/oxigraph').OxigraphStore} store - RDF store
 * @param {any} resource - Resource to check
 * @returns {{ allowed: boolean, current: number, max: number }}
 */
export function checkResourceCapacity(store, resource) {
  if (resource.capacity === -1) {
    return { allowed: true, current: 0, max: -1 };
  }

  const resourceNode = namedNode(`${YAWL_NS}resource/${resource.id}`);
  const activeAllocations = countActiveAllocations(store, resourceNode);

  return {
    allowed: activeAllocations < resource.capacity,
    current: activeAllocations,
    max: resource.capacity,
  };
}

/**
 * Find matching policy pack for resource
 *
 * @param {Array<any>} policyPacks - Sorted policy packs
 * @param {any} resource - Resource to match
 * @returns {any|undefined} Matching policy pack
 */
export function findMatchingPolicyPackForResource(policyPacks, resource) {
  for (const pack of policyPacks) {
    if (!pack.enabled) continue;

    const hasResource = pack.resources.some(r => r.id === resource.id);
    if (hasResource) {
      return pack;
    }
  }

  return undefined;
}

/**
 * Allocate a resource to a work item
 *
 * @param {import('@unrdf/oxigraph').OxigraphStore} store - RDF store
 * @param {any} workItem - Work item
 * @param {any} resource - Resource
 * @param {Object} options - Allocation options
 * @param {Array<any>} policyPacks - Available policy packs
 * @param {Object} state - Manager state
 * @returns {Promise<any>} Allocation receipt
 */
export async function performResourceAllocation(store, workItem, resource, options, policyPacks, state) {
  const validatedWorkItem = WorkItemSchema.parse(workItem);
  const validatedResource = ResourceSchema.parse(resource);

  const capacityCheck = checkResourceCapacity(store, validatedResource);
  if (!capacityCheck.allowed) {
    throw new Error(
      `Capacity exceeded for resource ${validatedResource.id}: ` +
      `${capacityCheck.current}/${capacityCheck.max}`
    );
  }

  const eligibilityCheck = await checkResourceEligibility(store, validatedResource, validatedWorkItem);
  if (!eligibilityCheck.eligible) {
    throw new Error(
      `Resource ${validatedResource.id} not eligible: ${eligibilityCheck.reason}`
    );
  }

  let policyPackId = options.policyPackId;
  if (!policyPackId) {
    const matchingPack = findMatchingPolicyPackForResource(policyPacks, validatedResource);
    policyPackId = matchingPack?.id;
  }

  state.allocationCounter++;
  const allocationId = `alloc-${Date.now()}-${state.allocationCounter}`;
  createAllocationRDF(store, allocationId, validatedWorkItem, validatedResource, options.duration);

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

/**
 * Deallocate a resource
 *
 * @param {import('@unrdf/oxigraph').OxigraphStore} store - RDF store
 * @param {string} allocationId - Allocation ID
 * @returns {boolean} Success status
 */
export function performResourceDeallocation(store, allocationId) {
  const allocationNode = namedNode(`${YAWL_NS}allocation/${allocationId}`);

  const allocations = store.match(allocationNode, rdf('type'), yawl('Allocation'), null);
  if (!Array.from(allocations).length) {
    return false;
  }

  store.add(quad(allocationNode, yawl('status'), literal('deallocated'), defaultGraph()));

  store.add(quad(
    allocationNode,
    yawl('deallocatedAt'),
    literal(new Date().toISOString(), namedNode(xsd('dateTime'))),
    defaultGraph()
  ));

  return true;
}
