/**
 * @fileoverview YAWL Resource Allocation Logic
 *
 * Core allocation/deallocation implementation
 *
 * @module @unrdf/yawl/resources/allocation
 * @version 1.0.0
 */

import { dataFactory } from '@unrdf/oxigraph';
import { ResourceError, StorageError } from '../errors.mjs';
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
  try {
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
  } catch (err) {
    throw new ResourceError('Failed to check resource capacity', {
      cause: err,
      context: { resourceId: resource?.id },
    });
  }
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
  try {
    const validatedWorkItem = WorkItemSchema.parse(workItem);
    const validatedResource = ResourceSchema.parse(resource);

    const capacityCheck = checkResourceCapacity(store, validatedResource);
    if (!capacityCheck.allowed) {
      throw new ResourceError(
        `Capacity exceeded for resource ${validatedResource.id}: ` +
        `${capacityCheck.current}/${capacityCheck.max}`,
        { context: { resourceId: validatedResource.id, capacityCheck } }
      );
    }

    const eligibilityCheck = await checkResourceEligibility(store, validatedResource, validatedWorkItem);
    if (!eligibilityCheck.eligible) {
      throw new ResourceError(
        `Resource ${validatedResource.id} not eligible: ${eligibilityCheck.reason}`,
        { context: { resourceId: validatedResource.id, workItemId: validatedWorkItem.id } }
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
  } catch (err) {
    if (err instanceof ResourceError) {
      throw err;
    }
    throw new ResourceError('Resource allocation failed', {
      cause: err,
      context: { workItem: workItem?.id, resource: resource?.id },
    });
  }
}

/**
 * Deallocate a resource
 *
 * @param {import('@unrdf/oxigraph').OxigraphStore} store - RDF store
 * @param {string} allocationId - Allocation ID
 * @returns {boolean} Success status
 */
export function performResourceDeallocation(store, allocationId) {
  try {
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
  } catch (err) {
    throw new ResourceError('Resource deallocation failed', {
      cause: err,
      context: { allocationId },
    });
  }
}
