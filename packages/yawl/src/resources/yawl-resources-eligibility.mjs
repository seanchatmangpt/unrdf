/**
 * @fileoverview YAWL Resource Eligibility Helpers
 *
 * Helper functions for checking resource eligibility
 *
 * @module @unrdf/yawl/resources/eligibility
 * @version 1.0.0
 */

import { dataFactory } from '@unrdf/oxigraph';
import { YAWL_NS, RDF_NS, FOAF_NS, yawl } from './yawl-resources-core.mjs';

const { namedNode } = dataFactory;

/**
 * Check resource eligibility via SPARQL
 *
 * @param {import('@unrdf/oxigraph').OxigraphStore} store - RDF store
 * @param {import('./yawl-resources-core.mjs').Resource} resource - Resource to check
 * @param {import('./yawl-resources-core.mjs').WorkItem} workItem - Work item context
 * @returns {Promise<{ eligible: boolean, reason?: string, sparqlResult?: boolean }>}
 */
export async function checkResourceEligibility(store, resource, workItem) {
  if (!resource.sparql) {
    return { eligible: true };
  }

  try {
    const sparqlQuery = resource.sparql
      .replace(/\?workItem/g, `<${YAWL_NS}workitem/${workItem.id}>`)
      .replace(/\?resource/g, `<${YAWL_NS}resource/${resource.id}>`)
      .replace(/\?case/g, `<${YAWL_NS}case/${workItem.caseId}>`);

    const result = store.query(sparqlQuery);

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

/**
 * Query RDF store for resources of a specific type
 *
 * @param {import('@unrdf/oxigraph').OxigraphStore} store - RDF store
 * @param {string} resourceType - Resource type to query
 * @returns {Array<import('./yawl-resources-core.mjs').Resource>} Matching resources
 */
export function queryResourcesByType(store, resourceType) {
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
    const results = store.query(query);
    const resources = [];

    for (const binding of results) {
      const resourceUri = binding.get('resource')?.value;
      if (!resourceUri) continue;

      const id = resourceUri.replace(`${YAWL_NS}resource/`, '');
      const name = binding.get('name')?.value;
      const capacity = binding.get('capacity')?.value
        ? parseInt(binding.get('capacity').value, 10)
        : 1;

      resources.push({ id, type: resourceType, name, capacity });
    }

    return resources;
  } catch {
    return [];
  }
}

/**
 * Get all active allocations from store
 *
 * @param {import('@unrdf/oxigraph').OxigraphStore} store - RDF store
 * @param {Object} [filter] - Filter options
 * @param {string} [filter.resourceId] - Filter by resource
 * @param {string} [filter.workItemId] - Filter by work item
 * @returns {Array<{ allocationId: string, resourceId: string, workItemId: string, allocatedAt: string }>}
 */
export function getActiveAllocationsFromStore(store, filter = {}) {
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
    query += `\n      FILTER(?resource = <${YAWL_NS}resource/${filter.resourceId}>)`;
  }

  if (filter.workItemId) {
    query += `\n      FILTER(?workItem = <${YAWL_NS}workitem/${filter.workItemId}>)`;
  }

  query += '\n    }';

  try {
    const results = store.query(query);
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

/**
 * Get capacity status for a resource
 *
 * @param {import('@unrdf/oxigraph').OxigraphStore} store - RDF store
 * @param {string} resourceId - Resource identifier
 * @param {Function} countAllocations - Function to count active allocations
 * @returns {{ current: number, max: number, available: number, utilizationPercent: number }}
 */
export function getResourceCapacityStatus(store, resourceId, countAllocations) {
  const resourceNode = namedNode(`${YAWL_NS}resource/${resourceId}`);

  const capacityQuads = store.match(resourceNode, yawl('capacity'), null, null);
  const capacityArr = Array.from(capacityQuads);
  let maxCapacity = 1;

  if (capacityArr.length > 0) {
    maxCapacity = parseInt(capacityArr[0].object.value, 10);
  }

  const currentAllocations = countAllocations(resourceNode);

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
 * Get eligible resources for a task/case from policy packs
 *
 * @param {Array<any>} policyPacks - Sorted policy packs
 * @param {string} taskId - Task identifier
 * @param {string} caseId - Case identifier
 * @param {Object} options - Query options
 * @param {Function} checkCapacity - Capacity check function
 * @param {Function} checkEligibility - Eligibility check function
 * @param {Function} getAvailability - Availability check function
 * @returns {Promise<Array<any>>} Eligible resources
 */
export async function findEligibleResourcesInPacks(
  policyPacks,
  taskId,
  caseId,
  options,
  checkCapacity,
  checkEligibility,
  getAvailability
) {
  const eligibleResources = [];

  for (const pack of policyPacks) {
    if (!pack.enabled) continue;

    for (const resource of pack.resources) {
      if (options.resourceType && resource.type !== options.resourceType) {
        continue;
      }

      const capacityCheck = checkCapacity(resource);
      if (!capacityCheck.allowed) {
        continue;
      }

      const mockWorkItem = { id: `wi-${taskId}`, taskId, caseId };
      const eligibility = await checkEligibility(resource, mockWorkItem);

      if (!eligibility.eligible) {
        continue;
      }

      if (options.checkAvailability) {
        const availability = getAvailability(resource.id);
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

  return eligibleResources.sort((a, b) => (b._priority || 0) - (a._priority || 0));
}
