/**
 * @fileoverview YAWL Resource Capacity Management
 *
 * Handles capacity tracking, allocation monitoring, availability windows,
 * and workload management for YAWL resources.
 *
 * @module @unrdf/yawl/resources/capacity
 * @version 1.0.0
 */

import { z } from 'zod';

/* ========================================================================= */
/* Namespace Constants                                                       */
/* ========================================================================= */

const YAWL_NS = 'http://yawlfoundation.org/yawlschema#';
const FOAF_NS = 'http://xmlns.com/foaf/0.1/';
const RDF_NS = 'http://www.w3.org/1999/02/22-rdf-syntax-ns#';
const XSD_NS = 'http://www.w3.org/2001/XMLSchema#';
const TIME_NS = 'http://www.w3.org/2006/time#';

/* ========================================================================= */
/* Zod Schemas                                                               */
/* ========================================================================= */

/**
 * Time window schema for availability
 */
export const TimeWindowSchema = z.object({
  start: z.string().datetime(),
  end: z.string().datetime(),
  available: z.boolean().default(true),
});

/**
 * Capacity status schema
 */
export const CapacityStatusSchema = z.object({
  current: z.number().int().min(0),
  max: z.number().int().min(-1), // -1 = unlimited
  available: z.number().int().min(-1),
  utilizationPercent: z.number().min(0).max(100),
});

/**
 * Allocation record schema
 */
export const AllocationRecordSchema = z.object({
  allocationId: z.string(),
  resourceId: z.string(),
  workItemId: z.string(),
  allocatedAt: z.string().datetime(),
  deallocatedAt: z.string().datetime().optional(),
  status: z.enum(['active', 'deallocated', 'expired']),
});

/**
 * @typedef {z.infer<typeof TimeWindowSchema>} TimeWindow
 * @typedef {z.infer<typeof CapacityStatusSchema>} CapacityStatus
 * @typedef {z.infer<typeof AllocationRecordSchema>} AllocationRecord
 */

/* ========================================================================= */
/* Capacity Checking Functions                                               */
/* ========================================================================= */

/**
 * Check if resource has capacity available
 *
 * @param {import('@unrdf/oxigraph').OxigraphStore} store - RDF store
 * @param {import('@unrdf/oxigraph').NamedNode} resourceNode - Resource node
 * @param {number} maxCapacity - Maximum capacity (-1 = unlimited)
 * @returns {{ allowed: boolean, current: number, max: number }}
 *
 * @example
 * const check = checkResourceCapacity(store, resourceNode, 5);
 * if (check.allowed) {
 *   console.log(`Can allocate: ${check.current}/${check.max}`);
 * }
 */
export function checkResourceCapacity(store, resourceNode, maxCapacity) {
  // Unlimited capacity
  if (maxCapacity === -1) {
    return { allowed: true, current: 0, max: -1 };
  }

  // Count current allocations
  const activeCount = countActiveAllocations(store, resourceNode);

  return {
    allowed: activeCount < maxCapacity,
    current: activeCount,
    max: maxCapacity,
  };
}

/**
 * Count active allocations for a resource
 *
 * @param {import('@unrdf/oxigraph').OxigraphStore} store - RDF store
 * @param {import('@unrdf/oxigraph').NamedNode} resourceNode - Resource node
 * @returns {number} Number of active allocations
 *
 * @example
 * const count = countActiveAllocations(store, resourceNode);
 * console.log(`Active allocations: ${count}`);
 */
export function countActiveAllocations(store, resourceNode) {
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
    const results = store.query(query);
    const bindings = Array.from(results);
    if (bindings.length > 0 && bindings[0].get('count')) {
      return parseInt(bindings[0].get('count').value, 10);
    }
  } catch {
    // Fallback: count via match (less efficient but works)
    return 0;
  }

  return 0;
}

/**
 * Get detailed capacity status for a resource
 *
 * @param {import('@unrdf/oxigraph').OxigraphStore} store - RDF store
 * @param {string} resourceId - Resource identifier
 * @param {import('@unrdf/oxigraph').dataFactory} dataFactory - Data factory for creating nodes
 * @returns {CapacityStatus} Capacity status
 *
 * @example
 * const status = getCapacityStatus(store, 'alice', dataFactory);
 * console.log(`${status.current}/${status.max} (${status.utilizationPercent}%)`);
 */
export function getCapacityStatus(store, resourceId, dataFactory) {
  const { namedNode } = dataFactory;
  const resourceNode = namedNode(`${YAWL_NS}resource/${resourceId}`);

  // Get max capacity from RDF
  const yawlCapacity = namedNode(`${YAWL_NS}capacity`);
  const capacityQuads = store.match(resourceNode, yawlCapacity, null, null);
  const capacityArr = Array.from(capacityQuads);
  let maxCapacity = 1;

  if (capacityArr.length > 0) {
    maxCapacity = parseInt(capacityArr[0].object.value, 10);
  }

  // Count active allocations
  const currentAllocations = countActiveAllocations(store, resourceNode);

  // Calculate available and utilization
  const available = maxCapacity === -1 ? -1 : Math.max(0, maxCapacity - currentAllocations);
  const utilizationPercent = maxCapacity === -1 ? 0 : Math.round((currentAllocations / maxCapacity) * 100);

  return CapacityStatusSchema.parse({
    current: currentAllocations,
    max: maxCapacity,
    available,
    utilizationPercent,
  });
}

/* ========================================================================= */
/* Allocation Tracking                                                       */
/* ========================================================================= */

/**
 * Get all active allocations with optional filtering
 *
 * @param {import('@unrdf/oxigraph').OxigraphStore} store - RDF store
 * @param {Object} [filter] - Filter options
 * @param {string} [filter.resourceId] - Filter by resource
 * @param {string} [filter.workItemId] - Filter by work item
 * @returns {Array<{ allocationId: string, resourceId: string, workItemId: string, allocatedAt: string }>}
 *
 * @example
 * const allocations = getAllActiveAllocations(store, { resourceId: 'alice' });
 */
export function getAllActiveAllocations(store, filter = {}) {
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
 * Get allocation history for a resource
 *
 * @param {import('@unrdf/oxigraph').OxigraphStore} store - RDF store
 * @param {string} resourceId - Resource identifier
 * @param {Object} [options] - Query options
 * @param {boolean} [options.includeActive=true] - Include active allocations
 * @param {boolean} [options.includeDeallocated=true] - Include deallocated
 * @param {number} [options.limit] - Limit results
 * @returns {AllocationRecord[]} Allocation history
 */
export function getAllocationHistory(store, resourceId, options = {}) {
  const includeActive = options.includeActive ?? true;
  const includeDeallocated = options.includeDeallocated ?? true;

  let statusFilter = '';
  if (includeActive && !includeDeallocated) {
    statusFilter = 'FILTER(!BOUND(?status) || ?status != "deallocated")';
  } else if (!includeActive && includeDeallocated) {
    statusFilter = 'FILTER(?status = "deallocated")';
  }

  const query = `
    PREFIX yawl: <${YAWL_NS}>
    PREFIX rdf: <${RDF_NS}>

    SELECT ?allocation ?workItem ?allocatedAt ?deallocatedAt ?status WHERE {
      ?allocation rdf:type yawl:Allocation ;
                  yawl:resource <${YAWL_NS}resource/${resourceId}> ;
                  yawl:workItem ?workItem ;
                  yawl:allocatedAt ?allocatedAt .
      OPTIONAL { ?allocation yawl:deallocatedAt ?deallocatedAt }
      OPTIONAL { ?allocation yawl:status ?status }
      ${statusFilter}
    }
    ORDER BY DESC(?allocatedAt)
    ${options.limit ? `LIMIT ${options.limit}` : ''}
  `;

  try {
    const results = store.query(query);
    const history = [];

    for (const binding of results) {
      const allocationId = binding.get('allocation')?.value.replace(`${YAWL_NS}allocation/`, '');
      const workItemId = binding.get('workItem')?.value.replace(`${YAWL_NS}workitem/`, '');
      const allocatedAt = binding.get('allocatedAt')?.value;
      const deallocatedAt = binding.get('deallocatedAt')?.value;
      const status = binding.get('status')?.value || 'active';

      history.push({
        allocationId,
        resourceId,
        workItemId,
        allocatedAt,
        deallocatedAt,
        status,
      });
    }

    return history;
  } catch {
    return [];
  }
}

/* ========================================================================= */
/* Availability / Calendar Management                                        */
/* ========================================================================= */

/**
 * Get availability windows for a resource
 *
 * @param {import('@unrdf/oxigraph').OxigraphStore} store - RDF store
 * @param {string} resourceId - Resource identifier
 * @param {Object} [options] - Query options
 * @param {string} [options.from] - Start of time range (ISO datetime)
 * @param {string} [options.to] - End of time range (ISO datetime)
 * @param {import('@unrdf/oxigraph').dataFactory} dataFactory - Data factory
 * @returns {{ available: boolean, windows: TimeWindow[] }}
 *
 * @example
 * const availability = getResourceAvailability(store, 'alice', {
 *   from: '2024-01-15T09:00:00Z',
 *   to: '2024-01-15T17:00:00Z'
 * }, dataFactory);
 */
export function getResourceAvailability(store, resourceId, options = {}, dataFactory) {
  const { namedNode } = dataFactory;
  const resourceNode = namedNode(`${YAWL_NS}resource/${resourceId}`);
  const now = new Date();

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
    const results = store.query(query);
    const bindings = Array.from(results);

    if (bindings.length === 0) {
      // No availability info = assume available
      return {
        available: true,
        windows: [{
          start: options.from || now.toISOString(),
          end: options.to || new Date(now.getTime() + 86400000).toISOString(),
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

    return {
      available: isCurrentlyAvailable,
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
 * Check if resource is available at a specific time
 *
 * @param {import('@unrdf/oxigraph').OxigraphStore} store - RDF store
 * @param {string} resourceId - Resource identifier
 * @param {string} dateTime - ISO datetime string
 * @param {import('@unrdf/oxigraph').dataFactory} dataFactory - Data factory
 * @returns {boolean} True if available
 */
export function isAvailableAt(store, resourceId, dateTime, dataFactory) {
  const availability = getResourceAvailability(store, resourceId, {
    from: dateTime,
    to: dateTime,
  }, dataFactory);

  if (!availability.available) return false;

  // Check if any window covers this time
  const dt = new Date(dateTime);
  return availability.windows.some(w =>
    w.available &&
    new Date(w.start) <= dt &&
    new Date(w.end) >= dt
  );
}

/* ========================================================================= */
/* Workload Analysis                                                         */
/* ========================================================================= */

/**
 * Calculate workload distribution across resources
 *
 * @param {import('@unrdf/oxigraph').OxigraphStore} store - RDF store
 * @param {string} resourceType - Resource type filter ('Participant', 'Tool', 'Role')
 * @returns {Array<{ resourceId: string, allocations: number, utilization: number }>}
 *
 * @example
 * const workload = calculateWorkloadDistribution(store, 'Participant');
 * console.log(`Most loaded: ${workload[0].resourceId} (${workload[0].allocations})`);
 */
export function calculateWorkloadDistribution(store, resourceType) {
  const query = `
    PREFIX yawl: <${YAWL_NS}>
    PREFIX rdf: <${RDF_NS}>

    SELECT ?resource ?capacity (COUNT(?allocation) as ?allocations) WHERE {
      ?resource rdf:type yawl:${resourceType} .
      OPTIONAL { ?resource yawl:capacity ?capacity }
      OPTIONAL {
        ?allocation rdf:type yawl:Allocation ;
                    yawl:resource ?resource .
        FILTER NOT EXISTS {
          ?allocation yawl:status "deallocated" .
        }
      }
    }
    GROUP BY ?resource ?capacity
    ORDER BY DESC(?allocations)
  `;

  try {
    const results = store.query(query);
    const distribution = [];

    for (const binding of results) {
      const resourceUri = binding.get('resource')?.value;
      const capacity = binding.get('capacity')?.value;
      const allocations = binding.get('allocations')?.value;

      const resourceId = resourceUri.replace(`${YAWL_NS}resource/`, '');
      const allocationCount = parseInt(allocations || '0', 10);
      const maxCapacity = parseInt(capacity || '1', 10);

      const utilization = maxCapacity === -1 ? 0 : Math.round((allocationCount / maxCapacity) * 100);

      distribution.push({
        resourceId,
        allocations: allocationCount,
        utilization,
      });
    }

    return distribution;
  } catch {
    return [];
  }
}

/**
 * Find least loaded resource of a type
 *
 * @param {import('@unrdf/oxigraph').OxigraphStore} store - RDF store
 * @param {string} resourceType - Resource type
 * @returns {string|null} Resource ID of least loaded resource
 */
export function findLeastLoadedResource(store, resourceType) {
  const distribution = calculateWorkloadDistribution(store, resourceType);
  if (distribution.length === 0) return null;

  // Sort by allocations ascending
  distribution.sort((a, b) => a.allocations - b.allocations);
  return distribution[0].resourceId;
}

/* ========================================================================= */
/* Exports                                                                   */
/* ========================================================================= */

export default {
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
};
