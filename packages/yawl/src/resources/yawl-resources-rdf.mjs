/**
 * @fileoverview YAWL Resource RDF Storage Helpers
 *
 * Helper functions for storing resources in RDF
 *
 * @module @unrdf/yawl/resources/rdf
 * @version 1.0.0
 */

import { dataFactory } from '@unrdf/oxigraph';
import {
  YAWL_NS,
  FOAF_NS,
  RDF_NS,
  XSD_NS,
  yawl,
  foaf,
  rdf,
  xsd,
} from './yawl-resources-types.mjs';

const { namedNode, literal, quad, defaultGraph } = dataFactory;

/**
 * Store policy pack in RDF
 *
 * @param {import('@unrdf/oxigraph').OxigraphStore} store - RDF store
 * @param {import('./yawl-resources-core.mjs').PolicyPack} policyPack - Policy pack to store
 * @returns {void}
 */
export function storePolicyPackRDF(store, policyPack) {
  const packNode = namedNode(`${YAWL_NS}policypack/${policyPack.id}`);

  store.add(quad(packNode, rdf('type'), yawl('PolicyPack'), defaultGraph()));

  if (policyPack.name) {
    store.add(quad(packNode, foaf('name'), literal(policyPack.name), defaultGraph()));
  }

  store.add(quad(
    packNode,
    yawl('priority'),
    literal(String(policyPack.priority || 0), namedNode(xsd('integer'))),
    defaultGraph()
  ));

  store.add(quad(
    packNode,
    yawl('enabled'),
    literal(String(policyPack.enabled !== false), namedNode(xsd('boolean'))),
    defaultGraph()
  ));

  for (const resource of policyPack.resources) {
    const resourceNode = namedNode(`${YAWL_NS}resource/${resource.id}`);
    store.add(quad(packNode, yawl('hasResource'), resourceNode, defaultGraph()));
    storeResourceRDF(store, resource, resourceNode);
  }
}

/**
 * Store resource definition in RDF
 *
 * @param {import('@unrdf/oxigraph').OxigraphStore} store - RDF store
 * @param {import('./yawl-resources-core.mjs').Resource} resource - Resource to store
 * @param {import('@unrdf/oxigraph').NamedNode} resourceNode - Resource RDF node
 * @returns {void}
 */
export function storeResourceRDF(store, resource, resourceNode) {
  store.add(quad(resourceNode, rdf('type'), yawl(resource.type), defaultGraph()));

  if (resource.name) {
    store.add(quad(resourceNode, foaf('name'), literal(resource.name), defaultGraph()));
  }

  store.add(quad(
    resourceNode,
    yawl('capacity'),
    literal(String(resource.capacity), namedNode(xsd('integer'))),
    defaultGraph()
  ));

  if (resource.sparql) {
    store.add(quad(
      resourceNode,
      yawl('eligibilitySparql'),
      literal(resource.sparql),
      defaultGraph()
    ));
  }
}

/**
 * Create allocation in RDF
 *
 * @param {import('@unrdf/oxigraph').OxigraphStore} store - RDF store
 * @param {string} allocationId - Allocation identifier
 * @param {import('./yawl-resources-core.mjs').WorkItem} workItem - Work item
 * @param {import('./yawl-resources-core.mjs').Resource} resource - Resource
 * @param {number} [duration] - Allocation duration in milliseconds
 * @returns {void}
 */
export function createAllocationRDF(store, allocationId, workItem, resource, duration) {
  const allocationNode = namedNode(`${YAWL_NS}allocation/${allocationId}`);
  const resourceNode = namedNode(`${YAWL_NS}resource/${resource.id}`);
  const workItemNode = namedNode(`${YAWL_NS}workitem/${workItem.id}`);

  const now = new Date();

  store.add(quad(allocationNode, rdf('type'), yawl('Allocation'), defaultGraph()));
  store.add(quad(allocationNode, yawl('resource'), resourceNode, defaultGraph()));
  store.add(quad(allocationNode, yawl('workItem'), workItemNode, defaultGraph()));
  store.add(quad(
    allocationNode,
    yawl('allocatedAt'),
    literal(now.toISOString(), namedNode(xsd('dateTime'))),
    defaultGraph()
  ));
  store.add(quad(allocationNode, yawl('status'), literal('active'), defaultGraph()));

  if (duration) {
    store.add(quad(
      allocationNode,
      yawl('expiresAt'),
      literal(new Date(now.getTime() + duration).toISOString(), namedNode(xsd('dateTime'))),
      defaultGraph()
    ));
  }
}

/**
 * Count active allocations for a resource
 *
 * @param {import('@unrdf/oxigraph').OxigraphStore} store - RDF store
 * @param {import('@unrdf/oxigraph').NamedNode} resourceNode - Resource node
 * @returns {number} Count of active allocations
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
    const allocations = store.match(null, yawl('resource'), resourceNode, null);
    return Array.from(allocations).length;
  }

  return 0;
}

/**
 * Store resource pool in RDF
 *
 * @param {import('@unrdf/oxigraph').OxigraphStore} store - RDF store
 * @param {Object} poolConfig - Pool configuration
 * @returns {void}
 */
export function storeResourcePoolRDF(store, poolConfig) {
  const poolNode = namedNode(`${YAWL_NS}pool/${poolConfig.id}`);

  store.add(quad(poolNode, rdf('type'), yawl('ResourcePool'), defaultGraph()));

  if (poolConfig.name) {
    store.add(quad(poolNode, foaf('name'), literal(poolConfig.name), defaultGraph()));
  }

  store.add(quad(
    poolNode,
    yawl('allocationStrategy'),
    literal(poolConfig.allocationStrategy),
    defaultGraph()
  ));

  for (const resource of poolConfig.resources) {
    const resourceNode = namedNode(`${YAWL_NS}resource/${resource.id}`);
    storeResourceRDF(store, resource, resourceNode);
    store.add(quad(poolNode, yawl('hasResource'), resourceNode, defaultGraph()));
  }
}
