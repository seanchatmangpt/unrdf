/**
 * @file How-To 01: Discover Capabilities
 * @module agents/how-to/discover-capabilities
 * 
 * @description
 * Task-focused guide for discovering available UNRDF capabilities via SPARQL.
 * This guide solves the problem: "How do I find what capabilities are available?"
 * 
 * @targetAudience AutonomicAgent, HyperintelligentSystem
 * @format machine-executable
 */

import { createDarkMatterCore } from 'unrdf';

/**
 * Problem: Need to discover available capabilities matching criteria
 * 
 * @param {Object} criteria - Search criteria
 * @param {string} [criteria.domain] - Capability domain (e.g., 'validation', 'query')
 * @param {number} [criteria.maxLatency] - Maximum p99 latency in ms
 * @param {number} [criteria.minThroughput] - Minimum throughput
 * @returns {Promise<Array>} Matching capabilities
 */
export async function discoverCapabilitiesByCriteria(criteria = {}) {
  const system = await createDarkMatterCore();
  
  let query = `
    PREFIX cap: <urn:unrdf:capability:>
    PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
    PREFIX slo: <urn:unrdf:slo:>
    
    SELECT ?capability ?name ?description ?latency ?throughput ?domain WHERE {
      ?capability a cap:Capability ;
                  rdfs:label ?name ;
                  rdfs:comment ?description ;
                  slo:p99Latency ?latency ;
                  slo:throughput ?throughput .
      OPTIONAL { ?capability cap:domain ?domain }
  `;
  
  const filters = [];
  if (criteria.domain) {
    filters.push(`FILTER(CONTAINS(LCASE(?name), "${criteria.domain.toLowerCase()}") || ?domain = "${criteria.domain}")`);
  }
  if (criteria.maxLatency) {
    filters.push(`FILTER(?latency < ${criteria.maxLatency})`);
  }
  if (criteria.minThroughput) {
    filters.push(`FILTER(?throughput >= ${criteria.minThroughput})`);
  }
  
  if (filters.length > 0) {
    query += filters.join(' . ') + ' .';
  }
  
  query += `
    }
    ORDER BY ?latency
  `;
  
  const results = await system.query({
    query,
    type: 'sparql-select'
  });
  
  return results.map(binding => ({
    uri: binding.capability.value,
    name: binding.name.value,
    description: binding.description?.value,
    latency: parseFloat(binding.latency.value),
    throughput: parseFloat(binding.throughput.value),
    domain: binding.domain?.value
  }));
}

/**
 * Problem: Need to find capabilities for a specific task
 * 
 * @param {string} taskDescription - Natural language task description
 * @returns {Promise<Array>} Recommended capabilities
 */
export async function findCapabilitiesForTask(taskDescription) {
  // Extract keywords from task description
  const keywords = taskDescription.toLowerCase().split(/\s+/);
  
  // Map keywords to capability domains
  const domainMap = {
    'validate': 'validation',
    'check': 'validation',
    'query': 'query',
    'search': 'query',
    'hook': 'hook',
    'trigger': 'hook',
    'monitor': 'monitoring',
    'observe': 'monitoring',
    'mapek': 'mapek',
    'autonomic': 'mapek',
    'analyze': 'analysis'
  };
  
  const domains = keywords
    .map(k => domainMap[k])
    .filter(Boolean)
    .filter((v, i, a) => a.indexOf(v) === i); // unique
  
  // Search for capabilities in identified domains
  const allCapabilities = [];
  for (const domain of domains) {
    const caps = await discoverCapabilitiesByCriteria({ domain });
    allCapabilities.push(...caps);
  }
  
  // Deduplicate and sort by relevance
  const unique = Array.from(
    new Map(allCapabilities.map(c => [c.uri, c])).values()
  );
  
  return unique.sort((a, b) => a.latency - b.latency);
}

/**
 * Problem: Need to verify capability preconditions before invocation
 * 
 * @param {string} capabilityUri - URI of capability to verify
 * @returns {Promise<Object>} Precondition verification result
 */
export async function verifyCapabilityPreconditions(capabilityUri) {
  const system = await createDarkMatterCore();
  
  // Query preconditions
  const preconditions = await system.query({
    query: `
      PREFIX cap: <urn:unrdf:capability:>
      
      SELECT ?precondition ?type ?value ?description WHERE {
        <${capabilityUri}> cap:precondition ?precondition .
        ?precondition cap:type ?type ;
                      cap:value ?value ;
                      cap:description ?description .
      }
    `,
    type: 'sparql-select'
  });
  
  // Verify each precondition
  const verificationResults = [];
  for (const binding of preconditions) {
    const type = binding.type.value;
    const value = binding.value.value;
    const description = binding.description?.value;
    
    let satisfied = false;
    let actualValue = null;
    let error = null;
    
    try {
      switch (type) {
        case 'runtime':
          satisfied = typeof globalThis !== 'undefined';
          actualValue = typeof globalThis;
          break;
        case 'nodeVersion':
          const required = parseFloat(value);
          const actual = parseFloat(process.versions.node.split('.')[0]);
          satisfied = actual >= required;
          actualValue = actual;
          break;
        case 'module':
          try {
            await import(value);
            satisfied = true;
            actualValue = 'loaded';
          } catch (e) {
            satisfied = false;
            actualValue = 'not-found';
            error = e.message;
          }
          break;
        case 'memory':
          const required = parseInt(value);
          const actual = process.memoryUsage().heapUsed;
          satisfied = actual < required;
          actualValue = actual;
          break;
        default:
          error = `Unknown precondition type: ${type}`;
      }
    } catch (e) {
      error = e.message;
    }
    
    verificationResults.push({
      precondition: binding.precondition.value,
      type,
      required: value,
      actual: actualValue,
      satisfied,
      description,
      error
    });
  }
  
  return {
    capability: capabilityUri,
    preconditions: verificationResults,
    allSatisfied: verificationResults.every(r => r.satisfied),
    failedPreconditions: verificationResults.filter(r => !r.satisfied)
  };
}

// Export for machine execution
export default {
  discoverCapabilitiesByCriteria,
  findCapabilitiesForTask,
  verifyCapabilityPreconditions
};

