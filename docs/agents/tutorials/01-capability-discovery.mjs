/**
 * @file Tutorial 01: Capability Discovery
 * @module agents/tutorials/capability-discovery
 * 
 * @description
 * Machine-executable tutorial for discovering UNRDF capabilities via SPARQL.
 * This tutorial demonstrates how agents can introspect available capabilities,
 * verify preconditions, and invoke them programmatically.
 * 
 * @targetAudience AutonomicAgent, HyperintelligentSystem
 * @format machine-executable
 */

import { createDarkMatterCore } from 'unrdf';
import { z } from 'zod';

/**
 * Step 1: Initialize system and discover capabilities
 * 
 * @returns {Promise<Object>} Discovered capabilities
 */
export async function discoverCapabilities() {
  const system = await createDarkMatterCore();
  
  // Query capability registry via SPARQL
  const capabilities = await system.query({
    query: `
      PREFIX cap: <urn:unrdf:capability:>
      PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
      PREFIX slo: <urn:unrdf:slo:>
      
      SELECT ?capability ?name ?description ?latency ?throughput WHERE {
        ?capability a cap:Capability ;
                    rdfs:label ?name ;
                    rdfs:comment ?description ;
                    slo:p99Latency ?latency ;
                    slo:throughput ?throughput .
      }
      ORDER BY ?latency
    `,
    type: 'sparql-select'
  });
  
  return {
    capabilities: capabilities.map(binding => ({
      uri: binding.capability.value,
      name: binding.name.value,
      description: binding.description?.value,
      latency: parseFloat(binding.latency.value),
      throughput: parseFloat(binding.throughput.value)
    }))
  };
}

/**
 * Step 2: Verify preconditions for a capability
 * 
 * @param {string} capabilityUri - URI of capability to verify
 * @returns {Promise<Object>} Precondition verification result
 */
export async function verifyPreconditions(capabilityUri) {
  const system = await createDarkMatterCore();
  
  // Query preconditions for capability
  const preconditions = await system.query({
    query: `
      PREFIX cap: <urn:unrdf:capability:>
      
      SELECT ?precondition ?type ?value WHERE {
        <${capabilityUri}> cap:precondition ?precondition .
        ?precondition cap:type ?type ;
                      cap:value ?value .
      }
    `,
    type: 'sparql-select'
  });
  
  // Verify each precondition
  const results = [];
  for (const binding of preconditions) {
    const type = binding.type.value;
    const value = binding.value.value;
    
    let satisfied = false;
    let actualValue = null;
    
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
        } catch {
          satisfied = false;
          actualValue = 'not-found';
        }
        break;
      case 'memory':
        const required = parseInt(value);
        const actual = process.memoryUsage().heapUsed;
        satisfied = actual < required;
        actualValue = actual;
        break;
    }
    
    results.push({
      precondition: binding.precondition.value,
      type,
      required: value,
      actual: actualValue,
      satisfied
    });
  }
  
  return {
    capability: capabilityUri,
    preconditions: results,
    allSatisfied: results.every(r => r.satisfied)
  };
}

/**
 * Step 3: Invoke a capability with validation
 * 
 * @param {string} capabilityName - Name of capability to invoke
 * @param {Object} input - Input parameters
 * @returns {Promise<Object>} Execution result
 */
export async function invokeCapability(capabilityName, input) {
  const system = await createDarkMatterCore();
  
  // Find capability URI
  const capability = await system.query({
    query: `
      PREFIX cap: <urn:unrdf:capability:>
      PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
      
      SELECT ?capability WHERE {
        ?capability a cap:Capability ;
                    rdfs:label "${capabilityName}" .
      }
      LIMIT 1
    `,
    type: 'sparql-select'
  });
  
  if (capability.length === 0) {
    throw new Error(`Capability "${capabilityName}" not found`);
  }
  
  const capabilityUri = capability[0].capability.value;
  
  // Verify preconditions
  const verification = await verifyPreconditions(capabilityUri);
  if (!verification.allSatisfied) {
    throw new Error('Preconditions not met', { verification });
  }
  
  // Invoke capability based on name
  switch (capabilityName) {
    case 'parseTurtle':
      const { parseTurtle } = await import('unrdf');
      return { result: await parseTurtle(input.content) };
      
    case 'query':
      return { result: await system.query(input) };
      
    case 'defineHook':
      const { defineHook } = await import('unrdf');
      return { result: defineHook(input) };
      
    default:
      throw new Error(`Capability "${capabilityName}" not implemented in tutorial`);
  }
}

/**
 * Complete tutorial execution
 * 
 * @returns {Promise<Object>} Tutorial results
 */
export async function runTutorial() {
  const results = {
    step1: await discoverCapabilities(),
    step2: null,
    step3: null
  };
  
  // Step 2: Verify preconditions for first capability
  if (results.step1.capabilities.length > 0) {
    const firstCap = results.step1.capabilities[0];
    results.step2 = await verifyPreconditions(firstCap.uri);
  }
  
  // Step 3: Invoke a simple capability
  try {
    results.step3 = await invokeCapability('parseTurtle', {
      content: '@prefix ex: <http://example.org/> . ex:test ex:works true .'
    });
  } catch (error) {
    results.step3 = { error: error.message };
  }
  
  return results;
}

// Export for machine execution
export default { discoverCapabilities, verifyPreconditions, invokeCapability, runTutorial };

