/**
 * @fileoverview Agent 1: Orchestrator & Integrator
 * Central integration point for all AUTONOMIC_INNOVATION agents (2-10)
 * @module agent-1
 */

import { AGENT_IDS, AGENT_NAMES, REQUIRED_EXPORTS, AGENT_STATUS, VERSION_INFO } from './constants.mjs';

/**
 * Agent export cache
 * @type {Map<string, Object>}
 */
const agentCache = new Map();

/**
 * Create stub exports for missing agents
 * @param {string} agentId - Agent identifier
 * @returns {Object} Stub exports
 */
function createStub(agentId) {
  const requiredExports = REQUIRED_EXPORTS[agentId] || [];
  const stub = {};

  for (const exportName of requiredExports) {
    stub[exportName] = (...args) => {
      throw new Error(`[${agentId}] ${exportName} not available - agent not implemented`);
    };
  }

  return stub;
}

/**
 * Load agent exports dynamically
 * @param {string} agentId - Agent identifier (e.g., 'agent-2')
 * @returns {Promise<Object>} Agent exports with status
 */
async function loadAgent(agentId) {
  if (agentCache.has(agentId)) {
    return agentCache.get(agentId);
  }

  try {
    const module = await import(`../${agentId}/index.mjs`);
    const result = {
      status: AGENT_STATUS.AVAILABLE,
      exports: module,
      error: null,
    };
    agentCache.set(agentId, result);
    return result;
  } catch (err) {
    const result = {
      status: AGENT_STATUS.STUB,
      exports: createStub(agentId),
      error: err.message,
    };
    agentCache.set(agentId, result);
    return result;
  }
}

/**
 * Get agent exports (cached or stub)
 * @param {string} agentId - Agent identifier
 * @returns {Promise<Object>} Agent exports
 */
export async function getAgentExports(agentId) {
  return await loadAgent(agentId);
}

/**
 * Validate all agent integrations
 * @returns {Promise<Array<Object>>} Validation results
 */
export async function validateIntegration() {
  const results = [];

  for (const agentId of AGENT_IDS) {
    const agent = await loadAgent(agentId);
    const requiredExports = REQUIRED_EXPORTS[agentId] || [];
    const missing = requiredExports.filter(name => !agent.exports[name]);

    results.push({
      agentId,
      name: AGENT_NAMES[agentId],
      status: agent.status,
      missing,
      error: agent.error,
    });
  }

  return results;
}

/**
 * Get integration status summary
 * @returns {Promise<Object>} Status summary
 */
export async function getIntegrationStatus() {
  const validation = await validateIntegration();

  const available = validation.filter(r => r.status === AGENT_STATUS.AVAILABLE).length;
  const stubs = validation.filter(r => r.status === AGENT_STATUS.STUB).length;
  const errors = validation.filter(r => r.status === AGENT_STATUS.ERROR).length;

  return {
    total: AGENT_IDS.length,
    available,
    stubs,
    errors,
    version: VERSION_INFO.version,
    agents: validation,
  };
}

/**
 * Pre-load all agents
 * @returns {Promise<void>}
 */
export async function preloadAgents() {
  await Promise.all(AGENT_IDS.map(id => loadAgent(id)));
}

// Export constants
export { AGENT_IDS, AGENT_NAMES, REQUIRED_EXPORTS, AGENT_STATUS, VERSION_INFO };

// Export types marker
export { TYPES_EXPORTED } from './types.mjs';
