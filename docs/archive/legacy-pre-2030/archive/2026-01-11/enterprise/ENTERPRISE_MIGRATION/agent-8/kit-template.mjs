/**
 * @fileoverview Kit template structure for domain migration kits
 * @module agent-8/kit-template
 */

/**
 * @typedef {Object} DomainKit
 * @property {string} domain - Domain name
 * @property {string} version - Kit version
 * @property {string} facade - Facade module code
 * @property {Array<Adapter>} adapters - Adapter functions
 * @property {Array<Scenario>} scenarios - Test scenarios
 * @property {Object} metadata - Kit metadata
 */

/**
 * @typedef {Object} Adapter
 * @property {string} name - Adapter name
 * @property {string} operation - Operation name
 * @property {Function} toSubstrate - Convert legacy to substrate
 * @property {Function} fromSubstrate - Convert substrate to legacy
 */

/**
 * @typedef {Object} Scenario
 * @property {string} name - Scenario name
 * @property {string} type - Scenario type (happy|error|edge)
 * @property {*} input - Input data
 * @property {*} expectedOutput - Expected output
 * @property {Object} metadata - Scenario metadata
 */

/**
 * Create empty kit structure for a domain
 *
 * @param {string} domain - Domain name
 * @returns {DomainKit} Empty kit structure
 */
export function createKitStructure(domain) {
  return {
    domain,
    version: '1.0.0',
    facade: '',
    adapters: [],
    scenarios: [],
    metadata: {
      generatedAt: new Date().toISOString(),
      generator: 'agent-8',
      status: 'scaffold'
    }
  };
}

/**
 * Add adapter to kit
 *
 * @param {DomainKit} kit - Domain kit
 * @param {Adapter} adapter - Adapter to add
 * @returns {DomainKit} Updated kit
 */
export function addAdapter(kit, adapter) {
  return {
    ...kit,
    adapters: [...kit.adapters, adapter],
    metadata: {
      ...kit.metadata,
      lastModified: new Date().toISOString()
    }
  };
}

/**
 * Add scenario to kit
 *
 * @param {DomainKit} kit - Domain kit
 * @param {Scenario} scenario - Scenario to add
 * @returns {DomainKit} Updated kit
 */
export function addScenario(kit, scenario) {
  return {
    ...kit,
    scenarios: [...kit.scenarios, scenario],
    metadata: {
      ...kit.metadata,
      lastModified: new Date().toISOString()
    }
  };
}

/**
 * Set facade code for kit
 *
 * @param {DomainKit} kit - Domain kit
 * @param {string} facadeCode - Facade module code
 * @returns {DomainKit} Updated kit
 */
export function setFacade(kit, facadeCode) {
  return {
    ...kit,
    facade: facadeCode,
    metadata: {
      ...kit.metadata,
      lastModified: new Date().toISOString()
    }
  };
}

/**
 * Validate kit structure
 *
 * @param {DomainKit} kit - Domain kit to validate
 * @returns {boolean} True if valid
 * @throws {Error} If kit is invalid
 */
export function validateKit(kit) {
  if (!kit.domain || typeof kit.domain !== 'string') {
    throw new Error('Kit must have a valid domain name');
  }

  if (!kit.version || typeof kit.version !== 'string') {
    throw new Error('Kit must have a valid version');
  }

  if (!Array.isArray(kit.adapters)) {
    throw new Error('Kit adapters must be an array');
  }

  if (!Array.isArray(kit.scenarios)) {
    throw new Error('Kit scenarios must be an array');
  }

  return true;
}

/**
 * Get kit statistics
 *
 * @param {DomainKit} kit - Domain kit
 * @returns {Object} Kit statistics
 */
export function getKitStats(kit) {
  return {
    domain: kit.domain,
    version: kit.version,
    adapterCount: kit.adapters.length,
    scenarioCount: kit.scenarios.length,
    hasFacade: kit.facade.length > 0,
    metadata: kit.metadata
  };
}
