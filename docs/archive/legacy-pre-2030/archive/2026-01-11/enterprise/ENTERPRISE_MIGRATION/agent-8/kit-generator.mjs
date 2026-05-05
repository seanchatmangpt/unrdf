/**
 * @fileoverview Main kit generator - generates complete domain migration kits
 * @module agent-8/kit-generator
 */

import { createKitStructure, addAdapter, addScenario, setFacade, validateKit } from './kit-template.mjs';
import { generateFacade } from './facade-generator.mjs';
import { generateAdapter, generateAdapterModule } from './adapter-generator.mjs';
import { generateScenarios, generateScenarioModule } from './scenario-generator.mjs';

/**
 * Generate complete domain kit from contracts
 *
 * @param {string} domain - Domain name
 * @param {Array<Object>} contracts - Domain contracts with lenses
 * @returns {Object} Complete domain kit
 */
export function generateDomainKit(domain, contracts) {
  // Create base kit structure
  let kit = createKitStructure(domain);

  // Generate facade
  const facadeCode = generateFacade(domain, contracts);
  kit = setFacade(kit, facadeCode);

  // Generate adapters from lenses
  for (const contract of contracts) {
    if (contract.lens) {
      const adapter = generateAdapter(contract.operation, contract.lens);
      kit = addAdapter(kit, adapter);
    }
  }

  // Generate test scenarios
  const scenarios = generateScenarios(domain, contracts);
  for (const scenario of scenarios) {
    kit = addScenario(kit, scenario);
  }

  // Validate and return
  validateKit(kit);

  return kit;
}

/**
 * Generate all domain kits from contract registry
 *
 * @param {Object} contractRegistry - Registry of all contracts by domain
 * @returns {Object} All domain kits
 */
export function generateAllKits(contractRegistry) {
  const kits = {};

  for (const [domain, contracts] of Object.entries(contractRegistry)) {
    kits[domain] = generateDomainKit(domain, contracts);
  }

  return kits;
}

/**
 * Write kit to filesystem
 *
 * @param {Object} kit - Domain kit
 * @param {string} outputPath - Base output path
 * @returns {Object} Written file paths
 */
export function writeKitToFS(kit, outputPath) {
  const domainPath = `${outputPath}/${kit.domain}`;

  // Extract lenses for adapter generation
  const lenses = kit.adapters.map(a => ({
    operation: a.operation,
    view: a.toSubstrate,
    review: a.fromSubstrate
  }));

  const files = {
    facade: `${domainPath}/facade.mjs`,
    adapters: `${domainPath}/adapters.mjs`,
    scenarios: `${domainPath}/scenarios.mjs`,
    metadata: `${domainPath}/metadata.json`
  };

  const fileContents = {
    facade: kit.facade,
    adapters: generateAdapterModule(kit.domain, lenses),
    scenarios: generateScenarioModule(kit.domain, kit.scenarios),
    metadata: JSON.stringify(kit.metadata, null, 2)
  };

  return { files, contents: fileContents };
}

/**
 * Generate kit summary statistics
 *
 * @param {Object} kit - Domain kit
 * @returns {Object} Summary statistics
 */
export function generateKitSummary(kit) {
  return {
    domain: kit.domain,
    version: kit.version,
    components: {
      facade: kit.facade.length > 0,
      adapters: kit.adapters.length,
      scenarios: kit.scenarios.length
    },
    scenarioBreakdown: {
      happy: kit.scenarios.filter(s => s.type === 'happy').length,
      error: kit.scenarios.filter(s => s.type === 'error').length,
      edge: kit.scenarios.filter(s => s.type === 'edge').length
    },
    metadata: kit.metadata
  };
}

/**
 * Validate all kits
 *
 * @param {Object} kits - All domain kits
 * @returns {Object} Validation results
 */
export function validateAllKits(kits) {
  const results = {
    total: Object.keys(kits).length,
    valid: 0,
    invalid: 0,
    errors: []
  };

  for (const [domain, kit] of Object.entries(kits)) {
    try {
      validateKit(kit);
      results.valid++;
    } catch (error) {
      results.invalid++;
      results.errors.push({ domain, error: error.message });
    }
  }

  return results;
}

/**
 * Generate deterministic kit (same contracts = same kit)
 *
 * @param {string} domain - Domain name
 * @param {Array<Object>} contracts - Domain contracts
 * @returns {Object} Deterministic kit
 */
export function generateDeterministicKit(domain, contracts) {
  // Sort contracts by operation name for determinism
  const sortedContracts = [...contracts].sort((a, b) =>
    a.operation.localeCompare(b.operation)
  );

  return generateDomainKit(domain, sortedContracts);
}
