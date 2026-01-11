/**
 * @fileoverview Agent 8 - Domain Kit Generator
 * Generates per-domain migration kits from contracts
 * @module agent-8
 */

export {
  generateDomainKit,
  generateAllKits,
  writeKitToFS,
  generateKitSummary,
  validateAllKits,
  generateDeterministicKit
} from './kit-generator.mjs';

export {
  createKitStructure,
  addAdapter,
  addScenario,
  setFacade,
  validateKit,
  getKitStats
} from './kit-template.mjs';

export {
  generateFacade,
  generateJSDoc,
  generateOperationFacade
} from './facade-generator.mjs';

export {
  generateAdapter,
  generateAdapterModule,
  generateErrorHandling,
  generateBatchAdapters
} from './adapter-generator.mjs';

export {
  generateScenarios,
  generateScenarioModule
} from './scenario-generator.mjs';

/**
 * Agent 8 metadata
 */
export const metadata = {
  agent: 'agent-8',
  name: 'Domain Kit Generator',
  version: '1.0.0',
  objective: 'Generate per-domain migration kit scaffolds from contracts',
  capabilities: [
    'kit-generation',
    'facade-generation',
    'adapter-generation',
    'scenario-generation',
    'deterministic-output'
  ],
  domains: [
    'oxigraph',
    'hooks',
    'streaming',
    'federation',
    'validation'
  ]
};
