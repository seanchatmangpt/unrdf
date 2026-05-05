/**
 * @fileoverview Agent 7: Convention-Preserving Code Generator
 * @module agent-7
 */

// Core generation functions
export {
  generateFacade,
  templateServiceClass,
  templateErrorHandler,
  templateTest,
  demoCustomerServiceSpec
} from './generator.mjs';

// Determinism utilities
export {
  canonicalizeGenerated,
  hashGeneratedCode,
  hashInputs,
  verifyDeterminism,
  areEquivalent,
  getDiff,
  formatDiff
} from './determinism.mjs';

/**
 * Agent 7 Metadata
 */
export const AGENT_7_METADATA = {
  name: 'Agent 7: Convention-Preserving Code Generator',
  version: '1.0.0',
  description: 'Generate service façade code matching organizational conventions exactly',
  capabilities: [
    'Generate CRUD service classes',
    'Generate error handlers matching profile',
    'Generate test files matching testing framework',
    'Deterministic code generation (same inputs → same outputs)',
    'Convention preservation (naming, logging, error handling)',
    'Template-based generation'
  ],
  inputs: [
    'serviceSpec: { name, entities, operations }',
    'compiledProfile: from Agent 6',
    'compiledLens: from Agent 3'
  ],
  outputs: [
    'code: Service class implementation',
    'testCode: Test file',
    'filename: Generated filename',
    'hash: SHA-256 of canonicalized code',
    'metadata: Generation metadata'
  ],
  guarantees: [
    'Deterministic: Same spec + profile → identical code always',
    'Valid JavaScript: All generated code can be imported/executed',
    'Convention compliance: Matches profile exactly',
    'Reproducible: Same hash across machines, runs, time'
  ]
};
