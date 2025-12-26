/**
 * @fileoverview Shared constants for AUTONOMIC_INNOVATION integration
 * @module agent-1/constants
 */

/**
 * Agent identifiers
 * @type {ReadonlyArray<string>}
 */
export const AGENT_IDS = Object.freeze([
  'agent-2',
  'agent-3',
  'agent-4',
  'agent-5',
  'agent-6',
  'agent-7',
  'agent-8',
  'agent-9',
  'agent-10',
]);

/**
 * Human-readable agent names
 * @type {Readonly<Record<string, string>>}
 */
export const AGENT_NAMES = Object.freeze({
  'agent-2': 'Capsules',
  'agent-3': 'Lenses',
  'agent-4': 'Impact Sets',
  'agent-5': 'Commutativity',
  'agent-6': 'Conventions',
  'agent-7': 'Generator',
  'agent-8': 'Store',
  'agent-9': 'Shadow',
  'agent-10': 'Quality',
});

/**
 * Required exports per agent
 * @type {Readonly<Record<string, ReadonlyArray<string>>>}
 */
export const REQUIRED_EXPORTS = Object.freeze({
  'agent-2': ['planCapsule', 'applyCapsule', 'verifyCapsule', 'canonicalize', 'hashCapsule'],
  'agent-3': ['defineLens', 'compileLens', 'executeLensToGraph', 'executeLensFromGraph'],
  'agent-4': ['computeImpactSet'],
  'agent-5': ['canReorder', 'conflictCertificate'],
  'agent-6': ['compileProfile', 'validateAgainstProfile', 'diagnosticReport'],
  'agent-7': ['generateFacade'],
  'agent-8': ['atomicApply', 'replayFromReceipt'],
  'agent-9': ['shadowWrite', 'shadowRead', 'partialServe', 'mismatchReport'],
  'agent-10': ['runQualityGates', 'e2eValidate'],
});

/**
 * Integration status values
 * @enum {string}
 */
export const AGENT_STATUS = Object.freeze({
  AVAILABLE: 'AVAILABLE',
  STUB: 'STUB',
  ERROR: 'ERROR',
});

/**
 * Version information
 * @type {Readonly<{version: string, buildDate: string}>}
 */
export const VERSION_INFO = Object.freeze({
  version: '0.1.0',
  buildDate: new Date().toISOString().split('T')[0],
});
