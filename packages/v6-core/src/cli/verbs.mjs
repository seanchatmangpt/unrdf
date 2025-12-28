/**
 * @fileoverview V6 Canonical Verb Definitions
 *
 * Defines verbs and their noun associations.
 * Every verb MUST emit a receipt for auditability.
 */

import { z } from 'zod';

/**
 * Verb schema - validates verb structure.
 * @type {z.ZodSchema}
 */
export const VerbSchema = z.object({
  name: z.string().describe('Verb identifier (e.g., compile, verify)'),
  description: z.string().describe('What this verb does'),
  emitsReceipt: z.boolean().default(true).describe('Whether verb emits receipt'),
  requiresAuth: z.boolean().default(false).describe('Requires authentication'),
  sideEffects: z.enum(['none', 'read', 'write', 'mutate']).describe('Side effect category'),
  applicableNouns: z.array(z.string()).describe('Nouns this verb can operate on'),
  argsSchema: z.custom(() => true).optional().describe('Zod schema for arguments')
});

/**
 * Canonical verbs for v6.
 *
 * Each verb represents a distinct operation type.
 * Verbs are reusable across multiple nouns.
 *
 * @type {Array<Object>}
 */
export const CANONICAL_VERBS = [
  {
    name: 'compile',
    description: 'AOT compilation or transformation',
    emitsReceipt: true,
    requiresAuth: false,
    sideEffects: 'write',
    applicableNouns: ['grammar', 'thesis']
  },
  {
    name: 'verify',
    description: 'Cryptographic or structural verification',
    emitsReceipt: true,
    requiresAuth: false,
    sideEffects: 'read',
    applicableNouns: ['universe', 'eventlog', 'receipt', 'workflow', 'delta', 'grammar', 'thesis']
  },
  {
    name: 'freeze',
    description: 'Create immutable snapshot',
    emitsReceipt: true,
    requiresAuth: false,
    sideEffects: 'write',
    applicableNouns: ['universe']
  },
  {
    name: 'reconstruct',
    description: 'Rebuild state from event log',
    emitsReceipt: true,
    requiresAuth: false,
    sideEffects: 'mutate',
    applicableNouns: ['eventlog', 'universe']
  },
  {
    name: 'replay',
    description: 'Replay events from log',
    emitsReceipt: true,
    requiresAuth: false,
    sideEffects: 'read',
    applicableNouns: ['eventlog', 'workflow']
  },
  {
    name: 'allocate',
    description: 'Allocate resources',
    emitsReceipt: true,
    requiresAuth: true,
    sideEffects: 'mutate',
    applicableNouns: ['resource']
  },
  {
    name: 'validate',
    description: 'Schema or rule validation',
    emitsReceipt: true,
    requiresAuth: false,
    sideEffects: 'read',
    applicableNouns: ['policy', 'grammar', 'thesis', 'package']
  },
  {
    name: 'render',
    description: 'Render to output format',
    emitsReceipt: true,
    requiresAuth: false,
    sideEffects: 'write',
    applicableNouns: ['thesis']
  },
  {
    name: 'export',
    description: 'Export to external format',
    emitsReceipt: true,
    requiresAuth: false,
    sideEffects: 'write',
    applicableNouns: ['universe', 'eventlog', 'receipt', 'policy', 'workflow', 'resource', 'grammar', 'thesis', 'package', 'delta']
  },
  {
    name: 'create',
    description: 'Create new entity',
    emitsReceipt: true,
    requiresAuth: false,
    sideEffects: 'mutate',
    applicableNouns: ['universe']
  },
  {
    name: 'restore',
    description: 'Restore from snapshot',
    emitsReceipt: true,
    requiresAuth: false,
    sideEffects: 'mutate',
    applicableNouns: ['universe']
  },
  {
    name: 'append',
    description: 'Append to event log',
    emitsReceipt: true,
    requiresAuth: false,
    sideEffects: 'write',
    applicableNouns: ['eventlog']
  },
  {
    name: 'chain',
    description: 'Show chain/lineage',
    emitsReceipt: true,
    requiresAuth: false,
    sideEffects: 'read',
    applicableNouns: ['receipt']
  },
  {
    name: 'anchor',
    description: 'Anchor to merkle tree',
    emitsReceipt: true,
    requiresAuth: true,
    sideEffects: 'write',
    applicableNouns: ['receipt']
  },
  {
    name: 'apply',
    description: 'Apply change/policy',
    emitsReceipt: true,
    requiresAuth: true,
    sideEffects: 'mutate',
    applicableNouns: ['policy', 'delta']
  },
  {
    name: 'test',
    description: 'Test execution',
    emitsReceipt: true,
    requiresAuth: false,
    sideEffects: 'read',
    applicableNouns: ['policy']
  },
  {
    name: 'start',
    description: 'Start workflow/process',
    emitsReceipt: true,
    requiresAuth: true,
    sideEffects: 'mutate',
    applicableNouns: ['workflow']
  },
  {
    name: 'pause',
    description: 'Pause execution',
    emitsReceipt: true,
    requiresAuth: true,
    sideEffects: 'mutate',
    applicableNouns: ['workflow']
  },
  {
    name: 'resume',
    description: 'Resume execution',
    emitsReceipt: true,
    requiresAuth: true,
    sideEffects: 'mutate',
    applicableNouns: ['workflow']
  },
  {
    name: 'release',
    description: 'Release resources',
    emitsReceipt: true,
    requiresAuth: true,
    sideEffects: 'mutate',
    applicableNouns: ['resource']
  },
  {
    name: 'query',
    description: 'Query state',
    emitsReceipt: false,
    requiresAuth: false,
    sideEffects: 'read',
    applicableNouns: ['resource']
  },
  {
    name: 'parse',
    description: 'Parse input',
    emitsReceipt: true,
    requiresAuth: false,
    sideEffects: 'read',
    applicableNouns: ['grammar']
  },
  {
    name: 'list',
    description: 'List entities',
    emitsReceipt: false,
    requiresAuth: false,
    sideEffects: 'read',
    applicableNouns: ['package']
  },
  {
    name: 'install',
    description: 'Install package',
    emitsReceipt: true,
    requiresAuth: true,
    sideEffects: 'mutate',
    applicableNouns: ['package']
  },
  {
    name: 'propose',
    description: 'Propose change delta',
    emitsReceipt: true,
    requiresAuth: false,
    sideEffects: 'write',
    applicableNouns: ['delta']
  }
];

/**
 * Validate all verbs against schema.
 *
 * @returns {Array<{verb: string, errors: Array}>} Validation errors
 */
export function validateVerbs() {
  const errors = [];

  for (const verb of CANONICAL_VERBS) {
    const result = VerbSchema.safeParse(verb);
    if (!result.success) {
      errors.push({
        verb: verb.name,
        errors: result.error.errors
      });
    }
  }

  return errors;
}

/**
 * Get verb definition by name.
 *
 * @param {string} name - Verb name
 * @returns {Object|undefined} Verb definition
 */
export function getVerb(name) {
  return CANONICAL_VERBS.find(v => v.name === name);
}

/**
 * Get verbs applicable to a noun.
 *
 * @param {string} noun - Noun name
 * @returns {Array<Object>} Verb definitions
 */
export function getVerbsForNoun(noun) {
  return CANONICAL_VERBS.filter(v => v.applicableNouns.includes(noun));
}

/**
 * Build noun-verb matrix.
 *
 * @returns {Object} Map of noun -> array of verbs
 */
export function buildNounVerbMatrix() {
  const matrix = {};

  for (const verb of CANONICAL_VERBS) {
    for (const noun of verb.applicableNouns) {
      if (!matrix[noun]) {
        matrix[noun] = [];
      }
      matrix[noun].push(verb.name);
    }
  }

  // Sort verb arrays for deterministic output
  for (const noun of Object.keys(matrix)) {
    matrix[noun].sort();
  }

  return matrix;
}

/**
 * Check if noun-verb combination is valid.
 *
 * @param {string} noun - Noun name
 * @param {string} verb - Verb name
 * @returns {boolean} Whether combination is valid
 */
export function isValidCombination(noun, verb) {
  const verbDef = getVerb(verb);
  return verbDef ? verbDef.applicableNouns.includes(noun) : false;
}

export default {
  CANONICAL_VERBS,
  VerbSchema,
  validateVerbs,
  getVerb,
  getVerbsForNoun,
  buildNounVerbMatrix,
  isValidCombination
};
