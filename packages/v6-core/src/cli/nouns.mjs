/**
 * @fileoverview V6 Canonical Noun Definitions
 *
 * Defines the 10 canonical nouns for v6 CLI unified spine.
 * Every package capability maps to `kgc <noun> <verb>`.
 *
 * Nouns are FROZEN for v6 - no additions without major version bump.
 */

import { z } from 'zod';

/**
 * Noun schema - validates noun structure.
 * @type {z.ZodSchema}
 */
export const NounSchema = z.object({
  name: z.string().describe('Noun identifier (e.g., universe, receipt)'),
  description: z.string().describe('Human-readable description'),
  package: z.string().optional().describe('Primary package that owns this noun'),
  verbs: z.array(z.string()).describe('Allowed verbs for this noun'),
  examples: z.array(z.string()).optional().describe('Example commands')
});

/**
 * Canonical nouns for v6.
 *
 * Order matters for help display but not for execution.
 * Each noun represents a distinct conceptual domain.
 *
 * @type {Array<Object>}
 */
export const CANONICAL_NOUNS = [
  {
    name: 'universe',
    description: 'KGC-4D universe operations - spacetime snapshots and reconstruction',
    package: '@unrdf/kgc-4d',
    verbs: ['create', 'freeze', 'restore', 'verify', 'export'],
    examples: [
      'kgc universe create --name "Test Universe"',
      'kgc universe freeze --id <uuid>',
      'kgc universe restore --snapshot <hash>'
    ]
  },
  {
    name: 'eventlog',
    description: 'Event log operations - append-only event sourcing',
    package: '@unrdf/kgc-4d',
    verbs: ['append', 'replay', 'reconstruct', 'verify', 'export'],
    examples: [
      'kgc eventlog append --event <json>',
      'kgc eventlog replay --from <index>',
      'kgc eventlog reconstruct --to <index>'
    ]
  },
  {
    name: 'receipt',
    description: 'Receipt management - cryptographic proof chains',
    package: '@unrdf/blockchain',
    verbs: ['verify', 'chain', 'anchor', 'export'],
    examples: [
      'kgc receipt verify --hash <sha256>',
      'kgc receipt chain --id <receipt-id>',
      'kgc receipt anchor --hash <merkle-root>'
    ]
  },
  {
    name: 'policy',
    description: 'Hook and policy pack operations - governance rules',
    package: '@unrdf/hooks',
    verbs: ['validate', 'apply', 'test', 'export'],
    examples: [
      'kgc policy validate --file hooks.mjs',
      'kgc policy apply --name pre-commit',
      'kgc policy test --suite integration'
    ]
  },
  {
    name: 'workflow',
    description: 'YAWL workflow operations - process orchestration',
    package: '@unrdf/yawl',
    verbs: ['start', 'pause', 'resume', 'verify', 'export'],
    examples: [
      'kgc workflow start --definition workflow.yaml',
      'kgc workflow pause --id <workflow-id>',
      'kgc workflow verify --definition workflow.yaml'
    ]
  },
  {
    name: 'resource',
    description: 'Resource allocation and management',
    package: '@unrdf/core',
    verbs: ['allocate', 'release', 'query', 'export'],
    examples: [
      'kgc resource allocate --type memory --amount 1GB',
      'kgc resource release --id <resource-id>',
      'kgc resource query --status active'
    ]
  },
  {
    name: 'grammar',
    description: 'SPARQL/SHACL/N3/OWL grammar operations',
    package: '@unrdf/validation',
    verbs: ['validate', 'compile', 'parse', 'export'],
    examples: [
      'kgc grammar validate --file query.sparql',
      'kgc grammar compile --input shapes.shacl',
      'kgc grammar parse --format n3 --file rules.n3'
    ]
  },
  {
    name: 'thesis',
    description: 'Documentation and LaTeX thesis operations',
    package: '@unrdf/docs',
    verbs: ['render', 'compile', 'validate', 'export'],
    examples: [
      'kgc thesis render --input thesis.md --output pdf',
      'kgc thesis compile --file main.tex',
      'kgc thesis validate --citations --references'
    ]
  },
  {
    name: 'package',
    description: 'Package registry and management operations',
    package: '@unrdf/kgc-cli',
    verbs: ['list', 'install', 'validate', 'export'],
    examples: [
      'kgc package list --status enabled',
      'kgc package install --name @unrdf/new-package',
      'kgc package validate --contracts'
    ]
  },
  {
    name: 'delta',
    description: 'Δ (change carrier) operations - admissible state transitions',
    package: '@unrdf/v6-core',
    verbs: ['propose', 'apply', 'verify', 'export'],
    examples: [
      'kgc delta propose --file change.json',
      'kgc delta apply --id <delta-id>',
      'kgc delta verify --id <delta-id> --against <state-hash>'
    ]
  }
];

/**
 * Validate all canonical nouns against schema.
 *
 * @returns {Array<{noun: string, errors: Array}>} Validation errors
 */
export function validateNouns() {
  const errors = [];

  for (const noun of CANONICAL_NOUNS) {
    const result = NounSchema.safeParse(noun);
    if (!result.success) {
      errors.push({
        noun: noun.name,
        errors: result.error.errors
      });
    }
  }

  return errors;
}

/**
 * Get noun definition by name.
 *
 * @param {string} name - Noun name
 * @returns {Object|undefined} Noun definition
 */
export function getNoun(name) {
  return CANONICAL_NOUNS.find(n => n.name === name);
}

/**
 * Get all noun names (for CLI autocomplete/help).
 *
 * @returns {Array<string>} Noun names
 */
export function getNounNames() {
  return CANONICAL_NOUNS.map(n => n.name);
}

/**
 * Build noun-to-package mapping.
 *
 * @returns {Object} Map of noun -> package
 */
export function buildNounPackageMap() {
  return Object.fromEntries(
    CANONICAL_NOUNS.map(n => [n.name, n.package || 'unknown'])
  );
}

export default {
  CANONICAL_NOUNS,
  NounSchema,
  validateNouns,
  getNoun,
  getNounNames,
  buildNounPackageMap
};
