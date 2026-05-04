/**
 * @fileoverview Real Project Snapshot Fixtures
 *
 * Realistic Node.js project structure for E2E testing:
 * - Multiple file types and sizes
 * - Typical directory structure
 * - Dependencies and configuration
 *
 * @module @unrdf/kgc-probe/test/fixtures/real-project-snapshot
 */

import { FROZEN_TIMESTAMP } from './frozen-environment.mjs';

// ============================================================================
// PROJECT STRUCTURE
// ============================================================================

/**
 * Complete project structure
 */
export const PROJECT_STRUCTURE = {
  name: 'sample-rdf-project',
  version: '1.0.0',
  type: 'module',
  rootDir: '/home/user/sample-project',

  directories: [
    'src',
    'src/agents',
    'src/guards',
    'src/storage',
    'test',
    'test/fixtures',
    'test/unit',
    'test/integration',
    'docs',
    'scripts',
    'node_modules'
  ],

  files: {
    'package.json': {
      type: 'json',
      size: 1240,
      content: {
        name: 'sample-rdf-project',
        version: '1.0.0',
        type: 'module',
        main: './src/index.mjs',
        scripts: {
          test: 'vitest run',
          lint: 'eslint src/',
          build: 'echo "ESM - no build needed"'
        },
        dependencies: {
          '@unrdf/oxigraph': 'workspace:*',
          '@unrdf/kgc-4d': 'workspace:*',
          'zod': '^4.1.13'
        },
        devDependencies: {
          'vitest': '^4.0.15',
          'eslint': '^9.17.0'
        }
      }
    },
    'tsconfig.json': {
      type: 'json',
      size: 420,
      content: {
        compilerOptions: {
          target: 'ES2022',
          module: 'NodeNext',
          moduleResolution: 'NodeNext',
          strict: true,
          noEmit: true,
          allowJs: true,
          checkJs: true
        },
        include: ['src/**/*', 'test/**/*']
      }
    },
    'vitest.config.mjs': {
      type: 'mjs',
      size: 380,
      content: `import { defineConfig } from 'vitest/config';
export default defineConfig({
  test: {
    environment: 'node',
    include: ['test/**/*.test.mjs'],
    coverage: {
      provider: 'v8',
      lines: 80
    }
  }
});`
    },
    '.eslintrc.json': {
      type: 'json',
      size: 250,
      content: {
        root: true,
        env: { es2022: true, node: true },
        extends: ['eslint:recommended']
      }
    },
    'README.md': {
      type: 'md',
      size: 2500,
      content: '# Sample RDF Project\n\nA sample project for testing KGC Probe...'
    },
    'src/index.mjs': {
      type: 'mjs',
      size: 450,
      content: `/**
 * @fileoverview Main entry point
 */
export { createStore } from './store.mjs';
export { runQuery } from './query.mjs';
export { validateGraph } from './validate.mjs';
`
    },
    'src/store.mjs': {
      type: 'mjs',
      size: 1200,
      content: `/**
 * @fileoverview RDF Store implementation
 */
export function createStore() {
  const quads = new Map();
  return {
    add: (quad) => quads.set(quad.subject.value, quad),
    get: (subject) => quads.get(subject),
    size: () => quads.size
  };
}
`
    },
    'src/query.mjs': {
      type: 'mjs',
      size: 850,
      content: `/**
 * @fileoverview SPARQL query execution
 */
export async function runQuery(store, query) {
  // Simple query execution
  return [];
}
`
    },
    'src/validate.mjs': {
      type: 'mjs',
      size: 680,
      content: `/**
 * @fileoverview Graph validation
 */
export function validateGraph(store) {
  return { valid: true, errors: [] };
}
`
    },
    'src/agents/completion.mjs': {
      type: 'mjs',
      size: 920,
      content: `/**
 * @fileoverview Completion agent
 */
export class CompletionAgent {
  async scan(config) {
    return [];
  }
}
`
    },
    'src/guards/quality.mjs': {
      type: 'mjs',
      size: 750,
      content: `/**
 * @fileoverview Quality guard
 */
export function validateQuality(observations) {
  return [];
}
`
    },
    'src/storage/memory.mjs': {
      type: 'mjs',
      size: 580,
      content: `/**
 * @fileoverview Memory storage
 */
export class MemoryStorage {
  constructor() { this.store = new Map(); }
  async save(key, value) { this.store.set(key, value); }
  async load(key) { return this.store.get(key); }
}
`
    },
    'test/store.test.mjs': {
      type: 'mjs',
      size: 1100,
      content: `import { describe, it, expect } from 'vitest';
import { createStore } from '../src/store.mjs';

describe('Store', () => {
  it('should create empty store', () => {
    const store = createStore();
    expect(store.size()).toBe(0);
  });
});
`
    },
    'docs/architecture.md': {
      type: 'md',
      size: 3200,
      content: '# Architecture\n\nThis document describes the architecture...'
    }
  }
};

// ============================================================================
// GRAPH DATA
// ============================================================================

/**
 * Sample RDF triples for the project
 */
export const SAMPLE_TRIPLES = [
  { subject: 'ex:Person1', predicate: 'rdf:type', object: 'schema:Person' },
  { subject: 'ex:Person1', predicate: 'schema:name', object: '"John Doe"' },
  { subject: 'ex:Person1', predicate: 'schema:email', object: '"john@example.com"' },
  { subject: 'ex:Person2', predicate: 'rdf:type', object: 'schema:Person' },
  { subject: 'ex:Person2', predicate: 'schema:name', object: '"Jane Smith"' },
  { subject: 'ex:Organization1', predicate: 'rdf:type', object: 'schema:Organization' },
  { subject: 'ex:Organization1', predicate: 'schema:name', object: '"Acme Corp"' },
  { subject: 'ex:Person1', predicate: 'schema:worksFor', object: 'ex:Organization1' },
  { subject: 'ex:Person2', predicate: 'schema:worksFor', object: 'ex:Organization1' },
  { subject: 'ex:Project1', predicate: 'rdf:type', object: 'ex:Project' },
  { subject: 'ex:Project1', predicate: 'ex:hasOwner', object: 'ex:Organization1' },
  { subject: 'ex:Project1', predicate: 'schema:name', object: '"KGC Probe"' }
];

/**
 * Sample observations for E2E testing
 */
export const SAMPLE_OBSERVATIONS = [
  {
    id: 'e2e-obs-001',
    agent: 'completion',
    timestamp: FROZEN_TIMESTAMP,
    kind: 'completeness',
    severity: 'warning',
    subject: 'ex:Person2',
    predicate: 'schema:email',
    evidence: {
      query: 'SELECT ?s WHERE { ?s a schema:Person. MINUS { ?s schema:email ?e } }',
      result: { count: 1 },
      witnesses: ['ex:Person2']
    },
    metrics: {
      confidence: 0.95,
      coverage: 0.83,
      latency_ms: 45
    },
    tags: ['missing_property', 'email']
  },
  {
    id: 'e2e-obs-002',
    agent: 'consistency',
    timestamp: FROZEN_TIMESTAMP,
    kind: 'consistency',
    severity: 'info',
    subject: 'ex:Organization1',
    predicate: 'schema:employee',
    evidence: {
      query: 'SELECT ?org WHERE { ?p schema:worksFor ?org }',
      result: { employeeCount: 2 },
      witnesses: ['ex:Person1', 'ex:Person2']
    },
    metrics: {
      confidence: 0.98,
      coverage: 1.0,
      latency_ms: 32
    },
    tags: ['employee_relation']
  },
  {
    id: 'e2e-obs-003',
    agent: 'coverage',
    timestamp: FROZEN_TIMESTAMP,
    kind: 'coverage',
    severity: 'info',
    subject: 'graph:default',
    evidence: {
      query: 'SELECT (COUNT(*) as ?count) WHERE { ?s ?p ?o }',
      result: { tripleCount: 12 },
      witnesses: []
    },
    metrics: {
      confidence: 1.0,
      coverage: 1.0,
      latency_ms: 15
    },
    tags: ['triple_count']
  }
];

// ============================================================================
// E2E WORKFLOW DATA
// ============================================================================

/**
 * Expected probe scan result
 */
export const EXPECTED_SCAN_RESULT = {
  status: 'success',
  observation_count: 3,
  agent_count: 10,
  guard_count: 5,
  execution_time_max_ms: 5000
};

/**
 * Expected artifact structure
 */
export const EXPECTED_ARTIFACT_STRUCTURE = {
  version: '1.0',
  universe_id: 'test-universe',
  required_fields: [
    'version',
    'universe_id',
    'snapshot_id',
    'generated_at',
    'probe_run_id',
    'shard_count',
    'shard_hash',
    'observations',
    'summary',
    'metadata',
    'integrity'
  ],
  summary_fields: [
    'total',
    'by_kind',
    'by_severity',
    'confidence_mean',
    'coverage_mean'
  ],
  integrity_fields: [
    'checksum'
  ]
};

/**
 * Validation criteria for E2E test
 */
export const E2E_VALIDATION_CRITERIA = {
  scan: {
    max_duration_ms: 30000,
    min_observations: 1,
    required_agents: ['completion', 'consistency']
  },
  merge: {
    max_duration_ms: 1000,
    deduplication_required: true
  },
  verify: {
    max_duration_ms: 10000,
    checksum_must_match: true,
    schema_must_validate: true
  },
  total: {
    max_duration_ms: 45000
  }
};

/**
 * Mock RDF store for E2E testing
 */
export function createMockStore() {
  const quads = new Map();

  for (const triple of SAMPLE_TRIPLES) {
    const key = `${triple.subject}|${triple.predicate}`;
    quads.set(key, triple);
  }

  return {
    type: 'mock',
    getSize: () => quads.size,
    getQuads: () => Array.from(quads.values()),
    match: (s, p, o) => {
      return Array.from(quads.values()).filter(q => {
        if (s && q.subject !== s) return false;
        if (p && q.predicate !== p) return false;
        if (o && q.object !== o) return false;
        return true;
      });
    }
  };
}

/**
 * E2E test configuration
 */
export const E2E_CONFIG = {
  universe_id: 'test-universe',
  snapshot_id: 'snap-e2e-001',
  distributed: false,
  persist: true,
  timeout_ms: 30000
};

export default {
  PROJECT_STRUCTURE,
  SAMPLE_TRIPLES,
  SAMPLE_OBSERVATIONS,
  EXPECTED_SCAN_RESULT,
  EXPECTED_ARTIFACT_STRUCTURE,
  E2E_VALIDATION_CRITERIA,
  createMockStore,
  E2E_CONFIG
};
