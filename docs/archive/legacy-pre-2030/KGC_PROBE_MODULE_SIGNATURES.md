# @unrdf/kgc-probe - Module Signatures & Type Stubs

**Reference Guide**: All public APIs and their TypeScript-style signatures (using JSDoc)

---

## 1. Main Entry Point: index.mjs

```javascript
/**
 * @unrdf/kgc-probe - Main entry point
 * Exports all public APIs for knowledge graph validation
 */

// ============= SCHEMAS =============
/** @type {typeof import('./schemas/observation.schema.mjs').ObservationSchema} */
export const ObservationSchema;

/** @type {typeof import('./schemas/probe-result.schema.mjs').ProbeResultSchema} */
export const ProbeResultSchema;

/** @type {typeof import('./schemas/guard-policy.schema.mjs').GuardPolicySchema} */
export const GuardPolicySchema;

/** @type {typeof import('./schemas/merge-config.schema.mjs').MergeConfigSchema} */
export const MergeConfigSchema;

/** @type {typeof import('./schemas/receipt-metadata.schema.mjs').ReceiptMetadataSchema} */
export const ReceiptMetadataSchema;

// ============= AGENTS =============
/** @type {typeof import('./agents/index.mjs').createProbeOrchestrator} */
export const createProbeOrchestrator;

/** @type {typeof import('./agents/index.mjs').getProbeRegistry} */
export const getProbeRegistry;

/** @type {typeof import('./agents/security-probe.mjs').SecurityProbe} */
export const SecurityProbe;

/** 9 more probes... */

// ============= GUARDS =============
/** @type {typeof import('./guards/index.mjs').createGuardComposer} */
export const createGuardComposer;

/** @type {typeof import('./guards/observation-guard.mjs').ObservationGuard} */
export const ObservationGuard;

/** 4 more guards... */

// ============= STORAGE =============
/** @type {typeof import('./storage/index.mjs').createProbeStore} */
export const createProbeStore;

/** @type {typeof import('./storage/graph-builder.mjs').GraphBuilder} */
export const GraphBuilder;

/** @type {typeof import('./storage/triple-generator.mjs').TripleGenerator} */
export const TripleGenerator;

/** @type {typeof import('./storage/namespaces.mjs').RDFNamespaces} */
export const RDFNamespaces;

// ============= ORCHESTRATION =============
/** @type {typeof import('./orchestrator/index.mjs').ProbeOrchestrator} */
export const ProbeOrchestrator;

/** @type {typeof import('./orchestrator/merge-engine.mjs').MergeEngine} */
export const MergeEngine;

/** @type {typeof import('./orchestrator/conflict-resolver.mjs').ConflictResolver} */
export const ConflictResolver;

/** @type {typeof import('./orchestrator/aggregator.mjs').Aggregator} */
export const Aggregator;

// ============= RECEIPTS =============
/** @type {typeof import('./receipts/index.mjs').createReceiptBuilder} */
export const createReceiptBuilder;

/** @type {typeof import('./receipts/merkle-integrator.mjs').MerkleIntegrator} */
export const MerkleIntegrator;

/** @type {typeof import('./receipts/verification.mjs').ReceiptVerifier} */
export const ReceiptVerifier;

// ============= CLI =============
/** @type {typeof import('./cli/index.mjs').registerProbeCommands} */
export const registerProbeCommands;

// ============= UTILITIES =============
/** @type {typeof import('./utils/logger.mjs').createLogger} */
export const createLogger;

/** @type {typeof import('./utils/error-handler.mjs').KGCProbeError} */
export const KGCProbeError;

/** @type {typeof import('./utils/error-handler.mjs').ValidationError} */
export const ValidationError;

// ============= DEFAULT FACTORY =============
/** @type {{
 *   createProbeOrchestrator: typeof createProbeOrchestrator,
 *   createProbeStore: typeof createProbeStore,
 *   createReceiptBuilder: typeof createReceiptBuilder,
 *   createLogger: typeof createLogger
 * }}
 */
export default { createProbeOrchestrator, createProbeStore, createReceiptBuilder, createLogger };
```

---

## 2. Schemas Directory

### observation.schema.mjs

```javascript
import { z } from 'zod';

/**
 * @typedef {Object} Observation
 * @property {Object} subject - RDF subject (namedNode or blankNode)
 * @property {Object} predicate - RDF predicate (namedNode)
 * @property {Object} object - RDF object (namedNode, blankNode, or literal)
 * @property {Object} [graph] - RDF graph term (defaults to defaultGraph)
 * @property {Object} metadata - Observation metadata
 * @property {string} metadata.timestamp - ISO 8601 timestamp
 * @property {string} metadata.source - Agent name/ID
 * @property {number} metadata.confidence - Confidence [0.0, 1.0]
 * @property {Object} [metadata.context] - Additional context
 */

/** @type {z.ZodType<Observation>} */
export const ObservationSchema = z.object({
  subject: z.instanceof(Object).describe('RDF subject term'),
  predicate: z.instanceof(Object).describe('RDF predicate (namedNode)'),
  object: z.instanceof(Object).describe('RDF object term'),
  graph: z.instanceof(Object).optional().describe('RDF graph term'),
  metadata: z.object({
    timestamp: z.string().datetime().describe('ISO 8601 timestamp'),
    source: z.string().min(1).describe('Source agent ID'),
    confidence: z.number().min(0).max(1).describe('Confidence score'),
    context: z.record(z.unknown()).optional().describe('Context metadata'),
  }).describe('Observation metadata'),
});

/**
 * Validate observation against schema
 * @param {any} data - Data to validate
 * @returns {Observation} Validated observation
 * @throws {z.ZodError} If validation fails
 */
export function validateObservation(data) {
  return ObservationSchema.parse(data);
}
```

### probe-result.schema.mjs

```javascript
import { z } from 'zod';

/**
 * @typedef {Object} Assertion
 * @property {string} id - Unique assertion ID
 * @property {'pass'|'fail'|'warning'|'skip'} status - Assertion status
 * @property {string} evidence - Evidence/explanation
 * @property {number} weight - Weight in score calculation [0.0, 1.0]
 */

/**
 * @typedef {Object} ProbeResult
 * @property {string} probe_id - UUID of probe execution
 * @property {'security'|'performance'|'correctness'|...} domain - Probe domain
 * @property {'pass'|'fail'|'warning'|'skip'} status - Overall status
 * @property {number} score - Score [0.0, 1.0]
 * @property {Assertion[]} assertions - Detailed assertions
 * @property {number} duration_ms - Execution time in milliseconds
 * @property {bigint} timestamp_ns - Execution timestamp (nanoseconds)
 * @property {Object} metadata - Additional metadata
 */

export const AssertionSchema = z.object({
  id: z.string().min(1).describe('Assertion ID'),
  status: z.enum(['pass', 'fail', 'warning', 'skip']).describe('Assertion status'),
  evidence: z.string().describe('Evidence/explanation'),
  weight: z.number().min(0).max(1).describe('Weight in score'),
});

export const ProbeResultSchema = z.object({
  probe_id: z.string().uuid().describe('Probe execution ID'),
  domain: z.string().min(1).describe('Probe domain'),
  status: z.enum(['pass', 'fail', 'warning', 'skip']).describe('Overall status'),
  score: z.number().min(0).max(1).describe('Score [0.0, 1.0]'),
  assertions: z.array(AssertionSchema).describe('Detailed assertions'),
  duration_ms: z.number().nonnegative().describe('Execution time'),
  timestamp_ns: z.bigint().describe('Timestamp (nanoseconds)'),
  metadata: z.record(z.unknown()).optional().describe('Metadata'),
});

/**
 * Validate probe result
 * @param {any} data - Data to validate
 * @returns {ProbeResult} Validated result
 * @throws {z.ZodError} If validation fails
 */
export function validateProbeResult(data) {
  return ProbeResultSchema.parse(data);
}
```

### guard-policy.schema.mjs

```javascript
import { z } from 'zod';

/**
 * @typedef {Object} RejectionCriteria
 * @property {string} path - JSONPath expression (e.g., "score")
 * @property {string} condition - Condition (e.g., "< 0.5")
 * @property {string} message - Error message
 */

/**
 * @typedef {Object} GuardPolicy
 * @property {'strict'|'warn'|'skip'} enforce_mode - Enforcement mode
 * @property {RejectionCriteria[]} rejection_criteria - Rejection rules
 * @property {number} allowed_failures - Failures before rejection
 * @property {number} timeout_ms - Timeout in milliseconds
 */

export const RejectionCriteriaSchema = z.object({
  path: z.string().describe('JSONPath expression'),
  condition: z.string().describe('Condition predicate'),
  message: z.string().describe('Error message'),
});

export const GuardPolicySchema = z.object({
  enforce_mode: z.enum(['strict', 'warn', 'skip']).describe('Enforcement mode'),
  rejection_criteria: z.array(RejectionCriteriaSchema).describe('Rejection rules'),
  allowed_failures: z.number().nonnegative().int().describe('Failure threshold'),
  timeout_ms: z.number().positive().int().default(5000).describe('Timeout'),
});

/**
 * Validate guard policy
 * @param {any} data - Data to validate
 * @returns {GuardPolicy} Validated policy
 * @throws {z.ZodError} If validation fails
 */
export function validateGuardPolicy(data) {
  return GuardPolicySchema.parse(data);
}
```

### merge-config.schema.mjs

```javascript
import { z } from 'zod';

/**
 * @typedef {Object} MergeConfig
 * @property {'consensus'|'max'|'min'|'weighted_sum'} strategy - Merge strategy
 * @property {'highest_confidence'|'newest_first'|'manual'} conflict_resolution - Conflict strategy
 * @property {string} aggregate_by - Grouping key
 * @property {Object<string, number>} weights - Domain weights (for weighted_sum)
 */

export const MergeConfigSchema = z.object({
  strategy: z.enum(['consensus', 'max', 'min', 'weighted_sum']).describe('Merge strategy'),
  conflict_resolution: z.enum(['highest_confidence', 'newest_first', 'manual']).describe('Conflict resolution'),
  aggregate_by: z.string().default('domain').describe('Grouping key'),
  weights: z.record(z.string(), z.number().positive()).optional().describe('Domain weights'),
});

/**
 * Validate merge configuration
 * @param {any} data - Data to validate
 * @returns {MergeConfig} Validated configuration
 * @throws {z.ZodError} If validation fails
 */
export function validateMergeConfig(data) {
  return MergeConfigSchema.parse(data);
}
```

---

## 3. Agents Directory

### agents/index.mjs

```javascript
/**
 * @typedef {Object} Probe
 * @property {string} domain - Probe domain ID
 * @property {string} name - Human-readable name
 * @property {string} version - Probe version
 * @property {(observation: Observation) => Promise<ProbeResult>} execute - Execute probe
 */

/**
 * Get registry of all available probes
 * @returns {Map<string, typeof Probe>} Probe registry
 *
 * @example
 * const registry = getProbeRegistry();
 * for (const [domain, ProbeClass] of registry) {
 *   console.log(`${domain}: ${ProbeClass.name}`);
 * }
 */
export function getProbeRegistry() {
  // Implementation
}

/**
 * Create probe orchestrator
 * @param {Object} options - Configuration
 * @param {import('../storage/probe-store.mjs').ProbeStore} options.store - Storage instance
 * @param {import('../guards/index.mjs').GuardComposer} [options.guards] - Guard composer
 * @returns {import('../orchestrator/probe-orchestrator.mjs').ProbeOrchestrator} Orchestrator
 *
 * @example
 * const orchestrator = createProbeOrchestrator({ store });
 * const result = await orchestrator.executeProbe('security', observation);
 */
export function createProbeOrchestrator(options) {
  // Implementation
}

// Export all probe classes
export { SecurityProbe } from './security-probe.mjs';
export { PerformanceProbe } from './performance-probe.mjs';
export { CorrectnessProbe } from './correctness-probe.mjs';
// ... 7 more probes
```

### security-probe.mjs (Example Probe)

```javascript
/**
 * Security Probe - Validates security aspects of knowledge graphs
 * Checks: Authentication, encryption, secrets, injection attacks
 */
export class SecurityProbe {
  /** @type {string} */
  static domain = 'security';

  /** @type {string} */
  static name = 'Security Probe';

  /** @type {string} */
  static version = '1.0.0';

  /**
   * Execute security probe on observation
   * @param {Observation} observation - Input observation
   * @param {Object} [config] - Configuration options
   * @param {number} [config.timeout_ms=5000] - Execution timeout
   * @param {boolean} [config.detailed_evidence=true] - Include detailed evidence
   * @returns {Promise<ProbeResult>} Probe result with score and assertions
   *
   * @example
   * const probe = new SecurityProbe();
   * const result = await probe.execute(observation);
   * console.log(`Score: ${result.score}, Status: ${result.status}`);
   *
   * @throws {KGCProbeError} If execution fails
   */
  async execute(observation, config = {}) {
    // Implementation
  }

  /**
   * Run security validation logic
   * @private
   * @param {Observation} observation - Validated observation
   * @returns {Promise<Assertion[]>} Array of assertions
   */
  async _validate(observation) {
    // Implementation
  }

  /**
   * Check authentication enforcement
   * @private
   * @param {Observation} observation
   * @returns {Assertion} Auth assertion
   */
  _checkAuthentication(observation) {
    // Implementation
  }

  /**
   * Check encryption standards
   * @private
   * @param {Observation} observation
   * @returns {Assertion} Encryption assertion
   */
  _checkEncryption(observation) {
    // Implementation
  }

  // ... other security checks
}
```

---

## 4. Guards Directory

### observation-guard.mjs

```javascript
/**
 * Observation Guard - Validates observations at entry point
 * Performs: Schema validation, RDF term checking, metadata validation
 */
export class ObservationGuard {
  /**
   * Create observation guard
   * @param {Object} [options] - Configuration
   * @param {GuardPolicy} [options.policy] - Enforcement policy
   * @param {Logger} [options.logger] - Logger instance
   */
  constructor(options = {}) {
    // Implementation
  }

  /**
   * Validate observation and throw if invalid
   * @param {any} observation - Observation to validate
   * @returns {Observation} Validated observation
   * @throws {ValidationError} If validation fails
   *
   * @example
   * const guard = new ObservationGuard();
   * const validated = guard.validate(observation);
   */
  validate(observation) {
    // Implementation
  }

  /**
   * Validate observation against policy
   * @param {GuardPolicy} policy - Enforcement policy
   * @param {any} observation - Observation to validate
   * @returns {Observation} Validated observation
   * @throws {KGCProbeError} If policy violation
   */
  enforcePolicy(policy, observation) {
    // Implementation
  }

  /**
   * Check RDF term types
   * @private
   * @param {Object} term - RDF term
   * @throws {ValidationError} If invalid term
   */
  _validateRDFTerm(term) {
    // Implementation
  }

  /**
   * Validate metadata structure
   * @private
   * @param {Object} metadata - Metadata object
   * @throws {ValidationError} If invalid metadata
   */
  _validateMetadata(metadata) {
    // Implementation
  }
}
```

### result-guard.mjs

```javascript
/**
 * Result Guard - Validates probe results before propagation
 * Checks: Score range, assertion invariants, duration validity
 */
export class ResultGuard {
  /**
   * Create result guard
   * @param {Object} [options] - Configuration
   * @param {GuardPolicy} [options.policy] - Enforcement policy
   * @param {Logger} [options.logger] - Logger instance
   */
  constructor(options = {}) {
    // Implementation
  }

  /**
   * Validate probe result
   * @param {any} result - Result to validate
   * @returns {ProbeResult} Validated result
   * @throws {ValidationError} If validation fails
   */
  validate(result) {
    // Implementation
  }

  /**
   * Verify assertion invariants
   * @private
   * @param {Assertion[]} assertions - Assertions to verify
   * @throws {ValidationError} If invariant violated
   */
  _verifyAssertions(assertions) {
    // Implementation
  }

  /**
   * Check score consistency with assertions
   * @private
   * @param {number} score - Score to check
   * @param {Assertion[]} assertions - Supporting assertions
   * @throws {ValidationError} If inconsistent
   */
  _checkScoreConsistency(score, assertions) {
    // Implementation
  }
}
```

### guards/index.mjs

```javascript
/**
 * Guard Composer - Orchestrates all guard validations
 * Provides centralized guard policy enforcement
 */
export class GuardComposer {
  /**
   * Create guard composer
   * @param {Object} [options] - Configuration
   * @param {Logger} [options.logger] - Logger instance
   */
  constructor(options = {}) {
    // Implementation
  }

  /**
   * Get guard by stage
   * @param {'observation'|'result'|'graph'|'merge'|'receipt'} stage - Stage name
   * @returns {ObservationGuard|ResultGuard|GraphGuard|MergeGuard|ReceiptGuard} Guard instance
   */
  getGuard(stage) {
    // Implementation
  }

  /**
   * Enforce all guards at a stage
   * @param {'observation'|'result'|'graph'|'merge'|'receipt'} stage - Stage name
   * @param {any} data - Data to validate
   * @param {GuardPolicy} [policy] - Optional policy override
   * @returns {any} Validated data
   * @throws {ValidationError|KGCProbeError} If validation fails
   */
  async enforceAll(stage, data, policy = null) {
    // Implementation
  }

  /**
   * Create guard composer with default guards
   * @static
   * @param {Object} [options] - Configuration
   * @returns {GuardComposer} Guard composer
   */
  static createDefault(options = {}) {
    // Implementation
  }
}

export { ObservationGuard } from './observation-guard.mjs';
export { ResultGuard } from './result-guard.mjs';
export { GraphGuard } from './graph-guard.mjs';
export { MergeGuard } from './merge-guard.mjs';
export { ReceiptGuard } from './receipt-guard.mjs';
```

---

## 5. Storage Directory

### probe-store.mjs

```javascript
import { KnowledgeStore } from '@unrdf/kgc-substrate';

/**
 * Probe Store - Extends KnowledgeStore with probe-specific methods
 * Provides immutable append-only log for probe results
 */
export class ProbeStore extends KnowledgeStore {
  /**
   * Create probe store
   * @param {Object} [options] - Configuration
   * @param {string} [options.nodeId] - Node ID for vector clock
   * @param {string} [options.gitDir] - Git directory for snapshots
   */
  constructor(options = {}) {
    super(options);
  }

  /**
   * Append probe result as RDF triples
   * @param {ProbeResult} probeResult - Result to append
   * @returns {Promise<{index: bigint}>} Log entry metadata
   *
   * @example
   * const { index } = await store.appendProbeResult(result);
   * console.log(`Result stored at index ${index}`);
   */
  async appendProbeResult(probeResult) {
    // Implementation
  }

  /**
   * Get all probe results for a domain
   * @param {string} domain - Probe domain
   * @returns {Promise<ProbeResult[]>} Array of results
   */
  async getProbeHistory(domain) {
    // Implementation
  }

  /**
   * Generate immutable snapshot of all probes
   * @returns {Promise<StorageSnapshot>} Snapshot metadata
   */
  async generateProbeSnapshot() {
    // Implementation
  }

  /**
   * Query probes by domain and predicate
   * @param {string} domain - Domain to query
   * @param {string} predicate - Predicate URI
   * @returns {Set<Object>} Matching quads
   */
  queryByDomain(domain, predicate) {
    // Implementation
  }
}

/**
 * Create probe store instance
 * @param {Object} [options] - Configuration
 * @returns {Promise<ProbeStore>} Probe store
 *
 * @example
 * const store = await createProbeStore({ nodeId: 'probe-agent-1' });
 */
export async function createProbeStore(options = {}) {
  // Implementation
}
```

### graph-builder.mjs

```javascript
/**
 * Graph Builder - Transforms probe results to RDF quads
 */
export class GraphBuilder {
  /**
   * Create graph builder
   * @param {Object} [options] - Configuration
   * @param {TripleGenerator} [options.tripleGenerator] - Triple generator
   */
  constructor(options = {}) {
    // Implementation
  }

  /**
   * Build RDF quads from observations
   * @param {Observation[]} observations - Observations to convert
   * @returns {Array<Quad>} Array of RDF quads
   */
  buildFromObservations(observations) {
    // Implementation
  }

  /**
   * Build RDF quads from probe result
   * @param {ProbeResult} result - Probe result to convert
   * @returns {Array<Quad>} Array of RDF quads with provenance
   *
   * @example
   * const quads = builder.buildFromProbeResult(result);
   * await store.appendTriple('add', ...);
   */
  buildFromProbeResult(result) {
    // Implementation
  }

  /**
   * Build RDF quads from merged result
   * @param {MergedResult} merged - Merged result to convert
   * @returns {Array<Quad>} Array of RDF quads
   */
  buildFromMergeResult(merged) {
    // Implementation
  }

  /**
   * Add provenance triples
   * @private
   * @param {string} entity - Entity URI
   * @param {string} source - Source agent
   * @param {bigint} timestamp - Timestamp (ns)
   * @returns {Array<Quad>} Provenance quads
   */
  _addProvenanceTriples(entity, source, timestamp) {
    // Implementation
  }
}
```

### triple-generator.mjs

```javascript
/**
 * Triple Generator - Creates RDF quads from domain objects
 */
export class TripleGenerator {
  /**
   * Create triple generator
   * @param {Object} [options] - Configuration
   * @param {Object} [options.namespaces] - RDF namespaces
   */
  constructor(options = {}) {
    // Implementation
  }

  /**
   * Generate RDF quad
   * @param {string} subject - Subject URI
   * @param {string} predicate - Predicate URI
   * @param {string|number|boolean} object - Object value
   * @param {string} [graph] - Graph URI (defaults to default graph)
   * @returns {Quad} RDF quad
   *
   * @example
   * const quad = generator.generateTriple(
   *   'http://kgc.io/result/001',
   *   'http://kgc.io/ontology/score',
   *   '0.92'
   * );
   */
  generateTriple(subject, predicate, object, graph = null) {
    // Implementation
  }

  /**
   * Generate provenance triples for entity
   * @param {string} entity - Entity URI
   * @param {string} source - Source agent
   * @param {bigint} timestamp - Timestamp (nanoseconds)
   * @returns {Array<Quad>} Provenance quads
   */
  generateProvenanceTriples(entity, source, timestamp) {
    // Implementation
  }

  /**
   * Generate metadata triples
   * @param {string} entity - Entity URI
   * @param {Object} metadata - Metadata object
   * @returns {Array<Quad>} Metadata quads
   */
  generateMetadataTriples(entity, metadata) {
    // Implementation
  }

  /**
   * Convert string to safe URI component
   * @static
   * @private
   * @param {string} str - String to slugify
   * @returns {string} Safe URI component
   */
  static slugify(str) {
    // Implementation
  }

  /**
   * Create unique probe result URI
   * @static
   * @private
   * @param {string} domain - Probe domain
   * @returns {string} Probe result URI
   */
  static uriFromDomain(domain) {
    // Implementation
  }

  /**
   * Convert nanosecond timestamp to RDF literal
   * @static
   * @private
   * @param {bigint} ns - Timestamp in nanoseconds
   * @returns {Object} RDF literal term
   */
  static timestampToLiteral(ns) {
    // Implementation
  }
}
```

### namespaces.mjs

```javascript
/**
 * RDF Namespace definitions for kgc-probe ontology
 *
 * @typedef {Object} RDFNamespaces
 * @property {string} kgc - http://kgc.io/ontology/
 * @property {string} probe - http://kgc.io/probe/
 * @property {string} result - http://kgc.io/result/
 * @property {string} security - http://kgc.io/probe/security/
 * @property {string} performance - http://kgc.io/probe/performance/
 * @property {string} correctness - http://kgc.io/probe/correctness/
 * @property {string} structure - http://kgc.io/probe/structure/
 * @property {string} completeness - http://kgc.io/probe/completeness/
 * @property {string} consistency - http://kgc.io/probe/consistency/
 * @property {string} compliance - http://kgc.io/probe/compliance/
 * @property {string} coverage - http://kgc.io/probe/coverage/
 * @property {string} mutation - http://kgc.io/probe/mutation/
 * @property {string} integration - http://kgc.io/probe/integration/
 * @property {string} rdf - http://www.w3.org/1999/02/22-rdf-syntax-ns#
 * @property {string} rdfs - http://www.w3.org/2000/01/rdf-schema#
 * @property {string} xsd - http://www.w3.org/2001/XMLSchema#
 */

export const RDFNamespaces = {
  kgc: 'http://kgc.io/ontology/',
  probe: 'http://kgc.io/probe/',
  result: 'http://kgc.io/result/',
  security: 'http://kgc.io/probe/security/',
  performance: 'http://kgc.io/probe/performance/',
  correctness: 'http://kgc.io/probe/correctness/',
  structure: 'http://kgc.io/probe/structure/',
  completeness: 'http://kgc.io/probe/completeness/',
  consistency: 'http://kgc.io/probe/consistency/',
  compliance: 'http://kgc.io/probe/compliance/',
  coverage: 'http://kgc.io/probe/coverage/',
  mutation: 'http://kgc.io/probe/mutation/',
  integration: 'http://kgc.io/probe/integration/',
  rdf: 'http://www.w3.org/1999/02/22-rdf-syntax-ns#',
  rdfs: 'http://www.w3.org/2000/01/rdf-schema#',
  xsd: 'http://www.w3.org/2001/XMLSchema#',
};

/**
 * Resolve namespace prefix to full URI
 * @param {string} prefix - Namespace prefix
 * @returns {string} Full namespace URI
 * @throws {Error} If prefix not found
 *
 * @example
 * const uri = resolveNamespace('kgc');
 * // Returns: 'http://kgc.io/ontology/'
 */
export function resolveNamespace(prefix) {
  // Implementation
}
```

---

## 6. Orchestrator Directory

### probe-orchestrator.mjs

```javascript
/**
 * Probe Orchestrator - Coordinates all probe executions
 */
export class ProbeOrchestrator {
  /**
   * Create probe orchestrator
   * @param {Object} options - Configuration (required)
   * @param {ProbeStore} options.store - Storage instance
   * @param {GuardComposer} [options.guards] - Guard composer
   * @param {Map<string, typeof Probe>} [options.registry] - Probe registry
   * @param {Logger} [options.logger] - Logger instance
   */
  constructor(options) {
    // Implementation
  }

  /**
   * Execute specific probe on observation
   * @param {string} domain - Probe domain
   * @param {Observation} observation - Input observation
   * @returns {Promise<ProbeResult>} Probe result
   * @throws {KGCProbeError} If probe not found or fails
   *
   * @example
   * const result = await orchestrator.executeProbe('security', observation);
   */
  async executeProbe(domain, observation) {
    // Implementation
  }

  /**
   * Execute all probes on observations
   * @param {Observation[]} observations - Input observations
   * @param {Object} [options] - Execution options
   * @param {boolean} [options.parallel=true] - Run probes in parallel
   * @param {number} [options.timeout_ms=5000] - Execution timeout
   * @returns {Promise<ProbeResult[][]>} Results [obs_idx][probe_idx]
   *
   * @example
   * const allResults = await orchestrator.executeAllProbes(observations);
   */
  async executeAllProbes(observations, options = {}) {
    // Implementation
  }

  /**
   * Merge multiple probe results
   * @param {ProbeResult[][]|ProbeResult[]} results - Results to merge
   * @param {MergeConfig} [config] - Merge configuration
   * @returns {Promise<MergedResult>} Merged result
   *
   * @example
   * const merged = await orchestrator.mergeResults(results);
   */
  async mergeResults(results, config = null) {
    // Implementation
  }

  /**
   * Get probe by domain
   * @private
   * @param {string} domain - Domain name
   * @returns {typeof Probe} Probe class
   * @throws {KGCProbeError} If probe not found
   */
  _getProbe(domain) {
    // Implementation
  }
}
```

### merge-engine.mjs

```javascript
/**
 * @typedef {Object} MergedResult
 * @property {number} overall_score - Aggregated score [0.0, 1.0]
 * @property {Object<string, number>} by_domain - Score per domain
 * @property {Assertion[]} assertions - All assertions
 * @property {Object} statistics - Score statistics (mean, median, stddev)
 * @property {bigint} timestamp_ns - Merge timestamp
 */

/**
 * Merge Engine - Merges probe results from multiple domains
 */
export class MergeEngine {
  /**
   * Create merge engine
   * @param {Object} options - Configuration (required)
   * @param {MergeConfig} options.config - Merge configuration
   * @param {ConflictResolver} [options.conflictResolver] - Conflict resolver
   * @param {Logger} [options.logger] - Logger instance
   */
  constructor(options) {
    // Implementation
  }

  /**
   * Merge multiple probe results
   * @param {ProbeResult[]|ProbeResult[][]} results - Results to merge
   * @returns {Promise<MergedResult>} Merged result
   *
   * @example
   * const merged = await engine.merge(results);
   * console.log(`Overall score: ${merged.overall_score}`);
   */
  async merge(results) {
    // Implementation
  }

  /**
   * Apply merge strategy to scores
   * @private
   * @param {number[]} scores - Scores to merge
   * @param {'consensus'|'max'|'min'|'weighted_sum'} strategy - Merge strategy
   * @returns {number} Merged score
   */
  _applyStrategy(scores, strategy) {
    // Implementation
  }

  /**
   * Check for conflicts in results
   * @private
   * @param {ProbeResult[]} results - Results to check
   * @returns {Array<{domain: string, results: ProbeResult[]}>} Conflicts
   */
  _findConflicts(results) {
    // Implementation
  }

  /**
   * Compute weighted sum score
   * @private
   * @param {Object<string, number>} scores - Domain -> score mapping
   * @param {Object<string, number>} weights - Domain -> weight mapping
   * @returns {number} Weighted score
   */
  _computeWeightedSum(scores, weights) {
    // Implementation
  }
}
```

### aggregator.mjs

```javascript
/**
 * @typedef {Object} AggregatedSummary
 * @property {number} overall_score - [0.0, 1.0]
 * @property {Object<string, number>} by_domain - Per-domain scores
 * @property {number} assertions_passed - Count of passing assertions
 * @property {number} assertions_failed - Count of failing assertions
 * @property {number} assertions_warning - Count of warnings
 * @property {number} assertions_skipped - Count of skipped
 * @property {number} total_duration_ms - Total execution time
 * @property {bigint} timestamp_ns - Aggregation timestamp
 */

/**
 * Aggregator - Produces summary statistics from probe results
 */
export class Aggregator {
  /**
   * Create aggregator
   * @param {Object} [options] - Configuration
   * @param {Logger} [options.logger] - Logger instance
   */
  constructor(options = {}) {
    // Implementation
  }

  /**
   * Aggregate multiple probe results into summary
   * @param {ProbeResult[]|MergedResult} results - Results to aggregate
   * @returns {AggregatedSummary} Summary statistics
   *
   * @example
   * const summary = aggregator.aggregateResults(results);
   * console.log(`${summary.assertions_passed}/${summary.assertions_passed + summary.assertions_failed} passed`);
   */
  aggregateResults(results) {
    // Implementation
  }

  /**
   * Compute score statistics
   * @param {number[]} scores - Scores to analyze
   * @returns {Object} Statistics (mean, median, stddev, min, max)
   */
  getStatistics(scores) {
    // Implementation
  }

  /**
   * Count assertions by status
   * @private
   * @param {Assertion[]} assertions - Assertions to count
   * @returns {Object} Status counts
   */
  _countAssertions(assertions) {
    // Implementation
  }
}
```

---

## 7. Receipts Directory

### receipt-builder.mjs

```javascript
import { BaseReceipt } from '@unrdf/v6-core/receipts/base-receipt.mjs';

/**
 * Receipt Builder - Creates and signs receipts from probe results
 */
export class ReceiptBuilder {
  /**
   * Create receipt builder
   * @param {Object} [options] - Configuration
   * @param {string} [options.privateKeyPath] - Path to private key (RSA-4096)
   * @param {Logger} [options.logger] - Logger instance
   */
  constructor(options = {}) {
    // Implementation
  }

  /**
   * Build receipt from aggregated result
   * @param {AggregatedSummary} aggregated - Aggregated result
   * @returns {Promise<Receipt>} Signed receipt
   *
   * @example
   * const receipt = await builder.build(summary);
   * console.log(`Receipt ID: ${receipt.receipt_id}`);
   */
  async build(aggregated) {
    // Implementation
  }

  /**
   * Sign receipt data with private key
   * @param {Object} data - Data to sign
   * @param {Buffer|string} privateKey - Private key (RSA-4096)
   * @returns {string} Base64-encoded signature
   */
  sign(data, privateKey) {
    // Implementation
  }

  /**
   * Generate proof-of-work hash puzzle
   * @private
   * @param {Object} data - Data to hash
   * @param {number} difficulty - Difficulty level (leading zeros)
   * @returns {string} PoW hash
   */
  _generateProofOfWork(data, difficulty = 4) {
    // Implementation
  }

  /**
   * Extend receipt with probe-specific fields
   * @private
   * @param {BaseReceipt} receipt - Base receipt
   * @param {AggregatedSummary} aggregated - Aggregated data
   * @returns {Object} Extended receipt
   */
  _extendWithProbeFields(receipt, aggregated) {
    // Implementation
  }
}

/**
 * Create receipt builder
 * @param {Object} [options] - Configuration
 * @returns {Promise<ReceiptBuilder>} Receipt builder
 *
 * @example
 * const builder = await createReceiptBuilder();
 */
export async function createReceiptBuilder(options = {}) {
  // Implementation
}
```

### merkle-integrator.mjs

```javascript
import { ReceiptChain } from '@unrdf/kgc-substrate';

/**
 * @typedef {Object} ChainedReceipt
 * @property {*} receipt - Receipt data
 * @property {string} merkle_root - Merkle root hash
 * @property {string[]} merkle_path - Path to root
 * @property {string} prev_hash - Hash of previous receipt
 * @property {string} chain_depth - Position in chain
 */

/**
 * Merkle Integrator - Chains receipts via merkle trees
 * Provides tamper-evident receipt linkage
 */
export class MerkleIntegrator {
  /**
   * Create merkle integrator
   * @param {Object} options - Configuration (required)
   * @param {ReceiptChain} options.receiptChain - ReceiptChain instance
   * @param {string} [options.hashAlgorithm='blake3'] - Hash algorithm
   * @param {Logger} [options.logger] - Logger instance
   */
  constructor(options) {
    // Implementation
  }

  /**
   * Chain receipt with merkle tree linking
   * @param {Object} receipt - Receipt to chain
   * @returns {Promise<ChainedReceipt>} Receipt with merkle data
   *
   * @example
   * const chained = await integrator.chain(receipt);
   * console.log(`Merkle root: ${chained.merkle_root}`);
   */
  async chain(receipt) {
    // Implementation
  }

  /**
   * Get merkle path to root
   * @private
   * @param {string} receiptHash - Receipt hash
   * @returns {Promise<string[]>} Path components
   */
  async _getMerklePath(receiptHash) {
    // Implementation
  }

  /**
   * Compute merkle root
   * @private
   * @param {string[]} hashes - Leaf hashes
   * @returns {string} Merkle root
   */
  _computeMerkleRoot(hashes) {
    // Implementation
  }

  /**
   * Append receipt to chain
   * @private
   * @param {Object} receipt - Receipt to append
   * @returns {Promise<string>} Receipt hash
   */
  async _appendToChain(receipt) {
    // Implementation
  }
}
```

### verification.mjs

```javascript
/**
 * @typedef {Object} VerificationResult
 * @property {boolean} valid - Is receipt valid
 * @property {string[]} errors - Error messages
 * @property {string[]} warnings - Warning messages
 * @property {Object} details - Detailed verification results
 */

/**
 * Receipt Verifier - Verifies receipt signatures and merkle chains
 */
export class ReceiptVerifier {
  /**
   * Create receipt verifier
   * @param {Object} [options] - Configuration
   * @param {MerkleIntegrator} [options.merkleIntegrator] - Merkle integrator
   * @param {string} [options.publicKeyPath] - Path to public key (RSA-4096)
   * @param {Logger} [options.logger] - Logger instance
   */
  constructor(options = {}) {
    // Implementation
  }

  /**
   * Verify receipt signature and integrity
   * @param {Object} receipt - Receipt to verify
   * @returns {Promise<VerificationResult>} Verification result
   *
   * @example
   * const result = await verifier.verify(receipt);
   * if (!result.valid) {
   *   console.error('Receipt invalid:', result.errors);
   * }
   */
  async verify(receipt) {
    // Implementation
  }

  /**
   * Verify receipt schema
   * @private
   * @param {Object} receipt - Receipt to validate
   * @throws {ValidationError} If schema invalid
   */
  _verifySchema(receipt) {
    // Implementation
  }

  /**
   * Verify RSA signature
   * @private
   * @param {Object} receipt - Receipt with signature
   * @param {Buffer|string} publicKey - Public key
   * @returns {boolean} Is signature valid
   */
  _verifySignature(receipt, publicKey) {
    // Implementation
  }

  /**
   * Verify merkle root and path
   * @private
   * @param {ChainedReceipt} receipt - Receipt with merkle data
   * @returns {Promise<boolean>} Is merkle valid
   */
  async _verifyMerkleChain(receipt) {
    // Implementation
  }

  /**
   * Verify timestamp reasonableness
   * @private
   * @param {bigint} timestamp_ns - Timestamp to check
   * @throws {ValidationError} If timestamp invalid
   */
  _verifyTimestamp(timestamp_ns) {
    // Implementation
  }
}
```

---

## 8. Utils Directory

### logger.mjs

```javascript
import pino from 'pino';

/**
 * @typedef {Object} Logger
 * @property {(msg: string, context?: Object) => void} debug
 * @property {(msg: string, context?: Object) => void} info
 * @property {(msg: string, context?: Object) => void} warn
 * @property {(msg: string, context?: Object) => void} error
 */

/**
 * Create structured logger with OTEL integration
 * @param {Object} [options] - Configuration
 * @param {'debug'|'info'|'warn'|'error'} [options.level='info'] - Log level
 * @param {string} [options.name='kgc-probe'] - Logger name
 * @param {boolean} [options.prettyPrint=false] - Pretty print output
 * @param {boolean} [options.otel=true] - Enable OTEL instrumentation
 * @returns {Logger} Logger instance
 *
 * @example
 * const logger = createLogger({ level: 'debug' });
 * logger.info('Probe started', { domain: 'security' });
 */
export function createLogger(options = {}) {
  // Implementation
}
```

### error-handler.mjs

```javascript
/**
 * KGCProbe Error - Base error class
 */
export class KGCProbeError extends Error {
  /**
   * Create KGC probe error
   * @param {string} message - Error message
   * @param {string} [code] - Error code
   * @param {Object} [context] - Context data
   */
  constructor(message, code = 'UNKNOWN', context = {}) {
    super(message);
    this.code = code;
    this.context = context;
  }
}

/**
 * Validation Error - Schema validation failure
 */
export class ValidationError extends KGCProbeError {
  /**
   * Create validation error
   * @param {string} message - Error message
   * @param {Object[]} errors - Validation errors
   * @param {Object} [context] - Context data
   */
  constructor(message, errors = [], context = {}) {
    super(message, 'VALIDATION_ERROR', context);
    this.validationErrors = errors;
  }
}

/**
 * Format error for user display
 * @param {Error|KGCProbeError} error - Error to format
 * @returns {Object} Formatted error object
 *
 * @example
 * const formatted = formatError(error);
 * console.error(`[${formatted.code}] ${formatted.message}`);
 */
export function formatError(error) {
  // Implementation
}

/**
 * Check if error is recoverable
 * @param {Error} error - Error to check
 * @returns {boolean} Can retry operation
 */
export function isRecoverable(error) {
  // Implementation
}

/**
 * Retry function with exponential backoff
 * @param {() => Promise<any>} fn - Function to retry
 * @param {Object} [options] - Configuration
 * @param {number} [options.maxAttempts=3] - Max retry attempts
 * @param {number} [options.initialDelayMs=100] - Initial delay
 * @param {number} [options.backoffMultiplier=2] - Backoff multiplier
 * @returns {Promise<any>} Function result
 * @throws {KGCProbeError} If all retries fail
 *
 * @example
 * const result = await retry(() => probe.execute(obs), { maxAttempts: 3 });
 */
export async function retry(fn, options = {}) {
  // Implementation
}
```

### types.mjs

```javascript
/**
 * @typedef {Object} Observation
 * @property {Object} subject - RDF subject
 * @property {Object} predicate - RDF predicate
 * @property {Object} object - RDF object
 * @property {Object} [graph] - RDF graph
 * @property {Object} metadata - Metadata
 */

/**
 * @typedef {Object} ProbeResult
 * @property {string} probe_id - UUID
 * @property {string} domain - Probe domain
 * @property {'pass'|'fail'|'warning'|'skip'} status
 * @property {number} score - [0.0, 1.0]
 * @property {Assertion[]} assertions
 * @property {number} duration_ms
 * @property {bigint} timestamp_ns
 */

/**
 * @typedef {Object} Assertion
 * @property {string} id
 * @property {'pass'|'fail'|'warning'|'skip'} status
 * @property {string} evidence
 * @property {number} weight
 */

/**
 * @typedef {Object} MergedResult
 * @property {number} overall_score
 * @property {Object<string, number>} by_domain
 * @property {Assertion[]} assertions
 * @property {bigint} timestamp_ns
 */

/**
 * @typedef {Object} Receipt
 * @property {string} receipt_id
 * @property {string} type
 * @property {number} score
 * @property {string} domain
 * @property {string} merkle_root
 * @property {string} signature
 * @property {bigint} timestamp_ns
 */

/**
 * @typedef {Object} GuardPolicy
 * @property {'strict'|'warn'|'skip'} enforce_mode
 * @property {Object[]} rejection_criteria
 * @property {number} allowed_failures
 * @property {number} timeout_ms
 */

/**
 * @typedef {Object} MergeConfig
 * @property {'consensus'|'max'|'min'|'weighted_sum'} strategy
 * @property {'highest_confidence'|'newest_first'|'manual'} conflict_resolution
 * @property {Object<string, number>} [weights]
 */

// Export types only (no implementation)
export {};
```

---

## Summary: Module Count & LOC Estimates

```
Directory          Modules    Est. LoC    Notes
────────────────────────────────────────────────────────────────
schemas/           5          245        Zod validators only
agents/            11         1,250      10 probes + registry
guards/            6          430        Guard implementations
storage/           5          530        RDF backend
orchestrator/      4          490        Merge + coordination
cli/               5          340        Command handlers
receipts/          4          330        Receipt generation
utils/             4          220        Support utilities
────────────────────────────────────────────────────────────────
Total              44         3,835      Implementation modules

test/              ~44        5,670      Jest tests (1.5:1 ratio)
────────────────────────────────────────────────────────────────
Grand Total        44         9,505      Code + tests
```

---

**END OF MODULE SIGNATURES DOCUMENT**
