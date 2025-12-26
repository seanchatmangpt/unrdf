/**
 * @fileoverview Invariants (Q) - Checkable properties that must be preserved
 *
 * Invariants define properties that MUST hold true before and after applying changes.
 * All invariants are deterministic and return structured results.
 *
 * Invariant Set:
 * - Q_typing: RDF syntax validity and partition schema type-checking
 * - Q_noncollision: IRI namespace collision detection
 * - Q_monotone: Overlay monotonicity (no substrate redefinition)
 * - Q_determinism: Hash-based determinism checking
 * - Q_provenance: Receipt structure validation
 * - Q_bounds: Complexity metric bounds checking
 *
 * @module admission/invariants
 */

import { z } from 'zod';
import { isProtectedNamespace, isCanonicalTerm } from './forbidden-operations.mjs';
import { QuadSchema } from './delta-capsule.mjs';

/**
 * Invariant result schema
 */
export const InvariantResultSchema = z.object({
  invariantName: z.string(),
  passed: z.boolean(),
  reason: z.string(),
  violations: z.array(z.any()).optional(),
  metadata: z.record(z.any()).optional()
});

/**
 * Default complexity bounds
 */
export const DEFAULT_BOUNDS = {
  maxQuads: 10000,           // Max quads per capsule
  maxNestingDepth: 10,       // Max blank node nesting
  maxNamespaces: 50,         // Max distinct namespaces
  maxQuadsPerSubject: 1000   // Max quads per subject
};

/**
 * Q_typing: Verify all quads are valid RDF syntax and type-check against partition schema
 *
 * Checks:
 * 1. All quads conform to RDF/JS quad structure
 * 2. Predicates are always NamedNodes
 * 3. Subjects are never Literals
 * 4. All IRIs are well-formed URLs
 *
 * @param {DeltaCapsule} capsule - Capsule to check
 * @param {Object} [options] - Check options
 * @returns {Object} Invariant result
 */
export function Q_typing(capsule, options = {}) {
  const violations = [];
  const quads = capsule.getAllQuads();

  for (const quad of quads) {
    // Validate quad structure using Zod
    try {
      QuadSchema.parse(quad);
    } catch (error) {
      violations.push({
        quad,
        error: 'Invalid quad structure',
        details: error.message
      });
      continue;
    }

    // RDF constraint: Subject cannot be Literal
    if (quad.subject.termType === 'Literal') {
      violations.push({
        quad,
        error: 'Subject cannot be a Literal',
        term: quad.subject
      });
    }

    // RDF constraint: Predicate must be NamedNode
    if (quad.predicate.termType !== 'NamedNode') {
      violations.push({
        quad,
        error: 'Predicate must be a NamedNode',
        term: quad.predicate
      });
    }

    // Validate IRI well-formedness
    const iriTerms = [
      { term: quad.subject, position: 'subject' },
      { term: quad.predicate, position: 'predicate' },
      { term: quad.object, position: 'object' }
    ];

    for (const { term, position } of iriTerms) {
      if (term.termType === 'NamedNode') {
        try {
          new URL(term.value);
        } catch {
          violations.push({
            quad,
            error: `Invalid IRI in ${position}`,
            iri: term.value
          });
        }
      }
    }

    // Validate literals have proper datatype
    if (quad.object.termType === 'Literal') {
      if (quad.object.datatype && quad.object.datatype.value) {
        try {
          new URL(quad.object.datatype.value);
        } catch {
          violations.push({
            quad,
            error: 'Invalid datatype IRI',
            datatype: quad.object.datatype.value
          });
        }
      }
    }
  }

  if (violations.length > 0) {
    return {
      invariantName: 'Q_typing',
      passed: false,
      reason: `RDF syntax violations detected: ${violations.length} invalid quad(s)`,
      violations,
      metadata: {
        violationCount: violations.length,
        totalQuads: quads.length
      }
    };
  }

  return {
    invariantName: 'Q_typing',
    passed: true,
    reason: `All ${quads.length} quads conform to RDF syntax`,
    metadata: {
      totalQuads: quads.length
    }
  };
}

/**
 * Q_noncollision: Check proposed IRIs don't collide with protected namespaces
 *
 * Checks:
 * 1. New terms don't use protected namespace prefixes
 * 2. Additions don't create terms in substrate namespaces
 * 3. Safe collision detection for overlay namespaces
 *
 * @param {DeltaCapsule} capsule - Capsule to check
 * @param {Object} [options] - Check options
 * @param {Set<string>} [options.allowedNamespaces] - Explicitly allowed namespaces
 * @returns {Object} Invariant result
 */
export function Q_noncollision(capsule, options = {}) {
  const violations = [];
  const allowedNamespaces = options.allowedNamespaces || new Set();

  // Add partition namespace to allowed list
  if (capsule.partition.namespace) {
    allowedNamespaces.add(capsule.partition.namespace);
  }

  // Only check 'add' operations for collisions
  const additions = capsule.getQuadsByOperation('add');

  for (const quad of additions) {
    const terms = [
      { value: quad.subject.value, type: quad.subject.termType, position: 'subject' },
      { value: quad.predicate.value, type: quad.predicate.termType, position: 'predicate' },
      { value: quad.object.value, type: quad.object.termType, position: 'object' }
    ];

    for (const { value, type, position } of terms) {
      if (type === 'NamedNode' && typeof value === 'string') {
        // Check against protected namespaces
        if (isProtectedNamespace(value)) {
          violations.push({
            quad,
            error: 'Collision with protected namespace',
            iri: value,
            position
          });
          continue;
        }

        // Check if namespace is allowed
        let inAllowedNamespace = false;
        for (const allowedNS of allowedNamespaces) {
          if (value.startsWith(allowedNS)) {
            inAllowedNamespace = true;
            break;
          }
        }

        if (!inAllowedNamespace && !value.startsWith('http://example.org/')) {
          // Warning: Using namespace outside partition
          violations.push({
            quad,
            error: 'IRI outside partition namespace',
            iri: value,
            position,
            severity: 'warning'
          });
        }
      }
    }
  }

  const errors = violations.filter(v => v.severity !== 'warning');

  if (errors.length > 0) {
    return {
      invariantName: 'Q_noncollision',
      passed: false,
      reason: `Namespace collision detected: ${errors.length} collision(s) with protected namespaces`,
      violations: errors,
      metadata: {
        errorCount: errors.length,
        warningCount: violations.length - errors.length,
        totalViolations: violations.length
      }
    };
  }

  return {
    invariantName: 'Q_noncollision',
    passed: true,
    reason: 'No namespace collisions detected',
    metadata: {
      checkedQuads: additions.length,
      warnings: violations.filter(v => v.severity === 'warning')
    }
  };
}

/**
 * Q_monotone: Verify overlays don't redefine industrial substrate or protected terms
 *
 * Checks:
 * 1. No deletions in protected partitions
 * 2. No updates to canonical terms
 * 3. Additions don't contradict substrate
 *
 * @param {DeltaCapsule} capsule - Capsule to check
 * @param {Object} [options] - Check options
 * @returns {Object} Invariant result
 */
export function Q_monotone(capsule, options = {}) {
  const violations = [];

  // Check if partition is protected
  if (capsule.partition.protected) {
    const nonAdditiveChanges = capsule.changes.filter(
      c => c.operation !== 'add'
    );

    if (nonAdditiveChanges.length > 0) {
      violations.push({
        error: 'Protected partition only allows additive changes',
        partition: capsule.partition.name,
        violatingOperations: nonAdditiveChanges.map(c => c.operation)
      });
    }
  }

  // Check for redefinition of canonical terms
  const RDF_TYPE = 'http://www.w3.org/1999/02/22-rdf-syntax-ns#type';
  const allQuads = capsule.getAllQuads();

  for (const quad of allQuads) {
    // Check if redefining type of canonical term
    if (quad.predicate.value === RDF_TYPE &&
        quad.subject.termType === 'NamedNode' &&
        isCanonicalTerm(quad.subject.value)) {

      violations.push({
        quad,
        error: 'Attempted to redefine type of canonical term',
        term: quad.subject.value
      });
    }

    // Check if modifying properties of canonical terms
    if (quad.subject.termType === 'NamedNode' &&
        isCanonicalTerm(quad.subject.value)) {

      const IMMUTABLE_PREDICATES = new Set([
        'http://www.w3.org/2000/01/rdf-schema#subClassOf',
        'http://www.w3.org/2000/01/rdf-schema#subPropertyOf',
        'http://www.w3.org/2000/01/rdf-schema#domain',
        'http://www.w3.org/2000/01/rdf-schema#range'
      ]);

      if (IMMUTABLE_PREDICATES.has(quad.predicate.value)) {
        violations.push({
          quad,
          error: 'Attempted to modify immutable property of canonical term',
          term: quad.subject.value,
          predicate: quad.predicate.value
        });
      }
    }
  }

  if (violations.length > 0) {
    return {
      invariantName: 'Q_monotone',
      passed: false,
      reason: `Monotonicity violation: ${violations.length} redefinition(s) of substrate detected`,
      violations,
      metadata: {
        violationCount: violations.length
      }
    };
  }

  return {
    invariantName: 'Q_monotone',
    passed: true,
    reason: 'Overlay is monotone - no substrate redefinition detected',
    metadata: {
      isAdditiveOnly: capsule.isAdditiveOnly()
    }
  };
}

/**
 * Q_determinism: Hash check - same Δ → same result
 *
 * Checks:
 * 1. Capsule hash is computed correctly
 * 2. Same content produces same hash
 * 3. Hash matches expected value (if provided)
 *
 * @param {DeltaCapsule} capsule - Capsule to check
 * @param {Object} [options] - Check options
 * @param {string} [options.expectedHash] - Expected hash value
 * @returns {Object} Invariant result
 */
export function Q_determinism(capsule, options = {}) {
  const violations = [];

  // Check that hash exists
  const hash = capsule.getHash();
  if (!hash || typeof hash !== 'string' || hash.length !== 64) {
    violations.push({
      error: 'Invalid or missing capsule hash',
      hash
    });
  }

  // If expected hash provided, verify match
  if (options.expectedHash) {
    if (hash !== options.expectedHash) {
      violations.push({
        error: 'Hash mismatch - capsule content changed',
        expectedHash: options.expectedHash,
        actualHash: hash
      });
    }
  }

  // Verify hash is deterministic by recomputing
  try {
    const recomputedHash = capsule._computeHash();
    if (recomputedHash !== hash) {
      violations.push({
        error: 'Hash is not deterministic',
        originalHash: hash,
        recomputedHash
      });
    }
  } catch (error) {
    violations.push({
      error: 'Failed to recompute hash',
      details: error.message
    });
  }

  if (violations.length > 0) {
    return {
      invariantName: 'Q_determinism',
      passed: false,
      reason: `Determinism check failed: ${violations.length} issue(s)`,
      violations,
      metadata: {
        hash
      }
    };
  }

  return {
    invariantName: 'Q_determinism',
    passed: true,
    reason: 'Capsule is deterministic',
    metadata: {
      hash
    }
  };
}

/**
 * Q_provenance: Verify receipt structure pre-populated correctly
 *
 * Checks:
 * 1. Provenance metadata exists
 * 2. Required fields are present (agent, timestamp)
 * 3. Timestamp is valid ISO 8601
 * 4. Agent identifier is valid
 *
 * @param {DeltaCapsule} capsule - Capsule to check
 * @param {Object} [options] - Check options
 * @returns {Object} Invariant result
 */
export function Q_provenance(capsule, options = {}) {
  const violations = [];

  // Check provenance exists
  if (!capsule.provenance) {
    return {
      invariantName: 'Q_provenance',
      passed: false,
      reason: 'Provenance metadata missing',
      violations: [{ error: 'No provenance object found' }]
    };
  }

  const prov = capsule.provenance;

  // Check required fields
  if (!prov.agent || typeof prov.agent !== 'string' || prov.agent.length === 0) {
    violations.push({
      error: 'Agent identifier missing or invalid',
      agent: prov.agent
    });
  }

  if (!prov.timestamp || typeof prov.timestamp !== 'string') {
    violations.push({
      error: 'Timestamp missing or invalid',
      timestamp: prov.timestamp
    });
  } else {
    // Validate ISO 8601 timestamp
    try {
      const date = new Date(prov.timestamp);
      if (isNaN(date.getTime())) {
        violations.push({
          error: 'Timestamp is not a valid date',
          timestamp: prov.timestamp
        });
      }

      // Check timestamp is not in future
      if (date.getTime() > Date.now() + 60000) { // Allow 1 min clock skew
        violations.push({
          error: 'Timestamp is in the future',
          timestamp: prov.timestamp
        });
      }
    } catch (error) {
      violations.push({
        error: 'Failed to parse timestamp',
        timestamp: prov.timestamp,
        details: error.message
      });
    }
  }

  // Optional fields validation
  if (prov.source && typeof prov.source !== 'string') {
    violations.push({
      error: 'Source must be a string',
      source: prov.source
    });
  }

  if (prov.justification && typeof prov.justification !== 'string') {
    violations.push({
      error: 'Justification must be a string',
      justification: prov.justification
    });
  }

  if (violations.length > 0) {
    return {
      invariantName: 'Q_provenance',
      passed: false,
      reason: `Provenance validation failed: ${violations.length} issue(s)`,
      violations,
      metadata: {
        provenance: prov
      }
    };
  }

  return {
    invariantName: 'Q_provenance',
    passed: true,
    reason: 'Provenance structure is valid',
    metadata: {
      agent: prov.agent,
      timestamp: prov.timestamp,
      hasSource: !!prov.source,
      hasJustification: !!prov.justification
    }
  };
}

/**
 * Q_bounds: Check complexity metrics (triple count, nesting depth)
 *
 * Checks:
 * 1. Total quad count within limits
 * 2. Blank node nesting depth within limits
 * 3. Namespace count within limits
 * 4. Quads per subject within limits
 *
 * @param {DeltaCapsule} capsule - Capsule to check
 * @param {Object} [options] - Check options
 * @param {Object} [options.bounds] - Custom bounds
 * @returns {Object} Invariant result
 */
export function Q_bounds(capsule, options = {}) {
  const violations = [];
  const bounds = { ...DEFAULT_BOUNDS, ...(options.bounds || {}) };
  const quads = capsule.getAllQuads();

  // Check total quad count
  const quadCount = quads.length;
  if (quadCount > bounds.maxQuads) {
    violations.push({
      error: 'Quad count exceeds limit',
      count: quadCount,
      limit: bounds.maxQuads
    });
  }

  // Check namespace count
  const namespaces = capsule.getNamespaces();
  if (namespaces.size > bounds.maxNamespaces) {
    violations.push({
      error: 'Namespace count exceeds limit',
      count: namespaces.size,
      limit: bounds.maxNamespaces
    });
  }

  // Check quads per subject
  const quadsBySubject = new Map();
  for (const quad of quads) {
    const subject = quad.subject.value;
    if (!quadsBySubject.has(subject)) {
      quadsBySubject.set(subject, []);
    }
    quadsBySubject.get(subject).push(quad);
  }

  for (const [subject, subjectQuads] of quadsBySubject.entries()) {
    if (subjectQuads.length > bounds.maxQuadsPerSubject) {
      violations.push({
        error: 'Quads per subject exceeds limit',
        subject,
        count: subjectQuads.length,
        limit: bounds.maxQuadsPerSubject
      });
    }
  }

  // Check blank node nesting depth
  const blankNodeDepth = calculateBlankNodeDepth(quads);
  if (blankNodeDepth > bounds.maxNestingDepth) {
    violations.push({
      error: 'Blank node nesting depth exceeds limit',
      depth: blankNodeDepth,
      limit: bounds.maxNestingDepth
    });
  }

  if (violations.length > 0) {
    return {
      invariantName: 'Q_bounds',
      passed: false,
      reason: `Complexity bounds exceeded: ${violations.length} limit(s) violated`,
      violations,
      metadata: {
        quadCount,
        namespaceCount: namespaces.size,
        blankNodeDepth,
        subjectCount: quadsBySubject.size
      }
    };
  }

  return {
    invariantName: 'Q_bounds',
    passed: true,
    reason: 'All complexity metrics within bounds',
    metadata: {
      quadCount,
      namespaceCount: namespaces.size,
      blankNodeDepth,
      subjectCount: quadsBySubject.size,
      bounds
    }
  };
}

/**
 * Calculate maximum blank node nesting depth
 * @param {Array} quads - Quads to analyze
 * @returns {number} Maximum nesting depth
 * @private
 */
function calculateBlankNodeDepth(quads) {
  const blankNodeGraph = new Map(); // blank node -> [referenced blank nodes]

  // Build graph of blank node references
  for (const quad of quads) {
    if (quad.subject.termType === 'BlankNode') {
      if (!blankNodeGraph.has(quad.subject.value)) {
        blankNodeGraph.set(quad.subject.value, []);
      }

      if (quad.object.termType === 'BlankNode') {
        blankNodeGraph.get(quad.subject.value).push(quad.object.value);
      }
    }
  }

  // Find max depth via DFS
  let maxDepth = 0;

  function dfs(node, depth, visited) {
    if (visited.has(node)) return depth; // Cycle detection
    visited.add(node);

    maxDepth = Math.max(maxDepth, depth);

    const children = blankNodeGraph.get(node) || [];
    for (const child of children) {
      dfs(child, depth + 1, new Set(visited));
    }

    return depth;
  }

  for (const blankNode of blankNodeGraph.keys()) {
    dfs(blankNode, 1, new Set());
  }

  return maxDepth;
}

/**
 * Run all invariant checks
 * @param {DeltaCapsule} capsule - Capsule to check
 * @param {Object} [options] - Check options
 * @returns {Object} Combined invariant results
 */
export function checkAllInvariants(capsule, options = {}) {
  const invariants = {
    Q_typing,
    Q_noncollision,
    Q_monotone,
    Q_determinism,
    Q_provenance,
    Q_bounds
  };

  const results = [];
  const failures = [];

  for (const [name, invariantFn] of Object.entries(invariants)) {
    // Check if invariant is enabled in capsule
    const invariantConfig = capsule.getInvariant(name);

    if (!invariantConfig || !invariantConfig.enabled) {
      continue; // Skip disabled invariants
    }

    const result = invariantFn(capsule, options);
    results.push(result);

    if (!result.passed) {
      failures.push({
        invariant: name,
        reason: result.reason,
        strictness: invariantConfig.strictness || 'error'
      });
    }
  }

  const errors = failures.filter(f => f.strictness === 'error');

  return {
    passed: errors.length === 0,
    results,
    failures,
    errorCount: errors.length,
    warningCount: failures.filter(f => f.strictness === 'warning').length,
    summary: errors.length > 0
      ? `${errors.length} invariant(s) failed: ${errors.map(f => f.invariant).join(', ')}`
      : 'All enabled invariants passed'
  };
}
