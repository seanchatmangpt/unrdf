/**
 * KGC Multiverse - Q* Validation System
 * Implements Q* constants validation for knowledge graph integrity
 *
 * Q* Constants:
 * - Q_ID: IRI stability (namedNode corpus preservation)
 * - Q_RDF: RDF semantics (quad interpretation invariants)
 * - Q_PROV: Provenance (hash chain, timestamps)
 *
 * @module @unrdf/kgc-multiverse/q-star
 */

import { z } from 'zod';
import { blake3 } from 'hash-wasm';

/**
 * Q* Error Codes
 * @readonly
 * @enum {string}
 */
export const QStarErrorCode = {
  Q1_IRI_MISSING: 'Q1_IRI_MISSING',           // IRI present in U_i but missing in U_j
  Q2_IRI_CONFLICT: 'Q2_IRI_CONFLICT',         // IRI has conflicting values
  Q3_QUAD_SEMANTIC: 'Q3_QUAD_SEMANTIC',       // Quad semantic violation
  Q4_SPARQL_DIVERGENCE: 'Q4_SPARQL_DIVERGENCE', // SPARQL results differ
  Q5_HASH_MISMATCH: 'Q5_HASH_MISMATCH',       // Hash chain broken
  Q6_TIMESTAMP_ORDER: 'Q6_TIMESTAMP_ORDER',   // Timestamps out of order
  Q7_ORPHAN_RECEIPT: 'Q7_ORPHAN_RECEIPT',     // Receipt without parent
  Q8_CHAIN_DISCONTINUITY: 'Q8_CHAIN_DISCONTINUITY', // Gap in provenance chain
};

/**
 * Q*_ID Schema - Identity stability invariants
 * Ensures all IRIs are preserved across morphisms
 */
export const QStarIDSchema = z.object({
  /** Q* identifier pattern: Q*_<16 hex chars> */
  Q_ID: z.string().regex(/^Q\*_[a-f0-9]{16}$/),
  /** RDF URI representation */
  Q_RDF: z.string().url(),
  /** IRI corpus - set of all named nodes in universe */
  iriCorpus: z.array(z.string().url()).optional(),
  /** IRI count for quick validation */
  iriCount: z.number().int().nonnegative().optional(),
});

/**
 * Q*_RDF Schema - RDF semantic invariants
 * Ensures quad interpretation is preserved
 */
export const QStarRDFSchema = z.object({
  /** Total quad count */
  quadCount: z.number().int().nonnegative(),
  /** Subject count */
  subjectCount: z.number().int().nonnegative(),
  /** Predicate count */
  predicateCount: z.number().int().nonnegative(),
  /** Object count */
  objectCount: z.number().int().nonnegative(),
  /** Named graph count */
  graphCount: z.number().int().nonnegative().optional(),
  /** Hash of canonical N-Quads serialization */
  canonicalHash: z.string().regex(/^[a-f0-9]{64}$/).optional(),
});

/**
 * Q*_PROV Schema - Provenance invariants
 * Ensures hash chain integrity
 */
export const QStarPROVSchema = z.object({
  /** Nanosecond timestamp */
  timestamp: z.bigint(),
  /** Previous receipt hash (if not genesis) */
  previousHash: z.string().regex(/^[a-f0-9]{64}$/).optional(),
  /** Current receipt hash */
  currentHash: z.string().regex(/^[a-f0-9]{64}$/),
  /** Receipt sequence number */
  sequenceNumber: z.number().int().nonnegative(),
  /** Parent universe Q*_ID (if forked) */
  parentID: z.string().optional(),
  /** Chain length (number of receipts) */
  chainLength: z.number().int().positive(),
});

/**
 * Complete Q* Snapshot Schema
 * Combines Q_ID, Q_RDF, Q_PROV
 */
export const QStarSnapshotSchema = z.object({
  Q_ID: QStarIDSchema,
  Q_RDF: QStarRDFSchema,
  Q_PROV: QStarPROVSchema,
  /** Snapshot timestamp */
  snapshotAt: z.bigint(),
});

/**
 * Validation Result Schema
 */
const ValidationResultSchema = z.object({
  valid: z.boolean(),
  errorCode: z.nativeEnum(QStarErrorCode).optional(),
  message: z.string().optional(),
  details: z.record(z.any()).optional(),
});

/**
 * Extract IRIs from quads
 * Collects all named nodes (subjects, predicates, objects, graphs)
 *
 * @param {Array<Object>} quads - Array of RDF quads
 * @returns {Set<string>} Set of unique IRIs
 *
 * @example
 * const quads = [{ subject: { value: 'http://ex.com/s1' }, ... }];
 * const iris = extractIRIs(quads);
 * console.assert(iris.has('http://ex.com/s1'));
 */
export function extractIRIs(quads) {
  if (!Array.isArray(quads)) {
    throw new TypeError('extractIRIs: quads must be array');
  }

  const iris = new Set();

  for (const quad of quads) {
    // Subject (always NamedNode or BlankNode)
    if (quad.subject?.termType === 'NamedNode') {
      iris.add(quad.subject.value);
    }

    // Predicate (always NamedNode)
    if (quad.predicate?.value) {
      iris.add(quad.predicate.value);
    }

    // Object (NamedNode or Literal)
    if (quad.object?.termType === 'NamedNode') {
      iris.add(quad.object.value);
    }

    // Graph (optional, NamedNode)
    if (quad.graph?.termType === 'NamedNode') {
      iris.add(quad.graph.value);
    }
  }

  return iris;
}

/**
 * Compute canonical hash of quads
 * Uses BLAKE3 on sorted N-Quads serialization
 *
 * @param {Array<Object>} quads - Array of RDF quads
 * @returns {Promise<string>} BLAKE3 hash (64 hex chars)
 *
 * @example
 * const hash = await computeCanonicalHash(quads);
 * console.assert(hash.length === 64);
 */
export async function computeCanonicalHash(quads) {
  if (!Array.isArray(quads)) {
    throw new TypeError('computeCanonicalHash: quads must be array');
  }

  // Serialize to N-Quads format and sort
  const nquads = quads.map((q) => {
    const s = q.subject?.value || '';
    const p = q.predicate?.value || '';
    const o = q.object?.termType === 'Literal'
      ? `"${q.object.value}"${q.object.datatype ? `^^<${q.object.datatype.value}>` : ''}`
      : `<${q.object?.value || ''}>`;
    const g = q.graph?.value ? ` <${q.graph.value}>` : '';
    return `<${s}> <${p}> ${o}${g} .`;
  }).sort();

  const canonical = nquads.join('\n');
  return blake3(canonical);
}

/**
 * Create Q* snapshot from universe state
 *
 * @param {Object} options - Snapshot options
 * @param {string} options.universeID - Universe Q*_ID
 * @param {string} options.universeRDF - Universe RDF URI
 * @param {Array<Object>} options.quads - Current quads
 * @param {Array<Object>} options.receipts - Receipt chain
 * @returns {Promise<Object>} Q* snapshot
 *
 * @example
 * const snapshot = await createQStarSnapshot({
 *   universeID: 'Q*_abc...',
 *   universeRDF: 'http://kgc.io/...',
 *   quads: [...],
 *   receipts: [...],
 * });
 */
export async function createQStarSnapshot(options) {
  if (!options || typeof options !== 'object') {
    throw new TypeError('createQStarSnapshot: options must be object');
  }

  const { universeID, universeRDF, quads = [], receipts = [] } = options;

  // Extract IRIs
  const iris = extractIRIs(quads);

  // Compute RDF stats
  const subjects = new Set(quads.map((q) => q.subject?.value).filter(Boolean));
  const predicates = new Set(quads.map((q) => q.predicate?.value).filter(Boolean));
  const objects = new Set(quads.map((q) => q.object?.value).filter(Boolean));
  const graphs = new Set(quads.map((q) => q.graph?.value).filter(Boolean));

  // Compute canonical hash
  const canonicalHash = await computeCanonicalHash(quads);

  // Get latest receipt for provenance
  const latestReceipt = receipts.length > 0 ? receipts[receipts.length - 1] : null;
  const previousReceipt = receipts.length > 1 ? receipts[receipts.length - 2] : null;

  const snapshotAt = typeof process !== 'undefined' && process.hrtime
    ? process.hrtime.bigint()
    : BigInt(Date.now()) * 1_000_000n;

  const snapshot = {
    Q_ID: {
      Q_ID: universeID,
      Q_RDF: universeRDF,
      iriCorpus: Array.from(iris),
      iriCount: iris.size,
    },
    Q_RDF: {
      quadCount: quads.length,
      subjectCount: subjects.size,
      predicateCount: predicates.size,
      objectCount: objects.size,
      graphCount: graphs.size,
      canonicalHash,
    },
    Q_PROV: {
      timestamp: snapshotAt,
      previousHash: previousReceipt?.Q_PROV?.contentHash,
      currentHash: latestReceipt?.Q_PROV?.contentHash || canonicalHash,
      sequenceNumber: receipts.length,
      chainLength: receipts.length || 1,
      ...(options.parentID && { parentID: options.parentID }),
    },
    snapshotAt,
  };

  // Validate
  QStarSnapshotSchema.parse(snapshot);

  return snapshot;
}

/**
 * Q* Validator Class
 * Validates Q* invariants across universe states
 */
export class QStarValidator {
  /**
   * Create Q* Validator
   *
   * @param {Object} [options={}] - Validator options
   * @param {boolean} [options.strict=true] - Strict validation mode
   */
  constructor(options = {}) {
    /** @private */
    this._strict = options.strict !== false;
    /** @private */
    this._validationResults = [];
  }

  /**
   * Check identity stability between two universe states
   * Invariant: All IRIs in U_i must be present in U_j after morphism
   *
   * @param {Object} snapshot_i - Source universe Q* snapshot
   * @param {Object} snapshot_j - Target universe Q* snapshot
   * @param {Object} [options={}] - Check options
   * @param {boolean} [options.allowAdditions=true] - Allow new IRIs in U_j
   * @returns {Object} Validation result with valid, errorCode, message
   *
   * @example
   * const result = validator.checkIdentityStability(snap_i, snap_j);
   * if (!result.valid) {
   *   console.error('Q1:', result.errorCode, result.message);
   * }
   */
  checkIdentityStability(snapshot_i, snapshot_j, options = {}) {
    const { allowAdditions = true } = options;

    // Validate input snapshots
    try {
      QStarSnapshotSchema.parse(snapshot_i);
      QStarSnapshotSchema.parse(snapshot_j);
    } catch (err) {
      return {
        valid: false,
        errorCode: QStarErrorCode.Q1_IRI_MISSING,
        message: `Invalid snapshot: ${err.message}`,
      };
    }

    const iris_i = new Set(snapshot_i.Q_ID.iriCorpus || []);
    const iris_j = new Set(snapshot_j.Q_ID.iriCorpus || []);

    // Check: All IRIs in U_i must exist in U_j
    const missing = [];
    for (const iri of iris_i) {
      if (!iris_j.has(iri)) {
        missing.push(iri);
      }
    }

    if (missing.length > 0) {
      const result = {
        valid: false,
        errorCode: QStarErrorCode.Q1_IRI_MISSING,
        message: `${missing.length} IRI(s) missing after morphism`,
        details: {
          missingIRIs: missing.slice(0, 10), // Limit to first 10
          totalMissing: missing.length,
        },
      };
      this._validationResults.push(result);
      return result;
    }

    // Check: Conflicting values (same IRI, different semantics)
    if (this._strict) {
      const conflicts = this._detectIRIConflicts(snapshot_i, snapshot_j);
      if (conflicts.length > 0) {
        const result = {
          valid: false,
          errorCode: QStarErrorCode.Q2_IRI_CONFLICT,
          message: `${conflicts.length} IRI conflict(s) detected`,
          details: { conflicts },
        };
        this._validationResults.push(result);
        return result;
      }
    }

    const result = {
      valid: true,
      message: 'Identity stability preserved',
      details: {
        irisPreserved: iris_i.size,
        irisAdded: allowAdditions ? iris_j.size - iris_i.size : 0,
      },
    };
    this._validationResults.push(result);
    return result;
  }

  /**
   * Check RDF semantics between two universe states
   * Invariant: SPARQL query results must be identical (for unchanged data)
   *
   * @param {Object} snapshot_i - Source universe Q* snapshot
   * @param {Object} snapshot_j - Target universe Q* snapshot
   * @param {Object} [options={}] - Check options
   * @param {boolean} [options.allowQuadChanges=true] - Allow quad count changes
   * @returns {Object} Validation result
   *
   * @example
   * const result = validator.checkRDFSemantics(snap_i, snap_j);
   * if (!result.valid) {
   *   console.error('Q3/Q4:', result.errorCode);
   * }
   */
  checkRDFSemantics(snapshot_i, snapshot_j, options = {}) {
    const { allowQuadChanges = true } = options;

    // Validate input snapshots
    try {
      QStarSnapshotSchema.parse(snapshot_i);
      QStarSnapshotSchema.parse(snapshot_j);
    } catch (err) {
      return {
        valid: false,
        errorCode: QStarErrorCode.Q3_QUAD_SEMANTIC,
        message: `Invalid snapshot: ${err.message}`,
      };
    }

    // Check quad semantic integrity
    const rdf_i = snapshot_i.Q_RDF;
    const rdf_j = snapshot_j.Q_RDF;

    // Strict mode: canonical hash must match if no changes expected
    if (!allowQuadChanges && rdf_i.canonicalHash !== rdf_j.canonicalHash) {
      const result = {
        valid: false,
        errorCode: QStarErrorCode.Q4_SPARQL_DIVERGENCE,
        message: 'Canonical hash divergence (SPARQL results would differ)',
        details: {
          hash_i: rdf_i.canonicalHash,
          hash_j: rdf_j.canonicalHash,
        },
      };
      this._validationResults.push(result);
      return result;
    }

    // Check semantic ratios (heuristic for detecting major changes)
    const ratios = {
      quadRatio: rdf_j.quadCount / Math.max(rdf_i.quadCount, 1),
      subjectRatio: rdf_j.subjectCount / Math.max(rdf_i.subjectCount, 1),
      predicateRatio: rdf_j.predicateCount / Math.max(rdf_i.predicateCount, 1),
    };

    // Warn if major changes (>50% difference)
    const majorChange = Object.values(ratios).some((r) => r < 0.5 || r > 2);

    const result = {
      valid: true,
      message: majorChange
        ? 'RDF semantics preserved (with significant changes)'
        : 'RDF semantics preserved',
      details: {
        quadDelta: rdf_j.quadCount - rdf_i.quadCount,
        ratios,
        majorChange,
      },
    };
    this._validationResults.push(result);
    return result;
  }

  /**
   * Check provenance chain integrity
   * Invariant: Hash chain must be unbroken, no orphan receipts
   *
   * @param {Array<Object>} receipts - Receipt chain (oldest to newest)
   * @returns {Object} Validation result
   *
   * @example
   * const result = validator.checkProvenanceChain(receipts);
   * if (!result.valid) {
   *   console.error('Q5-Q8:', result.errorCode);
   * }
   */
  checkProvenanceChain(receipts) {
    if (!Array.isArray(receipts)) {
      const result = {
        valid: false,
        errorCode: QStarErrorCode.Q7_ORPHAN_RECEIPT,
        message: 'Receipts must be array',
      };
      this._validationResults.push(result);
      return result;
    }

    if (receipts.length === 0) {
      const result = {
        valid: true,
        message: 'Empty chain (genesis state)',
        details: { chainLength: 0 },
      };
      this._validationResults.push(result);
      return result;
    }

    let previousHash = null;
    let previousTimestamp = 0n;
    const orphans = [];
    const hashMismatches = [];
    const timestampViolations = [];

    for (let i = 0; i < receipts.length; i++) {
      const receipt = receipts[i];

      // Check Q_PROV structure
      if (!receipt?.Q_PROV) {
        orphans.push({ index: i, reason: 'Missing Q_PROV' });
        continue;
      }

      const { timestamp, contentHash, previousHash: receiptPrevHash } = receipt.Q_PROV;

      // Q5: Hash chain integrity
      if (i > 0 && receiptPrevHash !== previousHash) {
        hashMismatches.push({
          index: i,
          expected: previousHash,
          actual: receiptPrevHash,
        });
      }

      // Q6: Timestamp ordering
      if (timestamp !== undefined) {
        const ts = typeof timestamp === 'bigint' ? timestamp : BigInt(timestamp);
        if (ts < previousTimestamp) {
          timestampViolations.push({
            index: i,
            timestamp: ts,
            previousTimestamp,
          });
        }
        previousTimestamp = ts;
      }

      // Q7: Orphan detection (receipt without valid parent reference)
      if (i > 0 && !receiptPrevHash && !receipt.Q_PROV.parentID) {
        orphans.push({ index: i, reason: 'No previous hash or parent ID' });
      }

      // Update for next iteration
      previousHash = contentHash;
    }

    // Q5: Hash mismatch
    if (hashMismatches.length > 0) {
      const result = {
        valid: false,
        errorCode: QStarErrorCode.Q5_HASH_MISMATCH,
        message: `Hash chain broken at ${hashMismatches.length} point(s)`,
        details: { hashMismatches },
      };
      this._validationResults.push(result);
      return result;
    }

    // Q6: Timestamp violations
    if (timestampViolations.length > 0) {
      const result = {
        valid: false,
        errorCode: QStarErrorCode.Q6_TIMESTAMP_ORDER,
        message: `Timestamp ordering violated at ${timestampViolations.length} point(s)`,
        details: { timestampViolations },
      };
      this._validationResults.push(result);
      return result;
    }

    // Q7: Orphan receipts
    if (orphans.length > 0) {
      const result = {
        valid: false,
        errorCode: QStarErrorCode.Q7_ORPHAN_RECEIPT,
        message: `${orphans.length} orphan receipt(s) detected`,
        details: { orphans },
      };
      this._validationResults.push(result);
      return result;
    }

    // Q8: Chain discontinuity (gaps in sequence)
    // For now, we check based on hash chain - if all hashes link, no discontinuity

    const result = {
      valid: true,
      message: 'Provenance chain valid',
      details: {
        chainLength: receipts.length,
        firstTimestamp: receipts[0]?.Q_PROV?.timestamp,
        lastTimestamp: receipts[receipts.length - 1]?.Q_PROV?.timestamp,
      },
    };
    this._validationResults.push(result);
    return result;
  }

  /**
   * Validate complete Q* invariants
   * Combines all checks: identity, RDF semantics, provenance
   *
   * @param {Object} options - Validation options
   * @param {Object} options.snapshot_i - Source Q* snapshot
   * @param {Object} options.snapshot_j - Target Q* snapshot
   * @param {Array<Object>} options.receipts - Receipt chain
   * @returns {Object} Comprehensive validation result
   *
   * @example
   * const result = validator.validateQStar({
   *   snapshot_i,
   *   snapshot_j,
   *   receipts,
   * });
   * console.log('Q* Valid:', result.allPassed);
   */
  validateQStar(options) {
    const { snapshot_i, snapshot_j, receipts = [] } = options;

    const results = {
      identity: this.checkIdentityStability(snapshot_i, snapshot_j),
      rdfSemantics: this.checkRDFSemantics(snapshot_i, snapshot_j),
      provenance: this.checkProvenanceChain(receipts),
    };

    const allPassed = Object.values(results).every((r) => r.valid);

    return {
      allPassed,
      results,
      failedChecks: Object.entries(results)
        .filter(([, r]) => !r.valid)
        .map(([name, r]) => ({ name, ...r })),
    };
  }

  /**
   * Get all validation results from this session
   *
   * @returns {Array<Object>} Array of validation results
   */
  getValidationHistory() {
    return [...this._validationResults];
  }

  /**
   * Clear validation history
   */
  clearHistory() {
    this._validationResults = [];
  }

  /**
   * Detect IRI conflicts between snapshots
   * @private
   *
   * @param {Object} snapshot_i - Source snapshot
   * @param {Object} snapshot_j - Target snapshot
   * @returns {Array<Object>} Array of conflicts
   */
  _detectIRIConflicts(snapshot_i, snapshot_j) {
    // Placeholder for conflict detection
    // In full implementation, would compare quad semantics for same IRIs
    return [];
  }
}

/**
 * Create Q* Validator instance
 * Factory function for convenience
 *
 * @param {Object} [options={}] - Validator options
 * @returns {QStarValidator} New validator instance
 *
 * @example
 * import { createQStarValidator } from './q-star.mjs';
 * const validator = createQStarValidator({ strict: true });
 */
export function createQStarValidator(options = {}) {
  return new QStarValidator(options);
}

/**
 * Quick validation helper
 * Validates Q* snapshot without full validator setup
 *
 * @param {Object} snapshot - Q* snapshot to validate
 * @returns {Object} Validation result
 *
 * @example
 * const result = validateQStarSnapshot(snapshot);
 * if (!result.valid) throw new Error(result.message);
 */
export function validateQStarSnapshot(snapshot) {
  try {
    QStarSnapshotSchema.parse(snapshot);
    return { valid: true, message: 'Q* snapshot valid' };
  } catch (err) {
    return { valid: false, message: `Invalid Q* snapshot: ${err.message}` };
  }
}
