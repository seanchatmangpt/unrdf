/**
 * @fileoverview Delta Capsule - Represents proposed changes to RDF partitions
 *
 * A Δ capsule encapsulates:
 * - Target partition where changes will be applied
 * - Proposed RDF triples (additions/deletions)
 * - Invariants that must be preserved
 * - Metadata for provenance tracking
 *
 * All changes to the RDF store MUST go through admission control via Δ capsules.
 *
 * @module admission/delta-capsule
 */

import { z } from 'zod';
import { createHash } from 'node:crypto';

/**
 * Zod schema for RDF Quad structure
 */
export const QuadSchema = z.object({
  subject: z.object({
    termType: z.enum(['NamedNode', 'BlankNode']),
    value: z.string().min(1)
  }),
  predicate: z.object({
    termType: z.literal('NamedNode'),
    value: z.string().url()
  }),
  object: z.object({
    termType: z.enum(['NamedNode', 'BlankNode', 'Literal']),
    value: z.string()
  }),
  graph: z.object({
    termType: z.enum(['NamedNode', 'DefaultGraph']),
    value: z.string().optional()
  }).optional()
});

/**
 * Zod schema for operation type
 */
export const OperationTypeSchema = z.enum(['add', 'delete', 'update']);

/**
 * Zod schema for proposed change
 */
export const ProposedChangeSchema = z.object({
  operation: OperationTypeSchema,
  quads: z.array(QuadSchema).min(1),
  reason: z.string().optional()
});

/**
 * Zod schema for partition identifier
 */
export const PartitionSchema = z.object({
  namespace: z.string().url(),
  name: z.string().min(1).max(100),
  protected: z.boolean().default(false)
});

/**
 * Zod schema for invariant reference
 */
export const InvariantRefSchema = z.object({
  name: z.string().min(1),
  enabled: z.boolean().default(true),
  strictness: z.enum(['error', 'warning', 'info']).default('error')
});

/**
 * Zod schema for provenance metadata
 */
export const ProvenanceSchema = z.object({
  agent: z.string().min(1),
  timestamp: z.string().datetime(),
  source: z.string().optional(),
  justification: z.string().optional()
});

/**
 * Zod schema for complete Delta Capsule
 */
export const DeltaCapsuleSchema = z.object({
  id: z.string().uuid(),
  partition: PartitionSchema,
  changes: z.array(ProposedChangeSchema).min(1),
  invariants: z.array(InvariantRefSchema).min(1),
  provenance: ProvenanceSchema,
  metadata: z.record(z.any()).optional()
});

/**
 * Delta Capsule class - Represents a proposed change batch
 *
 * @class DeltaCapsule
 * @example
 * const delta = new DeltaCapsule({
 *   partition: { namespace: 'http://example.org/', name: 'overlay-1' },
 *   changes: [{
 *     operation: 'add',
 *     quads: [quad1, quad2]
 *   }],
 *   invariants: [{ name: 'Q_typing' }, { name: 'Q_noncollision' }],
 *   provenance: {
 *     agent: 'user-123',
 *     timestamp: new Date().toISOString()
 *   }
 * });
 */
export class DeltaCapsule {
  /**
   * Create a new Delta Capsule
   * @param {Object} config - Capsule configuration
   * @throws {Error} If configuration is invalid
   */
  constructor(config) {
    // Validate and normalize config
    const validated = DeltaCapsuleSchema.parse({
      id: config.id || crypto.randomUUID(),
      partition: config.partition,
      changes: config.changes,
      invariants: config.invariants,
      provenance: config.provenance,
      metadata: config.metadata || {}
    });

    Object.assign(this, validated);

    // Compute hash for determinism checking
    this._hash = this._computeHash();
  }

  /**
   * Get all quads from all changes
   * @returns {Array} All proposed quads
   */
  getAllQuads() {
    return this.changes.flatMap(change => change.quads);
  }

  /**
   * Get quads by operation type
   * @param {string} operation - Operation type (add/delete/update)
   * @returns {Array} Quads for specified operation
   */
  getQuadsByOperation(operation) {
    return this.changes
      .filter(change => change.operation === operation)
      .flatMap(change => change.quads);
  }

  /**
   * Count total number of proposed quads
   * @returns {number} Total quad count
   */
  getQuadCount() {
    return this.getAllQuads().length;
  }

  /**
   * Get all namespaces used in this capsule
   * @returns {Set<string>} Set of namespace IRIs
   */
  getNamespaces() {
    const namespaces = new Set();

    for (const quad of this.getAllQuads()) {
      // Extract namespace from IRIs
      if (quad.subject.termType === 'NamedNode') {
        const ns = this._extractNamespace(quad.subject.value);
        if (ns) namespaces.add(ns);
      }

      const predNs = this._extractNamespace(quad.predicate.value);
      if (predNs) namespaces.add(predNs);

      if (quad.object.termType === 'NamedNode') {
        const objNs = this._extractNamespace(quad.object.value);
        if (objNs) namespaces.add(objNs);
      }
    }

    return namespaces;
  }

  /**
   * Extract namespace from IRI
   * @param {string} iri - IRI to process
   * @returns {string|null} Namespace or null
   * @private
   */
  _extractNamespace(iri) {
    try {
      const url = new URL(iri);
      // Return protocol + host + path up to last /
      const lastSlash = url.pathname.lastIndexOf('/');
      if (lastSlash >= 0) {
        return `${url.protocol}//${url.host}${url.pathname.substring(0, lastSlash + 1)}`;
      }
      return `${url.protocol}//${url.host}/`;
    } catch {
      return null;
    }
  }

  /**
   * Check if this capsule is additive only (no deletions or updates)
   * @returns {boolean} True if only additions
   */
  isAdditiveOnly() {
    return this.changes.every(change => change.operation === 'add');
  }

  /**
   * Check if a specific invariant is enabled
   * @param {string} name - Invariant name
   * @returns {boolean} True if enabled
   */
  hasInvariant(name) {
    return this.invariants.some(inv => inv.name === name && inv.enabled);
  }

  /**
   * Get invariant configuration
   * @param {string} name - Invariant name
   * @returns {Object|null} Invariant config or null
   */
  getInvariant(name) {
    return this.invariants.find(inv => inv.name === name) || null;
  }

  /**
   * Compute deterministic hash of this capsule
   * Same content → Same hash (for Q_determinism)
   * @returns {string} SHA-256 hash
   * @private
   */
  _computeHash() {
    // Create canonical representation
    const canonical = {
      partition: this.partition,
      changes: this.changes.map(change => ({
        operation: change.operation,
        quads: change.quads.map(q => ({
          s: q.subject.value,
          p: q.predicate.value,
          o: q.object.value,
          g: q.graph?.value || ''
        })).sort((a, b) => `${a.s}${a.p}${a.o}${a.g}`.localeCompare(`${b.s}${b.p}${b.o}${b.g}`))
      })).sort((a, b) => a.operation.localeCompare(b.operation)),
      invariants: [...this.invariants].sort((a, b) => a.name.localeCompare(b.name))
    };

    const json = JSON.stringify(canonical);
    return createHash('sha256').update(json).digest('hex');
  }

  /**
   * Get capsule hash for determinism checking
   * @returns {string} SHA-256 hash
   */
  getHash() {
    return this._hash;
  }

  /**
   * Convert capsule to JSON representation
   * @returns {Object} JSON representation
   */
  toJSON() {
    return {
      id: this.id,
      partition: this.partition,
      changes: this.changes,
      invariants: this.invariants,
      provenance: this.provenance,
      metadata: this.metadata,
      hash: this._hash,
      stats: {
        quadCount: this.getQuadCount(),
        isAdditiveOnly: this.isAdditiveOnly(),
        namespaces: Array.from(this.getNamespaces())
      }
    };
  }

  /**
   * Create a DeltaCapsule from JSON
   * @param {Object} json - JSON representation
   * @returns {DeltaCapsule} New capsule instance
   */
  static fromJSON(json) {
    return new DeltaCapsule({
      id: json.id,
      partition: json.partition,
      changes: json.changes,
      invariants: json.invariants,
      provenance: json.provenance,
      metadata: json.metadata
    });
  }

  /**
   * Validate a raw object as a Delta Capsule
   * @param {Object} obj - Object to validate
   * @returns {Object} Validation result {valid: boolean, error?: string}
   */
  static validate(obj) {
    try {
      DeltaCapsuleSchema.parse(obj);
      return { valid: true };
    } catch (error) {
      return {
        valid: false,
        error: error.message,
        issues: error.issues || []
      };
    }
  }
}
