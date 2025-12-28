/**
 * KGC Multiverse - Morphism Composition Engine
 * Implements morphism composition with algebraic laws verification
 *
 * Composition: (phi_2 . phi_1)(x) = phi_2(phi_1(x))
 *
 * Laws:
 * - Associativity: (phi_3 . phi_2) . phi_1 = phi_3 . (phi_2 . phi_1)
 * - Identity: phi_id . phi = phi = phi . phi_id
 * - Non-commutativity detection: phi_1 . phi_2 != phi_2 . phi_1 (in general)
 *
 * @module @unrdf/kgc-multiverse/composition
 */

import { z } from 'zod';
import { blake3 } from 'hash-wasm';
import { MorphismType } from './morphism.mjs';
import { createQStarValidator, createQStarSnapshot } from './q-star.mjs';

/**
 * Composition Error Codes
 * @readonly
 * @enum {string}
 */
export const CompositionErrorCode = {
  INCOMPATIBLE_UNIVERSES: 'INCOMPATIBLE_UNIVERSES',
  QSTAR_VIOLATION: 'QSTAR_VIOLATION',
  EMPTY_MORPHISM: 'EMPTY_MORPHISM',
  INVALID_MORPHISM: 'INVALID_MORPHISM',
  COMPOSITION_FAILED: 'COMPOSITION_FAILED',
  NON_ASSOCIATIVE: 'NON_ASSOCIATIVE',
};

/**
 * Composition Receipt Schema
 */
const CompositionReceiptSchema = z.object({
  Q_ID: z.string().regex(/^Q\*_[a-f0-9]{16}$/),
  Q_RDF: z.string().url(),
  Q_PROV: z.object({
    timestamp: z.bigint(),
    morphismIDs: z.array(z.string()),
    compositionOrder: z.array(z.string()),
    sourceUniverseID: z.string(),
    targetUniverseID: z.string(),
    contentHash: z.string().regex(/^[a-f0-9]{64}$/),
    qstarValid: z.boolean(),
  }),
});

/**
 * Generate Composition Receipt ID
 *
 * @param {Array<string>} morphismIDs - IDs of composed morphisms
 * @param {bigint} timestamp - Composition timestamp
 * @returns {Promise<string>} Receipt Q*_ID
 */
async function generateCompositionReceiptID(morphismIDs, timestamp) {
  const combined = `comp-${morphismIDs.join('-')}-${timestamp}`;
  const hash = await blake3(combined);
  return `Q*_${hash.slice(0, 16)}`;
}

/**
 * Apply deltas to quads (create new state)
 * Immutable operation - returns new quad array
 *
 * @param {Array<Object>} quads - Current quads
 * @param {Array<Object>} deltas - Deltas to apply
 * @returns {Array<Object>} New quads after delta application
 */
function applyDeltas(quads, deltas) {
  // Create working copy as Map for O(1) lookup
  const quadMap = new Map();

  // Index existing quads
  for (const quad of quads) {
    const key = `${quad.subject?.value}|${quad.predicate?.value}|${quad.object?.value}|${quad.graph?.value || ''}`;
    quadMap.set(key, quad);
  }

  // Apply deltas
  for (const delta of deltas) {
    const key = `${delta.subject}|${delta.predicate}|${delta.object?.value}|${delta.graph || ''}`;

    if (delta.type === 'delete') {
      quadMap.delete(key);
    } else if (delta.type === 'add') {
      // Convert delta to quad format
      const quad = {
        subject: { value: delta.subject, termType: 'NamedNode' },
        predicate: { value: delta.predicate, termType: 'NamedNode' },
        object: {
          value: delta.object.value,
          termType: delta.object.type,
          ...(delta.object.datatype && { datatype: { value: delta.object.datatype } }),
          ...(delta.object.language && { language: delta.object.language }),
        },
        ...(delta.graph && { graph: { value: delta.graph, termType: 'NamedNode' } }),
      };
      quadMap.set(key, quad);
    }
  }

  return Array.from(quadMap.values());
}

/**
 * Composition Engine
 * Composes morphisms with Q* validation
 */
export class CompositionEngine {
  /**
   * Create Composition Engine
   *
   * @param {Object} [options={}] - Engine options
   * @param {boolean} [options.validateQStar=true] - Enable Q* validation
   * @param {boolean} [options.strictComposition=false] - Strict mode
   */
  constructor(options = {}) {
    /** @private */
    this._validateQStar = options.validateQStar !== false;
    /** @private */
    this._strict = options.strictComposition === true;
    /** @private */
    this._validator = createQStarValidator({ strict: this._strict });
    /** @private */
    this._compositionHistory = [];
  }

  /**
   * Compose two morphisms: phi_2 . phi_1
   * Creates a new morphism that applies phi_1 then phi_2
   *
   * @param {Object} phi1 - First morphism (applied first)
   * @param {Object} phi2 - Second morphism (applied second)
   * @param {Object} [options={}] - Composition options
   * @returns {Promise<Object>} Composed morphism
   * @throws {Error} If composition fails
   *
   * @example
   * const engine = new CompositionEngine();
   * const composed = await engine.compose(phi1, phi2);
   * const deltas = composed.transform(quads);
   */
  async compose(phi1, phi2, options = {}) {
    // Validate morphisms
    this._validateMorphism(phi1, 'phi1');
    this._validateMorphism(phi2, 'phi2');

    const timestamp = this._getTimestamp();

    // Create composed transform function
    const composedTransform = (quads) => {
      // Apply phi1 first
      const deltas1 = phi1.transform(quads);

      // Apply deltas to get intermediate state
      const intermediateQuads = applyDeltas(quads, deltas1);

      // Apply phi2 to intermediate state
      const deltas2 = phi2.transform(intermediateQuads);

      // Combine deltas (optimized - remove canceling operations)
      return this._optimizeDeltas([...deltas1, ...deltas2]);
    };

    // Generate ID
    const id = await this._generateComposedID(phi1, phi2, timestamp);

    const composed = {
      id,
      type: MorphismType.COMPOSITE,
      name: `${phi2.name} . ${phi1.name}`,
      description: `Composition: apply ${phi1.name} then ${phi2.name}`,
      transform: composedTransform,
      metadata: {
        composition: {
          morphisms: [phi1.id, phi2.id],
          order: [phi1.name, phi2.name],
          composedAt: timestamp,
        },
        source: phi1,
        target: phi2,
      },
    };

    this._compositionHistory.push({
      composed: composed.id,
      from: [phi1.id, phi2.id],
      timestamp,
    });

    return composed;
  }

  /**
   * Compose multiple morphisms: phi_n . ... . phi_2 . phi_1
   * Applies morphisms left to right (phi_1 first)
   *
   * @param {Array<Object>} morphisms - Array of morphisms
   * @returns {Promise<Object>} Composed morphism
   * @throws {Error} If composition fails
   *
   * @example
   * const composed = await engine.composeMany([phi1, phi2, phi3, phi4, phi5]);
   */
  async composeMany(morphisms) {
    if (!Array.isArray(morphisms)) {
      throw new TypeError('composeMany: morphisms must be array');
    }

    if (morphisms.length === 0) {
      throw new Error('composeMany: Cannot compose empty array');
    }

    if (morphisms.length === 1) {
      return morphisms[0];
    }

    // Compose left to right: ((phi1 . phi2) . phi3) ...
    let result = morphisms[0];

    for (let i = 1; i < morphisms.length; i++) {
      result = await this.compose(result, morphisms[i]);
    }

    return result;
  }

  /**
   * Create identity morphism
   * phi_id . phi = phi = phi . phi_id
   *
   * @returns {Promise<Object>} Identity morphism
   *
   * @example
   * const identity = await engine.createIdentity();
   * const same = await engine.compose(phi, identity);
   * // same is equivalent to phi
   */
  async createIdentity() {
    const timestamp = this._getTimestamp();
    const hash = await blake3(`identity-${timestamp}`);

    return {
      id: `PHI_${hash.slice(0, 16)}`,
      type: MorphismType.STATE,
      name: 'identity',
      description: 'Identity morphism (no transformation)',
      transform: () => [],
      metadata: {
        isIdentity: true,
        createdAt: timestamp,
      },
    };
  }

  /**
   * Verify associativity: (phi_3 . phi_2) . phi_1 = phi_3 . (phi_2 . phi_1)
   *
   * @param {Object} phi1 - First morphism
   * @param {Object} phi2 - Second morphism
   * @param {Object} phi3 - Third morphism
   * @param {Array<Object>} testQuads - Quads to test with
   * @returns {Promise<Object>} Associativity check result
   *
   * @example
   * const result = await engine.verifyAssociativity(phi1, phi2, phi3, quads);
   * console.log('Associative:', result.isAssociative);
   */
  async verifyAssociativity(phi1, phi2, phi3, testQuads) {
    // Left grouping: (phi_3 . phi_2) . phi_1
    const left_inner = await this.compose(phi2, phi3);
    const left_composed = await this.compose(phi1, left_inner);
    const left_deltas = left_composed.transform(testQuads);

    // Right grouping: phi_3 . (phi_2 . phi_1)
    const right_inner = await this.compose(phi1, phi2);
    const right_composed = await this.compose(right_inner, phi3);
    const right_deltas = right_composed.transform(testQuads);

    // Compare results
    const leftHash = await this._hashDeltas(left_deltas);
    const rightHash = await this._hashDeltas(right_deltas);

    const isAssociative = leftHash === rightHash;

    return {
      isAssociative,
      leftDeltaCount: left_deltas.length,
      rightDeltaCount: right_deltas.length,
      leftHash,
      rightHash,
      details: isAssociative ? null : {
        message: 'Delta hashes differ - morphisms may not be associative for this input',
      },
    };
  }

  /**
   * Verify identity law: phi_id . phi = phi = phi . phi_id
   *
   * @param {Object} phi - Morphism to test
   * @param {Array<Object>} testQuads - Quads to test with
   * @returns {Promise<Object>} Identity law check result
   *
   * @example
   * const result = await engine.verifyIdentityLaw(phi, quads);
   * console.log('Identity law holds:', result.holds);
   */
  async verifyIdentityLaw(phi, testQuads) {
    const identity = await this.createIdentity();

    // phi_id . phi
    const leftComposed = await this.compose(phi, identity);
    const leftDeltas = leftComposed.transform(testQuads);

    // phi . phi_id
    const rightComposed = await this.compose(identity, phi);
    const rightDeltas = rightComposed.transform(testQuads);

    // Original phi
    const originalDeltas = phi.transform(testQuads);

    // Compare hashes
    const leftHash = await this._hashDeltas(leftDeltas);
    const rightHash = await this._hashDeltas(rightDeltas);
    const originalHash = await this._hashDeltas(originalDeltas);

    const leftEquals = leftHash === originalHash;
    const rightEquals = rightHash === originalHash;
    const holds = leftEquals && rightEquals;

    return {
      holds,
      leftIdentityHolds: leftEquals,
      rightIdentityHolds: rightEquals,
      details: {
        originalDeltaCount: originalDeltas.length,
        leftDeltaCount: leftDeltas.length,
        rightDeltaCount: rightDeltas.length,
      },
    };
  }

  /**
   * Check commutativity: phi_1 . phi_2 = phi_2 . phi_1
   * Most morphisms are NOT commutative
   *
   * @param {Object} phi1 - First morphism
   * @param {Object} phi2 - Second morphism
   * @param {Array<Object>} testQuads - Quads to test with
   * @returns {Promise<Object>} Commutativity check result
   *
   * @example
   * const result = await engine.checkCommutativity(phi1, phi2, quads);
   * if (!result.commutes) {
   *   console.log('Order matters for these morphisms');
   * }
   */
  async checkCommutativity(phi1, phi2, testQuads) {
    // phi_1 . phi_2
    const composed12 = await this.compose(phi1, phi2);
    const deltas12 = composed12.transform(testQuads);

    // phi_2 . phi_1
    const composed21 = await this.compose(phi2, phi1);
    const deltas21 = composed21.transform(testQuads);

    // Compare
    const hash12 = await this._hashDeltas(deltas12);
    const hash21 = await this._hashDeltas(deltas21);

    const commutes = hash12 === hash21;

    return {
      commutes,
      hash12,
      hash21,
      delta12Count: deltas12.length,
      delta21Count: deltas21.length,
      warning: !commutes
        ? 'These morphisms do not commute - composition order matters'
        : null,
    };
  }

  /**
   * Generate composition receipt
   *
   * @param {Object} composed - Composed morphism
   * @param {string} sourceUniverseID - Source universe Q*_ID
   * @param {string} targetUniverseID - Target universe Q*_ID
   * @param {boolean} qstarValid - Q* validation result
   * @returns {Promise<Object>} Composition receipt
   */
  async generateReceipt(composed, sourceUniverseID, targetUniverseID, qstarValid) {
    const timestamp = this._getTimestamp();
    const morphismIDs = composed.metadata?.composition?.morphisms || [composed.id];

    const Q_ID = await generateCompositionReceiptID(morphismIDs, timestamp);

    const contentHash = await blake3(
      JSON.stringify({
        morphismIDs,
        sourceUniverseID,
        targetUniverseID,
        timestamp: timestamp.toString(),
      }),
    );

    const receipt = {
      Q_ID,
      Q_RDF: `http://kgc.io/composition-receipts/${Q_ID.slice(3)}`,
      Q_PROV: {
        timestamp,
        morphismIDs,
        compositionOrder: composed.metadata?.composition?.order || [composed.name],
        sourceUniverseID,
        targetUniverseID,
        contentHash,
        qstarValid,
      },
    };

    // Validate
    CompositionReceiptSchema.parse(receipt);

    return receipt;
  }

  /**
   * Compose with Q* validation
   * Full pipeline: compose, apply, validate Q*
   *
   * @param {Object} options - Composition options
   * @param {Object} options.phi1 - First morphism
   * @param {Object} options.phi2 - Second morphism
   * @param {Array<Object>} options.sourceQuads - Source quads
   * @param {string} options.sourceUniverseID - Source universe ID
   * @param {string} options.targetUniverseID - Target universe ID
   * @returns {Promise<Object>} Composition result with receipt
   */
  async composeWithValidation(options) {
    const {
      phi1,
      phi2,
      sourceQuads,
      sourceUniverseID,
      targetUniverseID,
    } = options;

    // Compose morphisms
    const composed = await this.compose(phi1, phi2);

    // Apply to get deltas
    const deltas = composed.transform(sourceQuads);

    // Apply deltas to get target quads
    const targetQuads = applyDeltas(sourceQuads, deltas);

    // Create snapshots for Q* validation
    const sourceSnapshot = await createQStarSnapshot({
      universeID: sourceUniverseID,
      universeRDF: `http://kgc.io/multiverse/${sourceUniverseID.slice(3)}`,
      quads: sourceQuads,
      receipts: [],
    });

    const targetSnapshot = await createQStarSnapshot({
      universeID: targetUniverseID,
      universeRDF: `http://kgc.io/multiverse/${targetUniverseID.slice(3)}`,
      quads: targetQuads,
      receipts: [],
    });

    // Validate Q*
    let qstarValid = true;
    let validationResult = null;

    if (this._validateQStar) {
      validationResult = this._validator.validateQStar({
        snapshot_i: sourceSnapshot,
        snapshot_j: targetSnapshot,
        receipts: [],
      });
      qstarValid = validationResult.allPassed;
    }

    // Generate receipt
    const receipt = await this.generateReceipt(
      composed,
      sourceUniverseID,
      targetUniverseID,
      qstarValid,
    );

    return {
      composed,
      deltas,
      targetQuads,
      receipt,
      qstarValid,
      validationResult,
    };
  }

  /**
   * Get composition history
   *
   * @returns {Array<Object>} Array of composition records
   */
  getHistory() {
    return [...this._compositionHistory];
  }

  /**
   * Clear composition history
   */
  clearHistory() {
    this._compositionHistory = [];
  }

  /**
   * Validate morphism structure
   * @private
   *
   * @param {Object} morphism - Morphism to validate
   * @param {string} name - Parameter name for error messages
   * @throws {Error} If morphism is invalid
   */
  _validateMorphism(morphism, name) {
    if (!morphism) {
      throw new Error(`${name}: Morphism is required`);
    }

    if (typeof morphism.transform !== 'function') {
      throw new Error(`${name}: Morphism must have transform function`);
    }

    if (!morphism.id) {
      throw new Error(`${name}: Morphism must have id`);
    }
  }

  /**
   * Generate composed morphism ID
   * @private
   *
   * @param {Object} phi1 - First morphism
   * @param {Object} phi2 - Second morphism
   * @param {bigint} timestamp - Composition timestamp
   * @returns {Promise<string>} Composed morphism ID
   */
  async _generateComposedID(phi1, phi2, timestamp) {
    const combined = `${phi1.id}-${phi2.id}-${timestamp}`;
    const hash = await blake3(combined);
    return `PHI_${hash.slice(0, 16)}`;
  }

  /**
   * Hash deltas for comparison
   * @private
   *
   * @param {Array<Object>} deltas - Deltas to hash
   * @returns {Promise<string>} BLAKE3 hash
   */
  async _hashDeltas(deltas) {
    const sorted = [...deltas].sort((a, b) => {
      const aKey = `${a.type}|${a.subject}|${a.predicate}`;
      const bKey = `${b.type}|${b.subject}|${b.predicate}`;
      return aKey.localeCompare(bKey);
    });

    const serialized = JSON.stringify(sorted);
    return blake3(serialized);
  }

  /**
   * Optimize deltas by removing canceling operations
   * @private
   *
   * @param {Array<Object>} deltas - Raw deltas
   * @returns {Array<Object>} Optimized deltas
   */
  _optimizeDeltas(deltas) {
    const adds = new Map();
    const deletes = new Map();

    for (const delta of deltas) {
      const key = `${delta.subject}|${delta.predicate}|${delta.object?.value}`;

      if (delta.type === 'add') {
        // If we're adding something we previously deleted, they cancel
        if (deletes.has(key)) {
          deletes.delete(key);
        } else {
          adds.set(key, delta);
        }
      } else if (delta.type === 'delete') {
        // If we're deleting something we previously added, they cancel
        if (adds.has(key)) {
          adds.delete(key);
        } else {
          deletes.set(key, delta);
        }
      }
    }

    return [...deletes.values(), ...adds.values()];
  }

  /**
   * Get current timestamp
   * @private
   *
   * @returns {bigint} Nanosecond timestamp
   */
  _getTimestamp() {
    return typeof process !== 'undefined' && process.hrtime
      ? process.hrtime.bigint()
      : BigInt(Date.now()) * 1_000_000n;
  }
}

/**
 * Create Composition Engine instance
 * Factory function for convenience
 *
 * @param {Object} [options={}] - Engine options
 * @returns {CompositionEngine} New engine instance
 *
 * @example
 * import { createCompositionEngine } from './composition.mjs';
 * const engine = createCompositionEngine({ validateQStar: true });
 */
export function createCompositionEngine(options = {}) {
  return new CompositionEngine(options);
}

/**
 * Quick compose helper
 * Composes two morphisms without full engine setup
 *
 * @param {Object} phi1 - First morphism
 * @param {Object} phi2 - Second morphism
 * @returns {Promise<Object>} Composed morphism
 *
 * @example
 * import { quickCompose } from './composition.mjs';
 * const composed = await quickCompose(phi1, phi2);
 */
export async function quickCompose(phi1, phi2) {
  const engine = new CompositionEngine({ validateQStar: false });
  return engine.compose(phi1, phi2);
}

/**
 * Verify all algebraic laws
 * Helper to check associativity, identity, and document commutativity
 *
 * @param {Array<Object>} morphisms - Morphisms to test (need at least 3)
 * @param {Array<Object>} testQuads - Quads to test with
 * @returns {Promise<Object>} Law verification results
 *
 * @example
 * const results = await verifyAlgebraicLaws([phi1, phi2, phi3], quads);
 * console.log('All laws hold:', results.allPass);
 */
export async function verifyAlgebraicLaws(morphisms, testQuads) {
  if (morphisms.length < 3) {
    throw new Error('verifyAlgebraicLaws: Need at least 3 morphisms');
  }

  const engine = new CompositionEngine({ validateQStar: false });

  const [phi1, phi2, phi3] = morphisms;

  const associativity = await engine.verifyAssociativity(phi1, phi2, phi3, testQuads);
  const identity = await engine.verifyIdentityLaw(phi1, testQuads);
  const commutativity = await engine.checkCommutativity(phi1, phi2, testQuads);

  return {
    allPass: associativity.isAssociative && identity.holds,
    associativity,
    identity,
    commutativity,
  };
}
