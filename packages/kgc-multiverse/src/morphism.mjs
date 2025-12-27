/**
 * KGC Multiverse - Morphism Algebra
 * Implements Φ (Phi) morphisms for knowledge graph transformations
 *
 * Morphism Types:
 * - Schema Morphism (Φ_schema): Transforms RDF schemas/ontologies
 * - State Morphism (Φ_state): Transforms instance data/quads
 * - Artifact Morphism (Φ_artifact): Transforms derived artifacts
 *
 * @module @unrdf/kgc-multiverse/morphism
 */

import { z } from 'zod';
import { blake3 } from 'hash-wasm';
import { guardMorphismApplication } from './guards.mjs';

/**
 * Morphism Type Enum
 * @readonly
 * @enum {string}
 */
export const MorphismType = {
  SCHEMA: 'SCHEMA',     // Schema/ontology transformation
  STATE: 'STATE',       // Instance data transformation
  ARTIFACT: 'ARTIFACT', // Derived artifact transformation
  COMPOSITE: 'COMPOSITE', // Composition of multiple morphisms
};

/**
 * Morphism Schema (Zod validation)
 */
const MorphismSchema = z.object({
  id: z.string(),                           // Unique morphism ID
  type: z.nativeEnum(MorphismType),         // Morphism type
  name: z.string(),                         // Human-readable name
  description: z.string().optional(),       // Optional description
  transform: z.function(),                  // Transformation function
  inverse: z.function().optional(),         // Optional inverse transformation
  metadata: z.record(z.any()).optional(),   // Arbitrary metadata
});

/**
 * Delta Schema (Quad change representation)
 */
const DeltaSchema = z.object({
  type: z.enum(['add', 'delete']),          // Operation type
  subject: z.string(),                      // Subject URI or blank node
  predicate: z.string(),                    // Predicate URI
  object: z.object({                        // Object value
    type: z.enum(['NamedNode', 'BlankNode', 'Literal']),
    value: z.string(),
    datatype: z.string().optional(),        // Literal datatype
    language: z.string().optional(),        // Literal language tag
  }),
  graph: z.string().optional(),             // Named graph (default: Universe graph)
});

/**
 * Generate unique morphism ID
 *
 * @param {string} name - Morphism name
 * @returns {Promise<string>} Unique morphism ID
 *
 * @example
 * const id = await generateMorphismID('rename-predicate');
 * console.assert(id.startsWith('Φ_'), 'Morphism ID has Φ prefix');
 */
export async function generateMorphismID(name) {
  const timestamp = BigInt(Date.now()) * 1_000_000n;
  const combined = `${name}-${timestamp}`;
  const hash = await blake3(combined);
  return `Φ_${hash.slice(0, 16)}`;
}

/**
 * Create a morphism
 *
 * @param {Object} options - Morphism configuration
 * @param {string} options.type - Morphism type (from MorphismType enum)
 * @param {string} options.name - Human-readable name
 * @param {Function} options.transform - Transformation function (quads → deltas)
 * @param {Function} [options.inverse] - Optional inverse transformation
 * @param {string} [options.description] - Optional description
 * @param {Object} [options.metadata] - Arbitrary metadata
 * @returns {Promise<Object>} Created morphism
 * @throws {Error} If validation fails
 *
 * @example
 * import { createMorphism, MorphismType } from './morphism.mjs';
 * const phi = await createMorphism({
 *   type: MorphismType.SCHEMA,
 *   name: 'rename-predicate',
 *   transform: (quads) => quads.map(q => ({ type: 'add', ...q })),
 * });
 * console.assert(phi.id.startsWith('Φ_'), 'Has morphism ID');
 */
export async function createMorphism(options) {
  if (typeof options.transform !== 'function') {
    throw new TypeError('createMorphism: transform must be a function');
  }

  const id = await generateMorphismID(options.name);

  const morphism = {
    id,
    type: options.type,
    name: options.name,
    description: options.description,
    transform: options.transform,
    inverse: options.inverse,
    metadata: options.metadata || {},
  };

  // Validate
  MorphismSchema.parse(morphism);

  return morphism;
}

/**
 * Apply morphism to universe state
 * Φ: U → U' (Universe → Modified Universe)
 *
 * @param {Object} morphism - Morphism to apply
 * @param {Array<Object>} quads - Input quads (universe state)
 * @param {string} universeState - Current universe state
 * @returns {Array<Object>} Array of deltas (add/delete operations)
 * @throws {Error} If morphism application fails
 *
 * @example
 * const deltas = applyMorphism(phi, quads, 'ACTIVE');
 * console.log('Generated deltas:', deltas.length);
 */
export function applyMorphism(morphism, quads, universeState) {
  // Guard: Cannot apply to FROZEN or DISCARDED
  guardMorphismApplication(universeState);

  // Validate morphism
  if (!morphism || typeof morphism.transform !== 'function') {
    throw new TypeError('applyMorphism: morphism must have transform function');
  }

  if (!Array.isArray(quads)) {
    throw new TypeError('applyMorphism: quads must be array');
  }

  try {
    // Apply transformation
    const deltas = morphism.transform(quads);

    // Validate each delta
    if (!Array.isArray(deltas)) {
      throw new Error('applyMorphism: transform must return array of deltas');
    }

    deltas.forEach((delta, idx) => {
      try {
        DeltaSchema.parse(delta);
      } catch (err) {
        throw new Error(`applyMorphism: Invalid delta at index ${idx}: ${err.message}`);
      }
    });

    return deltas;
  } catch (error) {
    throw new Error(`applyMorphism failed: ${error.message}`);
  }
}

/**
 * Compose two morphisms: Φ₂ ∘ Φ₁
 * Creates a new morphism that applies Φ₁ then Φ₂
 *
 * @param {Object} phi1 - First morphism
 * @param {Object} phi2 - Second morphism
 * @returns {Promise<Object>} Composed morphism
 *
 * @example
 * const composed = await composeMorphisms(phi1, phi2);
 * const deltas = applyMorphism(composed, quads, 'ACTIVE');
 */
export async function composeMorphisms(phi1, phi2) {
  if (!phi1 || !phi2) {
    throw new TypeError('composeMorphisms: Both morphisms required');
  }

  const id = await generateMorphismID(`${phi1.name}∘${phi2.name}`);

  return {
    id,
    type: MorphismType.COMPOSITE,
    name: `${phi1.name} ∘ ${phi2.name}`,
    description: `Composition of ${phi1.name} then ${phi2.name}`,
    transform: (quads) => {
      // Apply phi1, then phi2 to results
      const deltas1 = phi1.transform(quads);

      // Apply deltas1 to get intermediate quads
      // (simplified - in full implementation, would apply deltas to quad set)
      const deltas2 = phi2.transform(quads);

      return [...deltas1, ...deltas2];
    },
    metadata: {
      composition: [phi1.id, phi2.id],
    },
  };
}

/**
 * Identity morphism: Φ_id
 * Returns empty delta set (no changes)
 *
 * @returns {Promise<Object>} Identity morphism
 *
 * @example
 * const identity = await createIdentityMorphism();
 * const deltas = applyMorphism(identity, quads, 'ACTIVE');
 * console.assert(deltas.length === 0, 'Identity produces no deltas');
 */
export async function createIdentityMorphism() {
  return createMorphism({
    type: MorphismType.STATE,
    name: 'identity',
    description: 'Identity morphism (no transformation)',
    transform: () => [], // No changes
  });
}

/**
 * Predicate Rename Morphism
 * Renames all occurrences of oldPredicate to newPredicate
 *
 * @param {string} oldPredicate - Old predicate URI
 * @param {string} newPredicate - New predicate URI
 * @returns {Promise<Object>} Rename morphism
 *
 * @example
 * const rename = await createPredicateRenameMorphism(
 *   'http://old.com/prop',
 *   'http://new.com/prop'
 * );
 */
export async function createPredicateRenameMorphism(oldPredicate, newPredicate) {
  return createMorphism({
    type: MorphismType.SCHEMA,
    name: `rename-${oldPredicate.split('/').pop()}`,
    description: `Rename ${oldPredicate} to ${newPredicate}`,
    transform: (quads) => {
      const deltas = [];

      for (const quad of quads) {
        if (quad.predicate.value === oldPredicate) {
          // Delete old triple
          deltas.push({
            type: 'delete',
            subject: quad.subject.value,
            predicate: oldPredicate,
            object: {
              type: quad.object.termType,
              value: quad.object.value,
              ...(quad.object.datatype && { datatype: quad.object.datatype.value }),
              ...(quad.object.language && { language: quad.object.language }),
            },
          });

          // Add new triple with renamed predicate
          deltas.push({
            type: 'add',
            subject: quad.subject.value,
            predicate: newPredicate,
            object: {
              type: quad.object.termType,
              value: quad.object.value,
              ...(quad.object.datatype && { datatype: quad.object.datatype.value }),
              ...(quad.object.language && { language: quad.object.language }),
            },
          });
        }
      }

      return deltas;
    },
    metadata: {
      oldPredicate,
      newPredicate,
    },
  });
}

/**
 * Filter Morphism
 * Removes quads matching a predicate
 *
 * @param {Function} filterFn - Filter function (quad → boolean)
 * @returns {Promise<Object>} Filter morphism
 *
 * @example
 * const filter = await createFilterMorphism(
 *   q => q.predicate.value === 'http://example.com/deprecated'
 * );
 */
export async function createFilterMorphism(filterFn) {
  if (typeof filterFn !== 'function') {
    throw new TypeError('createFilterMorphism: filterFn must be function');
  }

  return createMorphism({
    type: MorphismType.STATE,
    name: 'filter-quads',
    description: 'Remove quads matching filter predicate',
    transform: (quads) => {
      const deltas = [];

      for (const quad of quads) {
        if (filterFn(quad)) {
          // Delete matching quad
          deltas.push({
            type: 'delete',
            subject: quad.subject.value,
            predicate: quad.predicate.value,
            object: {
              type: quad.object.termType,
              value: quad.object.value,
              ...(quad.object.datatype && { datatype: quad.object.datatype.value }),
              ...(quad.object.language && { language: quad.object.language }),
            },
          });
        }
      }

      return deltas;
    },
  });
}

/**
 * Map Morphism
 * Applies a transformation to each quad
 *
 * @param {Function} mapFn - Map function (quad → quad)
 * @returns {Promise<Object>} Map morphism
 *
 * @example
 * const map = await createMapMorphism(
 *   q => ({ ...q, object: { ...q.object, value: q.object.value.toUpperCase() } })
 * );
 */
export async function createMapMorphism(mapFn) {
  if (typeof mapFn !== 'function') {
    throw new TypeError('createMapMorphism: mapFn must be function');
  }

  return createMorphism({
    type: MorphismType.STATE,
    name: 'map-quads',
    description: 'Transform each quad via map function',
    transform: (quads) => {
      const deltas = [];

      for (const quad of quads) {
        const transformed = mapFn(quad);

        // Delete original
        deltas.push({
          type: 'delete',
          subject: quad.subject.value,
          predicate: quad.predicate.value,
          object: {
            type: quad.object.termType,
            value: quad.object.value,
            ...(quad.object.datatype && { datatype: quad.object.datatype.value }),
            ...(quad.object.language && { language: quad.object.language }),
          },
        });

        // Add transformed
        deltas.push({
          type: 'add',
          subject: transformed.subject.value,
          predicate: transformed.predicate.value,
          object: {
            type: transformed.object.termType,
            value: transformed.object.value,
            ...(transformed.object.datatype && { datatype: transformed.object.datatype.value }),
            ...(transformed.object.language && { language: transformed.object.language }),
          },
        });
      }

      return deltas;
    },
  });
}
