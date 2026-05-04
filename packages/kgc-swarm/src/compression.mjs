/**
 * @file compression.mjs
 * @description Compression operator μ with idempotent properties
 *
 * Mathematical Properties:
 * - μ: O → A (Observable to Archive transformation)
 * - Idempotence: μ ∘ μ = μ  (i.e., μ(μ(O)) = μ(O))
 * - Merge operator: ⊔ (lattice join, associative & commutative)
 * - Cover function: Cover(O) extracts structural elements
 * - Glue function: Γ(O) := glue(Cover(O)) - reconstruction from cover
 *
 * Core Invariants:
 * 1. Hash stability: hash(μ(O)) = hash(μ(μ(O)))
 * 2. Merge associativity: (O₁ ⊔ O₂) ⊔ O₃ = O₁ ⊔ (O₂ ⊔ O₃)
 * 3. Merge commutativity: O₁ ⊔ O₂ = O₂ ⊔ O₁
 * 4. Idempotent merge: O ⊔ O = O
 */

import { blake3 } from 'hash-wasm';
import { z } from 'zod';

/**
 * Observable schema - represents temporal observations
 */
const ObservableSchema = z.object({
  id: z.string(),
  timestamp: z.number(),
  data: z.any(),
  metadata: z.any().optional(),
});

/**
 * Archive schema - compressed, deduplicated representation
 */
const ArchiveSchema = z.object({
  hash: z.string(),
  observables: z.array(z.any()),
  cover: z.array(z.string()),
  glue: z.any(),
  compressed: z.boolean(),
});

/**
 * Compute stable hash of observable data
 * @param {unknown} data - Data to hash
 * @returns {Promise<string>} Hash string
 */
async function computeHash(data) {
  const normalized = JSON.stringify(data, Object.keys(data).sort());
  return blake3(normalized);
}

/**
 * Cover function: Cover(O_τ) - Extract structural elements from observable
 *
 * The cover extracts unique keys across all observables, forming a basis
 * for the compression lattice.
 *
 * @param {Array<object>} observables - Array of observable objects
 * @returns {Array<string>} Unique structural keys (the cover)
 */
export function cover(observables) {
  const keys = new Set();

  for (const obs of observables) {
    // Extract all keys from data and metadata
    Object.keys(obs.data || {}).forEach(k => keys.add(k));
    Object.keys(obs.metadata || {}).forEach(k => keys.add(`meta.${k}`));
  }

  return Array.from(keys).sort(); // Sorted for deterministic output
}

/**
 * Glue function: glue(Cover(O_τ)) - Create index mapping from cover
 *
 * The glue function creates a mapping from each cover element to the
 * indices of observables that contain that element. This enables
 * efficient reconstruction and deduplication.
 *
 * @param {Array<object>} observables - Array of observable objects
 * @param {Array<string>} coverSet - The cover (structural keys)
 * @returns {Record<string, Array<number>>} Mapping from keys to observable indices
 */
export function glue(observables, coverSet) {
  const mapping = Object.fromEntries(coverSet.map(k => [k, []]));

  observables.forEach((obs, idx) => {
    coverSet.forEach(key => {
      if (key.startsWith('meta.')) {
        const metaKey = key.slice(5);
        if (obs.metadata?.[metaKey] !== undefined) {
          mapping[key].push(idx);
        }
      } else {
        if (obs.data?.[key] !== undefined) {
          mapping[key].push(idx);
        }
      }
    });
  });

  return mapping;
}

/**
 * Γ(O_τ) := glue(Cover(O_τ)) - Combined cover-glue operation
 *
 * @param {Array<object>} observables - Array of observable objects
 * @returns {{ cover: Array<string>, glue: Record<string, Array<number>> }}
 */
export function gamma(observables) {
  const coverSet = cover(observables);
  const glueMap = glue(observables, coverSet);
  return { cover: coverSet, glue: glueMap };
}

/**
 * Deduplicate observables using hash-based identity
 *
 * @param {Array<object>} observables - Array of observable objects
 * @returns {Promise<Array<object>>} Deduplicated observables
 */
async function deduplicate(observables) {
  const seen = new Map();
  const unique = [];

  for (const obs of observables) {
    const hash = await computeHash(obs);
    if (!seen.has(hash)) {
      seen.set(hash, true);
      unique.push(obs);
    }
  }

  return unique;
}

/**
 * Merge operator: O_τ ⊔ ΔO_{τ+1}
 *
 * Properties:
 * - Associativity: (O₁ ⊔ O₂) ⊔ O₃ = O₁ ⊔ (O₂ ⊔ O₃)
 * - Commutativity: O₁ ⊔ O₂ = O₂ ⊔ O₁
 * - Idempotence: O ⊔ O = O
 *
 * @param {Array<object>} obs1 - First set of observables
 * @param {Array<object>} obs2 - Second set of observables
 * @returns {Promise<Array<object>>} Merged and deduplicated observables
 */
export async function merge(obs1, obs2) {
  const combined = [...obs1, ...obs2];
  return deduplicate(combined);
}

/**
 * Compression operator μ: O → A
 *
 * Mathematical properties:
 * - Idempotence: μ ∘ μ = μ
 * - Hash stability: hash(μ(O)) = hash(μ(μ(O)))
 * - Preserves observable semantics
 *
 * Implementation strategy:
 * 1. Validate input observables
 * 2. Deduplicate using hash-based identity
 * 3. Sort by timestamp for deterministic ordering
 * 4. Compute cover and glue
 * 5. Compute stable hash of result
 *
 * @param {Array<object> | object} input - Observables or pre-compressed archive
 * @returns {Promise<object>} Archive object with hash, observables, cover, glue
 *
 * @example
 * const obs = [
 *   { id: '1', timestamp: 100, data: { x: 1, y: 2 } },
 *   { id: '2', timestamp: 200, data: { x: 3, z: 4 } }
 * ];
 * const archive = await compress(obs);
 * const recompressed = await compress(archive); // μ(μ(O))
 * assert(archive.hash === recompressed.hash); // Idempotence proof
 */
export async function compress(input) {
  // If already compressed (archive), extract observables for recompression
  let observables;

  if (input && typeof input === 'object' && 'compressed' in input && input.compressed === true) {
    // Already an archive - extract observables for idempotent reprocessing
    observables = input.observables || [];
  } else if (Array.isArray(input)) {
    observables = input;
  } else {
    observables = [input];
  }

  // Validate observables
  const validated = observables.map(obs => {
    try {
      return ObservableSchema.parse(obs);
    } catch (e) {
      // If validation fails, try to coerce into valid observable
      return {
        id: obs.id || String(Math.random()),
        timestamp: obs.timestamp || Date.now(),
        data: obs.data || obs,
        metadata: obs.metadata,
      };
    }
  });

  // Step 1: Deduplicate
  const deduplicated = await deduplicate(validated);

  // Step 2: Sort by timestamp for deterministic ordering
  const sorted = deduplicated.sort((a, b) => a.timestamp - b.timestamp);

  // Step 3: Compute cover and glue (Γ operation)
  const { cover: coverSet, glue: glueMap } = gamma(sorted);

  // Step 4: Create archive structure
  const archive = {
    observables: sorted,
    cover: coverSet,
    glue: glueMap,
    compressed: true,
    hash: '', // Computed next
  };

  // Step 5: Compute stable hash (excluding hash field itself)
  const hashInput = {
    observables: sorted,
    cover: coverSet,
    glue: glueMap,
  };
  archive.hash = await computeHash(hashInput);

  // Validate result
  return ArchiveSchema.parse(archive);
}

/**
 * Verify idempotence property: μ ∘ μ = μ
 *
 * @param {Array<object>} observables - Input observables
 * @returns {Promise<{ valid: boolean, proof: object }>} Verification result
 */
export async function verifyIdempotence(observables) {
  const mu1 = await compress(observables);
  const mu2 = await compress(mu1);

  return {
    valid: mu1.hash === mu2.hash,
    proof: {
      hash_mu1: mu1.hash,
      hash_mu2: mu2.hash,
      observables_mu1: mu1.observables.length,
      observables_mu2: mu2.observables.length,
      equal: mu1.hash === mu2.hash,
    },
  };
}

/**
 * Verify merge associativity: (O₁ ⊔ O₂) ⊔ O₃ = O₁ ⊔ (O₂ ⊔ O₃)
 *
 * @param {Array<object>} obs1 - First observable set
 * @param {Array<object>} obs2 - Second observable set
 * @param {Array<object>} obs3 - Third observable set
 * @returns {Promise<{ valid: boolean, proof: object }>} Verification result
 */
export async function verifyAssociativity(obs1, obs2, obs3) {
  // Left association: (O₁ ⊔ O₂) ⊔ O₃
  const left12 = await merge(obs1, obs2);
  const leftResult = await merge(left12, obs3);

  // Right association: O₁ ⊔ (O₂ ⊔ O₃)
  const right23 = await merge(obs2, obs3);
  const rightResult = await merge(obs1, right23);

  // Compare via compression (ensures structural equality)
  const leftCompressed = await compress(leftResult);
  const rightCompressed = await compress(rightResult);

  return {
    valid: leftCompressed.hash === rightCompressed.hash,
    proof: {
      left_hash: leftCompressed.hash,
      right_hash: rightCompressed.hash,
      left_count: leftResult.length,
      right_count: rightResult.length,
      equal: leftCompressed.hash === rightCompressed.hash,
    },
  };
}

export default {
  compress,
  merge,
  cover,
  glue,
  gamma,
  verifyIdempotence,
  verifyAssociativity,
};
