/**
 * @fileoverview Distance and similarity functions for hyperdimensional vectors
 *
 * Provides efficient nearest-neighbor, clustering, and ranking operations
 * optimized for browser/Node JS environments with Float32Array.
 *
 * @module @unrdf/kgc-4d/hdit/distance
 */

/**
 * Calculate cosine similarity between two vectors
 * Returns value in [-1, 1] where 1 = identical direction
 *
 * Fast and scale-invariant - best for semantic similarity
 *
 * @param {Float32Array} a - First vector
 * @param {Float32Array} b - Second vector
 * @returns {number} Cosine similarity in [-1, 1]
 *
 * @example
 * const sim = cosineSimilarity(coordsA, coordsB);
 * if (sim > 0.9) console.log('Very similar events');
 */
export function cosineSimilarity(a, b) {
  if (a.length !== b.length) {
    throw new Error(`Vector dimension mismatch: ${a.length} vs ${b.length}`);
  }

  let dotProduct = 0;
  let normA = 0;
  let normB = 0;

  for (let i = 0; i < a.length; i++) {
    dotProduct += a[i] * b[i];
    normA += a[i] * a[i];
    normB += b[i] * b[i];
  }

  const denominator = Math.sqrt(normA * normB);
  return denominator === 0 ? 0 : dotProduct / denominator;
}

/**
 * Calculate cosine distance (1 - cosine similarity)
 * Returns value in [0, 2] where 0 = identical
 *
 * @param {Float32Array} a - First vector
 * @param {Float32Array} b - Second vector
 * @returns {number} Cosine distance in [0, 2]
 */
export function cosineDistance(a, b) {
  return 1 - cosineSimilarity(a, b);
}

/**
 * Calculate Euclidean distance between two vectors
 * Standard L2 distance metric
 *
 * @param {Float32Array} a - First vector
 * @param {Float32Array} b - Second vector
 * @returns {number} Euclidean distance (≥ 0)
 *
 * @example
 * const dist = euclideanDistance(coordsA, coordsB);
 * console.log(`Events are ${dist.toFixed(3)} apart in feature space`);
 */
export function euclideanDistance(a, b) {
  if (a.length !== b.length) {
    throw new Error(`Vector dimension mismatch: ${a.length} vs ${b.length}`);
  }

  let sumSquares = 0;
  for (let i = 0; i < a.length; i++) {
    const diff = a[i] - b[i];
    sumSquares += diff * diff;
  }

  return Math.sqrt(sumSquares);
}

/**
 * Calculate squared Euclidean distance (faster, no sqrt)
 * Use when only relative distances matter
 *
 * @param {Float32Array} a - First vector
 * @param {Float32Array} b - Second vector
 * @returns {number} Squared Euclidean distance (≥ 0)
 */
export function euclideanDistanceSquared(a, b) {
  if (a.length !== b.length) {
    throw new Error(`Vector dimension mismatch: ${a.length} vs ${b.length}`);
  }

  let sumSquares = 0;
  for (let i = 0; i < a.length; i++) {
    const diff = a[i] - b[i];
    sumSquares += diff * diff;
  }

  return sumSquares;
}

/**
 * Calculate Manhattan distance (L1 distance)
 * Sum of absolute differences
 *
 * @param {Float32Array} a - First vector
 * @param {Float32Array} b - Second vector
 * @returns {number} Manhattan distance (≥ 0)
 */
export function manhattanDistance(a, b) {
  if (a.length !== b.length) {
    throw new Error(`Vector dimension mismatch: ${a.length} vs ${b.length}`);
  }

  let sum = 0;
  for (let i = 0; i < a.length; i++) {
    sum += Math.abs(a[i] - b[i]);
  }

  return sum;
}

/**
 * Find K nearest neighbors to a query vector
 * Uses specified distance metric (default: cosine)
 *
 * @param {Float32Array} query - Query vector
 * @param {Float32Array[]} vectors - Candidate vectors
 * @param {number} k - Number of neighbors to return
 * @param {'cosine' | 'euclidean' | 'manhattan'} [metric='cosine'] - Distance metric
 * @returns {Array<{index: number, distance: number}>} K nearest neighbors
 *
 * @example
 * const neighbors = findKNearest(queryCoords, allEventCoords, 10, 'cosine');
 * console.log('Top 10 similar events:', neighbors.map(n => events[n.index]));
 */
export function findKNearest(query, vectors, k, metric = 'cosine') {
  const distanceFn = getDistanceFunction(metric);

  // Calculate all distances
  const distances = vectors.map((vec, index) => ({
    index,
    distance: distanceFn(query, vec),
  }));

  // Sort and take top K
  // All distance metrics (including cosineDistance) have lower = better
  // So we always sort ascending
  distances.sort((a, b) => a.distance - b.distance);

  return distances.slice(0, k);
}

/**
 * Find all vectors within a distance threshold
 *
 * @param {Float32Array} query - Query vector
 * @param {Float32Array[]} vectors - Candidate vectors
 * @param {number} threshold - Distance threshold
 * @param {'cosine' | 'euclidean' | 'manhattan'} [metric='cosine'] - Distance metric
 * @returns {Array<{index: number, distance: number}>} Neighbors within threshold
 *
 * @example
 * // Find all events with cosine similarity > 0.8 (distance < 0.2)
 * const similar = findWithinThreshold(queryCoords, allEventCoords, 0.2, 'cosine');
 */
export function findWithinThreshold(query, vectors, threshold, metric = 'cosine') {
  const distanceFn = getDistanceFunction(metric);

  return vectors
    .map((vec, index) => ({
      index,
      distance: distanceFn(query, vec),
    }))
    .filter(item => {
      // For cosine distance, lower is more similar
      // For cosine similarity (1 - distance), we want distance < threshold
      return metric === 'cosine'
        ? item.distance <= threshold
        : item.distance <= threshold;
    });
}

/**
 * Calculate pairwise distance matrix
 * Returns NxN matrix of distances between all vector pairs
 *
 * @param {Float32Array[]} vectors - Vectors to compare
 * @param {'cosine' | 'euclidean' | 'manhattan'} [metric='cosine'] - Distance metric
 * @returns {Float32Array} Flattened NxN distance matrix
 *
 * @example
 * const distMatrix = pairwiseDistances(eventCoords, 'euclidean');
 * const dist_i_j = distMatrix[i * eventCoords.length + j];
 */
export function pairwiseDistances(vectors, metric = 'cosine') {
  const n = vectors.length;
  const distances = new Float32Array(n * n);
  const distanceFn = getDistanceFunction(metric);

  for (let i = 0; i < n; i++) {
    // Distance to self is 0 (or 1 for cosine similarity)
    distances[i * n + i] = metric === 'cosine' ? 1.0 : 0.0;

    for (let j = i + 1; j < n; j++) {
      const dist = distanceFn(vectors[i], vectors[j]);
      distances[i * n + j] = dist;
      distances[j * n + i] = dist; // Symmetric
    }
  }

  return distances;
}

/**
 * Get distance function by name
 * @private
 */
function getDistanceFunction(metric) {
  switch (metric) {
    case 'cosine':
      return cosineDistance;
    case 'euclidean':
      return euclideanDistance;
    case 'manhattan':
      return manhattanDistance;
    default:
      throw new Error(`Unknown distance metric: ${metric}`);
  }
}

/**
 * Calculate centroid (mean) of a set of vectors
 *
 * @param {Float32Array[]} vectors - Vectors to average
 * @returns {Float32Array} Centroid vector
 */
export function calculateCentroid(vectors) {
  if (vectors.length === 0) {
    throw new Error('Cannot calculate centroid of empty vector set');
  }

  const dimension = vectors[0].length;
  const centroid = new Float32Array(dimension);

  for (const vec of vectors) {
    for (let i = 0; i < dimension; i++) {
      centroid[i] += vec[i];
    }
  }

  for (let i = 0; i < dimension; i++) {
    centroid[i] /= vectors.length;
  }

  return centroid;
}

/**
 * Normalize vector to unit length (L2 norm = 1)
 *
 * @param {Float32Array} vec - Vector to normalize
 * @returns {Float32Array} Normalized vector
 */
export function normalize(vec) {
  const normalized = new Float32Array(vec.length);
  let norm = 0;

  for (let i = 0; i < vec.length; i++) {
    norm += vec[i] * vec[i];
  }

  norm = Math.sqrt(norm);

  if (norm === 0) {
    return normalized; // Return zero vector
  }

  for (let i = 0; i < vec.length; i++) {
    normalized[i] = vec[i] / norm;
  }

  return normalized;
}

/**
 * Calculate dot product of two vectors
 *
 * @param {Float32Array} a - First vector
 * @param {Float32Array} b - Second vector
 * @returns {number} Dot product
 */
export function dotProduct(a, b) {
  if (a.length !== b.length) {
    throw new Error(`Vector dimension mismatch: ${a.length} vs ${b.length}`);
  }

  let sum = 0;
  for (let i = 0; i < a.length; i++) {
    sum += a[i] * b[i];
  }

  return sum;
}
