/**
 * @fileoverview Dimensionality reduction and visualization helpers
 *
 * Project high-dimensional event coordinates to 2D/3D for browser visualization.
 * Enables "stadium mode" - seeing the event universe as a geometric object.
 *
 * @module @unrdf/kgc-4d/hdit/projection
 */

/**
 * Simple PCA (Principal Component Analysis) projection to 2D/3D
 *
 * @param {Float32Array[]} vectors - High-dimensional vectors
 * @param {number} [targetDim=2] - Target dimension (2 or 3)
 * @returns {Float32Array[]} Projected vectors
 *
 * @example
 * const projected2D = projectPCA(eventCoords, 2);
 * // Render as scatter plot: (projected2D[i][0], projected2D[i][1])
 */
export function projectPCA(vectors, targetDim = 2) {
  if (vectors.length === 0) {
    return [];
  }

  const n = vectors.length;
  const d = vectors[0].length;

  // Center the data
  const mean = calculateMean(vectors);
  const centered = vectors.map(vec => subtract(vec, mean));

  // Compute covariance matrix (d × d)
  const cov = computeCovarianceMatrix(centered);

  // Find top k eigenvectors (simplified power iteration for top components)
  const eigenvectors = findTopEigenvectors(cov, targetDim);

  // Project data onto principal components
  return centered.map(vec => project(vec, eigenvectors));
}

/**
 * Calculate mean vector
 * @private
 */
function calculateMean(vectors) {
  const d = vectors[0].length;
  const mean = new Float32Array(d);

  for (const vec of vectors) {
    for (let i = 0; i < d; i++) {
      mean[i] += vec[i];
    }
  }

  for (let i = 0; i < d; i++) {
    mean[i] /= vectors.length;
  }

  return mean;
}

/**
 * Subtract two vectors
 * @private
 */
function subtract(a, b) {
  const result = new Float32Array(a.length);
  for (let i = 0; i < a.length; i++) {
    result[i] = a[i] - b[i];
  }
  return result;
}

/**
 * Compute covariance matrix
 * @private
 */
function computeCovarianceMatrix(vectors) {
  const d = vectors[0].length;
  const n = vectors.length;
  const cov = Array(d).fill(null).map(() => new Float32Array(d));

  for (let i = 0; i < d; i++) {
    for (let j = i; j < d; j++) {
      let sum = 0;
      for (const vec of vectors) {
        sum += vec[i] * vec[j];
      }
      cov[i][j] = sum / n;
      cov[j][i] = cov[i][j]; // Symmetric
    }
  }

  return cov;
}

/**
 * Find top k eigenvectors using power iteration
 * Simplified implementation for visualization purposes
 * @private
 */
function findTopEigenvectors(cov, k) {
  const d = cov.length;
  const eigenvectors = [];

  for (let comp = 0; comp < k; comp++) {
    // Initialize random vector
    let vec = new Float32Array(d);
    for (let i = 0; i < d; i++) {
      vec[i] = Math.random() - 0.5;
    }

    // Power iteration (simplified, 20 iterations)
    for (let iter = 0; iter < 20; iter++) {
      // Multiply by covariance matrix
      const newVec = new Float32Array(d);
      for (let i = 0; i < d; i++) {
        let sum = 0;
        for (let j = 0; j < d; j++) {
          sum += cov[i][j] * vec[j];
        }
        newVec[i] = sum;
      }

      // Normalize
      let norm = 0;
      for (let i = 0; i < d; i++) {
        norm += newVec[i] * newVec[i];
      }
      norm = Math.sqrt(norm);

      for (let i = 0; i < d; i++) {
        vec[i] = newVec[i] / norm;
      }
    }

    eigenvectors.push(vec);

    // Deflate (remove component from covariance matrix)
    for (let i = 0; i < d; i++) {
      for (let j = 0; j < d; j++) {
        cov[i][j] -= vec[i] * vec[j];
      }
    }
  }

  return eigenvectors;
}

/**
 * Project vector onto basis vectors
 * @private
 */
function project(vec, basis) {
  const projected = new Float32Array(basis.length);

  for (let i = 0; i < basis.length; i++) {
    let dot = 0;
    for (let j = 0; j < vec.length; j++) {
      dot += vec[j] * basis[i][j];
    }
    projected[i] = dot;
  }

  return projected;
}

/**
 * Simple t-SNE-like projection (random projection with local structure preservation)
 * Much faster than true t-SNE, good enough for visualization
 *
 * @param {Float32Array[]} vectors - High-dimensional vectors
 * @param {number} [targetDim=2] - Target dimension (2 or 3)
 * @param {number} [seed=42] - Random seed
 * @returns {Float32Array[]} Projected vectors
 */
export function projectRandom(vectors, targetDim = 2, seed = 42) {
  if (vectors.length === 0) {
    return [];
  }

  const d = vectors[0].length;

  // Generate random projection matrix
  const projMatrix = generateRandomMatrix(d, targetDim, seed);

  // Project each vector
  return vectors.map(vec => {
    const projected = new Float32Array(targetDim);
    for (let i = 0; i < targetDim; i++) {
      let sum = 0;
      for (let j = 0; j < d; j++) {
        sum += vec[j] * projMatrix[j][i];
      }
      projected[i] = sum;
    }
    return projected;
  });
}

/**
 * Generate random projection matrix
 * @private
 */
function generateRandomMatrix(rows, cols, seed) {
  // Simple LCG for reproducible random numbers
  let state = seed;
  const random = () => {
    state = (state * 1664525 + 1013904223) >>> 0;
    return (state / 0xFFFFFFFF) - 0.5;
  };

  const matrix = [];
  for (let i = 0; i < rows; i++) {
    const row = new Float32Array(cols);
    for (let j = 0; j < cols; j++) {
      row[j] = random();
    }
    matrix.push(row);
  }

  return matrix;
}

/**
 * Normalize projected coordinates to [0, 1] for rendering
 *
 * @param {Float32Array[]} projected - Projected vectors
 * @returns {Float32Array[]} Normalized vectors
 */
export function normalizeProjection(projected) {
  if (projected.length === 0) {
    return [];
  }

  const dim = projected[0].length;

  // Find min/max for each dimension
  const mins = new Float32Array(dim).fill(Infinity);
  const maxs = new Float32Array(dim).fill(-Infinity);

  for (const vec of projected) {
    for (let i = 0; i < dim; i++) {
      mins[i] = Math.min(mins[i], vec[i]);
      maxs[i] = Math.max(maxs[i], vec[i]);
    }
  }

  // Normalize
  return projected.map(vec => {
    const normalized = new Float32Array(dim);
    for (let i = 0; i < dim; i++) {
      const range = maxs[i] - mins[i];
      normalized[i] = range === 0 ? 0.5 : (vec[i] - mins[i]) / range;
    }
    return normalized;
  });
}

/**
 * Create visualization-ready data for browser rendering
 *
 * @param {Float32Array[]} vectors - High-dimensional vectors
 * @param {Array<Object>} metadata - Metadata for each vector (labels, colors, etc.)
 * @param {'pca' | 'random'} [method='pca'] - Projection method
 * @param {number} [targetDim=2] - Target dimension
 * @returns {Array<Object>} Visualization data
 *
 * @example
 * const vizData = createVisualizationData(eventCoords, events.map(e => ({
 *   label: e.type,
 *   timestamp: e.timestamp,
 * })));
 *
 * // Render with D3, Three.js, or Canvas
 * vizData.forEach(({ x, y, label }) => {
 *   drawPoint(x * width, y * height, label);
 * });
 */
export function createVisualizationData(vectors, metadata, method = 'pca', targetDim = 2) {
  // Project to 2D/3D
  const projected = method === 'pca'
    ? projectPCA(vectors, targetDim)
    : projectRandom(vectors, targetDim);

  // Normalize to [0, 1]
  const normalized = normalizeProjection(projected);

  // Combine with metadata
  return normalized.map((coords, i) => ({
    x: coords[0],
    y: coords[1],
    z: targetDim === 3 ? coords[2] : undefined,
    ...metadata[i],
  }));
}

/**
 * Create interactive clusters for visualization
 * Simple k-means clustering in 2D/3D space
 *
 * @param {Float32Array[]} projected - Projected 2D/3D vectors
 * @param {number} k - Number of clusters
 * @param {number} [maxIter=10] - Maximum iterations
 * @returns {number[]} Cluster assignments
 */
export function clusterProjection(projected, k, maxIter = 10) {
  if (projected.length === 0) {
    return [];
  }

  const n = projected.length;
  const dim = projected[0].length;

  // Initialize centroids randomly
  const centroids = [];
  for (let i = 0; i < k; i++) {
    centroids.push(projected[Math.floor(Math.random() * n)]);
  }

  let assignments = new Array(n).fill(0);

  for (let iter = 0; iter < maxIter; iter++) {
    // Assign points to nearest centroid
    for (let i = 0; i < n; i++) {
      let minDist = Infinity;
      let bestCluster = 0;

      for (let c = 0; c < k; c++) {
        let dist = 0;
        for (let d = 0; d < dim; d++) {
          const diff = projected[i][d] - centroids[c][d];
          dist += diff * diff;
        }

        if (dist < minDist) {
          minDist = dist;
          bestCluster = c;
        }
      }

      assignments[i] = bestCluster;
    }

    // Update centroids
    const counts = new Array(k).fill(0);
    const newCentroids = Array(k).fill(null).map(() => new Float32Array(dim));

    for (let i = 0; i < n; i++) {
      const cluster = assignments[i];
      counts[cluster]++;
      for (let d = 0; d < dim; d++) {
        newCentroids[cluster][d] += projected[i][d];
      }
    }

    for (let c = 0; c < k; c++) {
      if (counts[c] > 0) {
        for (let d = 0; d < dim; d++) {
          centroids[c][d] = newCentroids[c][d] / counts[c];
        }
      }
    }
  }

  return assignments;
}
