/**
 * @fileoverview Tests for HDIT distance and similarity functions
 */

import { describe, it, expect } from 'vitest';
import {
  cosineSimilarity,
  cosineDistance,
  euclideanDistance,
  euclideanDistanceSquared,
  manhattanDistance,
  findKNearest,
  findWithinThreshold,
  pairwiseDistances,
  calculateCentroid,
  normalize,
  dotProduct,
} from '../../src/hdit/distance.mjs';

describe('cosineSimilarity', () => {
  it('should return 1 for identical vectors', () => {
    const a = new Float32Array([1, 2, 3, 4]);
    const b = new Float32Array([1, 2, 3, 4]);

    const sim = cosineSimilarity(a, b);
    expect(Math.abs(sim - 1.0) < 0.0001).toBe(true);
  });

  it('should return 0 for orthogonal vectors', () => {
    const a = new Float32Array([1, 0, 0]);
    const b = new Float32Array([0, 1, 0]);

    const sim = cosineSimilarity(a, b);
    expect(Math.abs(sim) < 0.0001).toBe(true);
  });

  it('should return -1 for opposite vectors', () => {
    const a = new Float32Array([1, 2, 3]);
    const b = new Float32Array([-1, -2, -3]);

    const sim = cosineSimilarity(a, b);
    expect(Math.abs(sim - (-1.0)) < 0.0001).toBe(true);
  });

  it('should be scale-invariant', () => {
    const a = new Float32Array([1, 2, 3]);
    const b = new Float32Array([2, 4, 6]); // 2x scaled

    const sim = cosineSimilarity(a, b);
    expect(Math.abs(sim - 1.0) < 0.0001).toBe(true);
  });

  it('should throw on dimension mismatch', () => {
    const a = new Float32Array([1, 2, 3]);
    const b = new Float32Array([1, 2]);

    expect(() => cosineSimilarity(a, b)).toThrow(/dimension mismatch/i);
  });
});

describe('cosineDistance', () => {
  it('should return 0 for identical vectors', () => {
    const a = new Float32Array([1, 2, 3]);
    const b = new Float32Array([1, 2, 3]);

    const dist = cosineDistance(a, b);
    expect(Math.abs(dist) < 0.0001).toBe(true);
  });

  it('should return 1 for orthogonal vectors', () => {
    const a = new Float32Array([1, 0, 0]);
    const b = new Float32Array([0, 1, 0]);

    const dist = cosineDistance(a, b);
    expect(Math.abs(dist - 1.0) < 0.0001).toBe(true);
  });
});

describe('euclideanDistance', () => {
  it('should return 0 for identical vectors', () => {
    const a = new Float32Array([1, 2, 3]);
    const b = new Float32Array([1, 2, 3]);

    const dist = euclideanDistance(a, b);
    expect(dist).toBe(0);
  });

  it('should calculate correct distance', () => {
    const a = new Float32Array([0, 0, 0]);
    const b = new Float32Array([3, 4, 0]);

    const dist = euclideanDistance(a, b);
    expect(dist).toBe(5); // 3-4-5 triangle
  });

  it('should be symmetric', () => {
    const a = new Float32Array([1, 2, 3]);
    const b = new Float32Array([4, 5, 6]);

    const distAB = euclideanDistance(a, b);
    const distBA = euclideanDistance(b, a);

    expect(distAB).toBe(distBA);
  });
});

describe('euclideanDistanceSquared', () => {
  it('should return squared distance', () => {
    const a = new Float32Array([0, 0, 0]);
    const b = new Float32Array([3, 4, 0]);

    const distSq = euclideanDistanceSquared(a, b);
    expect(distSq).toBe(25); // 5^2
  });

  it('should be faster than sqrt version for ranking', () => {
    // This is more of a usage test - squared is faster when comparing
    const a = new Float32Array([1, 2, 3]);
    const candidates = [
      new Float32Array([1, 2, 3]),
      new Float32Array([2, 3, 4]),
      new Float32Array([10, 20, 30]),
    ];

    const distances = candidates.map(c => euclideanDistanceSquared(a, c));
    expect(distances[0] < distances[1]).toBe(true);
    expect(distances[1] < distances[2]).toBe(true);
  });
});

describe('manhattanDistance', () => {
  it('should calculate L1 distance', () => {
    const a = new Float32Array([0, 0, 0]);
    const b = new Float32Array([1, 2, 3]);

    const dist = manhattanDistance(a, b);
    expect(dist).toBe(6); // |1| + |2| + |3|
  });

  it('should handle negative differences', () => {
    const a = new Float32Array([5, 5, 5]);
    const b = new Float32Array([2, 3, 1]);

    const dist = manhattanDistance(a, b);
    expect(dist).toBe(3 + 2 + 4); // 9
  });
});

describe('findKNearest', () => {
  it('should find K nearest neighbors by cosine similarity', () => {
    const query = new Float32Array([1, 0, 0]);
    const vectors = [
      new Float32Array([1, 0, 0]), // Identical
      new Float32Array([0.9, 0.1, 0]), // Very close
      new Float32Array([0, 1, 0]), // Orthogonal
      new Float32Array([-1, 0, 0]), // Opposite
    ];

    const neighbors = findKNearest(query, vectors, 2, 'cosine');

    expect(neighbors.length).toBe(2);
    expect(neighbors[0].index).toBe(0); // Most similar
    expect(neighbors[1].index).toBe(1); // Second most
  });

  it('should find K nearest by euclidean distance', () => {
    const query = new Float32Array([0, 0]);
    const vectors = [
      new Float32Array([1, 1]),
      new Float32Array([10, 10]),
      new Float32Array([0.5, 0.5]),
    ];

    const neighbors = findKNearest(query, vectors, 2, 'euclidean');

    expect(neighbors.length).toBe(2);
    expect(neighbors[0].index).toBe(2); // Closest
    expect(neighbors[1].index).toBe(0); // Second closest
  });

  it('should handle K larger than dataset', () => {
    const query = new Float32Array([1, 2, 3]);
    const vectors = [
      new Float32Array([1, 2, 3]),
      new Float32Array([2, 3, 4]),
    ];

    const neighbors = findKNearest(query, vectors, 10, 'cosine');

    expect(neighbors.length).toBe(2); // Only 2 available
  });
});

describe('findWithinThreshold', () => {
  it('should find all vectors within distance threshold', () => {
    const query = new Float32Array([0, 0]);
    const vectors = [
      new Float32Array([1, 0]),   // dist = 1
      new Float32Array([0, 1]),   // dist = 1
      new Float32Array([10, 0]),  // dist = 10
      new Float32Array([0.5, 0]), // dist = 0.5
    ];

    const within = findWithinThreshold(query, vectors, 1.5, 'euclidean');

    expect(within.length).toBe(3); // First three within 1.5
    expect(within.every(item => item.distance <= 1.5)).toBe(true);
  });

  it('should return empty for no matches', () => {
    const query = new Float32Array([0, 0]);
    const vectors = [
      new Float32Array([10, 10]),
      new Float32Array([20, 20]),
    ];

    const within = findWithinThreshold(query, vectors, 1.0, 'euclidean');

    expect(within.length).toBe(0);
  });
});

describe('pairwiseDistances', () => {
  it('should compute NxN distance matrix', () => {
    const vectors = [
      new Float32Array([0, 0]),
      new Float32Array([1, 0]),
      new Float32Array([0, 1]),
    ];

    const distMatrix = pairwiseDistances(vectors, 'euclidean');

    expect(distMatrix.length).toBe(9); // 3x3

    // Diagonal should be 0
    expect(distMatrix[0 * 3 + 0]).toBe(0);
    expect(distMatrix[1 * 3 + 1]).toBe(0);
    expect(distMatrix[2 * 3 + 2]).toBe(0);

    // Should be symmetric
    expect(distMatrix[0 * 3 + 1]).toBe(distMatrix[1 * 3 + 0]);
    expect(distMatrix[0 * 3 + 2]).toBe(distMatrix[2 * 3 + 0]);
  });
});

describe('calculateCentroid', () => {
  it('should calculate mean vector', () => {
    const vectors = [
      new Float32Array([1, 2, 3]),
      new Float32Array([3, 4, 5]),
      new Float32Array([5, 6, 7]),
    ];

    const centroid = calculateCentroid(vectors);

    expect(centroid[0]).toBe(3);
    expect(centroid[1]).toBe(4);
    expect(centroid[2]).toBe(5);
  });

  it('should throw on empty vector set', () => {
    expect(() => calculateCentroid([])).toThrow(/empty vector set/i);
  });
});

describe('normalize', () => {
  it('should normalize to unit length', () => {
    const vec = new Float32Array([3, 4, 0]);
    const normalized = normalize(vec);

    // Length should be 1
    let length = 0;
    for (let i = 0; i < normalized.length; i++) {
      length += normalized[i] * normalized[i];
    }
    length = Math.sqrt(length);

    expect(Math.abs(length - 1.0) < 0.0001).toBe(true);
  });

  it('should handle zero vector', () => {
    const vec = new Float32Array([0, 0, 0]);
    const normalized = normalize(vec);

    expect(normalized[0]).toBe(0);
    expect(normalized[1]).toBe(0);
    expect(normalized[2]).toBe(0);
  });

  it('should preserve direction', () => {
    const vec = new Float32Array([1, 2, 3]);
    const normalized = normalize(vec);

    // Check proportions are maintained
    const ratio1 = normalized[1] / normalized[0];
    const ratio2 = vec[1] / vec[0];
    expect(Math.abs(ratio1 - ratio2) < 0.0001).toBe(true);
  });
});

describe('dotProduct', () => {
  it('should calculate dot product', () => {
    const a = new Float32Array([1, 2, 3]);
    const b = new Float32Array([4, 5, 6]);

    const dot = dotProduct(a, b);
    expect(dot).toBe(1*4 + 2*5 + 3*6); // 32
  });

  it('should return 0 for orthogonal vectors', () => {
    const a = new Float32Array([1, 0, 0]);
    const b = new Float32Array([0, 1, 0]);

    const dot = dotProduct(a, b);
    expect(dot).toBe(0);
  });
});
