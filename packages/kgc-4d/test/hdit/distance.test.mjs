/**
 * @fileoverview Tests for HDIT distance and similarity functions
 */

import { describe, it } from 'node:test';
import assert from 'node:assert/strict';
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
    assert.ok(Math.abs(sim - 1.0) < 0.0001);
  });

  it('should return 0 for orthogonal vectors', () => {
    const a = new Float32Array([1, 0, 0]);
    const b = new Float32Array([0, 1, 0]);

    const sim = cosineSimilarity(a, b);
    assert.ok(Math.abs(sim) < 0.0001);
  });

  it('should return -1 for opposite vectors', () => {
    const a = new Float32Array([1, 2, 3]);
    const b = new Float32Array([-1, -2, -3]);

    const sim = cosineSimilarity(a, b);
    assert.ok(Math.abs(sim - (-1.0)) < 0.0001);
  });

  it('should be scale-invariant', () => {
    const a = new Float32Array([1, 2, 3]);
    const b = new Float32Array([2, 4, 6]); // 2x scaled

    const sim = cosineSimilarity(a, b);
    assert.ok(Math.abs(sim - 1.0) < 0.0001);
  });

  it('should throw on dimension mismatch', () => {
    const a = new Float32Array([1, 2, 3]);
    const b = new Float32Array([1, 2]);

    assert.throws(() => cosineSimilarity(a, b), /dimension mismatch/i);
  });
});

describe('cosineDistance', () => {
  it('should return 0 for identical vectors', () => {
    const a = new Float32Array([1, 2, 3]);
    const b = new Float32Array([1, 2, 3]);

    const dist = cosineDistance(a, b);
    assert.ok(Math.abs(dist) < 0.0001);
  });

  it('should return 1 for orthogonal vectors', () => {
    const a = new Float32Array([1, 0, 0]);
    const b = new Float32Array([0, 1, 0]);

    const dist = cosineDistance(a, b);
    assert.ok(Math.abs(dist - 1.0) < 0.0001);
  });
});

describe('euclideanDistance', () => {
  it('should return 0 for identical vectors', () => {
    const a = new Float32Array([1, 2, 3]);
    const b = new Float32Array([1, 2, 3]);

    const dist = euclideanDistance(a, b);
    assert.equal(dist, 0);
  });

  it('should calculate correct distance', () => {
    const a = new Float32Array([0, 0, 0]);
    const b = new Float32Array([3, 4, 0]);

    const dist = euclideanDistance(a, b);
    assert.equal(dist, 5); // 3-4-5 triangle
  });

  it('should be symmetric', () => {
    const a = new Float32Array([1, 2, 3]);
    const b = new Float32Array([4, 5, 6]);

    const distAB = euclideanDistance(a, b);
    const distBA = euclideanDistance(b, a);

    assert.equal(distAB, distBA);
  });
});

describe('euclideanDistanceSquared', () => {
  it('should return squared distance', () => {
    const a = new Float32Array([0, 0, 0]);
    const b = new Float32Array([3, 4, 0]);

    const distSq = euclideanDistanceSquared(a, b);
    assert.equal(distSq, 25); // 5^2
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
    assert.ok(distances[0] < distances[1]);
    assert.ok(distances[1] < distances[2]);
  });
});

describe('manhattanDistance', () => {
  it('should calculate L1 distance', () => {
    const a = new Float32Array([0, 0, 0]);
    const b = new Float32Array([1, 2, 3]);

    const dist = manhattanDistance(a, b);
    assert.equal(dist, 6); // |1| + |2| + |3|
  });

  it('should handle negative differences', () => {
    const a = new Float32Array([5, 5, 5]);
    const b = new Float32Array([2, 3, 1]);

    const dist = manhattanDistance(a, b);
    assert.equal(dist, 3 + 2 + 4); // 9
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

    assert.equal(neighbors.length, 2);
    assert.equal(neighbors[0].index, 0); // Most similar
    assert.equal(neighbors[1].index, 1); // Second most
  });

  it('should find K nearest by euclidean distance', () => {
    const query = new Float32Array([0, 0]);
    const vectors = [
      new Float32Array([1, 1]),
      new Float32Array([10, 10]),
      new Float32Array([0.5, 0.5]),
    ];

    const neighbors = findKNearest(query, vectors, 2, 'euclidean');

    assert.equal(neighbors.length, 2);
    assert.equal(neighbors[0].index, 2); // Closest
    assert.equal(neighbors[1].index, 0); // Second closest
  });

  it('should handle K larger than dataset', () => {
    const query = new Float32Array([1, 2, 3]);
    const vectors = [
      new Float32Array([1, 2, 3]),
      new Float32Array([2, 3, 4]),
    ];

    const neighbors = findKNearest(query, vectors, 10, 'cosine');

    assert.equal(neighbors.length, 2); // Only 2 available
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

    assert.equal(within.length, 3); // First three within 1.5
    assert.ok(within.every(item => item.distance <= 1.5));
  });

  it('should return empty for no matches', () => {
    const query = new Float32Array([0, 0]);
    const vectors = [
      new Float32Array([10, 10]),
      new Float32Array([20, 20]),
    ];

    const within = findWithinThreshold(query, vectors, 1.0, 'euclidean');

    assert.equal(within.length, 0);
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

    assert.equal(distMatrix.length, 9); // 3x3

    // Diagonal should be 0
    assert.equal(distMatrix[0 * 3 + 0], 0);
    assert.equal(distMatrix[1 * 3 + 1], 0);
    assert.equal(distMatrix[2 * 3 + 2], 0);

    // Should be symmetric
    assert.equal(distMatrix[0 * 3 + 1], distMatrix[1 * 3 + 0]);
    assert.equal(distMatrix[0 * 3 + 2], distMatrix[2 * 3 + 0]);
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

    assert.equal(centroid[0], 3);
    assert.equal(centroid[1], 4);
    assert.equal(centroid[2], 5);
  });

  it('should throw on empty vector set', () => {
    assert.throws(() => calculateCentroid([]), /empty vector set/i);
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

    assert.ok(Math.abs(length - 1.0) < 0.0001);
  });

  it('should handle zero vector', () => {
    const vec = new Float32Array([0, 0, 0]);
    const normalized = normalize(vec);

    assert.equal(normalized[0], 0);
    assert.equal(normalized[1], 0);
    assert.equal(normalized[2], 0);
  });

  it('should preserve direction', () => {
    const vec = new Float32Array([1, 2, 3]);
    const normalized = normalize(vec);

    // Check proportions are maintained
    const ratio1 = normalized[1] / normalized[0];
    const ratio2 = vec[1] / vec[0];
    assert.ok(Math.abs(ratio1 - ratio2) < 0.0001);
  });
});

describe('dotProduct', () => {
  it('should calculate dot product', () => {
    const a = new Float32Array([1, 2, 3]);
    const b = new Float32Array([4, 5, 6]);

    const dot = dotProduct(a, b);
    assert.equal(dot, 1*4 + 2*5 + 3*6); // 32
  });

  it('should return 0 for orthogonal vectors', () => {
    const a = new Float32Array([1, 0, 0]);
    const b = new Float32Array([0, 1, 0]);

    const dot = dotProduct(a, b);
    assert.equal(dot, 0);
  });
});
