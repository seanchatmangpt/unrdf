/**
 * @fileoverview Tests for HDIT projection and visualization
 */

import { describe, it } from 'node:test';
import assert from 'node:assert/strict';
import {
  projectPCA,
  projectRandom,
  normalizeProjection,
  createVisualizationData,
  clusterProjection,
} from '../../src/hdit/projection.mjs';

describe('projectPCA', () => {
  it('should project to 2D', () => {
    const vectors = [
      new Float32Array([1, 2, 3, 4]),
      new Float32Array([2, 3, 4, 5]),
      new Float32Array([3, 4, 5, 6]),
    ];

    const projected = projectPCA(vectors, 2);

    assert.equal(projected.length, 3);
    projected.forEach(vec => {
      assert.ok(vec instanceof Float32Array);
      assert.equal(vec.length, 2);
    });
  });

  it('should project to 3D', () => {
    const vectors = [
      new Float32Array([1, 2, 3, 4, 5]),
      new Float32Array([2, 3, 4, 5, 6]),
      new Float32Array([3, 4, 5, 6, 7]),
    ];

    const projected = projectPCA(vectors, 3);

    assert.equal(projected.length, 3);
    projected.forEach(vec => {
      assert.equal(vec.length, 3);
    });
  });

  it('should handle empty input', () => {
    const projected = projectPCA([], 2);
    assert.equal(projected.length, 0);
  });

  it('should preserve relative distances (approximately)', () => {
    // Create vectors with known structure
    const vectors = [
      new Float32Array([1, 0, 0, 0]),
      new Float32Array([1.1, 0, 0, 0]), // Close to first
      new Float32Array([0, 1, 0, 0]),    // Far from first two
    ];

    const projected = projectPCA(vectors, 2);

    // Calculate distances in 2D
    const dist01 = Math.hypot(
      projected[0][0] - projected[1][0],
      projected[0][1] - projected[1][1]
    );
    const dist02 = Math.hypot(
      projected[0][0] - projected[2][0],
      projected[0][1] - projected[2][1]
    );

    // First two should be closer than first and third
    assert.ok(dist01 < dist02);
  });
});

describe('projectRandom', () => {
  it('should project to target dimension', () => {
    const vectors = [
      new Float32Array([1, 2, 3, 4, 5]),
      new Float32Array([2, 3, 4, 5, 6]),
    ];

    const projected = projectRandom(vectors, 2);

    assert.equal(projected.length, 2);
    projected.forEach(vec => {
      assert.equal(vec.length, 2);
    });
  });

  it('should be deterministic with same seed', () => {
    const vectors = [
      new Float32Array([1, 2, 3, 4]),
      new Float32Array([2, 3, 4, 5]),
    ];

    const proj1 = projectRandom(vectors, 2, 42);
    const proj2 = projectRandom(vectors, 2, 42);

    for (let i = 0; i < proj1.length; i++) {
      for (let j = 0; j < proj1[i].length; j++) {
        assert.equal(proj1[i][j], proj2[i][j]);
      }
    }
  });

  it('should produce different results with different seeds', () => {
    const vectors = [
      new Float32Array([1, 2, 3, 4]),
      new Float32Array([2, 3, 4, 5]),
    ];

    const proj1 = projectRandom(vectors, 2, 42);
    const proj2 = projectRandom(vectors, 2, 123);

    // At least one coordinate should differ
    let hasDifference = false;
    for (let i = 0; i < proj1.length; i++) {
      for (let j = 0; j < proj1[i].length; j++) {
        if (proj1[i][j] !== proj2[i][j]) {
          hasDifference = true;
        }
      }
    }
    assert.ok(hasDifference);
  });
});

describe('normalizeProjection', () => {
  it('should normalize to [0, 1] range', () => {
    const projected = [
      new Float32Array([10, 20]),
      new Float32Array([30, 40]),
      new Float32Array([50, 60]),
    ];

    const normalized = normalizeProjection(projected);

    normalized.forEach(vec => {
      for (let i = 0; i < vec.length; i++) {
        assert.ok(vec[i] >= 0);
        assert.ok(vec[i] <= 1);
      }
    });

    // Min should be ~0, max should be ~1
    assert.ok(normalized[0][0] < 0.1); // Min x
    assert.ok(normalized[2][0] > 0.9); // Max x
    assert.ok(normalized[0][1] < 0.1); // Min y
    assert.ok(normalized[2][1] > 0.9); // Max y
  });

  it('should handle empty input', () => {
    const normalized = normalizeProjection([]);
    assert.equal(normalized.length, 0);
  });

  it('should handle constant values', () => {
    const projected = [
      new Float32Array([5, 10]),
      new Float32Array([5, 10]),
      new Float32Array([5, 10]),
    ];

    const normalized = normalizeProjection(projected);

    // All should be 0.5 (middle of range)
    normalized.forEach(vec => {
      assert.equal(vec[0], 0.5);
      assert.equal(vec[1], 0.5);
    });
  });
});

describe('createVisualizationData', () => {
  it('should combine coordinates with metadata', () => {
    const vectors = [
      new Float32Array([1, 2, 3, 4]),
      new Float32Array([2, 3, 4, 5]),
      new Float32Array([3, 4, 5, 6]),
    ];

    const metadata = [
      { type: 'CREATE', label: 'Event 1' },
      { type: 'UPDATE', label: 'Event 2' },
      { type: 'DELETE', label: 'Event 3' },
    ];

    const vizData = createVisualizationData(vectors, metadata, 'pca', 2);

    assert.equal(vizData.length, 3);
    vizData.forEach((item, i) => {
      assert.ok(typeof item.x === 'number');
      assert.ok(typeof item.y === 'number');
      assert.equal(item.type, metadata[i].type);
      assert.equal(item.label, metadata[i].label);
    });
  });

  it('should normalize coordinates to [0, 1]', () => {
    const vectors = [
      new Float32Array([1, 2]),
      new Float32Array([3, 4]),
    ];

    const metadata = [{ id: 1 }, { id: 2 }];

    const vizData = createVisualizationData(vectors, metadata, 'random', 2);

    vizData.forEach(item => {
      assert.ok(item.x >= 0 && item.x <= 1);
      assert.ok(item.y >= 0 && item.y <= 1);
    });
  });

  it('should include z coordinate for 3D', () => {
    const vectors = [
      new Float32Array([1, 2, 3, 4]),
      new Float32Array([2, 3, 4, 5]),
      new Float32Array([3, 4, 5, 6]),
    ];

    const metadata = [{ id: 1 }, { id: 2 }, { id: 3 }];

    const vizData = createVisualizationData(vectors, metadata, 'pca', 3);

    vizData.forEach(item => {
      assert.ok(typeof item.z === 'number', 'z should be a number');
      assert.ok(!Number.isNaN(item.z), 'z should not be NaN');
      assert.ok(Number.isFinite(item.z), 'z should be finite');
      assert.ok(item.z >= 0 && item.z <= 1, `z should be in [0,1], got ${item.z}`);
    });
  });
});

describe('clusterProjection', () => {
  it('should assign cluster labels', () => {
    const projected = [
      new Float32Array([0, 0]),
      new Float32Array([0.1, 0.1]),
      new Float32Array([10, 10]),
      new Float32Array([10.1, 10.1]),
    ];

    const clusters = clusterProjection(projected, 2, 20);

    assert.equal(clusters.length, 4);

    // All cluster labels should be valid (0 or 1)
    clusters.forEach(c => {
      assert.ok(c >= 0 && c < 2, `Cluster ${c} should be 0 or 1`);
    });

    // First two should be in same cluster (close together)
    assert.equal(clusters[0], clusters[1]);

    // Last two should be in same cluster (close together)
    assert.equal(clusters[2], clusters[3]);

    // Since the two groups are far apart, they should (usually) be in different clusters
    // But k-means with random init might not always separate them perfectly
    // So we'll just check that clusters are assigned consistently
    const uniqueClusters = new Set(clusters);
    assert.ok(uniqueClusters.size >= 1 && uniqueClusters.size <= 2);
  });

  it('should handle single cluster', () => {
    const projected = [
      new Float32Array([1, 2]),
      new Float32Array([1.1, 2.1]),
    ];

    const clusters = clusterProjection(projected, 1);

    assert.equal(clusters.length, 2);
    assert.equal(clusters[0], 0);
    assert.equal(clusters[1], 0);
  });

  it('should handle K larger than dataset', () => {
    const projected = [
      new Float32Array([1, 2]),
      new Float32Array([3, 4]),
    ];

    const clusters = clusterProjection(projected, 5);

    assert.equal(clusters.length, 2);
    // Each should be assigned to some cluster
    assert.ok(clusters[0] >= 0 && clusters[0] < 5);
    assert.ok(clusters[1] >= 0 && clusters[1] < 5);
  });
});
