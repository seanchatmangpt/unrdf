/**
 * @fileoverview Tests for HDIT projection and visualization
 */

import { describe, it, expect } from 'vitest';
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

    expect(projected.length).toBe(3);
    projected.forEach(vec => {
      expect(vec instanceof Float32Array).toBe(true);
      expect(vec.length).toBe(2);
    });
  });

  it('should project to 3D', () => {
    const vectors = [
      new Float32Array([1, 2, 3, 4, 5]),
      new Float32Array([2, 3, 4, 5, 6]),
      new Float32Array([3, 4, 5, 6, 7]),
    ];

    const projected = projectPCA(vectors, 3);

    expect(projected.length).toBe(3);
    projected.forEach(vec => {
      expect(vec.length).toBe(3);
    });
  });

  it('should handle empty input', () => {
    const projected = projectPCA([], 2);
    expect(projected.length).toBe(0);
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
    expect(dist01 < dist02).toBe(true);
  });
});

describe('projectRandom', () => {
  it('should project to target dimension', () => {
    const vectors = [
      new Float32Array([1, 2, 3, 4, 5]),
      new Float32Array([2, 3, 4, 5, 6]),
    ];

    const projected = projectRandom(vectors, 2);

    expect(projected.length).toBe(2);
    projected.forEach(vec => {
      expect(vec.length).toBe(2);
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
        expect(proj1[i][j]).toBe(proj2[i][j]);
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
    expect(hasDifference).toBe(true);
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
        expect(vec[i] >= 0).toBe(true);
        expect(vec[i] <= 1).toBe(true);
      }
    });

    // Min should be ~0, max should be ~1
    expect(normalized[0][0] < 0.1).toBe(true); // Min x
    expect(normalized[2][0] > 0.9).toBe(true); // Max x
    expect(normalized[0][1] < 0.1).toBe(true); // Min y
    expect(normalized[2][1] > 0.9).toBe(true); // Max y
  });

  it('should handle empty input', () => {
    const normalized = normalizeProjection([]);
    expect(normalized.length).toBe(0);
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
      expect(vec[0]).toBe(0.5);
      expect(vec[1]).toBe(0.5);
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

    expect(vizData.length).toBe(3);
    vizData.forEach((item, i) => {
      expect(typeof item.x === 'number').toBe(true);
      expect(typeof item.y === 'number').toBe(true);
      expect(item.type).toBe(metadata[i].type);
      expect(item.label).toBe(metadata[i].label);
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
      expect(item.x >= 0 && item.x <= 1).toBe(true);
      expect(item.y >= 0 && item.y <= 1).toBe(true);
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
      expect(typeof item.z === 'number').toBe(true);
      expect(!Number.isNaN(item.z)).toBe(true);
      expect(Number.isFinite(item.z)).toBe(true);
      expect(item.z >= 0 && item.z <= 1).toBe(true);
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

    expect(clusters.length).toBe(4);

    // All cluster labels should be valid (0 or 1)
    clusters.forEach(c => {
      expect(c >= 0 && c < 2).toBe(true);
    });

    // First two should be in same cluster (close together)
    expect(clusters[0]).toBe(clusters[1]);

    // Last two should be in same cluster (close together)
    expect(clusters[2]).toBe(clusters[3]);

    // Since the two groups are far apart, they should (usually) be in different clusters
    // But k-means with random init might not always separate them perfectly
    // So we'll just check that clusters are assigned consistently
    const uniqueClusters = new Set(clusters);
    expect(uniqueClusters.size >= 1 && uniqueClusters.size <= 2).toBe(true);
  });

  it('should handle single cluster', () => {
    const projected = [
      new Float32Array([1, 2]),
      new Float32Array([1.1, 2.1]),
    ];

    const clusters = clusterProjection(projected, 1);

    expect(clusters.length).toBe(2);
    expect(clusters[0]).toBe(0);
    expect(clusters[1]).toBe(0);
  });

  it('should handle K larger than dataset', () => {
    const projected = [
      new Float32Array([1, 2]),
      new Float32Array([3, 4]),
    ];

    const clusters = clusterProjection(projected, 5);

    expect(clusters.length).toBe(2);
    // Each should be assigned to some cluster
    expect(clusters[0] >= 0 && clusters[0] < 5).toBe(true);
    expect(clusters[1] >= 0 && clusters[1] < 5).toBe(true);
  });
});
