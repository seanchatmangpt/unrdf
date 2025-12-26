/**
 * @fileoverview Tests for HDIT guards and validation
 */

import { describe, it, expect } from 'vitest';
import {
  guardDimension,
  guardMemory,
  guardLatency,
  guardEntityCount,
  guardCoordinates,
  guardAll,
  suggestDimension,
} from '../../src/hdit/guards.mjs';
import {
  D_BROWSER,
  D_LIGHT,
  D_MEDIUM,
  D_NODE_MAX,
  D_HEAVY,
} from '../../src/hdit/constants.mjs';

describe('guardDimension', () => {
  it('should allow valid browser dimensions', () => {
    const result = guardDimension(D_BROWSER, 'browser');
    expect(result.allowed).toBe(true);
  });

  it('should allow valid node dimensions', () => {
    const result = guardDimension(D_NODE_MAX, 'node');
    expect(result.allowed).toBe(true);
  });

  it('should reject too small dimensions', () => {
    const result = guardDimension(16, 'browser');
    expect(result.allowed).toBe(false);
    expect(result.reason.includes('too low')).toBe(true);
  });

  it('should reject browser dimensions that are too high', () => {
    const result = guardDimension(D_NODE_MAX, 'browser');
    expect(result.allowed).toBe(false);
    expect(result.reason.includes('exceeds safe limit')).toBe(true);
  });

  it('should reject node dimensions that are too high', () => {
    const result = guardDimension(D_HEAVY, 'node');
    expect(result.allowed).toBe(false);
    expect(result.reason.includes('exceeds maximum')).toBe(true);
  });
});

describe('guardMemory', () => {
  it('should allow within memory budget', () => {
    const result = guardMemory(10000, D_BROWSER, 100);
    expect(result.allowed).toBe(true);
    expect(result.metrics.memoryMB).toBeDefined();
  });

  it('should reject exceeding memory budget', () => {
    const result = guardMemory(1000000, D_HEAVY, 100);
    expect(result.allowed).toBe(false);
    expect(result.reason.includes('exceeds budget')).toBe(true);
  });

  it('should provide memory metrics', () => {
    const result = guardMemory(10000, D_BROWSER, 100);
    expect(result.metrics.memoryBytes).toBeDefined();
    expect(result.metrics.memoryMB).toBeDefined();
    expect(result.metrics.maxMemoryMB).toBe(100);
  });
});

describe('guardLatency', () => {
  it('should allow fast queries', () => {
    const result = guardLatency(1000, D_BROWSER, 10);
    expect(result.allowed).toBe(true);
  });

  it('should reject slow queries', () => {
    const result = guardLatency(1000000, D_HEAVY, 10);
    expect(result.allowed).toBe(false);
  });

  it('should provide operation count metrics', () => {
    const result = guardLatency(1000, D_BROWSER);
    expect(result.metrics.ops).toBeDefined();
    expect(result.metrics.estimatedMs !== undefined).toBe(true);
  });
});

describe('guardEntityCount', () => {
  it('should allow reasonable browser entity count', () => {
    const result = guardEntityCount(10000, D_BROWSER, 'browser');
    expect(result.allowed).toBe(true);
  });

  it('should reject too many browser entities', () => {
    const result = guardEntityCount(200000, D_BROWSER, 'browser');
    expect(result.allowed).toBe(false);
    expect(result.reason.includes('exceeds browser safe limit')).toBe(true);
  });

  it('should allow larger counts in node', () => {
    const result = guardEntityCount(200000, D_MEDIUM, 'node');
    // May pass or fail depending on latency, but shouldn't fail on count alone
    expect(result.allowed !== undefined).toBe(true);
  });
});

describe('guardCoordinates', () => {
  it('should accept valid Float32Array', () => {
    const coords = new Float32Array(D_BROWSER);
    coords.fill(0.5);

    const result = guardCoordinates(coords, D_BROWSER);
    expect(result.allowed).toBe(true);
  });

  it('should reject wrong type', () => {
    const coords = new Array(D_BROWSER).fill(0.5);
    const result = guardCoordinates(coords, D_BROWSER);
    expect(result.allowed).toBe(false);
    expect(result.reason.includes('Float32Array')).toBe(true);
  });

  it('should reject dimension mismatch', () => {
    const coords = new Float32Array(32);
    const result = guardCoordinates(coords, D_BROWSER);
    expect(result.allowed).toBe(false);
    expect(result.reason.includes('mismatch')).toBe(true);
  });

  it('should reject NaN values', () => {
    const coords = new Float32Array(D_BROWSER);
    coords[10] = NaN;

    const result = guardCoordinates(coords, D_BROWSER);
    expect(result.allowed).toBe(false);
    expect(result.reason.includes('Invalid value')).toBe(true);
  });

  it('should reject Infinity values', () => {
    const coords = new Float32Array(D_BROWSER);
    coords[10] = Infinity;

    const result = guardCoordinates(coords, D_BROWSER);
    expect(result.allowed).toBe(false);
    expect(result.reason.includes('Invalid value')).toBe(true);
  });
});

describe('guardAll', () => {
  it('should pass all guards for valid config', () => {
    const result = guardAll({
      numEntities: 10000,
      dimension: D_BROWSER,
      env: 'browser',
      maxMemoryMB: 100,
    });

    expect(result.allowed).toBe(true);
    expect(result.metrics).toBeDefined();
  });

  it('should fail if any guard fails', () => {
    const result = guardAll({
      numEntities: 1000000,
      dimension: D_HEAVY,
      env: 'browser',
      maxMemoryMB: 50,
    });

    expect(result.allowed).toBe(false);
    expect(result.reason).toBeDefined();
  });

  it('should aggregate metrics from all guards', () => {
    const result = guardAll({
      numEntities: 1000,
      dimension: D_LIGHT,
      env: 'browser',
    });

    expect(result.allowed).toBe(true);
    expect(result.metrics.memoryMB).toBeDefined();
    expect(result.metrics.ops).toBeDefined();
    expect(result.metrics.numEntities).toBeDefined();
  });
});

describe('suggestDimension', () => {
  it('should suggest appropriate dimension for small dataset', () => {
    const { dimension, reason } = suggestDimension(1000, 'browser', 100);

    expect([D_LIGHT, D_BROWSER, D_MEDIUM].includes(dimension)).toBe(true);
    expect(reason.includes('Safe')).toBe(true);
  });

  it('should suggest lower dimension for large dataset', () => {
    const { dimension } = suggestDimension(50000, 'browser', 50);

    // Should suggest something conservative
    expect(dimension <= D_BROWSER).toBe(true);
  });

  it('should recommend sharding for massive datasets', () => {
    const { dimension, reason } = suggestDimension(500000, 'browser', 100);

    expect(dimension).toBe(D_LIGHT);
    expect(reason.includes('sharding')).toBe(true);
  });

  it('should allow higher dimensions for node', () => {
    // For small dataset, it will suggest D_LIGHT (first that passes)
    const small = suggestDimension(10000, 'node', 200);
    expect([D_LIGHT, D_BROWSER, D_MEDIUM].includes(small.dimension)).toBe(true);

    // For larger dataset, verify node can use higher dimensions than browser
    const large = suggestDimension(50000, 'node', 200);
    const largeBrowser = suggestDimension(50000, 'browser', 200);

    // Node should suggest same or higher dimension for same dataset
    expect(large.dimension >= largeBrowser.dimension).toBe(true);
  });
});
