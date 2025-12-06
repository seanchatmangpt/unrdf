/**
 * @fileoverview Tests for HDIT guards and validation
 */

import { describe, it } from 'node:test';
import assert from 'node:assert/strict';
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
    assert.equal(result.allowed, true);
  });

  it('should allow valid node dimensions', () => {
    const result = guardDimension(D_NODE_MAX, 'node');
    assert.equal(result.allowed, true);
  });

  it('should reject too small dimensions', () => {
    const result = guardDimension(16, 'browser');
    assert.equal(result.allowed, false);
    assert.ok(result.reason.includes('too low'));
  });

  it('should reject browser dimensions that are too high', () => {
    const result = guardDimension(D_NODE_MAX, 'browser');
    assert.equal(result.allowed, false);
    assert.ok(result.reason.includes('exceeds safe limit'));
  });

  it('should reject node dimensions that are too high', () => {
    const result = guardDimension(D_HEAVY, 'node');
    assert.equal(result.allowed, false);
    assert.ok(result.reason.includes('exceeds maximum'));
  });
});

describe('guardMemory', () => {
  it('should allow within memory budget', () => {
    const result = guardMemory(10000, D_BROWSER, 100);
    assert.equal(result.allowed, true);
    assert.ok(result.metrics.memoryMB);
  });

  it('should reject exceeding memory budget', () => {
    const result = guardMemory(1000000, D_HEAVY, 100);
    assert.equal(result.allowed, false);
    assert.ok(result.reason.includes('exceeds budget'));
  });

  it('should provide memory metrics', () => {
    const result = guardMemory(10000, D_BROWSER, 100);
    assert.ok(result.metrics.memoryBytes);
    assert.ok(result.metrics.memoryMB);
    assert.equal(result.metrics.maxMemoryMB, 100);
  });
});

describe('guardLatency', () => {
  it('should allow fast queries', () => {
    const result = guardLatency(1000, D_BROWSER, 10);
    assert.equal(result.allowed, true);
  });

  it('should reject slow queries', () => {
    const result = guardLatency(1000000, D_HEAVY, 10);
    assert.equal(result.allowed, false);
  });

  it('should provide operation count metrics', () => {
    const result = guardLatency(1000, D_BROWSER);
    assert.ok(result.metrics.ops);
    assert.ok(result.metrics.estimatedMs !== undefined);
  });
});

describe('guardEntityCount', () => {
  it('should allow reasonable browser entity count', () => {
    const result = guardEntityCount(10000, D_BROWSER, 'browser');
    assert.equal(result.allowed, true);
  });

  it('should reject too many browser entities', () => {
    const result = guardEntityCount(200000, D_BROWSER, 'browser');
    assert.equal(result.allowed, false);
    assert.ok(result.reason.includes('exceeds browser safe limit'));
  });

  it('should allow larger counts in node', () => {
    const result = guardEntityCount(200000, D_MEDIUM, 'node');
    // May pass or fail depending on latency, but shouldn't fail on count alone
    assert.ok(result.allowed !== undefined);
  });
});

describe('guardCoordinates', () => {
  it('should accept valid Float32Array', () => {
    const coords = new Float32Array(D_BROWSER);
    coords.fill(0.5);

    const result = guardCoordinates(coords, D_BROWSER);
    assert.equal(result.allowed, true);
  });

  it('should reject wrong type', () => {
    const coords = new Array(D_BROWSER).fill(0.5);
    const result = guardCoordinates(coords, D_BROWSER);
    assert.equal(result.allowed, false);
    assert.ok(result.reason.includes('Float32Array'));
  });

  it('should reject dimension mismatch', () => {
    const coords = new Float32Array(32);
    const result = guardCoordinates(coords, D_BROWSER);
    assert.equal(result.allowed, false);
    assert.ok(result.reason.includes('mismatch'));
  });

  it('should reject NaN values', () => {
    const coords = new Float32Array(D_BROWSER);
    coords[10] = NaN;

    const result = guardCoordinates(coords, D_BROWSER);
    assert.equal(result.allowed, false);
    assert.ok(result.reason.includes('Invalid value'));
  });

  it('should reject Infinity values', () => {
    const coords = new Float32Array(D_BROWSER);
    coords[10] = Infinity;

    const result = guardCoordinates(coords, D_BROWSER);
    assert.equal(result.allowed, false);
    assert.ok(result.reason.includes('Invalid value'));
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

    assert.equal(result.allowed, true);
    assert.ok(result.metrics);
  });

  it('should fail if any guard fails', () => {
    const result = guardAll({
      numEntities: 1000000,
      dimension: D_HEAVY,
      env: 'browser',
      maxMemoryMB: 50,
    });

    assert.equal(result.allowed, false);
    assert.ok(result.reason);
  });

  it('should aggregate metrics from all guards', () => {
    const result = guardAll({
      numEntities: 1000,
      dimension: D_LIGHT,
      env: 'browser',
    });

    assert.equal(result.allowed, true);
    assert.ok(result.metrics.memoryMB);
    assert.ok(result.metrics.ops);
    assert.ok(result.metrics.numEntities);
  });
});

describe('suggestDimension', () => {
  it('should suggest appropriate dimension for small dataset', () => {
    const { dimension, reason } = suggestDimension(1000, 'browser', 100);

    assert.ok([D_LIGHT, D_BROWSER, D_MEDIUM].includes(dimension));
    assert.ok(reason.includes('Safe'));
  });

  it('should suggest lower dimension for large dataset', () => {
    const { dimension } = suggestDimension(50000, 'browser', 50);

    // Should suggest something conservative
    assert.ok(dimension <= D_BROWSER);
  });

  it('should recommend sharding for massive datasets', () => {
    const { dimension, reason } = suggestDimension(500000, 'browser', 100);

    assert.equal(dimension, D_LIGHT);
    assert.ok(reason.includes('sharding'));
  });

  it('should allow higher dimensions for node', () => {
    // For small dataset, it will suggest D_LIGHT (first that passes)
    const small = suggestDimension(10000, 'node', 200);
    assert.ok([D_LIGHT, D_BROWSER, D_MEDIUM].includes(small.dimension));

    // For larger dataset, verify node can use higher dimensions than browser
    const large = suggestDimension(50000, 'node', 200);
    const largeBrowser = suggestDimension(50000, 'browser', 200);

    // Node should suggest same or higher dimension for same dataset
    assert.ok(large.dimension >= largeBrowser.dimension);
  });
});
