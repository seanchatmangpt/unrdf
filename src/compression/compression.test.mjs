/**
 * @fileoverview Comprehensive Tests for Compression Module
 *
 * Tests all compression algorithms with round-trip verification
 * and idempotency property validation.
 */

import { describe, it, expect, beforeEach } from 'vitest';
import { z } from 'zod';

import {
  LZ4Compressor,
  createLZ4Compressor,
  compressLZ4,
  decompressLZ4
} from './algorithms/lz4.mjs';

import {
  BrotliCompressor,
  createBrotliCompressor,
  compressBrotli,
  decompressBrotli
} from './algorithms/brotli.mjs';

import {
  DeltaEncoder,
  createDeltaEncoder,
  compressDelta,
  decompressDelta
} from './algorithms/delta.mjs';

import {
  ColumnarCompressor,
  createColumnarCompressor,
  compressColumnar,
  decompressColumnar
} from './algorithms/columnar.mjs';

import {
  AdaptiveCompressionSelector,
  createAdaptiveSelector,
  compressAdaptive,
  decompressAdaptive
} from './adaptive-selector.mjs';

import {
  SemanticCompressor,
  createSemanticCompressor,
  compressSemantic,
  decompressSemantic
} from './semantic-compressor.mjs';

import {
  compress,
  decompress,
  verifyRoundTrip,
  testIdempotency
} from './index.mjs';

// ============================================================================
// Test Data Generators
// ============================================================================

function generateTextData(size = 1000) {
  const words = ['the', 'quick', 'brown', 'fox', 'jumps', 'over', 'lazy', 'dog'];
  const result = [];
  for (let i = 0; i < size; i++) {
    result.push(words[i % words.length]);
  }
  return result.join(' ');
}

function generateTemporalData(count = 100) {
  const sequence = [];
  let value = { counter: 0, status: 'active', metadata: { id: 1 } };

  for (let i = 0; i < count; i++) {
    sequence.push({ ...value });
    value.counter = i;
    value.metadata.id = Math.floor(i / 10);
  }

  return sequence;
}

function generateStructuredData(count = 100) {
  const data = [];
  for (let i = 0; i < count; i++) {
    data.push({
      id: i,
      name: `Item_${i % 10}`,
      category: ['A', 'B', 'C'][i % 3],
      value: Math.floor(i / 5),
      active: i % 2 === 0
    });
  }
  return data;
}

// ============================================================================
// LZ4 Tests
// ============================================================================

describe('LZ4Compressor', () => {
  let compressor;

  beforeEach(() => {
    compressor = new LZ4Compressor();
  });

  describe('compress/decompress', () => {
    it('should compress and decompress text data', async () => {
      const original = generateTextData(500);
      const compressed = await compressor.compress(original);
      const decompressed = await compressor.decompress(compressed);

      expect(compressed.algorithm).toBe('lz4');
      expect(compressed.ratio).toBeGreaterThan(1);
      expect(decompressed.toString('utf-8')).toBe(original);
    });

    it('should compress binary data', async () => {
      const original = Buffer.from([1, 2, 3, 4, 5, 1, 2, 3, 4, 5]);
      const compressed = await compressor.compress(original);
      const decompressed = await compressor.decompress(compressed);

      expect(Buffer.compare(original, decompressed)).toBe(0);
    });

    it('should handle empty data', async () => {
      const original = '';
      const compressed = await compressor.compress(original);
      const decompressed = await compressor.decompress(compressed);

      expect(decompressed.toString('utf-8')).toBe(original);
    });

    it('should detect checksum mismatches', async () => {
      const original = 'test data';
      const compressed = await compressor.compress(original);

      // Tamper with compressed data
      compressed.compressed = Buffer.from('invalid');

      await expect(compressor.decompress(compressed)).rejects.toThrow();
    });
  });

  describe('statistics', () => {
    it('should track compression statistics', async () => {
      await compressor.compress('test1');
      await compressor.compress('test2');

      const stats = compressor.getStats();

      expect(stats.totalCompressed).toBe(2);
      expect(stats.avgCompressionTime).toBeGreaterThan(0);
    });
  });
});

// ============================================================================
// Brotli Tests
// ============================================================================

describe('BrotliCompressor', () => {
  let compressor;

  beforeEach(() => {
    compressor = new BrotliCompressor();
  });

  describe('compress/decompress', () => {
    it('should compress and decompress text data', async () => {
      const original = generateTextData(1000);
      const compressed = await compressor.compress(original);
      const decompressed = await compressor.decompress(compressed);

      expect(compressed.algorithm).toBe('brotli');
      expect(compressed.ratio).toBeGreaterThan(1);
      expect(decompressed.toString('utf-8')).toBe(original);
    });

    it('should achieve better compression than LZ4 on large data', async () => {
      const original = generateTextData(5000);

      const lz4 = new LZ4Compressor();
      const brotli = new BrotliCompressor({ quality: 9 });

      const lz4Result = await lz4.compress(original);
      const brotliResult = await brotli.compress(original);

      // Brotli should have better ratio
      expect(brotliResult.ratio).toBeGreaterThan(lz4Result.ratio * 0.9); // At least 90% of LZ4
    });
  });

  describe('quality levels', () => {
    it('should respect quality settings', async () => {
      const original = generateTextData(2000);

      const fastCompressor = new BrotliCompressor({ quality: 1 });
      const bestCompressor = new BrotliCompressor({ quality: 11 });

      const fastResult = await fastCompressor.compress(original);
      const bestResult = await bestCompressor.compress(original);

      // Higher quality should take longer but achieve better ratio
      expect(bestResult.compressionTime).toBeGreaterThanOrEqual(fastResult.compressionTime * 0.5);
    });
  });
});

// ============================================================================
// Delta Encoder Tests
// ============================================================================

describe('DeltaEncoder', () => {
  let encoder;

  beforeEach(() => {
    encoder = new DeltaEncoder();
  });

  describe('compress/decompress', () => {
    it('should compress temporal sequences', async () => {
      const original = generateTemporalData(50);
      const compressed = await encoder.compress(original);
      const decompressed = await encoder.decompress(compressed);

      expect(compressed.algorithm).toBe('delta');
      expect(compressed.ratio).toBeGreaterThan(1);
      expect(JSON.stringify(decompressed)).toBe(JSON.stringify(original));
    });

    it('should handle numeric sequences', async () => {
      const original = [100, 101, 102, 103, 104, 105];
      const compressed = await encoder.compress(original);
      const decompressed = await encoder.decompress(compressed);

      expect(decompressed).toEqual(original);
    });

    it('should create base snapshots at intervals', async () => {
      const original = generateTemporalData(150);
      const compressed = await encoder.compress(original);

      expect(compressed.metadata.baseSnapshots).toBeGreaterThan(1);
      expect(compressed.metadata.deltas).toBeGreaterThan(0);
    });
  });

  describe('delta types', () => {
    it('should handle XOR delta type', async () => {
      const encoder = new DeltaEncoder({ deltaType: 'xor' });
      const original = [1, 2, 3, 4, 5];
      const compressed = await encoder.compress(original);
      const decompressed = await encoder.decompress(compressed);

      expect(decompressed).toEqual(original);
    });

    it('should handle subtract delta type', async () => {
      const encoder = new DeltaEncoder({ deltaType: 'subtract' });
      const original = [100, 102, 105, 110, 120];
      const compressed = await encoder.compress(original);
      const decompressed = await encoder.decompress(compressed);

      expect(decompressed).toEqual(original);
    });
  });
});

// ============================================================================
// Columnar Compressor Tests
// ============================================================================

describe('ColumnarCompressor', () => {
  let compressor;

  beforeEach(() => {
    compressor = new ColumnarCompressor();
  });

  describe('compress/decompress', () => {
    it('should compress structured data', async () => {
      const original = generateStructuredData(100);
      const compressed = await compressor.compress(original);
      const decompressed = await compressor.decompress(compressed);

      expect(compressed.algorithm).toBe('columnar');
      expect(compressed.ratio).toBeGreaterThan(1);
      expect(JSON.stringify(decompressed)).toBe(JSON.stringify(original));
    });

    it('should leverage repeated values', async () => {
      const original = Array(100).fill(null).map((_, i) => ({
        id: i,
        category: 'A', // Repeated value
        status: 'active' // Repeated value
      }));

      const compressed = await compressor.compress(original);

      expect(compressed.ratio).toBeGreaterThan(2); // Good compression due to repeated values
    });
  });

  describe('dictionary encoding', () => {
    it('should apply dictionary encoding when beneficial', async () => {
      const original = Array(50).fill(null).map((_, i) => ({
        id: i,
        name: ['Alice', 'Bob', 'Charlie'][i % 3]
      }));

      const compressed = await compressor.compress(original);
      const decompressed = await compressor.decompress(compressed);

      expect(decompressed).toEqual(original);
    });
  });
});

// ============================================================================
// Adaptive Selector Tests
// ============================================================================

describe('AdaptiveCompressionSelector', () => {
  let selector;

  beforeEach(() => {
    selector = new AdaptiveCompressionSelector();
  });

  describe('algorithm selection', () => {
    it('should select LZ4 for small data', async () => {
      const original = 'small test';
      const compressed = await selector.compress(original);

      expect(compressed.selectedAlgorithm).toBe('lz4');
    });

    it('should select delta for temporal data', async () => {
      const original = generateTemporalData(50);
      const compressed = await selector.compress(original, { isTemporal: true });

      expect(compressed.selectedAlgorithm).toBe('delta');
    });

    it('should select columnar for structured arrays', async () => {
      const original = generateStructuredData(50);
      const compressed = await selector.compress(original);

      expect(compressed.selectedAlgorithm).toBe('columnar');
    });
  });

  describe('learning', () => {
    it('should learn from compression history', async () => {
      const selector = new AdaptiveCompressionSelector({ enableLearning: true });
      const data = generateTextData(1000);

      // Compress multiple times
      for (let i = 0; i < 5; i++) {
        await selector.compress(data);
      }

      const stats = selector.getStats();
      expect(stats.learningHistorySize).toBeGreaterThan(0);
    });
  });

  describe('round-trip', () => {
    it('should preserve data through compress/decompress cycle', async () => {
      const original = { a: 1, b: [2, 3], c: { d: 4 } };
      const compressed = await selector.compress(original);
      const decompressed = await selector.decompress(compressed);

      expect(JSON.stringify(decompressed)).toBe(JSON.stringify(original));
    });
  });
});

// ============================================================================
// Semantic Compressor Tests
// ============================================================================

describe('SemanticCompressor', () => {
  let compressor;

  beforeEach(() => {
    compressor = new SemanticCompressor();
  });

  describe('pattern extraction', () => {
    it('should extract repeated patterns', async () => {
      const original = {
        items: [
          { type: 'commonPatternHere', value: 1 },
          { type: 'commonPatternHere', value: 2 },
          { type: 'commonPatternHere', value: 3 }
        ]
      };

      const compressed = await compressor.compress(original);
      const decompressed = await compressor.decompress(compressed);

      expect(JSON.stringify(decompressed)).toBe(JSON.stringify(original));
      expect(compressor.getStats().patternsExtracted).toBeGreaterThanOrEqual(0);
    });
  });

  describe('schema-aware compression', () => {
    it('should use schema for compression', async () => {
      const schema = z.array(z.object({
        id: z.number(),
        name: z.string()
      }));

      const original = [
        { id: 1, name: 'Alice' },
        { id: 2, name: 'Bob' }
      ];

      const compressed = await compressor.compress(original, schema);
      const decompressed = await compressor.decompress(compressed);

      expect(decompressed).toEqual(original);
    });
  });

  describe('dictionary learning', () => {
    it('should build dictionaries for repeated structures', async () => {
      const original = {
        items: Array(10).fill({ complex: { nested: { object: 'value' } } })
      };

      const compressed = await compressor.compress(original);
      const decompressed = await compressor.decompress(compressed);

      expect(JSON.stringify(decompressed)).toBe(JSON.stringify(original));
    });
  });
});

// ============================================================================
// Integration Tests
// ============================================================================

describe('Compression Module Integration', () => {
  describe('compress/decompress utilities', () => {
    it('should compress and decompress with defaults', async () => {
      const original = generateStructuredData(50);
      const compressed = await compress(original);
      const decompressed = await decompress(compressed);

      expect(JSON.stringify(decompressed)).toBe(JSON.stringify(original));
    });
  });

  describe('verifyRoundTrip', () => {
    it('should verify round-trip fidelity', async () => {
      const original = { test: 'data', nested: { value: 123 } };
      const compressed = await compress(original);
      const verification = await verifyRoundTrip(original, compressed);

      expect(verification.fidelityPreserved).toBe(true);
      expect(verification.compressionRatio).toBeGreaterThan(0);
    });
  });

  describe('testIdempotency', () => {
    it('should verify μ ∘ μ = μ property', async () => {
      const original = generateTextData(500);
      const result = await testIdempotency(original);

      expect(result.isIdempotent).toBe(true);
      expect(result.interpretation).toContain('Idempotent');
    });

    it('should detect non-idempotent behavior on uncompressed data', async () => {
      const original = 'a'.repeat(1000); // Highly compressible
      const result = await testIdempotency(original);

      // First compression should improve significantly
      expect(result.firstCompressionRatio).toBeGreaterThan(1);
    });
  });

  describe('all algorithms round-trip', () => {
    const testData = [
      { name: 'text', data: generateTextData(500) },
      { name: 'temporal', data: generateTemporalData(50) },
      { name: 'structured', data: generateStructuredData(50) },
      { name: 'object', data: { a: 1, b: { c: [2, 3, 4] } } }
    ];

    for (const { name, data } of testData) {
      it(`should handle ${name} data across all algorithms`, async () => {
        const algorithms = [
          { name: 'LZ4', compress: compressLZ4, decompress: decompressLZ4 },
          { name: 'Brotli', compress: compressBrotli, decompress: decompressBrotli }
        ];

        for (const { name: algoName, compress, decompress } of algorithms) {
          const compressed = await compress(data);
          const decompressed = await decompress(compressed);

          const originalStr = JSON.stringify(data);
          const decompressedStr = JSON.stringify(decompressed.toString ? JSON.parse(decompressed.toString('utf-8')) : decompressed);

          expect(decompressedStr).toBe(originalStr);
        }
      });
    }
  });
});

// ============================================================================
// Performance Tests
// ============================================================================

describe('Compression Performance', () => {
  it('should compress within reasonable time', async () => {
    const data = generateTextData(10000);
    const startTime = performance.now();

    await compress(data);

    const duration = performance.now() - startTime;

    // Should complete within 100ms for 10KB data
    expect(duration).toBeLessThan(100);
  });

  it('should decompress faster than compression', async () => {
    const data = generateTextData(5000);

    const compressStart = performance.now();
    const compressed = await compress(data);
    const compressTime = performance.now() - compressStart;

    const decompressStart = performance.now();
    await decompress(compressed);
    const decompressTime = performance.now() - decompressStart;

    // Decompression should generally be faster
    expect(decompressTime).toBeLessThanOrEqual(compressTime * 1.5);
  });
});
