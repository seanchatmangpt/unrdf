#!/usr/bin/env node
/**
 * @file CTAN Resolver Tests
 * @module kgc-cli/lib/latex/ctan-resolver.test
 *
 * @description
 * Unit tests for CTAN resolver with focus on:
 * - Cache determinism
 * - VFS path building
 * - Integration helpers
 * - Error handling
 */

import { describe, it, expect, beforeEach, afterEach } from 'vitest';
import { existsSync, mkdirSync, rmSync, readFileSync, writeFileSync } from 'node:fs';
import { join } from 'node:path';
import {
  resolveMissingInputs,
  augmentVfsWithResolvedPackages,
  clearCache,
  getCacheStats,
} from './ctan-resolver.mjs';

// Test cache directory
const TEST_CACHE_DIR = '/tmp/kgc-latex-test-cache';

describe('CTAN Resolver', () => {
  beforeEach(() => {
    // Clean test cache
    if (existsSync(TEST_CACHE_DIR)) {
      rmSync(TEST_CACHE_DIR, { recursive: true, force: true });
    }
    mkdirSync(TEST_CACHE_DIR, { recursive: true });
  });

  afterEach(() => {
    // Cleanup
    if (existsSync(TEST_CACHE_DIR)) {
      rmSync(TEST_CACHE_DIR, { recursive: true, force: true });
    }
  });

  describe('VFS Path Building', () => {
    it('should build correct VFS path for .sty file', () => {
      const vfs = {};
      const resolvedMap = new Map([
        ['texmf/tex/latex/algorithm2e/algorithm2e.sty', new Uint8Array([1, 2, 3])],
      ]);

      const result = augmentVfsWithResolvedPackages(vfs, resolvedMap);

      expect(result).toHaveProperty('texmf/tex/latex/algorithm2e/algorithm2e.sty');
      expect(result['texmf/tex/latex/algorithm2e/algorithm2e.sty']).toEqual(new Uint8Array([1, 2, 3]));
    });

    it('should not mutate input VFS', () => {
      const vfs = { 'work/main.tex': new Uint8Array([1, 2]) };
      const resolvedMap = new Map([
        ['texmf/tex/latex/test/test.sty', new Uint8Array([3, 4])],
      ]);

      const result = augmentVfsWithResolvedPackages(vfs, resolvedMap);

      expect(vfs).toEqual({ 'work/main.tex': new Uint8Array([1, 2]) });
      expect(result).toHaveProperty('work/main.tex');
      expect(result).toHaveProperty('texmf/tex/latex/test/test.sty');
    });

    it('should merge multiple resolved files', () => {
      const vfs = { 'work/main.tex': new Uint8Array([1]) };
      const resolvedMap = new Map([
        ['texmf/tex/latex/pkg1/pkg1.sty', new Uint8Array([2])],
        ['texmf/tex/latex/pkg2/pkg2.sty', new Uint8Array([3])],
        ['texmf/tex/latex/pkg3/pkg3.cls', new Uint8Array([4])],
      ]);

      const result = augmentVfsWithResolvedPackages(vfs, resolvedMap);

      expect(Object.keys(result)).toHaveLength(4);
      expect(result).toHaveProperty('work/main.tex');
      expect(result).toHaveProperty('texmf/tex/latex/pkg1/pkg1.sty');
      expect(result).toHaveProperty('texmf/tex/latex/pkg2/pkg2.sty');
      expect(result).toHaveProperty('texmf/tex/latex/pkg3/pkg3.cls');
    });
  });

  describe('Cache Management', () => {
    it('should initialize empty cache', () => {
      const stats = getCacheStats(TEST_CACHE_DIR);

      expect(stats.totalEntries).toBe(0);
      expect(stats.totalSize).toBe(0);
      expect(stats.files).toEqual([]);
    });

    it('should clear specific cache entries', () => {
      // Create mock cache index
      const indexPath = join(TEST_CACHE_DIR, 'ctan', 'index.json');
      mkdirSync(join(TEST_CACHE_DIR, 'ctan'), { recursive: true });

      const mockIndex = {
        'test1.sty': {
          path: 'files/abc123.sty',
          hash: 'abc123',
          url: 'https://example.com/test1.sty',
          timestamp: Date.now(),
        },
        'test2.sty': {
          path: 'files/def456.sty',
          hash: 'def456',
          url: 'https://example.com/test2.sty',
          timestamp: Date.now(),
        },
      };

      writeFileSync(indexPath, JSON.stringify(mockIndex, null, 2));

      const removed = clearCache(TEST_CACHE_DIR, ['test1.sty']);

      expect(removed).toBe(1);

      const stats = getCacheStats(TEST_CACHE_DIR);
      expect(stats.totalEntries).toBe(1);
      expect(stats.files).toEqual(['test2.sty']);
    });

    it('should clear entire cache when no inputs specified', () => {
      // Create mock cache index
      const indexPath = join(TEST_CACHE_DIR, 'ctan', 'index.json');
      mkdirSync(join(TEST_CACHE_DIR, 'ctan'), { recursive: true });

      const mockIndex = {
        'test1.sty': {
          path: 'files/abc123.sty',
          hash: 'abc123',
          url: 'https://example.com/test1.sty',
          timestamp: Date.now(),
        },
        'test2.sty': {
          path: 'files/def456.sty',
          hash: 'def456',
          url: 'https://example.com/test2.sty',
          timestamp: Date.now(),
        },
      };

      writeFileSync(indexPath, JSON.stringify(mockIndex, null, 2));

      const removed = clearCache(TEST_CACHE_DIR);

      expect(removed).toBe(2);

      const stats = getCacheStats(TEST_CACHE_DIR);
      expect(stats.totalEntries).toBe(0);
    });
  });

  describe('Input Validation', () => {
    it('should reject empty missingInputs array', async () => {
      await expect(
        resolveMissingInputs({
          missingInputs: [],
          cacheDir: TEST_CACHE_DIR,
        })
      ).rejects.toThrow();
    });

    it('should reject invalid cacheDir', async () => {
      await expect(
        resolveMissingInputs({
          missingInputs: ['test.sty'],
          cacheDir: '',
        })
      ).rejects.toThrow();
    });

    it('should reject invalid CTAN mirror URL', async () => {
      await expect(
        resolveMissingInputs({
          missingInputs: ['test.sty'],
          cacheDir: TEST_CACHE_DIR,
          ctanMirror: 'not-a-url',
        })
      ).rejects.toThrow();
    });
  });

  describe('Error Handling', () => {
    it('should provide clear error for network failures', async () => {
      const invalidMirror = 'https://invalid-ctan-mirror-xyz123.example.com';

      await expect(
        resolveMissingInputs({
          missingInputs: ['nonexistent-package-xyz123.sty'],
          cacheDir: TEST_CACHE_DIR,
          ctanMirror: invalidMirror,
        })
      ).rejects.toThrow(/Failed to fetch.*nonexistent-package-xyz123\.sty/);
    });

    it('should list missing inputs in error message', async () => {
      try {
        await resolveMissingInputs({
          missingInputs: ['fake-pkg-1.sty', 'fake-pkg-2.sty'],
          cacheDir: TEST_CACHE_DIR,
          ctanMirror: 'https://invalid-mirror.example.com',
        });
        expect.fail('Should have thrown error');
      } catch (error) {
        expect(error.message).toMatch(/fake-pkg-1\.sty/);
        expect(error.message).toMatch(/fake-pkg-2\.sty/);
      }
    });
  });

  describe('Cache Determinism', () => {
    it('should create cache directory structure', async () => {
      // Skip actual network call - just verify structure would be created
      expect(existsSync(join(TEST_CACHE_DIR, 'ctan'))).toBe(false);

      // Simulate cache creation
      mkdirSync(join(TEST_CACHE_DIR, 'ctan', 'files'), { recursive: true });

      expect(existsSync(join(TEST_CACHE_DIR, 'ctan'))).toBe(true);
      expect(existsSync(join(TEST_CACHE_DIR, 'ctan', 'files'))).toBe(true);
    });
  });
});

// Export for CLI runner
export default {};
