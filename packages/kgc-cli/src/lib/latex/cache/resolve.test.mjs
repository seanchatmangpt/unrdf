/**
 * @file Resolver Tests
 * @module kgc-cli/lib/latex/cache/resolve.test
 */

import { describe, it, expect, beforeEach, afterEach } from 'vitest';
import { existsSync, mkdirSync, rmSync, writeFileSync } from 'node:fs';
import { join } from 'node:path';
import {
  resolveMissingInputs,
  augmentVfsWithResolvedPackages,
  clearCache,
  getCacheStats,
} from './resolve.mjs';

const TEST_CACHE_DIR = '/tmp/kgc-resolve-test-cache';

describe('Resolver with Retry Logic', () => {
  beforeEach(() => {
    if (existsSync(TEST_CACHE_DIR)) {
      rmSync(TEST_CACHE_DIR, { recursive: true, force: true });
    }
    mkdirSync(TEST_CACHE_DIR, { recursive: true });
  });

  afterEach(() => {
    if (existsSync(TEST_CACHE_DIR)) {
      rmSync(TEST_CACHE_DIR, { recursive: true, force: true });
    }
  });

  describe('VFS Augmentation', () => {
    it('should merge resolved files into VFS', () => {
      const vfs = { 'work/main.tex': new Uint8Array([1, 2]) };
      const resolvedMap = new Map([
        ['texmf/tex/latex/test/test.sty', new Uint8Array([3, 4])],
      ]);

      const result = augmentVfsWithResolvedPackages(vfs, resolvedMap);

      expect(result).toHaveProperty('work/main.tex');
      expect(result).toHaveProperty('texmf/tex/latex/test/test.sty');
      expect(result['work/main.tex']).toEqual(new Uint8Array([1, 2]));
      expect(result['texmf/tex/latex/test/test.sty']).toEqual(new Uint8Array([3, 4]));
    });

    it('should not mutate input VFS', () => {
      const vfs = { 'work/main.tex': new Uint8Array([1, 2]) };
      const resolvedMap = new Map([
        ['texmf/tex/latex/test/test.sty', new Uint8Array([3, 4])],
      ]);

      const result = augmentVfsWithResolvedPackages(vfs, resolvedMap);

      expect(vfs).toEqual({ 'work/main.tex': new Uint8Array([1, 2]) });
      expect(Object.keys(vfs)).toHaveLength(1);
      expect(Object.keys(result)).toHaveLength(2);
    });
  });

  describe('Cache Management', () => {
    it('should initialize empty cache', () => {
      const stats = getCacheStats(TEST_CACHE_DIR);

      expect(stats.totalEntries).toBe(0);
      expect(stats.totalSize).toBe(0);
      expect(stats.files).toEqual([]);
    });

    it('should clear entire cache', () => {
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

    it('should clear specific cache entries', () => {
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
  });

  describe('Input Validation', () => {
    it('should reject empty missingInputs', async () => {
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

    it('should reject invalid registry URL', async () => {
      await expect(
        resolveMissingInputs({
          missingInputs: ['test.sty'],
          cacheDir: TEST_CACHE_DIR,
          registry: 'not-a-url',
        })
      ).rejects.toThrow();
    });

    it('should reject invalid maxRetries', async () => {
      await expect(
        resolveMissingInputs({
          missingInputs: ['test.sty'],
          cacheDir: TEST_CACHE_DIR,
          maxRetries: -1,
        })
      ).rejects.toThrow();
    });

    it('should reject maxRetries > 10', async () => {
      await expect(
        resolveMissingInputs({
          missingInputs: ['test.sty'],
          cacheDir: TEST_CACHE_DIR,
          maxRetries: 11,
        })
      ).rejects.toThrow();
    });
  });

  describe('Error Handling', () => {
    it('should provide clear error for network failures', async () => {
      const invalidRegistry = 'https://invalid-mirror-xyz123.example.com';

      await expect(
        resolveMissingInputs({
          missingInputs: ['nonexistent-package.sty'],
          cacheDir: TEST_CACHE_DIR,
          registry: invalidRegistry,
          maxRetries: 1,
          initialDelay: 10,
        })
      ).rejects.toThrow(/Failed to resolve.*nonexistent-package\.sty/);
    });

    it('should list all failed inputs in error message', async () => {
      try {
        await resolveMissingInputs({
          missingInputs: ['fake1.sty', 'fake2.sty'],
          cacheDir: TEST_CACHE_DIR,
          registry: 'https://invalid.example.com',
          maxRetries: 0,
          initialDelay: 10,
        });
        expect.fail('Should have thrown error');
      } catch (error) {
        expect(error.message).toMatch(/fake1\.sty/);
        expect(error.message).toMatch(/fake2\.sty/);
        expect(error.message).toMatch(/Failed to resolve 2 input/);
      }
    });

    it('should include troubleshooting steps in error', async () => {
      try {
        await resolveMissingInputs({
          missingInputs: ['nonexistent.sty'],
          cacheDir: TEST_CACHE_DIR,
          registry: 'https://invalid.example.com',
          maxRetries: 0,
        });
        expect.fail('Should have thrown error');
      } catch (error) {
        expect(error.message).toMatch(/Check package name/);
        expect(error.message).toMatch(/tlmgr install/);
      }
    });
  });

  describe('Retry Logic', () => {
    it('should accept custom retry parameters', async () => {
      // This test verifies parameter validation, not actual retry behavior
      // (actual retry behavior would require mock server)
      await expect(
        resolveMissingInputs({
          missingInputs: ['test.sty'],
          cacheDir: TEST_CACHE_DIR,
          registry: 'https://invalid.example.com',
          maxRetries: 2,
          initialDelay: 50,
        })
      ).rejects.toThrow();
    });
  });
});

export default {};
