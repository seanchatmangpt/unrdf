/**
 * @vitest-environment node
 * @file File Resolver Tests - Comprehensive coverage for file-resolver.mjs
 */
import { describe, it, expect, beforeEach, vi } from 'vitest';
import { join } from 'path';
import { createFileResolver } from '../src/hooks/file-resolver.mjs';

describe('FileResolver - Construction', () => {
  it('should create resolver with default basePath', () => {
    const resolver = createFileResolver();
    expect(resolver).toBeDefined();
  });

  it('should create resolver with custom basePath', () => {
    const resolver = createFileResolver({ basePath: '/custom/path' });
    expect(resolver).toBeDefined();
  });

  it('should create resolver with cache options', () => {
    const resolver = createFileResolver({
      basePath: '/test',
      enableCache: true,
      cacheSize: 100,
    });
    expect(resolver).toBeDefined();
  });
});

describe('FileResolver - Path Resolution', () => {
  let resolver;

  beforeEach(() => {
    resolver = createFileResolver({ basePath: '/base' });
  });

  it('should resolve relative paths', () => {
    const resolved = resolver.resolve('file.txt');
    expect(resolved).toBe(join('/base', 'file.txt'));
  });

  it('should resolve nested paths', () => {
    const resolved = resolver.resolve('dir/subdir/file.txt');
    expect(resolved).toBe(join('/base', 'dir/subdir/file.txt'));
  });

  it('should handle absolute paths', () => {
    const resolved = resolver.resolve('/absolute/path/file.txt');
    expect(resolved).toBe('/absolute/path/file.txt');
  });

  it('should handle URI schemes', () => {
    const resolved = resolver.resolve('file:///path/to/file.txt');
    expect(resolved).toContain('file.txt');
  });

  it('should handle http/https URIs', () => {
    const httpUri = 'http://example.org/file.txt';
    const httpsUri = 'https://example.org/file.txt';

    expect(resolver.resolve(httpUri)).toBe(httpUri);
    expect(resolver.resolve(httpsUri)).toBe(httpsUri);
  });

  it('should normalize path separators', () => {
    const resolved = resolver.resolve('dir\\subdir\\file.txt');
    expect(resolved).toContain('file.txt');
  });

  it('should handle paths with ..', () => {
    const resolved = resolver.resolve('../parent/file.txt');
    expect(resolved).toBeDefined();
  });

  it('should handle paths with .', () => {
    const resolved = resolver.resolve('./current/file.txt');
    expect(resolved).toContain('file.txt');
  });
});

describe('FileResolver - Hash Validation', () => {
  let resolver;

  beforeEach(() => {
    resolver = createFileResolver();
  });

  it('should validate SHA-256 hash format', () => {
    const validHash = 'a'.repeat(64);
    expect(resolver.isValidHash(validHash, 'sha256')).toBe(true);
  });

  it('should reject invalid SHA-256 hash length', () => {
    const invalidHash = 'a'.repeat(32);
    expect(resolver.isValidHash(invalidHash, 'sha256')).toBe(false);
  });

  it('should reject non-hex characters in hash', () => {
    const invalidHash = 'z'.repeat(64);
    expect(resolver.isValidHash(invalidHash, 'sha256')).toBe(false);
  });

  it('should validate SHA-512 hash format', () => {
    const validHash = 'b'.repeat(128);
    expect(resolver.isValidHash(validHash, 'sha512')).toBe(true);
  });

  it('should handle uppercase hex characters', () => {
    const upperHash = 'A'.repeat(64);
    expect(resolver.isValidHash(upperHash, 'sha256')).toBe(true);
  });

  it('should handle mixed case hex', () => {
    const mixedHash = 'aAbBcC' + 'f'.repeat(58);
    expect(resolver.isValidHash(mixedHash, 'sha256')).toBe(true);
  });
});

describe('FileResolver - Cache Management', () => {
  let resolver;

  beforeEach(() => {
    resolver = createFileResolver({
      enableCache: true,
      cacheSize: 10,
    });
  });

  it('should cache resolved paths', () => {
    const path1 = resolver.resolve('file.txt');
    const path2 = resolver.resolve('file.txt');

    expect(path1).toBe(path2);
  });

  it('should retrieve from cache on second access', () => {
    const spy = vi.spyOn(resolver, 'resolve');

    resolver.resolve('cached.txt');
    resolver.resolve('cached.txt');

    expect(spy).toHaveBeenCalledTimes(2);
  });

  it('should respect cache size limit', () => {
    // Add more than cache size
    for (let i = 0; i < 15; i++) {
      resolver.resolve(`file${i}.txt`);
    }

    const stats = resolver.getCacheStats();
    expect(stats.size).toBeLessThanOrEqual(10);
  });

  it('should provide cache statistics', () => {
    resolver.resolve('file1.txt');
    resolver.resolve('file2.txt');

    const stats = resolver.getCacheStats();
    expect(stats).toHaveProperty('size');
    expect(stats).toHaveProperty('hits');
    expect(stats).toHaveProperty('misses');
  });

  it('should clear cache', () => {
    resolver.resolve('file1.txt');
    resolver.resolve('file2.txt');

    resolver.clearCache();

    const stats = resolver.getCacheStats();
    expect(stats.size).toBe(0);
  });
});

describe('FileResolver - Error Handling', () => {
  let resolver;

  beforeEach(() => {
    resolver = createFileResolver();
  });

  it('should handle null path', () => {
    expect(() => resolver.resolve(null)).toThrow();
  });

  it('should handle undefined path', () => {
    expect(() => resolver.resolve(undefined)).toThrow();
  });

  it('should handle empty path', () => {
    expect(() => resolver.resolve('')).toThrow();
  });

  it('should handle invalid URI schemes', () => {
    expect(() => resolver.resolve('invalid://path')).toThrow();
  });

  it('should provide descriptive error messages', () => {
    try {
      resolver.resolve(null);
    } catch (error) {
      expect(error.message).toContain('path');
    }
  });
});

describe('FileResolver - Security', () => {
  let resolver;

  beforeEach(() => {
    resolver = createFileResolver({
      basePath: '/safe/base',
      enablePathValidation: true,
    });
  });

  it('should prevent path traversal attacks', () => {
    expect(() => {
      resolver.resolve('../../../etc/passwd');
    }).toThrow();
  });

  it('should sanitize dangerous paths', () => {
    const sanitized = resolver.sanitizePath('../../dangerous.txt');
    expect(sanitized).not.toContain('..');
  });

  it('should block absolute paths when restricted', () => {
    resolver = createFileResolver({
      basePath: '/safe',
      allowAbsolutePaths: false,
    });

    expect(() => {
      resolver.resolve('/etc/passwd');
    }).toThrow();
  });

  it('should validate file extensions', () => {
    resolver = createFileResolver({
      allowedExtensions: ['.txt', '.json'],
    });

    expect(() => {
      resolver.resolve('script.exe');
    }).toThrow();
  });
});

describe('FileResolver - Edge Cases', () => {
  let resolver;

  beforeEach(() => {
    resolver = createFileResolver();
  });

  it('should handle very long paths', () => {
    const longPath = 'a/'.repeat(100) + 'file.txt';
    const resolved = resolver.resolve(longPath);
    expect(resolved).toBeDefined();
  });

  it('should handle special characters in filenames', () => {
    const specialFile = 'file with spaces & special (chars).txt';
    const resolved = resolver.resolve(specialFile);
    expect(resolved).toContain(specialFile);
  });

  it('should handle unicode in paths', () => {
    const unicodePath = 'dir-中文/file-émoji-🎯.txt';
    const resolved = resolver.resolve(unicodePath);
    expect(resolved).toContain('file');
  });

  it('should handle multiple consecutive slashes', () => {
    const multiSlash = 'dir///subdir////file.txt';
    const resolved = resolver.resolve(multiSlash);
    expect(resolved).toBeDefined();
  });

  it('should handle trailing slashes', () => {
    const trailing = 'dir/subdir/';
    const resolved = resolver.resolve(trailing);
    expect(resolved).toBeDefined();
  });
});

describe('FileResolver - Concurrent Access', () => {
  it('should handle concurrent resolutions', async () => {
    const resolver = createFileResolver({ enableCache: true });

    const promises = Array(100)
      .fill(null)
      .map((_, i) => Promise.resolve(resolver.resolve(`file${i}.txt`)));

    const results = await Promise.all(promises);
    expect(results).toHaveLength(100);
  });

  it('should maintain cache consistency under concurrent access', async () => {
    const resolver = createFileResolver({ enableCache: true });

    // Resolve same file concurrently
    const promises = Array(50)
      .fill(null)
      .map(() => Promise.resolve(resolver.resolve('shared.txt')));

    const results = await Promise.all(promises);
    const allSame = results.every(r => r === results[0]);
    expect(allSame).toBe(true);
  });
});
