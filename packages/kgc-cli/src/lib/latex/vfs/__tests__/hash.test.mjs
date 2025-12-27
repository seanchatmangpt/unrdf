/**
 * @fileoverview Tests for VFS hashing utilities
 */

import { describe, it } from 'node:test';
import assert from 'node:assert/strict';
import {
  hashFile,
  hashVfs,
  hashVfsByExtension,
  areVfsEqual,
  getVfsHashMetadata,
} from '../hash.mjs';

describe('VFS Hash Utilities', () => {
  describe('hashFile', () => {
    it('should hash empty file', () => {
      const content = new Uint8Array([]);
      const hash = hashFile(content);

      // SHA256 of empty string
      assert.equal(
        hash,
        'e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855'
      );
    });

    it('should hash file content', () => {
      const content = new Uint8Array([1, 2, 3]);
      const hash = hashFile(content);

      assert.equal(typeof hash, 'string');
      assert.equal(hash.length, 64); // SHA256 hex = 64 chars
      assert.match(hash, /^[a-f0-9]{64}$/);
    });

    it('should produce same hash for same content', () => {
      const content1 = new Uint8Array([1, 2, 3]);
      const content2 = new Uint8Array([1, 2, 3]);

      assert.equal(hashFile(content1), hashFile(content2));
    });

    it('should produce different hash for different content', () => {
      const content1 = new Uint8Array([1, 2, 3]);
      const content2 = new Uint8Array([1, 2, 4]);

      assert.notEqual(hashFile(content1), hashFile(content2));
    });

    it('should throw on invalid input', () => {
      assert.throws(() => hashFile('not a uint8array'), TypeError);
      assert.throws(() => hashFile([1, 2, 3]), TypeError);
      assert.throws(() => hashFile(null), TypeError);
    });
  });

  describe('hashVfs', () => {
    it('should hash empty VFS', () => {
      const vfs = new Map();
      const hash = hashVfs(vfs);

      // SHA256 of empty string
      assert.equal(
        hash,
        'e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855'
      );
    });

    it('should hash single file VFS', () => {
      const vfs = new Map([
        ['work/main.tex', new Uint8Array([1, 2, 3])],
      ]);

      const hash = hashVfs(vfs);
      assert.equal(typeof hash, 'string');
      assert.equal(hash.length, 64);
    });

    it('should produce deterministic hash (stable ordering)', () => {
      // Create VFS with files in different order
      const vfs1 = new Map([
        ['work/main.tex', new Uint8Array([1, 2, 3])],
        ['work/preamble.tex', new Uint8Array([4, 5, 6])],
        ['work/chapter1.tex', new Uint8Array([7, 8, 9])],
      ]);

      const vfs2 = new Map([
        ['work/chapter1.tex', new Uint8Array([7, 8, 9])],
        ['work/preamble.tex', new Uint8Array([4, 5, 6])],
        ['work/main.tex', new Uint8Array([1, 2, 3])],
      ]);

      // Despite different insertion order, hash should be identical
      assert.equal(hashVfs(vfs1), hashVfs(vfs2));
    });

    it('should detect content changes', () => {
      const vfs1 = new Map([
        ['work/main.tex', new Uint8Array([1, 2, 3])],
      ]);

      const vfs2 = new Map([
        ['work/main.tex', new Uint8Array([1, 2, 4])], // Changed last byte
      ]);

      assert.notEqual(hashVfs(vfs1), hashVfs(vfs2));
    });

    it('should detect path changes', () => {
      const vfs1 = new Map([
        ['work/main.tex', new Uint8Array([1, 2, 3])],
      ]);

      const vfs2 = new Map([
        ['work/main2.tex', new Uint8Array([1, 2, 3])], // Different path
      ]);

      assert.notEqual(hashVfs(vfs1), hashVfs(vfs2));
    });

    it('should detect file additions', () => {
      const vfs1 = new Map([
        ['work/main.tex', new Uint8Array([1, 2, 3])],
      ]);

      const vfs2 = new Map([
        ['work/main.tex', new Uint8Array([1, 2, 3])],
        ['work/new.tex', new Uint8Array([4, 5, 6])],
      ]);

      assert.notEqual(hashVfs(vfs1), hashVfs(vfs2));
    });

    it('should handle nested paths', () => {
      const vfs = new Map([
        ['work/main.tex', new Uint8Array([1])],
        ['work/chapters/ch1.tex', new Uint8Array([2])],
        ['work/chapters/ch2.tex', new Uint8Array([3])],
        ['work/packages/custom.sty', new Uint8Array([4])],
      ]);

      const hash1 = hashVfs(vfs);

      // Same files, different insertion order
      const vfs2 = new Map([
        ['work/packages/custom.sty', new Uint8Array([4])],
        ['work/chapters/ch2.tex', new Uint8Array([3])],
        ['work/main.tex', new Uint8Array([1])],
        ['work/chapters/ch1.tex', new Uint8Array([2])],
      ]);

      const hash2 = hashVfs(vfs2);

      assert.equal(hash1, hash2);
    });

    it('should throw on invalid input', () => {
      assert.throws(() => hashVfs({}), TypeError);
      assert.throws(() => hashVfs([]), TypeError);
      assert.throws(() => hashVfs(null), TypeError);
    });
  });

  describe('hashVfsByExtension', () => {
    it('should hash only specified extensions', () => {
      const vfs = new Map([
        ['work/main.tex', new Uint8Array([1, 2, 3])],
        ['work/style.sty', new Uint8Array([4, 5, 6])],
        ['work/image.png', new Uint8Array([7, 8, 9])],
      ]);

      const texHash = hashVfsByExtension(vfs, ['.tex']);
      const styHash = hashVfsByExtension(vfs, ['.sty']);
      const pngHash = hashVfsByExtension(vfs, ['.png']);

      // All should be different since they hash different files
      assert.notEqual(texHash, styHash);
      assert.notEqual(texHash, pngHash);
      assert.notEqual(styHash, pngHash);
    });

    it('should handle multiple extensions', () => {
      const vfs = new Map([
        ['work/main.tex', new Uint8Array([1, 2, 3])],
        ['work/style.sty', new Uint8Array([4, 5, 6])],
        ['work/image.png', new Uint8Array([7, 8, 9])],
      ]);

      const texStyHash = hashVfsByExtension(vfs, ['.tex', '.sty']);
      const allHash = hashVfs(vfs);

      // Should differ from full VFS hash (missing .png)
      assert.notEqual(texStyHash, allHash);
    });

    it('should be case-insensitive for extensions', () => {
      const vfs = new Map([
        ['work/main.tex', new Uint8Array([1, 2, 3])],
      ]);

      const hash1 = hashVfsByExtension(vfs, ['.tex']);
      const hash2 = hashVfsByExtension(vfs, ['.TEX']);

      assert.equal(hash1, hash2);
    });
  });

  describe('areVfsEqual', () => {
    it('should return true for identical VFS', () => {
      const vfs1 = new Map([
        ['work/main.tex', new Uint8Array([1, 2, 3])],
        ['work/preamble.tex', new Uint8Array([4, 5, 6])],
      ]);

      const vfs2 = new Map([
        ['work/preamble.tex', new Uint8Array([4, 5, 6])],
        ['work/main.tex', new Uint8Array([1, 2, 3])],
      ]);

      assert.equal(areVfsEqual(vfs1, vfs2), true);
    });

    it('should return false for different VFS', () => {
      const vfs1 = new Map([
        ['work/main.tex', new Uint8Array([1, 2, 3])],
      ]);

      const vfs2 = new Map([
        ['work/main.tex', new Uint8Array([1, 2, 4])],
      ]);

      assert.equal(areVfsEqual(vfs1, vfs2), false);
    });

    it('should return true for empty VFS', () => {
      assert.equal(areVfsEqual(new Map(), new Map()), true);
    });
  });

  describe('getVfsHashMetadata', () => {
    it('should return metadata for empty VFS', () => {
      const vfs = new Map();
      const metadata = getVfsHashMetadata(vfs);

      assert.equal(metadata.fileCount, 0);
      assert.equal(metadata.totalBytes, 0);
      assert.deepEqual(metadata.paths, []);
      assert.equal(typeof metadata.hash, 'string');
    });

    it('should return complete metadata', () => {
      const vfs = new Map([
        ['work/main.tex', new Uint8Array([1, 2, 3])],
        ['work/preamble.tex', new Uint8Array([4, 5, 6, 7])],
        ['work/chapter1.tex', new Uint8Array([8, 9])],
      ]);

      const metadata = getVfsHashMetadata(vfs);

      assert.equal(metadata.fileCount, 3);
      assert.equal(metadata.totalBytes, 9); // 3 + 4 + 2
      assert.equal(metadata.paths.length, 3);
      assert.equal(typeof metadata.hash, 'string');
      assert.equal(metadata.hash.length, 64);
    });

    it('should return sorted paths', () => {
      const vfs = new Map([
        ['work/z.tex', new Uint8Array([1])],
        ['work/a.tex', new Uint8Array([2])],
        ['work/m.tex', new Uint8Array([3])],
      ]);

      const metadata = getVfsHashMetadata(vfs);

      // Paths should be sorted
      assert.deepEqual(metadata.paths, [
        'work/a.tex',
        'work/m.tex',
        'work/z.tex',
      ]);
    });
  });

  describe('Hash Stability Regression Tests', () => {
    it('should maintain hash stability across multiple runs', () => {
      const vfs = new Map([
        ['work/main.tex', new Uint8Array([1, 2, 3])],
        ['work/preamble.tex', new Uint8Array([4, 5, 6])],
      ]);

      const hashes = [];
      for (let i = 0; i < 10; i++) {
        hashes.push(hashVfs(vfs));
      }

      // All hashes should be identical
      assert.equal(new Set(hashes).size, 1);
    });

    it('should handle UTF-8 paths correctly', () => {
      const vfs = new Map([
        ['work/café.tex', new Uint8Array([1, 2, 3])],
        ['work/日本語.tex', new Uint8Array([4, 5, 6])],
      ]);

      const hash1 = hashVfs(vfs);
      const hash2 = hashVfs(vfs);

      assert.equal(hash1, hash2);
    });

    it('should handle large files', () => {
      const largeContent = new Uint8Array(1024 * 1024); // 1MB
      for (let i = 0; i < largeContent.length; i++) {
        largeContent[i] = i % 256;
      }

      const vfs = new Map([
        ['work/large.tex', largeContent],
      ]);

      const hash1 = hashVfs(vfs);
      const hash2 = hashVfs(vfs);

      assert.equal(hash1, hash2);
    });
  });
});
