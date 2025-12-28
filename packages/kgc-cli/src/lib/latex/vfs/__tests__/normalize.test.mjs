/**
 * @fileoverview Tests for VFS path normalization
 */

import { describe, it } from 'node:test';
import assert from 'node:assert/strict';
import {
  normalizePath,
  isRelativePath,
  isValidVfsPath,
} from '../normalize.mjs';

describe('VFS Path Normalization', () => {
  describe('normalizePath', () => {
    it('should handle POSIX paths', () => {
      assert.equal(normalizePath('foo/bar/baz.tex'), 'foo/bar/baz.tex');
      assert.equal(normalizePath('main.tex'), 'main.tex');
    });

    it('should convert Windows paths to POSIX', () => {
      assert.equal(normalizePath('foo\\bar\\baz.tex'), 'foo/bar/baz.tex');
      assert.equal(normalizePath('main.tex'), 'main.tex');
    });

    it('should remove leading ./', () => {
      assert.equal(normalizePath('./main.tex'), 'main.tex');
      assert.equal(normalizePath('./foo/bar.tex'), 'foo/bar.tex');
    });

    it('should remove leading /', () => {
      assert.equal(normalizePath('/main.tex'), 'main.tex');
      assert.equal(normalizePath('/foo/bar.tex'), 'foo/bar.tex');
    });

    it('should remove duplicate slashes', () => {
      assert.equal(normalizePath('foo//bar.tex'), 'foo/bar.tex');
      assert.equal(normalizePath('foo///bar///baz.tex'), 'foo/bar/baz.tex');
    });

    it('should remove trailing slash', () => {
      assert.equal(normalizePath('foo/bar/'), 'foo/bar');
      assert.equal(normalizePath('main.tex/'), 'main.tex');
    });

    it('should handle mixed separators', () => {
      assert.equal(normalizePath('./foo\\bar/baz.tex'), 'foo/bar/baz.tex');
    });

    it('should throw on invalid input', () => {
      assert.throws(() => normalizePath(null), TypeError);
      assert.throws(() => normalizePath(undefined), TypeError);
      assert.throws(() => normalizePath(123), TypeError);
    });
  });

  describe('isRelativePath', () => {
    it('should return true for relative paths', () => {
      assert.equal(isRelativePath('foo/bar.tex'), true);
      assert.equal(isRelativePath('main.tex'), true);
      assert.equal(isRelativePath('./foo.tex'), true);
      assert.equal(isRelativePath('a/b/c/d.tex'), true);
    });

    it('should return false for absolute POSIX paths', () => {
      assert.equal(isRelativePath('/foo/bar.tex'), false);
      assert.equal(isRelativePath('/main.tex'), false);
    });

    it('should return false for Windows absolute paths', () => {
      assert.equal(isRelativePath('C:\\foo\\bar.tex'), false);
      assert.equal(isRelativePath('D:/foo/bar.tex'), false);
      assert.equal(isRelativePath('c:\\main.tex'), false);
    });

    it('should return false for invalid input', () => {
      assert.equal(isRelativePath(null), false);
      assert.equal(isRelativePath(undefined), false);
      assert.equal(isRelativePath(123), false);
    });
  });

  describe('isValidVfsPath', () => {
    it('should return true for valid VFS paths', () => {
      assert.equal(isValidVfsPath('foo/bar.tex'), true);
      assert.equal(isValidVfsPath('main.tex'), true);
      assert.equal(isValidVfsPath('a/b/c/d.tex'), true);
    });

    it('should return false for path traversal', () => {
      assert.equal(isValidVfsPath('../etc/passwd'), false);
      assert.equal(isValidVfsPath('foo/../bar.tex'), false);
      assert.equal(isValidVfsPath('../../main.tex'), false);
    });

    it('should return false for double slashes', () => {
      assert.equal(isValidVfsPath('foo//bar.tex'), false);
      assert.equal(isValidVfsPath('a///b.tex'), false);
    });

    it('should return false for backslashes', () => {
      assert.equal(isValidVfsPath('foo\\bar.tex'), false);
      assert.equal(isValidVfsPath('main\\test.tex'), false);
    });

    it('should return false for leading slash', () => {
      assert.equal(isValidVfsPath('/foo/bar.tex'), false);
      assert.equal(isValidVfsPath('/main.tex'), false);
    });

    it('should return false for empty path', () => {
      assert.equal(isValidVfsPath(''), false);
    });

    it('should return false for invalid input', () => {
      assert.equal(isValidVfsPath(null), false);
      assert.equal(isValidVfsPath(undefined), false);
      assert.equal(isValidVfsPath(123), false);
    });
  });

  describe('Path Normalization Edge Cases', () => {
    it('should handle empty segments', () => {
      assert.equal(normalizePath('foo//bar'), 'foo/bar');
      assert.equal(normalizePath('a///b///c'), 'a/b/c');
    });

    it('should handle multiple leading slashes', () => {
      assert.equal(normalizePath('///foo/bar'), 'foo/bar');
    });

    it('should handle multiple trailing slashes', () => {
      assert.equal(normalizePath('foo/bar///'), 'foo/bar');
    });

    it('should handle complex mixed cases', () => {
      assert.equal(
        normalizePath('./foo\\\\bar//baz.tex'),
        'foo/bar/baz.tex'
      );
    });

    it('should preserve UTF-8 characters', () => {
      assert.equal(normalizePath('café/日本語.tex'), 'café/日本語.tex');
      assert.equal(normalizePath('./café/日本語.tex'), 'café/日本語.tex');
    });
  });
});
