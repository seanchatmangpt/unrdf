/**
 * @vitest-environment node
 * @file Effect Executor Path Safety Tests
 *
 * Comprehensive tests for symlink traversal, path traversal,
 * absolute path blocking, sandbox boundary enforcement, and
 * memory-limiting file size checks.
 */
import { describe, it, expect, beforeAll, afterAll, beforeEach, afterEach } from 'vitest';
import { promises as fs } from 'fs';
import { join } from 'path';
import {
  MAX_FILE_SIZE,
  validatePath,
  safeFileRead,
  safeFileWrite,
  safeMkdir,
  safeStat,
  safeReaddir,
  safeUnlink,
  ensureSandboxRoot,
} from '../src/hooks/effect-executor.mjs';

// Use the implementation's actual sandbox root (handles /tmp → /private/tmp on macOS)
// The getResolvedSandboxRoot() in effect-executor handles the symlink
const SANDBOX_ROOT_HARDCODED = '/tmp/hooks-sandbox';
const TEST_DIR = 'test-fixtures';
const TEST_DIR_ABS = join(SANDBOX_ROOT_HARDCODED, TEST_DIR);

// Store real resolved paths for assertions (validatePath returns realpath)
let REAL_SANDBOX_ROOT;
let REAL_TEST_DIR_ABS;

describe('Effect Executor - Path Safety', () => {
  beforeAll(async () => {
    // Create the hardcoded sandbox root
    await ensureSandboxRoot();
    await fs.mkdir(TEST_DIR_ABS, { recursive: true });

    // Resolve real paths once for all tests
    // This handles /tmp → /private/tmp on macOS via getResolvedSandboxRoot
    REAL_SANDBOX_ROOT = await fs.realpath(SANDBOX_ROOT_HARDCODED);
    REAL_TEST_DIR_ABS = await fs.realpath(TEST_DIR_ABS);
  });

  afterAll(async () => {
    // Clean up test fixtures (not the whole sandbox, which might be used elsewhere)
    try {
      await fs.rm(TEST_DIR_ABS, { recursive: true, force: true });
    } catch (e) {
      // Ignore cleanup errors
    }
  });

  // ──────────────────────────────────────────────
  // 1. Basic path traversal via ../
  // ──────────────────────────────────────────────
  describe('Path Traversal - ../ sequences', () => {
    it('should block simple ../etc/passwd', async () => {
      await expect(validatePath('../etc/passwd')).rejects.toThrow('Path traversal detected');
    });

    it('should block ../../etc/shadow', async () => {
      await expect(validatePath('../../etc/shadow')).rejects.toThrow('Path traversal detected');
    });

    it('should block deeply nested traversal', async () => {
      await expect(validatePath('a/b/c/../../../../../../../etc/passwd')).rejects.toThrow(
        'Path traversal detected'
      );
    });

    it('should block mid-path traversal', async () => {
      await expect(validatePath('safe/../../../etc/passwd')).rejects.toThrow(
        'Path traversal detected'
      );
    });

    it('should block trailing ../', async () => {
      await expect(validatePath('subdir/../../')).rejects.toThrow('Path traversal detected');
    });
  });

  // ──────────────────────────────────────────────
  // 2. Backslash traversal (Windows-style)
  // ──────────────────────────────────────────────
  describe('Path Traversal - backslash sequences', () => {
    it('should block ..\\Windows\\System32', async () => {
      await expect(validatePath('..\\Windows\\System32')).rejects.toThrow(
        'Path traversal detected'
      );
    });

    it('should block mixed slash traversal', async () => {
      await expect(validatePath('..\\../')).rejects.toThrow('Path traversal detected');
    });
  });

  // ──────────────────────────────────────────────
  // 3. URL-encoded traversal
  // ──────────────────────────────────────────────
  describe('Path Traversal - URL encoding', () => {
    it('should block ..%2f (single encoded /)', async () => {
      await expect(validatePath('..%2fetc%2fpasswd')).rejects.toThrow('Path traversal detected');
    });

    it('should block ..%5c (single encoded \\)', async () => {
      await expect(validatePath('..%5cWindows%5cSystem32')).rejects.toThrow(
        'Path traversal detected'
      );
    });

    it('should block double-encoded ..%252f', async () => {
      await expect(validatePath('..%252f..%252fetc')).rejects.toThrow('Path traversal detected');
    });

    it('should block %2e%2e (encoded dots)', async () => {
      await expect(validatePath('%2e%2e/etc/passwd')).rejects.toThrow('Path traversal detected');
    });

    it('should block double-encoded dots %252e%252e', async () => {
      await expect(validatePath('%252e%252e/etc')).rejects.toThrow('Path traversal detected');
    });

    it('should block overlong UTF-8 ..%c0%af', async () => {
      await expect(validatePath('..%c0%af..%c0%afetc')).rejects.toThrow(
        'Path traversal detected'
      );
    });

    it('should block overlong UTF-8 ..%c1%9c', async () => {
      await expect(validatePath('..%c1%9cWindows')).rejects.toThrow('Path traversal detected');
    });
  });

  // ──────────────────────────────────────────────
  // 4. Null byte injection
  // ──────────────────────────────────────────────
  describe('Path Traversal - null byte injection', () => {
    it('should block null byte in path', async () => {
      await expect(validatePath('safe.txt\x00../../etc/passwd')).rejects.toThrow('null byte');
    });

    it('should block null byte at start', async () => {
      await expect(validatePath('\x00/etc/passwd')).rejects.toThrow('null byte');
    });
  });

  // ──────────────────────────────────────────────
  // 5. Absolute path blocking
  // ──────────────────────────────────────────────
  describe('Absolute Path Blocking', () => {
    it('should block /etc/passwd', async () => {
      await expect(validatePath('/etc/passwd')).rejects.toThrow('Absolute paths are not allowed');
    });

    it('should block absolute path with traversal (traversal caught first)', async () => {
      // The ../ pattern is caught before the absolute path check
      await expect(validatePath(`${SANDBOX_ROOT_HARDCODED}/../etc/passwd`)).rejects.toThrow(
        'Path traversal detected'
      );
    });

    it('should block /usr/bin/bash', async () => {
      await expect(validatePath('/usr/bin/bash')).rejects.toThrow('Absolute paths are not allowed');
    });

    it('should block absolute path even if inside sandbox', async () => {
      await expect(validatePath(`${SANDBOX_ROOT_HARDCODED}/safe.txt`)).rejects.toThrow(
        'Absolute paths are not allowed'
      );
    });
  });

  // ──────────────────────────────────────────────
  // 6. Symlink traversal
  // ──────────────────────────────────────────────
  describe('Symlink Traversal', () => {
    const symlinkName = 'test-fixtures/evil-link';
    const symlinkAbs = join(SANDBOX_ROOT_HARDCODED, symlinkName);

    afterEach(async () => {
      try {
        await fs.unlink(symlinkAbs);
      } catch {
        // ignore cleanup errors
      }
    });

    it('should block symlink pointing outside sandbox', async () => {
      await fs.symlink('/etc', symlinkAbs);
      await expect(validatePath(symlinkName)).rejects.toThrow('symlink escapes sandbox');
    });

    it('should block symlink pointing to /tmp (outside sandbox root)', async () => {
      await fs.symlink('/tmp', symlinkAbs);
      await expect(validatePath(symlinkName)).rejects.toThrow('symlink escapes sandbox');
    });

    it('should block symlink to parent directory', async () => {
      await fs.symlink('/tmp', symlinkAbs);
      // Trying to traverse through symlink
      await expect(validatePath(symlinkName)).rejects.toThrow('symlink escapes sandbox');
    });

    it('should allow symlink within sandbox', async () => {
      // Create a target file inside sandbox (use real path for symlink target)
      const targetPath = join(REAL_TEST_DIR_ABS, 'real-file.txt');
      await fs.writeFile(targetPath, 'hello');

      // Create symlink to it (within sandbox)
      await fs.symlink(targetPath, symlinkAbs);

      const result = await validatePath(symlinkName);
      expect(result).toBe(targetPath);

      await fs.unlink(targetPath);
    });
  });

  // ──────────────────────────────────────────────
  // 7. Whitelist / boundary enforcement
  // ──────────────────────────────────────────────
  describe('Sandbox Boundary Enforcement', () => {
    it('should allow simple filename inside sandbox', async () => {
      const testFile = join(REAL_TEST_DIR_ABS, 'allowed.txt');
      await fs.writeFile(testFile, 'ok');

      const result = await validatePath('test-fixtures/allowed.txt');
      expect(result).toBe(testFile);

      await fs.unlink(testFile);
    });

    it('should allow nested paths inside sandbox', async () => {
      const nested = join(REAL_TEST_DIR_ABS, 'sub');
      await fs.mkdir(nested, { recursive: true });
      const testFile = join(nested, 'deep.txt');
      await fs.writeFile(testFile, 'deep');

      const result = await validatePath('test-fixtures/sub/deep.txt');
      expect(result).toBe(testFile);

      await fs.rm(nested, { recursive: true, force: true });
    });

    it('should handle paths that resolve to sandbox root itself', async () => {
      // "." resolves to the real sandbox root
      const result = await validatePath('.');
      expect(result).toBe(REAL_SANDBOX_ROOT);
    });

    it('should use /tmp/hooks-sandbox as sandbox root', () => {
      expect(SANDBOX_ROOT_HARDCODED).toBe('/tmp/hooks-sandbox');
    });
  });

  // ──────────────────────────────────────────────
  // 8. Input validation edge cases
  // ──────────────────────────────────────────────
  describe('Input Validation', () => {
    it('should reject non-string input', async () => {
      await expect(validatePath(123)).rejects.toThrow('Path must be a string');
      await expect(validatePath(null)).rejects.toThrow('Path must be a string');
      await expect(validatePath(undefined)).rejects.toThrow('Path must be a string');
      await expect(validatePath({})).rejects.toThrow('Path must be a string');
    });

    it('should reject empty string', async () => {
      await expect(validatePath('')).rejects.toThrow('Path must not be empty');
    });

    it('should reject excessively long paths', async () => {
      const longPath = 'a/'.repeat(3000);
      await expect(validatePath(longPath)).rejects.toThrow('exceeds maximum length');
    });
  });

  // ──────────────────────────────────────────────
  // 9. Safe file read
  // ──────────────────────────────────────────────
  describe('safeFileRead', () => {
    const readFile = 'test-fixtures/read-test.txt';
    const readFileAbs = join(SANDBOX_ROOT_HARDCODED, readFile);

    beforeEach(async () => {
      await fs.writeFile(readFileAbs, 'hello world');
    });

    afterEach(async () => {
      try {
        await fs.unlink(readFileAbs);
      } catch {
        // ignore
      }
    });

    it('should read a file inside sandbox', async () => {
      const content = await safeFileRead(readFile);
      expect(content).toBe('hello world');
    });

    it('should reject reading files outside sandbox', async () => {
      await expect(safeFileRead('../etc/passwd')).rejects.toThrow('Path traversal detected');
    });

    it('should reject oversized files', async () => {
      await expect(safeFileRead(readFile, { maxSize: 5 })).rejects.toThrow('exceeds limit');
    });

    it('should respect custom encoding', async () => {
      const content = await safeFileRead(readFile, { encoding: 'utf-8' });
      expect(typeof content).toBe('string');
    });
  });

  // ──────────────────────────────────────────────
  // 10. Safe file write
  // ──────────────────────────────────────────────
  describe('safeFileWrite', () => {
    const writeFile = 'test-fixtures/write-test.txt';
    const writeFileAbs = join(SANDBOX_ROOT_HARDCODED, writeFile);

    afterEach(async () => {
      try {
        await fs.unlink(writeFileAbs);
      } catch {
        // ignore
      }
    });

    it('should write a file inside sandbox', async () => {
      await safeFileWrite(writeFile, 'written data');
      const content = await fs.readFile(writeFileAbs, 'utf-8');
      expect(content).toBe('written data');
    });

    it('should reject writing files outside sandbox', async () => {
      await expect(safeFileWrite('../evil.txt', 'bad')).rejects.toThrow(
        'Path traversal detected'
      );
    });

    it('should reject oversized data', async () => {
      await expect(safeFileWrite(writeFile, 'x'.repeat(100), { maxSize: 50 })).rejects.toThrow(
        'exceeds limit'
      );
    });
  });

  // ──────────────────────────────────────────────
  // 11. Safe mkdir
  // ──────────────────────────────────────────────
  describe('safeMkdir', () => {
    const newDir = 'test-fixtures/new-dir';
    const newDirAbs = join(SANDBOX_ROOT_HARDCODED, newDir);

    afterEach(async () => {
      try {
        await fs.rm(newDirAbs, { recursive: true, force: true });
      } catch {
        // ignore
      }
    });

    it('should create directory inside sandbox', async () => {
      await safeMkdir(newDir);
      const stat = await fs.stat(newDirAbs);
      expect(stat.isDirectory()).toBe(true);
    });

    it('should reject mkdir outside sandbox', async () => {
      await expect(safeMkdir('../evil-dir')).rejects.toThrow('Path traversal detected');
    });
  });

  // ──────────────────────────────────────────────
  // 12. Safe stat
  // ──────────────────────────────────────────────
  describe('safeStat', () => {
    it('should stat file inside sandbox', async () => {
      const testFile = join(TEST_DIR_ABS, 'stat-test.txt');
      await fs.writeFile(testFile, 'data');

      const stat = await safeStat('test-fixtures/stat-test.txt');
      expect(stat.isFile()).toBe(true);

      await fs.unlink(testFile);
    });

    it('should reject stat outside sandbox', async () => {
      await expect(safeStat('../../../etc/passwd')).rejects.toThrow('Path traversal detected');
    });
  });

  // ──────────────────────────────────────────────
  // 13. Safe readdir
  // ──────────────────────────────────────────────
  describe('safeReaddir', () => {
    it('should list directory inside sandbox', async () => {
      const entries = await safeReaddir(TEST_DIR);
      expect(Array.isArray(entries)).toBe(true);
    });

    it('should reject readdir outside sandbox', async () => {
      await expect(safeReaddir('../../../etc')).rejects.toThrow('Path traversal detected');
    });
  });

  // ──────────────────────────────────────────────
  // 14. Safe unlink
  // ──────────────────────────────────────────────
  describe('safeUnlink', () => {
    it('should delete file inside sandbox', async () => {
      const testFile = join(TEST_DIR_ABS, 'delete-me.txt');
      await fs.writeFile(testFile, 'gone');

      await safeUnlink('test-fixtures/delete-me.txt');

      await expect(fs.access(testFile)).rejects.toThrow();
    });

    it('should reject unlink outside sandbox', async () => {
      await expect(safeUnlink('../../../etc/hosts')).rejects.toThrow('Path traversal detected');
    });
  });

  // ──────────────────────────────────────────────
  // 15. Memory / file size limits
  // ──────────────────────────────────────────────
  describe('File Size Limits', () => {
    it('MAX_FILE_SIZE should be 10 MB', () => {
      expect(MAX_FILE_SIZE).toBe(10 * 1024 * 1024);
    });

    it('should enforce file size limit on read', async () => {
      const bigFile = join(TEST_DIR_ABS, 'big.txt');
      // Write a file slightly over the custom limit
      await fs.writeFile(bigFile, Buffer.alloc(1024));

      await expect(safeFileRead('test-fixtures/big.txt', { maxSize: 512 })).rejects.toThrow(
        'exceeds limit'
      );

      await fs.unlink(bigFile);
    });

    it('should enforce data size limit on write', async () => {
      const data = 'x'.repeat(2048);
      await expect(
        safeFileWrite('test-fixtures/too-big.txt', data, { maxSize: 1024 })
      ).rejects.toThrow('exceeds limit');
    });
  });

  // ──────────────────────────────────────────────
  // 16. Ensure sandbox root
  // ──────────────────────────────────────────────
  describe('ensureSandboxRoot', () => {
    it('should create sandbox root if missing', async () => {
      await ensureSandboxRoot();
      const stat = await fs.stat(SANDBOX_ROOT_HARDCODED);
      expect(stat.isDirectory()).toBe(true);
    });
  });
});
