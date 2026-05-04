/**
 * KGEN Atomic Writer Tests
 *
 * Tests for atomic file operations, backup creation,
 * and transactional multi-file operations.
 */

import { describe, test, expect, beforeEach, afterEach } from 'vitest';
import { promises as fs } from 'fs';
import { join } from 'path';
import { tmpdir } from 'os';

import { AtomicWriter } from '../atomic-writer.js';

describe('AtomicWriter', () => {
  let tempDir;
  let writer;
  let testFile;

  beforeEach(async () => {
    tempDir = await fs.mkdtemp(join(tmpdir(), 'atomic-writer-test-'));
    writer = new AtomicWriter({
      backupEnabled: true,
      preservePermissions: true
    });
    testFile = join(tempDir, 'test.txt');
  });

  afterEach(async () => {
    await fs.rm(tempDir, { recursive: true, force: true });
  });

  describe('Atomic Write Operations', () => {
    test('should write file atomically', async () => {
      const content = 'Hello, atomic world!';

      const result = await writer.writeAtomic(testFile, content);

      expect(result.success).toBe(true);
      expect(result.filePath).toBe(testFile);
      expect(result.checksum).toMatch(/^[a-f0-9]{64}$/);

      const actualContent = await fs.readFile(testFile, 'utf8');
      expect(actualContent).toBe(content);
    });

    test('should create backup when file exists', async () => {
      // Setup existing file
      const originalContent = 'Original content';
      await fs.writeFile(testFile, originalContent);

      const newContent = 'New content';
      const operationId = 'test-operation';

      const result = await writer.writeAtomic(testFile, newContent, {
        backup: true,
        operationId
      });

      expect(result.success).toBe(true);
      expect(result.backupPath).toBeDefined();

      // Verify backup exists and contains original content
      const backupExists = await fs.access(result.backupPath)
        .then(() => true, () => false);
      expect(backupExists).toBe(true);

      const backupContent = await fs.readFile(result.backupPath, 'utf8');
      expect(backupContent).toBe(originalContent);

      // Verify main file has new content
      const mainContent = await fs.readFile(testFile, 'utf8');
      expect(mainContent).toBe(newContent);
    });

    test('should preserve file permissions', async () => {
      // Setup file with specific permissions
      await fs.writeFile(testFile, 'test content');
      await fs.chmod(testFile, 0o755);

      const newContent = 'new content';

      await writer.writeAtomic(testFile, newContent, {
        preserveMetadata: true
      });

      // Check permissions were preserved
      const stats = await fs.stat(testFile);
      expect(stats.mode & 0o777).toBe(0o755);
    });

    test('should handle concurrent writes with file locking', async () => {
      const promises = [];

      // Attempt multiple concurrent writes
      for (let i = 0; i < 5; i++) {
        const promise = writer.writeAtomic(testFile, `Content ${i}`);
        promises.push(promise);
      }

      const results = await Promise.all(promises);

      // All writes should succeed
      results.forEach(result => {
        expect(result.success).toBe(true);
      });

      // File should contain one of the contents
      const finalContent = await fs.readFile(testFile, 'utf8');
      expect(finalContent).toMatch(/^Content \d$/);
    });

    test('should clean up temp file on failure', async () => {
      // Mock a write failure
      const originalWriteFile = fs.writeFile;
      fs.writeFile = async (path, content) => {
        if (path.includes('.kgen-temp-')) {
          throw new Error('Simulated write failure');
        }
        return originalWriteFile(path, content);
      };

      try {
        await writer.writeAtomic(testFile, 'test content');
        expect(true).toBe(false); // Should not reach here
      } catch (error) {
        expect(error.message).toBe('Simulated write failure');
      }

      // Check no temp files were left behind
      const files = await fs.readdir(tempDir);
      const tempFiles = files.filter(f => f.includes('.kgen-temp-'));
      expect(tempFiles).toHaveLength(0);

      // Restore original function
      fs.writeFile = originalWriteFile;
    });

    test('should restore from backup on write failure', async () => {
      // Setup existing file
      const originalContent = 'Original content';
      await fs.writeFile(testFile, originalContent);

      // Mock a failure during the atomic rename
      const originalRename = fs.rename;
      fs.rename = async () => {
        throw new Error('Simulated rename failure');
      };

      const result = await writer.writeAtomic(testFile, 'New content', {
        backup: true
      }).catch(error => error);

      expect(result).toBeInstanceOf(Error);

      // File should be restored to original content
      const restoredContent = await fs.readFile(testFile, 'utf8');
      expect(restoredContent).toBe(originalContent);

      // Restore original function
      fs.rename = originalRename;
    });
  });

  describe('Transaction Operations', () => {
    test('should handle multi-file atomic transaction', async () => {
      const files = [
        join(tempDir, 'file1.txt'),
        join(tempDir, 'file2.txt'),
        join(tempDir, 'file3.txt')
      ];

      const transaction = await writer.beginTransaction('test-transaction');

      // Prepare writes for all files
      await transaction.prepareWrite(files[0], 'Content 1');
      await transaction.prepareWrite(files[1], 'Content 2');
      await transaction.prepareWrite(files[2], 'Content 3');

      // Commit transaction
      const result = await transaction.commit();

      expect(result.success).toBe(true);
      expect(result.filesWritten).toBe(3);

      // Verify all files were written
      for (let i = 0; i < files.length; i++) {
        const content = await fs.readFile(files[i], 'utf8');
        expect(content).toBe(`Content ${i + 1}`);
      }
    });

    test('should rollback transaction on failure', async () => {
      const files = [
        join(tempDir, 'file1.txt'),
        join(tempDir, 'file2.txt')
      ];

      // Create existing files with content
      await fs.writeFile(files[0], 'Original 1');
      await fs.writeFile(files[1], 'Original 2');

      const transaction = await writer.beginTransaction('test-transaction');

      // Prepare first write
      await transaction.prepareWrite(files[0], 'New Content 1');

      // Mock failure on second write
      const originalWriteFile = fs.writeFile;
      let writeCount = 0;
      fs.writeFile = async (path, content, options) => {
        writeCount++;
        if (writeCount === 2 && path.includes('file2')) {
          throw new Error('Simulated failure');
        }
        return originalWriteFile(path, content, options);
      };

      try {
        await transaction.prepareWrite(files[1], 'New Content 2');
        await transaction.commit();
        expect(true).toBe(false); // Should not reach here
      } catch (error) {
        // Transaction should rollback automatically
      }

      // Verify original content is preserved
      const content1 = await fs.readFile(files[0], 'utf8');
      const content2 = await fs.readFile(files[1], 'utf8');
      expect(content1).toBe('Original 1');
      expect(content2).toBe('Original 2');

      // Restore original function
      fs.writeFile = originalWriteFile;
    });

    test('should handle transaction rollback explicitly', async () => {
      const files = [
        join(tempDir, 'file1.txt'),
        join(tempDir, 'file2.txt')
      ];

      // Create existing files
      await fs.writeFile(files[0], 'Original 1');
      await fs.writeFile(files[1], 'Original 2');

      const transaction = await writer.beginTransaction('test-transaction');

      // Prepare writes
      await transaction.prepareWrite(files[0], 'New Content 1');
      await transaction.prepareWrite(files[1], 'New Content 2');

      // Explicitly rollback
      await transaction.rollback();

      // Verify files are restored
      const content1 = await fs.readFile(files[0], 'utf8');
      const content2 = await fs.readFile(files[1], 'utf8');
      expect(content1).toBe('Original 1');
      expect(content2).toBe('Original 2');
    });

    test('should prevent double commit', async () => {
      const transaction = await writer.beginTransaction('test-transaction');

      await transaction.prepareWrite(testFile, 'Test content');
      await transaction.commit();

      // Second commit should fail
      await expect(transaction.commit())
        .rejects.toThrow('Transaction already committed');
    });

    test('should validate temp files before commit', async () => {
      const transaction = await writer.beginTransaction('test-transaction');

      await transaction.prepareWrite(testFile, 'Test content');

      // Manually remove temp file to simulate corruption
      const tempFile = transaction.preparedWrites[0].tempPath;
      await fs.unlink(tempFile);

      // Commit should fail
      await expect(transaction.commit())
        .rejects.toThrow();
    });
  });

  describe('File Locking', () => {
    test('should acquire and release locks properly', async () => {
      const lockFile = `${testFile}.kgen-lock`;

      // Start first operation
      const promise1 = writer.writeAtomic(testFile, 'Content 1');

      // Try to start second operation immediately
      const promise2 = writer.writeAtomic(testFile, 'Content 2');

      // Both should complete successfully (one waits for the other)
      const results = await Promise.all([promise1, promise2]);

      expect(results[0].success).toBe(true);
      expect(results[1].success).toBe(true);

      // Lock file should be cleaned up
      const lockExists = await fs.access(lockFile)
        .then(() => true, () => false);
      expect(lockExists).toBe(false);
    });

    test('should handle stale locks', async () => {
      const lockFile = `${testFile}.kgen-lock`;

      // Create stale lock file (old timestamp)
      await fs.writeFile(lockFile, 'stale-lock-id');
      const oldTime = Date.now() - 15000; // 15 seconds ago
      await fs.utimes(lockFile, new Date(oldTime), new Date(oldTime));

      // Should still be able to write (stale lock removed)
      const result = await writer.writeAtomic(testFile, 'Test content');

      expect(result.success).toBe(true);
    });

    test('should timeout on persistent locks', async () => {
      // Create a persistent lock
      const lockFile = `${testFile}.kgen-lock`;
      await fs.writeFile(lockFile, 'persistent-lock');

      // Set very short timeout for testing
      const shortTimeoutWriter = new AtomicWriter({
        backupEnabled: false,
        lockConfig: { TIMEOUT: 100, RETRY_DELAY: 10 }
      });

      // Update lock timestamp continuously to prevent stale detection
      const updateInterval = setInterval(async () => {
        try {
          await fs.utimes(lockFile, new Date(), new Date());
        } catch {
          // Ignore errors
        }
      }, 50);

      try {
        await expect(shortTimeoutWriter.writeAtomic(testFile, 'Test'))
          .rejects.toThrow(/Failed to acquire lock.*within timeout/);
      } finally {
        clearInterval(updateInterval);
        try {
          await fs.unlink(lockFile);
        } catch {
          // Ignore cleanup errors
        }
      }
    }, 10000); // Increase test timeout
  });

  describe('Checksum Calculation', () => {
    test('should calculate consistent checksums', async () => {
      const content = 'Test content for checksum';

      const result1 = await writer.writeAtomic(testFile, content);

      // Delete and recreate with same content
      await fs.unlink(testFile);
      const result2 = await writer.writeAtomic(testFile, content);

      // Checksums should be identical
      expect(result1.checksum).toBe(result2.checksum);
    });

    test('should have different checksums for different content', async () => {
      const result1 = await writer.writeAtomic(testFile, 'Content 1');

      await fs.unlink(testFile);
      const result2 = await writer.writeAtomic(testFile, 'Content 2');

      expect(result1.checksum).not.toBe(result2.checksum);
    });
  });
});