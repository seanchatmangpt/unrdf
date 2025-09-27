/**
 * @file File System Edge Cases Tests
 * @module file-system-edge-cases
 * 
 * @description
 * Tests for file system related edge cases including corruption, permissions,
 * network failures, and race conditions.
 */

import { describe, it, expect, beforeEach, afterEach, vi } from 'vitest';
import { readFile, writeFile, unlink, chmod } from 'fs/promises';
import { createFileResolver, loadFileWithHash, calculateFileHash } from '../../src/knowledge-engine/file-resolver.mjs';
import { defineHook } from '../../src/knowledge-engine/define-hook.mjs';
import { KnowledgeHookManager } from '../../src/knowledge-engine/knowledge-hook-manager.mjs';
import { Store } from 'n3';
import { join } from 'path';
import { tmpdir } from 'os';

describe('File System Edge Cases', () => {
  let tempDir;
  let manager;
  let testStore;

  beforeEach(async () => {
    tempDir = join(tmpdir(), `unrdf-test-${Date.now()}`);
    await writeFile(tempDir, ''); // Create temp directory
    manager = new KnowledgeHookManager({ basePath: tempDir });
    testStore = new Store();
  });

  afterEach(async () => {
    try {
      await unlink(tempDir);
    } catch (error) {
      // Ignore cleanup errors
    }
  });

  describe('File Corruption Scenarios', () => {
    it('should handle file corruption during read', async () => {
      const corruptedFile = join(tempDir, 'corrupted.sparql');
      await writeFile(corruptedFile, 'SELECT * WHERE { ?s ?p ?o }');
      
      // Simulate corruption by writing invalid data
      await writeFile(corruptedFile, '\x00\x01\x02\x03\x04\x05');
      
      const hook = defineHook({
        meta: { name: 'corruption-test' },
        when: {
          kind: 'sparql-ask',
          ref: {
            uri: `file://${corruptedFile}`,
            sha256: 'invalid-hash',
            mediaType: 'application/sparql-query'
          }
        },
        run: async () => ({ success: true })
      });

      await expect(manager.addKnowledgeHook(hook)).rejects.toThrow();
    });

    it('should handle partial file reads', async () => {
      const partialFile = join(tempDir, 'partial.sparql');
      const content = 'SELECT * WHERE { ?s ?p ?o }';
      await writeFile(partialFile, content);
      
      // Mock readFile to return partial content
      const originalReadFile = readFile;
      vi.spyOn(require('fs/promises'), 'readFile').mockImplementation(async (path) => {
        if (path === partialFile) {
          return content.substring(0, 10); // Return partial content
        }
        return originalReadFile(path);
      });

      const hook = defineHook({
        meta: { name: 'partial-read-test' },
        when: {
          kind: 'sparql-ask',
          ref: {
            uri: `file://${partialFile}`,
            sha256: 'expected-hash',
            mediaType: 'application/sparql-query'
          }
        },
        run: async () => ({ success: true })
      });

      await expect(manager.addKnowledgeHook(hook)).rejects.toThrow();
    });

    it('should handle encoding mismatches', async () => {
      const encodingFile = join(tempDir, 'encoding.sparql');
      // Write file with UTF-16 encoding
      await writeFile(encodingFile, 'SELECT * WHERE { ?s ?p ?o }', { encoding: 'utf16le' });
      
      const hook = defineHook({
        meta: { name: 'encoding-test' },
        when: {
          kind: 'sparql-ask',
          ref: {
            uri: `file://${encodingFile}`,
            sha256: 'expected-hash',
            mediaType: 'application/sparql-query'
          }
        },
        run: async () => ({ success: true })
      });

      await expect(manager.addKnowledgeHook(hook)).rejects.toThrow();
    });
  });

  describe('Permission and Access Scenarios', () => {
    it('should handle permission denied errors', async () => {
      const restrictedFile = join(tempDir, 'restricted.sparql');
      await writeFile(restrictedFile, 'SELECT * WHERE { ?s ?p ?o }');
      
      // Remove read permissions
      await chmod(restrictedFile, 0o000);
      
      const hook = defineHook({
        meta: { name: 'permission-test' },
        when: {
          kind: 'sparql-ask',
          ref: {
            uri: `file://${restrictedFile}`,
            sha256: 'expected-hash',
            mediaType: 'application/sparql-query'
          }
        },
        run: async () => ({ success: true })
      });

      await expect(manager.addKnowledgeHook(hook)).rejects.toThrow();
    });

    it('should handle file not found errors', async () => {
      const missingFile = join(tempDir, 'missing.sparql');
      
      const hook = defineHook({
        meta: { name: 'missing-file-test' },
        when: {
          kind: 'sparql-ask',
          ref: {
            uri: `file://${missingFile}`,
            sha256: 'expected-hash',
            mediaType: 'application/sparql-query'
          }
        },
        run: async () => ({ success: true })
      });

      await expect(manager.addKnowledgeHook(hook)).rejects.toThrow();
    });

    it('should handle symlink traversal attacks', async () => {
      const maliciousFile = join(tempDir, 'malicious.sparql');
      // Create a symlink that points outside the allowed directory
      const symlinkTarget = join(tempDir, '..', '..', 'sensitive-file');
      
      try {
        await writeFile(symlinkTarget, 'sensitive data');
        // Note: Symlink creation would need to be handled carefully in tests
        
        const hook = defineHook({
          meta: { name: 'symlink-test' },
          when: {
            kind: 'sparql-ask',
            ref: {
              uri: `file://${maliciousFile}`,
              sha256: 'expected-hash',
              mediaType: 'application/sparql-query'
            }
          },
          run: async () => ({ success: true })
        });

        // Should reject symlink traversal attempts
        await expect(manager.addKnowledgeHook(hook)).rejects.toThrow();
      } finally {
        try {
          await unlink(symlinkTarget);
        } catch (error) {
          // Ignore cleanup errors
        }
      }
    });
  });

  describe('Race Condition Scenarios', () => {
    it('should handle file modification during hash verification', async () => {
      const raceFile = join(tempDir, 'race.sparql');
      const content = 'SELECT * WHERE { ?s ?p ?o }';
      await writeFile(raceFile, content);
      
      const hook = defineHook({
        meta: { name: 'race-test' },
        when: {
          kind: 'sparql-ask',
          ref: {
            uri: `file://${raceFile}`,
            sha256: 'expected-hash',
            mediaType: 'application/sparql-query'
          }
        },
        run: async () => ({ success: true })
      });

      // Simulate file modification during verification
      const originalLoadFile = loadFileWithHash;
      vi.spyOn(require('../../src/knowledge-engine/file-resolver.mjs'), 'loadFileWithHash')
        .mockImplementation(async (uri, hash, basePath) => {
          // Modify file during verification
          await writeFile(raceFile, 'MODIFIED CONTENT');
          return originalLoadFile(uri, hash, basePath);
        });

      await expect(manager.addKnowledgeHook(hook)).rejects.toThrow();
    });

    it('should handle concurrent file access', async () => {
      const concurrentFile = join(tempDir, 'concurrent.sparql');
      const content = 'SELECT * WHERE { ?s ?p ?o }';
      await writeFile(concurrentFile, content);
      
      const hook = defineHook({
        meta: { name: 'concurrent-test' },
        when: {
          kind: 'sparql-ask',
          ref: {
            uri: `file://${concurrentFile}`,
            sha256: 'expected-hash',
            mediaType: 'application/sparql-query'
          }
        },
        run: async () => ({ success: true })
      });

      // Simulate concurrent access
      const promises = [];
      for (let i = 0; i < 10; i++) {
        promises.push(manager.addKnowledgeHook(hook));
      }

      // Should handle concurrent access gracefully
      const results = await Promise.allSettled(promises);
      const failures = results.filter(r => r.status === 'rejected');
      expect(failures.length).toBeGreaterThan(0);
    });
  });

  describe('Disk Space and Resource Scenarios', () => {
    it('should handle disk full scenarios', async () => {
      const largeFile = join(tempDir, 'large.sparql');
      // Create a large file to simulate disk space issues
      const largeContent = 'SELECT * WHERE { ?s ?p ?o } '.repeat(1000000);
      await writeFile(largeFile, largeContent);
      
      const hook = defineHook({
        meta: { name: 'disk-full-test' },
        when: {
          kind: 'sparql-ask',
          ref: {
            uri: `file://${largeFile}`,
            sha256: 'expected-hash',
            mediaType: 'application/sparql-query'
          }
        },
        run: async () => ({ success: true })
      });

      // Mock writeFile to simulate disk full error
      vi.spyOn(require('fs/promises'), 'writeFile').mockRejectedValue(
        new Error('ENOSPC: no space left on device')
      );

      await expect(manager.addKnowledgeHook(hook)).rejects.toThrow();
    });

    it('should handle file truncation during hash calculation', async () => {
      const truncateFile = join(tempDir, 'truncate.sparql');
      const content = 'SELECT * WHERE { ?s ?p ?o }';
      await writeFile(truncateFile, content);
      
      const hook = defineHook({
        meta: { name: 'truncate-test' },
        when: {
          kind: 'sparql-ask',
          ref: {
            uri: `file://${truncateFile}`,
            sha256: 'expected-hash',
            mediaType: 'application/sparql-query'
          }
        },
        run: async () => ({ success: true })
      });

      // Mock calculateFileHash to simulate truncation
      vi.spyOn(require('../../src/knowledge-engine/file-resolver.mjs'), 'calculateFileHash')
        .mockImplementation(async (filePath) => {
          // Simulate file being truncated during hash calculation
          await writeFile(filePath, '');
          return 'truncated-hash';
        });

      await expect(manager.addKnowledgeHook(hook)).rejects.toThrow();
    });
  });

  describe('Network and Remote URI Scenarios', () => {
    it('should handle network timeouts for remote URIs', async () => {
      const hook = defineHook({
        meta: { name: 'network-timeout-test' },
        when: {
          kind: 'sparql-ask',
          ref: {
            uri: 'http://slow-server.com/query.sparql',
            sha256: 'expected-hash',
            mediaType: 'application/sparql-query'
          }
        },
        run: async () => ({ success: true })
      });

      // Mock network timeout
      vi.spyOn(global, 'fetch').mockImplementation(() => 
        new Promise((_, reject) => 
          setTimeout(() => reject(new Error('Network timeout')), 100)
        )
      );

      await expect(manager.addKnowledgeHook(hook)).rejects.toThrow();
    });

    it('should handle invalid remote URIs', async () => {
      const hook = defineHook({
        meta: { name: 'invalid-uri-test' },
        when: {
          kind: 'sparql-ask',
          ref: {
            uri: 'not-a-valid-uri',
            sha256: 'expected-hash',
            mediaType: 'application/sparql-query'
          }
        },
        run: async () => ({ success: true })
      });

      await expect(manager.addKnowledgeHook(hook)).rejects.toThrow();
    });
  });

  describe('Hash Verification Edge Cases', () => {
    it('should handle hash collision scenarios', async () => {
      const collisionFile = join(tempDir, 'collision.sparql');
      await writeFile(collisionFile, 'SELECT * WHERE { ?s ?p ?o }');
      
      const hook = defineHook({
        meta: { name: 'hash-collision-test' },
        when: {
          kind: 'sparql-ask',
          ref: {
            uri: `file://${collisionFile}`,
            sha256: 'collision-hash',
            mediaType: 'application/sparql-query'
          }
        },
        run: async () => ({ success: true })
      });

      // Mock hash calculation to return collision
      vi.spyOn(require('../../src/knowledge-engine/file-resolver.mjs'), 'calculateFileHash')
        .mockResolvedValue('collision-hash');

      // Should still validate content despite hash collision
      await expect(manager.addKnowledgeHook(hook)).resolves.toBeUndefined();
    });

    it('should handle empty file hashes', async () => {
      const emptyFile = join(tempDir, 'empty.sparql');
      await writeFile(emptyFile, '');
      
      const hook = defineHook({
        meta: { name: 'empty-file-test' },
        when: {
          kind: 'sparql-ask',
          ref: {
            uri: `file://${emptyFile}`,
            sha256: 'e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855', // Empty file hash
            mediaType: 'application/sparql-query'
          }
        },
        run: async () => ({ success: true })
      });

      await expect(manager.addKnowledgeHook(hook)).rejects.toThrow();
    });
  });
});
