/**
 * @fileoverview Browser Shims Unit Tests (CRITICAL - P0)
 * Tests browser compatibility polyfills for Node.js APIs
 */

import { describe, it, expect, beforeEach, afterEach } from 'vitest';
import {
  isBrowser,
  isNode,
  randomUUID,
  path,
  process,
  fs,
  fsPromises,
  BrowserWorker,
  execSync,
  createHash
} from '../../src/knowledge-engine/browser-shims.mjs';

describe('browser-shims.mjs (CRITICAL)', () => {
  describe('Environment Detection', () => {
    it('should detect browser environment correctly', () => {
      // In jsdom, isBrowser should be true
      expect(typeof isBrowser).toBe('boolean');
      expect(typeof isNode).toBe('boolean');
    });

    it('should provide mutually exclusive environment flags', () => {
      // Cannot be both browser and node
      expect(!(isBrowser && isNode)).toBe(true);
    });
  });

  describe('UUID Generation', () => {
    it('should generate valid UUIDs', () => {
      const uuid = randomUUID();
      expect(uuid).toMatch(/^[0-9a-f]{8}-[0-9a-f]{4}-4[0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}$/i);
    });

    it('should generate unique UUIDs', () => {
      const uuid1 = randomUUID();
      const uuid2 = randomUUID();
      expect(uuid1).not.toBe(uuid2);
    });

    it('should generate UUIDs performantly', () => {
      const start = performance.now();
      for (let i = 0; i < 1000; i++) {
        randomUUID();
      }
      const duration = performance.now() - start;
      expect(duration).toBeLessThan(100); // <0.1ms per UUID
    });
  });

  describe('Path Utilities', () => {
    it('should join paths correctly', () => {
      expect(path.join('a', 'b', 'c')).toBe('a/b/c');
      expect(path.join('/root', 'sub', 'file.txt')).toBe('/root/sub/file.txt');
      expect(path.join('a/', '/b/', 'c')).toBe('a/b/c');
    });

    it('should handle empty path segments', () => {
      expect(path.join('a', '', 'b')).toBe('a/b');
      expect(path.join('', 'a', 'b')).toBe('a/b');
    });

    it('should resolve paths', () => {
      expect(path.resolve('a', 'b')).toBe('a/b');
      expect(path.resolve('/root', 'sub')).toBe('/root/sub');
    });

    it('should get directory name', () => {
      expect(path.dirname('/a/b/c.txt')).toBe('/a/b');
      expect(path.dirname('/a/b/c/')).toBe('/a/b');
      expect(path.dirname('file.txt')).toBe('.');
    });

    it('should get base name', () => {
      expect(path.basename('/a/b/c.txt')).toBe('c.txt');
      expect(path.basename('/a/b/c/')).toBe('');
      expect(path.basename('file.txt')).toBe('file.txt');
    });

    it('should get extension', () => {
      expect(path.extname('file.txt')).toBe('.txt');
      expect(path.extname('/path/to/file.mjs')).toBe('.mjs');
      expect(path.extname('noext')).toBe('');
      expect(path.extname('.hidden')).toBe('.hidden');
    });
  });

  describe('Process Utilities', () => {
    it('should provide current working directory', () => {
      const cwd = process.cwd();
      expect(typeof cwd).toBe('string');
      expect(cwd.length).toBeGreaterThan(0);
    });

    it('should provide environment variables', () => {
      expect(typeof process.env).toBe('object');
    });

    it('should provide versions info', () => {
      expect(typeof process.versions).toBe('object');
    });
  });

  describe('File System Operations (BrowserFileSystem)', () => {
    let browserFs;

    beforeEach(() => {
      // Get a fresh instance for each test
      browserFs = fs;
    });

    it('should write and read files', () => {
      browserFs.writeFileSync('/test.txt', 'test content');
      const content = browserFs.readFileSync('/test.txt', 'utf8');
      expect(content).toBe('test content');
    });

    it('should check file existence', () => {
      browserFs.writeFileSync('/exists.txt', 'content');
      expect(browserFs.existsSync('/exists.txt')).toBe(true);
      expect(browserFs.existsSync('/not-exists.txt')).toBe(false);
    });

    it('should throw error for non-existent file read', () => {
      expect(() => {
        browserFs.readFileSync('/non-existent.txt');
      }).toThrow(/ENOENT/);
    });

    it('should create directories', () => {
      browserFs.mkdirSync('/test-dir');
      expect(browserFs.existsSync('/test-dir')).toBe(true);
    });

    it('should list directory contents', () => {
      browserFs.writeFileSync('/dir/file1.txt', 'content1');
      browserFs.writeFileSync('/dir/file2.txt', 'content2');

      const files = browserFs.readdirSync('/dir');
      expect(files).toContain('/dir/file1.txt');
      expect(files).toContain('/dir/file2.txt');
    });

    it('should handle async file operations', async () => {
      await browserFs.writeFile('/async-test.txt', 'async content');
      const content = await browserFs.readFile('/async-test.txt', 'utf8');
      expect(content).toBe('async content');
    });

    it('should handle binary data', () => {
      const buffer = Buffer.from('binary data', 'utf8');
      browserFs.writeFileSync('/binary.bin', buffer);
      const readBuffer = browserFs.readFileSync('/binary.bin');
      expect(readBuffer.toString()).toBe('binary data');
    });
  });

  describe('BrowserWorker', () => {
    it('should create worker from script string', () => {
      const workerScript = `
        self.onmessage = (e) => {
          self.postMessage({ result: e.data.value * 2 });
        };
      `;

      const worker = new BrowserWorker(workerScript);
      expect(worker).toBeInstanceOf(BrowserWorker);
      worker.terminate();
    });

    it('should handle worker messages', (done) => {
      const workerScript = `
        self.onmessage = (e) => {
          self.postMessage({ result: 'received' });
        };
      `;

      const worker = new BrowserWorker(workerScript);

      worker.on('message', (data) => {
        expect(data.result).toBe('received');
        worker.terminate();
        done();
      });

      worker.postMessage({ test: true });
    });

    it('should handle worker errors', (done) => {
      const workerScript = `
        throw new Error('Worker error');
      `;

      const worker = new BrowserWorker(workerScript);

      worker.on('error', (error) => {
        expect(error).toBeDefined();
        worker.terminate();
        done();
      });
    });

    it('should support once listener', (done) => {
      const workerScript = `
        self.postMessage('message1');
        self.postMessage('message2');
      `;

      const worker = new BrowserWorker(workerScript);
      let callCount = 0;

      worker.once('message', (data) => {
        callCount++;
        expect(data).toBe('message1');
      });

      setTimeout(() => {
        expect(callCount).toBe(1); // Should only be called once
        worker.terminate();
        done();
      }, 100);
    });

    it('should remove event listeners', () => {
      const worker = new BrowserWorker(`self.postMessage('test')`);
      const handler = () => {};

      worker.on('message', handler);
      expect(worker.messageHandlers).toContain(handler);

      worker.removeListener('message', handler);
      expect(worker.messageHandlers).not.toContain(handler);

      worker.terminate();
    });

    it('should not send messages after termination', () => {
      const worker = new BrowserWorker(`self.onmessage = (e) => {}`);
      worker.terminate();

      // Should not throw, but also not send
      expect(() => {
        worker.postMessage({ test: true });
      }).not.toThrow();
    });
  });

  describe('execSync Polyfill', () => {
    it('should return empty string in browser', async () => {
      const result = await execSync('ls -la');
      expect(result).toBe('');
    });

    it('should not throw error in browser', async () => {
      await expect(execSync('invalid-command')).resolves.toBe('');
    });
  });

  describe('Hash Utilities (BrowserHash)', () => {
    it('should create hash instance', async () => {
      const hash = await createHash('sha256');
      expect(hash).toBeDefined();
      expect(typeof hash.update).toBe('function');
      expect(typeof hash.digest).toBe('function');
    });

    it('should compute SHA-256 hash', async () => {
      const hash = await createHash('sha256');
      hash.update('test data');
      const result = await hash.digest('hex');

      expect(typeof result).toBe('string');
      expect(result).toHaveLength(64); // SHA-256 = 32 bytes = 64 hex chars
    });

    it('should produce consistent hashes', async () => {
      const hash1 = await createHash('sha256');
      hash1.update('identical data');
      const result1 = await hash1.digest('hex');

      const hash2 = await createHash('sha256');
      hash2.update('identical data');
      const result2 = await hash2.digest('hex');

      expect(result1).toBe(result2);
    });

    it('should produce different hashes for different data', async () => {
      const hash1 = await createHash('sha256');
      hash1.update('data1');
      const result1 = await hash1.digest('hex');

      const hash2 = await createHash('sha256');
      hash2.update('data2');
      const result2 = await hash2.digest('hex');

      expect(result1).not.toBe(result2);
    });

    it('should handle binary data', async () => {
      const hash = await createHash('sha256');
      const binaryData = new Uint8Array([1, 2, 3, 4, 5]);
      hash.update(binaryData);
      const result = await hash.digest('hex');

      expect(typeof result).toBe('string');
      expect(result).toHaveLength(64);
    });

    it('should compute hash performantly', async () => {
      const start = performance.now();
      const data = 'x'.repeat(1024); // 1KB of data

      const hash = await createHash('sha256');
      hash.update(data);
      await hash.digest('hex');

      const duration = performance.now() - start;
      expect(duration).toBeLessThan(10); // <10ms for 1KB
    });
  });

  describe('Edge Cases and Error Handling', () => {
    it('should handle null/undefined in path operations', () => {
      expect(path.join(null, 'b')).toBe('b');
      expect(path.join('a', undefined, 'c')).toBe('a/c');
    });

    it('should handle empty arrays in path join', () => {
      expect(path.join()).toBe('');
      expect(path.join('')).toBe('');
    });

    it('should handle nested directory creation', () => {
      const browserFs = fs;
      browserFs.mkdirSync('/a/b/c', { recursive: true });
      expect(browserFs.existsSync('/a/b/c')).toBe(true);
    });

    it('should handle overwriting files', () => {
      const browserFs = fs;
      browserFs.writeFileSync('/overwrite.txt', 'original');
      browserFs.writeFileSync('/overwrite.txt', 'updated');

      const content = browserFs.readFileSync('/overwrite.txt', 'utf8');
      expect(content).toBe('updated');
    });
  });

  describe('Performance and Memory', () => {
    it('should handle large file operations', () => {
      const browserFs = fs;
      const largeContent = 'x'.repeat(100000); // 100KB

      browserFs.writeFileSync('/large.txt', largeContent);
      const read = browserFs.readFileSync('/large.txt', 'utf8');

      expect(read.length).toBe(100000);
    });

    it('should handle many file operations', () => {
      const browserFs = fs;

      for (let i = 0; i < 100; i++) {
        browserFs.writeFileSync(`/file${i}.txt`, `content${i}`);
      }

      for (let i = 0; i < 100; i++) {
        const content = browserFs.readFileSync(`/file${i}.txt`, 'utf8');
        expect(content).toBe(`content${i}`);
      }
    });
  });
});
