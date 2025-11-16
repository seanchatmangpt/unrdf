/**
 * @fileoverview Browser Shims Tests
 * @module test/browser/browser-shims
 *
 * @description
 * Tests for browser compatibility shims that provide Node.js API polyfills
 * in browser environments. Validates environment detection, file system
 * abstraction, Worker polyfills, and crypto utilities.
 */

import { describe, it, expect, beforeEach, afterEach } from 'vitest';
import {
  isBrowser,
  isNode,
  randomUUID,
  path,
  process as shimProcess,
  fs,
  fsPromises,
  Worker as BrowserWorker,
  execSync,
  createHash
} from '../../src/knowledge-engine/browser-shims.mjs';

describe('Browser Shims', () => {
  describe('Environment Detection', () => {
    it('should detect Node.js environment', () => {
      expect(isNode).toBe(true);
    });

    it('should detect non-browser environment', () => {
      expect(isBrowser).toBe(false);
    });

    it('should provide consistent environment flags', () => {
      expect(isNode).not.toBe(isBrowser);
    });
  });

  describe('UUID Generation', () => {
    it('should generate valid UUIDs', () => {
      const uuid = randomUUID();

      const uuidRegex = /^[0-9a-f]{8}-[0-9a-f]{4}-4[0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}$/i;
      expect(uuid).toMatch(uuidRegex);
    });

    it('should generate unique UUIDs', () => {
      const uuid1 = randomUUID();
      const uuid2 = randomUUID();
      const uuid3 = randomUUID();

      expect(uuid1).not.toBe(uuid2);
      expect(uuid2).not.toBe(uuid3);
      expect(uuid1).not.toBe(uuid3);
    });

    it('should generate UUIDs with correct version', () => {
      const uuid = randomUUID();
      const version = uuid.split('-')[2][0];

      expect(version).toBe('4'); // UUID v4
    });

    it('should generate UUIDs with correct variant', () => {
      const uuid = randomUUID();
      const variant = uuid.split('-')[3][0];

      expect(['8', '9', 'a', 'b']).toContain(variant);
    });
  });

  describe('Path Utilities', () => {
    it('should join path segments', () => {
      expect(path.join('a', 'b', 'c')).toBe('a/b/c');
      expect(path.join('/a', 'b', 'c')).toBe('/a/b/c');
      expect(path.join('a/', '/b', 'c')).toBe('a/b/c');
    });

    it('should normalize multiple slashes', () => {
      expect(path.join('a//b///c')).toBe('a/b/c');
      expect(path.join('/a//b', 'c')).toBe('/a/b/c');
    });

    it('should handle empty segments', () => {
      expect(path.join('a', '', 'b')).toBe('a/b');
      expect(path.join('', 'a', 'b')).toBe('a/b');
    });

    it('should resolve paths', () => {
      expect(path.resolve('a', 'b')).toBe('a/b');
      expect(path.resolve('/a', 'b', 'c')).toBe('/a/b/c');
    });

    it('should get directory name', () => {
      expect(path.dirname('/a/b/c.txt')).toBe('/a/b');
      expect(path.dirname('/a/b/c')).toBe('/a/b');
      expect(path.dirname('a/b')).toBe('a');
    });

    it('should get base name', () => {
      expect(path.basename('/a/b/c.txt')).toBe('c.txt');
      expect(path.basename('/a/b/c')).toBe('c');
      expect(path.basename('file.txt')).toBe('file.txt');
    });

    it('should get extension name', () => {
      expect(path.extname('file.txt')).toBe('.txt');
      expect(path.extname('file.test.js')).toBe('.js');
      expect(path.extname('file')).toBe('');
      expect(path.extname('.gitignore')).toBe('.gitignore');
    });
  });

  describe('Process Utilities', () => {
    it('should provide cwd function', () => {
      const cwd = shimProcess.cwd();
      expect(cwd).toBeDefined();
      expect(typeof cwd).toBe('string');
    });

    it('should provide env object', () => {
      expect(shimProcess.env).toBeDefined();
      expect(typeof shimProcess.env).toBe('object');
    });

    it('should provide versions object', () => {
      expect(shimProcess.versions).toBeDefined();
      expect(typeof shimProcess.versions).toBe('object');
    });
  });

  describe('File System Shims', () => {
    describe('Sync Operations', () => {
      it('should write and read files', () => {
        const testPath = '/test-file.txt';
        const content = 'Hello World';

        fs.writeFileSync(testPath, content);
        const read = fs.readFileSync(testPath, 'utf8');

        expect(read).toBe(content);
      });

      it('should check file existence', () => {
        const testPath = '/test-exists.txt';

        expect(fs.existsSync(testPath)).toBe(false);

        fs.writeFileSync(testPath, 'content');

        expect(fs.existsSync(testPath)).toBe(true);
      });

      it('should create directories', () => {
        const dirPath = '/test-dir';

        fs.mkdirSync(dirPath);

        expect(fs.existsSync(dirPath)).toBe(true);
      });

      it('should list directory contents', () => {
        const dirPath = '/test-list';

        fs.mkdirSync(dirPath);
        fs.writeFileSync(`${dirPath}/file1.txt`, 'content1');
        fs.writeFileSync(`${dirPath}/file2.txt`, 'content2');

        const files = fs.readdirSync(dirPath);

        expect(files).toHaveLength(2);
        expect(files).toContain(`${dirPath}/file1.txt`);
        expect(files).toContain(`${dirPath}/file2.txt`);
      });

      it('should throw on reading non-existent file', () => {
        expect(() => {
          fs.readFileSync('/nonexistent.txt');
        }).toThrow(/ENOENT/);
      });

      it('should handle binary data', () => {
        const testPath = '/binary-file.bin';
        const data = 'test data';

        fs.writeFileSync(testPath, data, 'utf8');
        const buffer = fs.readFileSync(testPath, 'binary');

        expect(buffer).toBeDefined();
      });
    });

    describe('Async Operations', () => {
      it('should write and read files asynchronously', async () => {
        const testPath = '/async-test.txt';
        const content = 'Async content';

        await fsPromises.writeFile(testPath, content);
        const read = await fsPromises.readFile(testPath, 'utf8');

        expect(read).toBe(content);
      });

      it('should create directories asynchronously', async () => {
        const dirPath = '/async-dir';

        await fsPromises.mkdir(dirPath);

        expect(fs.existsSync(dirPath)).toBe(true);
      });

      it('should handle errors in async operations', async () => {
        await expect(
          fsPromises.readFile('/nonexistent-async.txt')
        ).rejects.toThrow(/ENOENT/);
      });
    });
  });

  describe('Worker Polyfill', () => {
    it('should create worker from source code', () => {
      const workerCode = `
        self.onmessage = (event) => {
          self.postMessage({ result: event.data.value * 2 });
        };
      `;

      const worker = new BrowserWorker(workerCode);

      expect(worker).toBeDefined();
      expect(worker.worker).toBeDefined();
    });

    it('should send and receive messages', (done) => {
      const workerCode = `
        self.onmessage = (event) => {
          self.postMessage({ result: event.data.value * 2 });
        };
      `;

      const worker = new BrowserWorker(workerCode);

      worker.on('message', (data) => {
        expect(data.result).toBe(84);
        worker.terminate();
        done();
      });

      worker.postMessage({ value: 42 });
    });

    it('should handle worker errors', (done) => {
      const workerCode = `
        self.onmessage = (event) => {
          throw new Error('Worker error');
        };
      `;

      const worker = new BrowserWorker(workerCode);

      worker.on('error', (error) => {
        expect(error).toBeDefined();
        worker.terminate();
        done();
      });

      worker.postMessage({ test: true });
    });

    it('should support once listener', (done) => {
      const workerCode = `
        self.onmessage = (event) => {
          self.postMessage({ count: 1 });
          self.postMessage({ count: 2 });
        };
      `;

      const worker = new BrowserWorker(workerCode);

      let callCount = 0;
      worker.once('message', () => {
        callCount++;
      });

      setTimeout(() => {
        expect(callCount).toBe(1);
        worker.terminate();
        done();
      }, 100);

      worker.postMessage({});
    });

    it('should remove listeners', () => {
      const workerCode = `
        self.onmessage = (event) => {
          self.postMessage({ test: true });
        };
      `;

      const worker = new BrowserWorker(workerCode);

      const handler = () => {};
      worker.on('message', handler);
      worker.removeListener('message', handler);

      expect(worker.messageHandlers).not.toContain(handler);
      worker.terminate();
    });

    it('should not send messages after termination', () => {
      const workerCode = `self.onmessage = () => {}`;
      const worker = new BrowserWorker(workerCode);

      worker.terminate();

      // Should not throw, but message won't be sent
      expect(() => {
        worker.postMessage({ test: true });
      }).not.toThrow();

      expect(worker.terminated).toBe(true);
    });
  });

  describe('ExecSync Shim', () => {
    it('should execute commands in Node.js', async () => {
      const result = await execSync('echo test');

      expect(result).toBeDefined();
      expect(typeof result).toBe('string');
    });

    it('should handle command options', async () => {
      const result = await execSync('echo test', { encoding: 'utf8' });

      expect(result).toBeDefined();
    });

    it('should warn in browser environment', async () => {
      // Would log warning but return empty string in browser
      // In Node.js, executes normally
      const result = await execSync('echo test');

      expect(result).toBeDefined();
    });
  });

  describe('Hash Utilities', () => {
    it('should create hash in Node.js', async () => {
      const hash = await createHash('sha256');

      expect(hash).toBeDefined();
      expect(hash.update).toBeDefined();
      expect(hash.digest).toBeDefined();
    });

    it('should compute SHA-256 hash', async () => {
      const hash = await createHash('sha256');
      hash.update('test data');
      const digest = await hash.digest('hex');

      expect(digest).toBeDefined();
      expect(typeof digest).toBe('string');
      expect(digest.length).toBe(64); // SHA-256 produces 64 hex characters
    });

    it('should produce consistent hashes', async () => {
      const hash1 = await createHash('sha256');
      hash1.update('test data');
      const digest1 = await hash1.digest('hex');

      const hash2 = await createHash('sha256');
      hash2.update('test data');
      const digest2 = await hash2.digest('hex');

      expect(digest1).toBe(digest2);
    });

    it('should support different algorithms', async () => {
      const algorithms = ['sha256', 'sha384', 'sha512'];

      for (const algo of algorithms) {
        const hash = await createHash(algo);
        hash.update('test');
        const digest = await hash.digest('hex');

        expect(digest).toBeDefined();
        expect(digest.length).toBeGreaterThan(0);
      }
    });
  });

  describe('Cross-Environment Compatibility', () => {
    it('should work in Node.js environment', () => {
      expect(isNode).toBe(true);

      // All shims should work
      expect(() => randomUUID()).not.toThrow();
      expect(() => path.join('a', 'b')).not.toThrow();
      expect(() => fs.existsSync('/test')).not.toThrow();
    });

    it('should provide consistent API across environments', () => {
      const apis = {
        randomUUID,
        path,
        process: shimProcess,
        fs,
        fsPromises,
        BrowserWorker,
        execSync,
        createHash
      };

      for (const [name, api] of Object.entries(apis)) {
        expect(api, `${name} should be defined`).toBeDefined();
      }
    });

    it('should handle graceful degradation', () => {
      // All operations should work or fail gracefully
      expect(() => randomUUID()).not.toThrow();
      expect(() => path.join('a', 'b')).not.toThrow();
      expect(() => shimProcess.cwd()).not.toThrow();
    });
  });

  describe('Edge Cases', () => {
    it('should handle empty path segments', () => {
      expect(path.join()).toBe('');
      expect(path.join('', '')).toBe('');
    });

    it('should handle root paths', () => {
      expect(path.join('/')).toBe('/');
      expect(path.dirname('/')).toBe('.');
    });

    it('should handle files without extensions', () => {
      expect(path.extname('README')).toBe('');
      expect(path.extname('/path/to/file')).toBe('');
    });

    it('should handle empty file content', () => {
      const testPath = '/empty-file.txt';

      fs.writeFileSync(testPath, '');
      const content = fs.readFileSync(testPath, 'utf8');

      expect(content).toBe('');
    });

    it('should handle large file content', () => {
      const testPath = '/large-file.txt';
      const largeContent = 'x'.repeat(100000);

      fs.writeFileSync(testPath, largeContent);
      const content = fs.readFileSync(testPath, 'utf8');

      expect(content).toHaveLength(100000);
    });

    it('should handle special characters in paths', () => {
      const specialPaths = [
        '/file with spaces.txt',
        '/file-with-dashes.txt',
        '/file_with_underscores.txt',
        '/file.multiple.dots.txt'
      ];

      for (const testPath of specialPaths) {
        fs.writeFileSync(testPath, 'content');
        expect(fs.existsSync(testPath)).toBe(true);
      }
    });

    it('should handle unicode in file content', () => {
      const testPath = '/unicode-file.txt';
      const unicodeContent = 'Hello 世界 🌍';

      fs.writeFileSync(testPath, unicodeContent);
      const content = fs.readFileSync(testPath, 'utf8');

      expect(content).toBe(unicodeContent);
    });
  });

  describe('Performance', () => {
    it('should generate UUIDs quickly', () => {
      const start = Date.now();

      for (let i = 0; i < 1000; i++) {
        randomUUID();
      }

      const duration = Date.now() - start;

      expect(duration, 'Should generate 1000 UUIDs in under 100ms').toBeLessThan(100);
    });

    it('should perform path operations quickly', () => {
      const start = Date.now();

      for (let i = 0; i < 10000; i++) {
        path.join('a', 'b', 'c', 'd', 'e');
      }

      const duration = Date.now() - start;

      expect(duration, 'Should perform 10k path operations in under 50ms').toBeLessThan(50);
    });

    it('should handle many file operations', () => {
      const start = Date.now();

      for (let i = 0; i < 100; i++) {
        fs.writeFileSync(`/perf-test-${i}.txt`, `content ${i}`);
      }

      const duration = Date.now() - start;

      expect(duration, 'Should write 100 files in under 200ms').toBeLessThan(200);
    });
  });
});
