/**
 * @fileoverview Test fixtures setup utilities
 *
 * Provides helper functions for LaTeX pipeline tests:
 * - Local HTTP server for mocking CTAN
 * - Temporary directory creation and cleanup
 * - Test fixture loading
 * - Common test utilities
 *
 * @module test/fixtures/setup
 */

import { createServer } from 'node:http';
import { promises as fs } from 'node:fs';
import { tmpdir } from 'node:os';
import { join } from 'node:path';

/**
 * Create a temporary directory for test isolation
 *
 * @param {string} [prefix='latex-test-'] - Directory name prefix
 * @returns {Promise<string>} - Absolute path to temp directory
 */
export async function createTempDir(prefix = 'latex-test-') {
  const randomSuffix = Math.random().toString(36).slice(2, 10);
  const tempPath = join(tmpdir(), `${prefix}${randomSuffix}`);
  await fs.mkdir(tempPath, { recursive: true });
  return tempPath;
}

/**
 * Cleanup temporary directory
 *
 * @param {string} dirPath - Directory to remove
 * @returns {Promise<void>}
 */
export async function cleanupTempDir(dirPath) {
  if (!dirPath) return;
  try {
    await fs.rm(dirPath, { recursive: true, force: true });
  } catch (err) {
    // Ignore cleanup errors
    console.warn(`Failed to cleanup ${dirPath}: ${err.message}`);
  }
}

/**
 * Create a mock CTAN HTTP server for testing package resolution
 *
 * @param {Object} options - Server options
 * @param {number} [options.port=0] - Port to listen on (0 = random)
 * @param {Map<string, Buffer>} [options.packages=new Map()] - Package name -> tar.gz content
 * @returns {Promise<{server: Server, port: number, url: string, close: Function}>}
 *
 * @example
 * const ctan = await createMockCTAN({
 *   packages: new Map([
 *     ['amsmath.tar.gz', Buffer.from(...)]
 *   ])
 * });
 * // Use ctan.url in tests
 * await ctan.close();
 */
export async function createMockCTAN(options = {}) {
  const { port = 0, packages = new Map() } = options;

  const server = createServer((req, res) => {
    // Extract package name from URL (e.g., /tex-archive/macros/latex/amsmath.tar.gz)
    const pkgName = req.url.split('/').pop();

    if (packages.has(pkgName)) {
      const content = packages.get(pkgName);
      res.writeHead(200, {
        'Content-Type': 'application/gzip',
        'Content-Length': content.length
      });
      res.end(content);
    } else {
      res.writeHead(404, { 'Content-Type': 'text/plain' });
      res.end('Package not found');
    }
  });

  return new Promise((resolve, reject) => {
    server.listen(port, (err) => {
      if (err) return reject(err);

      const actualPort = server.address().port;
      const url = `http://localhost:${actualPort}`;

      resolve({
        server,
        port: actualPort,
        url,
        close: () => new Promise((res, rej) => {
          server.close((err) => err ? rej(err) : res());
        })
      });
    });
  });
}

/**
 * Load a test fixture file
 *
 * @param {string} relativePath - Path relative to fixtures/latex/ directory
 * @returns {Promise<Buffer>} - File contents
 *
 * @example
 * const minimalTex = await loadFixture('minimal.tex');
 */
export async function loadFixture(relativePath) {
  const fixturesDir = join(import.meta.dirname, 'fixtures', 'latex');
  const fullPath = join(fixturesDir, relativePath);
  return fs.readFile(fullPath);
}

/**
 * Write a temporary LaTeX file for testing
 *
 * @param {string} content - LaTeX source code
 * @param {string} [filename='test.tex'] - Output filename
 * @param {string} [tempDir] - Optional temp directory (created if not provided)
 * @returns {Promise<{filePath: string, cleanup: Function}>}
 *
 * @example
 * const { filePath, cleanup } = await writeTexFile('\\documentclass{article}...');
 * // Use filePath in tests
 * await cleanup();
 */
export async function writeTexFile(content, filename = 'test.tex', tempDir = null) {
  const dir = tempDir || await createTempDir('tex-');
  const filePath = join(dir, filename);
  await fs.writeFile(filePath, content, 'utf8');

  return {
    filePath,
    cleanup: async () => {
      if (!tempDir) {
        // Only cleanup if we created the temp dir
        await cleanupTempDir(dir);
      }
    }
  };
}

/**
 * Validate PDF binary format
 *
 * @param {Uint8Array|Buffer} pdfBytes - PDF file bytes
 * @returns {Object} - Validation result with boolean flags
 *
 * @example
 * const validation = validatePDFFormat(pdfBytes);
 * expect(validation.valid).toBe(true);
 */
export function validatePDFFormat(pdfBytes) {
  if (!(pdfBytes instanceof Uint8Array || pdfBytes instanceof Buffer)) {
    return { valid: false, error: 'Not a Uint8Array or Buffer' };
  }

  // Check PDF magic bytes: %PDF-
  const header = new TextDecoder().decode(pdfBytes.slice(0, 5));
  const hasPDFMagic = header === '%PDF-';

  // Check minimum size (5KB = 5120 bytes for non-trivial content)
  const isLargeEnough = pdfBytes.length > 5120;

  // Check EOF marker exists (%%EOF near end)
  const tail = new TextDecoder().decode(pdfBytes.slice(-128));
  const hasEOF = tail.includes('%%EOF');

  return {
    hasPDFMagic,
    isLargeEnough,
    hasEOF,
    size: pdfBytes.length,
    valid: hasPDFMagic && isLargeEnough && hasEOF,
  };
}

/**
 * Wait for a condition to be true (polling with timeout)
 *
 * @param {Function} condition - Function returning boolean or Promise<boolean>
 * @param {Object} options - Options
 * @param {number} [options.timeout=5000] - Timeout in ms
 * @param {number} [options.interval=100] - Polling interval in ms
 * @returns {Promise<void>}
 *
 * @example
 * await waitFor(() => fs.existsSync('/tmp/output.pdf'), { timeout: 3000 });
 */
export async function waitFor(condition, options = {}) {
  const { timeout = 5000, interval = 100 } = options;
  const startTime = Date.now();

  while (true) {
    const result = await condition();
    if (result) return;

    if (Date.now() - startTime > timeout) {
      throw new Error(`Timeout waiting for condition after ${timeout}ms`);
    }

    await new Promise(resolve => setTimeout(resolve, interval));
  }
}

/**
 * Assert that a file exists
 *
 * @param {string} filePath - Path to check
 * @returns {Promise<void>}
 * @throws {Error} If file does not exist
 */
export async function assertFileExists(filePath) {
  try {
    await fs.access(filePath);
  } catch (err) {
    throw new Error(`File does not exist: ${filePath}`);
  }
}

/**
 * Count files matching a pattern in a directory
 *
 * @param {string} dirPath - Directory to search
 * @param {RegExp|string} pattern - Pattern to match (string or regex)
 * @returns {Promise<number>} - Count of matching files
 *
 * @example
 * const logCount = await countFiles(cacheDir, /\.log$/);
 */
export async function countFiles(dirPath, pattern) {
  try {
    const files = await fs.readdir(dirPath, { recursive: true });
    const regex = typeof pattern === 'string' ? new RegExp(pattern) : pattern;
    return files.filter(f => regex.test(f)).length;
  } catch (err) {
    return 0;
  }
}
