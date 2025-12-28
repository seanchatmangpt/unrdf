/**
 * @fileoverview Integration test - Complete VFS workflow
 *
 * Demonstrates full Agent 2 → Agent 3+ handoff
 */

import { describe, it, before, after } from 'node:test';
import assert from 'node:assert/strict';
import { mkdir, writeFile, rm } from 'node:fs/promises';
import { join } from 'node:path';
import { tmpdir } from 'node:os';

// Import from main vfs.mjs (backward compatibility)
import { collectProjectFiles, hashVfs } from '../index.mjs';

// Import from new structure (preferred)
import {
  packDirectory,
  getVfsText,
  setVfsText,
  mergeVfs,
  getVfsHashMetadata,
  areVfsEqual,
} from '../index.mjs';

describe('Integration: Agent 2 → Agent 3+ Handoff', () => {
  let testDir;

  before(async () => {
    testDir = join(tmpdir(), `vfs-integration-${Date.now()}`);
    await mkdir(testDir, { recursive: true });

    // Create minimal LaTeX project
    await writeFile(
      join(testDir, 'main.tex'),
      '\\documentclass{article}\n\\begin{document}\nHello World\n\\end{document}'
    );
    await writeFile(join(testDir, 'preamble.tex'), '\\usepackage{amsmath}');
    await mkdir(join(testDir, 'chapters'), { recursive: true });
    await writeFile(join(testDir, 'chapters', 'ch1.tex'), '\\chapter{One}');
  });

  after(async () => {
    await rm(testDir, { recursive: true, force: true });
  });

  it('should pack directory and produce stable hash', async () => {
    // Agent 2 deliverable: Pack directory
    const vfs = await packDirectory(testDir);

    // Verify VFS structure
    assert.equal(vfs instanceof Map, true);
    assert.equal(vfs.size, 3); // main.tex, preamble.tex, ch1.tex

    // Verify paths are normalized
    const paths = [...vfs.keys()];
    for (const path of paths) {
      assert.equal(path.startsWith('work/'), true);
    }

    // Agent 3 deliverable: Use hash as cache key
    const cacheKey = hashVfs(vfs);
    assert.equal(typeof cacheKey, 'string');
    assert.equal(cacheKey.length, 64); // SHA256 hex

    // Verify determinism: same directory = same hash
    const vfs2 = await packDirectory(testDir);
    const cacheKey2 = hashVfs(vfs2);
    assert.equal(cacheKey, cacheKey2);
  });

  it('should demonstrate Agent 4 workflow (dependency scanning)', async () => {
    const vfs = await packDirectory(testDir);

    // Read main.tex to find dependencies
    const mainTex = getVfsText(vfs, 'work/main.tex');
    assert.equal(typeof mainTex, 'string');
    assert.match(mainTex, /\\documentclass/);

    // Agent 4 would parse \usepackage{...} here
    const hasAmsmath = mainTex.includes('amsmath') ||
      getVfsText(vfs, 'work/preamble.tex')?.includes('amsmath');

    assert.equal(hasAmsmath, true);
  });

  it('should demonstrate metadata extraction', async () => {
    const vfs = await packDirectory(testDir);
    const metadata = getVfsHashMetadata(vfs);

    // Metadata includes everything needed for cache management
    assert.equal(metadata.fileCount, 3);
    assert.equal(metadata.totalBytes > 0, true);
    assert.equal(metadata.paths.length, 3);
    assert.equal(metadata.hash.length, 64);

    // Paths are sorted (depth-first, then alphabetical)
    assert.deepEqual(metadata.paths, [
      'work/main.tex',        // depth 1
      'work/preamble.tex',    // depth 1
      'work/chapters/ch1.tex', // depth 2
    ]);
  });

  it('should demonstrate VFS modification workflow', async () => {
    const vfs = await packDirectory(testDir);
    const originalHash = hashVfs(vfs);

    // Add a file to VFS (simulate code generation)
    setVfsText(vfs, 'work/generated.tex', '% Generated content');

    // Hash should change
    const newHash = hashVfs(vfs);
    assert.notEqual(originalHash, newHash);

    // Verify file exists
    const generated = getVfsText(vfs, 'work/generated.tex');
    assert.equal(generated, '% Generated content');
  });

  it('should demonstrate VFS merging', async () => {
    const baseVfs = await packDirectory(testDir);

    // Create overlay VFS with custom files
    const customVfs = new Map([
      ['work/custom.sty', new Uint8Array([1, 2, 3])],
    ]);

    // Merge: custom overrides base
    const merged = mergeVfs(baseVfs, customVfs);

    assert.equal(merged.size, baseVfs.size + 1);
    assert.equal(merged.has('work/custom.sty'), true);
  });

  it('should demonstrate backward compatibility', async () => {
    // Old API still works
    const vfs1 = await collectProjectFiles(testDir);

    // New API produces same result
    const vfs2 = await packDirectory(testDir);

    // Both produce same hash
    assert.equal(areVfsEqual(vfs1, vfs2), true);
  });

  it('should handle cache hit/miss scenario', async () => {
    // Simulate Agent 3 cache logic
    const cache = new Map(); // Simple in-memory cache

    // First compilation: cache miss
    const vfs1 = await packDirectory(testDir);
    const key1 = hashVfs(vfs1);

    if (!cache.has(key1)) {
      // Simulate compilation
      const pdf = new Uint8Array([0x25, 0x50, 0x44, 0x46]); // %PDF header
      cache.set(key1, pdf);
    }

    // Second compilation: cache hit (same directory)
    const vfs2 = await packDirectory(testDir);
    const key2 = hashVfs(vfs2);

    assert.equal(key1, key2); // Keys match
    assert.equal(cache.has(key2), true); // Cache hit!

    const cachedPdf = cache.get(key2);
    assert.equal(cachedPdf instanceof Uint8Array, true);
  });

  it('should detect file changes for cache invalidation', async () => {
    const vfs1 = await packDirectory(testDir);
    const hash1 = hashVfs(vfs1);

    // Modify a file
    await writeFile(join(testDir, 'main.tex'), '\\documentclass{book}');

    const vfs2 = await packDirectory(testDir);
    const hash2 = hashVfs(vfs2);

    // Hash changes → cache miss → recompile
    assert.notEqual(hash1, hash2);

    // Restore
    await writeFile(
      join(testDir, 'main.tex'),
      '\\documentclass{article}\n\\begin{document}\nHello World\n\\end{document}'
    );
  });
});
