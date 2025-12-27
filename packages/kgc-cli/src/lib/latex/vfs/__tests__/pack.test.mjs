/**
 * @fileoverview Tests for VFS packing utilities
 */

import { describe, it, before, after } from 'node:test';
import assert from 'node:assert/strict';
import { mkdir, writeFile, rm } from 'node:fs/promises';
import { join } from 'node:path';
import { tmpdir } from 'node:os';
import { packDirectory, packDirectoryClean } from '../pack.mjs';
import { hashVfs } from '../hash.mjs';

describe('VFS Packing', () => {
  let testDir;

  before(async () => {
    // Create temporary test directory
    testDir = join(tmpdir(), `vfs-pack-test-${Date.now()}`);
    await mkdir(testDir, { recursive: true });

    // Create test files
    await writeFile(join(testDir, 'main.tex'), '\\documentclass{article}');
    await writeFile(join(testDir, 'preamble.tex'), '\\usepackage{amsmath}');
    await writeFile(join(testDir, 'style.sty'), '% Custom style');

    // Create subdirectory
    await mkdir(join(testDir, 'chapters'), { recursive: true });
    await writeFile(join(testDir, 'chapters', 'ch1.tex'), '\\chapter{One}');
    await writeFile(join(testDir, 'chapters', 'ch2.tex'), '\\chapter{Two}');

    // Create files that should be excluded
    await writeFile(join(testDir, 'main.aux'), 'auxiliary');
    await writeFile(join(testDir, 'main.log'), 'log file');

    // Create node_modules (should be excluded)
    await mkdir(join(testDir, 'node_modules'), { recursive: true });
    await writeFile(join(testDir, 'node_modules', 'test.js'), 'module');
  });

  after(async () => {
    // Clean up
    await rm(testDir, { recursive: true, force: true });
  });

  describe('packDirectory', () => {
    it('should pack directory with default options', async () => {
      const vfs = await packDirectory(testDir);

      assert.equal(vfs instanceof Map, true);
      assert.equal(vfs.size > 0, true);

      // Should include .tex and .sty files
      const paths = [...vfs.keys()];
      assert.equal(
        paths.some(p => p.includes('main.tex')),
        true
      );
      assert.equal(
        paths.some(p => p.includes('preamble.tex')),
        true
      );
      assert.equal(
        paths.some(p => p.includes('style.sty')),
        true
      );

      // Should exclude node_modules
      assert.equal(
        paths.some(p => p.includes('node_modules')),
        false
      );
    });

    it('should pack nested directories', async () => {
      const vfs = await packDirectory(testDir);
      const paths = [...vfs.keys()];

      assert.equal(
        paths.some(p => p.includes('chapters/ch1.tex')),
        true
      );
      assert.equal(
        paths.some(p => p.includes('chapters/ch2.tex')),
        true
      );
    });

    it('should respect include patterns', async () => {
      const vfs = await packDirectory(testDir, {
        include: ['.tex'], // Only .tex files
      });

      const paths = [...vfs.keys()];

      // Should include .tex
      assert.equal(
        paths.some(p => p.endsWith('.tex')),
        true
      );

      // Should NOT include .sty
      assert.equal(
        paths.some(p => p.endsWith('.sty')),
        false
      );
    });

    it('should respect exclude patterns', async () => {
      const vfs = await packDirectory(testDir, {
        exclude: ['node_modules', 'chapters'],
      });

      const paths = [...vfs.keys()];

      // Should exclude chapters directory
      assert.equal(
        paths.some(p => p.includes('chapters/')),
        false
      );
    });

    it('should produce stable ordering', async () => {
      const vfs1 = await packDirectory(testDir);
      const vfs2 = await packDirectory(testDir);

      // Same directory should produce same hash (deterministic)
      assert.equal(hashVfs(vfs1), hashVfs(vfs2));
    });

    it('should handle empty directory', async () => {
      const emptyDir = join(tmpdir(), `empty-${Date.now()}`);
      await mkdir(emptyDir, { recursive: true });

      const vfs = await packDirectory(emptyDir);
      assert.equal(vfs.size, 0);

      await rm(emptyDir, { recursive: true, force: true });
    });
  });

  describe('packDirectoryClean', () => {
    it('should exclude auxiliary files', async () => {
      const vfs = await packDirectoryClean(testDir);
      const paths = [...vfs.keys()];

      // Should NOT include .aux or .log
      assert.equal(
        paths.some(p => p.endsWith('.aux')),
        false
      );
      assert.equal(
        paths.some(p => p.endsWith('.log')),
        false
      );
    });

    it('should still include source files', async () => {
      const vfs = await packDirectoryClean(testDir);
      const paths = [...vfs.keys()];

      assert.equal(
        paths.some(p => p.includes('main.tex')),
        true
      );
    });
  });

  describe('Determinism Tests', () => {
    it('should produce identical hash for identical directory content', async () => {
      const vfs1 = await packDirectory(testDir);

      // Pack again
      const vfs2 = await packDirectory(testDir);

      const hash1 = hashVfs(vfs1);
      const hash2 = hashVfs(vfs2);

      assert.equal(hash1, hash2);
    });

    it('should detect when files change', async () => {
      const vfs1 = await packDirectory(testDir);
      const hash1 = hashVfs(vfs1);

      // Modify a file
      await writeFile(join(testDir, 'main.tex'), '\\documentclass{book}');

      const vfs2 = await packDirectory(testDir);
      const hash2 = hashVfs(vfs2);

      assert.notEqual(hash1, hash2);

      // Restore original
      await writeFile(join(testDir, 'main.tex'), '\\documentclass{article}');
    });

    it('should detect when files are added', async () => {
      const vfs1 = await packDirectory(testDir);
      const hash1 = hashVfs(vfs1);

      // Add new file
      await writeFile(join(testDir, 'new.tex'), '\\section{New}');

      const vfs2 = await packDirectory(testDir);
      const hash2 = hashVfs(vfs2);

      assert.notEqual(hash1, hash2);
      assert.equal(vfs2.size, vfs1.size + 1);

      // Clean up
      await rm(join(testDir, 'new.tex'));
    });
  });

  describe('Path Normalization in Packing', () => {
    it('should normalize all paths to VFS format', async () => {
      const vfs = await packDirectory(testDir);
      const paths = [...vfs.keys()];

      // All paths should start with 'work/'
      for (const path of paths) {
        assert.equal(path.startsWith('work/'), true);
        assert.equal(path.includes('\\'), false); // No backslashes
      }
    });

    it('should preserve directory structure in VFS paths', async () => {
      const vfs = await packDirectory(testDir);
      const paths = [...vfs.keys()];

      const ch1Path = paths.find(p => p.includes('ch1.tex'));
      assert.match(ch1Path, /work\/chapters\/ch1\.tex$/);
    });
  });
});
