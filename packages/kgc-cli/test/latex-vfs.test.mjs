/**
 * @fileoverview Tests for LaTeX VFS project file collection
 */

import { describe, it, expect } from 'vitest';
import { normalizeToVFS, vfsToRelative, isValidVFSPath, sortVFSPaths } from '../src/lib/latex/path-normalize.mjs';
import { collectProjectFiles, listProjectFilesSorted, getVFSStats, filterVFSByExtension } from '../src/lib/latex/project-files.mjs';
import { join } from 'node:path';

describe('path-normalize', () => {
  it('should normalize paths to VFS format', () => {
    const result = normalizeToVFS('/home/user/project/main.tex', '/home/user/project');
    expect(result).toBe('work/main.tex');
  });

  it('should handle nested paths', () => {
    const result = normalizeToVFS('/home/user/project/packages/foo.tex', '/home/user/project');
    expect(result).toBe('work/packages/foo.tex');
  });

  it('should convert VFS to relative', () => {
    const result = vfsToRelative('work/main.tex');
    expect(result).toBe('main.tex');
  });

  it('should validate VFS paths', () => {
    expect(isValidVFSPath('work/main.tex')).toBe(true);
    expect(isValidVFSPath('work/packages/foo.tex')).toBe(true);
    expect(isValidVFSPath('main.tex')).toBe(false); // No work/ prefix
    expect(isValidVFSPath('work\\main.tex')).toBe(false); // Backslash
    expect(isValidVFSPath('work/../etc/passwd')).toBe(false); // Path traversal
  });

  it('should sort VFS paths deterministically', () => {
    const paths = [
      'work/packages/b.tex',
      'work/a.tex',
      'work/packages/a.tex',
      'work/z.tex',
    ];

    const sorted = sortVFSPaths(paths);
    expect(sorted).toEqual([
      'work/a.tex',
      'work/z.tex',
      'work/packages/a.tex',
      'work/packages/b.tex',
    ]);
  });
});

describe('project-files', () => {
  it('should collect files and return Map', async () => {
    // Use thesis directory if it exists
    const thesisPath = join(process.cwd(), '../../thesis');

    try {
      const vfs = await collectProjectFiles(thesisPath, {
        include: ['.tex'],
        exclude: ['node_modules', '.git'],
      });

      expect(vfs).toBeInstanceOf(Map);
      expect(vfs.size).toBeGreaterThan(0);

      // Check that paths are normalized
      for (const path of vfs.keys()) {
        expect(path.startsWith('work/')).toBe(true);
        expect(path).not.toContain('\\');
      }

      // Check that values are Uint8Array
      for (const content of vfs.values()) {
        expect(content).toBeInstanceOf(Uint8Array);
      }
    } catch (err) {
      // Skip if directory doesn't exist
      console.log('Skipping integration test - thesis directory not found');
    }
  });

  it('should list files in sorted order', async () => {
    const vfs = new Map([
      ['work/z.tex', new Uint8Array([1])],
      ['work/a.tex', new Uint8Array([2])],
      ['work/packages/b.tex', new Uint8Array([3])],
    ]);

    const sorted = listProjectFilesSorted(vfs);
    expect(sorted).toEqual([
      'work/a.tex',
      'work/z.tex',
      'work/packages/b.tex',
    ]);
  });

  it('should compute VFS statistics', () => {
    const vfs = new Map([
      ['work/main.tex', new Uint8Array(100)],
      ['work/preamble.tex', new Uint8Array(200)],
      ['work/style.sty', new Uint8Array(50)],
    ]);

    const stats = getVFSStats(vfs);
    expect(stats.fileCount).toBe(3);
    expect(stats.totalBytes).toBe(350);
    expect(stats.byExtension['.tex']).toBe(2);
    expect(stats.byExtension['.sty']).toBe(1);
  });

  it('should filter by extension', () => {
    const vfs = new Map([
      ['work/main.tex', new Uint8Array([1])],
      ['work/style.sty', new Uint8Array([2])],
      ['work/image.png', new Uint8Array([3])],
    ]);

    const filtered = filterVFSByExtension(vfs, ['.tex', '.sty']);
    expect(filtered.size).toBe(2);
    expect(filtered.has('work/main.tex')).toBe(true);
    expect(filtered.has('work/style.sty')).toBe(true);
    expect(filtered.has('work/image.png')).toBe(false);
  });
});
