/**
 * @fileoverview VFS packing utilities
 *
 * Collects files from filesystem into in-memory VFS with deterministic
 * ordering and configurable include/exclude patterns.
 *
 * @module @unrdf/kgc-cli/lib/latex/vfs/pack
 */

export {
  collectProjectFiles,
  listProjectFilesSorted,
  getVFSStats,
  filterVFSByExtension,
} from '../project-files.mjs';

import { collectProjectFiles as collectFiles } from '../project-files.mjs';

/**
 * Pack directory into VFS with stable ordering
 *
 * Recursively collects files from a directory into a Map<string, Uint8Array>
 * with VFS paths as keys. Files are collected in deterministic sorted order.
 *
 * @param {string} dirPath - Absolute path to directory
 * @param {Object} [options={}] - Packing options
 * @param {string[]} [options.include] - File extensions to include (default: .tex, .sty, .cls, .bib, .bst, images)
 * @param {string[]} [options.exclude] - Directory patterns to exclude (default: node_modules, .git, *.aux, *.log, *.pdf, .latex-cache)
 * @param {number} [options.maxFileSize] - Maximum file size in bytes (default: 10MB)
 * @returns {Promise<Map<string, Uint8Array>>} VFS map with stable ordering
 *
 * @example
 * const vfs = await packDirectory('/home/user/thesis', {
 *   include: ['.tex', '.sty', '.cls', '.bib', '.png'],
 *   exclude: ['build', 'node_modules', '.git']
 * });
 *
 * // vfs.get('work/main.tex') => Uint8Array([...])
 * // Files are in sorted order for deterministic hashing
 */
export async function packDirectory(dirPath, options = {}) {
  // Set defaults for LaTeX projects
  const defaultOptions = {
    include: options.include || [
      '.tex', '.sty', '.cls',      // LaTeX source
      '.bib', '.bst',               // Bibliography
      '.png', '.jpg', '.jpeg',      // Raster images
      '.svg', '.pdf',               // Vector graphics
    ],
    exclude: options.exclude || [
      'node_modules',
      '.git',
      '.kgc',
      '.latex-cache',
      'build',
      'dist',
      '.claude-flow',
    ],
    maxFileSize: options.maxFileSize || 10 * 1024 * 1024, // 10MB
    followSymlinks: options.followSymlinks || false,
  };

  return collectFiles(dirPath, defaultOptions);
}

/**
 * Pack directory with exclude patterns for auxiliary files
 *
 * Excludes common LaTeX auxiliary files (*.aux, *.log, *.toc, etc.)
 * in addition to standard exclusions.
 *
 * @param {string} dirPath - Absolute path to directory
 * @param {Object} [options={}] - Packing options
 * @returns {Promise<Map<string, Uint8Array>>} VFS map
 *
 * @example
 * const vfs = await packDirectoryClean('/home/user/thesis');
 * // Excludes: *.aux, *.log, *.toc, *.out, *.synctex.gz, etc.
 */
export async function packDirectoryClean(dirPath, options = {}) {
  const cleanOptions = {
    ...options,
    exclude: [
      ...(options.exclude || []),
      '*.aux',
      '*.log',
      '*.toc',
      '*.out',
      '*.synctex.gz',
      '*.fdb_latexmk',
      '*.fls',
      '*.blg',
      '*.bbl',
    ],
  };

  return packDirectory(dirPath, cleanOptions);
}
