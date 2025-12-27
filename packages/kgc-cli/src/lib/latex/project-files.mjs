/**
 * @fileoverview Project file collection for VFS packing
 *
 * Collects LaTeX project files into an in-memory VFS (Virtual File System)
 * represented as Map<string, Uint8Array>. All paths are normalized to
 * forward-slash format with work/ prefix.
 *
 * @module @unrdf/kgc-cli/lib/latex/project-files
 */

import { readdir, readFile, stat } from 'node:fs/promises';
import { join, extname } from 'node:path';
import { normalizeToVFS, sortVFSPaths } from './path-normalize.mjs';

/**
 * Default file extensions to include in VFS
 * @type {string[]}
 */
const DEFAULT_INCLUDES = [
  '.tex', '.sty', '.cls',        // LaTeX source
  '.bib', '.bst',                 // Bibliography
  '.png', '.jpg', '.jpeg',        // Raster images
  '.pdf', '.svg',                 // Vector graphics
  '.md', '.txt',                  // Documentation
];

/**
 * Default directories to exclude from VFS
 * @type {string[]}
 */
const DEFAULT_EXCLUDES = [
  'node_modules',
  'target',
  'dist',
  'build',
  '.git',
  '.kgc',
  'thesis/graphs',                // Exclude generated graphs
  '.claude-flow',
  '.cache',
];

/**
 * Options for file collection
 * @typedef {Object} CollectOptions
 * @property {string[]} [include] - File extensions to include (default: DEFAULT_INCLUDES)
 * @property {string[]} [exclude] - Directory patterns to exclude (default: DEFAULT_EXCLUDES)
 * @property {number} [maxFileSize] - Maximum file size in bytes (default: 10MB)
 * @property {boolean} [followSymlinks] - Follow symbolic links (default: false)
 */

/**
 * Check if a path should be excluded
 *
 * @param {string} relPath - Relative path from project root
 * @param {string[]} excludePatterns - Exclude patterns
 * @returns {boolean} True if path should be excluded
 */
function shouldExclude(relPath, excludePatterns) {
  const normalized = relPath.split(/[/\\]/).join('/');

  return excludePatterns.some(pattern => {
    const patternNormalized = pattern.split(/[/\\]/).join('/');

    // Exact match or prefix match
    return normalized === patternNormalized ||
           normalized.startsWith(patternNormalized + '/') ||
           normalized.includes('/' + patternNormalized + '/') ||
           normalized.startsWith(patternNormalized);
  });
}

/**
 * Check if a file should be included based on extension
 *
 * @param {string} filePath - File path
 * @param {string[]} includeExtensions - Extensions to include
 * @returns {boolean} True if file should be included
 */
function shouldInclude(filePath, includeExtensions) {
  const ext = extname(filePath).toLowerCase();
  return includeExtensions.includes(ext);
}

/**
 * Recursively collect files from directory
 *
 * @param {string} dirPath - Directory path
 * @param {string} projectRoot - Project root directory
 * @param {CollectOptions} options - Collection options
 * @param {Map<string, string>} fileMap - Accumulated file map (VFS path -> absolute path)
 * @returns {Promise<void>}
 */
async function collectFilesRecursive(dirPath, projectRoot, options, fileMap) {
  const entries = await readdir(dirPath, { withFileTypes: true });

  for (const entry of entries) {
    const fullPath = join(dirPath, entry.name);
    const relPath = fullPath.replace(projectRoot, '').replace(/^[/\\]/, '');

    // Check exclusions
    if (shouldExclude(relPath, options.exclude)) {
      continue;
    }

    if (entry.isDirectory()) {
      await collectFilesRecursive(fullPath, projectRoot, options, fileMap);
    } else if (entry.isFile()) {
      if (shouldInclude(entry.name, options.include)) {
        // Check file size
        const stats = await stat(fullPath);
        if (stats.size > options.maxFileSize) {
          console.warn(`Skipping large file (${stats.size} bytes): ${relPath}`);
          continue;
        }

        const vfsPath = normalizeToVFS(fullPath, projectRoot);
        fileMap.set(vfsPath, fullPath);
      }
    } else if (entry.isSymbolicLink() && options.followSymlinks) {
      // Handle symlinks if enabled
      try {
        const stats = await stat(fullPath);
        if (stats.isFile() && shouldInclude(entry.name, options.include)) {
          if (stats.size <= options.maxFileSize) {
            const vfsPath = normalizeToVFS(fullPath, projectRoot);
            fileMap.set(vfsPath, fullPath);
          }
        } else if (stats.isDirectory()) {
          await collectFilesRecursive(fullPath, projectRoot, options, fileMap);
        }
      } catch (err) {
        console.warn(`Skipping broken symlink: ${relPath}`);
      }
    }
  }
}

/**
 * Collect project files into VFS map
 *
 * Recursively scans project directory and collects all matching files
 * into an in-memory Map with normalized VFS paths as keys and file
 * contents as Uint8Array values.
 *
 * @param {string} projectRoot - Absolute path to project root
 * @param {Partial<CollectOptions>} [options={}] - Collection options
 * @returns {Promise<Map<string, Uint8Array>>} VFS map (path -> binary content)
 *
 * @example
 * const vfs = await collectProjectFiles('/home/user/thesis', {
 *   include: ['.tex', '.sty'],
 *   exclude: ['build', 'node_modules']
 * });
 *
 * // vfs.get('work/main.tex') => Uint8Array([...])
 * // vfs.get('work/packages/foo.tex') => Uint8Array([...])
 */
export async function collectProjectFiles(projectRoot, options = {}) {
  const opts = {
    include: options.include || DEFAULT_INCLUDES,
    exclude: options.exclude || DEFAULT_EXCLUDES,
    maxFileSize: options.maxFileSize || 10 * 1024 * 1024, // 10MB default
    followSymlinks: options.followSymlinks || false,
  };

  // First pass: collect file paths (VFS path -> absolute path)
  const filePathMap = new Map();
  await collectFilesRecursive(projectRoot, projectRoot, opts, filePathMap);

  // Second pass: read files into binary format
  const vfs = new Map();

  // Sort paths for deterministic order
  const sortedPaths = sortVFSPaths([...filePathMap.keys()]);

  for (const vfsPath of sortedPaths) {
    const absPath = filePathMap.get(vfsPath);
    try {
      const content = await readFile(absPath);
      vfs.set(vfsPath, new Uint8Array(content));
    } catch (err) {
      console.warn(`Failed to read ${vfsPath}: ${err.message}`);
    }
  }

  return vfs;
}

/**
 * List all files in VFS in deterministic sorted order
 *
 * @param {Map<string, Uint8Array>} vfs - VFS map
 * @returns {string[]} Sorted array of VFS paths
 *
 * @example
 * const files = listProjectFilesSorted(vfs);
 * // => ['work/main.tex', 'work/preamble.tex', 'work/packages/index.tex']
 */
export function listProjectFilesSorted(vfs) {
  return sortVFSPaths([...vfs.keys()]);
}

/**
 * Get VFS statistics
 *
 * @param {Map<string, Uint8Array>} vfs - VFS map
 * @returns {Object} Statistics object
 * @property {number} fileCount - Total number of files
 * @property {number} totalBytes - Total size in bytes
 * @property {Object} byExtension - Count by file extension
 *
 * @example
 * const stats = getVFSStats(vfs);
 * // => { fileCount: 10, totalBytes: 52480, byExtension: { '.tex': 8, '.sty': 2 } }
 */
export function getVFSStats(vfs) {
  let totalBytes = 0;
  const byExtension = {};

  for (const [path, content] of vfs.entries()) {
    totalBytes += content.byteLength;

    const ext = extname(path).toLowerCase();
    byExtension[ext] = (byExtension[ext] || 0) + 1;
  }

  return {
    fileCount: vfs.size,
    totalBytes,
    byExtension,
  };
}

/**
 * Create a VFS subset with only specified extensions
 *
 * @param {Map<string, Uint8Array>} vfs - Source VFS map
 * @param {string[]} extensions - Extensions to include (e.g., ['.tex', '.sty'])
 * @returns {Map<string, Uint8Array>} Filtered VFS map
 */
export function filterVFSByExtension(vfs, extensions) {
  const filtered = new Map();
  const normalizedExts = extensions.map(ext => ext.toLowerCase());

  for (const [path, content] of vfs.entries()) {
    const ext = extname(path).toLowerCase();
    if (normalizedExts.includes(ext)) {
      filtered.set(path, content);
    }
  }

  return filtered;
}
