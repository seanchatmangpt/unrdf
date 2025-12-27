/**
 * @fileoverview File I/O operations for paper generation
 *
 * @description
 * Provides async file operations for reading, writing, and managing
 * generated LaTeX papers and theses. All operations are Promise-based
 * and include proper error handling with descriptive messages.
 *
 * @module integration/file-io
 * @version 1.0.0
 * @license MIT
 */

import { promises as fs } from 'node:fs';
import { join, dirname, basename, extname, resolve } from 'node:path';
import { fileURLToPath } from 'node:url';
import { z } from 'zod';

const __dirname = dirname(fileURLToPath(import.meta.url));

/**
 * Default output directory for generated papers
 * @type {string}
 */
export const DEFAULT_OUTPUT_DIR = resolve(join(__dirname, '../../output'));

/**
 * Supported file extensions for paper output
 * @type {Set<string>}
 */
export const SUPPORTED_EXTENSIONS = new Set(['.tex', '.bib', '.cls', '.sty', '.txt']);

/**
 * File info schema for validation
 */
export const FileInfoSchema = z.object({
  name: z.string(),
  path: z.string(),
  size: z.number(),
  modified: z.date(),
  extension: z.string().optional(),
  isDirectory: z.boolean().optional()
});

/**
 * Write options schema
 */
export const WriteOptionsSchema = z
  .object({
    encoding: z.enum(['utf-8', 'utf8', 'ascii', 'binary']).default('utf-8'),
    mode: z.number().optional(),
    createDir: z.boolean().default(true),
    overwrite: z.boolean().default(true)
  })
  .default({});

/**
 * @typedef {Object} FileInfo
 * @property {string} name - File name without path
 * @property {string} path - Full absolute path
 * @property {number} size - File size in bytes
 * @property {Date} modified - Last modification date
 * @property {string} [extension] - File extension
 * @property {boolean} [isDirectory] - Whether entry is a directory
 */

/**
 * @typedef {Object} WriteOptions
 * @property {string} [encoding='utf-8'] - File encoding
 * @property {number} [mode] - File permissions mode
 * @property {boolean} [createDir=true] - Create parent directories if missing
 * @property {boolean} [overwrite=true] - Overwrite existing file
 */

/**
 * Ensure output directory exists, creating it if necessary
 *
 * @param {string} dir - Directory path to ensure
 * @returns {Promise<void>}
 * @throws {Error} If directory creation fails
 *
 * @example
 * await ensureOutputDir('/path/to/output');
 */
export async function ensureOutputDir(dir) {
  const resolvedDir = resolve(dir);

  try {
    await fs.mkdir(resolvedDir, { recursive: true });
  } catch (error) {
    if (error.code !== 'EEXIST') {
      throw new Error(`Failed to create output directory '${resolvedDir}': ${error.message}`, {
        cause: error
      });
    }
  }
}

/**
 * Write paper content to disk
 *
 * @param {string} path - Output file path
 * @param {string} content - LaTeX content to write
 * @param {WriteOptions} [options] - Write options
 * @returns {Promise<void>}
 * @throws {Error} If write fails or file exists with overwrite=false
 *
 * @example
 * await writePaper('/output/paper.tex', latexContent);
 * await writePaper('/output/paper.tex', content, { overwrite: false });
 */
export async function writePaper(path, content, options = {}) {
  const opts = WriteOptionsSchema.parse(options);
  const resolvedPath = resolve(path);

  // Validate extension
  const ext = extname(resolvedPath).toLowerCase();
  if (ext && !SUPPORTED_EXTENSIONS.has(ext)) {
    console.warn(`Warning: Writing file with unsupported extension '${ext}'`);
  }

  // Check for existing file if overwrite is disabled
  if (!opts.overwrite) {
    try {
      await fs.access(resolvedPath);
      throw new Error(`File already exists: ${resolvedPath}. Set overwrite=true to replace.`);
    } catch (error) {
      if (error.code !== 'ENOENT') {
        throw error;
      }
      // File doesn't exist, proceed with write
    }
  }

  // Ensure parent directory exists
  if (opts.createDir) {
    await ensureOutputDir(dirname(resolvedPath));
  }

  try {
    await fs.writeFile(resolvedPath, content, {
      encoding: opts.encoding,
      mode: opts.mode
    });
  } catch (error) {
    throw new Error(`Failed to write paper to '${resolvedPath}': ${error.message}`, {
      cause: error
    });
  }
}

/**
 * Read paper content from disk
 *
 * @param {string} path - File path to read
 * @param {string} [encoding='utf-8'] - File encoding
 * @returns {Promise<string>} File content
 * @throws {Error} If file doesn't exist or read fails
 *
 * @example
 * const content = await readPaper('/output/paper.tex');
 */
export async function readPaper(path, encoding = 'utf-8') {
  const resolvedPath = resolve(path);

  try {
    const content = await fs.readFile(resolvedPath, { encoding });
    return content;
  } catch (error) {
    if (error.code === 'ENOENT') {
      throw new Error(`Paper not found: ${resolvedPath}`);
    }
    if (error.code === 'EACCES') {
      throw new Error(`Permission denied reading paper: ${resolvedPath}`);
    }
    throw new Error(`Failed to read paper from '${resolvedPath}': ${error.message}`, {
      cause: error
    });
  }
}

/**
 * List all generated papers in a directory
 *
 * @param {string} [dir] - Directory to list (defaults to DEFAULT_OUTPUT_DIR)
 * @param {Object} [options] - List options
 * @param {string} [options.extension] - Filter by extension (e.g., '.tex')
 * @param {boolean} [options.recursive=false] - Include subdirectories
 * @returns {Promise<FileInfo[]>} Array of file info objects
 *
 * @example
 * const papers = await listPapers();
 * const texFiles = await listPapers('/output', { extension: '.tex' });
 */
export async function listPapers(dir = DEFAULT_OUTPUT_DIR, options = {}) {
  const resolvedDir = resolve(dir);
  const { extension, recursive = false } = options;

  try {
    await fs.access(resolvedDir);
  } catch (error) {
    if (error.code === 'ENOENT') {
      return []; // Directory doesn't exist, return empty array
    }
    throw new Error(`Cannot access directory '${resolvedDir}': ${error.message}`, {
      cause: error
    });
  }

  /**
   * Recursively collect files from a directory
   * @param {string} currentDir - Current directory path
   * @returns {Promise<FileInfo[]>} Files found
   */
  async function collectFiles(currentDir) {
    const entries = await fs.readdir(currentDir, { withFileTypes: true });
    const files = [];

    for (const entry of entries) {
      const fullPath = join(currentDir, entry.name);

      if (entry.isDirectory()) {
        if (recursive) {
          const subFiles = await collectFiles(fullPath);
          files.push(...subFiles);
        }
      } else if (entry.isFile()) {
        const ext = extname(entry.name).toLowerCase();

        // Filter by extension if specified
        if (extension && ext !== extension.toLowerCase()) {
          continue;
        }

        // Skip non-paper files unless no extension filter
        if (!extension && !SUPPORTED_EXTENSIONS.has(ext)) {
          continue;
        }

        try {
          const stats = await fs.stat(fullPath);
          files.push({
            name: entry.name,
            path: fullPath,
            size: stats.size,
            modified: stats.mtime,
            extension: ext,
            isDirectory: false
          });
        } catch {
          // Skip files we can't stat
          continue;
        }
      }
    }

    return files;
  }

  const files = await collectFiles(resolvedDir);

  // Sort by modification date (newest first)
  files.sort((a, b) => b.modified.getTime() - a.modified.getTime());

  return files;
}

/**
 * Delete a generated paper
 *
 * @param {string} path - File path to delete
 * @returns {Promise<void>}
 * @throws {Error} If deletion fails
 *
 * @example
 * await deletePaper('/output/old-paper.tex');
 */
export async function deletePaper(path) {
  const resolvedPath = resolve(path);

  try {
    await fs.unlink(resolvedPath);
  } catch (error) {
    if (error.code === 'ENOENT') {
      throw new Error(`Paper not found: ${resolvedPath}`);
    }
    if (error.code === 'EACCES') {
      throw new Error(`Permission denied deleting paper: ${resolvedPath}`);
    }
    throw new Error(`Failed to delete paper '${resolvedPath}': ${error.message}`, {
      cause: error
    });
  }
}

/**
 * Check if a paper exists
 *
 * @param {string} path - File path to check
 * @returns {Promise<boolean>} True if file exists
 *
 * @example
 * if (await paperExists('/output/paper.tex')) {
 *   console.log('Paper already generated');
 * }
 */
export async function paperExists(path) {
  const resolvedPath = resolve(path);

  try {
    await fs.access(resolvedPath);
    return true;
  } catch {
    return false;
  }
}

/**
 * Get detailed information about a paper file
 *
 * @param {string} path - File path
 * @returns {Promise<FileInfo>} File information
 * @throws {Error} If file doesn't exist
 *
 * @example
 * const info = await getPaperInfo('/output/paper.tex');
 * console.log(`Size: ${info.size} bytes, Modified: ${info.modified}`);
 */
export async function getPaperInfo(path) {
  const resolvedPath = resolve(path);

  try {
    const stats = await fs.stat(resolvedPath);
    return {
      name: basename(resolvedPath),
      path: resolvedPath,
      size: stats.size,
      modified: stats.mtime,
      extension: extname(resolvedPath).toLowerCase(),
      isDirectory: stats.isDirectory()
    };
  } catch (error) {
    if (error.code === 'ENOENT') {
      throw new Error(`Paper not found: ${resolvedPath}`);
    }
    throw new Error(`Failed to get paper info for '${resolvedPath}': ${error.message}`, {
      cause: error
    });
  }
}

/**
 * Copy a paper to a new location
 *
 * @param {string} source - Source file path
 * @param {string} destination - Destination file path
 * @param {Object} [options] - Copy options
 * @param {boolean} [options.overwrite=false] - Overwrite existing file
 * @returns {Promise<void>}
 * @throws {Error} If copy fails
 *
 * @example
 * await copyPaper('/output/draft.tex', '/output/final.tex');
 */
export async function copyPaper(source, destination, options = {}) {
  const { overwrite = false } = options;
  const resolvedSource = resolve(source);
  const resolvedDest = resolve(destination);

  // Check source exists
  try {
    await fs.access(resolvedSource);
  } catch (error) {
    throw new Error(`Source paper not found: ${resolvedSource}`);
  }

  // Check destination if not overwriting
  if (!overwrite) {
    try {
      await fs.access(resolvedDest);
      throw new Error(`Destination already exists: ${resolvedDest}. Set overwrite=true to replace.`);
    } catch (error) {
      if (error.code !== 'ENOENT' && !error.message.includes('already exists')) {
        throw error;
      }
    }
  }

  // Ensure destination directory exists
  await ensureOutputDir(dirname(resolvedDest));

  try {
    await fs.copyFile(resolvedSource, resolvedDest);
  } catch (error) {
    throw new Error(`Failed to copy paper from '${resolvedSource}' to '${resolvedDest}': ${error.message}`, {
      cause: error
    });
  }
}

/**
 * Move/rename a paper
 *
 * @param {string} source - Source file path
 * @param {string} destination - Destination file path
 * @param {Object} [options] - Move options
 * @param {boolean} [options.overwrite=false] - Overwrite existing file
 * @returns {Promise<void>}
 * @throws {Error} If move fails
 *
 * @example
 * await movePaper('/output/draft.tex', '/output/final.tex');
 */
export async function movePaper(source, destination, options = {}) {
  const { overwrite = false } = options;
  const resolvedSource = resolve(source);
  const resolvedDest = resolve(destination);

  // Check destination if not overwriting
  if (!overwrite) {
    try {
      await fs.access(resolvedDest);
      throw new Error(`Destination already exists: ${resolvedDest}. Set overwrite=true to replace.`);
    } catch (error) {
      if (error.code !== 'ENOENT' && !error.message.includes('already exists')) {
        throw error;
      }
    }
  }

  // Ensure destination directory exists
  await ensureOutputDir(dirname(resolvedDest));

  try {
    await fs.rename(resolvedSource, resolvedDest);
  } catch (error) {
    // If cross-device link error, fallback to copy + delete
    if (error.code === 'EXDEV') {
      await copyPaper(resolvedSource, resolvedDest, { overwrite });
      await deletePaper(resolvedSource);
      return;
    }

    throw new Error(`Failed to move paper from '${resolvedSource}' to '${resolvedDest}': ${error.message}`, {
      cause: error
    });
  }
}

/**
 * Get output directory statistics
 *
 * @param {string} [dir] - Directory to analyze (defaults to DEFAULT_OUTPUT_DIR)
 * @returns {Promise<Object>} Directory statistics
 *
 * @example
 * const stats = await getOutputStats();
 * console.log(`Total papers: ${stats.fileCount}, Total size: ${stats.totalSize} bytes`);
 */
export async function getOutputStats(dir = DEFAULT_OUTPUT_DIR) {
  const papers = await listPapers(dir, { recursive: true });

  const byExtension = {};
  let totalSize = 0;
  let oldestFile = null;
  let newestFile = null;

  for (const paper of papers) {
    totalSize += paper.size;

    const ext = paper.extension || 'unknown';
    byExtension[ext] = (byExtension[ext] || 0) + 1;

    if (!oldestFile || paper.modified < oldestFile.modified) {
      oldestFile = paper;
    }
    if (!newestFile || paper.modified > newestFile.modified) {
      newestFile = paper;
    }
  }

  return {
    directory: resolve(dir),
    fileCount: papers.length,
    totalSize,
    totalSizeFormatted: formatFileSize(totalSize),
    byExtension,
    oldestFile: oldestFile
      ? { name: oldestFile.name, modified: oldestFile.modified }
      : null,
    newestFile: newestFile
      ? { name: newestFile.name, modified: newestFile.modified }
      : null
  };
}

/**
 * Format file size to human-readable string
 *
 * @param {number} bytes - Size in bytes
 * @returns {string} Formatted size (e.g., '1.5 MB')
 */
export function formatFileSize(bytes) {
  if (bytes === 0) return '0 B';

  const units = ['B', 'KB', 'MB', 'GB', 'TB'];
  const k = 1024;
  const i = Math.floor(Math.log(bytes) / Math.log(k));

  return parseFloat((bytes / Math.pow(k, i)).toFixed(2)) + ' ' + units[i];
}

/**
 * Clean output directory (remove all generated files)
 *
 * @param {string} [dir] - Directory to clean (defaults to DEFAULT_OUTPUT_DIR)
 * @param {Object} [options] - Clean options
 * @param {string} [options.extension] - Only delete files with this extension
 * @param {Date} [options.olderThan] - Only delete files older than this date
 * @returns {Promise<number>} Number of files deleted
 *
 * @example
 * const count = await cleanOutputDir();
 * console.log(`Deleted ${count} files`);
 */
export async function cleanOutputDir(dir = DEFAULT_OUTPUT_DIR, options = {}) {
  const { extension, olderThan } = options;

  const papers = await listPapers(dir, { extension, recursive: true });
  let deletedCount = 0;

  for (const paper of papers) {
    // Skip if olderThan specified and file is newer
    if (olderThan && paper.modified >= olderThan) {
      continue;
    }

    try {
      await deletePaper(paper.path);
      deletedCount++;
    } catch {
      // Continue with other files if one fails
      console.warn(`Warning: Could not delete ${paper.path}`);
    }
  }

  return deletedCount;
}
