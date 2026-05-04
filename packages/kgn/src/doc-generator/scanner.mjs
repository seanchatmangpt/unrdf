/**
 * @fileoverview Workspace scanner for batch documentation generation
 * Scans packages and finds all documentable source files
 */

import { readdir, stat } from 'fs/promises';
import { join, relative } from 'path';
import { fileURLToPath } from 'url';

const __filename = fileURLToPath(import.meta.url);

/**
 * Scan workspace for source files
 * @param {string} rootDir - Workspace root directory
 * @param {Object} options - Scanner options
 * @returns {Promise<Array>} Array of file paths grouped by package
 */
export async function scanWorkspace(rootDir, options = {}) {
  const {
    packagesDir = 'packages',
    include = ['**/*.mjs', '**/*.js'],
    exclude = ['**/node_modules/**', '**/dist/**', '**/test/**', '**/*.test.*', '**/*.spec.*'],
  } = options;

  const packagesPath = join(rootDir, packagesDir);
  const packages = [];

  try {
    const packageDirs = await readdir(packagesPath);

    for (const pkgName of packageDirs) {
      const pkgPath = join(packagesPath, pkgName);
      const pkgStat = await stat(pkgPath);

      if (!pkgStat.isDirectory()) continue;

      // Check if package has a src directory
      const srcPath = join(pkgPath, 'src');
      let hasSrc = false;
      try {
        const srcStat = await stat(srcPath);
        hasSrc = srcStat.isDirectory();
      } catch {
        // No src directory
      }

      if (!hasSrc) continue;

      // Scan for source files
      const sourceFiles = await scanDirectory(srcPath, {
        include,
        exclude,
        rootDir,
      });

      if (sourceFiles.length > 0) {
        packages.push({
          name: pkgName,
          path: pkgPath,
          relativePath: relative(rootDir, pkgPath),
          srcPath,
          sourceFiles,
        });
      }
    }

    return packages;
  } catch (error) {
    console.error(`Failed to scan workspace: ${error.message}`);
    return [];
  }
}

/**
 * Recursively scan directory for source files
 * @param {string} dir - Directory to scan
 * @param {Object} options - Scan options
 * @returns {Promise<Array>} Array of file paths
 */
async function scanDirectory(dir, options) {
  const { include, exclude, rootDir } = options;
  const files = [];

  try {
    const entries = await readdir(dir, { withFileTypes: true });

    for (const entry of entries) {
      const fullPath = join(dir, entry.name);
      const relativePath = relative(rootDir, fullPath);

      // Check exclusions
      if (shouldExclude(relativePath, exclude)) {
        continue;
      }

      if (entry.isDirectory()) {
        // Recursively scan subdirectories
        const subFiles = await scanDirectory(fullPath, options);
        files.push(...subFiles);
      } else if (entry.isFile()) {
        // Check if file matches include patterns
        if (shouldInclude(entry.name, include)) {
          files.push(fullPath);
        }
      }
    }
  } catch (error) {
    console.error(`Failed to scan directory ${dir}: ${error.message}`);
  }

  return files;
}

/**
 * Check if path should be excluded
 * @param {string} path - Path to check
 * @param {Array<string>} patterns - Exclusion patterns
 * @returns {boolean} True if should exclude
 */
function shouldExclude(path, patterns) {
  return patterns.some(pattern => {
    const regex = patternToRegex(pattern);
    return regex.test(path);
  });
}

/**
 * Check if filename should be included
 * @param {string} filename - Filename to check
 * @param {Array<string>} patterns - Inclusion patterns
 * @returns {boolean} True if should include
 */
function shouldInclude(filename, patterns) {
  return patterns.some(pattern => {
    const regex = patternToRegex(pattern);
    return regex.test(filename);
  });
}

/**
 * Convert glob pattern to regex
 * @param {string} pattern - Glob pattern
 * @returns {RegExp} Regular expression
 */
function patternToRegex(pattern) {
  // Simple glob to regex conversion
  const regexPattern = pattern
    .replace(/\*\*/g, '.*')
    .replace(/\*/g, '[^/]*')
    .replace(/\./g, '\\.');

  return new RegExp(`^${regexPattern}$`);
}

/**
 * Group files by package and module
 * @param {Array} packages - Scanned packages
 * @returns {Object} Grouped file structure
 */
export function groupFilesByModule(packages) {
  const grouped = {};

  packages.forEach(pkg => {
    const modules = new Map();

    pkg.sourceFiles.forEach(filePath => {
      const relativePath = relative(pkg.srcPath, filePath);
      const parts = relativePath.split('/');

      // Group by top-level directory in src/
      const moduleName = parts.length > 1 ? parts[0] : 'root';

      if (!modules.has(moduleName)) {
        modules.set(moduleName, []);
      }

      modules.get(moduleName).push(filePath);
    });

    grouped[pkg.name] = {
      ...pkg,
      modules: Object.fromEntries(modules),
    };
  });

  return grouped;
}

export default { scanWorkspace, groupFilesByModule };
