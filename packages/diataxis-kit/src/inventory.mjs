import { readdir, readFile, stat } from 'node:fs/promises';
import { join, resolve, dirname } from 'node:path';
import { existsSync } from 'node:fs';

/**
 * @typedef {Object} PackageEntry
 * @property {string} name - Package name
 * @property {string} dir - Absolute path to package directory
 * @property {string} version - Package version
 * @property {string} description - Package description
 * @property {Record<string, string>} exports - Package exports map
 * @property {Record<string, string> | string} bin - Binary executables
 * @property {string[]} keywords - Package keywords
 * @property {boolean} hasReadme - Whether README.md exists
 * @property {boolean} hasDocs - Whether docs/ directory exists
 * @property {boolean} hasExamples - Whether examples/ directory exists
 * @property {boolean} hasTests - Whether test/ directory exists
 * @property {Record<string, string>} scripts - Selected scripts (test, dev, build)
 */

/**
 * Discovers all workspace packages and returns structured metadata
 * @param {string} workspaceRoot - Path to monorepo root
 * @param {Object} [options={}] - Discovery options
 * @param {string[]} [options.skipDirs=['node_modules', '.git', 'dist', 'build']] - Directories to skip
 * @param {boolean} [options.includeNested=true] - Whether to discover nested workspaces
 * @returns {Promise<PackageEntry[]>} Array of package entries, sorted by name
 */
export async function discoverPackages(workspaceRoot, options = {}) {
  const {
    skipDirs = ['node_modules', '.git', 'dist', 'build'],
    includeNested = true
  } = options;

  const rootPath = resolve(workspaceRoot);

  // Detect workspace configuration
  const workspacePatterns = await detectWorkspacePatterns(rootPath);

  if (workspacePatterns.length === 0) {
    console.warn('No workspace configuration found');
  }

  // Find all package.json files
  const packagePaths = await findPackageJsonFiles(rootPath, skipDirs, includeNested);

  // Process each package
  const packages = [];
  for (const pkgPath of packagePaths) {
    try {
      const entry = await processPackage(pkgPath);
      if (entry) {
        packages.push(entry);
      }
    } catch (error) {
      console.warn(`Skipping package at ${pkgPath}: ${error.message}`);
    }
  }

  // Sort by package name for stable ordering (deterministic)
  packages.sort((a, b) => a.name.localeCompare(b.name));

  return packages;
}

/**
 * Validates a package entry structure
 * @param {any} entry - Entry to validate
 * @returns {{ valid: boolean, errors: string[] }} Validation result
 */
export function validatePackageEntry(entry) {
  const errors = [];

  if (!entry || typeof entry !== 'object') {
    return { valid: false, errors: ['Entry must be an object'] };
  }

  // Check required string fields
  const requiredStrings = ['name', 'dir', 'version', 'description'];
  for (const field of requiredStrings) {
    if (typeof entry[field] !== 'string') {
      errors.push(`Field '${field}' must be a string`);
    }
  }

  // Check exports (must be object)
  if (typeof entry.exports !== 'object' || entry.exports === null) {
    errors.push('Field \'exports\' must be an object');
  }

  // Check bin (can be object or string)
  if (entry.bin !== null && typeof entry.bin !== 'object' && typeof entry.bin !== 'string') {
    errors.push('Field \'bin\' must be an object, string, or null');
  }

  // Check keywords array
  if (!Array.isArray(entry.keywords)) {
    errors.push('Field \'keywords\' must be an array');
  }

  // Check boolean fields
  const booleanFields = ['hasReadme', 'hasDocs', 'hasExamples', 'hasTests'];
  for (const field of booleanFields) {
    if (typeof entry[field] !== 'boolean') {
      errors.push(`Field '${field}' must be a boolean`);
    }
  }

  // Check scripts (must be object)
  if (typeof entry.scripts !== 'object' || entry.scripts === null) {
    errors.push('Field \'scripts\' must be an object');
  }

  return {
    valid: errors.length === 0,
    errors
  };
}

/**
 * Detects workspace patterns from pnpm-workspace.yaml and package.json
 * @param {string} rootPath - Root directory path
 * @returns {Promise<string[]>} Array of workspace patterns
 */
async function detectWorkspacePatterns(rootPath) {
  const patterns = [];

  // Check pnpm-workspace.yaml
  const pnpmWorkspacePath = join(rootPath, 'pnpm-workspace.yaml');
  try {
    if (existsSync(pnpmWorkspacePath)) {
      const content = await readFile(pnpmWorkspacePath, 'utf-8');
      // Simple YAML parsing for packages array
      const lines = content.split('\n');
      let inPackages = false;
      for (const line of lines) {
        if (line.trim().startsWith('packages:')) {
          inPackages = true;
          continue;
        }
        if (inPackages && line.trim().startsWith('-')) {
          const pattern = line.trim().substring(1).trim().replace(/['"]/g, '');
          patterns.push(pattern);
        } else if (inPackages && line.trim() && !line.trim().startsWith('#')) {
          // End of packages section (non-empty, non-comment line)
          break;
        }
      }
    }
  } catch (error) {
    // Gracefully skip if unable to read
  }

  // Check package.json workspaces
  const pkgJsonPath = join(rootPath, 'package.json');
  try {
    if (existsSync(pkgJsonPath)) {
      const content = await readFile(pkgJsonPath, 'utf-8');
      const pkg = JSON.parse(content);
      if (pkg.workspaces) {
        if (Array.isArray(pkg.workspaces)) {
          patterns.push(...pkg.workspaces);
        } else if (pkg.workspaces.packages && Array.isArray(pkg.workspaces.packages)) {
          patterns.push(...pkg.workspaces.packages);
        }
      }
    }
  } catch (error) {
    // Gracefully skip if unable to read or parse
  }

  return patterns;
}

/**
 * Recursively finds all package.json files
 * @param {string} dir - Directory to search
 * @param {string[]} skipDirs - Directories to skip
 * @param {boolean} includeNested - Whether to include nested workspaces
 * @returns {Promise<string[]>} Array of package.json file paths
 */
async function findPackageJsonFiles(dir, skipDirs, includeNested) {
  const results = [];

  try {
    const entries = await readdir(dir, { withFileTypes: true });

    for (const entry of entries) {
      const fullPath = join(dir, entry.name);

      // Skip directories in skipDirs list
      if (skipDirs.includes(entry.name)) {
        continue;
      }

      if (entry.isDirectory()) {
        // Recursively search subdirectories
        const subResults = await findPackageJsonFiles(fullPath, skipDirs, includeNested);
        results.push(...subResults);
      } else if (entry.name === 'package.json') {
        // Found a package.json file
        results.push(fullPath);
      }
    }
  } catch (error) {
    // Gracefully ignore errors (e.g., permission denied)
  }

  return results;
}

/**
 * Processes a single package.json file and extracts metadata
 * @param {string} pkgJsonPath - Path to package.json
 * @returns {Promise<PackageEntry | null>} Package entry or null if invalid
 */
async function processPackage(pkgJsonPath) {
  try {
    const content = await readFile(pkgJsonPath, 'utf-8');
    const pkg = JSON.parse(content);

    if (!pkg.name) {
      // Skip packages without a name
      return null;
    }

    const packageDir = dirname(pkgJsonPath);

    // Extract selected scripts (test, dev, build only)
    const scripts = {};
    if (pkg.scripts) {
      for (const scriptName of ['test', 'dev', 'build']) {
        if (pkg.scripts[scriptName]) {
          scripts[scriptName] = pkg.scripts[scriptName];
        }
      }
    }

    // Check for various files and directories
    const hasReadme = existsSync(join(packageDir, 'README.md'));
    const hasDocs = await checkDirectory(join(packageDir, 'docs'));
    const hasExamples = await checkDirectory(join(packageDir, 'examples'));

    // Check multiple common test directory names
    const hasTests = await checkDirectory(join(packageDir, 'test')) ||
                     await checkDirectory(join(packageDir, 'tests')) ||
                     await checkDirectory(join(packageDir, '__tests__'));

    return {
      name: pkg.name,
      dir: packageDir,
      version: pkg.version || '0.0.0',
      description: pkg.description || '',
      exports: pkg.exports || {},
      bin: pkg.bin || {},
      keywords: pkg.keywords || [],
      hasReadme,
      hasDocs,
      hasExamples,
      hasTests,
      scripts
    };
  } catch (error) {
    throw new Error(`Failed to process package: ${error.message}`);
  }
}

/**
 * Checks if a directory exists
 * @param {string} dirPath - Path to check
 * @returns {Promise<boolean>} True if directory exists
 */
async function checkDirectory(dirPath) {
  try {
    const stats = await stat(dirPath);
    return stats.isDirectory();
  } catch (error) {
    return false;
  }
}
