#!/usr/bin/env node
/**
 * Contract Scanner - Extracts API contracts from UNRDF packages
 *
 * Scans package.json exports and source files to build a comprehensive
 * contract inventory for enterprise API governance.
 *
 * @module agent-2/contract-scanner
 */

import { readFile, readdir, stat } from 'node:fs/promises';
import { join, dirname, relative } from 'node:path';
import { fileURLToPath } from 'node:url';

const __dirname = dirname(fileURLToPath(import.meta.url));
const PACKAGES_ROOT = join(__dirname, '../../packages');

/**
 * Scans a single package for contract information
 * @param {string} packagePath - Absolute path to package directory
 * @returns {Promise<Object|null>} Contract data or null if invalid
 */
export async function scanPackage(packagePath) {
  try {
    const pkgJsonPath = join(packagePath, 'package.json');
    const pkgJson = JSON.parse(await readFile(pkgJsonPath, 'utf-8'));

    // Skip packages without exports
    if (!pkgJson.exports) {
      return null;
    }

    const contracts = {
      name: pkgJson.name,
      version: pkgJson.version,
      description: pkgJson.description || '',
      exports: {},
      functions: {},
      types: {},
      errors: {},
      logging: {},
      idRules: {},
    };

    // Process each export
    const exportEntries = typeof pkgJson.exports === 'string'
      ? { '.': pkgJson.exports }
      : pkgJson.exports;

    for (const [exportName, exportPath] of Object.entries(exportEntries)) {
      const resolvedPath = join(packagePath, exportPath);

      try {
        const sourceCode = await readFile(resolvedPath, 'utf-8');
        const exportData = await analyzeSourceFile(sourceCode, exportPath);

        contracts.exports[exportName] = {
          path: exportPath,
          ...exportData,
        };
      } catch (error) {
        // Skip files that don't exist or can't be read
        contracts.exports[exportName] = {
          path: exportPath,
          status: 'unreadable',
          error: error.message,
        };
      }
    }

    // Scan for error codes
    contracts.errors = await scanErrorCodes(packagePath);

    // Scan for logging patterns
    contracts.logging = await scanLoggingPatterns(packagePath);

    // Extract ID/naming rules
    contracts.idRules = extractIdRules(pkgJson.name);

    return contracts;
  } catch (error) {
    console.error(`Failed to scan package at ${packagePath}:`, error.message);
    return null;
  }
}

/**
 * Analyzes a source file to extract exported functions and types
 * @param {string} sourceCode - Source code content
 * @param {string} filePath - Relative file path
 * @returns {Promise<Object>} Extracted contract information
 */
async function analyzeSourceFile(sourceCode, filePath) {
  const result = {
    exports: [],
    functions: {},
    reExports: [],
  };

  // Extract named exports
  const namedExportRegex = /export\s+(?:async\s+)?(?:function|const|let|var|class)\s+(\w+)/g;
  let match;
  while ((match = namedExportRegex.exec(sourceCode)) !== null) {
    result.exports.push(match[1]);
  }

  // Extract re-exports (export { ... } from '...')
  const reExportRegex = /export\s+\{([^}]+)\}\s+from\s+['"]([^'"]+)['"]/g;
  while ((match = reExportRegex.exec(sourceCode)) !== null) {
    const names = match[1].split(',').map(n => n.trim().split(/\s+as\s+/).pop());
    result.reExports.push(...names);
    result.exports.push(...names);
  }

  // Extract standalone export statements
  const standaloneExportRegex = /export\s+\{([^}]+)\}/g;
  while ((match = standaloneExportRegex.exec(sourceCode)) !== null) {
    const names = match[1].split(',').map(n => n.trim().split(/\s+as\s+/).pop());
    result.exports.push(...names);
  }

  // Extract function signatures from JSDoc
  const functionDocRegex = /@param\s+\{([^}]+)\}\s+(?:\[)?(\w+)(?:\])?\s*-?\s*([^\n]*)/g;
  const functions = {};

  // Find function definitions with JSDoc
  const funcDefRegex = /\/\*\*[\s\S]*?\*\/\s*export\s+(?:async\s+)?function\s+(\w+)\s*\(([^)]*)\)/g;
  while ((match = funcDefRegex.exec(sourceCode)) !== null) {
    const funcName = match[1];
    const params = match[2].split(',').map(p => p.trim()).filter(Boolean);

    functions[funcName] = {
      parameters: params.map(p => {
        const [name, defaultValue] = p.split('=').map(s => s.trim());
        return {
          name: name.replace(/[{}[\]]/g, ''),
          optional: p.includes('=') || p.includes('?'),
          defaultValue: defaultValue || undefined,
        };
      }),
      async: match[0].includes('async'),
    };
  }

  result.functions = functions;
  result.exports = [...new Set(result.exports)]; // Deduplicate

  return result;
}

/**
 * Scans a package for error code definitions
 * @param {string} packagePath - Package directory path
 * @returns {Promise<Object>} Error code inventory
 */
async function scanErrorCodes(packagePath) {
  const errors = {
    codes: [],
    patterns: [],
  };

  try {
    const srcPath = join(packagePath, 'src');
    const files = await findSourceFiles(srcPath);

    for (const file of files) {
      const content = await readFile(file, 'utf-8');

      // Look for error code constants
      const errorCodeRegex = /const\s+(\w*ERROR\w*)\s*=\s*['"]([^'"]+)['"]/gi;
      let match;
      while ((match = errorCodeRegex.exec(content)) !== null) {
        errors.codes.push({
          name: match[1],
          code: match[2],
          file: relative(packagePath, file),
        });
      }

      // Look for Error class instantiations
      const errorThrowRegex = /throw\s+new\s+Error\(['"]([^'"]+)['"]/g;
      while ((match = errorThrowRegex.exec(content)) !== null) {
        errors.patterns.push({
          message: match[1],
          file: relative(packagePath, file),
        });
      }
    }
  } catch (error) {
    // Package may not have src directory
  }

  return errors;
}

/**
 * Scans for logging field patterns
 * @param {string} packagePath - Package directory path
 * @returns {Promise<Object>} Logging field inventory
 */
async function scanLoggingPatterns(packagePath) {
  const logging = {
    fields: new Set(),
    levels: new Set(),
  };

  try {
    const srcPath = join(packagePath, 'src');
    const files = await findSourceFiles(srcPath);

    for (const file of files) {
      const content = await readFile(file, 'utf-8');

      // Look for logger calls
      const loggerRegex = /logger\.(debug|info|warn|error|fatal)/g;
      let match;
      while ((match = loggerRegex.exec(content)) !== null) {
        logging.levels.add(match[1]);
      }

      // Look for common log fields
      const fieldPatterns = [
        'packageName', 'functionName', 'operation', 'duration',
        'error', 'statusCode', 'requestId', 'userId', 'traceId',
      ];

      for (const field of fieldPatterns) {
        if (content.includes(field)) {
          logging.fields.add(field);
        }
      }
    }
  } catch (error) {
    // Package may not have logging
  }

  return {
    fields: Array.from(logging.fields).sort(),
    levels: Array.from(logging.levels).sort(),
  };
}

/**
 * Extracts ID/naming rules from package name
 * @param {string} packageName - Package name (e.g., @unrdf/oxigraph)
 * @returns {Object} ID rules
 */
function extractIdRules(packageName) {
  const scope = packageName.startsWith('@') ? packageName.split('/')[0] : null;
  const name = packageName.startsWith('@') ? packageName.split('/')[1] : packageName;

  return {
    scope,
    name,
    conventions: {
      functions: 'camelCase',
      classes: 'PascalCase',
      constants: 'UPPER_SNAKE_CASE',
      files: 'kebab-case.mjs',
    },
  };
}

/**
 * Recursively finds all .mjs source files
 * @param {string} dirPath - Directory to search
 * @returns {Promise<string[]>} Array of file paths
 */
async function findSourceFiles(dirPath) {
  const files = [];

  try {
    const entries = await readdir(dirPath);

    for (const entry of entries) {
      const fullPath = join(dirPath, entry);
      const stats = await stat(fullPath);

      if (stats.isDirectory()) {
        files.push(...await findSourceFiles(fullPath));
      } else if (entry.endsWith('.mjs')) {
        files.push(fullPath);
      }
    }
  } catch (error) {
    // Directory doesn't exist or not accessible
  }

  return files;
}

/**
 * Scans all packages in the UNRDF monorepo
 * @returns {Promise<Object>} Complete contract inventory
 */
export async function scanAllPackages() {
  const inventory = {
    scannedAt: new Date().toISOString(),
    packages: {},
    summary: {
      totalPackages: 0,
      totalExports: 0,
      totalFunctions: 0,
    },
  };

  try {
    const packageDirs = await readdir(PACKAGES_ROOT);

    for (const dir of packageDirs) {
      const packagePath = join(PACKAGES_ROOT, dir);
      const stats = await stat(packagePath);

      if (!stats.isDirectory()) continue;

      const contract = await scanPackage(packagePath);

      if (contract) {
        inventory.packages[contract.name] = contract;
        inventory.summary.totalPackages++;
        inventory.summary.totalExports += Object.keys(contract.exports).length;

        // Count functions across all exports
        for (const exp of Object.values(contract.exports)) {
          if (exp.functions) {
            inventory.summary.totalFunctions += Object.keys(exp.functions).length;
          }
        }
      }
    }
  } catch (error) {
    console.error('Failed to scan packages:', error);
    throw error;
  }

  return inventory;
}

/**
 * Main execution when run as script
 */
if (import.meta.url === `file://${process.argv[1]}`) {
  console.log('🔍 Scanning UNRDF packages for contracts...\n');

  const inventory = await scanAllPackages();

  console.log('📊 Scan Results:');
  console.log(`  Packages: ${inventory.summary.totalPackages}`);
  console.log(`  Exports: ${inventory.summary.totalExports}`);
  console.log(`  Functions: ${inventory.summary.totalFunctions}`);
  console.log('\n✅ Scan complete');

  // Output summary of key packages
  const keyPackages = [
    '@unrdf/oxigraph',
    '@unrdf/core',
    '@unrdf/hooks',
    '@unrdf/streaming',
    '@unrdf/federation',
  ];

  console.log('\n📦 Key Package Exports:');
  for (const pkg of keyPackages) {
    if (inventory.packages[pkg]) {
      const contract = inventory.packages[pkg];
      const exportCount = Object.keys(contract.exports).length;
      console.log(`  ${pkg}: ${exportCount} export(s)`);
    }
  }
}
