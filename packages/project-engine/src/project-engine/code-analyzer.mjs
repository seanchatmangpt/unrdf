/**
 * @file Code Analyzer - Package quality metrics and analysis
 * @module @unrdf/project-engine/code-analyzer
 */

import { readFile, readdir } from 'node:fs/promises';
import { join } from 'node:path';
import { z } from 'zod';

/**
 * Package analysis schema
 */
const PackageAnalysisSchema = z.object({
  name: z.string(),
  linesOfCode: z.number(),
  fileCount: z.number(),
  exportCount: z.number(),
  testCoverage: z.number().min(0).max(100),
  dependencies: z.array(z.string()),
  devDependencies: z.array(z.string()),
  publicApis: z.array(z.string()),
  complexity: z.enum(['low', 'medium', 'high']),
});

/**
 * Analyze package quality metrics
 * @param {string} packagePath - Path to package directory
 * @returns {Promise<Object>} Package analysis results
 *
 * @throws {TypeError} If packagePath is not a string
 * @throws {Error} If package cannot be analyzed
 *
 * @example
 * const analysis = await analyzePackage('./packages/core');
 * console.log('Lines of code:', analysis.linesOfCode);
 * console.log('Test coverage:', analysis.testCoverage);
 */
export async function analyzePackage(packagePath) {
  if (typeof packagePath !== 'string') {
    throw new TypeError('analyzePackage: packagePath must be a string');
  }

  try {
    // Read package.json
    const packageJsonPath = join(packagePath, 'package.json');
    const packageJsonContent = await readFile(packageJsonPath, 'utf-8');
    const packageJson = JSON.parse(packageJsonContent);

    // Count lines of code
    const srcPath = join(packagePath, 'src');
    const sourceFiles = await findFiles(srcPath, '.mjs');
    let linesOfCode = 0;

    for (const file of sourceFiles) {
      const content = await readFile(file, 'utf-8');
      linesOfCode += content.split('\n').length;
    }

    // Find all exports
    const publicApis = [];
    for (const file of sourceFiles) {
      const exports = await findExports(file);
      publicApis.push(...exports);
    }

    // Estimate test coverage
    const testPath = join(packagePath, 'test');
    const testFiles = await findFiles(testPath, '.test.mjs');
    const testCoverage = await estimateTestCoverage(sourceFiles, testFiles);

    // Determine complexity
    const complexity = determineComplexity(linesOfCode, publicApis.length, sourceFiles.length);

    const analysis = {
      name: packageJson.name,
      linesOfCode,
      fileCount: sourceFiles.length,
      exportCount: publicApis.length,
      testCoverage,
      dependencies: Object.keys(packageJson.dependencies || {}),
      devDependencies: Object.keys(packageJson.devDependencies || {}),
      publicApis,
      complexity,
    };

    return PackageAnalysisSchema.parse(analysis);
  } catch (error) {
    throw new Error(`analyzePackage failed: ${error.message}`);
  }
}

/**
 * Find all exports from a module
 * @param {string} filePath - Path to module file
 * @returns {Promise<Array<string>>} List of exported identifiers
 *
 * @throws {TypeError} If filePath is not a string
 * @throws {Error} If file cannot be read
 *
 * @example
 * const exports = await findExports('./src/index.mjs');
 * console.log('Exports:', exports);
 */
export async function findExports(filePath) {
  if (typeof filePath !== 'string') {
    throw new TypeError('findExports: filePath must be a string');
  }

  try {
    const content = await readFile(filePath, 'utf-8');
    const exports = [];

    // Named exports: export function/const/let/class name
    const namedMatches = content.matchAll(
      /export\s+(async\s+)?(function|const|let|class)\s+(\w+)/g
    );
    for (const match of namedMatches) {
      exports.push(match[3]);
    }

    // Export { name1, name2 }
    const braceMatches = content.matchAll(/export\s+\{([^}]+)\}/g);
    for (const match of braceMatches) {
      const names = match[1].split(',').map(n => n.trim().split(/\s+as\s+/)[0]);
      exports.push(...names);
    }

    return exports;
  } catch (error) {
    throw new Error(`findExports failed: ${error.message}`);
  }
}

/**
 * Estimate test coverage percentage
 * @param {Array<string>} sourceFiles - Source file paths
 * @param {Array<string>} testFiles - Test file paths
 * @returns {Promise<number>} Estimated coverage percentage
 *
 * @throws {TypeError} If arguments are not arrays
 *
 * @example
 * const coverage = await countCoverage(['src/a.mjs'], ['test/a.test.mjs']);
 * console.log('Coverage:', coverage + '%');
 */
export async function countCoverage(sourceFiles, testFiles) {
  if (!Array.isArray(sourceFiles)) {
    throw new TypeError('countCoverage: sourceFiles must be an array');
  }
  if (!Array.isArray(testFiles)) {
    throw new TypeError('countCoverage: testFiles must be an array');
  }

  return estimateTestCoverage(sourceFiles, testFiles);
}

/**
 * Find files with specific extension
 * @param {string} dirPath - Directory path
 * @param {string} extension - File extension
 * @returns {Promise<Array<string>>} List of file paths
 */
async function findFiles(dirPath, extension) {
  const files = [];

  try {
    const entries = await readdir(dirPath, { withFileTypes: true });

    for (const entry of entries) {
      const fullPath = join(dirPath, entry.name);

      if (entry.isDirectory()) {
        const subFiles = await findFiles(fullPath, extension);
        files.push(...subFiles);
      } else if (entry.isFile() && entry.name.endsWith(extension)) {
        files.push(fullPath);
      }
    }
  } catch (error) {
    // Directory doesn't exist
  }

  return files;
}

/**
 * Estimate test coverage heuristically
 * @param {Array<string>} sourceFiles - Source file paths
 * @param {Array<string>} testFiles - Test file paths
 * @returns {Promise<number>} Coverage percentage
 */
async function estimateTestCoverage(sourceFiles, testFiles) {
  if (sourceFiles.length === 0) {
    return 0;
  }

  if (testFiles.length === 0) {
    return 0;
  }

  // Heuristic: ratio of test files to source files
  const ratio = testFiles.length / sourceFiles.length;

  // Count test assertions as proxy for coverage
  let totalAssertions = 0;
  for (const testFile of testFiles) {
    const content = await readFile(testFile, 'utf-8');
    const assertionCount = (content.match(/expect\(/g) || []).length;
    totalAssertions += assertionCount;
  }

  // Count source functions
  let totalFunctions = 0;
  for (const srcFile of sourceFiles) {
    const content = await readFile(srcFile, 'utf-8');
    const functionCount = (content.match(/export\s+(async\s+)?function/g) || []).length;
    totalFunctions += functionCount;
  }

  if (totalFunctions === 0) {
    return ratio * 50; // Base estimate
  }

  // Coverage estimate: assertions per function
  const assertionsPerFunction = totalAssertions / totalFunctions;
  const coverageEstimate = Math.min(assertionsPerFunction * 30, 100);

  return Math.round(coverageEstimate);
}

/**
 * Determine package complexity
 * @param {number} linesOfCode - Total lines of code
 * @param {number} exportCount - Number of exports
 * @param {number} fileCount - Number of files
 * @returns {string} Complexity level
 */
function determineComplexity(linesOfCode, exportCount, fileCount) {
  const score = linesOfCode / 100 + exportCount * 2 + fileCount * 5;

  if (score < 50) {
    return 'low';
  } else if (score < 200) {
    return 'medium';
  } else {
    return 'high';
  }
}
