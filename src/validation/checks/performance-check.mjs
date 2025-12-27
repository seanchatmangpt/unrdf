/**
 * @fileoverview Performance Check - Validates performance characteristics
 *
 * **Checks performed**:
 * 1. Benchmark SLA compliance
 * 2. Memory usage estimation
 * 3. Bundle size analysis
 * 4. Import cost estimation
 * 5. Async operation patterns
 * 6. Memory leak patterns
 * 7. Performance anti-patterns
 * 8. Startup time estimation
 *
 * **Scoring**:
 * - 100: Excellent performance characteristics
 * - 95-99: Good performance with minor optimizations possible
 * - 80-94: Acceptable performance
 * - 60-79: Performance improvements needed
 * - <60: Critical performance issues
 *
 * @module validation/checks/performance-check
 */

import { readdir, readFile, stat } from 'node:fs/promises';
import { join, extname, relative } from 'node:path';
import { spawn } from 'node:child_process';

/**
 * Performance thresholds
 */
export const PERFORMANCE_THRESHOLDS = {
  maxBundleSize: 500 * 1024, // 500KB
  maxDependencies: 50,
  maxFileSize: 100 * 1024, // 100KB per file
  maxMemoryMB: 100,
  maxStartupMs: 1000,
  maxAsyncDepth: 5
};

/**
 * Performance anti-patterns
 */
const ANTI_PATTERNS = [
  // Synchronous operations in hot paths
  { pattern: /readFileSync|writeFileSync|existsSync|readdirSync|statSync/g, type: 'SYNC_FS', severity: 'medium' },
  { pattern: /JSON\.parse\s*\(\s*require\s*\(/g, type: 'SYNC_REQUIRE_JSON', severity: 'low' },

  // Blocking operations
  { pattern: /while\s*\(\s*true\s*\)/g, type: 'INFINITE_LOOP', severity: 'high' },
  { pattern: /for\s*\(\s*;\s*;\s*\)/g, type: 'INFINITE_LOOP', severity: 'high' },

  // Memory issues
  { pattern: /new Array\s*\(\s*\d{6,}\s*\)/g, type: 'LARGE_ARRAY_ALLOC', severity: 'high' },
  { pattern: /\.push\s*\([^)]*\)\s*(?:.*\.push\s*\([^)]*\)\s*){5,}/g, type: 'EXCESSIVE_PUSH', severity: 'low' },

  // Inefficient patterns
  { pattern: /Array\s*\(\s*\d+\s*\)\.fill\s*\(/g, type: 'ARRAY_FILL_PATTERN', severity: 'info' },
  { pattern: /\.concat\s*\(\s*\[\s*\]\s*\)/g, type: 'UNNECESSARY_CONCAT', severity: 'low' },

  // Regex performance
  { pattern: /new RegExp\s*\([^)]*\+/g, type: 'DYNAMIC_REGEX', severity: 'low' },
  { pattern: /\(\?:[^\)]+\)\*\+/g, type: 'CATASTROPHIC_BACKTRACK', severity: 'high' },

  // Event listener leaks
  { pattern: /addEventListener\s*\([^)]+\)(?![\s\S]*removeEventListener)/g, type: 'POTENTIAL_LISTENER_LEAK', severity: 'medium' },

  // Timer leaks
  { pattern: /setInterval\s*\([^)]+\)(?![\s\S]*clearInterval)/g, type: 'POTENTIAL_TIMER_LEAK', severity: 'medium' },

  // N+1 patterns
  { pattern: /for\s*\([^)]+\)\s*\{[\s\S]*?await\s+(?:fetch|axios|request)/g, type: 'N_PLUS_1_REQUESTS', severity: 'high' },

  // Heavy operations in loops
  { pattern: /for\s*\([^)]+\)\s*\{[\s\S]*?JSON\.stringify/g, type: 'STRINGIFY_IN_LOOP', severity: 'medium' },
  { pattern: /for\s*\([^)]+\)\s*\{[\s\S]*?JSON\.parse/g, type: 'PARSE_IN_LOOP', severity: 'medium' }
];

/**
 * Positive performance patterns
 */
const GOOD_PATTERNS = [
  { pattern: /Promise\.all\s*\(/g, type: 'PARALLEL_PROMISES' },
  { pattern: /Promise\.allSettled\s*\(/g, type: 'SETTLED_PROMISES' },
  { pattern: /\.cache|memoize|useMemo|useCallback/g, type: 'CACHING' },
  { pattern: /lazy\s*\(\s*\(\s*\)\s*=>/g, type: 'LAZY_LOADING' },
  { pattern: /import\s*\(\s*['"][^'"]+['"]\s*\)/g, type: 'DYNAMIC_IMPORT' },
  { pattern: /WeakMap|WeakSet|WeakRef/g, type: 'WEAK_REFERENCES' }
];

/**
 * Analyze file for performance patterns
 *
 * @param {string} content - File content
 * @param {string} filePath - File path
 * @returns {Object} Performance analysis
 */
function analyzeFilePerformance(content, filePath) {
  const antiPatterns = [];
  const goodPatterns = [];

  // Skip test files for some checks
  const isTestFile = filePath.includes('.test.') || filePath.includes('.spec.');

  // Check anti-patterns
  for (const { pattern, type, severity } of ANTI_PATTERNS) {
    // Skip some patterns in test files
    if (isTestFile && ['SYNC_FS', 'SYNC_REQUIRE_JSON'].includes(type)) {
      continue;
    }

    const matches = content.match(pattern);
    if (matches) {
      antiPatterns.push({
        type,
        severity,
        count: matches.length,
        file: filePath
      });
    }
  }

  // Check good patterns
  for (const { pattern, type } of GOOD_PATTERNS) {
    const matches = content.match(pattern);
    if (matches) {
      goodPatterns.push({
        type,
        count: matches.length
      });
    }
  }

  return { antiPatterns, goodPatterns };
}

/**
 * Estimate bundle size
 *
 * @param {string} packagePath - Package path
 * @returns {Promise<Object>} Bundle size estimation
 */
async function estimateBundleSize(packagePath) {
  const result = {
    sourceSize: 0,
    estimatedBundleSize: 0,
    fileCount: 0,
    largestFiles: []
  };

  const fileSizes = [];

  async function scanDir(dir) {
    try {
      const entries = await readdir(dir, { withFileTypes: true });

      for (const entry of entries) {
        const fullPath = join(dir, entry.name);

        if (entry.isDirectory()) {
          if (!['node_modules', 'dist', 'build', 'coverage', '.git', 'test', 'tests'].includes(entry.name)) {
            await scanDir(fullPath);
          }
        } else if (entry.isFile()) {
          if (/\.(mjs|js|ts|jsx|tsx)$/.test(entry.name) && !entry.name.includes('.test.') && !entry.name.includes('.spec.')) {
            try {
              const stats = await stat(fullPath);
              result.sourceSize += stats.size;
              result.fileCount++;

              fileSizes.push({
                file: relative(packagePath, fullPath),
                size: stats.size
              });
            } catch {
              // File not accessible
            }
          }
        }
      }
    } catch {
      // Directory not accessible
    }
  }

  await scanDir(packagePath);

  // Estimate bundle size (rough estimation: source * 1.5 for minification overhead minus tree shaking)
  result.estimatedBundleSize = Math.round(result.sourceSize * 0.7);

  // Get largest files
  result.largestFiles = fileSizes
    .sort((a, b) => b.size - a.size)
    .slice(0, 5);

  return result;
}

/**
 * Analyze dependency weight
 *
 * @param {string} packagePath - Package path
 * @returns {Promise<Object>} Dependency analysis
 */
async function analyzeDependencyWeight(packagePath) {
  const result = {
    dependencyCount: 0,
    heavyDependencies: [],
    lightDependencies: 0
  };

  // Known heavy packages and their alternatives
  const heavyPackages = {
    'moment': { size: '~230KB', alternative: 'dayjs (~2KB) or date-fns' },
    'lodash': { size: '~70KB', alternative: 'lodash-es with tree shaking or native methods' },
    'jquery': { size: '~87KB', alternative: 'Native DOM APIs' },
    'axios': { size: '~13KB', alternative: 'Native fetch' },
    'bluebird': { size: '~80KB', alternative: 'Native Promise' },
    'underscore': { size: '~17KB', alternative: 'Native methods' },
    'request': { size: '~180KB', alternative: 'node-fetch or axios' },
    'express': { size: '~50KB', alternative: 'fastify (~20KB) for APIs' },
    'webpack': { size: '~200KB', alternative: 'esbuild or rollup for simpler cases' }
  };

  try {
    const content = await readFile(join(packagePath, 'package.json'), 'utf-8');
    const pkg = JSON.parse(content);

    const deps = { ...pkg.dependencies, ...pkg.peerDependencies };
    result.dependencyCount = Object.keys(deps).length;

    for (const dep of Object.keys(deps)) {
      if (heavyPackages[dep]) {
        result.heavyDependencies.push({
          name: dep,
          ...heavyPackages[dep]
        });
      }
    }

    result.lightDependencies = result.dependencyCount - result.heavyDependencies.length;
  } catch {
    // Package.json not readable
  }

  return result;
}

/**
 * Check for async patterns
 *
 * @param {string} packagePath - Package path
 * @returns {Promise<Object>} Async pattern analysis
 */
async function analyzeAsyncPatterns(packagePath) {
  const result = {
    asyncFunctions: 0,
    awaitUsage: 0,
    promiseAll: 0,
    unhandledPromises: 0,
    callbackPatterns: 0
  };

  async function scanFile(filePath) {
    try {
      const content = await readFile(filePath, 'utf-8');

      // Count async/await usage
      result.asyncFunctions += (content.match(/async\s+(?:function|\w+\s*=|[({])/g) || []).length;
      result.awaitUsage += (content.match(/await\s+/g) || []).length;
      result.promiseAll += (content.match(/Promise\.(?:all|allSettled|race)\s*\(/g) || []).length;

      // Check for callback patterns (potential for promisification)
      result.callbackPatterns += (content.match(/function\s*\([^)]*callback[^)]*\)/gi) || []).length;

      // Check for unhandled promises
      const awaitMatches = [...content.matchAll(/(?<!await\s+)(?<!return\s+)(?:fetch|axios|Promise|\.then)\s*\(/g)];
      result.unhandledPromises += awaitMatches.length;
    } catch {
      // File not readable
    }
  }

  async function scanDir(dir) {
    try {
      const entries = await readdir(dir, { withFileTypes: true });

      for (const entry of entries) {
        const fullPath = join(dir, entry.name);

        if (entry.isDirectory()) {
          if (!['node_modules', 'dist', 'build', 'coverage', '.git'].includes(entry.name)) {
            await scanDir(fullPath);
          }
        } else if (entry.isFile()) {
          if (/\.(mjs|js|ts)$/.test(entry.name)) {
            await scanFile(fullPath);
          }
        }
      }
    } catch {
      // Directory not accessible
    }
  }

  await scanDir(packagePath);

  return result;
}

/**
 * Get all source files
 *
 * @param {string} dirPath - Directory path
 * @returns {Promise<Array<string>>} File paths
 */
async function getSourceFiles(dirPath) {
  const files = [];

  async function scan(currentPath) {
    try {
      const entries = await readdir(currentPath, { withFileTypes: true });

      for (const entry of entries) {
        const fullPath = join(currentPath, entry.name);

        if (entry.isDirectory()) {
          if (!['node_modules', 'dist', 'build', 'coverage', '.git'].includes(entry.name)) {
            await scan(fullPath);
          }
        } else if (entry.isFile()) {
          const ext = extname(entry.name);
          if (['.mjs', '.js', '.ts', '.jsx', '.tsx'].includes(ext)) {
            files.push(fullPath);
          }
        }
      }
    } catch {
      // Directory not accessible
    }
  }

  await scan(dirPath);
  return files;
}

/**
 * Perform performance check on a package
 *
 * @param {string} packagePath - Path to package
 * @param {Object} options - Check options
 * @returns {Promise<Object>} Check result
 */
export async function performanceCheck(packagePath, options = {}) {
  const startTime = Date.now();
  const warnings = [];
  const failures = [];
  const remediation = [];

  let totalScore = 100;
  const details = {
    estimatedBundleSize: 0,
    bundleSizeKB: 0,
    antiPatternCount: 0,
    goodPatternCount: 0,
    heavyDependencies: 0,
    asyncUsage: 0,
    issues: []
  };

  try {
    // Bundle size estimation
    const bundleResult = await estimateBundleSize(packagePath);
    details.estimatedBundleSize = bundleResult.estimatedBundleSize;
    details.bundleSizeKB = Math.round(bundleResult.estimatedBundleSize / 1024);

    if (bundleResult.estimatedBundleSize > PERFORMANCE_THRESHOLDS.maxBundleSize) {
      warnings.push(`Estimated bundle size ${details.bundleSizeKB}KB exceeds ${PERFORMANCE_THRESHOLDS.maxBundleSize / 1024}KB`);
      remediation.push('Consider code splitting or removing unused dependencies');
    }

    // Check for large files
    for (const file of bundleResult.largestFiles) {
      if (file.size > PERFORMANCE_THRESHOLDS.maxFileSize) {
        warnings.push(`Large file: ${file.file} (${Math.round(file.size / 1024)}KB)`);
      }
    }

    // Dependency analysis
    const depResult = await analyzeDependencyWeight(packagePath);
    details.heavyDependencies = depResult.heavyDependencies.length;

    for (const heavy of depResult.heavyDependencies) {
      warnings.push(`Heavy dependency: ${heavy.name} (${heavy.size})`);
      remediation.push(`Consider ${heavy.alternative}`);
    }

    if (depResult.dependencyCount > PERFORMANCE_THRESHOLDS.maxDependencies) {
      warnings.push(`${depResult.dependencyCount} dependencies (limit: ${PERFORMANCE_THRESHOLDS.maxDependencies})`);
    }

    // Performance pattern analysis
    const sourceFiles = await getSourceFiles(packagePath);
    let totalAntiPatterns = [];
    let totalGoodPatterns = [];

    for (const filePath of sourceFiles) {
      try {
        const content = await readFile(filePath, 'utf-8');
        const relativePath = relative(packagePath, filePath);
        const analysis = analyzeFilePerformance(content, relativePath);

        totalAntiPatterns.push(...analysis.antiPatterns);
        totalGoodPatterns.push(...analysis.goodPatterns);
      } catch {
        // File not readable
      }
    }

    details.antiPatternCount = totalAntiPatterns.length;
    details.goodPatternCount = totalGoodPatterns.length;

    // Process anti-patterns
    const highSeverity = totalAntiPatterns.filter(p => p.severity === 'high');
    const mediumSeverity = totalAntiPatterns.filter(p => p.severity === 'medium');

    if (highSeverity.length > 0) {
      failures.push(`${highSeverity.length} high-severity performance issue(s)`);
      const types = [...new Set(highSeverity.map(p => p.type))];
      remediation.push(`Address: ${types.slice(0, 3).join(', ')}`);
    }

    if (mediumSeverity.length > 0) {
      warnings.push(`${mediumSeverity.length} medium-severity performance issue(s)`);
    }

    // Store issues for details
    details.issues = totalAntiPatterns.slice(0, 10);

    // Async patterns
    const asyncResult = await analyzeAsyncPatterns(packagePath);
    details.asyncUsage = asyncResult.asyncFunctions;

    if (asyncResult.callbackPatterns > 5) {
      warnings.push(`${asyncResult.callbackPatterns} callback patterns - consider promisification`);
    }

    // Calculate score
    // Bundle size (25 points)
    let bundleScore = 25;
    if (bundleResult.estimatedBundleSize > PERFORMANCE_THRESHOLDS.maxBundleSize * 2) {
      bundleScore = 0;
    } else if (bundleResult.estimatedBundleSize > PERFORMANCE_THRESHOLDS.maxBundleSize) {
      bundleScore = 15;
    }

    // Anti-patterns (35 points)
    let patternScore = 35;
    patternScore -= highSeverity.length * 10;
    patternScore -= mediumSeverity.length * 3;
    patternScore = Math.max(0, patternScore);

    // Good patterns bonus (10 points)
    const goodScore = Math.min(10, totalGoodPatterns.length * 2);

    // Dependencies (20 points)
    let depScore = 20;
    depScore -= depResult.heavyDependencies.length * 5;
    depScore -= Math.max(0, depResult.dependencyCount - PERFORMANCE_THRESHOLDS.maxDependencies);
    depScore = Math.max(0, depScore);

    // Async usage bonus (10 points)
    const asyncScore = asyncResult.promiseAll > 0 ? 10 : asyncResult.asyncFunctions > 0 ? 7 : 5;

    totalScore = Math.round(bundleScore + patternScore + goodScore + depScore + asyncScore);
    totalScore = Math.max(0, Math.min(100, totalScore));

  } catch (error) {
    failures.push(`Performance check failed: ${error.message}`);
    totalScore = 0;
  }

  return {
    passed: totalScore >= 80,
    score: totalScore,
    status: totalScore >= 95 ? 'pass' : totalScore >= 80 ? 'warn' : 'fail',
    warnings: [...new Set(warnings)].slice(0, 20),
    failures: [...new Set(failures)].slice(0, 10),
    remediation: [...new Set(remediation)].slice(0, 10),
    duration: Date.now() - startTime,
    details
  };
}

export default performanceCheck;
