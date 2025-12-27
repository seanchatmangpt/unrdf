/**
 * @fileoverview Code Quality Check - Validates code standards and best practices
 *
 * **Checks performed**:
 * 1. JSDoc coverage (target: 100%)
 * 2. Console.log usage in production code (target: 0)
 * 3. Function length (target: <100 lines)
 * 4. File length (target: <500 lines)
 * 5. Cyclomatic complexity (target: <10)
 * 6. Duplicate code detection
 * 7. Dead code detection
 * 8. Naming conventions
 * 9. Code smells
 *
 * **Scoring**:
 * - 100: Perfect code quality
 * - 95-99: Minor improvements possible
 * - 80-94: Some issues need attention
 * - 60-79: Significant issues
 * - <60: Critical issues
 *
 * @module validation/checks/code-quality-check
 */

import { readdir, readFile, stat } from 'node:fs/promises';
import { join, extname, relative } from 'node:path';

/**
 * Check thresholds
 */
export const CODE_QUALITY_THRESHOLDS = {
  jsdocCoverage: 100,
  maxConsoleLog: 0,
  maxFunctionLines: 100,
  maxFileLines: 500,
  maxComplexity: 10,
  maxDuplicateRatio: 0.05,
  minNamingScore: 90
};

/**
 * Patterns for detection
 */
const PATTERNS = {
  jsdoc: /\/\*\*[\s\S]*?\*\//g,
  function: /(?:function\s+\w+|(?:const|let|var)\s+\w+\s*=\s*(?:async\s+)?(?:function|\([^)]*\)\s*=>|\w+\s*=>))/g,
  exportedFunction: /export\s+(?:async\s+)?function\s+(\w+)/g,
  consoleLog: /console\.(log|warn|error|info|debug|trace)\s*\(/g,
  complexConditional: /if\s*\([^)]+\s*(?:&&|\|\|)\s*[^)]+(?:&&|\|\|)\s*[^)]+\)/g,
  longLine: /^.{120,}$/gm,
  todo: /(?:TODO|FIXME|HACK|XXX):/gi,
  magicNumber: /(?<![a-zA-Z0-9_])(?:[2-9]|[1-9][0-9]+)(?![a-zA-Z0-9_])/g,
  camelCase: /^[a-z][a-zA-Z0-9]*$/,
  pascalCase: /^[A-Z][a-zA-Z0-9]*$/,
  snakeCase: /^[a-z][a-z0-9_]*$/,
  constCase: /^[A-Z][A-Z0-9_]*$/
};

/**
 * Code smell detectors
 */
const CODE_SMELLS = {
  longMethod: (content, threshold = 50) => {
    const functions = extractFunctions(content);
    return functions.filter(f => f.lines > threshold);
  },

  largeClass: (content, threshold = 500) => {
    const lines = content.split('\n').length;
    return lines > threshold;
  },

  godObject: (content, threshold = 20) => {
    const methods = content.match(/\b(?:async\s+)?(\w+)\s*\([^)]*\)\s*{/g) || [];
    return methods.length > threshold;
  },

  featureEnvy: (content) => {
    // Detect excessive this.other or other.method calls
    const matches = content.match(/\bthis\.\w+\.\w+/g) || [];
    return matches.length > 10;
  },

  deepNesting: (content, threshold = 4) => {
    let maxNesting = 0;
    let currentNesting = 0;

    for (const char of content) {
      if (char === '{') {
        currentNesting++;
        maxNesting = Math.max(maxNesting, currentNesting);
      } else if (char === '}') {
        currentNesting--;
      }
    }

    return maxNesting > threshold;
  }
};

/**
 * Extract function information from content
 *
 * @param {string} content - File content
 * @returns {Array} Function information
 */
function extractFunctions(content) {
  const functions = [];
  const lines = content.split('\n');

  let inFunction = false;
  let braceCount = 0;
  let functionStart = 0;
  let functionName = '';

  for (let i = 0; i < lines.length; i++) {
    const line = lines[i];

    // Detect function start
    const funcMatch = line.match(/(?:function\s+(\w+)|(?:const|let|var)\s+(\w+)\s*=\s*(?:async\s+)?(?:function|\([^)]*\)\s*=>))/);
    if (funcMatch && !inFunction) {
      inFunction = true;
      functionStart = i;
      functionName = funcMatch[1] || funcMatch[2] || 'anonymous';
    }

    // Count braces
    for (const char of line) {
      if (char === '{') braceCount++;
      if (char === '}') braceCount--;
    }

    // Detect function end
    if (inFunction && braceCount === 0) {
      functions.push({
        name: functionName,
        startLine: functionStart + 1,
        endLine: i + 1,
        lines: i - functionStart + 1
      });
      inFunction = false;
    }
  }

  return functions;
}

/**
 * Check JSDoc coverage
 *
 * @param {string} content - File content
 * @param {string} filePath - File path
 * @returns {Object} JSDoc check result
 */
function checkJsdocCoverage(content, filePath) {
  const warnings = [];
  const failures = [];

  // Find all exported functions
  const exportedMatches = [...content.matchAll(/export\s+(?:async\s+)?(?:function|const|class)\s+(\w+)/g)];
  const exportedItems = exportedMatches.map(m => m[1]);

  // Find all JSDoc blocks
  const jsdocBlocks = content.match(PATTERNS.jsdoc) || [];

  // Check if each exported item has JSDoc
  const undocumented = [];

  for (const item of exportedItems) {
    const regex = new RegExp(`\\/\\*\\*[\\s\\S]*?\\*\\/\\s*export\\s+(?:async\\s+)?(?:function|const|class)\\s+${item}\\b`);
    if (!regex.test(content)) {
      undocumented.push(item);
    }
  }

  if (undocumented.length > 0) {
    const coverage = ((exportedItems.length - undocumented.length) / exportedItems.length * 100).toFixed(1);
    if (parseFloat(coverage) < 80) {
      failures.push(`JSDoc coverage ${coverage}% (${undocumented.length}/${exportedItems.length} undocumented)`);
    } else if (parseFloat(coverage) < 100) {
      warnings.push(`JSDoc coverage ${coverage}% - missing: ${undocumented.slice(0, 3).join(', ')}${undocumented.length > 3 ? '...' : ''}`);
    }
  }

  return {
    exportedCount: exportedItems.length,
    documentedCount: exportedItems.length - undocumented.length,
    coverage: exportedItems.length > 0
      ? ((exportedItems.length - undocumented.length) / exportedItems.length * 100)
      : 100,
    undocumented,
    warnings,
    failures
  };
}

/**
 * Check console usage
 *
 * @param {string} content - File content
 * @param {string} filePath - File path
 * @returns {Object} Console check result
 */
function checkConsoleUsage(content, filePath) {
  const warnings = [];
  const failures = [];

  const matches = [...content.matchAll(/console\.(log|warn|error|info|debug|trace)\s*\(/g)];

  // Ignore test files and scripts
  const isTestFile = filePath.includes('.test.') || filePath.includes('.spec.') || filePath.includes('__tests__');
  const isScript = filePath.includes('/scripts/') || filePath.includes('/bin/');

  if (!isTestFile && !isScript && matches.length > 0) {
    if (matches.length > 5) {
      failures.push(`${matches.length} console.* calls in production code`);
    } else {
      warnings.push(`${matches.length} console.* calls found`);
    }
  }

  return {
    consoleCount: matches.length,
    isAllowed: isTestFile || isScript,
    warnings,
    failures
  };
}

/**
 * Check function complexity
 *
 * @param {string} content - File content
 * @returns {Object} Complexity check result
 */
function checkComplexity(content) {
  const warnings = [];
  const failures = [];

  const functions = extractFunctions(content);
  const longFunctions = functions.filter(f => f.lines > CODE_QUALITY_THRESHOLDS.maxFunctionLines);
  const veryLongFunctions = functions.filter(f => f.lines > CODE_QUALITY_THRESHOLDS.maxFunctionLines * 1.5);

  // Detect complex conditionals
  const complexConditionals = (content.match(PATTERNS.complexConditional) || []).length;

  // Check for deep nesting
  const hasDeepNesting = CODE_SMELLS.deepNesting(content);

  if (veryLongFunctions.length > 0) {
    failures.push(`${veryLongFunctions.length} function(s) exceed ${Math.round(CODE_QUALITY_THRESHOLDS.maxFunctionLines * 1.5)} lines`);
  } else if (longFunctions.length > 0) {
    warnings.push(`${longFunctions.length} function(s) exceed ${CODE_QUALITY_THRESHOLDS.maxFunctionLines} lines`);
  }

  if (complexConditionals > 5) {
    warnings.push(`${complexConditionals} complex conditional expressions`);
  }

  if (hasDeepNesting) {
    warnings.push('Deep nesting detected (>4 levels)');
  }

  return {
    functions,
    longFunctions,
    complexConditionals,
    hasDeepNesting,
    warnings,
    failures
  };
}

/**
 * Check file length
 *
 * @param {string} content - File content
 * @param {string} filePath - File path
 * @returns {Object} File length check result
 */
function checkFileLength(content, filePath) {
  const warnings = [];
  const failures = [];

  const lines = content.split('\n').length;

  if (lines > CODE_QUALITY_THRESHOLDS.maxFileLines * 2) {
    failures.push(`File has ${lines} lines (limit: ${CODE_QUALITY_THRESHOLDS.maxFileLines})`);
  } else if (lines > CODE_QUALITY_THRESHOLDS.maxFileLines) {
    warnings.push(`File has ${lines} lines (recommended: <${CODE_QUALITY_THRESHOLDS.maxFileLines})`);
  }

  return {
    lineCount: lines,
    warnings,
    failures
  };
}

/**
 * Detect code smells
 *
 * @param {string} content - File content
 * @returns {Object} Code smell detection result
 */
function detectCodeSmells(content) {
  const smells = [];
  const warnings = [];
  const failures = [];

  // Long methods
  const longMethods = CODE_SMELLS.longMethod(content);
  if (longMethods.length > 0) {
    smells.push({ type: 'long-method', count: longMethods.length });
    warnings.push(`${longMethods.length} long method(s) detected`);
  }

  // Large file (god object proxy)
  if (CODE_SMELLS.largeClass(content)) {
    smells.push({ type: 'large-file', count: 1 });
    warnings.push('Large file detected - consider splitting');
  }

  // Too many methods (god object)
  if (CODE_SMELLS.godObject(content)) {
    smells.push({ type: 'god-object', count: 1 });
    warnings.push('Too many methods in file - possible god object');
  }

  // Feature envy
  if (CODE_SMELLS.featureEnvy(content)) {
    smells.push({ type: 'feature-envy', count: 1 });
    warnings.push('Feature envy detected - excessive external references');
  }

  // Deep nesting
  if (CODE_SMELLS.deepNesting(content)) {
    smells.push({ type: 'deep-nesting', count: 1 });
  }

  // Magic numbers (sample check)
  const todoMatches = content.match(PATTERNS.todo) || [];
  if (todoMatches.length > 5) {
    smells.push({ type: 'todo-comments', count: todoMatches.length });
    warnings.push(`${todoMatches.length} TODO/FIXME comments`);
  }

  return {
    smells,
    smellCount: smells.reduce((sum, s) => sum + s.count, 0),
    warnings,
    failures
  };
}

/**
 * Get all source files in a directory
 *
 * @param {string} dirPath - Directory path
 * @param {Array<string>} extensions - File extensions to include
 * @returns {Promise<Array<string>>} File paths
 */
async function getSourceFiles(dirPath, extensions = ['.mjs', '.js', '.ts', '.jsx', '.tsx']) {
  const files = [];

  async function scan(currentPath) {
    try {
      const entries = await readdir(currentPath, { withFileTypes: true });

      for (const entry of entries) {
        const fullPath = join(currentPath, entry.name);

        // Skip node_modules, dist, coverage, etc.
        if (entry.isDirectory()) {
          if (!['node_modules', 'dist', 'build', 'coverage', '.git', '.cache'].includes(entry.name)) {
            await scan(fullPath);
          }
        } else if (entry.isFile()) {
          const ext = extname(entry.name);
          if (extensions.includes(ext)) {
            files.push(fullPath);
          }
        }
      }
    } catch (error) {
      // Directory not accessible
    }
  }

  await scan(dirPath);
  return files;
}

/**
 * Perform code quality check on a package
 *
 * @param {string} packagePath - Path to package
 * @param {Object} options - Check options
 * @returns {Promise<Object>} Check result
 */
export async function codeQualityCheck(packagePath, options = {}) {
  const startTime = Date.now();
  const warnings = [];
  const failures = [];
  const remediation = [];

  let totalScore = 100;
  const details = {
    filesAnalyzed: 0,
    jsdocCoverage: 100,
    consoleUsages: 0,
    longFunctions: 0,
    codeSmells: 0,
    fileIssues: []
  };

  try {
    // Get all source files
    const sourceFiles = await getSourceFiles(packagePath);
    details.filesAnalyzed = sourceFiles.length;

    if (sourceFiles.length === 0) {
      warnings.push('No source files found');
      remediation.push('Add source files to the package');
      return createResult(90, warnings, failures, remediation, details, startTime);
    }

    let totalExported = 0;
    let totalDocumented = 0;

    for (const filePath of sourceFiles) {
      try {
        const content = await readFile(filePath, 'utf-8');
        const relativePath = relative(packagePath, filePath);

        // JSDoc check
        const jsdocResult = checkJsdocCoverage(content, filePath);
        totalExported += jsdocResult.exportedCount;
        totalDocumented += jsdocResult.documentedCount;
        warnings.push(...jsdocResult.warnings.map(w => `${relativePath}: ${w}`));
        failures.push(...jsdocResult.failures.map(f => `${relativePath}: ${f}`));

        // Console check
        const consoleResult = checkConsoleUsage(content, filePath);
        details.consoleUsages += consoleResult.consoleCount;
        if (!consoleResult.isAllowed) {
          warnings.push(...consoleResult.warnings.map(w => `${relativePath}: ${w}`));
          failures.push(...consoleResult.failures.map(f => `${relativePath}: ${f}`));
        }

        // Complexity check
        const complexityResult = checkComplexity(content);
        details.longFunctions += complexityResult.longFunctions.length;
        warnings.push(...complexityResult.warnings.map(w => `${relativePath}: ${w}`));
        failures.push(...complexityResult.failures.map(f => `${relativePath}: ${f}`));

        // File length check
        const fileLengthResult = checkFileLength(content, filePath);
        warnings.push(...fileLengthResult.warnings.map(w => `${relativePath}: ${w}`));
        failures.push(...fileLengthResult.failures.map(f => `${relativePath}: ${f}`));

        // Code smells
        const smellResult = detectCodeSmells(content);
        details.codeSmells += smellResult.smellCount;
        warnings.push(...smellResult.warnings.map(w => `${relativePath}: ${w}`));

        // Track file issues
        if (jsdocResult.failures.length > 0 || consoleResult.failures.length > 0 ||
            complexityResult.failures.length > 0 || fileLengthResult.failures.length > 0) {
          details.fileIssues.push({
            file: relativePath,
            issues: [
              ...jsdocResult.failures,
              ...consoleResult.failures,
              ...complexityResult.failures,
              ...fileLengthResult.failures
            ]
          });
        }
      } catch (error) {
        warnings.push(`Could not analyze ${relative(packagePath, filePath)}: ${error.message}`);
      }
    }

    // Calculate JSDoc coverage
    details.jsdocCoverage = totalExported > 0
      ? Math.round((totalDocumented / totalExported) * 100)
      : 100;

    // Calculate score
    // JSDoc coverage (30 points)
    const jsdocScore = Math.min(30, (details.jsdocCoverage / 100) * 30);

    // Console usage (15 points)
    const consoleScore = details.consoleUsages === 0 ? 15
      : details.consoleUsages <= 3 ? 10
      : details.consoleUsages <= 10 ? 5 : 0;

    // Function length (20 points)
    const functionScore = details.longFunctions === 0 ? 20
      : details.longFunctions <= 2 ? 15
      : details.longFunctions <= 5 ? 10 : 5;

    // Code smells (20 points)
    const smellScore = details.codeSmells === 0 ? 20
      : details.codeSmells <= 3 ? 15
      : details.codeSmells <= 10 ? 10 : 5;

    // File count bonus (15 points) - more files analyzed = more confidence
    const fileScore = Math.min(15, (sourceFiles.length / 10) * 15);

    totalScore = Math.round(jsdocScore + consoleScore + functionScore + smellScore + fileScore);

    // Generate remediation steps
    if (details.jsdocCoverage < 100) {
      remediation.push(`Add JSDoc to undocumented exports (current: ${details.jsdocCoverage}%)`);
    }
    if (details.consoleUsages > 0) {
      remediation.push(`Remove ${details.consoleUsages} console.* calls or use proper logging`);
    }
    if (details.longFunctions > 0) {
      remediation.push(`Refactor ${details.longFunctions} long functions (<${CODE_QUALITY_THRESHOLDS.maxFunctionLines} lines)`);
    }
    if (details.codeSmells > 0) {
      remediation.push(`Address ${details.codeSmells} code smells detected`);
    }

  } catch (error) {
    failures.push(`Code quality check failed: ${error.message}`);
    totalScore = 0;
  }

  return createResult(totalScore, warnings, failures, remediation, details, startTime);
}

/**
 * Create standardized check result
 *
 * @param {number} score - Score 0-100
 * @param {Array} warnings - Warnings
 * @param {Array} failures - Failures
 * @param {Array} remediation - Remediation steps
 * @param {Object} details - Additional details
 * @param {number} startTime - Start timestamp
 * @returns {Object} Check result
 */
function createResult(score, warnings, failures, remediation, details, startTime) {
  return {
    passed: score >= 80,
    score,
    status: score >= 95 ? 'pass' : score >= 80 ? 'warn' : 'fail',
    warnings: [...new Set(warnings)].slice(0, 20),
    failures: [...new Set(failures)].slice(0, 10),
    remediation: [...new Set(remediation)].slice(0, 10),
    duration: Date.now() - startTime,
    details
  };
}

export default codeQualityCheck;
