/**
 * @fileoverview Compatibility Check - Validates runtime and platform compatibility
 *
 * **Checks performed**:
 * 1. Node.js version requirements (>=18)
 * 2. ES module compatibility
 * 3. TypeScript compatibility
 * 4. Deno compatibility indicators
 * 5. Browser compatibility (for web packages)
 * 6. Engine requirements specification
 * 7. Export map configuration
 * 8. Type definitions availability
 *
 * **Scoring**:
 * - 100: Excellent compatibility
 * - 95-99: Good compatibility with minor gaps
 * - 80-94: Acceptable compatibility
 * - 60-79: Compatibility improvements needed
 * - <60: Critical compatibility issues
 *
 * @module validation/checks/compatibility-check
 */

import { readdir, readFile, stat, access } from 'node:fs/promises';
import { join, extname, relative } from 'node:path';

/**
 * Compatibility thresholds
 */
export const COMPATIBILITY_THRESHOLDS = {
  minNodeVersion: 18,
  recommendedNodeVersion: 20
};

/**
 * Node.js APIs that require specific versions
 */
const NODE_VERSION_APIS = {
  18: [
    'fetch',
    'crypto.webcrypto',
    'navigator',
    'structuredClone',
    'AbortSignal.timeout'
  ],
  20: [
    'import.meta.resolve',
    'test',
    'WebSocketStream'
  ],
  21: [
    'WebSocket'
  ]
};

/**
 * ESM-only APIs/patterns
 */
const ESM_PATTERNS = {
  esmImport: /import\s+(?:\{[^}]+\}|\*\s+as\s+\w+|\w+)\s+from\s+['"][^'"]+['"]/g,
  esmExport: /export\s+(?:default|{|\*|const|let|var|function|class|async)/g,
  dynamicImport: /import\s*\(\s*['"][^'"]+['"]\s*\)/g,
  topLevelAwait: /^(?:const|let|var)?\s*\w+\s*=\s*await\s+/gm,
  importMeta: /import\.meta\./g
};

/**
 * CJS patterns that might cause issues
 */
const CJS_PATTERNS = {
  require: /require\s*\(\s*['"][^'"]+['"]\s*\)/g,
  moduleExports: /module\.exports\s*=/g,
  exportsProperty: /exports\.\w+\s*=/g,
  dirname: /__dirname/g,
  filename: /__filename/g
};

/**
 * Deno compatibility indicators
 */
const DENO_PATTERNS = {
  denoJson: 'deno.json',
  denoLock: 'deno.lock',
  npmSpecifier: /from\s+['"]npm:/g,
  denoLand: /from\s+['"]https:\/\/deno\.land/g
};

/**
 * Browser-specific APIs that won't work in Node
 */
const BROWSER_ONLY_APIS = [
  'document',
  'window',
  'localStorage',
  'sessionStorage',
  'navigator',
  'history',
  'location',
  'alert',
  'confirm',
  'prompt',
  'XMLHttpRequest'
];

/**
 * Parse Node version from engines field
 *
 * @param {string} versionSpec - Version specification
 * @returns {number|null} Minimum version number
 */
function parseNodeVersion(versionSpec) {
  if (!versionSpec) return null;

  // Handle >=X.Y.Z, ^X.Y.Z, ~X.Y.Z, X.Y.Z
  const match = versionSpec.match(/[>=~^]*(\d+)/);
  return match ? parseInt(match[1], 10) : null;
}

/**
 * Check package.json for compatibility settings
 *
 * @param {string} packagePath - Package path
 * @returns {Promise<Object>} Package compatibility info
 */
async function checkPackageJsonCompatibility(packagePath) {
  const result = {
    hasType: false,
    isESM: false,
    hasEngines: false,
    nodeVersion: null,
    hasExports: false,
    hasMain: false,
    hasModule: false,
    hasTypes: false,
    hasBrowser: false,
    issues: [],
    goodPractices: []
  };

  try {
    const content = await readFile(join(packagePath, 'package.json'), 'utf-8');
    const pkg = JSON.parse(content);

    // Check type field
    result.hasType = 'type' in pkg;
    result.isESM = pkg.type === 'module';

    if (!result.hasType) {
      result.issues.push('Missing "type" field in package.json');
    } else if (result.isESM) {
      result.goodPractices.push('ES module package');
    }

    // Check engines
    result.hasEngines = Boolean(pkg.engines?.node);
    if (result.hasEngines) {
      result.nodeVersion = parseNodeVersion(pkg.engines.node);
      result.goodPractices.push('Engine requirements specified');

      if (result.nodeVersion && result.nodeVersion < COMPATIBILITY_THRESHOLDS.minNodeVersion) {
        result.issues.push(`Node ${result.nodeVersion} specified, recommend >= ${COMPATIBILITY_THRESHOLDS.minNodeVersion}`);
      }
    } else {
      result.issues.push('No engines.node specified');
    }

    // Check exports
    result.hasExports = Boolean(pkg.exports);
    if (result.hasExports) {
      result.goodPractices.push('Export map configured');

      // Check for dual package (CJS + ESM)
      if (typeof pkg.exports === 'object') {
        const hasImport = Object.values(pkg.exports).some(v =>
          typeof v === 'object' && v.import
        );
        const hasRequire = Object.values(pkg.exports).some(v =>
          typeof v === 'object' && v.require
        );

        if (hasImport && hasRequire) {
          result.goodPractices.push('Dual package (ESM + CJS)');
        }
      }
    }

    // Check main/module
    result.hasMain = Boolean(pkg.main);
    result.hasModule = Boolean(pkg.module);

    // Check types
    result.hasTypes = Boolean(pkg.types || pkg.typings);
    if (result.hasTypes) {
      result.goodPractices.push('TypeScript types included');
    }

    // Check browser field
    result.hasBrowser = Boolean(pkg.browser);
    if (result.hasBrowser) {
      result.goodPractices.push('Browser field configured');
    }

  } catch (error) {
    result.issues.push('Cannot read package.json');
  }

  return result;
}

/**
 * Analyze source files for compatibility patterns
 *
 * @param {string} packagePath - Package path
 * @returns {Promise<Object>} Source analysis result
 */
async function analyzeSourceCompatibility(packagePath) {
  const result = {
    filesAnalyzed: 0,
    esmFiles: 0,
    cjsFiles: 0,
    mixedFiles: 0,
    topLevelAwait: 0,
    importMeta: 0,
    cjsGlobals: 0,
    browserAPIs: 0,
    issues: [],
    patterns: {}
  };

  async function analyzeFile(filePath) {
    try {
      const content = await readFile(filePath, 'utf-8');
      const relativePath = relative(packagePath, filePath);

      let hasESM = false;
      let hasCJS = false;

      // Check ESM patterns
      if (ESM_PATTERNS.esmImport.test(content) || ESM_PATTERNS.esmExport.test(content)) {
        hasESM = true;
      }

      // Check CJS patterns
      if (CJS_PATTERNS.require.test(content) || CJS_PATTERNS.moduleExports.test(content)) {
        hasCJS = true;
      }

      if (hasESM && !hasCJS) {
        result.esmFiles++;
      } else if (hasCJS && !hasESM) {
        result.cjsFiles++;
      } else if (hasESM && hasCJS) {
        result.mixedFiles++;
        result.issues.push(`Mixed ESM/CJS: ${relativePath}`);
      }

      // Check top-level await
      if (ESM_PATTERNS.topLevelAwait.test(content)) {
        result.topLevelAwait++;
      }

      // Check import.meta usage
      if (ESM_PATTERNS.importMeta.test(content)) {
        result.importMeta++;
      }

      // Check CJS globals in ESM files
      if (hasESM) {
        if (CJS_PATTERNS.dirname.test(content)) {
          result.cjsGlobals++;
          result.issues.push(`__dirname used in ESM: ${relativePath}`);
        }
        if (CJS_PATTERNS.filename.test(content)) {
          result.cjsGlobals++;
          result.issues.push(`__filename used in ESM: ${relativePath}`);
        }
      }

      // Check browser-only APIs (for Node packages)
      for (const api of BROWSER_ONLY_APIS) {
        const regex = new RegExp(`\\b${api}\\b(?!['"\\w])`, 'g');
        if (regex.test(content) && !relativePath.includes('.test.') && !relativePath.includes('.spec.')) {
          // Could be intentional for browser builds
          if (!content.includes(`typeof ${api}`) && !content.includes(`globalThis.${api}`)) {
            result.browserAPIs++;
          }
        }
      }

      result.filesAnalyzed++;
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
          if (/\.(mjs|js|ts|jsx|tsx)$/.test(entry.name) && !entry.name.includes('.d.ts')) {
            await analyzeFile(fullPath);
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
 * Check for Deno compatibility
 *
 * @param {string} packagePath - Package path
 * @returns {Promise<Object>} Deno compatibility result
 */
async function checkDenoCompatibility(packagePath) {
  const result = {
    hasDenoConfig: false,
    hasDenoLock: false,
    usesNpmSpecifiers: false,
    isDenoCompatible: false
  };

  // Check for deno.json
  try {
    await access(join(packagePath, 'deno.json'));
    result.hasDenoConfig = true;
    result.isDenoCompatible = true;
  } catch {
    try {
      await access(join(packagePath, 'deno.jsonc'));
      result.hasDenoConfig = true;
      result.isDenoCompatible = true;
    } catch {
      // No Deno config
    }
  }

  // Check for deno.lock
  try {
    await access(join(packagePath, 'deno.lock'));
    result.hasDenoLock = true;
  } catch {
    // No Deno lock
  }

  return result;
}

/**
 * Check TypeScript configuration
 *
 * @param {string} packagePath - Package path
 * @returns {Promise<Object>} TypeScript config result
 */
async function checkTypeScriptConfig(packagePath) {
  const result = {
    hasTsConfig: false,
    hasDeclarations: false,
    target: null,
    module: null,
    strict: false
  };

  try {
    const content = await readFile(join(packagePath, 'tsconfig.json'), 'utf-8');
    result.hasTsConfig = true;

    // Parse tsconfig (might have comments, so be careful)
    const cleanContent = content.replace(/\/\*[\s\S]*?\*\/|\/\/.*/g, '');
    const config = JSON.parse(cleanContent);

    const options = config.compilerOptions || {};
    result.target = options.target;
    result.module = options.module;
    result.strict = options.strict === true;
    result.hasDeclarations = options.declaration === true;
  } catch {
    // No tsconfig or invalid
  }

  // Check for .d.ts files
  if (!result.hasDeclarations) {
    try {
      const entries = await readdir(packagePath);
      result.hasDeclarations = entries.some(e => e.endsWith('.d.ts'));
    } catch {
      // Directory not readable
    }
  }

  return result;
}

/**
 * Perform compatibility check on a package
 *
 * @param {string} packagePath - Path to package
 * @param {Object} options - Check options
 * @returns {Promise<Object>} Check result
 */
export async function compatibilityCheck(packagePath, options = {}) {
  const startTime = Date.now();
  const warnings = [];
  const failures = [];
  const remediation = [];

  let totalScore = 100;
  const details = {
    isESM: false,
    nodeVersion: null,
    hasExports: false,
    hasTypes: false,
    filesAnalyzed: 0,
    moduleFormat: 'unknown',
    denoCompatible: false,
    issues: []
  };

  try {
    // Package.json analysis
    const pkgCompat = await checkPackageJsonCompatibility(packagePath);
    details.isESM = pkgCompat.isESM;
    details.nodeVersion = pkgCompat.nodeVersion;
    details.hasExports = pkgCompat.hasExports;
    details.hasTypes = pkgCompat.hasTypes;

    // Source file analysis
    const sourceCompat = await analyzeSourceCompatibility(packagePath);
    details.filesAnalyzed = sourceCompat.filesAnalyzed;

    // Determine module format
    if (sourceCompat.esmFiles > 0 && sourceCompat.cjsFiles === 0) {
      details.moduleFormat = 'esm';
    } else if (sourceCompat.cjsFiles > 0 && sourceCompat.esmFiles === 0) {
      details.moduleFormat = 'cjs';
    } else if (sourceCompat.esmFiles > 0 && sourceCompat.cjsFiles > 0) {
      details.moduleFormat = 'mixed';
    }

    // Deno compatibility
    const denoCompat = await checkDenoCompatibility(packagePath);
    details.denoCompatible = denoCompat.isDenoCompatible;

    // TypeScript config
    const tsCompat = await checkTypeScriptConfig(packagePath);

    // Collect issues
    details.issues = [...pkgCompat.issues, ...sourceCompat.issues.slice(0, 5)];

    // Generate warnings and failures
    if (!pkgCompat.hasType) {
      warnings.push('Missing "type" field in package.json');
      remediation.push('Add "type": "module" for ESM or "type": "commonjs" for CJS');
    }

    if (!pkgCompat.hasEngines) {
      warnings.push('No Node.js version requirement specified');
      remediation.push(`Add "engines": { "node": ">=${COMPATIBILITY_THRESHOLDS.minNodeVersion}" }`);
    } else if (pkgCompat.nodeVersion && pkgCompat.nodeVersion < COMPATIBILITY_THRESHOLDS.minNodeVersion) {
      warnings.push(`Node ${pkgCompat.nodeVersion} specified, ${COMPATIBILITY_THRESHOLDS.minNodeVersion}+ recommended`);
    }

    if (!pkgCompat.hasExports && sourceCompat.filesAnalyzed > 0) {
      warnings.push('No exports map in package.json');
      remediation.push('Add "exports" field for better module resolution');
    }

    if (!pkgCompat.hasTypes && !tsCompat.hasDeclarations) {
      warnings.push('No TypeScript types available');
      remediation.push('Add "types" field or include .d.ts files');
    }

    if (sourceCompat.mixedFiles > 0) {
      failures.push(`${sourceCompat.mixedFiles} file(s) mix ESM and CJS syntax`);
      remediation.push('Use consistent module format across all files');
    }

    if (sourceCompat.cjsGlobals > 0 && pkgCompat.isESM) {
      warnings.push(`${sourceCompat.cjsGlobals} uses of __dirname/__filename in ESM`);
      remediation.push('Use import.meta.url with fileURLToPath for ESM');
    }

    if (details.moduleFormat === 'cjs' && pkgCompat.isESM) {
      failures.push('Package.json declares ESM but source uses CJS');
    }

    // Calculate score
    // Module format clarity (30 points)
    let moduleScore = 30;
    if (!pkgCompat.hasType) moduleScore -= 10;
    if (sourceCompat.mixedFiles > 0) moduleScore -= 15;
    if (details.moduleFormat === 'cjs' && pkgCompat.isESM) moduleScore -= 15;
    moduleScore = Math.max(0, moduleScore);

    // Node version specification (20 points)
    let nodeScore = 20;
    if (!pkgCompat.hasEngines) nodeScore -= 10;
    else if (pkgCompat.nodeVersion && pkgCompat.nodeVersion < COMPATIBILITY_THRESHOLDS.minNodeVersion) nodeScore -= 5;

    // Exports map (20 points)
    let exportsScore = 20;
    if (!pkgCompat.hasExports && sourceCompat.filesAnalyzed > 0) exportsScore -= 10;
    if (!pkgCompat.hasMain && !pkgCompat.hasExports) exportsScore -= 5;

    // TypeScript support (15 points)
    let tsScore = 15;
    if (!pkgCompat.hasTypes && !tsCompat.hasDeclarations) tsScore -= 10;
    if (tsCompat.hasTsConfig && tsCompat.strict) tsScore += 2;

    // Good practices bonus (15 points)
    let practiceScore = 0;
    practiceScore += pkgCompat.goodPractices.length * 3;
    if (details.moduleFormat === 'esm') practiceScore += 3;
    if (denoCompat.isDenoCompatible) practiceScore += 2;
    practiceScore = Math.min(15, practiceScore);

    totalScore = Math.round(moduleScore + nodeScore + exportsScore + tsScore + practiceScore);
    totalScore = Math.max(0, Math.min(100, totalScore));

  } catch (error) {
    failures.push(`Compatibility check failed: ${error.message}`);
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

export default compatibilityCheck;
