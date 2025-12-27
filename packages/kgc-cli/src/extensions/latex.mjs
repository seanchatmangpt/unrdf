/**
 * @fileoverview LaTeX CLI extension - PDF compilation using internal WASM engine.
 *
 * Provides commands for:
 * - Building LaTeX documents to PDF (no external TeX required)
 * - Diagnosing compilation errors with structured output
 * - Managing compilation cache and lockfiles
 * - Bundling resolved dependencies for offline builds
 *
 * Exit codes:
 * - 0: Success
 * - 1: Compilation failed (with diagnostics)
 * - 2: Invalid arguments
 * - 3: Missing dependencies
 */

import { z } from 'zod';
import { resolve, join, dirname } from 'node:path';
import { promises as fs, existsSync, createReadStream } from 'node:fs';
import { createHash } from 'node:crypto';

// ============================================================================
// Library imports
// ============================================================================

import { compileLatexToPdf } from '../lib/latex/compile.mjs';
import {
  parseMissingInputsFromLog,
  extractErrorSummary,
  isCompileSuccessful,
  LatexCompileError
} from '../lib/latex/diagnostics.mjs';
import {
  loadLatexLock,
  saveLatexLock,
  createLatexLock,
  validateCachedFile,
  isLockValid,
  getResolvedInput
} from '../lib/latex/latex-lock.mjs';

// ============================================================================
// Zod schemas for command arguments
// ============================================================================

/**
 * Schema for latex build command
 */
const BuildSchema = z.object({
  entry: z.string().min(1).describe('Path to main .tex file (e.g., main.tex)'),
  out: z.string().optional().default('out.pdf').describe('Output PDF path'),
  root: z.string().optional().describe('Project root directory (defaults to dirname(entry))'),
  mode: z.enum(['offline', 'fetch']).optional().default('fetch').describe('Dependency resolution mode'),
  lockfile: z.string().optional().default('latex.lock.json').describe('Lockfile path'),
  engine: z.enum(['xetex', 'pdftex', 'luatex']).optional().default('xetex').describe('LaTeX engine'),
  passes: z.number().int().min(1).max(5).optional().default(2).describe('Number of compilation passes'),
  cacheDir: z.string().optional().default('.latex-cache').describe('Cache directory')
});

/**
 * Schema for latex diagnose command
 */
const DiagnoseSchema = z.object({
  entry: z.string().min(1).describe('Path to main .tex file'),
  root: z.string().optional().describe('Project root directory'),
  cacheDir: z.string().optional().default('.latex-cache').describe('Cache directory'),
  lastRun: z.boolean().optional().default(false).describe('Show diagnostics from last run log')
});

/**
 * Schema for latex cache add command
 */
const CacheAddSchema = z.object({
  lockfile: z.string().optional().default('latex.lock.json').describe('Lockfile path'),
  input: z.string().min(1).describe('Input file to add to cache'),
  source: z.string().optional().describe('Source URL or path'),
  cacheDir: z.string().optional().default('.latex-cache').describe('Cache directory')
});

/**
 * Schema for latex cache verify command
 */
const CacheVerifySchema = z.object({
  lockfile: z.string().optional().default('latex.lock.json').describe('Lockfile path'),
  cacheDir: z.string().optional().default('.latex-cache').describe('Cache directory'),
  fix: z.boolean().optional().default(false).describe('Automatically fix issues')
});

/**
 * Schema for latex bundle make command
 */
const BundleMakeSchema = z.object({
  lockfile: z.string().optional().default('latex.lock.json').describe('Lockfile path'),
  out: z.string().min(1).describe('Output bundle directory'),
  cacheDir: z.string().optional().default('.latex-cache').describe('Cache directory'),
  includeCache: z.boolean().optional().default(true).describe('Include cache files in bundle')
});

// ============================================================================
// Helper functions
// ============================================================================

/**
 * Calculate SHA-256 hash of file
 * @param {string} filePath - Path to file
 * @returns {Promise<string>} Hex hash
 */
async function hashFile(filePath) {
  const hash = createHash('sha256');
  const stream = createReadStream(filePath);

  for await (const chunk of stream) {
    hash.update(chunk);
  }

  return hash.digest('hex');
}

/**
 * Format file size in human-readable format
 * @param {number} bytes - Size in bytes
 * @returns {string} Formatted size
 */
function formatSize(bytes) {
  if (bytes < 1024) return `${bytes} B`;
  if (bytes < 1024 * 1024) return `${(bytes / 1024).toFixed(1)} KB`;
  return `${(bytes / (1024 * 1024)).toFixed(1)} MB`;
}

/**
 * Create structured error for CLI output
 * @param {string} code - Error code
 * @param {string} message - Error message
 * @param {Object} details - Additional details
 * @returns {Error} Error with code and details properties
 */
function createCliError(code, message, details = {}) {
  const error = new Error(message);
  error.code = code;
  error.details = details;
  return error;
}

// ============================================================================
// Command handlers
// ============================================================================

/**
 * Handle latex build command
 * @param {Object} args - Validated arguments from BuildSchema
 * @returns {Promise<Object>} Result object
 */
async function handleBuild(args) {
  const startTime = Date.now();

  // Resolve paths
  const inputTexPath = resolve(args.entry);
  const projectRoot = args.root ? resolve(args.root) : dirname(inputTexPath);
  const outputPath = resolve(args.out);
  const cacheDir = resolve(projectRoot, args.cacheDir);
  const lockfilePath = resolve(cacheDir, args.lockfile);

  // Validate input file exists
  if (!existsSync(inputTexPath)) {
    throw createCliError('MISSING_INPUT', `Input file not found: ${inputTexPath}`, {
      path: inputTexPath
    });
  }

  try {
    // Call Agent 10's compile module
    const pdfBytes = await compileLatexToPdf({
      inputTexPath,
      projectDir: projectRoot,
      engine: args.engine,
      cacheDir,
      passes: args.passes
    });

    // Write output PDF
    await fs.mkdir(dirname(outputPath), { recursive: true });
    await fs.writeFile(outputPath, pdfBytes);

    const duration = Date.now() - startTime;

    return {
      success: true,
      output: outputPath,
      size: pdfBytes.length,
      sizeFormatted: formatSize(pdfBytes.length),
      duration,
      engine: args.engine,
      passes: args.passes,
      lockfile: lockfilePath,
      message: `✓ Compiled successfully in ${(duration / 1000).toFixed(2)}s`
    };
  } catch (error) {
    if (error instanceof LatexCompileError) {
      throw createCliError('COMPILATION_FAILED', error.message, {
        engine: error.engine,
        inputTexPath: error.inputTexPath,
        logFilePath: error.logFilePath,
        missingInputs: error.missingInputs,
        exitCode: error.exitCode
      });
    }
    throw error;
  }
}

/**
 * Handle latex diagnose command
 * @param {Object} args - Validated arguments from DiagnoseSchema
 * @returns {Promise<Object>} Diagnostic result
 */
async function handleDiagnose(args) {
  const inputTexPath = resolve(args.entry);
  const projectRoot = args.root ? resolve(args.root) : dirname(inputTexPath);
  const cacheDir = resolve(projectRoot, args.cacheDir);
  const runsDir = join(cacheDir, 'runs');

  // Check if input file exists
  const inputExists = existsSync(inputTexPath);

  // Find latest log file if requested
  let logAnalysis = null;
  if (args.lastRun) {
    try {
      const logFiles = await fs.readdir(runsDir);
      if (logFiles.length > 0) {
        // Sort by name (timestamp-based) and get latest
        const latestLog = logFiles.sort().reverse()[0];
        const logPath = join(runsDir, latestLog);
        const logContent = await fs.readFile(logPath, 'utf-8');

        const missingInputs = parseMissingInputsFromLog(logContent);
        const errorSummary = extractErrorSummary(logContent);
        const success = isCompileSuccessful(logContent);

        logAnalysis = {
          logFile: logPath,
          success,
          errorSummary,
          missingInputs,
          missingCount: missingInputs.length
        };
      }
    } catch (err) {
      // Ignore errors reading logs
    }
  }

  // Check cache status
  let cacheStatus = null;
  try {
    const lockfilePath = join(cacheDir, 'latex.lock.json');
    const lockfile = await loadLatexLock(lockfilePath);

    if (lockfile) {
      const inputCount = Object.keys(lockfile.resolvedInputs).length;
      cacheStatus = {
        lockfileExists: true,
        lockfilePath,
        engine: lockfile.engine,
        resolvedInputs: inputCount,
        lastUpdated: lockfile.updatedAt
      };
    } else {
      cacheStatus = {
        lockfileExists: false,
        message: 'No lockfile found - first build will resolve dependencies'
      };
    }
  } catch (err) {
    cacheStatus = {
      error: err.message
    };
  }

  return {
    input: {
      path: inputTexPath,
      exists: inputExists,
      projectRoot
    },
    cache: cacheStatus,
    lastRun: logAnalysis,
    recommendations: generateRecommendations({ inputExists, logAnalysis, cacheStatus })
  };
}

/**
 * Generate actionable recommendations based on diagnostics
 * @param {Object} diagnostics - Diagnostic data
 * @returns {string[]} Array of recommendations
 */
function generateRecommendations({ inputExists, logAnalysis, cacheStatus }) {
  const recommendations = [];

  if (!inputExists) {
    recommendations.push('Create the main .tex file before building');
  }

  if (logAnalysis && !logAnalysis.success) {
    if (logAnalysis.missingInputs.length > 0) {
      recommendations.push(`Resolve ${logAnalysis.missingInputs.length} missing inputs: ${logAnalysis.missingInputs.join(', ')}`);
      recommendations.push('Run build again to auto-fetch from CTAN');
    }
    if (logAnalysis.errorSummary) {
      recommendations.push(`Fix error: ${logAnalysis.errorSummary}`);
    }
  }

  if (cacheStatus && !cacheStatus.lockfileExists) {
    recommendations.push('Run build to generate lockfile for reproducible builds');
  }

  if (recommendations.length === 0) {
    recommendations.push('No issues detected - ready to build');
  }

  return recommendations;
}

/**
 * Handle latex cache add command
 * @param {Object} args - Validated arguments from CacheAddSchema
 * @returns {Promise<Object>} Result
 */
async function handleCacheAdd(args) {
  const cacheDir = resolve(args.cacheDir);
  const lockfilePath = resolve(cacheDir, args.lockfile);
  const inputPath = resolve(args.input);

  // Validate input exists
  if (!existsSync(inputPath)) {
    throw createCliError('MISSING_INPUT', `Input file not found: ${inputPath}`, {
      path: inputPath
    });
  }

  // Load or create lockfile
  let lockfile = await loadLatexLock(lockfilePath);
  if (!lockfile) {
    lockfile = createLatexLock('xetex'); // Default engine
  }

  // Read and hash the input file
  const content = await fs.readFile(inputPath);
  const hash = createHash('sha256').update(content).digest('hex');
  const inputName = inputPath.split('/').pop();

  // Copy to cache
  const cacheFilesDir = join(cacheDir, 'files');
  await fs.mkdir(cacheFilesDir, { recursive: true });
  const cachedPath = join(cacheFilesDir, `${hash.slice(0, 16)}_${inputName}`);
  await fs.copyFile(inputPath, cachedPath);

  // Update lockfile
  const resolvedInput = {
    hash,
    cachedPath,
    resolvedAt: new Date().toISOString()
  };

  if (args.source) {
    resolvedInput.sourceUrl = args.source;
  }

  lockfile.resolvedInputs[inputName] = resolvedInput;

  // Save lockfile
  await saveLatexLock(lockfilePath, lockfile);

  return {
    success: true,
    inputName,
    hash: hash.slice(0, 16),
    cachedPath,
    lockfile: lockfilePath,
    message: `✓ Added ${inputName} to cache`
  };
}

/**
 * Handle latex cache verify command
 * @param {Object} args - Validated arguments from CacheVerifySchema
 * @returns {Promise<Object>} Verification result
 */
async function handleCacheVerify(args) {
  const cacheDir = resolve(args.cacheDir);
  const lockfilePath = resolve(cacheDir, args.lockfile);

  // Load lockfile
  const lockfile = await loadLatexLock(lockfilePath);
  if (!lockfile) {
    throw createCliError('MISSING_LOCKFILE', `Lockfile not found: ${lockfilePath}`, {
      path: lockfilePath,
      hint: 'Run build command to generate lockfile'
    });
  }

  const issues = [];
  const verified = [];
  let fixed = 0;

  // Verify each resolved input
  for (const [inputName, entry] of Object.entries(lockfile.resolvedInputs)) {
    const cachedPath = entry.cachedPath;

    // Check if cached file exists
    if (!existsSync(cachedPath)) {
      issues.push({
        inputName,
        issue: 'MISSING_FILE',
        message: `Cached file not found: ${cachedPath}`,
        severity: 'error'
      });

      if (args.fix) {
        // Remove from lockfile
        delete lockfile.resolvedInputs[inputName];
        fixed++;
      }
      continue;
    }

    // Verify hash
    try {
      const actualHash = await hashFile(cachedPath);
      if (!validateCachedFile(entry, actualHash)) {
        issues.push({
          inputName,
          issue: 'HASH_MISMATCH',
          message: `Hash mismatch for ${inputName}`,
          expected: entry.hash,
          actual: actualHash,
          severity: 'error'
        });

        if (args.fix) {
          // Remove corrupted entry
          delete lockfile.resolvedInputs[inputName];
          fixed++;
        }
      } else {
        verified.push({
          inputName,
          hash: entry.hash.slice(0, 16),
          path: cachedPath
        });
      }
    } catch (err) {
      issues.push({
        inputName,
        issue: 'VERIFICATION_ERROR',
        message: `Error verifying ${inputName}: ${err.message}`,
        severity: 'warning'
      });
    }
  }

  // Save lockfile if fixes were applied
  if (args.fix && fixed > 0) {
    await saveLatexLock(lockfilePath, lockfile);
  }

  const totalEntries = verified.length + issues.length;
  const hasErrors = issues.some(i => i.severity === 'error');

  return {
    success: !hasErrors,
    lockfile: lockfilePath,
    engine: lockfile.engine,
    totalEntries,
    verified: verified.length,
    issues: issues.length,
    fixed: args.fix ? fixed : 0,
    verifiedInputs: verified,
    issuesFound: issues,
    message: hasErrors
      ? `✗ Verification failed: ${issues.length} issues found`
      : `✓ All ${verified.length} cache entries verified`
  };
}

/**
 * Handle latex bundle make command
 * @param {Object} args - Validated arguments from BundleMakeSchema
 * @returns {Promise<Object>} Bundle result
 */
async function handleBundleMake(args) {
  const cacheDir = resolve(args.cacheDir);
  const lockfilePath = resolve(cacheDir, args.lockfile);
  const bundleDir = resolve(args.out);

  // Load lockfile
  const lockfile = await loadLatexLock(lockfilePath);
  if (!lockfile) {
    throw createCliError('MISSING_LOCKFILE', `Lockfile not found: ${lockfilePath}`, {
      path: lockfilePath,
      hint: 'Run build command first to generate lockfile'
    });
  }

  // Create bundle directory
  await fs.mkdir(bundleDir, { recursive: true });

  const bundledFiles = [];
  let totalSize = 0;

  // Copy lockfile
  const bundleLockfilePath = join(bundleDir, 'latex.lock.json');
  await fs.copyFile(lockfilePath, bundleLockfilePath);

  // Copy resolved inputs if requested
  if (args.includeCache) {
    const bundleCacheDir = join(bundleDir, 'cache');
    await fs.mkdir(bundleCacheDir, { recursive: true });

    for (const [inputName, entry] of Object.entries(lockfile.resolvedInputs)) {
      const sourcePath = entry.cachedPath;

      if (!existsSync(sourcePath)) {
        bundledFiles.push({
          inputName,
          status: 'skipped',
          reason: 'file not found'
        });
        continue;
      }

      const destPath = join(bundleCacheDir, inputName);
      await fs.copyFile(sourcePath, destPath);

      const stats = await fs.stat(destPath);
      totalSize += stats.size;

      bundledFiles.push({
        inputName,
        status: 'bundled',
        size: stats.size,
        hash: entry.hash.slice(0, 16)
      });
    }
  }

  // Create README
  const readme = [
    '# LaTeX Compilation Bundle',
    '',
    `Generated: ${new Date().toISOString()}`,
    `Engine: ${lockfile.engine}`,
    `Resolved inputs: ${Object.keys(lockfile.resolvedInputs).length}`,
    '',
    '## Contents',
    '- latex.lock.json: Lockfile with dependency metadata',
    args.includeCache ? '- cache/: Cached dependency files' : '',
    '',
    '## Usage',
    '1. Copy this bundle to target machine',
    '2. Extract to project directory',
    '3. Run build with --mode offline flag'
  ].join('\n');

  await fs.writeFile(join(bundleDir, 'README.md'), readme, 'utf-8');

  return {
    success: true,
    bundleDir,
    lockfile: bundleLockfilePath,
    filesCount: bundledFiles.filter(f => f.status === 'bundled').length,
    totalSize,
    totalSizeFormatted: formatSize(totalSize),
    bundledFiles,
    message: `✓ Bundle created at ${bundleDir}`
  };
}

// ============================================================================
// Extension definition
// ============================================================================

/**
 * LaTeX extension definition.
 * @type {Object}
 */
const extension = {
  id: '@unrdf/latex',
  description: 'LaTeX compilation using internal WASM engine',

  nouns: {
    latex: {
      description: 'LaTeX document compilation and management',
      verbs: {
        build: {
          description: 'Compile LaTeX document to PDF using internal WASM engine',
          meta: {
            args: {
              entry: {
                type: 'string',
                description: 'Path to main .tex file (e.g., main.tex)',
                required: true
              },
              out: {
                type: 'string',
                description: 'Output PDF path',
                default: 'out.pdf'
              },
              root: {
                type: 'string',
                description: 'Project root directory (defaults to dirname(entry))'
              },
              mode: {
                type: 'string',
                description: 'Dependency resolution mode (offline|fetch)',
                default: 'fetch'
              },
              lockfile: {
                type: 'string',
                description: 'Lockfile path',
                default: 'latex.lock.json'
              },
              engine: {
                type: 'string',
                description: 'LaTeX engine (xetex|pdftex|luatex)',
                default: 'xetex'
              },
              passes: {
                type: 'string',
                description: 'Number of compilation passes (1-5)',
                default: '2'
              },
              cacheDir: {
                type: 'string',
                description: 'Cache directory',
                default: '.latex-cache'
              }
            }
          },
          handler: async (rawArgs, ctx) => {
            try {
              // Convert and validate with Zod
              const args = BuildSchema.parse({
                ...rawArgs,
                passes: rawArgs.passes ? parseInt(rawArgs.passes, 10) : 2
              });
              return await handleBuild(args);
            } catch (error) {
              // Exit with appropriate code
              if (error.code === 'COMPILATION_FAILED') {
                process.exitCode = 1;
              } else if (error.code === 'INVALID_ARGS') {
                process.exitCode = 2;
              } else if (error.code === 'MISSING_INPUT' || error.code === 'MISSING_LOCKFILE') {
                process.exitCode = 3;
              }
              throw error;
            }
          }
        },

        diagnose: {
          description: 'Diagnose LaTeX compilation issues and show cache status',
          meta: {
            args: {
              entry: {
                type: 'string',
                description: 'Path to main .tex file',
                required: true
              },
              root: {
                type: 'string',
                description: 'Project root directory'
              },
              cacheDir: {
                type: 'string',
                description: 'Cache directory',
                default: '.latex-cache'
              },
              lastRun: {
                type: 'boolean',
                description: 'Show diagnostics from last run log',
                default: false
              }
            }
          },
          handler: async (rawArgs) => {
            const args = DiagnoseSchema.parse(rawArgs);
            return await handleDiagnose(args);
          }
        }
      }
    },

    // Nested cache commands using compound noun pattern
    'latex-cache': {
      description: 'LaTeX cache management',
      verbs: {
        add: {
          description: 'Add input file to cache manually',
          meta: {
            args: {
              input: {
                type: 'string',
                description: 'Input file to add to cache',
                required: true
              },
              lockfile: {
                type: 'string',
                description: 'Lockfile path',
                default: 'latex.lock.json'
              },
              source: {
                type: 'string',
                description: 'Source URL or path'
              },
              cacheDir: {
                type: 'string',
                description: 'Cache directory',
                default: '.latex-cache'
              }
            }
          },
          handler: async (rawArgs) => {
            try {
              const args = CacheAddSchema.parse(rawArgs);
              return await handleCacheAdd(args);
            } catch (error) {
              if (error.code === 'MISSING_INPUT') {
                process.exitCode = 3;
              }
              throw error;
            }
          }
        },

        verify: {
          description: 'Verify cache integrity against lockfile',
          meta: {
            args: {
              lockfile: {
                type: 'string',
                description: 'Lockfile path',
                default: 'latex.lock.json'
              },
              cacheDir: {
                type: 'string',
                description: 'Cache directory',
                default: '.latex-cache'
              },
              fix: {
                type: 'boolean',
                description: 'Automatically fix issues',
                default: false
              }
            }
          },
          handler: async (rawArgs) => {
            try {
              const args = CacheVerifySchema.parse(rawArgs);
              return await handleCacheVerify(args);
            } catch (error) {
              if (error.code === 'MISSING_LOCKFILE') {
                process.exitCode = 3;
              }
              throw error;
            }
          }
        }
      }
    },

    'latex-bundle': {
      description: 'LaTeX bundle operations',
      verbs: {
        make: {
          description: 'Create portable bundle with lockfile and cache',
          meta: {
            args: {
              out: {
                type: 'string',
                description: 'Output bundle directory',
                required: true
              },
              lockfile: {
                type: 'string',
                description: 'Lockfile path',
                default: 'latex.lock.json'
              },
              cacheDir: {
                type: 'string',
                description: 'Cache directory',
                default: '.latex-cache'
              },
              includeCache: {
                type: 'boolean',
                description: 'Include cache files in bundle',
                default: true
              }
            }
          },
          handler: async (rawArgs) => {
            try {
              const args = BundleMakeSchema.parse(rawArgs);
              return await handleBundleMake(args);
            } catch (error) {
              if (error.code === 'MISSING_LOCKFILE') {
                process.exitCode = 3;
              }
              throw error;
            }
          }
        }
      }
    }
  },

  priority: 15 // High priority - core infrastructure
};

export default extension;
