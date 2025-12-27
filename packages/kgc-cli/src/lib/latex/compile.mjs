/**
 * LaTeX Compilation Pipeline Integrator (Agent 10)
 *
 * Orchestrates the complete LaTeX to PDF compilation pipeline:
 * - VFS collection (Agent 2)
 * - SwiftLaTeX engine execution (Agent 3)
 * - Missing input resolution (Agent 4)
 * - Lockfile management (Agent 5)
 * - Diagnostics and error reporting (Agent 6)
 *
 * @module lib/latex/compile
 */

import { promises as fs } from 'node:fs';
import { join, dirname as _dirname, isAbsolute } from 'node:path';
import { createHash } from 'node:crypto';

// ============================================================================
// INTEGRATION POINTS - Dependencies from other agents
// ============================================================================

// Agent 2: VFS Collection
// Actual: collectProjectFiles(projectRoot, options) -> Promise<Map<string, Uint8Array>>
import { collectProjectFiles } from './project-files.mjs';

// Agent 3: Engine Runner
// Actual: compileWithSwiftLatex({ engine, vfs, entry, cacheDir, passes, verbose }) -> Promise<CompileResult>
// CompileResult: { ok: boolean, pdf?: Uint8Array, log: string, artifacts?: Map, missingInputs?: string[], error?: string }
import { compileWithSwiftLatex } from './swiftlatex-engine.mjs';

// Agent 4: Resolver
// Actual: resolveMissingInputs({ missingInputs, cacheDir, ctanMirror? }) -> Promise<Map<string, Uint8Array>>
import { resolveMissingInputs, augmentVfsWithResolvedPackages } from './ctan-resolver.mjs';

// Agent 5: Lockfile Manager
// Actual exports:
// - loadLatexLock(lockPath) -> Promise<Lockfile | null>
// - saveLatexLock(lockPath, lockObj) -> Promise<void>
// - createLatexLock(engine) -> Lockfile
// - recordResolvedInput(lockObj, entry) -> void
import {
  loadLatexLock,
  saveLatexLock,
  createLatexLock,
  recordResolvedInput,
} from './latex-lock.mjs';

// Agent 6: Diagnostics
// Exports: LatexCompileError, parseMissingInputsFromLog, writeDiagnosticLog
import {
  LatexCompileError,
  parseMissingInputsFromLog as _parseMissingInputsFromLog,
  writeDiagnosticLog,
} from './diagnostics.mjs';

// ============================================================================
// CONSTANTS
// ============================================================================

/** Maximum compilation cycles before giving up */
const MAX_COMPILATION_CYCLES = 2;

/** Default LaTeX engine */
const DEFAULT_ENGINE = 'xetex'; // SwiftLaTeX supports xetex and pdftex

/** Default number of compilation passes */
const DEFAULT_PASSES = 2;

/** Default cache directory name */
const DEFAULT_CACHE_DIR = '.latex-cache';

/** Lockfile name */
const LOCKFILE_NAME = 'latex.lock.json';

// ============================================================================
// VALIDATION
// ============================================================================

/**
 * Validate compilation input parameters
 * @param {object} params - Compilation parameters
 * @throws {Error} If validation fails
 */
async function validateInputs({ inputTexPath, projectDir, cacheDir }) {
  // Validate inputTexPath exists
  try {
    const stats = await fs.stat(inputTexPath);
    if (!stats.isFile()) {
      throw new Error(`Input path is not a file: ${inputTexPath}`);
    }
  } catch (error) {
    throw new Error(`Input file not found: ${inputTexPath} - ${error.message}`);
  }

  // Validate projectDir exists
  try {
    const stats = await fs.stat(projectDir);
    if (!stats.isDirectory()) {
      throw new Error(`Project path is not a directory: ${projectDir}`);
    }
  } catch (error) {
    throw new Error(`Project directory not found: ${projectDir} - ${error.message}`);
  }

  // Ensure cacheDir is creatable
  try {
    await fs.mkdir(cacheDir, { recursive: true });
  } catch (error) {
    throw new Error(`Cannot create cache directory: ${cacheDir} - ${error.message}`);
  }
}

// ============================================================================
// DETERMINISM UTILITIES
// ============================================================================

/**
 * Sort VFS entries deterministically by key
 * @param {Map<string, Uint8Array>} vfs - Virtual file system
 * @returns {Map<string, Uint8Array>} Sorted VFS
 */
function _sortVFS(vfs) {
  const sorted = new Map();
  const keys = Array.from(vfs.keys()).sort();
  for (const key of keys) {
    sorted.set(key, vfs.get(key));
  }
  return sorted;
}

/**
 * Generate stable cache key from compilation parameters
 * @param {object} params - Compilation parameters
 * @returns {string} Cache key (hex hash)
 */
function generateCacheKey({ inputTexPath, engine, version = '1.0.0' }) {
  const hash = createHash('sha256');
  hash.update(`engine:${engine}`);
  hash.update(`version:${version}`);
  hash.update(`input:${inputTexPath}`);
  return hash.digest('hex').slice(0, 16);
}

// ============================================================================
// VFS MANAGEMENT
// ============================================================================

/**
 * Initialize VFS with project files
 * @param {string} projectDir - Project directory path
 * @returns {Promise<Map<string, Uint8Array>>} VFS map
 */
async function initializeVFS(projectDir) {
  // Agent 2: Collect all project files (already returns sorted Map)
  const vfs = await collectProjectFiles(projectDir, {
    // Use defaults from project-files.mjs
    // Include: .tex, .sty, .cls, .bib, .bst, images, etc.
    // Exclude: node_modules, build, .git, etc.
  });

  // VFS is already sorted by project-files.mjs
  return vfs;
}

// Note: VFS augmentation is handled by Agent 4's augmentVfsWithResolvedPackages()
// which mutates the VFS in place for efficiency

// ============================================================================
// COMPILATION PIPELINE
// ============================================================================

/**
 * Execute single compilation cycle
 * @param {object} params - Compilation parameters
 * @returns {Promise<object>} Compilation result from swiftlatex-engine
 */
async function executeCompilationCycle({ engine, vfs, cacheDir, passes }) {
  // Agent 3: compileWithSwiftLatex returns { ok, pdf?, log, artifacts?, missingInputs?, error? }
  return await compileWithSwiftLatex({
    engine,
    vfs,
    entry: 'main.tex', // swiftlatex-engine expects just filename, not work/main.tex
    cacheDir,
    passes,
    verbose: false,
  });
}

/**
 * Handle compilation failure with missing inputs
 * @param {object} params - Resolution parameters
 * @returns {Promise<Map<string, Uint8Array>>} Resolved inputs (VFS path -> content)
 */
async function handleMissingInputs({ missingInputs, cacheDir, lockfile }) {
  if (!missingInputs || missingInputs.length === 0) {
    // No missing inputs detected, compilation failed for other reasons
    return new Map();
  }

  // Agent 4: Resolve missing inputs (packages, classes, etc.)
  // resolveMissingInputs expects { missingInputs, cacheDir, ctanMirror? }
  const resolvedMap = await resolveMissingInputs({
    missingInputs,
    cacheDir,
  });

  // Agent 5: Update lockfile with resolved dependencies
  // recordResolvedInput expects { inputName, hash, sourceUrl?, cachedPath }
  for (const [vfsPath, content] of resolvedMap.entries()) {
    const inputName = vfsPath.split('/').pop(); // Extract filename
    const hash = createHash('sha256').update(content).digest('hex');

    recordResolvedInput(lockfile, {
      inputName,
      hash,
      cachedPath: vfsPath, // VFS path serves as identifier
    });
  }

  return resolvedMap;
}

/**
 * Compilation pipeline with automatic dependency resolution
 * @param {object} params - Pipeline parameters
 * @returns {Promise<Uint8Array>} PDF bytes
 * @throws {LatexCompileError} On compilation failure
 */
async function runCompilationPipeline({
  vfs,
  engine,
  cacheDir,
  passes,
  projectDir,
  lockfile,
}) {
  let currentVFS = vfs;
  let lastLog = '';
  let cycle = 0;

  while (cycle < MAX_COMPILATION_CYCLES) {
    cycle++;

    // Agent 3: Execute compilation
    const result = await executeCompilationCycle({
      engine,
      vfs: currentVFS,
      cacheDir,
      passes,
    });

    lastLog = result.log || '';

    if (result.ok && result.pdf) {
      // SUCCESS: Save lockfile and return PDF
      const lockfilePath = join(cacheDir, LOCKFILE_NAME);
      await saveLatexLock(lockfilePath, lockfile);

      return result.pdf;
    }

    // FAILURE: Check if we can resolve missing inputs
    if (cycle === MAX_COMPILATION_CYCLES) {
      // Max cycles reached, give up
      break;
    }

    // Attempt to resolve missing inputs
    const resolvedInputs = await handleMissingInputs({
      missingInputs: result.missingInputs || [],
      cacheDir,
      lockfile,
    });

    if (resolvedInputs.size === 0) {
      // No missing inputs to resolve, compilation failed for other reasons
      break;
    }

    // Augment VFS with resolved inputs and retry
    // augmentVfsWithResolvedPackages mutates the VFS in place (Agent 4 helper)
    augmentVfsWithResolvedPackages(currentVFS, resolvedInputs);

    // Save intermediate lockfile state
    const lockfilePath = join(cacheDir, LOCKFILE_NAME);
    await saveLatexLock(lockfilePath, lockfile);
  }

  // FINAL FAILURE: Write diagnostic log and throw
  const logFilePath = await writeDiagnosticLog({
    log: lastLog,
    projectDir,
    timestamp: new Date().toISOString(),
  });

  throw new LatexCompileError(
    `LaTeX compilation failed after ${cycle} cycles. See log: ${logFilePath}`,
    { logFilePath, cycles: cycle, lastLog }
  );
}

// ============================================================================
// MAIN EXPORT
// ============================================================================

/**
 * Compile LaTeX project to PDF with automatic dependency resolution
 *
 * Pipeline flow:
 * 1. Validate inputs (file exists, cache creatable)
 * 2. Initialize VFS from project files (Agent 2)
 * 3. Load or create lockfile (Agent 5)
 * 4. Execute compilation cycles (max 2):
 *    a. Compile with SwiftLaTeX (Agent 3)
 *    b. If missing inputs, resolve them (Agent 4, Agent 6)
 *    c. Augment VFS and retry
 * 5. On success: Update lockfile, return PDF bytes
 * 6. On failure: Write diagnostics (Agent 6), throw error
 *
 * @param {object} params - Compilation parameters
 * @param {string} params.inputTexPath - Absolute path to main .tex file
 * @param {string} params.projectDir - Absolute path to project directory
 * @param {string} [params.engine='xetex'] - LaTeX engine (xetex, pdftex)
 * @param {string} [params.cacheDir] - Cache directory (default: projectDir/.latex-cache)
 * @param {number} [params.passes=2] - Number of compilation passes (for cross-refs, ToC, etc.)
 * @returns {Promise<Uint8Array>} PDF file bytes
 * @throws {LatexCompileError} On compilation failure (includes log file path)
 * @throws {Error} On validation failure
 *
 * @example
 * ```js
 * const pdfBytes = await compileLatexToPdf({
 *   inputTexPath: '/absolute/path/to/main.tex',
 *   projectDir: '/absolute/path/to/project',
 *   engine: 'xetex',
 *   passes: 2,
 * });
 * await fs.writeFile('output.pdf', pdfBytes);
 * ```
 */
export async function compileLatexToPdf({
  inputTexPath,
  projectDir,
  engine = DEFAULT_ENGINE,
  cacheDir,
  passes = DEFAULT_PASSES,
}) {
  // Ensure absolute paths
  const absInputPath = isAbsolute(inputTexPath)
    ? inputTexPath
    : join(process.cwd(), inputTexPath);

  const absProjectDir = isAbsolute(projectDir)
    ? projectDir
    : join(process.cwd(), projectDir);

  const absCacheDir = cacheDir
    ? (isAbsolute(cacheDir) ? cacheDir : join(process.cwd(), cacheDir))
    : join(absProjectDir, DEFAULT_CACHE_DIR);

  // Step A: Validate inputs
  await validateInputs({
    inputTexPath: absInputPath,
    projectDir: absProjectDir,
    cacheDir: absCacheDir,
  });

  // Step B: Initialize VFS (Agent 2)
  const vfs = await initializeVFS(absProjectDir);

  // Step C: Load or create lockfile (Agent 5)
  const lockfilePath = join(absCacheDir, LOCKFILE_NAME);
  let lockfile = await loadLatexLock(lockfilePath);
  if (!lockfile) {
    lockfile = createLatexLock(engine);
  }

  // Steps D-F: Execute compilation pipeline
  const pdfBytes = await runCompilationPipeline({
    vfs,
    engine,
    cacheDir: absCacheDir,
    passes,
    projectDir: absProjectDir,
    lockfile,
  });

  return pdfBytes;
}

/**
 * Generate deterministic cache key for compilation
 * Exported for testing and cache management
 *
 * @param {object} params - Cache key parameters
 * @param {string} params.inputTexPath - Path to input file
 * @param {string} params.engine - LaTeX engine
 * @param {string} [params.version='1.0.0'] - Compiler version
 * @returns {string} Hex hash (16 chars)
 */
export { generateCacheKey };
