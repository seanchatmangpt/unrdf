/**
 * @fileoverview SwiftLaTeX WASM Engine Wrapper
 *
 * Pure ESM implementation of LaTeX compilation using SwiftLaTeX WASM engines.
 * Supports xetex and pdftex with virtual file system integration.
 *
 * Design Principles:
 * - No system binaries required - pure WASM execution
 * - Virtual FS abstraction for complete control over inputs/outputs
 * - Multi-pass compilation for cross-references
 * - Detailed error parsing for missing input detection
 * - Graceful degradation with actionable error messages
 *
 * Integration Points:
 * - Agent 4 (Resolver): Consumes `missingInputs` array to fetch dependencies
 * - VFS Provider: Accepts Map<string, Uint8Array> for complete isolation
 * - Cache Manager: Uses `cacheDir` for intermediate artifacts
 *
 * @module swiftlatex-engine
 */

import { fileURLToPath } from 'node:url';
import { dirname, join, resolve } from 'node:path';
import { access } from 'node:fs/promises';
import { constants } from 'node:fs';
import { z } from 'zod';

// =============================================================================
// Type Definitions (JSDoc)
// =============================================================================

/**
 * @typedef {Object} CompileOptions
 * @property {'xetex'|'pdftex'} engine - TeX engine to use
 * @property {Map<string, Uint8Array>} vfs - Virtual file system (path -> content)
 * @property {string} entry - Entry point filename (e.g., 'main.tex')
 * @property {string} [cacheDir='work'] - Working directory path in VFS
 * @property {number} [passes=2] - Number of compilation passes (for cross-refs)
 * @property {boolean} [verbose=false] - Enable detailed logging
 */

/**
 * @typedef {Object} CompileResult
 * @property {boolean} ok - Compilation succeeded
 * @property {Uint8Array} [pdf] - Generated PDF bytes (if successful)
 * @property {string} [log] - Compilation log output
 * @property {Map<string, Uint8Array>} [artifacts] - Intermediate files (.aux, .toc, etc.)
 * @property {string[]} [missingInputs] - Files that were not found (for resolver)
 * @property {string} [error] - Human-readable error message
 */

/**
 * @typedef {Object} EngineInstance
 * @property {Function} setTexContent - Set input TeX content
 * @property {Function} compileLaTeX - Run compilation
 * @property {Function} flushCache - Clear caches
 * @property {Function} getFileContent - Read output file
 * @property {Function} writeMemFSFile - Write to virtual FS
 * @property {Function} readMemFSFile - Read from virtual FS
 */

// =============================================================================
// Validation Schemas
// =============================================================================

const EngineSchema = z.enum(['xetex', 'pdftex']);

const CompileOptionsSchema = z.object({
  engine: EngineSchema,
  vfs: z.instanceof(Map),
  entry: z.string().min(1),
  cacheDir: z.string().default('work'),
  passes: z.number().int().min(1).max(5).default(2),
  verbose: z.boolean().default(false)
});

// =============================================================================
// Constants
// =============================================================================

/** WASM module filenames for each engine */
const ENGINE_FILES = {
  xetex: 'xetex.wasm',
  pdftex: 'pdftex.wasm'
};

/** LaTeX error patterns for missing file detection */
const MISSING_FILE_PATTERNS = [
  /! LaTeX Error: File `([^']+)' not found/g,
  /^! I can't find file `([^']+)'/gm,
  /\(([^)]+\.(?:sty|cls|bib|bst|def|fd|cfg|clo))\s+not found/gi,
  /.*?:\d+: Package \w+ Error: File `([^']+)' not found/g,
  /No file ([^\s]+\.(?:aux|toc|lof|lot|bbl))\./g
];

/** Output file patterns to capture as artifacts */
const ARTIFACT_PATTERNS = ['.aux', '.log', '.toc', '.lof', '.lot', '.out', '.bbl', '.blg'];

// =============================================================================
// WASM Engine Loading
// =============================================================================

/**
 * Resolve vendor directory path relative to this module.
 * @returns {string} Absolute path to vendor/swiftlatex directory
 */
function getVendorPath() {
  const currentFile = fileURLToPath(import.meta.url);
  const currentDir = dirname(currentFile);
  // From: packages/kgc-cli/src/lib/latex/swiftlatex-engine.mjs
  // To:   packages/kgc-cli/vendor/swiftlatex/
  return resolve(currentDir, '../../../vendor/swiftlatex');
}

/**
 * Check if WASM file exists for the specified engine.
 * @param {'xetex'|'pdftex'} engine - Engine identifier
 * @returns {Promise<{exists: boolean, path: string}>}
 */
async function checkEngineFile(engine) {
  const vendorPath = getVendorPath();
  const wasmFile = ENGINE_FILES[engine];
  const fullPath = join(vendorPath, wasmFile);

  try {
    await access(fullPath, constants.R_OK);
    return { exists: true, path: fullPath };
  } catch {
    return { exists: false, path: fullPath };
  }
}

/**
 * Load WASM engine module (placeholder for actual SwiftLaTeX loader).
 *
 * NOTE: This is a mock implementation. Real SwiftLaTeX integration requires:
 * 1. Actual WASM binary in vendor/swiftlatex/
 * 2. JavaScript glue code (e.g., swiftlatex.js)
 * 3. Emscripten runtime initialization
 *
 * @param {'xetex'|'pdftex'} engine - Engine to load
 * @param {string} wasmPath - Path to WASM file
 * @returns {Promise<EngineInstance>}
 * @throws {Error} If engine cannot be loaded
 */
async function loadEngine(engine, wasmPath) {
  // In production, this would:
  // 1. Load the SwiftLaTeX JS glue code
  // 2. Initialize the Emscripten module
  // 3. Return the engine instance with FS access

  // For now, we return a mock structure that shows the interface
  throw new Error(
    `SwiftLaTeX WASM engine not yet integrated. ` +
    `Expected file: ${wasmPath}\n\n` +
    `To complete integration:\n` +
    `1. Download SwiftLaTeX WASM binaries to vendor/swiftlatex/\n` +
    `2. Add JavaScript glue code (swiftlatex.js)\n` +
    `3. Implement Emscripten module initialization\n` +
    `4. Replace this mock with real loader`
  );
}

// =============================================================================
// Virtual File System Operations
// =============================================================================

/**
 * Populate engine virtual FS with input files from VFS map.
 * @param {EngineInstance} engine - Loaded engine instance
 * @param {Map<string, Uint8Array>} vfs - Virtual file system
 * @param {string} workDir - Working directory in engine FS
 * @returns {Promise<void>}
 */
async function populateEngineFS(engine, vfs, workDir) {
  for (const [path, content] of vfs.entries()) {
    const targetPath = join(workDir, path);
    try {
      await engine.writeMemFSFile(targetPath, content);
    } catch (err) {
      throw new Error(`Failed to write VFS file ${path}: ${err.message}`);
    }
  }
}

/**
 * Extract output artifacts from engine FS.
 * @param {EngineInstance} engine - Loaded engine instance
 * @param {string} workDir - Working directory in engine FS
 * @param {string[]} patterns - File extensions to capture
 * @returns {Promise<Map<string, Uint8Array>>}
 */
async function extractArtifacts(engine, workDir, patterns) {
  const artifacts = new Map();

  for (const pattern of patterns) {
    // In real implementation, would list directory and match patterns
    // For now, attempt to read known files
    const filename = `main${pattern}`;
    const fullPath = join(workDir, filename);

    try {
      const content = await engine.readMemFSFile(fullPath);
      if (content && content.length > 0) {
        artifacts.set(filename, content);
      }
    } catch {
      // File doesn't exist, skip
    }
  }

  return artifacts;
}

// =============================================================================
// Error Parsing
// =============================================================================

/**
 * Parse LaTeX log to extract missing input files.
 * @param {string} log - LaTeX compilation log
 * @returns {string[]} Array of missing file paths
 */
function parseMissingInputs(log) {
  const missing = new Set();

  for (const pattern of MISSING_FILE_PATTERNS) {
    let match;
    // Reset regex state
    pattern.lastIndex = 0;

    while ((match = pattern.exec(log)) !== null) {
      const filename = match[1].trim();
      if (filename && filename.length > 0) {
        missing.add(filename);
      }
    }
  }

  return Array.from(missing).sort();
}

/**
 * Determine if compilation failed due to missing inputs.
 * @param {string} log - LaTeX compilation log
 * @returns {boolean}
 */
function hasMissingInputs(log) {
  return MISSING_FILE_PATTERNS.some(pattern => {
    pattern.lastIndex = 0;
    return pattern.test(log);
  });
}

/**
 * Extract human-readable error summary from log.
 * @param {string} log - LaTeX compilation log
 * @returns {string|null}
 */
function extractErrorSummary(log) {
  // Look for LaTeX error messages
  const errorMatch = log.match(/! .*?Error:.*$/m);
  if (errorMatch) return errorMatch[0];

  // Look for fatal errors
  const fatalMatch = log.match(/! Emergency stop/);
  if (fatalMatch) {
    const context = log.slice(Math.max(0, fatalMatch.index - 200), fatalMatch.index);
    const lastLine = context.split('\n').filter(l => l.trim()).pop();
    return lastLine || 'Emergency stop encountered';
  }

  // Look for undefined control sequence
  const undefMatch = log.match(/! Undefined control sequence\.\s*\n.*?\n(.*)/);
  if (undefMatch) return `Undefined control sequence: ${undefMatch[1]}`;

  return null;
}

// =============================================================================
// Main Compilation Function
// =============================================================================

/**
 * Compile LaTeX source using SwiftLaTeX WASM engine.
 *
 * Execution Flow:
 * 1. Validate inputs
 * 2. Check WASM engine availability
 * 3. Load engine instance
 * 4. Populate virtual FS with input files
 * 5. Run compilation (multi-pass if needed)
 * 6. Extract PDF and artifacts
 * 7. Parse errors and identify missing inputs
 *
 * Error Handling:
 * - Missing WASM files: Return actionable error with file path
 * - Missing inputs: Return `missingInputs` array for resolver
 * - Compilation errors: Return log with error summary
 *
 * @param {CompileOptions} options - Compilation options
 * @returns {Promise<CompileResult>}
 *
 * @example
 * const vfs = new Map([
 *   ['main.tex', new TextEncoder().encode('\\documentclass{article}...')],
 *   ['refs.bib', bibContent]
 * ]);
 *
 * const result = await compileWithSwiftLatex({
 *   engine: 'xetex',
 *   vfs,
 *   entry: 'main.tex',
 *   passes: 2
 * });
 *
 * if (result.ok) {
 *   console.log('PDF size:', result.pdf.length);
 * } else if (result.missingInputs?.length > 0) {
 *   console.log('Missing:', result.missingInputs);
 *   // Agent 4 would resolve these
 * }
 */
export async function compileWithSwiftLatex(options) {
  // ============= Validation =============
  let validated;
  try {
    validated = CompileOptionsSchema.parse(options);
  } catch (err) {
    return {
      ok: false,
      error: `Invalid options: ${err.message}`,
      log: err.errors ? JSON.stringify(err.errors, null, 2) : err.message
    };
  }

  const { engine, vfs, entry, cacheDir, passes, verbose } = validated;

  // ============= Engine Availability Check =============
  const { exists, path: wasmPath } = await checkEngineFile(engine);

  if (!exists) {
    return {
      ok: false,
      error: `WASM engine not found: ${engine}`,
      log: `Expected file: ${wasmPath}\n\n` +
           `SwiftLaTeX WASM binaries must be placed in vendor/swiftlatex/:\n` +
           `- xetex.wasm\n` +
           `- pdftex.wasm\n\n` +
           `Download from: https://github.com/SwiftLaTeX/SwiftLaTeX`,
      missingInputs: []
    };
  }

  // ============= Engine Loading =============
  let engineInstance;
  try {
    engineInstance = await loadEngine(engine, wasmPath);
  } catch (err) {
    return {
      ok: false,
      error: `Failed to load engine: ${engine}`,
      log: err.message,
      missingInputs: []
    };
  }

  // ============= VFS Population =============
  try {
    await populateEngineFS(engineInstance, vfs, cacheDir);
  } catch (err) {
    return {
      ok: false,
      error: 'Failed to populate virtual FS',
      log: err.message,
      missingInputs: []
    };
  }

  // ============= Multi-Pass Compilation =============
  let lastLog = '';
  let compilationSucceeded = false;

  for (let pass = 1; pass <= passes; pass++) {
    if (verbose) {
      console.log(`[SwiftLaTeX] Pass ${pass}/${passes} with ${engine}`);
    }

    try {
      const entryPath = join(cacheDir, entry);
      await engineInstance.setTexContent(entryPath);

      const result = await engineInstance.compileLaTeX();
      lastLog = result.log || '';

      // Check for compilation success (exit code 0 or PDF exists)
      if (result.status === 0 || result.pdfGenerated) {
        compilationSucceeded = true;
        if (pass === passes) {
          // Only break on final pass
          break;
        }
      } else {
        // Failed - check if it's due to missing inputs
        if (hasMissingInputs(lastLog)) {
          // Don't continue passes if files are missing
          break;
        }
      }
    } catch (err) {
      lastLog += `\n\n[Engine Error] ${err.message}`;
      break;
    }
  }

  // ============= Output Extraction =============
  const pdfPath = join(cacheDir, 'main.pdf');
  let pdfContent = null;

  try {
    pdfContent = await engineInstance.readMemFSFile(pdfPath);
  } catch (err) {
    // PDF not generated - expected if compilation failed
    if (verbose) {
      console.log('[SwiftLaTeX] No PDF generated');
    }
  }

  // Extract artifacts (.aux, .log, etc.)
  const artifacts = await extractArtifacts(engineInstance, cacheDir, ARTIFACT_PATTERNS);

  // Add log to artifacts
  const logPath = join(cacheDir, 'main.log');
  try {
    const logContent = await engineInstance.readMemFSFile(logPath);
    if (logContent && logContent.length > 0) {
      artifacts.set('main.log', logContent);
      // Override lastLog with actual log file if available
      lastLog = new TextDecoder().decode(logContent);
    }
  } catch {
    // Log file not found, use captured log
  }

  // ============= Error Analysis =============
  if (!compilationSucceeded || !pdfContent) {
    const missingInputs = parseMissingInputs(lastLog);
    const errorSummary = extractErrorSummary(lastLog);

    return {
      ok: false,
      log: lastLog,
      artifacts,
      missingInputs,
      error: errorSummary || (missingInputs.length > 0
        ? `Missing ${missingInputs.length} input file(s)`
        : 'Compilation failed - see log for details')
    };
  }

  // ============= Success =============
  return {
    ok: true,
    pdf: pdfContent,
    log: lastLog,
    artifacts
  };
}

// =============================================================================
// Utility Functions
// =============================================================================

/**
 * Get list of supported engines.
 * @returns {Promise<{engine: string, available: boolean, path: string}[]>}
 */
export async function getSupportedEngines() {
  const engines = ['xetex', 'pdftex'];
  const results = [];

  for (const engine of engines) {
    const { exists, path } = await checkEngineFile(engine);
    results.push({ engine, available: exists, path });
  }

  return results;
}

/**
 * Validate VFS for required TeX structure.
 * @param {Map<string, Uint8Array>} vfs - Virtual file system
 * @param {string} entry - Entry point filename
 * @returns {{valid: boolean, errors: string[]}}
 */
export function validateVFS(vfs, entry) {
  const errors = [];

  if (!vfs.has(entry)) {
    errors.push(`Entry file not found in VFS: ${entry}`);
  }

  // Check for common issues
  const texFiles = Array.from(vfs.keys()).filter(k => k.endsWith('.tex'));
  if (texFiles.length === 0) {
    errors.push('No .tex files found in VFS');
  }

  // Warn about potentially problematic filenames
  for (const filename of vfs.keys()) {
    if (filename.includes(' ')) {
      errors.push(`Filename contains spaces: ${filename} (may cause issues)`);
    }
    if (filename.startsWith('/')) {
      errors.push(`Absolute path in VFS: ${filename} (use relative paths)`);
    }
  }

  return {
    valid: errors.length === 0,
    errors
  };
}

/**
 * Create minimal VFS for testing.
 * @param {string} texContent - LaTeX source content
 * @returns {Map<string, Uint8Array>}
 */
export function createMinimalVFS(texContent) {
  const encoder = new TextEncoder();
  return new Map([
    ['main.tex', encoder.encode(texContent)]
  ]);
}

// =============================================================================
// Module Exports
// =============================================================================

export default compileWithSwiftLatex;
