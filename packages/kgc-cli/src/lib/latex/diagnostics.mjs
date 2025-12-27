/**
 * @fileoverview LaTeX compilation diagnostics and error surface.
 *
 * Core responsibilities:
 * - Capture and persist compilation logs
 * - Parse LaTeX error output for actionable diagnostics
 * - Provide structured error objects with context
 * - Enable silent-by-default operation with full artifact trail
 *
 * Design principles:
 * - No noisy output; all logs written to cache
 * - Predictable file paths for automation/debugging
 * - Pure functions (no OTEL in business logic)
 * - Stable missing-input detection for resolver integration
 */

import { mkdir, writeFile } from 'node:fs/promises';
import { join, dirname as _dirname } from 'node:path';
import { z } from 'zod';

/**
 * LaTeX compilation error with structured context.
 *
 * @class LatexCompileError
 * @extends Error
 */
export class LatexCompileError extends Error {
  /**
   * @param {string} message - Human-readable error message
   * @param {Object} context - Error context
   * @param {string} context.engine - LaTeX engine used (pdflatex, lualatex, xelatex)
   * @param {string} context.inputTexPath - Absolute path to .tex file
   * @param {string} context.logFilePath - Absolute path to captured log artifact
   * @param {string[]} context.missingInputs - Array of missing files detected in log
   * @param {string} [context.exitCode] - Process exit code (optional)
   */
  constructor(message, { engine, inputTexPath, logFilePath, missingInputs = [], exitCode }) {
    super(message);
    this.name = 'LatexCompileError';

    /** @type {string} LaTeX engine that failed */
    this.engine = engine;

    /** @type {string} Input .tex file path */
    this.inputTexPath = inputTexPath;

    /** @type {string} Path to written log artifact */
    this.logFilePath = logFilePath;

    /** @type {string[]} Missing files detected from log */
    this.missingInputs = missingInputs;

    /** @type {string|undefined} Process exit code */
    this.exitCode = exitCode;

    // Ensure stack trace is captured
    if (Error.captureStackTrace) {
      Error.captureStackTrace(this, LatexCompileError);
    }
  }

  /**
   * Serialize to JSON for --json output envelope.
   * @returns {Object} JSON-safe error representation
   */
  toJSON() {
    return {
      name: this.name,
      message: this.message,
      engine: this.engine,
      inputTexPath: this.inputTexPath,
      logFilePath: this.logFilePath,
      missingInputs: this.missingInputs,
      exitCode: this.exitCode
    };
  }
}

/**
 * Write LaTeX compilation log to cache directory.
 *
 * Creates deterministic log files: `${cacheDir}/runs/<timestamp>_<engine>.log`
 * Ensures `${cacheDir}/runs/` directory exists before writing.
 *
 * @param {Object} options - Log write options
 * @param {string} options.cacheDir - Base cache directory (e.g., .latex-cache)
 * @param {string} options.engine - LaTeX engine (pdflatex, lualatex, xelatex)
 * @param {string} options.inputTexPath - Path to .tex file (for metadata)
 * @param {string} options.logText - Raw log output from LaTeX process
 * @returns {Promise<string>} Absolute path to written log file
 * @throws {Error} If directory creation or file write fails
 *
 * @example
 * const logPath = await writeLatexRunLog({
 *   cacheDir: '.latex-cache',
 *   engine: 'pdflatex',
 *   inputTexPath: '/path/to/main.tex',
 *   logText: rawStderr
 * });
 * console.log(`Log written to ${logPath}`);
 */
export async function writeLatexRunLog({ cacheDir, engine, inputTexPath, logText }) {
  // Validate inputs
  const schema = z.object({
    cacheDir: z.string().min(1),
    engine: z.string().min(1),
    inputTexPath: z.string().min(1),
    logText: z.string()
  });

  const validated = schema.parse({ cacheDir, engine, inputTexPath, logText });

  // Generate timestamp: YYYYMMDD_HHMMSS (ISO-like but filesystem-safe)
  const now = new Date();
  const timestamp = now.toISOString()
    .replace(/[:.]/g, '-')    // Replace colons and dots with dashes
    .replace('T', '_')        // Replace T with underscore
    .split('.')[0];           // Remove milliseconds

  // Construct log file path
  const runsDir = join(validated.cacheDir, 'runs');
  const logFileName = `${timestamp}_${validated.engine}.log`;
  const logFilePath = join(runsDir, logFileName);

  // Ensure runs directory exists
  await mkdir(runsDir, { recursive: true });

  // Write log with metadata header
  const header = [
    `# LaTeX Compilation Log`,
    `# Engine: ${validated.engine}`,
    `# Input:  ${validated.inputTexPath}`,
    `# Time:   ${now.toISOString()}`,
    `# ================================================`,
    ''
  ].join('\n');

  const fullLog = header + validated.logText;

  await writeFile(logFilePath, fullLog, 'utf8');

  return logFilePath;
}

/**
 * Parse missing input files from LaTeX log output.
 *
 * Detects common LaTeX error patterns:
 * - `! LaTeX Error: File \`X' not found.`
 * - `! I can't find file \`X'.`
 * - `! Package X Error: File \`Y' not found.`
 * - `! File \`X' not found.`
 *
 * Returns unique list of missing filenames (duplicates removed).
 *
 * @param {string} logText - Raw LaTeX log output
 * @returns {string[]} Array of missing file names (unique, sorted)
 *
 * @example
 * const missing = parseMissingInputsFromLog(logOutput);
 * // => ['thesis.cls', 'chapter1.tex', 'logo.pdf']
 */
export function parseMissingInputsFromLog(logText) {
  if (!logText || typeof logText !== 'string') {
    return [];
  }

  const missingFiles = new Set();

  // Pattern 1: ! LaTeX Error: File `X' not found.
  const latexErrorPattern = /! LaTeX Error: File [`']([^'`]+)[''] not found\./gi;

  // Pattern 2: ! I can't find file `X'.
  const cantFindPattern = /! I can't find file [`']([^'`]+)['']\.?/gi;

  // Pattern 3: ! Package X Error: File `Y' not found.
  const packageErrorPattern = /! Package .+ Error: File [`']([^'`]+)[''] not found\./gi;

  // Pattern 4: ! File `X' not found.
  const simpleFilePattern = /! File [`']([^'`]+)[''] not found\./gi;

  // Pattern 5: (file not found)
  const parenthesisPattern = /\(([^)]+) not found\)/gi;

  // Apply all patterns
  const patterns = [
    latexErrorPattern,
    cantFindPattern,
    packageErrorPattern,
    simpleFilePattern,
    parenthesisPattern
  ];

  for (const pattern of patterns) {
    let match;
    while ((match = pattern.exec(logText)) !== null) {
      const filename = match[1].trim();
      if (filename) {
        missingFiles.add(filename);
      }
    }
  }

  // Return sorted array (deterministic output)
  return Array.from(missingFiles).sort();
}

/**
 * Extract error summary from LaTeX log.
 *
 * Finds the first critical error line for display in CLI output.
 * Returns undefined if no clear error found.
 *
 * @param {string} logText - Raw LaTeX log output
 * @returns {string|undefined} First error line or undefined
 *
 * @example
 * const errorSummary = extractErrorSummary(logOutput);
 * // => "! Undefined control sequence."
 */
export function extractErrorSummary(logText) {
  if (!logText || typeof logText !== 'string') {
    return undefined;
  }

  // Find first line starting with "!"
  const lines = logText.split('\n');
  const errorLine = lines.find(line => line.trim().startsWith('!'));

  return errorLine?.trim();
}

/**
 * Check if log indicates successful compilation.
 *
 * LaTeX can exit with 0 even if there were warnings.
 * This checks for definitive success markers.
 *
 * @param {string} logText - Raw LaTeX log output
 * @returns {boolean} True if compilation appears successful
 *
 * @example
 * const success = isCompileSuccessful(logOutput);
 * if (!success) {
 *   throw new LatexCompileError(...);
 * }
 */
export function isCompileSuccessful(logText) {
  if (!logText || typeof logText !== 'string') {
    return false;
  }

  // LaTeX success indicators:
  // - "Output written on X.pdf (N pages)"
  // - No "! " error markers
  const hasOutputWritten = /Output written on .+\.pdf \(\d+ pages/.test(logText);
  const hasCriticalError = /^!/m.test(logText);

  return hasOutputWritten && !hasCriticalError;
}

/**
 * Schema for log write options (for external validation).
 * @type {z.ZodSchema}
 */
export const LogWriteOptionsSchema = z.object({
  cacheDir: z.string().min(1).describe('Cache directory path'),
  engine: z.enum(['pdflatex', 'lualatex', 'xelatex']).describe('LaTeX engine'),
  inputTexPath: z.string().min(1).describe('Input .tex file path'),
  logText: z.string().describe('Raw log output')
});

// Alias for backward compatibility with Agent 10's interface expectations
export const writeDiagnosticLog = writeLatexRunLog;

export default {
  LatexCompileError,
  writeLatexRunLog,
  writeDiagnosticLog,
  parseMissingInputsFromLog,
  extractErrorSummary,
  isCompileSuccessful,
  LogWriteOptionsSchema
};
