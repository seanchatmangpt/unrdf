/**
 * @fileoverview LaTeX Diagnostics Module Index
 *
 * Central export point for all diagnostic utilities:
 * - Log parsing and error extraction
 * - CLI formatting and display
 * - Diagnostic schemas and validation
 *
 * Re-exports:
 * - parseLatexLog() - Main parsing function
 * - formatDiagnosticsForCLI() - Terminal output formatting
 * - createDiagnosticSummary() - Summary generation
 * - DiagnosticSchema, ParseResultSchema - Zod validation schemas
 *
 * Usage:
 * ```javascript
 * import { parseLatexLog, formatDiagnosticsForCLI } from './diagnostics/index.mjs';
 *
 * const result = parseLatexLog(logText);
 * console.log(formatDiagnosticsForCLI(result.diagnostics));
 * ```
 *
 * @module diagnostics
 */

export {
  parseLatexLog,
  formatDiagnosticsForCLI,
  createDiagnosticSummary,
  DiagnosticSchema,
  ParseResultSchema
} from './parse-log.mjs';

// Default export for convenience
export { default } from './parse-log.mjs';
