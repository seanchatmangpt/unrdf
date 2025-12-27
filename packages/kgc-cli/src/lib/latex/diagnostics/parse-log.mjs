/**
 * @fileoverview LaTeX Log Parser - Comprehensive Diagnostic Extraction
 *
 * Core responsibilities:
 * - Parse LaTeX compilation logs for errors, warnings, and info messages
 * - Extract structured diagnostics with file/line context
 * - Detect missing inputs (packages, classes, fonts, bibliography files)
 * - Identify rerun-needed hints for cross-references
 * - Provide actionable suggestions for common errors
 *
 * Design principles:
 * - Pure functions (no OTEL in business logic)
 * - Zod validation for all output structures
 * - Pattern-based detection with comprehensive coverage
 * - Deterministic output (sorted, deduplicated)
 *
 * Integration:
 * - Called by compile.mjs after engine execution
 * - Used by CLI for `kgc latex diagnose` command
 * - Missing inputs fed to Agent 4's resolver
 *
 * @module diagnostics/parse-log
 */

import { z } from 'zod';

// =============================================================================
// Type Definitions & Schemas
// =============================================================================

/**
 * Diagnostic severity levels matching LSP conventions.
 * @typedef {'error'|'warning'|'info'} Severity
 */

/**
 * Structured diagnostic entry with context and suggestions.
 *
 * @typedef {Object} Diagnostic
 * @property {'error'|'warning'|'info'} severity - Diagnostic level
 * @property {string} code - Error code (e.g., 'MISSING_STY', 'UNDEFINED_CONTROL')
 * @property {string} message - Human-readable error message
 * @property {string} [file] - Source file where error occurred
 * @property {number} [line] - Line number in source file
 * @property {string} [suggestion] - Actionable fix suggestion
 * @property {string} [raw] - Raw log excerpt for debugging
 */

/**
 * Complete parse result with all diagnostics and metadata.
 *
 * @typedef {Object} ParseResult
 * @property {Diagnostic[]} diagnostics - All diagnostic entries
 * @property {string[]} missingInputs - Missing files to resolve
 * @property {boolean} rerunNeeded - Whether to rerun compilation
 * @property {boolean} success - Overall compilation success
 * @property {number} errors - Count of error-level diagnostics
 * @property {number} warnings - Count of warning-level diagnostics
 */

/**
 * Zod schema for diagnostic validation.
 */
export const DiagnosticSchema = z.object({
  severity: z.enum(['error', 'warning', 'info']),
  code: z.string().min(1),
  message: z.string().min(1),
  file: z.string().optional(),
  line: z.number().int().positive().optional(),
  suggestion: z.string().optional(),
  raw: z.string().optional()
});

/**
 * Zod schema for parse result validation.
 */
export const ParseResultSchema = z.object({
  diagnostics: z.array(DiagnosticSchema),
  missingInputs: z.array(z.string()),
  rerunNeeded: z.boolean(),
  success: z.boolean(),
  errors: z.number().int().nonnegative(),
  warnings: z.number().int().nonnegative()
});

// =============================================================================
// Error Pattern Definitions
// =============================================================================

/**
 * LaTeX error patterns with associated metadata.
 * Each pattern includes:
 * - regex: Pattern to match in log
 * - code: Error code identifier
 * - severity: Diagnostic level
 * - extractMessage: Function to extract message from match
 * - extractContext: Function to extract file/line info
 * - suggestion: Fixed or dynamic suggestion
 */
const ERROR_PATTERNS = [
  // Missing package/class files
  {
    regex: /! LaTeX Error: File [`']([^'`]+\.(?:sty|cls|def|fd|cfg|clo))[''] not found\./gi,
    code: 'MISSING_PACKAGE',
    severity: 'error',
    extractMessage: (match) => `File '${match[1]}' not found`,
    extractFile: (match) => match[1],
    suggestion: (match) => `Install package providing ${match[1]} or add to project`
  },

  // Missing input files (user files)
  {
    regex: /! LaTeX Error: File [`']([^'`]+\.tex)[''] not found\./gi,
    code: 'MISSING_INPUT',
    severity: 'error',
    extractMessage: (match) => `Input file '${match[1]}' not found`,
    extractFile: (match) => match[1],
    suggestion: (match) => `Create file ${match[1]} or fix \\input/\\include path`
  },

  // Missing graphics files
  {
    regex: /! Package [\w.]+ Error: File [`'']([^'`'']+\.(?:pdf|png|jpg|jpeg|eps|svg))[''] not found/gi,
    code: 'MISSING_GRAPHIC',
    severity: 'error',
    extractMessage: (match) => `Graphic file '${match[1]}' not found`,
    extractFile: (match) => match[1],
    suggestion: (match) => `Add ${match[1]} to project or fix file path`
  },

  // Can't find file (alternative syntax)
  {
    regex: /! I can't find file [`']([^'`]+)['']\.?/gi,
    code: 'FILE_NOT_FOUND',
    severity: 'error',
    extractMessage: (match) => `Cannot find file '${match[1]}'`,
    extractFile: (match) => match[1],
    suggestion: (match) => {
      const ext = match[1].split('.').pop();
      if (ext === 'sty' || ext === 'cls') {
        return `Install package ${match[1].replace(/\.[^.]+$/, '')}`;
      }
      return `Check file path and name: ${match[1]}`;
    }
  },

  // Undefined control sequence
  {
    regex: /! Undefined control sequence\.\s*\nl\.\d+\s+\\([a-zA-Z]+)/gm,
    code: 'UNDEFINED_CONTROL',
    severity: 'error',
    extractMessage: (match) => `Undefined command: \\${match[1]}`,
    suggestion: (match) => {
      const cmd = match[1];
      const commonFixes = {
        'cite': 'Load package: \\usepackage{cite} or \\usepackage{natbib}',
        'includegraphics': 'Load package: \\usepackage{graphicx}',
        'includegraphix': 'Load package: \\usepackage{graphicx}', // Common typo
        'textcolor': 'Load package: \\usepackage{xcolor}',
        'url': 'Load package: \\usepackage{url} or \\usepackage{hyperref}',
        'SI': 'Load package: \\usepackage{siunitx}',
        'toprule': 'Load package: \\usepackage{booktabs}'
      };

      // Exact match
      if (commonFixes[cmd]) {
        return commonFixes[cmd];
      }

      // Fuzzy match for common patterns
      if (cmd.includes('graphic')) {
        return 'Load package: \\usepackage{graphicx}';
      }
      if (cmd.includes('color') || cmd.includes('colour')) {
        return 'Load package: \\usepackage{xcolor}';
      }
      if (cmd.includes('cite') || cmd.includes('ref')) {
        return 'Load package: \\usepackage{cite} or \\usepackage{natbib}';
      }

      return `Check command spelling or load required package for \\${cmd}`;
    }
  },

  // Missing font files
  {
    regex: /Font.*?=\s*([^\s]+)\s+not found/gi,
    code: 'MISSING_FONT',
    severity: 'error',
    extractMessage: (match) => `Font '${match[1]}' not found`,
    suggestion: () => 'Install required font package or use different font'
  },

  // Package errors (general)
  {
    regex: /! Package (\w+) Error: (.+?)(?:\.|$)/gi,
    code: 'PACKAGE_ERROR',
    severity: 'error',
    extractMessage: (match) => `Package ${match[1]}: ${match[2]}`,
    suggestion: (match) => `Check ${match[1]} package documentation`
  },

  // Emergency stop
  {
    regex: /! Emergency stop\./gi,
    code: 'EMERGENCY_STOP',
    severity: 'error',
    extractMessage: () => 'Emergency stop - critical error encountered',
    suggestion: () => 'Review preceding errors in log for root cause'
  },

  // Missing auxiliary files (not found pattern)
  {
    regex: /\(([^\s)]+\.(?:aux|toc|lof|lot|bbl))\s+not found\)/gi,
    code: 'MISSING_AUX',
    severity: 'info',
    extractMessage: (match) => `Auxiliary file '${match[1]}' not found (first run)`,
    extractFile: (match) => match[1],
    suggestion: () => 'This is normal on first compilation - will be created'
  },

  // File line error pattern (newer format)
  {
    regex: /^(.+?):(\d+):\s*(.+)$/gm,
    code: 'FILE_LINE_ERROR',
    severity: 'error',
    extractMessage: (match) => match[3],
    extractFile: (match) => match[1],
    extractLine: (match) => parseInt(match[2], 10),
    suggestion: () => 'Check syntax at indicated line'
  }
];

/**
 * Warning patterns for non-fatal issues.
 */
const WARNING_PATTERNS = [
  // Overfull/underfull hbox
  {
    regex: /(?:Overfull|Underfull) \\hbox \((.+?)\) in paragraph at lines (\d+)--(\d+)/gi,
    code: 'BADNESS_HBOX',
    severity: 'warning',
    extractMessage: (match) => `${match[0].startsWith('Over') ? 'Overfull' : 'Underfull'} hbox (${match[1]}) at lines ${match[2]}-${match[3]}`,
    extractLine: (match) => parseInt(match[2], 10),
    suggestion: () => 'Adjust text formatting or allow line breaking'
  },

  // Citation warnings
  {
    regex: /LaTeX Warning: Citation [`']([^'`]+)[''] (?:on page (\d+) )?undefined/gi,
    code: 'UNDEFINED_CITATION',
    severity: 'warning',
    extractMessage: (match) => `Citation '${match[1]}' undefined`,
    extractLine: (match) => match[2] ? parseInt(match[2], 10) : undefined,
    suggestion: (match) => `Add entry for '${match[1]}' in bibliography or run BibTeX`
  },

  // Reference warnings
  {
    regex: /LaTeX Warning: Reference [`']([^'`]+)[''] (?:on page (\d+) )?undefined/gi,
    code: 'UNDEFINED_REFERENCE',
    severity: 'warning',
    extractMessage: (match) => `Reference '${match[1]}' undefined`,
    extractLine: (match) => match[2] ? parseInt(match[2], 10) : undefined,
    suggestion: () => 'Check \\label{} exists and rerun compilation'
  },

  // Package warnings
  {
    regex: /Package (\w+) Warning: (.+?)(?:\.|$)/gi,
    code: 'PACKAGE_WARNING',
    severity: 'warning',
    extractMessage: (match) => `Package ${match[1]}: ${match[2]}`,
    suggestion: (match) => `Review ${match[1]} package options`
  },

  // Font shape warnings
  {
    regex: /LaTeX Font Warning: (.+)/gi,
    code: 'FONT_WARNING',
    severity: 'warning',
    extractMessage: (match) => match[1],
    suggestion: () => 'Font substitution occurred - output may differ from expected'
  }
];

/**
 * Info patterns for rerun hints and non-critical messages.
 */
const INFO_PATTERNS = [
  // Rerun to get cross-references right
  {
    regex: /LaTeX Warning: Label\(s\) may have changed\. Rerun to get cross-references right\./gi,
    code: 'RERUN_NEEDED',
    severity: 'info',
    extractMessage: () => 'Labels changed - rerun needed for cross-references',
    suggestion: () => 'Run compilation again to resolve references',
    rerunHint: true
  },

  // Rerun LaTeX (general)
  {
    regex: /(?:Rerun LaTeX|Please rerun LaTeX)/gi,
    code: 'RERUN_NEEDED',
    severity: 'info',
    extractMessage: (match) => match[0],
    suggestion: () => 'Run compilation again',
    rerunHint: true
  },

  // Table of contents rerun
  {
    regex: /No file (.+?\.toc)\./gi,
    code: 'TOC_RERUN',
    severity: 'info',
    extractMessage: (match) => `Table of contents file '${match[1]}' will be created`,
    suggestion: () => 'Rerun to generate table of contents',
    rerunHint: true
  }
];

// =============================================================================
// Core Parsing Functions
// =============================================================================

/**
 * Apply pattern set to log text and extract diagnostics.
 *
 * @param {string} log - LaTeX log text
 * @param {Object[]} patterns - Pattern definitions
 * @returns {Diagnostic[]} Extracted diagnostics
 */
function applyPatterns(log, patterns) {
  const diagnostics = [];

  for (const pattern of patterns) {
    // Reset regex state
    pattern.regex.lastIndex = 0;

    let match;
    while ((match = pattern.regex.exec(log)) !== null) {
      const diagnostic = {
        severity: pattern.severity,
        code: pattern.code,
        message: pattern.extractMessage(match),
        raw: match[0].slice(0, 200) // Limit raw excerpt
      };

      // Extract file context if available
      if (pattern.extractFile) {
        diagnostic.file = pattern.extractFile(match);
      }

      // Extract line context if available
      if (pattern.extractLine) {
        const line = pattern.extractLine(match);
        if (line && !isNaN(line)) {
          diagnostic.line = line;
        }
      }

      // Add suggestion (can be string or function)
      if (pattern.suggestion) {
        diagnostic.suggestion = typeof pattern.suggestion === 'function'
          ? pattern.suggestion(match)
          : pattern.suggestion;
      }

      // Track rerun hints
      if (pattern.rerunHint) {
        diagnostic.rerunHint = true;
      }

      diagnostics.push(diagnostic);
    }
  }

  return diagnostics;
}

/**
 * Extract missing input files from diagnostics.
 *
 * @param {Diagnostic[]} diagnostics - All diagnostics
 * @returns {string[]} Unique missing file paths
 */
function extractMissingInputs(diagnostics) {
  const missing = new Set();

  const fileCodes = new Set([
    'MISSING_PACKAGE',
    'MISSING_INPUT',
    'MISSING_GRAPHIC',
    'FILE_NOT_FOUND'
  ]);

  for (const diag of diagnostics) {
    if (fileCodes.has(diag.code) && diag.file) {
      missing.add(diag.file);
    }
  }

  return Array.from(missing).sort();
}

/**
 * Check if rerun is needed based on diagnostics.
 *
 * @param {Diagnostic[]} diagnostics - All diagnostics
 * @returns {boolean} True if rerun recommended
 */
function checkRerunNeeded(diagnostics) {
  return diagnostics.some(d => d.rerunHint === true);
}

/**
 * Deduplicate diagnostics by message+file+line key.
 *
 * @param {Diagnostic[]} diagnostics - Raw diagnostics
 * @returns {Diagnostic[]} Deduplicated diagnostics
 */
function deduplicateDiagnostics(diagnostics) {
  const seen = new Map();
  const unique = [];

  for (const diag of diagnostics) {
    const key = `${diag.code}:${diag.message}:${diag.file || ''}:${diag.line || ''}`;

    if (!seen.has(key)) {
      seen.set(key, true);
      unique.push(diag);
    }
  }

  return unique;
}

/**
 * Check if compilation was successful based on log content.
 *
 * @param {string} log - LaTeX log text
 * @param {number} errorCount - Count of error-level diagnostics
 * @returns {boolean} True if compilation succeeded
 */
function checkCompilationSuccess(log, errorCount) {
  // If we found critical errors, compilation failed
  if (errorCount > 0) {
    return false;
  }

  // Check for success markers
  const hasOutputWritten = /Output written on .+\.pdf \(\d+ pages/.test(log);
  const hasEmergencyStop = /! Emergency stop/.test(log);

  return hasOutputWritten && !hasEmergencyStop;
}

// =============================================================================
// Public API
// =============================================================================

/**
 * Parse LaTeX compilation log for diagnostics.
 *
 * Extracts:
 * - Errors (missing files, undefined commands, package errors)
 * - Warnings (citations, references, formatting issues)
 * - Info messages (rerun hints, auxiliary file creation)
 *
 * Returns structured diagnostic data with:
 * - Severity levels (error/warning/info)
 * - Error codes (MISSING_PACKAGE, UNDEFINED_CONTROL, etc.)
 * - File and line context where available
 * - Actionable suggestions
 * - Missing input files for resolver
 * - Rerun recommendation
 *
 * @param {string} log - Raw LaTeX log output
 * @returns {ParseResult} Structured diagnostics
 * @throws {z.ZodError} If output validation fails
 *
 * @example
 * const result = parseLatexLog(logText);
 * console.log(`Found ${result.errors} errors, ${result.warnings} warnings`);
 * console.log(`Missing files:`, result.missingInputs);
 * console.log(`Rerun needed:`, result.rerunNeeded);
 *
 * for (const diag of result.diagnostics) {
 *   console.log(`[${diag.severity}] ${diag.message}`);
 *   if (diag.suggestion) {
 *     console.log(`  Suggestion: ${diag.suggestion}`);
 *   }
 * }
 */
export function parseLatexLog(log) {
  // Validate input
  if (!log || typeof log !== 'string') {
    return ParseResultSchema.parse({
      diagnostics: [],
      missingInputs: [],
      rerunNeeded: false,
      success: false,
      errors: 0,
      warnings: 0
    });
  }

  // Apply all pattern sets
  const errorDiags = applyPatterns(log, ERROR_PATTERNS);
  const warningDiags = applyPatterns(log, WARNING_PATTERNS);
  const infoDiags = applyPatterns(log, INFO_PATTERNS);

  // Combine and deduplicate
  const allDiagnostics = [
    ...errorDiags,
    ...warningDiags,
    ...infoDiags
  ];

  const diagnostics = deduplicateDiagnostics(allDiagnostics);

  // Extract metadata
  const missingInputs = extractMissingInputs(diagnostics);
  const rerunNeeded = checkRerunNeeded(diagnostics);

  // Count by severity
  const errors = diagnostics.filter(d => d.severity === 'error').length;
  const warnings = diagnostics.filter(d => d.severity === 'warning').length;

  // Determine overall success
  const success = checkCompilationSuccess(log, errors);

  // Construct result
  const result = {
    diagnostics,
    missingInputs,
    rerunNeeded,
    success,
    errors,
    warnings
  };

  // Validate output schema
  return ParseResultSchema.parse(result);
}

/**
 * Format diagnostics for CLI output (human-readable).
 *
 * Produces colored, indented output suitable for terminal display.
 * Groups diagnostics by severity and provides summary.
 *
 * @param {Diagnostic[]} diagnostics - Diagnostics to format
 * @param {Object} [options] - Formatting options
 * @param {boolean} [options.colors=true] - Use ANSI colors
 * @param {boolean} [options.verbose=false] - Include raw log excerpts
 * @returns {string} Formatted diagnostic output
 *
 * @example
 * const formatted = formatDiagnosticsForCLI(result.diagnostics);
 * console.log(formatted);
 *
 * @example
 * const formatted = formatDiagnosticsForCLI(result.diagnostics, {
 *   colors: false,
 *   verbose: true
 * });
 */
export function formatDiagnosticsForCLI(diagnostics, options = {}) {
  const { colors = true, verbose = false } = options;

  // ANSI color codes
  const c = colors ? {
    red: '\x1b[31m',
    yellow: '\x1b[33m',
    blue: '\x1b[34m',
    gray: '\x1b[90m',
    bold: '\x1b[1m',
    reset: '\x1b[0m'
  } : {
    red: '', yellow: '', blue: '', gray: '', bold: '', reset: ''
  };

  if (diagnostics.length === 0) {
    return `${c.blue}No diagnostics found${c.reset}`;
  }

  const lines = [];

  // Group by severity
  const errors = diagnostics.filter(d => d.severity === 'error');
  const warnings = diagnostics.filter(d => d.severity === 'warning');
  const infos = diagnostics.filter(d => d.severity === 'info');

  // Format errors
  if (errors.length > 0) {
    lines.push(`${c.bold}${c.red}Errors (${errors.length}):${c.reset}`);
    for (const diag of errors) {
      lines.push(formatDiagnostic(diag, c, verbose));
    }
    lines.push('');
  }

  // Format warnings
  if (warnings.length > 0) {
    lines.push(`${c.bold}${c.yellow}Warnings (${warnings.length}):${c.reset}`);
    for (const diag of warnings) {
      lines.push(formatDiagnostic(diag, c, verbose));
    }
    lines.push('');
  }

  // Format info
  if (infos.length > 0) {
    lines.push(`${c.bold}${c.blue}Info (${infos.length}):${c.reset}`);
    for (const diag of infos) {
      lines.push(formatDiagnostic(diag, c, verbose));
    }
    lines.push('');
  }

  // Summary
  lines.push(`${c.bold}Summary:${c.reset} ${errors.length} errors, ${warnings.length} warnings, ${infos.length} info`);

  return lines.join('\n');
}

/**
 * Format single diagnostic entry.
 *
 * @param {Diagnostic} diag - Diagnostic to format
 * @param {Object} c - Color codes
 * @param {boolean} verbose - Include raw excerpts
 * @returns {string} Formatted diagnostic
 * @private
 */
function formatDiagnostic(diag, c, verbose) {
  const lines = [];

  // Main message with location
  const location = diag.file
    ? `${diag.file}${diag.line ? `:${diag.line}` : ''}`
    : '';

  const prefix = {
    error: `${c.red}✗${c.reset}`,
    warning: `${c.yellow}⚠${c.reset}`,
    info: `${c.blue}ℹ${c.reset}`
  }[diag.severity];

  lines.push(`  ${prefix} ${diag.message}`);

  if (location) {
    lines.push(`    ${c.gray}at ${location}${c.reset}`);
  }

  if (diag.suggestion) {
    lines.push(`    ${c.blue}→${c.reset} ${diag.suggestion}`);
  }

  if (verbose && diag.raw) {
    lines.push(`    ${c.gray}Raw: ${diag.raw.slice(0, 100)}...${c.reset}`);
  }

  return lines.join('\n');
}

/**
 * Create summary statistics from parse result.
 *
 * @param {ParseResult} result - Parse result
 * @returns {string} Human-readable summary
 *
 * @example
 * const summary = createDiagnosticSummary(parseResult);
 * console.log(summary);
 * // => "Compilation failed: 3 errors, 5 warnings. Missing 2 files. Rerun needed."
 */
export function createDiagnosticSummary(result) {
  const parts = [];

  parts.push(result.success ? 'Compilation succeeded' : 'Compilation failed');

  if (result.errors > 0 || result.warnings > 0) {
    parts.push(`${result.errors} errors, ${result.warnings} warnings`);
  }

  if (result.missingInputs.length > 0) {
    parts.push(`Missing ${result.missingInputs.length} files`);
  }

  if (result.rerunNeeded) {
    parts.push('Rerun needed');
  }

  return parts.join('. ') + '.';
}

// =============================================================================
// Module Exports
// =============================================================================

export default {
  parseLatexLog,
  formatDiagnosticsForCLI,
  createDiagnosticSummary,
  DiagnosticSchema,
  ParseResultSchema
};
