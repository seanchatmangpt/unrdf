/**
 * @fileoverview Tests for LaTeX Log Parser
 *
 * Verifies:
 * - Pattern detection accuracy
 * - Structured output format
 * - Zod schema validation
 * - Missing input extraction
 * - Rerun detection
 * - CLI formatting
 *
 * Test strategy:
 * - Real LaTeX log excerpts
 * - Edge cases (empty logs, malformed input)
 * - All error patterns
 * - Deduplication logic
 */

import { describe, it } from 'node:test';
import assert from 'node:assert/strict';
import {
  parseLatexLog,
  formatDiagnosticsForCLI,
  createDiagnosticSummary,
  DiagnosticSchema,
  ParseResultSchema
} from '../parse-log.mjs';

// =============================================================================
// Test Fixtures - Real LaTeX Log Excerpts
// =============================================================================

const MISSING_PACKAGE_LOG = `
! LaTeX Error: File 'thesis.cls' not found.

Type X to quit or <RETURN> to proceed,
or enter new name. (Default extension: cls)

Enter file name:
! Emergency stop.
`;

const UNDEFINED_CONTROL_LOG = `
! Undefined control sequence.
l.42 \\includegraphix
                    {logo.pdf}
?
`;

const MISSING_GRAPHIC_LOG = `
! Package pdftex.def Error: File 'figure1.pdf' not found.

See the pdftex.def package documentation for explanation.
`;

const RERUN_NEEDED_LOG = `
LaTeX Warning: Label(s) may have changed. Rerun to get cross-references right.

Output written on main.pdf (5 pages, 123456 bytes).
`;

const CITATION_WARNING_LOG = `
LaTeX Warning: Citation 'smith2020' on page 3 undefined on input line 142.

[3]
`;

const OVERFULL_HBOX_LOG = `
Overfull \\hbox (12.34567pt too wide) in paragraph at lines 89--91
 []\\OT1/cmr/m/n/10 This is a very long line that does not fit within the mar-
`;

const SUCCESS_LOG = `
Output written on main.pdf (10 pages, 234567 bytes).
PDF statistics:
 123 PDF objects out of 1000 (max. 8388607)
 89 compressed objects within 1 object stream
`;

const MULTI_ERROR_LOG = `
! LaTeX Error: File 'custom.sty' not found.

Type X to quit or <RETURN> to proceed,
or enter new name. (Default extension: sty)

! Undefined control sequence.
l.15 \\customcommand
                    {test}

! Package graphicx Error: File 'plot.pdf' not found.

LaTeX Warning: Citation 'doe2019' undefined.

LaTeX Warning: Label(s) may have changed. Rerun to get cross-references right.
`;

// =============================================================================
// Core Parsing Tests
// =============================================================================

describe('parseLatexLog', () => {
  it('should parse missing package error', () => {
    const result = parseLatexLog(MISSING_PACKAGE_LOG);

    assert.ok(result.diagnostics.length > 0, 'Should find diagnostics');

    const packageError = result.diagnostics.find(d => d.code === 'MISSING_PACKAGE');
    assert.ok(packageError, 'Should detect MISSING_PACKAGE error');
    assert.strictEqual(packageError.severity, 'error');
    assert.ok(packageError.message.includes('thesis.cls'), 'Should include filename');
    assert.strictEqual(packageError.file, 'thesis.cls');
    assert.ok(packageError.suggestion, 'Should provide suggestion');

    assert.ok(result.missingInputs.includes('thesis.cls'), 'Should add to missingInputs');
    assert.strictEqual(result.success, false, 'Should mark as failed');
    assert.ok(result.errors > 0, 'Should count errors');
  });

  it('should parse undefined control sequence', () => {
    const result = parseLatexLog(UNDEFINED_CONTROL_LOG);

    const undefError = result.diagnostics.find(d => d.code === 'UNDEFINED_CONTROL');
    assert.ok(undefError, 'Should detect UNDEFINED_CONTROL error');
    assert.ok(undefError.message.includes('includegraphix'), 'Should extract command name');
    assert.ok(undefError.suggestion.includes('graphicx'), 'Should suggest graphicx package');
  });

  it('should parse missing graphic error', () => {
    const result = parseLatexLog(MISSING_GRAPHIC_LOG);

    const graphicError = result.diagnostics.find(d => d.code === 'MISSING_GRAPHIC');
    assert.ok(graphicError, 'Should detect MISSING_GRAPHIC error');
    assert.strictEqual(graphicError.file, 'figure1.pdf');
    assert.ok(result.missingInputs.includes('figure1.pdf'), 'Should add to missingInputs');
  });

  it('should detect rerun needed', () => {
    const result = parseLatexLog(RERUN_NEEDED_LOG);

    assert.strictEqual(result.rerunNeeded, true, 'Should detect rerun needed');

    const rerunInfo = result.diagnostics.find(d => d.code === 'RERUN_NEEDED');
    assert.ok(rerunInfo, 'Should have rerun diagnostic');
    assert.strictEqual(rerunInfo.severity, 'info');
  });

  it('should parse citation warning', () => {
    const result = parseLatexLog(CITATION_WARNING_LOG);

    const citationWarn = result.diagnostics.find(d => d.code === 'UNDEFINED_CITATION');
    assert.ok(citationWarn, 'Should detect citation warning');
    assert.strictEqual(citationWarn.severity, 'warning');
    assert.ok(citationWarn.message.includes('smith2020'), 'Should include citation key');
  });

  it('should parse overfull hbox warning', () => {
    const result = parseLatexLog(OVERFULL_HBOX_LOG);

    const hboxWarn = result.diagnostics.find(d => d.code === 'BADNESS_HBOX');
    assert.ok(hboxWarn, 'Should detect hbox warning');
    assert.strictEqual(hboxWarn.severity, 'warning');
    assert.strictEqual(hboxWarn.line, 89, 'Should extract line number');
  });

  it('should detect successful compilation', () => {
    const result = parseLatexLog(SUCCESS_LOG);

    assert.strictEqual(result.success, true, 'Should mark as successful');
    assert.strictEqual(result.errors, 0, 'Should have zero errors');
    assert.strictEqual(result.diagnostics.length, 0, 'Should have no diagnostics');
  });

  it('should handle multiple errors and warnings', () => {
    const result = parseLatexLog(MULTI_ERROR_LOG);

    assert.ok(result.errors >= 3, 'Should count multiple errors');
    assert.ok(result.warnings >= 1, 'Should count warnings');
    assert.ok(result.missingInputs.length >= 2, 'Should extract multiple missing files');
    assert.strictEqual(result.rerunNeeded, true, 'Should detect rerun needed');
    assert.strictEqual(result.success, false, 'Should mark as failed');

    // Check all expected codes present
    const codes = result.diagnostics.map(d => d.code);
    assert.ok(codes.includes('MISSING_PACKAGE'), 'Should have MISSING_PACKAGE');
    assert.ok(codes.includes('UNDEFINED_CONTROL'), 'Should have UNDEFINED_CONTROL');
    assert.ok(codes.includes('MISSING_GRAPHIC'), 'Should have MISSING_GRAPHIC');
    assert.ok(codes.includes('UNDEFINED_CITATION'), 'Should have UNDEFINED_CITATION');
    assert.ok(codes.includes('RERUN_NEEDED'), 'Should have RERUN_NEEDED');
  });

  it('should deduplicate identical diagnostics', () => {
    const duplicateLog = `
! LaTeX Error: File 'test.sty' not found.
! LaTeX Error: File 'test.sty' not found.
! LaTeX Error: File 'test.sty' not found.
    `;

    const result = parseLatexLog(duplicateLog);
    const packageErrors = result.diagnostics.filter(d => d.code === 'MISSING_PACKAGE');

    assert.strictEqual(packageErrors.length, 1, 'Should deduplicate identical errors');
    assert.strictEqual(result.missingInputs.length, 1, 'Should have single missing file');
  });

  it('should handle empty log', () => {
    const result = parseLatexLog('');

    assert.strictEqual(result.diagnostics.length, 0);
    assert.strictEqual(result.missingInputs.length, 0);
    assert.strictEqual(result.errors, 0);
    assert.strictEqual(result.warnings, 0);
    assert.strictEqual(result.success, false);
  });

  it('should handle null/undefined input', () => {
    const resultNull = parseLatexLog(null);
    const resultUndef = parseLatexLog(undefined);

    assert.strictEqual(resultNull.diagnostics.length, 0);
    assert.strictEqual(resultUndef.diagnostics.length, 0);
  });
});

// =============================================================================
// Schema Validation Tests
// =============================================================================

describe('Schema Validation', () => {
  it('should validate diagnostic schema', () => {
    const validDiag = {
      severity: 'error',
      code: 'TEST_CODE',
      message: 'Test message',
      file: 'test.tex',
      line: 42,
      suggestion: 'Fix this'
    };

    const result = DiagnosticSchema.parse(validDiag);
    assert.deepStrictEqual(result, validDiag);
  });

  it('should validate parse result schema', () => {
    const validResult = {
      diagnostics: [
        {
          severity: 'error',
          code: 'TEST',
          message: 'Test'
        }
      ],
      missingInputs: ['test.sty'],
      rerunNeeded: true,
      success: false,
      errors: 1,
      warnings: 0
    };

    const result = ParseResultSchema.parse(validResult);
    assert.deepStrictEqual(result, validResult);
  });

  it('should reject invalid severity', () => {
    const invalid = {
      severity: 'critical',
      code: 'TEST',
      message: 'Test'
    };

    assert.throws(
      () => DiagnosticSchema.parse(invalid),
      'Should reject invalid severity'
    );
  });

  it('should reject negative line numbers', () => {
    const invalid = {
      severity: 'error',
      code: 'TEST',
      message: 'Test',
      line: -5
    };

    assert.throws(
      () => DiagnosticSchema.parse(invalid),
      'Should reject negative line numbers'
    );
  });
});

// =============================================================================
// CLI Formatting Tests
// =============================================================================

describe('formatDiagnosticsForCLI', () => {
  it('should format diagnostics with colors', () => {
    const diagnostics = [
      {
        severity: 'error',
        code: 'TEST_ERROR',
        message: 'Test error message',
        file: 'test.tex',
        line: 42,
        suggestion: 'Fix this issue'
      },
      {
        severity: 'warning',
        code: 'TEST_WARNING',
        message: 'Test warning message'
      }
    ];

    const output = formatDiagnosticsForCLI(diagnostics, { colors: true });

    assert.ok(output.includes('Errors (1)'), 'Should show error count');
    assert.ok(output.includes('Warnings (1)'), 'Should show warning count');
    assert.ok(output.includes('Test error message'), 'Should include error message');
    assert.ok(output.includes('test.tex:42'), 'Should include location');
    assert.ok(output.includes('Fix this issue'), 'Should include suggestion');
    assert.ok(output.includes('\x1b['), 'Should include ANSI colors');
  });

  it('should format without colors', () => {
    const diagnostics = [
      {
        severity: 'error',
        code: 'TEST',
        message: 'Test message'
      }
    ];

    const output = formatDiagnosticsForCLI(diagnostics, { colors: false });

    assert.ok(!output.includes('\x1b['), 'Should not include ANSI colors');
    assert.ok(output.includes('Test message'), 'Should include message');
  });

  it('should handle empty diagnostics', () => {
    const output = formatDiagnosticsForCLI([]);

    assert.ok(output.includes('No diagnostics'), 'Should show no diagnostics message');
  });

  it('should include raw excerpts in verbose mode', () => {
    const diagnostics = [
      {
        severity: 'error',
        code: 'TEST',
        message: 'Test',
        raw: 'Raw log excerpt here'
      }
    ];

    const outputVerbose = formatDiagnosticsForCLI(diagnostics, { verbose: true });
    const outputNormal = formatDiagnosticsForCLI(diagnostics, { verbose: false });

    assert.ok(outputVerbose.includes('Raw:'), 'Verbose should include raw');
    assert.ok(!outputNormal.includes('Raw:'), 'Normal should not include raw');
  });
});

// =============================================================================
// Summary Tests
// =============================================================================

describe('createDiagnosticSummary', () => {
  it('should create summary for failed compilation', () => {
    const result = {
      diagnostics: [],
      missingInputs: ['test.sty', 'test.cls'],
      rerunNeeded: true,
      success: false,
      errors: 3,
      warnings: 5
    };

    const summary = createDiagnosticSummary(result);

    assert.ok(summary.includes('failed'), 'Should say failed');
    assert.ok(summary.includes('3 errors'), 'Should show error count');
    assert.ok(summary.includes('5 warnings'), 'Should show warning count');
    assert.ok(summary.includes('Missing 2 files'), 'Should show missing file count');
    assert.ok(summary.includes('Rerun needed'), 'Should mention rerun');
  });

  it('should create summary for successful compilation', () => {
    const result = {
      diagnostics: [],
      missingInputs: [],
      rerunNeeded: false,
      success: true,
      errors: 0,
      warnings: 0
    };

    const summary = createDiagnosticSummary(result);

    assert.ok(summary.includes('succeeded'), 'Should say succeeded');
  });
});
