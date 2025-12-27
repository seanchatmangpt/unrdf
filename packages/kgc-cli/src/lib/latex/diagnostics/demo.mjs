#!/usr/bin/env node
/**
 * @fileoverview LaTeX Diagnostics Parser Demo
 *
 * Demonstrates diagnostic parsing capabilities with real-world LaTeX log examples.
 * Run: node demo.mjs
 */

import { parseLatexLog, formatDiagnosticsForCLI, createDiagnosticSummary } from './parse-log.mjs';

// =============================================================================
// Example LaTeX Logs
// =============================================================================

const EXAMPLES = [
  {
    name: 'Missing Package',
    log: `
! LaTeX Error: File 'thesis.cls' not found.

Type X to quit or <RETURN> to proceed,
or enter new name. (Default extension: cls)

Enter file name:
! Emergency stop.
`
  },
  {
    name: 'Undefined Control Sequence',
    log: `
! Undefined control sequence.
l.42 \\includegraphix
                    {logo.pdf}
?
`
  },
  {
    name: 'Missing Multiple Files + Warnings',
    log: `
! LaTeX Error: File 'custom.sty' not found.

Type X to quit or <RETURN> to proceed,

! Package pdftex.def Error: File 'plot.pdf' not found.

LaTeX Warning: Citation 'doe2019' undefined on input line 89.

Overfull \\hbox (5.0pt too wide) in paragraph at lines 50--52
 []\\OT1/cmr/m/n/10 This is a very long line of text

LaTeX Warning: Label(s) may have changed. Rerun to get cross-references right.
`
  },
  {
    name: 'Successful Compilation',
    log: `
LaTeX2e <2023-11-01> patch level 1
L3 programming layer <2024-01-22>

(/usr/share/texlive/texmf-dist/tex/latex/base/article.cls
Document Class: article 2023/05/17 v1.4n Standard LaTeX document class
(/usr/share/texlive/texmf-dist/tex/latex/base/size10.clo
File: size10.clo 2023/05/17 v1.4n Standard LaTeX file (size option)
))
No file main.aux.
[1] (./main.aux)
Output written on main.pdf (1 page, 12345 bytes).
PDF statistics:
 18 PDF objects out of 1000 (max. 8388607)
 12 compressed objects within 1 object stream
 0 named destinations out of 1000 (max. 500000)
 1 words of extra memory for PDF output out of 10000 (max. 10000000)
`
  }
];

// =============================================================================
// Demo Functions
// =============================================================================

/**
 * Run all examples and display results.
 */
function runDemo() {
  console.log('='.repeat(80));
  console.log('LaTeX Diagnostics Parser - Demonstration');
  console.log('='.repeat(80));
  console.log();

  for (const example of EXAMPLES) {
    console.log('─'.repeat(80));
    console.log(`Example: ${example.name}`);
    console.log('─'.repeat(80));
    console.log();

    // Parse log
    const result = parseLatexLog(example.log);

    // Display summary
    console.log('\x1b[1mSummary:\x1b[0m');
    console.log(createDiagnosticSummary(result));
    console.log();

    // Display structured data
    console.log('\x1b[1mStructured Data:\x1b[0m');
    console.log(`  Success: ${result.success}`);
    console.log(`  Errors: ${result.errors}`);
    console.log(`  Warnings: ${result.warnings}`);
    console.log(`  Missing inputs: ${result.missingInputs.length > 0 ? result.missingInputs.join(', ') : 'none'}`);
    console.log(`  Rerun needed: ${result.rerunNeeded}`);
    console.log();

    // Display formatted diagnostics
    if (result.diagnostics.length > 0) {
      console.log('\x1b[1mFormatted Diagnostics:\x1b[0m');
      console.log(formatDiagnosticsForCLI(result.diagnostics, { colors: true }));
    } else {
      console.log('\x1b[34mNo diagnostics found - compilation clean!\x1b[0m');
    }

    console.log();
  }

  console.log('='.repeat(80));
  console.log('Demo Complete');
  console.log('='.repeat(80));
}

/**
 * Display JSON output for programmatic use.
 */
function runJsonDemo() {
  console.log('JSON Output Mode:');
  console.log();

  for (const example of EXAMPLES) {
    const result = parseLatexLog(example.log);

    console.log(`// ${example.name}`);
    console.log(JSON.stringify({
      name: example.name,
      result: {
        success: result.success,
        errors: result.errors,
        warnings: result.warnings,
        missingInputs: result.missingInputs,
        rerunNeeded: result.rerunNeeded,
        diagnostics: result.diagnostics.map(d => ({
          severity: d.severity,
          code: d.code,
          message: d.message,
          file: d.file,
          line: d.line,
          suggestion: d.suggestion
        }))
      }
    }, null, 2));
    console.log();
  }
}

// =============================================================================
// Main
// =============================================================================

const args = process.argv.slice(2);

if (args.includes('--json')) {
  runJsonDemo();
} else {
  runDemo();
}
