# LaTeX Diagnostics Parser - Usage Examples

## Overview

The diagnostics parser extracts structured error information from LaTeX compilation logs. It provides:

- **Error detection**: Missing files, undefined commands, package errors
- **Warning detection**: Citations, references, formatting issues
- **Info detection**: Rerun hints, auxiliary file creation
- **Structured output**: Zod-validated JSON with file/line context
- **CLI formatting**: Human-readable terminal output

## Basic Usage

```javascript
import { parseLatexLog, formatDiagnosticsForCLI } from './diagnostics/index.mjs';

// Parse log
const result = parseLatexLog(logText);

// Display in terminal
console.log(formatDiagnosticsForCLI(result.diagnostics));

// Access structured data
console.log(`Errors: ${result.errors}`);
console.log(`Warnings: ${result.warnings}`);
console.log(`Missing files:`, result.missingInputs);
console.log(`Rerun needed: ${result.rerunNeeded}`);
```

## Example Outputs

### Example 1: Missing Package Error

**Input Log:**
```
! LaTeX Error: File 'thesis.cls' not found.

Type X to quit or <RETURN> to proceed,
or enter new name. (Default extension: cls)

Enter file name:
! Emergency stop.
```

**Parsed Output:**
```json
{
  "diagnostics": [
    {
      "severity": "error",
      "code": "MISSING_PACKAGE",
      "message": "File 'thesis.cls' not found",
      "file": "thesis.cls",
      "suggestion": "Install package providing thesis.cls or add to project"
    },
    {
      "severity": "error",
      "code": "EMERGENCY_STOP",
      "message": "Emergency stop - critical error encountered",
      "suggestion": "Review preceding errors in log for root cause"
    }
  ],
  "missingInputs": ["thesis.cls"],
  "rerunNeeded": false,
  "success": false,
  "errors": 2,
  "warnings": 0
}
```

**CLI Output:**
```
Errors (2):
  ✗ File 'thesis.cls' not found
    at thesis.cls
    → Install package providing thesis.cls or add to project

  ✗ Emergency stop - critical error encountered
    → Review preceding errors in log for root cause

Summary: 2 errors, 0 warnings, 0 info
```

---

### Example 2: Undefined Control Sequence

**Input Log:**
```
! Undefined control sequence.
l.42 \\includegraphix
                    {logo.pdf}
?
```

**Parsed Output:**
```json
{
  "diagnostics": [
    {
      "severity": "error",
      "code": "UNDEFINED_CONTROL",
      "message": "Undefined command: \\includegraphix",
      "suggestion": "Load package: \\usepackage{graphicx}"
    }
  ],
  "missingInputs": [],
  "rerunNeeded": false,
  "success": false,
  "errors": 1,
  "warnings": 0
}
```

**CLI Output:**
```
Errors (1):
  ✗ Undefined command: \includegraphix
    → Load package: \usepackage{graphicx}

Summary: 1 errors, 0 warnings, 0 info
```

---

### Example 3: Missing Graphic File

**Input Log:**
```
! Package pdftex.def Error: File 'figure1.pdf' not found.

See the pdftex.def package documentation for explanation.
```

**Parsed Output:**
```json
{
  "diagnostics": [
    {
      "severity": "error",
      "code": "MISSING_GRAPHIC",
      "message": "Graphic file 'figure1.pdf' not found",
      "file": "figure1.pdf",
      "suggestion": "Add figure1.pdf to project or fix file path"
    }
  ],
  "missingInputs": ["figure1.pdf"],
  "rerunNeeded": false,
  "success": false,
  "errors": 1,
  "warnings": 0
}
```

---

### Example 4: Citation and Reference Warnings

**Input Log:**
```
LaTeX Warning: Citation 'smith2020' on page 3 undefined on input line 142.

LaTeX Warning: Reference 'fig:architecture' on page 5 undefined.

LaTeX Warning: Label(s) may have changed. Rerun to get cross-references right.
```

**Parsed Output:**
```json
{
  "diagnostics": [
    {
      "severity": "warning",
      "code": "UNDEFINED_CITATION",
      "message": "Citation 'smith2020' undefined",
      "line": 142,
      "suggestion": "Add entry for 'smith2020' in bibliography or run BibTeX"
    },
    {
      "severity": "warning",
      "code": "UNDEFINED_REFERENCE",
      "message": "Reference 'fig:architecture' undefined",
      "suggestion": "Check \\label{} exists and rerun compilation"
    },
    {
      "severity": "info",
      "code": "RERUN_NEEDED",
      "message": "Labels changed - rerun needed for cross-references",
      "suggestion": "Run compilation again to resolve references"
    }
  ],
  "missingInputs": [],
  "rerunNeeded": true,
  "success": true,
  "errors": 0,
  "warnings": 2
}
```

**CLI Output:**
```
Warnings (2):
  ⚠ Citation 'smith2020' undefined
    → Add entry for 'smith2020' in bibliography or run BibTeX

  ⚠ Reference 'fig:architecture' undefined
    → Check \label{} exists and rerun compilation

Info (1):
  ℹ Labels changed - rerun needed for cross-references
    → Run compilation again to resolve references

Summary: 0 errors, 2 warnings, 1 info
```

---

### Example 5: Overfull Hbox Warning

**Input Log:**
```
Overfull \\hbox (12.34567pt too wide) in paragraph at lines 89--91
 []\\OT1/cmr/m/n/10 This is a very long line that does not fit within the mar-
```

**Parsed Output:**
```json
{
  "diagnostics": [
    {
      "severity": "warning",
      "code": "BADNESS_HBOX",
      "message": "Overfull hbox (12.34567pt too wide) at lines 89-91",
      "line": 89,
      "suggestion": "Adjust text formatting or allow line breaking"
    }
  ],
  "missingInputs": [],
  "rerunNeeded": false,
  "success": true,
  "errors": 0,
  "warnings": 1
}
```

---

### Example 6: Successful Compilation

**Input Log:**
```
Output written on main.pdf (10 pages, 234567 bytes).
PDF statistics:
 123 PDF objects out of 1000 (max. 8388607)
```

**Parsed Output:**
```json
{
  "diagnostics": [],
  "missingInputs": [],
  "rerunNeeded": false,
  "success": true,
  "errors": 0,
  "warnings": 0
}
```

**CLI Output:**
```
No diagnostics found
```

---

### Example 7: Complex Multi-Error Scenario

**Input Log:**
```
! LaTeX Error: File 'custom.sty' not found.

Type X to quit or <RETURN> to proceed,

! Undefined control sequence.
l.15 \\customcommand
                    {test}

! Package graphicx Error: File 'plot.pdf' not found.

LaTeX Warning: Citation 'doe2019' undefined.

Overfull \\hbox (5.0pt too wide) in paragraph at lines 50--52

LaTeX Warning: Label(s) may have changed. Rerun to get cross-references right.
```

**Parsed Output:**
```json
{
  "diagnostics": [
    {
      "severity": "error",
      "code": "MISSING_PACKAGE",
      "message": "File 'custom.sty' not found",
      "file": "custom.sty",
      "suggestion": "Install package providing custom.sty or add to project"
    },
    {
      "severity": "error",
      "code": "UNDEFINED_CONTROL",
      "message": "Undefined command: \\customcommand",
      "suggestion": "Check command spelling or load required package for \\customcommand"
    },
    {
      "severity": "error",
      "code": "MISSING_GRAPHIC",
      "message": "Graphic file 'plot.pdf' not found",
      "file": "plot.pdf",
      "suggestion": "Add plot.pdf to project or fix file path"
    },
    {
      "severity": "warning",
      "code": "UNDEFINED_CITATION",
      "message": "Citation 'doe2019' undefined",
      "suggestion": "Add entry for 'doe2019' in bibliography or run BibTeX"
    },
    {
      "severity": "warning",
      "code": "BADNESS_HBOX",
      "message": "Overfull hbox (5.0pt too wide) at lines 50-52",
      "line": 50,
      "suggestion": "Adjust text formatting or allow line breaking"
    },
    {
      "severity": "info",
      "code": "RERUN_NEEDED",
      "message": "Labels changed - rerun needed for cross-references",
      "suggestion": "Run compilation again to resolve references"
    }
  ],
  "missingInputs": ["custom.sty", "plot.pdf"],
  "rerunNeeded": true,
  "success": false,
  "errors": 3,
  "warnings": 2
}
```

**CLI Output:**
```
Errors (3):
  ✗ File 'custom.sty' not found
    at custom.sty
    → Install package providing custom.sty or add to project

  ✗ Undefined command: \customcommand
    → Check command spelling or load required package for \customcommand

  ✗ Graphic file 'plot.pdf' not found
    at plot.pdf
    → Add plot.pdf to project or fix file path

Warnings (2):
  ⚠ Citation 'doe2019' undefined
    → Add entry for 'doe2019' in bibliography or run BibTeX

  ⚠ Overfull hbox (5.0pt too wide) at lines 50-52
    → Adjust text formatting or allow line breaking

Info (1):
  ℹ Labels changed - rerun needed for cross-references
    → Run compilation again to resolve references

Summary: 3 errors, 2 warnings, 1 info
```

---

## Integration with Agent 4 (Resolver)

The parser extracts missing files for Agent 4's resolver to fetch:

```javascript
import { parseLatexLog } from './diagnostics/index.mjs';
import { resolveFiles } from './resolver.mjs'; // Agent 4

const result = parseLatexLog(logText);

if (result.missingInputs.length > 0) {
  console.log(`Resolving ${result.missingInputs.length} missing files...`);

  // Feed to Agent 4's resolver
  const resolved = await resolveFiles(result.missingInputs);

  // Retry compilation with resolved files
  // ...
}
```

## Error Code Reference

| Code | Severity | Description | Typical Suggestion |
|------|----------|-------------|-------------------|
| `MISSING_PACKAGE` | error | `.sty`, `.cls`, `.def` file not found | Install package or add to project |
| `MISSING_INPUT` | error | User `.tex` file not found | Create file or fix `\input` path |
| `MISSING_GRAPHIC` | error | Image file not found | Add file or fix path |
| `FILE_NOT_FOUND` | error | Generic file not found | Check file path and name |
| `UNDEFINED_CONTROL` | error | Unknown command used | Load required package |
| `MISSING_FONT` | error | Font file not found | Install font package |
| `PACKAGE_ERROR` | error | Package-specific error | Check package docs |
| `EMERGENCY_STOP` | error | Critical compilation failure | Review preceding errors |
| `FILE_LINE_ERROR` | error | Modern error format with file:line | Check syntax at line |
| `BADNESS_HBOX` | warning | Text overfull/underfull | Adjust formatting |
| `UNDEFINED_CITATION` | warning | BibTeX citation not found | Add to bibliography |
| `UNDEFINED_REFERENCE` | warning | `\ref{}` target missing | Check `\label{}` exists |
| `PACKAGE_WARNING` | warning | Package-specific warning | Review package options |
| `FONT_WARNING` | warning | Font substitution | Output may differ |
| `RERUN_NEEDED` | info | Cross-references need update | Rerun compilation |
| `TOC_RERUN` | info | Table of contents needs update | Rerun compilation |
| `MISSING_AUX` | info | Auxiliary file not yet created | Normal on first run |

## Advanced Usage

### Custom Formatting

```javascript
import { parseLatexLog, formatDiagnosticsForCLI } from './diagnostics/index.mjs';

const result = parseLatexLog(logText);

// No colors
const plain = formatDiagnosticsForCLI(result.diagnostics, { colors: false });

// Verbose mode (includes raw log excerpts)
const verbose = formatDiagnosticsForCLI(result.diagnostics, { verbose: true });

// Custom filtering
const errorsOnly = result.diagnostics.filter(d => d.severity === 'error');
const formatted = formatDiagnosticsForCLI(errorsOnly);
```

### Summary Generation

```javascript
import { parseLatexLog, createDiagnosticSummary } from './diagnostics/index.mjs';

const result = parseLatexLog(logText);
const summary = createDiagnosticSummary(result);

console.log(summary);
// => "Compilation failed. 3 errors, 5 warnings. Missing 2 files. Rerun needed."
```

### Schema Validation

```javascript
import { DiagnosticSchema, ParseResultSchema } from './diagnostics/index.mjs';

// Validate individual diagnostic
try {
  const validated = DiagnosticSchema.parse({
    severity: 'error',
    code: 'CUSTOM_ERROR',
    message: 'Custom error message',
    file: 'test.tex',
    line: 42,
    suggestion: 'Fix this'
  });
  console.log('Valid diagnostic:', validated);
} catch (err) {
  console.error('Invalid diagnostic:', err.errors);
}

// Validate full result
try {
  const validated = ParseResultSchema.parse(result);
  console.log('Valid result');
} catch (err) {
  console.error('Invalid result:', err.errors);
}
```

## Performance Notes

- **Pattern matching**: Uses compiled regexes for efficiency
- **Deduplication**: Prevents duplicate diagnostics from multi-pass compilation
- **Deterministic output**: Sorted arrays for consistent results
- **Large logs**: Handles logs up to several MB efficiently
- **Memory**: Low memory footprint - processes line by line

## Testing

Run comprehensive tests:

```bash
node --test packages/kgc-cli/src/lib/latex/diagnostics/__tests__/parse-log.test.mjs
```

Test coverage:
- ✅ All error pattern detection
- ✅ All warning pattern detection
- ✅ All info pattern detection
- ✅ Missing input extraction
- ✅ Rerun detection
- ✅ Deduplication logic
- ✅ Schema validation
- ✅ CLI formatting
- ✅ Edge cases (empty logs, malformed input)
