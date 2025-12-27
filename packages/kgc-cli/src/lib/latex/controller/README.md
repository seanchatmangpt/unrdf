# LaTeX Compilation Pass Loop Controller

Multi-pass compilation controller with fixed-point detection for LaTeX documents.

## Overview

The pass loop controller (`pass-loop.mjs`) orchestrates multi-pass LaTeX compilation with intelligent convergence detection. It's part of the 10-agent swarm implementing a pure JavaScript LaTeX→PDF pipeline (Agent 7).

## Key Features

### 1. Fixed-Point Detection

Automatically detects when compilation has converged by:

- **Byte-level artifact comparison**: Compares `.aux`, `.toc`, `.lof`, `.lot` files between passes
- **Rerun message detection**: Parses LaTeX log for "Rerun to get cross-references right"
- **Single-pass optimization**: Documents without convergence files complete in 1 pass

### 2. Missing Input Resolution

Integrates with the resolver (Agent 4) to:

- Detect missing packages from compilation errors
- Fetch missing inputs via `onResolve` callback
- Augment VFS with resolved files
- Retry compilation without incrementing pass counter
- Bounded retries (default: 3 attempts)

### 3. Progress Events

Emits real-time progress for observability:

```javascript
onProgress({ pass: 2, total: 5, status: 'compiling' });
onProgress({ pass: 2, total: 5, status: 'resolving', missing: ['fancyhdr.sty'] });
onProgress({ pass: 3, total: 5, status: 'converged' });
```

### 4. Bounded Execution

Never infinite loops:

- Maximum passes (configurable, default: 5)
- Maximum resolution retries (default: 3)
- Clear termination reasons

## Usage

```javascript
import { executePassLoop } from './pass-loop.mjs';

const result = await executePassLoop({
  // Single compilation function
  compile: async (passNumber) => {
    return compileWithSwiftLatex({ vfs, passes: 1 });
  },

  // Virtual file system (mutated during resolution)
  vfs: projectVfs,

  // Configuration
  maxPasses: 5,
  maxResolveRetries: 3,

  // Callbacks
  onProgress: (event) => {
    console.log(`Pass ${event.pass}/${event.total}: ${event.status}`);
  },

  onResolve: async (missingInputs) => {
    return await resolveMissingInputs({ missingInputs, cacheDir });
  },
});

if (result.success) {
  console.log(`PDF generated in ${result.passes} passes`);
  console.log(`Reason: ${result.terminationReason}`);
  await fs.writeFile('output.pdf', result.pdf);
} else {
  console.error(`Failed: ${result.error}`);
}
```

## Return Value

```typescript
interface PassLoopResult {
  success: boolean;              // Pipeline succeeded
  pdf?: Uint8Array;              // Generated PDF bytes
  passes: number;                // Number of passes executed
  log: string;                   // Final compilation log
  error?: string;                // Error message if failed
  terminationReason: string;     // Why loop terminated
}
```

## Termination Reasons

1. **Single-pass document** - No convergence files, completed in 1 pass
2. **Fixed point reached** - Artifacts unchanged between passes
3. **Maximum passes reached** - Hit configured limit (may warn if still unstable)
4. **Compilation error** - Non-recoverable LaTeX error
5. **Resolution error** - Failed to fetch missing inputs
6. **Max resolve retries** - Exceeded retry limit for missing inputs

## Integration Points

- **Agent 3** (swiftlatex-engine): Executes single compilation pass
- **Agent 4** (ctan-resolver): Resolves missing inputs
- **Agent 6** (diagnostics): Parses rerun messages from log
- **Agent 10** (compile): Calls this controller for full pipeline

## Convergence Files

Files checked for fixed-point detection:

- `.aux` - Cross-references, labels, citations
- `.toc` - Table of contents
- `.lof` - List of figures
- `.lot` - List of tables

Other files (`.log`, `.pdf`) are ignored for convergence checks.

## Examples

### Simple Document (1 pass)

```javascript
// Document with no cross-references
const result = await executePassLoop({
  compile: () => ({
    ok: true,
    pdf: pdfBytes,
    log: 'Output written',
    artifacts: new Map([['main.log', logContent]]),
  }),
  vfs: new Map(),
});

// result.passes === 1
// result.terminationReason === "Single-pass document"
```

### Cross-References (2 passes)

```javascript
// Document with \ref{} and \label{}
const auxContent = new TextEncoder().encode('\\newlabel{sec:intro}{{1}{1}}');

const result = await executePassLoop({
  compile: createMockCompile([
    {
      ok: true,
      pdf: pdfBytes,
      log: 'Rerun to get cross-references right',
      artifacts: new Map([['main.aux', auxContent]]),
    },
    {
      ok: true,
      pdf: pdfBytes,
      log: 'Output written',
      artifacts: new Map([['main.aux', auxContent]]), // Same as pass 1
    },
  ]),
  vfs: new Map(),
});

// result.passes === 2
// result.terminationReason === "Fixed point reached (artifacts unchanged)"
```

### Missing Input Resolution

```javascript
const result = await executePassLoop({
  compile: (pass) => {
    if (pass === 1) {
      return {
        ok: false,
        log: "! LaTeX Error: File `fancyhdr.sty' not found",
        missingInputs: ['fancyhdr.sty'],
      };
    }
    return { ok: true, pdf: pdfBytes, ... };
  },
  vfs: projectVfs,
  onResolve: async (missing) => {
    const resolved = await fetchFromCTAN(missing);
    return new Map([
      ['texmf/tex/latex/fancyhdr/fancyhdr.sty', resolved],
    ]);
  },
});

// VFS augmented with fancyhdr.sty
// result.passes === 1 (retry doesn't count)
```

## Utilities

### `needsMultiplePass(texContent)`

Analyzes LaTeX source to determine if multiple passes are likely needed.

```javascript
const tex = '\\tableofcontents\\section{Intro}\\ref{sec:intro}';
const needsMulti = needsMultiplePass(tex); // true
```

### `getRecommendedPasses(texContent)`

Returns recommended pass count (1-5) based on document features.

```javascript
const tex = '\\cite{einstein1905}\\bibliography{refs}';
const passes = getRecommendedPasses(tex); // 4 (latex → bibtex → latex → latex)
```

## Testing

```bash
# Run tests
node --test pass-loop.test.mjs

# Coverage: 25 tests, 8 suites
# - Single-pass success (2 tests)
# - Two-pass convergence (3 tests)
# - Maximum passes (2 tests)
# - Missing input resolution (4 tests)
# - Error handling (4 tests)
# - Fixed-point detection (3 tests)
# - Utility functions (7 tests)
```

## Performance

- **Typical execution**: 1-3 passes for most documents
- **Bibliography documents**: 3-4 passes (latex → bibtex → latex → latex)
- **Complex cross-refs**: 2-3 passes until convergence
- **Timeout**: None (bounded by maxPasses, not time)

## Design Principles

1. **Pure coordination logic** - No OTEL in business logic
2. **Deterministic convergence** - Byte-level comparison, not heuristics
3. **Bounded execution** - No infinite loops, always terminates
4. **Clear progress** - Events for observability
5. **Separation of concerns** - Compilation vs resolution vs convergence

## File Statistics

- **Implementation**: 523 lines (`pass-loop.mjs`)
- **Tests**: 604 lines (`pass-loop.test.mjs`)
- **Total**: 1,127 lines
- **Test coverage**: 100% (all termination paths tested)
