/**
 * @fileoverview TeX WASM Engine Runtime
 *
 * Executes LaTeX compilation using loaded WASM engine.
 *
 * @module engine/run
 */

import { z } from 'zod';

// =============================================================================
// Type Definitions (JSDoc)
// =============================================================================

/**
 * @typedef {Object} CompileOptions
 * @property {Map<string, Uint8Array>} vfs - Virtual file system (path → content)
 * @property {string} entry - Entry point filename (e.g., 'main.tex')
 * @property {number} [passes=2] - Number of compilation passes
 * @property {string[]} [args=[]] - Additional LaTeX engine arguments
 * @property {boolean} [verbose=false] - Enable detailed logging
 */

/**
 * @typedef {Object} CompileResult
 * @property {number} code - Exit code (0 = success)
 * @property {string} log - Compilation log output
 * @property {Map<string, Uint8Array>} fsSnapshot - Post-compilation file system state
 * @property {Uint8Array} [pdf] - Generated PDF (if successful)
 * @property {string[]} [errors] - Parsed error messages
 */

// =============================================================================
// Validation Schemas
// =============================================================================

const CompileOptionsSchema = z.object({
  vfs: z.instanceof(Map),
  entry: z.string().min(1),
  passes: z.number().int().min(1).max(5).default(2),
  args: z.array(z.string()).default([]),
  verbose: z.boolean().default(false)
});

// =============================================================================
// Engine Execution (Stub)
// =============================================================================

/**
 * Run LaTeX compilation using WASM engine
 *
 * NOTE: This is a stub implementation defining the API contract.
 * Actual implementation requires a loaded engine instance.
 *
 * Execution flow:
 * 1. Populate engine VFS with input files
 * 2. Set entry point and working directory
 * 3. Execute compilation (multi-pass if needed)
 * 4. Capture logs and extract artifacts
 * 5. Return PDF + metadata
 *
 * @param {import('./load.mjs').EngineInstance} engine - Loaded engine instance
 * @param {CompileOptions} options - Compilation options
 * @returns {Promise<CompileResult>}
 *
 * @example
 * const encoder = new TextEncoder();
 * const vfs = new Map([
 *   ['main.tex', encoder.encode('\\documentclass{article}\\begin{document}Hello\\end{document}')]
 * ]);
 *
 * const result = await runEngine(engine, { vfs, entry: 'main.tex' });
 * if (result.code === 0) {
 *   console.log('PDF size:', result.pdf.length);
 * } else {
 *   console.error('Errors:', result.errors);
 * }
 */
export async function runEngine(engine, options) {
  // Validate options
  const validated = CompileOptionsSchema.parse(options);
  const { vfs, entry, passes, args, verbose } = validated;

  if (verbose) {
    console.log(`[TeX Engine] Compiling ${entry} (${passes} passes)`);
  }

  // ============= Implementation Required =============
  //
  // Full implementation would:
  //
  // 1. Populate VFS:
  //    for (const [path, content] of vfs) {
  //      await engine.writeFile(path, content);
  //    }
  //
  // 2. Set entry point:
  //    await engine.setMainFile(entry);
  //
  // 3. Run compilation:
  //    let log = '';
  //    for (let i = 0; i < passes; i++) {
  //      const result = await engine.compile({ dvi: false });
  //      log += result.log;
  //      if (result.status !== 0) break;
  //    }
  //
  // 4. Extract PDF:
  //    const pdfName = entry.replace(/\.tex$/, '.pdf');
  //    const pdf = await engine.readFile(pdfName);
  //
  // 5. Snapshot FS:
  //    const fsSnapshot = await engine.listFiles();
  //
  // ===================================================

  throw new Error(
    `Engine execution not implemented.\n\n` +
    `This function requires a working engine instance from loadEngine().\n` +
    `See: packages/kgc-cli/src/lib/latex/engine/load.mjs\n\n` +
    `Expected API:\n` +
    `  - engine.writeFile(path, content)\n` +
    `  - engine.setMainFile(entry)\n` +
    `  - engine.compile(options)\n` +
    `  - engine.readFile(path)\n`
  );
}

/**
 * Parse LaTeX log for errors and warnings
 * @param {string} log - LaTeX compilation log
 * @returns {{errors: string[], warnings: string[], missingInputs: string[]}}
 */
export function parseLog(log) {
  const errors = [];
  const warnings = [];
  const missingInputs = [];

  // Error patterns
  const errorPatterns = [
    /^! LaTeX Error: (.+)$/gm,
    /^! (.+)$/gm
  ];

  // Warning patterns
  const warningPatterns = [
    /^LaTeX Warning: (.+)$/gm,
    /^Package \w+ Warning: (.+)$/gm
  ];

  // Missing file patterns
  const missingPatterns = [
    /! LaTeX Error: File `([^']+)' not found/g,
    /! I can't find file `([^']+)'/g
  ];

  for (const pattern of errorPatterns) {
    let match;
    pattern.lastIndex = 0;
    while ((match = pattern.exec(log)) !== null) {
      errors.push(match[1].trim());
    }
  }

  for (const pattern of warningPatterns) {
    let match;
    pattern.lastIndex = 0;
    while ((match = pattern.exec(log)) !== null) {
      warnings.push(match[1].trim());
    }
  }

  for (const pattern of missingPatterns) {
    let match;
    pattern.lastIndex = 0;
    while ((match = pattern.exec(log)) !== null) {
      const file = match[1].trim();
      if (file && !missingInputs.includes(file)) {
        missingInputs.push(file);
      }
    }
  }

  return { errors, warnings, missingInputs };
}

/**
 * Create minimal test VFS
 * @param {string} [content] - Custom TeX content
 * @returns {Map<string, Uint8Array>}
 */
export function createTestVFS(content = null) {
  const encoder = new TextEncoder();

  const defaultContent = `\\documentclass{article}
\\begin{document}
Hello World from KGC CLI!
\\end{document}
`;

  return new Map([
    ['main.tex', encoder.encode(content || defaultContent)]
  ]);
}
