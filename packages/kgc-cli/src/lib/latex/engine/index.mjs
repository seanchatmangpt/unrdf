/**
 * @fileoverview TeX WASM Engine - Public API
 *
 * Entry point for LaTeX→PDF compilation using SwiftLaTeX WASM engine.
 *
 * STATUS: ⚠️  Partial Implementation
 * - API defined and documented
 * - WASM binaries can be downloaded
 * - Node.js adapter requires completion
 *
 * @module engine
 *
 * @example
 * import { loadEngine, runEngine, checkEngineAvailability } from './engine/index.mjs';
 *
 * // Check availability first
 * const { available, error } = await checkEngineAvailability();
 * if (!available) {
 *   console.error('Engine not available:', error);
 *   process.exit(1);
 * }
 *
 * // Load engine
 * const engine = await loadEngine({ engine: 'pdftex', verbose: true });
 *
 * // Prepare VFS
 * const encoder = new TextEncoder();
 * const vfs = new Map([
 *   ['main.tex', encoder.encode('\\documentclass{article}...')]
 * ]);
 *
 * // Compile
 * const result = await runEngine(engine, { vfs, entry: 'main.tex' });
 * if (result.code === 0) {
 *   // Success - result.pdf contains PDF bytes
 * }
 */

// Import for local use
import { loadEngine, checkEngineAvailability, getEngineInfo } from './load.mjs';
import { runEngine, parseLog, createTestVFS } from './run.mjs';

// Re-export all public APIs
export {
  loadEngine,
  checkEngineAvailability,
  getEngineInfo,
  runEngine,
  parseLog,
  createTestVFS
};

/**
 * Convenience function: Load engine and compile in one call
 *
 * @param {Object} options - Combined options
 * @param {Map<string, Uint8Array>} options.vfs - Virtual file system
 * @param {string} options.entry - Entry point filename
 * @param {'pdftex'|'xetex'} [options.engine='pdftex'] - TeX engine
 * @param {number} [options.passes=2] - Compilation passes
 * @param {boolean} [options.verbose=false] - Verbose logging
 * @returns {Promise<import('./run.mjs').CompileResult>}
 */
export async function compileLatex(options) {
  const { vfs, entry, engine = 'pdftex', passes = 2, verbose = false } = options;

  // Load engine
  const engineInstance = await loadEngine({ engine, verbose });

  // Run compilation
  return runEngine(engineInstance, { vfs, entry, passes, verbose });
}
