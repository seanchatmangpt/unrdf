/**
 * @fileoverview TeX WASM Engine Loader
 *
 * Loads and initializes SwiftLaTeX WASM engine for Node.js.
 *
 * STATUS: ⚠️  PARTIAL IMPLEMENTATION
 * - WASM binaries can be downloaded via scripts/vendor-tex-engine.mjs
 * - Node.js adaptation requires XMLHttpRequest polyfill or fetch API
 * - Web Worker API must be replaced with direct Module interaction
 *
 * @module engine/load
 */

import { readFile, access } from 'node:fs/promises';
import { constants } from 'node:fs';
import { fileURLToPath } from 'node:url';
import { dirname, join } from 'node:path';
import { z } from 'zod';

// =============================================================================
// Type Definitions (JSDoc)
// =============================================================================

/**
 * @typedef {Object} EngineInstance
 * @property {Function} writeFile - Write file to virtual FS
 * @property {Function} readFile - Read file from virtual FS
 * @property {Function} compile - Run LaTeX compilation
 * @property {Function} cleanup - Clean up resources
 */

/**
 * @typedef {Object} EngineOptions
 * @property {'pdftex'|'xetex'} [engine='pdftex'] - TeX engine to use
 * @property {string} [vendorDir] - Custom vendor directory path
 * @property {boolean} [verbose=false] - Enable detailed logging
 */

// =============================================================================
// Validation Schemas
// =============================================================================

const EngineOptionsSchema = z.object({
  engine: z.enum(['pdftex', 'xetex']).default('pdftex'),
  vendorDir: z.string().optional(),
  verbose: z.boolean().default(false)
});

// =============================================================================
// Path Resolution
// =============================================================================

/**
 * Resolve vendor directory path
 * @returns {string} Absolute path to vendor/swiftlatex directory
 */
function getVendorPath() {
  const currentFile = fileURLToPath(import.meta.url);
  const currentDir = dirname(currentFile);
  // From: packages/kgc-cli/src/lib/latex/engine/load.mjs
  // To:   packages/kgc-cli/vendor/swiftlatex/
  return join(currentDir, '../../../../vendor/swiftlatex');
}

/**
 * Check if WASM binaries are available
 * @param {string} vendorDir - Vendor directory path
 * @returns {Promise<{available: boolean, wasmPath: string, jsPath: string, error?: string}>}
 */
async function checkWasmAvailability(vendorDir) {
  const wasmPath = join(vendorDir, 'swiftlatex.wasm');
  const jsPath = join(vendorDir, 'swiftlatex.js');

  try {
    await access(wasmPath, constants.R_OK);
    await access(jsPath, constants.R_OK);

    // Check if files are placeholders (< 10KB)
    const wasmStats = await import('node:fs/promises').then(fs => fs.stat(wasmPath));

    if (wasmStats.size < 10000) {
      return {
        available: false,
        wasmPath,
        jsPath,
        error: 'WASM binaries are placeholders. Run: node scripts/vendor-tex-engine.mjs'
      };
    }

    return { available: true, wasmPath, jsPath };
  } catch {
    return {
      available: false,
      wasmPath,
      jsPath,
      error: 'WASM binaries not found. Run: node scripts/vendor-tex-engine.mjs'
    };
  }
}

// =============================================================================
// Engine Loading (Node.js Adaptation Required)
// =============================================================================

/**
 * Load SwiftLaTeX WASM engine in Node.js
 *
 * NOTE: This is a stub implementation showing the required API.
 * Full implementation requires:
 * 1. Adapting Web Worker message-passing to direct Module calls
 * 2. Polyfilling XMLHttpRequest or rewriting package download logic
 * 3. Setting up Emscripten FS (File System) API
 *
 * @param {EngineOptions} options - Engine options
 * @returns {Promise<EngineInstance>}
 * @throws {Error} If WASM binaries unavailable or loading fails
 *
 * @example
 * const engine = await loadEngine({ engine: 'pdftex', verbose: true });
 * await engine.writeFile('main.tex', texContent);
 * const result = await engine.compile('main.tex');
 * console.log('PDF size:', result.pdf.length);
 */
export async function loadEngine(options = {}) {
  // Validate options
  const validated = EngineOptionsSchema.parse(options);
  const { engine, vendorDir = getVendorPath(), verbose } = validated;

  if (verbose) {
    console.log(`[TeX Engine] Loading ${engine} from ${vendorDir}`);
  }

  // Check WASM availability
  const { available, wasmPath, jsPath, error } = await checkWasmAvailability(vendorDir);

  if (!available) {
    throw new Error(
      `SwiftLaTeX WASM engine not available.\n\n` +
      `Error: ${error}\n\n` +
      `Expected files:\n` +
      `  - ${wasmPath}\n` +
      `  - ${jsPath}\n\n` +
      `To install:\n` +
      `  node packages/kgc-cli/scripts/vendor-tex-engine.mjs\n`
    );
  }

  // ============= CRITICAL: Node.js Adaptation Needed =============
  //
  // SwiftLaTeX's swiftlatex.js is designed for Web Workers with:
  // - self.onmessage / self.postMessage (browser-only)
  // - XMLHttpRequest for package downloads (browser-only)
  //
  // To make this work in Node.js, we need to:
  //
  // 1. Load swiftlatex.js as a module (not a worker)
  // 2. Polyfill XMLHttpRequest or replace with node:https
  // 3. Call Module functions directly instead of postMessage
  // 4. Set up Emscripten FS with our VFS
  //
  // Example adaptation (requires implementation):
  //
  //   import { readFileSync } from 'node:fs';
  //   import { createRequire } from 'node:module';
  //
  //   // Polyfill XMLHttpRequest
  //   global.XMLHttpRequest = XHRPolyfill;
  //
  //   // Load Emscripten module
  //   const moduleFactory = await import(jsPath);
  //   const Module = await moduleFactory.default({
  //     wasmBinary: readFileSync(wasmPath),
  //     locateFile: (path) => join(vendorDir, path)
  //   });
  //
  //   return createEngineInstance(Module);
  //
  // ================================================================

  throw new Error(
    `SwiftLaTeX WASM engine loading not yet implemented for Node.js.\n\n` +
    `The WASM binaries are available at: ${vendorDir}\n` +
    `But Node.js adaptation requires:\n` +
    `  1. XMLHttpRequest polyfill or fetch replacement\n` +
    `  2. Web Worker → direct Module call adaptation\n` +
    `  3. Emscripten FS integration with VFS\n\n` +
    `Alternative approaches:\n` +
    `  A. Use latexjs (npm package, Emscripten-based)\n` +
    `  B. Shell out to system pdflatex (requires TeX install)\n` +
    `  C. Complete SwiftLaTeX Node.js adapter (significant effort)\n\n` +
    `See: packages/kgc-cli/vendor/swiftlatex/README.md for details`
  );
}

/**
 * Check if TeX WASM engine is available
 * @returns {Promise<{available: boolean, engine: string, error?: string}>}
 */
export async function checkEngineAvailability() {
  const vendorDir = getVendorPath();
  const { available, error } = await checkWasmAvailability(vendorDir);

  return {
    available,
    engine: 'swiftlatex',
    ...(error && { error })
  };
}

/**
 * Get engine information and status
 * @returns {Promise<Object>} Engine metadata
 */
export async function getEngineInfo() {
  const vendorDir = getVendorPath();
  const { available, wasmPath, jsPath, error } = await checkWasmAvailability(vendorDir);

  const info = {
    engine: 'swiftlatex',
    available,
    vendorDir,
    files: { wasm: wasmPath, js: jsPath }
  };

  if (error) {
    info.error = error;
  }

  if (available) {
    try {
      const manifestPath = join(vendorDir, 'MANIFEST.json');
      const manifest = JSON.parse(await readFile(manifestPath, 'utf-8'));
      info.version = manifest.version;
      info.installedAt = manifest.installedAt;
    } catch {
      // Manifest optional
    }
  }

  return info;
}
