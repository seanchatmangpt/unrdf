/**
 * @fileoverview LaTeX WASM Availability Checker
 *
 * Checks if SwiftLaTeX WASM binaries are installed and functional.
 * Used by tests to skip LaTeX tests when WASM is not available.
 *
 * @module test/fixtures/check-latex-available
 */

import { access, stat } from 'node:fs/promises';
import { constants } from 'node:fs';
import { join, dirname } from 'node:path';
import { fileURLToPath } from 'node:url';

const __filename = fileURLToPath(import.meta.url);
const __dirname = dirname(__filename);

/**
 * Get path to SwiftLaTeX vendor directory
 * @returns {string} Path to vendor/swiftlatex
 */
function getVendorPath() {
  // From: packages/kgc-cli/test/fixtures/
  // To:   packages/kgc-cli/vendor/swiftlatex/
  return join(__dirname, '..', '..', 'vendor', 'swiftlatex');
}

/**
 * Check if a file exists and is larger than placeholder size (10KB)
 * @param {string} path - File path to check
 * @returns {Promise<boolean>}
 */
async function isRealWasmFile(path) {
  try {
    await access(path, constants.R_OK);
    const stats = await stat(path);
    // Placeholder files are < 10KB, real WASM files are > 1MB
    return stats.size > 10240; // 10KB threshold
  } catch {
    return false;
  }
}

/**
 * Check if SwiftLaTeX WASM binaries are available and functional
 * @returns {Promise<{available: boolean, reason?: string}>}
 */
export async function checkLatexAvailable() {
  const vendorPath = getVendorPath();

  // Check if vendor directory exists
  try {
    await access(vendorPath, constants.R_OK);
  } catch {
    return {
      available: false,
      reason: 'SwiftLaTeX vendor directory not found'
    };
  }

  // Check for xetex.wasm (primary engine)
  const xetexPath = join(vendorPath, 'xetex.wasm');
  const hasXetex = await isRealWasmFile(xetexPath);

  if (!hasXetex) {
    return {
      available: false,
      reason: 'SwiftLaTeX WASM binaries not installed (placeholders only). Run: node scripts/vendor-tex-engine.mjs'
    };
  }

  // Check for pdftex.wasm (secondary engine)
  const pdftexPath = join(vendorPath, 'pdftex.wasm');
  const hasPdftex = await isRealWasmFile(pdftexPath);

  if (!hasPdftex) {
    return {
      available: false,
      reason: 'pdftex.wasm not found or is placeholder'
    };
  }

  return { available: true };
}

/**
 * Skip tests if LaTeX is not available with informative message
 * @param {object} _describe - Vitest describe function
 * @param {object} _it - Vitest it function
 * @returns {Promise<boolean>} True if LaTeX is available
 */
export async function skipIfLatexUnavailable(_describe, _it) {
  const result = await checkLatexAvailable();

  if (!result.available) {
    // Override describe and it to skip
    const skipMessage = `LaTeX tests skipped: ${result.reason}`;
    console.warn(`\n⚠️  ${skipMessage}\n`);
    return false;
  }

  return true;
}
