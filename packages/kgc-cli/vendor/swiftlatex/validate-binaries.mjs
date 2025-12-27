#!/usr/bin/env node
/**
 * @file validate-binaries.mjs
 * @description Validates SwiftLaTeX WASM binaries are properly installed
 * @usage node vendor/swiftlatex/validate-binaries.mjs
 */

import { readFileSync, statSync } from 'node:fs';
import { fileURLToPath } from 'node:url';
import { dirname, join } from 'node:path';

const __filename = fileURLToPath(import.meta.url);
const __dirname = dirname(__filename);

// Expected binary sizes (approximate)
const MIN_WASM_SIZE = 1024 * 1024; // 1 MB minimum (placeholders are <1 KB)
const MAX_WASM_SIZE = 30 * 1024 * 1024; // 30 MB maximum

const EXPECTED_BINARIES = [
  { name: 'xetex.wasm', minSize: 10 * 1024 * 1024, maxSize: 25 * 1024 * 1024 },
  { name: 'pdftex.wasm', minSize: 8 * 1024 * 1024, maxSize: 20 * 1024 * 1024 },
];

/**
 * Validates a single WASM binary
 * @param {string} name - Binary filename
 * @param {number} minSize - Minimum expected size in bytes
 * @param {number} maxSize - Maximum expected size in bytes
 * @returns {{valid: boolean, error?: string, size?: number}}
 */
function validateBinary(name, minSize, maxSize) {
  const path = join(__dirname, name);

  try {
    const stats = statSync(path);
    const size = stats.size;

    // Check if placeholder (too small)
    if (size < MIN_WASM_SIZE) {
      return {
        valid: false,
        error: `PLACEHOLDER: File is ${size} bytes (expected >${(minSize / 1024 / 1024).toFixed(1)} MB)`,
        size,
      };
    }

    // Check if within expected range
    if (size < minSize || size > maxSize) {
      return {
        valid: false,
        error: `Size ${(size / 1024 / 1024).toFixed(1)} MB outside expected range ${(minSize / 1024 / 1024).toFixed(1)}-${(maxSize / 1024 / 1024).toFixed(1)} MB`,
        size,
      };
    }

    // Read first bytes to check WASM magic number
    const buffer = readFileSync(path, { encoding: null });
    const magic = buffer.slice(0, 4);
    const expectedMagic = Buffer.from([0x00, 0x61, 0x73, 0x6d]); // \0asm

    if (!magic.equals(expectedMagic)) {
      return {
        valid: false,
        error: `Invalid WASM magic number: expected \\0asm, got ${magic.toString('hex')}`,
        size,
      };
    }

    // Check WASM version (should be 1)
    const version = buffer.readUInt32LE(4);
    if (version !== 1) {
      return {
        valid: false,
        error: `Invalid WASM version: expected 1, got ${version}`,
        size,
      };
    }

    return { valid: true, size };

  } catch (error) {
    return {
      valid: false,
      error: `Failed to read file: ${error.message}`,
    };
  }
}

/**
 * Main validation routine
 */
function main() {
  console.log('SwiftLaTeX WASM Binary Validation\n');
  console.log('Location:', __dirname);
  console.log('─'.repeat(70));

  let allValid = true;
  const results = [];

  for (const binary of EXPECTED_BINARIES) {
    const result = validateBinary(binary.name, binary.minSize, binary.maxSize);
    results.push({ name: binary.name, ...result });

    const status = result.valid ? '✅ VALID' : '❌ INVALID';
    const sizeStr = result.size ? `${(result.size / 1024 / 1024).toFixed(2)} MB` : 'N/A';

    console.log(`\n${binary.name}:`);
    console.log(`  Status: ${status}`);
    console.log(`  Size:   ${sizeStr}`);

    if (!result.valid) {
      console.log(`  Error:  ${result.error}`);
      allValid = false;
    }
  }

  console.log('\n' + '─'.repeat(70));

  if (allValid) {
    console.log('\n✅ All binaries validated successfully!');
    console.log('SwiftLaTeX WASM engines are ready to use.\n');
    process.exit(0);
  } else {
    console.log('\n❌ Validation failed!');
    console.log('\nPlaceholder files detected. To install production binaries:');
    console.log('  1. See vendor/INSTALLATION.md for instructions');
    console.log('  2. Download from https://github.com/SwiftLaTeX/SwiftLaTeX');
    console.log('  3. Verify checksums before copying to vendor/swiftlatex/\n');
    process.exit(1);
  }
}

main();
