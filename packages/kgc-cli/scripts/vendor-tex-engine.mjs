#!/usr/bin/env node
/**
 * @fileoverview Vendor Script - Download and Setup SwiftLaTeX WASM Binaries
 *
 * This script downloads SwiftLaTeX WASM binaries and sets up the vendor directory
 * for LaTeX→PDF compilation in Node.js.
 *
 * Usage:
 *   node scripts/vendor-tex-engine.mjs
 *
 * What it does:
 * 1. Downloads SwiftLaTeX release from GitHub
 * 2. Extracts WASM binaries and JS glue code
 * 3. Copies to packages/kgc-cli/vendor/swiftlatex/
 * 4. Validates file integrity
 *
 * @module vendor-tex-engine
 */

import { createWriteStream, mkdirSync, existsSync } from 'node:fs';
import { readdir, stat, copyFile, writeFile } from 'node:fs/promises';
import { dirname, join } from 'node:path';
import { fileURLToPath } from 'node:url';
import { pipeline } from 'node:stream/promises';
import { createGunzip } from 'node:zlib';
import { Extract } from 'tar';
import https from 'node:https';

// =============================================================================
// Configuration
// =============================================================================

const SWIFTLATEX_RELEASE = 'v15022022';
const SWIFTLATEX_URL = `https://github.com/SwiftLaTeX/SwiftLaTeX/archive/refs/tags/${SWIFTLATEX_RELEASE}.zip`;

const CURRENT_FILE = fileURLToPath(import.meta.url);
const SCRIPT_DIR = dirname(CURRENT_FILE);
const PKG_ROOT = join(SCRIPT_DIR, '..');
const VENDOR_DIR = join(PKG_ROOT, 'vendor', 'swiftlatex');
const TEMP_DIR = join(PKG_ROOT, '.tmp');

// =============================================================================
// Utility Functions
// =============================================================================

/**
 * Download file from URL to destination
 * @param {string} url - URL to download
 * @param {string} dest - Destination file path
 * @returns {Promise<void>}
 */
async function downloadFile(url, dest) {
  mkdirSync(dirname(dest), { recursive: true });

  return new Promise((resolve, reject) => {
    const file = createWriteStream(dest);

    https.get(url, (response) => {
      // Handle redirects
      if (response.statusCode === 301 || response.statusCode === 302) {
        file.close();
        return downloadFile(response.headers.location, dest).then(resolve, reject);
      }

      if (response.statusCode !== 200) {
        file.close();
        return reject(new Error(`Failed to download: ${response.statusCode}`));
      }

      response.pipe(file);

      file.on('finish', () => {
        file.close(resolve);
      });
    }).on('error', (err) => {
      file.close();
      reject(err);
    });
  });
}

/**
 * Extract ZIP archive (requires unzip command)
 * @param {string} zipPath - Path to ZIP file
 * @param {string} extractDir - Directory to extract to
 * @returns {Promise<void>}
 */
async function extractZip(zipPath, extractDir) {
  const { exec } = await import('node:child_process');
  const { promisify } = await import('node:util');
  const execAsync = promisify(exec);

  mkdirSync(extractDir, { recursive: true });

  try {
    await execAsync(`unzip -q "${zipPath}" -d "${extractDir}"`);
  } catch (err) {
    throw new Error(`Failed to extract ZIP: ${err.message}. Ensure 'unzip' is installed.`);
  }
}

/**
 * Copy files matching pattern from source to destination
 * @param {string} srcDir - Source directory
 * @param {string} destDir - Destination directory
 * @param {RegExp} pattern - File pattern to match
 * @returns {Promise<string[]>} - List of copied files
 */
async function copyMatchingFiles(srcDir, destDir, pattern) {
  const copied = [];

  async function walk(dir) {
    const entries = await readdir(dir, { withFileTypes: true });

    for (const entry of entries) {
      const fullPath = join(dir, entry.name);

      if (entry.isDirectory()) {
        await walk(fullPath);
      } else if (pattern.test(entry.name)) {
        const destPath = join(destDir, entry.name);
        await copyFile(fullPath, destPath);
        copied.push(entry.name);
        console.log(`  ✓ Copied: ${entry.name}`);
      }
    }
  }

  await walk(srcDir);
  return copied;
}

// =============================================================================
// Main Installation Logic
// =============================================================================

async function main() {
  console.log('SwiftLaTeX WASM Engine Vendor Script');
  console.log('=====================================\n');

  // Step 1: Check if already vendored
  if (existsSync(join(VENDOR_DIR, 'swiftlatex.wasm'))) {
    const stats = await stat(join(VENDOR_DIR, 'swiftlatex.wasm'));
    if (stats.size > 100000) { // Placeholder files are ~1KB
      console.log('✓ SwiftLaTeX WASM binaries already vendored');
      console.log(`  Size: ${(stats.size / 1024 / 1024).toFixed(2)} MB\n`);
      return;
    }
  }

  console.log('Downloading SwiftLaTeX WASM binaries...\n');

  // Step 2: Download release
  const zipPath = join(TEMP_DIR, `swiftlatex-${SWIFTLATEX_RELEASE}.zip`);
  const extractPath = join(TEMP_DIR, 'swiftlatex-extracted');

  try {
    console.log(`Downloading: ${SWIFTLATEX_URL}`);
    await downloadFile(SWIFTLATEX_URL, zipPath);
    console.log(`✓ Downloaded to: ${zipPath}\n`);

    // Step 3: Extract
    console.log('Extracting archive...');
    await extractZip(zipPath, extractPath);
    console.log('✓ Extracted\n');

    // Step 4: Copy required files
    const srcDir = join(extractPath, `SwiftLaTeX-${SWIFTLATEX_RELEASE.slice(1)}`);
    const binDir = join(srcDir, 'editor', 'public', 'bin');

    console.log('Copying WASM binaries to vendor directory...');
    mkdirSync(VENDOR_DIR, { recursive: true });

    const files = await copyMatchingFiles(binDir, VENDOR_DIR, /^swiftlatex\.(wasm|js)$/);

    if (files.length < 2) {
      throw new Error('Expected swiftlatex.wasm and swiftlatex.js not found');
    }

    // Step 5: Create manifest
    const manifest = {
      version: SWIFTLATEX_RELEASE,
      source: SWIFTLATEX_URL,
      installedAt: new Date().toISOString(),
      files: files
    };

    await writeFile(
      join(VENDOR_DIR, 'MANIFEST.json'),
      JSON.stringify(manifest, null, 2)
    );

    console.log('\n✓ Installation complete!\n');
    console.log('Files installed:');
    for (const file of files) {
      const filePath = join(VENDOR_DIR, file);
      const stats = await stat(filePath);
      console.log(`  - ${file} (${(stats.size / 1024 / 1024).toFixed(2)} MB)`);
    }

  } catch (err) {
    console.error('\n✗ Installation failed:');
    console.error(`  ${err.message}\n`);
    process.exit(1);
  }
}

main().catch((err) => {
  console.error('Fatal error:', err);
  process.exit(1);
});
