/**
 * @fileoverview AtomVM WASM Asset Download and Verification Script
 * @module scripts/download-atomvm
 *
 * @description
 * Downloads or verifies AtomVM WASM assets for browser and Node.js execution.
 * Ensures the required .js and .wasm files are present in the public/ directory.
 *
 * **Usage**:
 * ```bash
 * node scripts/download-atomvm.mjs
 * ```
 *
 * @version 1.0.0
 * @license MIT
 */

import { existsSync, mkdirSync } from 'node:fs';
import { join, dirname } from 'node:path';
import { fileURLToPath } from 'node:url';

const __filename = fileURLToPath(import.meta.url);
const __dirname = dirname(__filename);

/**
 * AtomVM version to download/verify
 * @constant {string}
 */
const ATOMVM_VERSION = 'v0.6.6';

/**
 * Required WASM assets
 * @constant {Array<string>}
 */
const REQUIRED_ASSETS = [
  `AtomVM-web-${ATOMVM_VERSION}.js`,
  `AtomVM-web-${ATOMVM_VERSION}.wasm`,
  `AtomVM-node-${ATOMVM_VERSION}.js`,
  `AtomVM-node-${ATOMVM_VERSION}.wasm`,
];

/**
 * Get the public directory path
 * @returns {string} Absolute path to public/ directory
 */
function getPublicDir() {
  return join(__dirname, '..', 'public');
}

/**
 * Check if all required assets exist
 * @returns {{missing: string[], present: string[], allPresent: boolean}} Asset status
 */
export function checkAssets() {
  const publicDir = getPublicDir();
  const missing = [];
  const present = [];

  for (const asset of REQUIRED_ASSETS) {
    const assetPath = join(publicDir, asset);
    if (existsSync(assetPath)) {
      present.push(asset);
    } else {
      missing.push(asset);
    }
  }

  return {
    missing,
    present,
    allPresent: missing.length === 0,
  };
}

/**
 * Ensure public directory exists
 * @returns {void}
 */
function ensurePublicDir() {
  const publicDir = getPublicDir();
  if (!existsSync(publicDir)) {
    console.log(`Creating public directory: ${publicDir}`);
    mkdirSync(publicDir, { recursive: true });
  }
}

/**
 * Main execution function
 * @returns {Promise<void>}
 */
async function main() {
  console.log('AtomVM Asset Verification');
  console.log('========================\n');

  ensurePublicDir();

  const status = checkAssets();
  
  console.log(`Version: ${ATOMVM_VERSION}`);
  console.log(`Public directory: ${getPublicDir()}\n`);

  if (status.allPresent) {
    console.log('✅ All required assets present:');
    status.present.forEach(asset => console.log(`   - ${asset}`));
    console.log('\n✅ AtomVM WASM assets verified successfully');
    process.exit(0);
  } else {
    console.log('❌ Missing assets:');
    status.missing.forEach(asset => console.log(`   - ${asset}`));
    
    if (status.present.length > 0) {
      console.log('\n✅ Present assets:');
      status.present.forEach(asset => console.log(`   - ${asset}`));
    }

    console.log('\n⚠️  Assets are missing. Options:');
    console.log('   1. Download from: https://github.com/atomvm/AtomVM/releases');
    console.log(`   2. Extract ${ATOMVM_VERSION} assets to: ${getPublicDir()}/`);
    console.log('   3. Or they should be bundled with this package');
    
    process.exit(1);
  }
}

// Run if called directly
if (import.meta.url === `file://${process.argv[1]}`) {
  main().catch(error => {
    console.error('Error:', error.message);
    process.exit(1);
  });
}

export default {
  checkAssets,
  getPublicDir,
  ATOMVM_VERSION,
  REQUIRED_ASSETS,
};
