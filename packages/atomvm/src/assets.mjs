/**
 * @file AtomVM WASM asset naming - single source of truth.
 *
 * Browser-safe (no node: imports). The repo is version-agnostic, so the tag
 * embedded in asset file names is fixed rather than a release number. Runtime
 * code, download/verify scripts and tests all derive names from here so they
 * cannot drift from the files in public/.
 */

/** Tag embedded in asset file names (version-agnostic by repo policy). */
export const ATOMVM_VERSION = 'agnostic';

/** Emscripten hardcodes this wasm name; loaders must redirect it via locateFile. */
export const EMSCRIPTEN_WASM_NAME = 'AtomVM.wasm';

/**
 * @param {'web'|'node'} variant
 * @param {'js'|'wasm'} ext
 * @returns {string} file name inside public/
 */
export function atomvmAssetName(variant, ext) {
  if (variant !== 'web' && variant !== 'node') {
    throw new TypeError(`variant must be 'web' or 'node', got ${String(variant)}`);
  }
  if (ext !== 'js' && ext !== 'wasm') {
    throw new TypeError(`ext must be 'js' or 'wasm', got ${String(ext)}`);
  }
  return `AtomVM-${variant}-${ATOMVM_VERSION}.${ext}`;
}

/** Every asset a complete deployment must ship. */
export const REQUIRED_ASSET_NAMES = Object.freeze([
  atomvmAssetName('web', 'js'),
  atomvmAssetName('web', 'wasm'),
  atomvmAssetName('node', 'js'),
  atomvmAssetName('node', 'wasm'),
]);
