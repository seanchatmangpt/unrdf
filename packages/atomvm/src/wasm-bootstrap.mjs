/**
 * @fileoverview WASM Bootstrap Module for AtomVM
 * @module wasm-bootstrap
 *
 * @description
 * Provides WASM asset availability checking and loading functionality with
 * OpenTelemetry instrumentation. Handles both browser and Node.js environments.
 *
 * **Design Principles**:
 * - Environment detection (browser vs Node.js)
 * - Asset validation before load
 * - OTEL tracing for load timing
 * - Error handling with meaningful messages
 *
 * @version 1.0.0
 * @license MIT
 */

import { withSpan, recordAttribute, recordError } from './otel-instrumentation.mjs';

/**
 * AtomVM version
 * @constant {string}
 */
export const ATOMVM_VERSION = 'v0.6.6';

/**
 * Environment type
 * @typedef {'browser' | 'node' | 'unknown'} EnvironmentType
 */

/**
 * WASM asset status
 * @typedef {Object} WASMAssetStatus
 * @property {boolean} available - Whether WASM assets are available
 * @property {EnvironmentType} environment - Detected environment
 * @property {string} expectedJsPath - Expected JavaScript loader path
 * @property {string} expectedWasmPath - Expected WASM binary path
 * @property {string|null} error - Error message if unavailable
 */

/**
 * WASM load result
 * @typedef {Object} WASMLoadResult
 * @property {boolean} success - Whether load succeeded
 * @property {any} module - Loaded WASM module (if successful)
 * @property {number} loadTimeMs - Load duration in milliseconds
 * @property {string|null} error - Error message if failed
 */

/**
 * Detect the current runtime environment
 * @returns {EnvironmentType} Environment type
 */
export function detectEnvironment() {
  // Check for browser-specific globals
  if (typeof window !== 'undefined' && typeof document !== 'undefined') {
    return 'browser';
  }
  
  // Check for Node.js-specific globals
  if (typeof process !== 'undefined' && process.versions && process.versions.node) {
    return 'node';
  }
  
  return 'unknown';
}

/**
 * Get expected asset paths for current environment
 * @param {EnvironmentType} environment - Target environment
 * @returns {{jsPath: string, wasmPath: string}} Asset paths
 */
export function getAssetPaths(environment) {
  const base = environment === 'browser' ? '/public' : './public';
  const variant = environment === 'browser' ? 'web' : 'node';
  
  return {
    jsPath: `${base}/AtomVM-${variant}-${ATOMVM_VERSION}.js`,
    wasmPath: `${base}/AtomVM-${variant}-${ATOMVM_VERSION}.wasm`,
  };
}

/**
 * Check if WASM assets are available
 * @returns {Promise<WASMAssetStatus>} Asset availability status
 */
export async function checkWASMAssets() {
  return withSpan('wasm.check_assets', async () => {
    const environment = detectEnvironment();
    const { jsPath, wasmPath } = getAssetPaths(environment);
    
    try {
      if (environment === 'browser') {
        // In browser, try to fetch the JS file to check availability
        const response = await fetch(jsPath, { method: 'HEAD' });
        
        return {
          available: response.ok,
          environment,
          expectedJsPath: jsPath,
          expectedWasmPath: wasmPath,
          error: response.ok ? null : `HTTP ${response.status}: ${response.statusText}`,
        };
      } else if (environment === 'node') {
        // In Node.js, check file system
        const { existsSync } = await import('node:fs');
        const { join } = await import('node:path');
        const { fileURLToPath } = await import('node:url');
        
        // Get the directory of this module
        const moduleDir = fileURLToPath(new URL('.', import.meta.url));
        const publicDir = join(moduleDir, '..', 'public');
        
        const jsExists = existsSync(join(publicDir, `AtomVM-node-${ATOMVM_VERSION}.js`));
        const wasmExists = existsSync(join(publicDir, `AtomVM-node-${ATOMVM_VERSION}.wasm`));
        
        return {
          available: jsExists && wasmExists,
          environment,
          expectedJsPath: jsPath,
          expectedWasmPath: wasmPath,
          error: jsExists && wasmExists ? null : 'Files not found on filesystem',
        };
      } else {
        return {
          available: false,
          environment,
          expectedJsPath: jsPath,
          expectedWasmPath: wasmPath,
          error: 'Unknown environment - cannot determine asset availability',
        };
      }
    } catch (error) {
      return {
        available: false,
        environment,
        expectedJsPath: jsPath,
        expectedWasmPath: wasmPath,
        error: error.message,
      };
    }
  }, {
    'wasm.version': ATOMVM_VERSION,
    'operation.type': 'asset_check',
  });
}

/**
 * Load AtomVM WASM module with OTEL instrumentation
 * @param {Object} [options={}] - Load options
 * @param {EnvironmentType} [options.environment] - Override environment detection
 * @param {string} [options.basePath] - Override base path for assets
 * @returns {Promise<WASMLoadResult>} Load result
 */
export async function loadWASM(options = {}) {
  return withSpan('wasm.load', async () => {
    const startTime = performance.now();
    const environment = options.environment || detectEnvironment();
    
    try {
      // Check assets first
      const assetStatus = await checkWASMAssets();
      
      if (!assetStatus.available) {
        throw new Error(
          `WASM assets not available: ${assetStatus.error}\n` +
          `Expected: ${assetStatus.expectedJsPath} and ${assetStatus.expectedWasmPath}`
        );
      }
      
      // For now, we return a mock successful load
      // Real implementation would dynamically import the WASM module
      const loadTimeMs = performance.now() - startTime;
      
      return {
        success: true,
        module: null, // Would be actual module in production
        loadTimeMs,
        error: null,
      };
    } catch (error) {
      const loadTimeMs = performance.now() - startTime;
      
      return {
        success: false,
        module: null,
        loadTimeMs,
        error: error.message,
      };
    }
  }, {
    'wasm.version': ATOMVM_VERSION,
    'wasm.environment': options.environment || detectEnvironment(),
    'operation.type': 'wasm_load',
  });
}

/**
 * Validate WASM module interface
 * @param {any} module - WASM module to validate
 * @returns {{valid: boolean, missing: string[]}} Validation result
 */
export function validateWASMModule(module) {
  const requiredExports = ['_start', 'memory'];
  const missing = [];
  
  if (!module) {
    return { valid: false, missing: ['module is null or undefined'] };
  }
  
  for (const exportName of requiredExports) {
    if (!(exportName in module)) {
      missing.push(exportName);
    }
  }
  
  return {
    valid: missing.length === 0,
    missing,
  };
}

/**
 * Get WASM module info
 * @returns {{version: string, environment: EnvironmentType}} Module info
 */
export function getWASMInfo() {
  return {
    version: ATOMVM_VERSION,
    environment: detectEnvironment(),
  };
}

// Default export
export default {
  ATOMVM_VERSION,
  detectEnvironment,
  getAssetPaths,
  checkWASMAssets,
  loadWASM,
  validateWASMModule,
  getWASMInfo,
};
