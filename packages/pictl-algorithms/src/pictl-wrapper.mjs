/**
 * @unrdf/pictl-algorithms - PICTL WASM Bridge
 *
 * Bridge to pictl's WASM-compiled process mining algorithms.
 * This module handles loading the pictl WASM binary and provides
 * a JavaScript interface to the 41 process mining algorithms.
 *
 * @module pictl-wrapper
 */

import { resolve } from 'node:path';
import { pathToFileURL } from 'node:url';

// pictl WASM module cache
let pictlWasm = null;
let pictlModule = null;

/**
 * Load pictl WASM module
 *
 * @param {Object} options - Loading options
 * @param {string} [options.wasmPath] - Path to pictl.wasm (optional, uses npm package by default)
 * @param {string} [options.profile='cloud'] - Deployment profile (browser, iot, edge, fog, cloud)
 * @returns {Promise<Object>} pictl WASM module
 *
 * @example
 * const pictl = await loadPictlWasm({ profile: 'cloud' });
 */
export async function loadPictlWasm(options = {}) {
  const { wasmPath, profile = 'cloud' } = options;

  if (pictlModule) {
    return pictlModule;
  }

  const span = startSpan('pictl.load_wasm', { profile });

  try {
    // Try to load from @seanchatmangpt/pictl package
    // If that fails, try to load from local path
    try {
      pictlModule = await import('@seanchatmangpt/pictl');
    } catch (importError) {
      if (wasmPath) {
        // Load from custom path
        // pathToFileURL handles relative paths, spaces and Windows drive letters
        const { loadPictlWasm: loadCustom } = await import(pathToFileURL(resolve(wasmPath)).href);
        pictlModule = await loadCustom({ profile });
      } else {
        throw importError;
      }
    }

    span.end({ status: 'ok' });
    return pictlModule;
  } catch (error) {
    span.end({ status: 'error', error: error.message });
    throw new Error(`Failed to load pictl WASM: ${error.message}`);
  }
}

/**
 * Get pictl kernel instance
 *
 * @param {Object} [options] - Options forwarded to loadPictlWasm (wasmPath, profile)
 * @returns {Promise<Object>} pictl kernel
 *
 * @example
 * const kernel = await getKernel();
 * const result = await kernel.discoverDFG(eventLogHandle);
 */
export async function getKernel(options = {}) {
  const pictl = await loadPictlWasm(options);

  if (pictlWasm) {
    return pictlWasm;
  }

  const span = startSpan('pictl.create_kernel');

  try {
    // Initialize pictl kernel
    if (pictl.PictlKernel) {
      pictlWasm = await pictl.PictlKernel.create();
    } else if (pictl.default && pictl.default.PictlKernel) {
      pictlWasm = await pictl.default.PictlKernel.create();
    } else if (pictl.create) {
      // Direct WASM module
      pictlWasm = await pictl.create();
    } else {
      throw new Error('Cannot find pictl kernel constructor');
    }

    span.end({ status: 'ok' });
    return pictlWasm;
  } catch (error) {
    span.end({ status: 'error', error: error.message });
    throw new Error(`Failed to create pictl kernel: ${error.message}`);
  }
}

/**
 * Reset pictl kernel state (clear all handles)
 *
 * @returns {Promise<void>}
 *
 * @example
 * await resetKernel();
 */
export async function resetKernel() {
  if (pictlWasm && pictlWasm.softReset) {
    await pictlWasm.softReset();
  } else {
    pictlWasm = null;
  }
}

/**
 * Get pictl kernel info
 *
 * @param {Object} [options] - Options forwarded to getKernel
 * @returns {Promise<Object>} Kernel information
 *
 * @example
 * const info = await getKernelInfo();
 * console.log(info.version);
 */
export async function getKernelInfo(options = {}) {
  const kernel = await getKernel(options);

  if (kernel.version) {
    return {
      version: kernel.version(),
      algorithms: kernel.algorithms ? Object.keys(kernel.algorithms()) : [],
      capabilities: kernel.getCapabilities ? kernel.getCapabilities() : {},
    };
  }

  // Fallback: return basic info
  return {
    version: 'unknown',
    algorithms: [],
    capabilities: {
      discovery: true,
      conformance: true,
      prediction: false,
    },
  };
}

/**
 * Start an OpenTelemetry span
 *
 * @param {string} name - Span name
 * @param {Object} attributes - Span attributes
 * @returns {Object} Span object with end() method
 */
function startSpan(name, attributes = {}) {
  // Check if OTEL is available
  if (typeof globalThis.otel !== 'undefined' && globalThis.otel.trace) {
    const tracer = globalThis.otel.trace.getTracer('@unrdf/pictl-algorithms');
    const span = tracer.startSpan(name, { attributes });

    return {
      end: (attrs = {}) => {
        if (attrs.status === 'error') {
          span.recordException(attrs.error);
          span.setStatus({ code: 2, message: attrs.error });
        }
        span.setAttributes(attrs);
        span.end();
      },
    };
  }

  // No-op fallback
  return {
    end: () => {},
  };
}
