/**
 * Vitest setup file - runs before each test
 * Centralizes WASM module initialization and cleanup
 */

import { beforeEach, afterEach } from 'vitest';

// Initialize WASM module state cleanup
beforeEach(async () => {
  try {
    // Import WASM module
    const wasm = await import('../pkg/wasm4pm.js');
    // Clear any prior state (tests initialize WASM separately)
    if (typeof wasm.clear_all_objects === 'function') {
      wasm.clear_all_objects();
    }
  } catch (e) {
    // Silently ignore - tests handle their own init
  }
});

// Clean up handles after each test
afterEach(async () => {
  try {
    const wasm = await import('../pkg/wasm4pm.js');
    if (typeof wasm.clear_all_objects === 'function') {
      wasm.clear_all_objects();
    }
  } catch (e) {
    // Silently ignore cleanup errors
  }
});
