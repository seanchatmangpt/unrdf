/**
 * @file L1 Maturity Tests - Compiles & Runs
 * @description Validates that all P0+P1 modules compile and run without errors
 *
 * CRITERIA:
 * - Test: npm run build && npm test (0 errors)
 * - Evidence: Build log + test summary
 */

import { test, describe, expect } from 'vitest';
import { execSync } from 'node:child_process';

describe('L1: Compiles & Runs', () => {
  test('[L1.1] v6-core builds without errors', () => {
    console.log('[L1.1] Testing v6-core build');
    try {
      const result = execSync('cd /home/user/unrdf/packages/v6-core && npm run build', {
        encoding: 'utf-8',
        timeout: 5000,
      });
      console.log('[L1.1] Build output:', result);
      expect(true).toBeTruthy();
    } catch (error) {
      throw new Error('Test failed');
    }
  });

  test('[L1.2] oxigraph builds without errors', () => {
    console.log('[L1.2] Testing oxigraph build');
    try {
      const result = execSync('cd /home/user/unrdf/packages/oxigraph && npm run build', {
        encoding: 'utf-8',
        timeout: 5000,
      });
      console.log('[L1.2] Build output:', result);
      expect(true).toBeTruthy();
    } catch (error) {
      throw new Error('Test failed');
    }
  });

  test('[L1.3] core builds without errors', () => {
    console.log('[L1.3] Testing core build');
    try {
      const result = execSync('cd /home/user/unrdf/packages/core && npm run build', {
        encoding: 'utf-8',
        timeout: 5000,
      });
      console.log('[L1.3] Build output:', result);
      expect(true).toBeTruthy();
    } catch (error) {
      throw new Error('Test failed');
    }
  });

  test('[L1.4] All P0+P1 modules import successfully', async () => {
    console.log('[L1.4] Testing module imports');

    try {
      // Import v6-core
      const v6Core = await import('@unrdf/v6-core');
      expect(v6Core).toBeTruthy();
      console.log('[L1.4] v6-core exports:', Object.keys(v6Core));

      // Import oxigraph
      const oxigraph = await import('@unrdf/oxigraph');
      expect(oxigraph).toBeTruthy();
      expect(oxigraph.createStore).toBeTruthy();
      console.log('[L1.4] oxigraph exports:', Object.keys(oxigraph));

      // Import core
      const core = await import('@unrdf/core');
      expect(core).toBeTruthy();
      console.log('[L1.4] core exports:', Object.keys(core));

      console.log('[L1.4] ✅ All imports successful');
    } catch (error) {
      throw new Error('Test failed');
    }
  });

  test('[L1.5] Basic store creation works', async () => {
    console.log('[L1.5] Testing basic store creation');

    try {
      const { createStore } = await import('@unrdf/oxigraph');
      const store = createStore();
      expect(store).toBeTruthy();
      console.log('[L1.5] ✅ Store creation works');
    } catch (error) {
      throw new Error('Test failed');
    }
  });
});

// Export for evidence reporting
export const L1_CRITERIA = {
  level: 'L1',
  name: 'Compiles & Runs',
  tests: 5,
  target: '100% (0 build/run errors)',
};
