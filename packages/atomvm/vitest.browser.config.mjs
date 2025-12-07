/**
 * @file Vitest Browser Configuration for AtomVM
 * @description
 * Real browser testing via Playwright for service worker and COI verification.
 * Tests run against built production bundle served via simple HTTP server.
 */

import { defineConfig } from 'vitest/config';
import { resolve } from 'path';

export default defineConfig({
  test: {
    name: 'atomvm-browser',
    include: ['test/browser/**/*.test.mjs'],
    browser: {
      enabled: true,
      provider: 'playwright',
      name: 'chromium',
      headless: true,
      screenshotFailures: true,
    },
    testTimeout: 30000,
    hookTimeout: 15000,
    isolate: true,
  },
  resolve: {
    alias: {
      '@unrdf/atomvm': resolve(__dirname, 'src/index.mjs'),
    },
  },
});

