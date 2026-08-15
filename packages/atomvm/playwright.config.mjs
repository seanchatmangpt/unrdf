/**
 * Playwright configuration for the real AtomVM/WASM OCEL v2 peer explorer.
 * Local preview emits COOP/COEP headers. When PLAYWRIGHT_BASE_URL is set the
 * same tests execute against the already-deployed public origin instead.
 */
import { defineConfig, devices } from '@playwright/test';

const remoteBaseUrl = process.env.PLAYWRIGHT_BASE_URL;

export default defineConfig({
  testDir: './test/playwright',
  fullyParallel: false,
  forbidOnly: !!process.env.CI,
  retries: process.env.CI ? 1 : 0,
  workers: 1,
  reporter: process.env.CI ? [['line'], ['html', { open: 'never' }]] : 'html',
  timeout: 60000,
  expect: { timeout: 20000 },

  use: {
    baseURL: remoteBaseUrl || 'http://127.0.0.1:8080',
    trace: 'retain-on-failure',
    screenshot: 'only-on-failure',
    video: 'retain-on-failure',
  },

  projects: [
    {
      name: 'chromium',
      use: { ...devices['Desktop Chrome'] },
    },
    {
      name: 'firefox-smoke',
      use: { ...devices['Desktop Firefox'] },
      testIgnore: /ocel-p2p\.spec\.mjs/,
    },
    {
      name: 'webkit-smoke',
      use: { ...devices['Desktop Safari'] },
      testIgnore: /ocel-p2p\.spec\.mjs/,
    },
  ],

  webServer: remoteBaseUrl ? undefined : {
    command: 'pnpm exec vite preview --host 127.0.0.1 --port 8080',
    url: 'http://127.0.0.1:8080',
    reuseExistingServer: !process.env.CI,
    timeout: 120000,
    stdout: 'pipe',
    stderr: 'pipe',
  },
});
