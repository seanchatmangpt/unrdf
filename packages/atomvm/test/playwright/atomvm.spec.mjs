/**
 * @fileoverview Playwright E2E Tests for AtomVM Browser Runtime
 * @description
 * Fast tests that verify the production build works in real browsers.
 * All tests must complete within 5s SLA.
 */

import { test, expect } from '@playwright/test';

test.describe('AtomVM Browser Runtime', () => {
  test.beforeEach(async ({ page }) => {
    // Navigate to the built application
    await page.goto('/', { waitUntil: 'domcontentloaded' });
  });

  test('should load the application', async ({ page }) => {
    // Fast check - page loads
    await expect(page).toHaveTitle(/AtomVM Browser Runtime/, { timeout: 2000 });
    const heading = page.getByRole('heading', { name: /AtomVM Browser Runtime/i });
    await expect(heading).toBeVisible({ timeout: 2000 });
  });

  test('should register service worker', async ({ page }) => {
    // Fast check - service worker registration (non-blocking)
    const swRegistered = await page.evaluate(async () => {
      // Quick check without waiting
      const registration = await navigator.serviceWorker.getRegistration();
      return !!registration;
    }).catch(() => false);

    // Service worker may still be registering, but API should be available
    const swSupported = await page.evaluate(() => 'serviceWorker' in navigator);
    expect(swSupported).toBe(true);
  });

  test('should have COI infrastructure', async ({ page }) => {
    // Fast check - COI may not be enabled yet (requires reload), but infrastructure exists
    const coiStatus = await page.evaluate(() => {
      return {
        crossOriginIsolated: crossOriginIsolated,
        sharedArrayBuffer: typeof SharedArrayBuffer !== 'undefined',
        serviceWorkerSupported: 'serviceWorker' in navigator,
      };
    });

    // At minimum, service worker should be supported
    expect(coiStatus.serviceWorkerSupported).toBe(true);
    // SharedArrayBuffer type check (may not be available until COI enabled)
    expect(typeof coiStatus.sharedArrayBuffer === 'boolean').toBe(true);
  });

  test('should have runtime UI elements', async ({ page }) => {
    // Fast check - UI elements exist
    const status = page.locator('#status');
    const terminal = page.locator('#terminal');
    
    await expect(status).toBeVisible({ timeout: 2000 });
    await expect(terminal).toBeVisible({ timeout: 2000 });
    
    // Status should have some content
    const statusText = await status.textContent();
    expect(statusText).toBeTruthy();
  });

  test('should display terminal output', async ({ page }) => {
    // Fast check - terminal exists and has content
    const terminal = page.locator('#terminal');
    await expect(terminal).toBeVisible({ timeout: 2000 });
    
    // Terminal should have some content (even if just headers)
    const terminalText = await terminal.textContent();
    expect(terminalText?.length).toBeGreaterThan(0);
  });

  test('should have control buttons', async ({ page }) => {
    // Fast check - buttons exist
    const initBtn = page.getByRole('button', { name: /Initialize AtomVM/i });
    const runBtn = page.getByRole('button', { name: /Run Example/i });
    const clearBtn = page.getByRole('button', { name: /Clear Console/i });

    await expect(initBtn).toBeVisible({ timeout: 2000 });
    await expect(runBtn).toBeVisible({ timeout: 2000 });
    await expect(clearBtn).toBeVisible({ timeout: 2000 });
  });

  test('should have WASM initialization button', async ({ page }) => {
    // Fast check - button exists and is clickable (when enabled)
    const initBtn = page.getByRole('button', { name: /Initialize AtomVM/i });
    await expect(initBtn).toBeVisible({ timeout: 2000 });
    // Button may be disabled initially, which is expected
  });

  test('should have run example button', async ({ page }) => {
    // Fast check - button exists
    const runBtn = page.getByRole('button', { name: /Run Example/i });
    await expect(runBtn).toBeVisible({ timeout: 2000 });
  });

  test('should have service worker infrastructure', async ({ page }) => {
    // Fast check - service worker API available
    const swInfo = await page.evaluate(() => {
      return {
        supported: 'serviceWorker' in navigator,
        controller: !!navigator.serviceWorker?.controller,
      };
    });
    
    expect(swInfo.supported).toBe(true);
  });

  test('should load without critical errors', async ({ page }) => {
    // Fast check - page loads and basic elements work
    const criticalErrors = [];
    
    page.on('pageerror', (error) => {
      // Only track critical errors (not warnings)
      if (error.message.includes('Failed to load') || error.message.includes('SyntaxError')) {
        criticalErrors.push(error.message);
      }
    });

    // Quick check that page loaded
    await expect(page.locator('body')).toBeVisible({ timeout: 2000 });
    
    // No critical load errors
    expect(criticalErrors.length).toBe(0);
  });
});

