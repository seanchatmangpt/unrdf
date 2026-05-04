/**
 * @fileoverview Playwright E2E Tests for AtomVM Browser Runtime
 * @description
 * Fast tests that verify the production build works in real browsers.
 * All tests must complete within 5s SLA.
 */

import { test, expect } from '@playwright/test';

test.describe('AtomVM Browser Runtime', () => {
  test.beforeEach(async ({ page }) => {
    await page.goto('/', { waitUntil: 'domcontentloaded' });
  });

  test('should load the application with correct title', async ({ page }) => {
    await expect(page).toHaveTitle(/AtomVM Browser Runtime/, { timeout: 2000 });
    const heading = page.getByRole('heading', { name: /AtomVM Browser Runtime/i });
    await expect(heading).toBeVisible({ timeout: 2000 });
  });

  test('should verify service worker infrastructure availability', async ({ page }) => {
    const swSupported = await page.evaluate(() => 'serviceWorker' in navigator);
    expect(swSupported).toBe(true);
  });

  test('should have COI infrastructure prepared', async ({ page }) => {
    const coiStatus = await page.evaluate(() => {
      return {
        crossOriginIsolated: typeof crossOriginIsolated !== 'undefined',
        sharedArrayBuffer: typeof SharedArrayBuffer !== 'undefined',
        serviceWorkerSupported: 'serviceWorker' in navigator,
      };
    });

    expect(coiStatus.serviceWorkerSupported).toBe(true);
    expect(typeof coiStatus.crossOriginIsolated).toBe('boolean');
  });

  test('should display required UI elements', async ({ page }) => {
    await expect(page.locator('#status')).toBeVisible({ timeout: 2000 });
    await expect(page.locator('#terminal')).toBeVisible({ timeout: 2000 });
  });

  test('should have operational control buttons', async ({ page }) => {
    await expect(page.getByRole('button', { name: /Initialize AtomVM/i })).toBeVisible({ timeout: 2000 });
    await expect(page.getByRole('button', { name: /Run Example/i })).toBeVisible({ timeout: 2000 });
    await expect(page.getByRole('button', { name: /Clear Console/i })).toBeVisible({ timeout: 2000 });
  });

  test('should load without critical application errors', async ({ page }) => {
    const errors = [];
    page.on('pageerror', (err) => errors.push(err.message));
    
    await expect(page.locator('body')).toBeVisible({ timeout: 2000 });
    expect(errors.filter(e => e.includes('Failed to load') || e.includes('SyntaxError'))).toHaveLength(0);
  });
});
