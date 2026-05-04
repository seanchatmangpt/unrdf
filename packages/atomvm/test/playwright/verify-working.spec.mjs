/**
 * @fileoverview Step-by-step verification of AtomVM runtime components.
 */

import { test, expect } from '@playwright/test';

test.describe('Step-by-Step Verification', () => {
  test.beforeEach(async ({ page }) => {
    await page.goto('/', { waitUntil: 'networkidle' });
  });

  test('Page loads and is titled correctly', async ({ page }) => {
    await expect(page).toHaveTitle(/AtomVM/);
    await expect(page.locator('h1')).toContainText(/AtomVM/);
  });

  test('Core JavaScript bundle loads without errors', async ({ page }) => {
    const errors = [];
    page.on('console', msg => { if (msg.type() === 'error') errors.push(msg.text()); });
    
    await expect(page.locator('#terminal')).toBeVisible();
    expect(errors.filter(e => e.includes('Failed to load') || e.includes('SyntaxError'))).toHaveLength(0);
  });

  test('Service worker infrastructure check', async ({ page }) => {
    const swSupported = await page.evaluate(() => 'serviceWorker' in navigator);
    expect(swSupported).toBe(true);
  });

  test('COI infrastructure check', async ({ page }) => {
    const coiInfo = await page.evaluate(() => ({
      hasCrossOriginIsolated: typeof crossOriginIsolated !== 'undefined',
      hasSharedArrayBuffer: typeof SharedArrayBuffer !== 'undefined',
    }));
    
    expect(coiInfo.hasCrossOriginIsolated).toBe(true);
    expect(typeof coiInfo.hasSharedArrayBuffer).toBe('boolean');
  });

  test('Runtime UI is interactive', async ({ page }) => {
    await expect(page.locator('#status')).toBeVisible();
    await expect(page.getByRole('button', { name: /Initialize AtomVM/i })).toBeVisible();
  });
});
