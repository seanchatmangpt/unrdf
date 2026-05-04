/**
 * @fileoverview Tests for Real AtomVM Execution
 * @description
 * Verifies AtomVM WASM initialization and .avm execution capability.
 */

import { test, expect } from '@playwright/test';

test.describe('Real AtomVM Execution', () => {
  test.beforeEach(async ({ page }) => {
    await page.goto('/?module=hello_world', { waitUntil: 'networkidle' });
    await page.waitForSelector('.status.ready', { timeout: 15000 });
  });

  test('should initialize real AtomVM WASM module', async ({ page }) => {
    await page.getByRole('button', { name: /Initialize AtomVM/i }).click();
    const terminal = page.locator('#terminal');
    
    await expect(terminal).toContainText(/AtomVM WASM module loaded/i, { timeout: 10000 });
  });

  test('should execute .avm example when triggered', async ({ page }) => {
    await page.getByRole('button', { name: /Initialize AtomVM/i }).click();
    await expect(page.locator('#terminal')).toContainText(/AtomVM WASM module loaded/i, { timeout: 10000 });

    await page.getByRole('button', { name: /Run Example/i }).click();
    await expect(page.locator('#terminal')).toContainText(/Executing/i, { timeout: 5000 });
  });

  test('should verify Module object availability', async ({ page }) => {
    await page.getByRole('button', { name: /Initialize AtomVM/i }).click();
    await expect(page.locator('#terminal')).toContainText(/AtomVM WASM module loaded/i, { timeout: 10000 });

    const moduleExists = await page.evaluate(() => typeof window.Module !== 'undefined');
    expect(moduleExists).toBe(true);
  });
});
