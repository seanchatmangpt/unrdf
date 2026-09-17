# Instructions

- Following Playwright test failed.
- Explain why, be concise, respect Playwright best practices.
- Provide a snippet of code with the fix, if possible.

# Test info

- Name: atomvm.spec.mjs >> AtomVM Browser Runtime >> should display required UI elements
- Location: test/playwright/atomvm.spec.mjs:39:3

# Error details

```
Error: page.goto: The network connection was lost.
Call log:
  - navigating to "http://localhost:8080/", waiting until "domcontentloaded"

```

# Test source

```ts
  1  | /**
  2  |  * @fileoverview Playwright E2E Tests for AtomVM Browser Runtime
  3  |  * @description
  4  |  * Fast tests that verify the production build works in real browsers.
  5  |  * All tests must complete within 5s SLA.
  6  |  */
  7  | 
  8  | import { test, expect } from '@playwright/test';
  9  | 
  10 | test.describe('AtomVM Browser Runtime', () => {
  11 |   test.beforeEach(async ({ page }) => {
> 12 |     await page.goto('/', { waitUntil: 'domcontentloaded' });
     |                ^ Error: page.goto: The network connection was lost.
  13 |   });
  14 | 
  15 |   test('should load the application with correct title', async ({ page }) => {
  16 |     await expect(page).toHaveTitle(/AtomVM Browser Runtime/, { timeout: 2000 });
  17 |     const heading = page.getByRole('heading', { name: /AtomVM Browser Runtime/i });
  18 |     await expect(heading).toBeVisible({ timeout: 2000 });
  19 |   });
  20 | 
  21 |   test('should verify service worker infrastructure availability', async ({ page }) => {
  22 |     const swSupported = await page.evaluate(() => 'serviceWorker' in navigator);
  23 |     expect(swSupported).toBe(true);
  24 |   });
  25 | 
  26 |   test('should have COI infrastructure prepared', async ({ page }) => {
  27 |     const coiStatus = await page.evaluate(() => {
  28 |       return {
  29 |         crossOriginIsolated: typeof crossOriginIsolated !== 'undefined',
  30 |         sharedArrayBuffer: typeof SharedArrayBuffer !== 'undefined',
  31 |         serviceWorkerSupported: 'serviceWorker' in navigator,
  32 |       };
  33 |     });
  34 | 
  35 |     expect(coiStatus.serviceWorkerSupported).toBe(true);
  36 |     expect(typeof coiStatus.crossOriginIsolated).toBe('boolean');
  37 |   });
  38 | 
  39 |   test('should display required UI elements', async ({ page }) => {
  40 |     await expect(page.locator('#status')).toBeVisible({ timeout: 2000 });
  41 |     await expect(page.locator('#terminal')).toBeVisible({ timeout: 2000 });
  42 |   });
  43 | 
  44 |   test('should have operational control buttons', async ({ page }) => {
  45 |     await expect(page.getByRole('button', { name: /Initialize AtomVM/i })).toBeVisible({ timeout: 2000 });
  46 |     await expect(page.getByRole('button', { name: /Run Example/i })).toBeVisible({ timeout: 2000 });
  47 |     await expect(page.getByRole('button', { name: /Clear Console/i })).toBeVisible({ timeout: 2000 });
  48 |   });
  49 | 
  50 |   test('should load without critical application errors', async ({ page }) => {
  51 |     const errors = [];
  52 |     page.on('pageerror', (err) => errors.push(err.message));
  53 |     
  54 |     await expect(page.locator('body')).toBeVisible({ timeout: 2000 });
  55 |     expect(errors.filter(e => e.includes('Failed to load') || e.includes('SyntaxError'))).toHaveLength(0);
  56 |   });
  57 | });
  58 | 
```