# Instructions

- Following Playwright test failed.
- Explain why, be concise, respect Playwright best practices.
- Provide a snippet of code with the fix, if possible.

# Test info

- Name: verify-working.spec.mjs >> Step-by-Step Verification >> Page loads and is titled correctly
- Location: test/playwright/verify-working.spec.mjs:12:3

# Error details

```
Error: page.goto: net::ERR_EMPTY_RESPONSE at http://localhost:8080/
Call log:
  - navigating to "http://localhost:8080/", waiting until "networkidle"

```

# Test source

```ts
  1  | /**
  2  |  * @fileoverview Step-by-step verification of AtomVM runtime components.
  3  |  */
  4  | 
  5  | import { test, expect } from '@playwright/test';
  6  | 
  7  | test.describe('Step-by-Step Verification', () => {
  8  |   test.beforeEach(async ({ page }) => {
> 9  |     await page.goto('/', { waitUntil: 'networkidle' });
     |                ^ Error: page.goto: net::ERR_EMPTY_RESPONSE at http://localhost:8080/
  10 |   });
  11 | 
  12 |   test('Page loads and is titled correctly', async ({ page }) => {
  13 |     await expect(page).toHaveTitle(/AtomVM/);
  14 |     await expect(page.locator('h1')).toContainText(/AtomVM/);
  15 |   });
  16 | 
  17 |   test('Core JavaScript bundle loads without errors', async ({ page }) => {
  18 |     const errors = [];
  19 |     page.on('console', msg => { if (msg.type() === 'error') errors.push(msg.text()); });
  20 |     
  21 |     await expect(page.locator('#terminal')).toBeVisible();
  22 |     expect(errors.filter(e => e.includes('Failed to load') || e.includes('SyntaxError'))).toHaveLength(0);
  23 |   });
  24 | 
  25 |   test('Service worker infrastructure check', async ({ page }) => {
  26 |     const swSupported = await page.evaluate(() => 'serviceWorker' in navigator);
  27 |     expect(swSupported).toBe(true);
  28 |   });
  29 | 
  30 |   test('COI infrastructure check', async ({ page }) => {
  31 |     const coiInfo = await page.evaluate(() => ({
  32 |       hasCrossOriginIsolated: typeof crossOriginIsolated !== 'undefined',
  33 |       hasSharedArrayBuffer: typeof SharedArrayBuffer !== 'undefined',
  34 |     }));
  35 |     
  36 |     expect(coiInfo.hasCrossOriginIsolated).toBe(true);
  37 |     expect(typeof coiInfo.hasSharedArrayBuffer).toBe('boolean');
  38 |   });
  39 | 
  40 |   test('Runtime UI is interactive', async ({ page }) => {
  41 |     await expect(page.locator('#status')).toBeVisible();
  42 |     await expect(page.getByRole('button', { name: /Initialize AtomVM/i })).toBeVisible();
  43 |   });
  44 | });
  45 | 
```