# Instructions

- Following Playwright test failed.
- Explain why, be concise, respect Playwright best practices.
- Provide a snippet of code with the fix, if possible.

# Test info

- Name: erlang-simulation.test.mjs >> Real AtomVM Execution >> should verify Module object availability
- Location: test/playwright/erlang-simulation.test.mjs:30:3

# Error details

```
Error: page.goto: NS_ERROR_NET_RESET
Call log:
  - navigating to "http://localhost:8080/?module=hello_world", waiting until "networkidle"

```

# Page snapshot

```yaml
- generic [ref=e3]:
  - heading [level=1] [ref=e5]
  - paragraph
  - paragraph
```

# Test source

```ts
  1  | /**
  2  |  * @fileoverview Tests for Real AtomVM Execution
  3  |  * @description
  4  |  * Verifies AtomVM WASM initialization and .avm execution capability.
  5  |  */
  6  | 
  7  | import { test, expect } from '@playwright/test';
  8  | 
  9  | test.describe('Real AtomVM Execution', () => {
  10 |   test.beforeEach(async ({ page }) => {
> 11 |     await page.goto('/?module=hello_world', { waitUntil: 'networkidle' });
     |                ^ Error: page.goto: NS_ERROR_NET_RESET
  12 |     await page.waitForSelector('.status.ready', { timeout: 15000 });
  13 |   });
  14 | 
  15 |   test('should initialize real AtomVM WASM module', async ({ page }) => {
  16 |     await page.getByRole('button', { name: /Initialize AtomVM/i }).click();
  17 |     const terminal = page.locator('#terminal');
  18 |     
  19 |     await expect(terminal).toContainText(/AtomVM WASM module loaded/i, { timeout: 10000 });
  20 |   });
  21 | 
  22 |   test('should execute .avm example when triggered', async ({ page }) => {
  23 |     await page.getByRole('button', { name: /Initialize AtomVM/i }).click();
  24 |     await expect(page.locator('#terminal')).toContainText(/AtomVM WASM module loaded/i, { timeout: 10000 });
  25 | 
  26 |     await page.getByRole('button', { name: /Run Example/i }).click();
  27 |     await expect(page.locator('#terminal')).toContainText(/Executing/i, { timeout: 5000 });
  28 |   });
  29 | 
  30 |   test('should verify Module object availability', async ({ page }) => {
  31 |     await page.getByRole('button', { name: /Initialize AtomVM/i }).click();
  32 |     await expect(page.locator('#terminal')).toContainText(/AtomVM WASM module loaded/i, { timeout: 10000 });
  33 | 
  34 |     const moduleExists = await page.evaluate(() => typeof window.Module !== 'undefined');
  35 |     expect(moduleExists).toBe(true);
  36 |   });
  37 | });
  38 | 
```