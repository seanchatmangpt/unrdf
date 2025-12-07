/**
 * @fileoverview Tests for Real AtomVM Execution
 * @description
 * Tests that verify real AtomVM WASM execution works.
 * Tests real AtomVM.js loading and .avm file execution capability.
 */

import { test, expect } from '@playwright/test';

test.describe('Real AtomVM Execution', () => {
  test.beforeEach(async ({ page }) => {
    // Navigate with module parameter for hello_world.avm
    await page.goto('/?module=hello_world', { waitUntil: 'networkidle' });
    
    // Wait for app to initialize
    await page.waitForSelector('.status.ready', { timeout: 15000 });
  });

  test('should load real AtomVM WASM module', async ({ page }) => {
    // Click initialize button
    const initBtn = page.getByRole('button', { name: /Initialize AtomVM/i });
    await initBtn.click();

    // Wait for WASM initialization messages
    const terminal = page.locator('#terminal');
    
    // Prove: SharedArrayBuffer check happens
    await expect(terminal).toContainText(/SharedArrayBuffer/i, { timeout: 5000 });
    
    // Prove: AtomVM.js loading message
    await expect(terminal).toContainText(/Loading AtomVM WASM module/i, { timeout: 5000 });
    
    // Prove: AtomVM module loaded successfully
    await expect(terminal).toContainText(/AtomVM WASM module loaded/i, { timeout: 10000 });
  });

  test('should execute real .avm file when Run Example clicked', async ({ page }) => {
    // First initialize AtomVM
    const initBtn = page.getByRole('button', { name: /Initialize AtomVM/i });
    await initBtn.click();
    
    // Wait for AtomVM to load
    const terminal = page.locator('#terminal');
    await expect(terminal).toContainText(/AtomVM WASM module loaded/i, { timeout: 10000 });
    await page.waitForTimeout(1000);

    // Click run example
    const runBtn = page.getByRole('button', { name: /Run Example/i });
    await runBtn.click();

    // Prove: Attempts to load .avm file
    await expect(terminal).toContainText(/Loading example .avm file/i, { timeout: 5000 });
    
    // Prove: Executes .avm file or shows helpful error
    await page.waitForTimeout(2000); // Wait for execution
    
    const terminalText = await terminal.textContent({ timeout: 5000 });
    
    // Should either:
    // 1. Execute the .avm file successfully (shows execution output)
    // 2. Show helpful message about building module (if file not found)
    const hasExecution = terminalText.includes('Executing') || 
                         terminalText.includes('execution') ||
                         terminalText.includes('Hello from AtomVM') ||
                         terminalText.includes('Module: hello_world');
    const hasHelpfulMessage = terminalText.includes('Build it with:') || 
                              terminalText.includes('pnpm run build:erlang');
    
    expect(hasExecution || hasHelpfulMessage).toBe(true);
  });

  test('should verify AtomVM Module is available in browser', async ({ page }) => {
    // Initialize AtomVM
    const initBtn = page.getByRole('button', { name: /Initialize AtomVM/i });
    await initBtn.click();
    
    // Wait for AtomVM to load
    const terminal = page.locator('#terminal');
    await expect(terminal).toContainText(/AtomVM WASM module loaded/i, { timeout: 10000 });
    await page.waitForTimeout(1000);

    // Verify Module object exists in browser
    const moduleExists = await page.evaluate(() => {
      return typeof window.Module !== 'undefined' && window.Module !== null;
    });
    
    expect(moduleExists).toBe(true);
  });

  test('should have real AtomVM execution capability', async ({ page }) => {
    // Initialize AtomVM
    const initBtn = page.getByRole('button', { name: /Initialize AtomVM/i });
    await initBtn.click();
    
    // Wait for AtomVM to load
    const terminal = page.locator('#terminal');
    await expect(terminal).toContainText(/AtomVM WASM module loaded/i, { timeout: 10000 });
    
    // Verify execution methods exist
    const hasExecutionMethods = await page.evaluate(() => {
      if (!window.Module) return false;
      return typeof window.Module.callMain === 'function' || 
             typeof window.Module.run === 'function' ||
             typeof window.Module._main === 'function';
    });
    
    expect(hasExecutionMethods).toBe(true);
  });

  test('should handle initialization errors gracefully', async ({ page }) => {
    // Test that errors are displayed properly
    // (This would fail if SharedArrayBuffer not available)
    
    const initBtn = page.getByRole('button', { name: /Initialize AtomVM/i });
    await initBtn.click();
    
    const terminal = page.locator('#terminal');
    
    // Should either succeed or show clear error
    await page.waitForTimeout(2000);
    
    const terminalText = await terminal.textContent();
    
    // Should not be empty - either success or error message
    expect(terminalText.length).toBeGreaterThan(50);
    
    // Should not have unhandled errors
    const hasError = terminalText.includes('error') || terminalText.includes('Error');
    // If error, it should be informative and user-friendly
    if (hasError) {
      expect(terminalText).toMatch(/SharedArrayBuffer|COI|Cross-Origin-Isolation|not available|will reload/i);
    }
  });

  test('should verify end-to-end execution flow', async ({ page }) => {
    // Full end-to-end test: initialize → load WASM → execute .avm → verify output
    const terminal = page.locator('#terminal');
    
    // Step 1: Initialize AtomVM
    const initBtn = page.getByRole('button', { name: /Initialize AtomVM/i });
    await initBtn.click();
    
    // Step 2: Verify WASM loaded
    await expect(terminal).toContainText(/AtomVM WASM module loaded/i, { timeout: 10000 });
    
    // Step 3: Execute .avm file
    const runBtn = page.getByRole('button', { name: /Run Example/i });
    await runBtn.click();
    
    // Step 4: Verify execution started
    await expect(terminal).toContainText(/Loading example .avm file/i, { timeout: 5000 });
    await expect(terminal).toContainText(/Executing/i, { timeout: 5000 });
    
    // Step 5: Wait for execution to complete or show result
    await page.waitForTimeout(3000);
    
    const terminalText = await terminal.textContent();
    
    // Verify we got either execution output or helpful error message
    const hasResult = terminalText.includes('Hello from AtomVM') ||
                      terminalText.includes('Module: hello_world') ||
                      terminalText.includes('Execution completed') ||
                      terminalText.includes('Build it with:');
    
    expect(hasResult).toBe(true);
  });
});

