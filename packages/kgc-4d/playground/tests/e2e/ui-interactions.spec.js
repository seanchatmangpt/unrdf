import { test, expect } from './fixtures.js';

/**
 * UI Interaction Tests
 *
 * Tests:
 * - Tab navigation
 * - Component visibility
 * - Status indicators
 * - Error handling in UI
 */

test.describe('UI Interactions', () => {
  test('should navigate between tabs', async ({ page }) => {
    await page.goto('/');
    await page.waitForTimeout(2000);

    const tabs = ['4D Explorer', 'Entities', 'Shard Data', 'Documentation'];

    for (const tabName of tabs) {
      const tab = page.locator(`button:has-text("${tabName}")`);
      await expect(tab).toBeVisible();
      await tab.click();

      await page.waitForTimeout(500);

      // Tab should be highlighted/active
      const activeTab = page.locator(
        `button:has-text("${tabName}"):has([class*="universe-500"])`
      );
      await expect(activeTab).toBeVisible();
    }
  });

  test('should display Universe Explorer', async ({ page }) => {
    await page.goto('/');
    await page.waitForTimeout(2000);

    const explorerTab = page.locator('button:has-text("4D Explorer")');
    await explorerTab.click();

    await page.waitForTimeout(1000);

    // Should show architecture diagram
    expect(await page.locator('text=Server — The Universe').isVisible()).toBe(true);
    expect(await page.locator('text=Browser — The Shard').isVisible()).toBe(true);

    // Should show stats
    expect(await page.locator('text=Oxigraph').isVisible()).toBe(true);
    expect(await page.locator('text=Event Log').isVisible()).toBe(true);
  });

  test('should display Event Timeline', async ({ page }) => {
    await page.goto('/');
    await page.waitForTimeout(2000);

    const explorerTab = page.locator('button:has-text("4D Explorer")');
    await explorerTab.click();

    await page.waitForTimeout(1000);

    // Should show event timeline
    expect(await page.locator('text=Event Timeline').isVisible()).toBe(true);

    // Should have events
    const events = page.locator('[class*="flex items-start gap-3 p-3"]');
    expect(await events.count()).toBeGreaterThan(0);
  });

  test('should display Connection Status', async ({ page }) => {
    await page.goto('/');
    await page.waitForTimeout(2000);

    // Connection status should be visible in header
    const connectionStatus = page.locator('[class*="flex items-center gap-3"]');
    await expect(connectionStatus).toBeVisible();
  });

  test('should show/hide edit buttons on hover', async ({ page }) => {
    await page.goto('/');
    await page.waitForTimeout(2000);

    const entitiesTab = page.locator('button:has-text("Entities")');
    await entitiesTab.click();

    await page.waitForTimeout(1000);

    // Select a project
    const projectItem = page.locator('[class*="flex items-center justify-between p-3"]').first();
    await projectItem.click();

    await page.waitForTimeout(500);

    // Find a property row
    const propertyRow = page.locator('[class*="flex items-center gap-3 p-3"]').first();

    // Initially, edit button might be hidden (depends on implementation)
    // On hover, it should appear
    await propertyRow.hover();
    await page.waitForTimeout(300);

    const editBtn = propertyRow.locator('button');
    await expect(editBtn.first()).toBeVisible();
  });

  test('should show hero section with key metrics', async ({ page }) => {
    await page.goto('/');
    await page.waitForTimeout(2000);

    // Hero section metrics
    expect(await page.locator('text=Check-Out/In').isVisible()).toBe(true);
    expect(await page.locator('text=Authoritative').isVisible()).toBe(true);
    expect(await page.locator('text=Thin View').isVisible()).toBe(true);
    expect(await page.locator('text=Real-Time SSE').isVisible()).toBe(true);
  });

  test('should display responsive design on different screen sizes', async ({ page, browserName }) => {
    await page.goto('/');
    await page.waitForTimeout(2000);

    // Test tablet size
    await page.setViewportSize({ width: 768, height: 1024 });
    await page.waitForTimeout(500);

    // Should still be navigable
    const tabs = page.locator('button:has-text("4D Explorer")');
    await expect(tabs).toBeVisible();

    // Test mobile size
    await page.setViewportSize({ width: 375, height: 667 });
    await page.waitForTimeout(500);

    // Should still work (might be stacked differently)
    await expect(tabs).toBeVisible();

    // Reset to desktop
    await page.setViewportSize({ width: 1280, height: 720 });
  });

  test('should handle rapid tab switching', async ({ page }) => {
    await page.goto('/');
    await page.waitForTimeout(2000);

    const tabs = ['4D Explorer', 'Entities', 'Shard Data'];

    // Rapidly click tabs
    for (let i = 0; i < 3; i++) {
      for (const tabName of tabs) {
        const tab = page.locator(`button:has-text("${tabName}")`);
        await tab.click();
      }
    }

    await page.waitForTimeout(1000);

    // Should not crash
    const body = page.locator('body');
    await expect(body).toBeVisible();
  });

  test('should display footer with branding', async ({ page }) => {
    await page.goto('/');

    // Scroll to bottom
    await page.evaluate(() => window.scrollTo(0, document.body.scrollHeight));
    await page.waitForTimeout(500);

    // Should have footer with info
    const footer = page.locator('footer');
    await expect(footer).toBeVisible();

    expect(await page.locator('text=Shard-Based Architecture').isVisible()).toBe(true);
  });
});
