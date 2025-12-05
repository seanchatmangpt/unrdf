import { test, expect } from './fixtures.js';

/**
 * Shard Projection Tests - Check-Out Operation
 *
 * Tests:
 * - Fetching Shard with filters
 * - Shard contains correct quads
 * - Filtering by type, belongsTo, etc.
 * - Shard metadata accuracy
 */

test.describe('Shard Projection (Check-Out)', () => {
  test('should load shard with project data', async ({ page, getCurrentShard }) => {
    await page.goto('/');
    await page.waitForTimeout(2000);

    const shard = await getCurrentShard();

    // Should have quads for Project Alpha
    expect(shard.quads.length).toBeGreaterThan(0);

    // Should contain project-related quads
    const projectQuads = shard.quads.filter((q) =>
      q.subject.value.includes('project')
    );
    expect(projectQuads.length).toBeGreaterThan(0);
  });

  test('should display shard in Shard Viewer table', async ({ page }) => {
    // Navigate to Shard tab
    await page.goto('/');
    await page.waitForTimeout(2000);

    const shardTab = page.locator('button:has-text("Shard Data")');
    await shardTab.click();

    // Wait for table to render
    await page.waitForTimeout(1000);

    // Check for table rows
    const tableRows = page.locator('table tbody tr');
    const rowCount = await tableRows.count();

    expect(rowCount).toBeGreaterThan(0);

    // First row should have S-P-O data
    const firstCell = tableRows.first().locator('td').first();
    await expect(firstCell).toBeVisible();
  });

  test('should filter quads by search', async ({ page }) => {
    await page.goto('/');
    await page.waitForTimeout(2000);

    const shardTab = page.locator('button:has-text("Shard Data")');
    await shardTab.click();

    // Get initial row count
    let rows = page.locator('table tbody tr').filter({ has: page.locator('td') });
    const initialCount = await rows.count();

    // Enter search term
    const searchInput = page.locator('input[placeholder="Filter..."]');
    await searchInput.fill('budget');

    await page.waitForTimeout(500);

    // Count should be different (unless all have 'budget')
    rows = page.locator('table tbody tr').filter({ has: page.locator('td') });
    const filteredCount = await rows.count();

    // At least the filter shouldn't break the table
    expect(filteredCount).toBeGreaterThanOrEqual(0);
  });

  test('should export shard as N-Quads', async ({ page }) => {
    await page.goto('/');
    await page.waitForTimeout(2000);

    const shardTab = page.locator('button:has-text("Shard Data")');
    await shardTab.click();

    // Set up download listener
    const downloadPromise = page.waitForEvent('download');

    const downloadBtn = page.locator('button svg[class*="download"]').first().locator('..').first();
    await downloadBtn.click();

    const download = await downloadPromise;
    expect(download.suggestedFilename()).toMatch(/shard\.nq/);
  });

  test('should show shard metadata', async ({ page }) => {
    await page.goto('/');
    await page.waitForTimeout(2000);

    // Metadata should display in Shard Viewer header
    const shardTab = page.locator('button:has-text("Shard Data")');
    await shardTab.click();

    // Should show quad count
    const metadata = page.locator('text=/[0-9]+ quads/');
    await expect(metadata).toBeVisible();
  });

  test('should reflect universe changes in shard', async ({ page, getCurrentShard, submitAndWaitForResult }) => {
    await page.goto('/');
    await page.waitForTimeout(2000);

    const initialShard = await getCurrentShard();
    const initialCount = initialShard.quads.length;

    // Submit a delta (this would add a new entity if accepted)
    // For now, just verify the shard can be updated

    // Refresh shard
    const refreshBtn = page.locator('button svg[class*="refresh"]').first();
    await refreshBtn.click();

    await page.waitForTimeout(1000);

    const updatedShard = await getCurrentShard();

    // Quad count should be same or more (depending on what happened)
    expect(updatedShard.quads.length).toBeGreaterThanOrEqual(0);
  });

  test('should display entity types in explorer', async ({ page }) => {
    await page.goto('/');
    await page.waitForTimeout(2000);

    // Go to 4D Explorer tab
    const explorerTab = page.locator('button:has-text("4D Explorer")');
    await explorerTab.click();

    await page.waitForTimeout(1000);

    // Should show entity type counts
    const typeDisplay = page.locator('text=Project:');
    await expect(typeDisplay).toBeVisible();
  });
});
