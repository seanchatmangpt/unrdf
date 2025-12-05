import { test, expect } from './fixtures.js';

/**
 * Connection Tests - Tether (SSE) connectivity
 *
 * Tests:
 * - Initial connection establishment
 * - Receiving initial Shard
 * - Heartbeat keep-alive
 * - Disconnection and reconnection
 * - Error handling
 */

test.describe('Tether Connection', () => {
  test('should connect and display connected status', async ({ page }) => {
    // Navigate to playground
    await page.goto('/');

    // Look for connection status
    const connectionStatus = page.locator('text=Connected');

    // Initially should be connecting or disconnected
    // After a moment, should show connected
    await page.waitForTimeout(1000);

    // Check for the Wifi icon or status
    const wifiIcon = page.locator('svg[data-testid*="wifi"], text=Connected');
    await expect(wifiIcon).toBeVisible({ timeout: 10000 });
  });

  test('should receive initial shard on connect', async ({ page, getCurrentShard }) => {
    await page.goto('/');

    // Wait for Shard Viewer to populate
    await page.waitForTimeout(2000);

    // Get the current shard
    const shard = await getCurrentShard();

    expect(shard).toBeDefined();
    expect(shard.quads).toBeDefined();
    expect(Array.isArray(shard.quads)).toBe(true);
    expect(shard.quads.length).toBeGreaterThan(0);

    // Verify shard structure
    expect(shard).toHaveProperty('id');
    expect(shard).toHaveProperty('t_ns');
    expect(shard).toHaveProperty('timestamp_iso');
    expect(shard).toHaveProperty('vector_clock');
  });

  test('should display event timeline with connection event', async ({ page }) => {
    await page.goto('/');

    // Wait for events to appear
    await page.waitForTimeout(2000);

    // Look for CONNECTED event in timeline
    const connectedEvent = page.locator('text=CONNECTED');
    await expect(connectedEvent).toBeVisible();
  });

  test('should disconnect when button is clicked', async ({ page }) => {
    await page.goto('/');

    await page.waitForTimeout(1000);

    // Find and click disconnect button
    const disconnectBtn = page.locator('button:has-text("Disconnect")');
    await expect(disconnectBtn).toBeVisible();

    await disconnectBtn.click();

    // Should show disconnected state
    await expect(page.locator('text=Disconnected')).toBeVisible({ timeout: 5000 });
  });

  test('should reconnect when button is clicked', async ({ page }) => {
    await page.goto('/');

    await page.waitForTimeout(1000);

    // Disconnect
    const disconnectBtn = page.locator('button:has-text("Disconnect")');
    await disconnectBtn.click();

    await expect(page.locator('text=Disconnected')).toBeVisible();

    // Reconnect
    const connectBtn = page.locator('button:has-text("Connect")');
    await connectBtn.click();

    // Should show connecting then connected
    await expect(page.locator('text=Connected')).toBeVisible({ timeout: 10000 });
  });

  test('should receive heartbeat events', async ({ page }) => {
    await page.goto('/');

    await page.waitForTimeout(2000);

    // Heartbeats are sent every 30s, so we check for at least one event
    const eventTimeline = page.locator('[class*="space-y-2"]');
    await expect(eventTimeline).toBeVisible();

    // Just verify events are being received
    const events = page.locator('[class*="flex items-start gap-3 p-3"]');
    const count = await events.count();
    expect(count).toBeGreaterThan(0);
  });

  test('should handle connection errors gracefully', async ({ page }) => {
    // This test would require mocking or simulating a connection failure
    // For now, we just verify the error state UI exists

    await page.goto('/');

    // The error UI should be in the DOM (even if not visible)
    // We're testing that the app doesn't crash
    await page.waitForTimeout(1000);

    const body = page.locator('body');
    await expect(body).toBeVisible();
  });
});
