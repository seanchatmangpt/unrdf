import { test, expect } from '@playwright/test';

test.describe('AtomVM OCEL explorer browser surface', () => {
  test.beforeEach(async ({ page }) => {
    await page.goto('/?peer=smoke', { waitUntil: 'domcontentloaded' });
  });

  test('renders the UNRDF peer exploration surface', async ({ page }) => {
    await expect(page).toHaveTitle(/UNRDF AtomVM OCEL v2 Peer Explorer/);
    await expect(page.getByRole('heading', { name: /AtomVM OCEL v2 Peer Explorer/i })).toBeVisible();
    await expect(page.locator('#ocelGraph')).toBeVisible();
    await expect(page.locator('#rawOcel')).toContainText('objectTypes');
    await expect(page.locator('#rawOcel')).toContainText('eventTypes');
  });

  test('boots an observed AtomVM/WASM runtime', async ({ page }) => {
    await expect.poll(async () => page.evaluate(() => window.__atomvmExplorer?.state()?.runtimeStanding), { timeout: 30000 }).toBe('ALIVE');
    const proof = await page.evaluate(() => window.__atomvmExplorer.runtime.observe('smoke-proof'));
    expect(proof.sequence).toBeGreaterThan(0);
    expect(proof.checksum).toBeGreaterThan(0);
    await expect(page.locator('#runtimeStanding')).toHaveAttribute('data-standing', 'ALIVE');
  });

  test('exposes real WebRTC signaling controls without a graph server', async ({ page }) => {
    await expect(page.getByRole('button', { name: 'Create offer' })).toBeVisible();
    await expect(page.getByRole('button', { name: 'Accept offer' })).toBeVisible();
    await expect(page.getByRole('button', { name: 'Accept answer' })).toBeVisible();
    const hasWebRtc = await page.evaluate(() => typeof RTCPeerConnection === 'function');
    expect(hasWebRtc).toBe(true);
  });
});
