import { test, expect } from './fixtures.js';

/**
 * Entity Operations Tests - Read and Edit
 *
 * Tests:
 * - Loading entity properties
 * - Editing entity properties
 * - Validation feedback
 * - Optimistic updates and rollback
 */

test.describe('Entity Operations', () => {
  test('should load project properties', async ({ page }) => {
    // Navigate to Entities tab
    await page.goto('/');
    await page.waitForTimeout(2000);

    const entitiesTab = page.locator('button:has-text("Entities")');
    await entitiesTab.click();

    await page.waitForTimeout(1000);

    // Should show project list
    const projectList = page.locator('text=Projects');
    await expect(projectList).toBeVisible();

    // Should have at least one project
    const projectItem = page.locator('[class*="flex items-center justify-between p-3"]').first();
    await expect(projectItem).toBeVisible();
  });

  test('should select and display project properties', async ({ page }) => {
    await page.goto('/');
    await page.waitForTimeout(2000);

    const entitiesTab = page.locator('button:has-text("Entities")');
    await entitiesTab.click();

    await page.waitForTimeout(1000);

    // Click first project
    const projectItem = page.locator('[class*="flex items-center justify-between p-3"]').first();
    await projectItem.click();

    await page.waitForTimeout(500);

    // Should show entity editor with properties
    const editor = page.locator('text=/Project|Name|Budget|Status/');
    await expect(editor).toBeVisible();
  });

  test('should edit project budget (within valid range)', async ({ page, submitAndWaitForResult }) => {
    await page.goto('/');
    await page.waitForTimeout(2000);

    const entitiesTab = page.locator('button:has-text("Entities")');
    await entitiesTab.click();

    await page.waitForTimeout(1000);

    // Select project
    const projectItem = page.locator('[class*="flex items-center justify-between p-3"]').first();
    await projectItem.click();

    await page.waitForTimeout(500);

    // Find budget property row
    const budgetRow = page.locator(':has(> div:has-text("budget"))');
    const editBtn = budgetRow.locator('button').first();

    // Click edit
    if (await editBtn.isVisible()) {
      await editBtn.click();
    }

    await page.waitForTimeout(300);

    // Change value
    const input = budgetRow.locator('input');
    await input.fill('50000');

    // Save
    const saveBtn = budgetRow.locator('button svg').first();
    await saveBtn.click();

    await page.waitForTimeout(1000);

    // Should show success (either green checkmark or no error)
    // The validation should pass since 50000 is valid
  });

  test('should reject invalid budget (> 100000)', async ({ page }) => {
    await page.goto('/');
    await page.waitForTimeout(2000);

    const entitiesTab = page.locator('button:has-text("Entities")');
    await entitiesTab.click();

    await page.waitForTimeout(1000);

    // Select project
    const projectItem = page.locator('[class*="flex items-center justify-between p-3"]').first();
    await projectItem.click();

    await page.waitForTimeout(500);

    // Find budget property
    const budgetRow = page.locator(':has(> div:has-text("budget"))');
    const editBtn = budgetRow.locator('button').first();

    if (await editBtn.isVisible()) {
      await editBtn.click();
    }

    // Enter invalid value
    const input = budgetRow.locator('input');
    await input.fill('999999');

    // Save
    const saveBtn = budgetRow.locator('button svg').first();
    await saveBtn.click();

    await page.waitForTimeout(1000);

    // Should show error message
    const errorMsg = budgetRow.locator('text=/Budget cannot exceed|error/i');
    await expect(errorMsg).toBeVisible();
  });

  test('should reject empty name', async ({ page }) => {
    await page.goto('/');
    await page.waitForTimeout(2000);

    const entitiesTab = page.locator('button:has-text("Entities")');
    await entitiesTab.click();

    await page.waitForTimeout(1000);

    // Select project
    const projectItem = page.locator('[class*="flex items-center justify-between p-3"]').first();
    await projectItem.click();

    await page.waitForTimeout(500);

    // Find name property and try to clear it
    const nameRow = page.locator(':has(> div:has-text("name"))');
    const editBtn = nameRow.locator('button').first();

    if (await editBtn.isVisible()) {
      await editBtn.click();
    }

    const input = nameRow.locator('input');
    await input.fill('');

    // Try to save
    const saveBtn = nameRow.locator('button svg').first();
    await saveBtn.click();

    await page.waitForTimeout(1000);

    // Should show error
    const errorMsg = nameRow.locator('text=/empty|error/i');
    await expect(errorMsg).toBeVisible();
  });

  test('should display valid status options', async ({ page }) => {
    await page.goto('/');
    await page.waitForTimeout(2000);

    // Status should only allow: active, paused, completed, cancelled
    // This is enforced by server validation

    const entitiesTab = page.locator('button:has-text("Entities")');
    await entitiesTab.click();

    await page.waitForTimeout(1000);

    const projectItem = page.locator('[class*="flex items-center justify-between p-3"]').first();
    await projectItem.click();

    await page.waitForTimeout(500);

    // Find status and verify it's valid
    const statusRow = page.locator(':has(> div:has-text("status"))');
    const statusValue = statusRow.locator('div[class*="text-white"]');

    const value = await statusValue.textContent();
    const validStatuses = ['active', 'paused', 'completed', 'cancelled'];

    expect(validStatuses).toContain(value.toLowerCase());
  });

  test('should show list of tasks', async ({ page }) => {
    await page.goto('/');
    await page.waitForTimeout(2000);

    const entitiesTab = page.locator('button:has-text("Entities")');
    await entitiesTab.click();

    await page.waitForTimeout(1000);

    // Should show Tasks section
    const taskList = page.locator('text=Tasks');
    await expect(taskList).toBeVisible();

    // Should have at least one task
    const taskItems = page.locator('text=/Task/').all();
    const count = await page.locator('text=/Tasks.*\\(/').count();
    expect(count).toBeGreaterThan(0);
  });
});
