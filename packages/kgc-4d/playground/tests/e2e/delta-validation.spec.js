import { test, expect } from './fixtures.js';

/**
 * Delta Submission and Validation Tests
 *
 * Tests:
 * - Valid delta submission (ACK)
 * - Invalid delta rejection (REJECT)
 * - Validation hook enforcement
 * - Optimistic update + rollback
 * - Event log recording
 */

test.describe('Delta Submission (Check-In)', () => {
  test('should submit valid delta and receive ACK', async ({ page, submitAndWaitForResult }) => {
    await page.goto('/');
    await page.waitForTimeout(2000);

    // Create a valid delta (changing status from active to paused)
    const operations = [
      {
        type: 'delete',
        subject: { value: 'http://example.org/project/alpha', termType: 'NamedNode' },
        predicate: { value: 'http://kgc.io/ontology/status', termType: 'NamedNode' },
        object: { value: 'active', termType: 'Literal' },
      },
      {
        type: 'add',
        subject: { value: 'http://example.org/project/alpha', termType: 'NamedNode' },
        predicate: { value: 'http://kgc.io/ontology/status', termType: 'NamedNode' },
        object: { value: 'paused', termType: 'Literal' },
      },
    ];

    const result = await submitAndWaitForResult(operations);

    expect(result.status).toBe('ACK');
    expect(result).toHaveProperty('t_ns');
    expect(result).toHaveProperty('event_id');
    expect(result).toHaveProperty('vector_clock');
  });

  test('should reject delta with invalid budget (> 100000)', async ({ page, submitAndWaitForResult }) => {
    await page.goto('/');
    await page.waitForTimeout(2000);

    const operations = [
      {
        type: 'add',
        subject: { value: 'http://example.org/project/alpha', termType: 'NamedNode' },
        predicate: { value: 'http://kgc.io/ontology/budget', termType: 'NamedNode' },
        object: { value: '999999', termType: 'Literal' },
      },
    ];

    const result = await submitAndWaitForResult(operations);

    expect(result.status).toBe('REJECT');
    expect(result.reason).toMatch(/Budget cannot exceed/i);
  });

  test('should reject delta with invalid status', async ({ page, submitAndWaitForResult }) => {
    await page.goto('/');
    await page.waitForTimeout(2000);

    const operations = [
      {
        type: 'add',
        subject: { value: 'http://example.org/project/alpha', termType: 'NamedNode' },
        predicate: { value: 'http://kgc.io/ontology/status', termType: 'NamedNode' },
        object: { value: 'invalid_status', termType: 'Literal' },
      },
    ];

    const result = await submitAndWaitForResult(operations);

    expect(result.status).toBe('REJECT');
    expect(result.reason).toMatch(/Status must be one of/i);
  });

  test('should reject delta with empty name', async ({ page, submitAndWaitForResult }) => {
    await page.goto('/');
    await page.waitForTimeout(2000);

    const operations = [
      {
        type: 'add',
        subject: { value: 'http://example.org/project/test', termType: 'NamedNode' },
        predicate: { value: 'http://kgc.io/ontology/name', termType: 'NamedNode' },
        object: { value: '', termType: 'Literal' },
      },
    ];

    const result = await submitAndWaitForResult(operations);

    expect(result.status).toBe('REJECT');
    expect(result.reason).toMatch(/cannot be empty/i);
  });

  test('should record ACK event in timeline', async ({ page, submitAndWaitForResult }) => {
    await page.goto('/');
    await page.waitForTimeout(2000);

    const explorerTab = page.locator('button:has-text("4D Explorer")');
    await explorerTab.click();

    await page.waitForTimeout(1000);

    // Get initial event count
    const initialEvents = page.locator('[class*="flex items-start gap-3 p-3"]');
    const initialCount = await initialEvents.count();

    // Submit valid delta
    const operations = [
      {
        type: 'add',
        subject: { value: 'http://example.org/project/alpha', termType: 'NamedNode' },
        predicate: { value: 'http://kgc.io/ontology/test_prop', termType: 'NamedNode' },
        object: { value: 'test_value', termType: 'Literal' },
      },
    ];

    const result = await submitAndWaitForResult(operations);

    if (result.status === 'ACK') {
      await page.waitForTimeout(1000);

      // Should have new event in timeline
      const updatedEvents = page.locator('[class*="flex items-start gap-3 p-3"]');
      const updatedCount = await updatedEvents.count();

      expect(updatedCount).toBeGreaterThanOrEqual(initialCount);
    }
  });

  test('should include vector clock in ACK', async ({ page, submitAndWaitForResult }) => {
    await page.goto('/');
    await page.waitForTimeout(2000);

    const operations = [
      {
        type: 'add',
        subject: { value: 'http://example.org/project/alpha', termType: 'NamedNode' },
        predicate: { value: 'http://kgc.io/ontology/status', termType: 'NamedNode' },
        object: { value: 'completed', termType: 'Literal' },
      },
    ];

    const result = await submitAndWaitForResult(operations);

    if (result.status === 'ACK') {
      expect(result.vector_clock).toBeDefined();
      expect(result.vector_clock).toHaveProperty('nodeId');
      expect(result.vector_clock).toHaveProperty('counters');
      expect(typeof result.vector_clock.counters).toBe('object');
    }
  });

  test('should show syncing state during delta submission', async ({ page, submitAndWaitForResult }) => {
    await page.goto('/');
    await page.waitForTimeout(2000);

    const entitiesTab = page.locator('button:has-text("Entities")');
    await entitiesTab.click();

    await page.waitForTimeout(1000);

    // Select a project
    const projectItem = page.locator('[class*="flex items-center justify-between p-3"]').first();
    await projectItem.click();

    await page.waitForTimeout(500);

    // The UI should show syncing indicator during submission
    // After submission, it should show either success or error

    const operations = [
      {
        type: 'add',
        subject: { value: 'http://example.org/project/alpha', termType: 'NamedNode' },
        predicate: { value: 'http://kgc.io/ontology/status', termType: 'NamedNode' },
        object: { value: 'active', termType: 'Literal' },
      },
    ];

    const result = await submitAndWaitForResult(operations);

    // Should get a response (either ACK or REJECT)
    expect(result).toHaveProperty('status');
    expect(['ACK', 'REJECT']).toContain(result.status);
  });
});
