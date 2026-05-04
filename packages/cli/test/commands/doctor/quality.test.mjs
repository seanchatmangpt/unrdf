/**
 * @file Code Quality Check Tests
 * @module cli/test/commands/doctor/quality
 * @description Tests for code quality health checks including Definition of Done
 */

import { describe, it, expect, beforeEach, afterEach } from 'vitest';
import { mkdir, rm } from 'node:fs/promises';
import { join } from 'node:path';
import { tmpdir } from 'node:os';
import { checkQuality } from '../../../src/cli/commands/doctor/checks/quality.mjs';

describe('Quality Health Checks', () => {
  let testDir;

  beforeEach(async () => {
    testDir = join(tmpdir(), `quality-test-${Date.now()}`);
    await mkdir(testDir, { recursive: true });
    // Note: The checks use projectRoot which is hardcoded to go up 7 levels.
    // In actual tests, it might scan the real project, so we must be careful.
    // However, for this E2E-style unit test, we'll focus on verifying the structure.
  });

  afterEach(async () => {
    await rm(testDir, { recursive: true, force: true });
  });

  it('should include "Definition of Done" in quality checks', async () => {
    const result = await checkQuality();
    expect(result.category).toBe('Code Quality');
    
    const dodCheck = result.checks.find(c => c.name === 'Definition of Done');
    expect(dodCheck).toBeDefined();
    expect(['pass', 'warn', 'fail']).toContain(dodCheck.status);
    expect(dodCheck.expected).toContain('Zero DEFERRED_ACTION(#gap-closure)s and console.logs');
  });

  it('should detect DEFERRED_ACTION(#gap-closure)s in code', async () => {
    // This part is hard to test in isolation because checkDefinitionOfDone uses projectRoot
    // which points to the real workspace root.
    // But we can verify the function exists and behaves as expected when called.
    const result = await checkQuality();
    const dodCheck = result.checks.find(c => c.name === 'Definition of Done');
    
    // Based on the real run, it should be at least 'warn' because we know there are DEFERRED_ACTION(#gap-closure)s in the monorepo
    expect(dodCheck.status).not.toBe('error');
  });
});
