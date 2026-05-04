/**
 * @file Epistemic Truth Check Tests
 * @module cli/test/commands/doctor/truth
 * @description Tests for epistemic truth health checks (anti-lie layer)
 */

import { describe, it, expect } from 'vitest';
import { checkTruth } from '../../../src/cli/commands/doctor/checks/truth.mjs';

describe('Epistemic Truth Health Checks', () => {
  it('should include "Version Consistency" and "Build Artifact Truth" checks', async () => {
    const result = await checkTruth();
    expect(result.category).toBe('Epistemic Truth');
    
    const checkNames = result.checks.map(c => c.name);
    expect(checkNames).toContain('Version Consistency');
    expect(checkNames).toContain('Build Artifact Truth');
  });

  it('checks should return valid status objects', async () => {
    const result = await checkTruth();
    for (const check of result.checks) {
      expect(['pass', 'warn', 'fail', 'error']).toContain(check.status);
      expect(check.actual).toBeDefined();
      expect(check.expected).toBeDefined();
    }
  });
});
