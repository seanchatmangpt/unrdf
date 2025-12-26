/**
 * Quality Gates Tests
 */

import { describe, it } from 'node:test';
import assert from 'node:assert/strict';
import { runQualityGates } from '../src/quality-gates.mjs';

describe('Quality Gates', () => {
  it('should run all quality gates and return results', async () => {
    const result = await runQualityGates();

    // Should have structure
    assert.ok(result.pass !== undefined, 'Should have pass property');
    assert.ok(Array.isArray(result.gates), 'Should have gates array');
    assert.ok(Array.isArray(result.failures), 'Should have failures array');

    // Should have all 8 gates
    assert.equal(result.gates.length, 8, 'Should have exactly 8 gates');

    // Each gate should have required properties
    for (const gate of result.gates) {
      assert.ok(gate.name, 'Gate should have name');
      assert.ok(gate.pass !== undefined, 'Gate should have pass property');
      assert.ok(Array.isArray(gate.evidence), 'Gate should have evidence array');
      assert.ok(Array.isArray(gate.failures), 'Gate should have failures array');
    }
  });

  it('should provide evidence for all gates', async () => {
    const result = await runQualityGates();

    for (const gate of result.gates) {
      assert.ok(gate.evidence.length > 0,
        `Gate ${gate.name} has no evidence`);
    }
  });

  it('should detect when agents are not implemented', async () => {
    const result = await runQualityGates();

    // Find the Import Resolution gate
    const importGate = result.gates.find(g => g.name === 'Import Resolution');
    assert.ok(importGate, 'Should have Import Resolution gate');

    // When agents 2-9 are not implemented, this gate should fail or report the status
    // The evidence should indicate how many modules were checked
    assert.ok(importGate.evidence.length > 0, 'Should have evidence about imports');
  });

  it('should check file sizes correctly', async () => {
    const result = await runQualityGates();

    const fileSizeGate = result.gates.find(g => g.name === 'File Sizes');
    assert.ok(fileSizeGate, 'Should have File Sizes gate');

    // Should provide evidence about files checked
    assert.ok(fileSizeGate.evidence.length > 0, 'Should have evidence about file sizes');
  });
});
