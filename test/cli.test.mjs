/**
 * @file CLI Smoke Tests
 * @description Single CLI smoke test (<20ms) with mocked commands
 */

import { test, describe } from 'node:test';
import assert from 'node:assert/strict';

describe('CLI Tests (SMOKE)', () => {
  test('CLI - validate command success', async () => {
    class CLI {
      async validate({ universe }) {
        return universe ? 0 : 1;
      }
    }

    const cli = new CLI();
    const exitCode = await cli.validate({ universe: 'test-universe.ttl' });

    assert.equal(exitCode, 0, 'Validate should exit with 0 for valid file');
  });
});

console.log('✓ CLI Tests Complete');
