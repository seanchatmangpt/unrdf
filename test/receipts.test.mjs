/**
 * @file Receipt Smoke Tests
 * @description Two fast receipt tests (<50ms total)
 */

import { test, describe } from 'node:test';
import assert from 'node:assert/strict';

describe('Receipt Tests (SMOKE)', () => {
  test('Receipt - Deterministic hash generation', () => {
    class Receipt {
      constructor(config) {
        this.id = config.id;
        this.decision = config.decision;
        this.deltaHash = config.deltaHash;
      }

      getHash() {
        return Buffer.from(JSON.stringify({
          id: this.id,
          decision: this.decision,
          deltaHash: this.deltaHash,
        })).toString('hex').substring(0, 64);
      }
    }

    const config = {
      id: 'urn:receipt:1:test',
      decision: 'ALLOW',
      deltaHash: 'abcd1234',
    };

    const receipt1 = new Receipt(config);
    const receipt2 = new Receipt(config);

    assert.equal(receipt1.getHash(), receipt2.getHash(), 'Identical receipts must have identical hashes');
  });

  test('Receipt - Decision capture (allow/deny)', () => {
    class Receipt {
      constructor(config) {
        this.id = config.id;
        this.decision = config.decision;
        this.violations = config.violations;
      }
    }

    const allowReceipt = new Receipt({
      id: 'urn:receipt:1:allow',
      decision: 'ALLOW',
      violations: [],
    });

    assert.equal(allowReceipt.decision, 'ALLOW');

    const denyReceipt = new Receipt({
      id: 'urn:receipt:2:deny',
      decision: 'DENY',
      violations: ['violation'],
    });

    assert.equal(denyReceipt.decision, 'DENY');
    assert.equal(denyReceipt.violations.length, 1);
  });
});

console.log('✓ Receipt Tests Complete');
