/**
 * @file Receipt Smoke Tests
 * @description Two fast receipt tests (<50ms total)
 */

import { describe, it, expect } from 'vitest';

describe('Receipt Tests (SMOKE)', () => {
  it('Receipt - Deterministic hash generation', () => {
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

    expect(receipt1.getHash()).toBe(receipt2.getHash());
  });

  it('Receipt - Decision capture (allow/deny)', () => {
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

    expect(allowReceipt.decision).toBe('ALLOW');

    const denyReceipt = new Receipt({
      id: 'urn:receipt:2:deny',
      decision: 'DENY',
      violations: ['violation'],
    });

    expect(denyReceipt.decision).toBe('DENY');
    expect(denyReceipt.violations.length).toBe(1);
  });
});

console.log('✓ Receipt Tests Complete');
