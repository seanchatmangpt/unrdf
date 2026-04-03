/**
 * @file Streaming Admission Tests
 * @description Tests for streaming RDF delta validation with receipt chaining
 */

import { describe, it, expect, beforeEach } from 'vitest';
import { createStore, dataFactory } from '@unrdf/oxigraph';
const { namedNode, literal } = dataFactory;
import { StreamingAdmission, createStreamingAdmission } from '../src/stream-admit.mjs';

/**
 * Helper to create RDF quads from simple notation
 */
function quad(s, p, o) {
  return {
    subject: namedNode(s),
    predicate: namedNode(p),
    object: (o && o.startsWith('http')) ? namedNode(o) : literal(o),
  };
}

describe('Streaming Admission Tests', () => {
  let store;

  beforeEach(() => {
    store = createStore();
  });

  it('[TEST] Basic delta admission without condition', async () => {
    const admin = new StreamingAdmission(store);

    const delta = {
      additions: [
        quad('https://example.org/subject1', 'http://www.w3.org/2000/01/rdf-schema#label', 'Test Label'),
      ],
      removals: [],
    };

    const { receipt, admitted } = await admin.admit(delta);

    expect(admitted).toBe(true);
    expect(receipt).toBeDefined();
    expect(receipt.status).toBe('admitted');
    expect(receipt.receiptHash).toBeDefined();
    expect(receipt.receiptHash.length).toBe(64);
  });

  it('[TEST] Receipt chain integrity - first receipt has null previousReceiptHash', async () => {
    const admin = new StreamingAdmission(store);

    const delta1 = {
      additions: [
        quad('https://example.org/s1', 'http://www.w3.org/2000/01/rdf-schema#label', 'Label1'),
      ],
      removals: [],
    };

    const { receipt: receipt1 } = await admin.admit(delta1);

    expect(receipt1.previousReceiptHash).toBeNull();
    expect(receipt1.receiptHash).toBeDefined();
  });

  it('[TEST] Receipt chain continuity - linked hashes', async () => {
    const admin = new StreamingAdmission(store);

    const delta1 = {
      additions: [
        quad('https://example.org/s1', 'http://www.w3.org/2000/01/rdf-schema#label', 'Label1'),
      ],
      removals: [],
    };

    const delta2 = {
      additions: [
        quad('https://example.org/s2', 'http://www.w3.org/2000/01/rdf-schema#label', 'Label2'),
      ],
      removals: [],
    };

    const { receipt: receipt1 } = await admin.admit(delta1);
    const { receipt: receipt2 } = await admin.admit(delta2);

    expect(receipt2.previousReceiptHash).toBe(receipt1.receiptHash);
  });

  it('[TEST] Determinism - same delta produces same hashes', async () => {
    const admin1 = new StreamingAdmission(createStore());
    const admin2 = new StreamingAdmission(createStore());

    const delta = {
      additions: [
        quad('https://example.org/subject', 'http://www.w3.org/2000/01/rdf-schema#label', 'Test'),
      ],
      removals: [],
    };

    const { receipt: receipt1 } = await admin1.admit(delta);
    const { receipt: receipt2 } = await admin2.admit(delta);

    expect(receipt1.deltaHash).toBe(receipt2.deltaHash);
    expect(receipt1.inputHash).toBe(receipt2.inputHash);
    expect(receipt1.outputHash).toBe(receipt2.outputHash);
  });

  it('[TEST] Hash lengths are correct (64 hex chars for BLAKE3)', async () => {
    const admin = new StreamingAdmission(store);

    const delta = {
      additions: [
        quad('https://example.org/s', 'http://example.org/p', 'o'),
      ],
      removals: [],
    };

    const { receipt } = await admin.admit(delta);

    expect(receipt.inputHash.length).toBe(64);
    expect(receipt.outputHash.length).toBe(64);
    expect(receipt.deltaHash.length).toBe(64);
    expect(receipt.receiptHash.length).toBe(64);
  });

  it('[TEST] Quad count tracking before and after', async () => {
    const admin = new StreamingAdmission(createStore());

    const delta1 = {
      additions: [
        quad('https://example.org/s1', 'http://example.org/p', 'o1'),
        quad('https://example.org/s2', 'http://example.org/p', 'o2'),
      ],
      removals: [],
    };

    const { receipt: receipt1 } = await admin.admit(delta1);

    expect(receipt1.quadCountBefore).toBe(0);
    expect(receipt1.quadCountAfter).toBe(2);

    const delta2 = {
      additions: [
        quad('https://example.org/s3', 'http://example.org/p', 'o3'),
      ],
      removals: [],
    };

    const { receipt: receipt2 } = await admin.admit(delta2);

    expect(receipt2.quadCountBefore).toBe(2);
    expect(receipt2.quadCountAfter).toBe(3);
  });

  it('[TEST] Delta condition - reject when output hash mismatch', async () => {
    const admin = new StreamingAdmission(createStore(), {
      condition: {
        kind: 'delta',
        hash: 'aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa',
      },
    });

    const delta = {
      additions: [
        quad('https://example.org/s', 'http://example.org/p', 'o'),
      ],
      removals: [],
    };

    const { admitted, receipt } = await admin.admit(delta);

    expect(admitted).toBe(false);
    expect(receipt.status).toBe('rejected');
    expect(receipt.rejectionReason).toBeDefined();
  });

  it('[TEST] Rollback on failure - store unchanged when delta rejected', async () => {
    const store = createStore();
    const admin = new StreamingAdmission(store, {
      condition: {
        kind: 'delta',
        hash: 'aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa',
      },
    });

    const initialQuads = Array.from(store.getQuads());

    const delta = {
      additions: [
        quad('https://example.org/s', 'http://example.org/p', 'o'),
      ],
      removals: [],
    };

    await admin.admit(delta);

    const finalQuads = Array.from(store.getQuads());

    expect(initialQuads.length).toBe(finalQuads.length);
  });

  it('[TEST] Admissions and rejections count', async () => {
    const admin = new StreamingAdmission(createStore(), {
      rollbackOnFailure: true,
    });

    // Admit one delta without condition (should pass)
    await admin.admit({
      additions: [
        quad('https://example.org/s1', 'http://example.org/p', 'o1'),
      ],
      removals: [],
    });

    // Reject one delta with bad hash
    const admin2 = new StreamingAdmission(createStore(), {
      condition: {
        kind: 'delta',
        hash: 'badbadbadbadbadbadbadbadbadbadbadbadbadbadbadbadbadbadbadbadbadb',
      },
    });

    await admin2.admit(
      {
        additions: [
          quad('https://example.org/s2', 'http://example.org/p', 'o2'),
        ],
        removals: [],
      }
    );

    const stats = admin.getStats();
    expect(stats.admitted).toBe(1);
  });

  it('[TEST] Verify chain integrity - valid chain', async () => {
    const admin = new StreamingAdmission(createStore());

    const delta1 = {
      additions: [
        quad('https://example.org/s1', 'http://example.org/p', 'o1'),
      ],
      removals: [],
    };

    const delta2 = {
      additions: [
        quad('https://example.org/s2', 'http://example.org/p', 'o2'),
      ],
      removals: [],
    };

    await admin.admit(delta1);
    await admin.admit(delta2);

    const verification = admin.verifyChain();

    expect(verification.valid).toBe(true);
    expect(verification.mismatches.length).toBe(0);
  });

  it('[TEST] Stream admit multiple deltas', async () => {
    const admin = new StreamingAdmission(createStore());

    const deltas = [
      {
        additions: [
          quad('https://example.org/s1', 'http://example.org/p', 'o1'),
        ],
        removals: [],
      },
      {
        additions: [
          quad('https://example.org/s2', 'http://example.org/p', 'o2'),
        ],
        removals: [],
      },
    ];

    const results = await admin.admitStream(deltas);

    expect(results.length).toBe(2);
    expect(results[0].admitted).toBe(true);
    expect(results[1].admitted).toBe(true);
  });

  it('[TEST] Receipt ID is valid UUID', async () => {
    const admin = new StreamingAdmission(createStore());

    const delta = {
      additions: [
        quad('https://example.org/s', 'http://example.org/p', 'o'),
      ],
      removals: [],
    };

    const { receipt } = await admin.admit(delta);

    const uuidRegex = /^[0-9a-f]{8}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{12}$/i;
    expect(uuidRegex.test(receipt.id)).toBe(true);
  });

  it('[TEST] createStreamingAdmission factory function', () => {
    const store = createStore();
    const admin = createStreamingAdmission(store, { nodeId: 'test-node' });

    expect(admin).toBeInstanceOf(StreamingAdmission);
    expect(admin.nodeId).toBe('test-node');
  });

  it('[TEST] Removal and addition in same delta', async () => {
    const store = createStore();

    // Add initial quad
    store.add(quad('https://example.org/s1', 'http://example.org/p', 'old_value'));

    const admin = new StreamingAdmission(store);

    const delta = {
      additions: [
        quad('https://example.org/s1', 'http://example.org/p', 'new_value'),
      ],
      removals: [
        quad('https://example.org/s1', 'http://example.org/p', 'old_value'),
      ],
    };

    const { receipt } = await admin.admit(delta);

    expect(receipt.quadCountBefore).toBe(1);
    expect(receipt.quadCountAfter).toBe(1);
  });

  it('[TEST] Empty additions/removals defaults', async () => {
    const admin = new StreamingAdmission(createStore());

    const delta = {
      // No additions or removals
    };

    const { receipt, admitted } = await admin.admit(delta);

    expect(admitted).toBe(true);
    expect(receipt.status).toBe('admitted');
  });

  it('[TEST] Different deltas produce different deltaHash', async () => {
    const admin1 = new StreamingAdmission(createStore());
    const admin2 = new StreamingAdmission(createStore());

    const delta1 = {
      additions: [
        quad('https://example.org/s1', 'http://example.org/p', 'o1'),
      ],
      removals: [],
    };

    const delta2 = {
      additions: [
        quad('https://example.org/s2', 'http://example.org/p', 'o2'),
      ],
      removals: [],
    };

    const { receipt: receipt1 } = await admin1.admit(delta1);
    const { receipt: receipt2 } = await admin2.admit(delta2);

    expect(receipt1.deltaHash).not.toBe(receipt2.deltaHash);
  });

  it('[TEST] Identical output hashes for identical quads', async () => {
    const admin1 = new StreamingAdmission(createStore());
    const admin2 = new StreamingAdmission(createStore());

    const singleQuad = quad('https://example.org/s', 'http://example.org/p', 'o');

    const delta = {
      additions: [singleQuad],
      removals: [],
    };

    const { receipt: receipt1 } = await admin1.admit(delta);
    const { receipt: receipt2 } = await admin2.admit(delta);

    expect(receipt1.outputHash).toBe(receipt2.outputHash);
  });

  it('[TEST] Chain stats endpoint', async () => {
    const admin = new StreamingAdmission(createStore());

    const delta = {
      additions: [
        quad('https://example.org/s', 'http://example.org/p', 'o'),
      ],
      removals: [],
    };

    await admin.admit(delta);

    const stats = admin.getStats();

    expect(stats.total).toBe(1);
    expect(stats.admitted).toBe(1);
    expect(stats.rejected).toBe(0);
    expect(stats.chainLength).toBe(1);
    expect(stats.currentHeadHash).toBeDefined();
  });

  it('[TEST] Rejection reason preserved in receipt', async () => {
    const admin = new StreamingAdmission(createStore(), {
      condition: {
        kind: 'delta',
        hash: 'badbadbadbadbadbadbadbadbadbadbadbadbadbadbadbadbadbadbadbadbadb',
      },
    });

    const delta = {
      additions: [
        quad('https://example.org/s', 'http://example.org/p', 'o'),
      ],
      removals: [],
    };

    const { receipt } = await admin.admit(delta);

    expect(receipt.status).toBe('rejected');
    expect(receipt.rejectionReason).toBeDefined();
    expect(receipt.rejectionReason).toMatch(/Output hash mismatch/);
  });

  it('[TEST] Timestamp is valid bigint', async () => {
    const admin1 = new StreamingAdmission(createStore(), { deterministic: true });
    const admin2 = new StreamingAdmission(createStore(), { deterministic: true });

    const delta = {
      additions: [
        quad('https://example.org/s', 'http://example.org/p', 'o'),
      ],
      removals: [],
    };

    const { receipt: receipt1 } = await admin1.admit(delta);
    const { receipt: receipt2 } = await admin2.admit(delta);

    expect(typeof receipt1.timestamp).toBe('bigint');
    expect(typeof receipt2.timestamp).toBe('bigint');
    expect(receipt1.timestampIso).toBeDefined();
    expect(receipt2.timestampIso).toBeDefined();
  });
});
