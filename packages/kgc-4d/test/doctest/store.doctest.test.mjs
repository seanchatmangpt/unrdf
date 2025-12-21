import { describe, test, expect } from 'vitest';

import { KGCStore } from '../../src/store.mjs'

describe('Doctests: store.mjs', () => {
  test('MAX_PAYLOAD_SIZE_BYT example 1 (line 1)', async () => {
    const store = new KGCStore();
console.assert(store.vectorClock.nodeId.startsWith('node-'), 'Generated node ID has node- prefix');
  });

  test('eventId example 2 (line 57)', async () => {
    const store = new KGCStore({ nodeId: 'test-node' });
const receipt = await store.appendEvent({ type: 'CREATE', payload: { label: 'test' } });
console.assert(receipt.receipt.id, 'Event has ID');
console.assert(receipt.receipt.event_count === 1, 'Event count incremented');
  });
});
