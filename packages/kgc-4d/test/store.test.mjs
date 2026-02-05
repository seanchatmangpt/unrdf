/**
 * KGC Store Tests - Ultra-fast
 * CRUD smoke tests only
 */

import { describe, it, expect, beforeEach, vi } from 'vitest';
import { KGCStore } from '../src/store.mjs';

describe('KGCStore', () => {
  let store;

  beforeEach(() => {
    store = new KGCStore();
  });

  it('should append event', async () => {
    const result = await store.appendEvent({ type: 'CREATE' }, []);
    expect(result.receipt).toBeDefined();
  });

  it('should retrieve event', async () => {
    await store.appendEvent({ type: 'CREATE' }, []);
    expect(store.getEventCount()).toBe(1);
  });
});
