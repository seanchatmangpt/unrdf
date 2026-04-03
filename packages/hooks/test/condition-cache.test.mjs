/**
 * @vitest-environment node
 * @file Condition Cache Tests - Comprehensive coverage for condition-cache.mjs
 */
import { describe, it, expect, beforeEach, vi } from 'vitest';
import { ConditionCache } from '../src/hooks/condition-cache.mjs';

describe('ConditionCache - Construction', () => {
  it('should create cache with default TTL', () => {
    const cache = new ConditionCache();
    const stats = cache.stats();

    expect(stats.ttl).toBe(60000); // 60 seconds
    expect(stats.size).toBe(0);
  });

  it('should create cache with custom TTL', () => {
    const cache = new ConditionCache({ ttl: 30000 });
    const stats = cache.stats();

    expect(stats.ttl).toBe(30000);
  });
});

describe('ConditionCache - Get/Set Operations', () => {
  let cache;

  beforeEach(() => {
    cache = new ConditionCache({ ttl: 60000 });
  });

  it('should store and retrieve values', () => {
    cache.set('hook1', 'v1', true);

    const result = cache.get('hook1', 'v1');
    expect(result).toBe(true);
  });

  it('should return undefined for non-existent keys', () => {
    const result = cache.get('nonexistent', 'v1');
    expect(result).toBeUndefined();
  });

  it('should coerce values to boolean', () => {
    cache.set('hook1', 'v1', 'truthy');
    expect(cache.get('hook1', 'v1')).toBe(true);

    cache.set('hook2', 'v1', 0);
    expect(cache.get('hook2', 'v1')).toBe(false);

    cache.set('hook3', 'v1', null);
    expect(cache.get('hook3', 'v1')).toBe(false);
  });

  it('should handle multiple hook/version combinations', () => {
    cache.set('hook1', 'v1', true);
    cache.set('hook1', 'v2', false);
    cache.set('hook2', 'v1', true);

    expect(cache.get('hook1', 'v1')).toBe(true);
    expect(cache.get('hook1', 'v2')).toBe(false);
    expect(cache.get('hook2', 'v1')).toBe(true);
  });

  it('should ignore invalid hookId', () => {
    cache.set('', 'v1', true);
    cache.set(null, 'v1', true);
    cache.set(undefined, 'v1', true);

    expect(cache.stats().size).toBe(0);
  });

  it('should ignore invalid storeVersion', () => {
    cache.set('hook1', '', true);
    cache.set('hook1', null, true);
    cache.set('hook1', undefined, true);

    expect(cache.stats().size).toBe(0);
  });

  it('should return undefined for invalid parameters in get', () => {
    cache.set('hook1', 'v1', true);

    expect(cache.get('', 'v1')).toBeUndefined();
    expect(cache.get('hook1', '')).toBeUndefined();
    expect(cache.get(null, 'v1')).toBeUndefined();
    expect(cache.get('hook1', null)).toBeUndefined();
  });
});

describe('ConditionCache - TTL Expiration', () => {
  it('should expire entries after TTL', () => {
    vi.useFakeTimers();

    const cache = new ConditionCache({ ttl: 1000 }); // 1 second TTL
    cache.set('hook1', 'v1', true);

    // Immediately retrievable
    expect(cache.get('hook1', 'v1')).toBe(true);

    // Advance time by 500ms - still valid
    vi.advanceTimersByTime(500);
    expect(cache.get('hook1', 'v1')).toBe(true);

    // Advance time by another 600ms - expired (total 1100ms)
    vi.advanceTimersByTime(600);
    expect(cache.get('hook1', 'v1')).toBeUndefined();

    vi.useRealTimers();
  });

  it('should remove expired entries from cache', () => {
    vi.useFakeTimers();

    const cache = new ConditionCache({ ttl: 1000 });
    cache.set('hook1', 'v1', true);

    expect(cache.stats().size).toBe(1);

    // Expire entry
    vi.advanceTimersByTime(1100);
    cache.get('hook1', 'v1'); // Triggers cleanup

    expect(cache.stats().size).toBe(0);

    vi.useRealTimers();
  });

  it('should handle multiple entries with different ages', () => {
    vi.useFakeTimers();

    const cache = new ConditionCache({ ttl: 1000 });

    cache.set('hook1', 'v1', true);
    vi.advanceTimersByTime(500);
    cache.set('hook2', 'v1', false);

    // After 600ms more (1100ms total):
    // - hook1 expired (1100ms old)
    // - hook2 still valid (600ms old)
    vi.advanceTimersByTime(600);

    expect(cache.get('hook1', 'v1')).toBeUndefined();
    expect(cache.get('hook2', 'v1')).toBe(false);

    vi.useRealTimers();
  });
});

describe('ConditionCache - Clear', () => {
  let cache;

  beforeEach(() => {
    cache = new ConditionCache();
  });

  it('should clear all entries', () => {
    cache.set('hook1', 'v1', true);
    cache.set('hook2', 'v2', false);
    cache.set('hook3', 'v3', true);

    expect(cache.stats().size).toBe(3);

    cache.clear();

    expect(cache.stats().size).toBe(0);
  });

  it('should allow new entries after clear', () => {
    cache.set('hook1', 'v1', true);
    cache.clear();

    cache.set('hook2', 'v2', false);
    expect(cache.get('hook2', 'v2')).toBe(false);
  });
});

describe('ConditionCache - Statistics', () => {
  let cache;

  beforeEach(() => {
    cache = new ConditionCache({ ttl: 45000 });
  });

  it('should return accurate statistics', () => {
    cache.set('hook1', 'v1', true);
    cache.set('hook2', 'v2', false);
    cache.set('hook3', 'v3', true);

    const stats = cache.stats();

    expect(stats.size).toBe(3);
    expect(stats.ttl).toBe(45000);
    expect(stats.entries).toHaveLength(3);
  });

  it('should include all cache keys in entries', () => {
    cache.set('hook1', 'v1', true);
    cache.set('hook1', 'v2', false);
    cache.set('hook2', 'v1', true);

    const stats = cache.stats();

    expect(stats.entries).toContain('hook1-v1');
    expect(stats.entries).toContain('hook1-v2');
    expect(stats.entries).toContain('hook2-v1');
  });

  it('should reflect cache state changes', () => {
    expect(cache.stats().size).toBe(0);

    cache.set('hook1', 'v1', true);
    expect(cache.stats().size).toBe(1);

    cache.set('hook2', 'v2', false);
    expect(cache.stats().size).toBe(2);

    cache.clear();
    expect(cache.stats().size).toBe(0);
  });
});

describe('ConditionCache - Edge Cases', () => {
  let cache;

  beforeEach(() => {
    cache = new ConditionCache();
  });

  it('should handle rapid successive sets to same key', () => {
    cache.set('hook1', 'v1', true);
    cache.set('hook1', 'v1', false); // overwrite
    cache.set('hook1', 'v1', true); // overwrite again

    expect(cache.get('hook1', 'v1')).toBe(true);
    expect(cache.stats().size).toBe(1); // Only one entry
  });

  it('should handle special characters in hookId/version', () => {
    cache.set('hook:with:colons', 'v1.2.3', true);
    cache.set('hook-with-dashes', 'v2', false);
    cache.set('hook/with/slashes', 'v3', true);

    expect(cache.get('hook:with:colons', 'v1.2.3')).toBe(true);
    expect(cache.get('hook-with-dashes', 'v2')).toBe(false);
    expect(cache.get('hook/with/slashes', 'v3')).toBe(true);
  });

  it('should handle very long hookId/version strings', () => {
    const longId = 'a'.repeat(1000);
    const longVersion = 'b'.repeat(1000);

    cache.set(longId, longVersion, true);
    expect(cache.get(longId, longVersion)).toBe(true);
  });

  it('should handle unicode characters', () => {
    cache.set('hook-émoji-🎯', 'version-中文', true);
    expect(cache.get('hook-émoji-🎯', 'version-中文')).toBe(true);
  });
});

describe('ConditionCache - Performance', () => {
  it('should handle large number of entries', () => {
    const cache = new ConditionCache();

    // Add 10,000 entries
    for (let i = 0; i < 10000; i++) {
      cache.set(`hook${i}`, `v${i % 100}`, i % 2 === 0);
    }

    expect(cache.stats().size).toBe(10000);

    // Verify random access
    expect(cache.get('hook5000', 'v0')).toBe(true);
    expect(cache.get('hook5001', 'v1')).toBe(false);
  });

  it('should clear large cache efficiently', () => {
    const cache = new ConditionCache();

    for (let i = 0; i < 10000; i++) {
      cache.set(`hook${i}`, 'v1', true);
    }

    cache.clear();
    expect(cache.stats().size).toBe(0);
  });
});
