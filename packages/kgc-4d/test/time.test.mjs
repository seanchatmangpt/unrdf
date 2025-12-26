/**
 * KGC Time Module Tests - Chicago School TDD
 * Tests nanosecond-precision timestamps with monotonic ordering
 * Validates Big Bang 80/20 principle: tests DRIVE implementation
 */

import { describe, it, expect, beforeEach, afterEach } from 'vitest';
import { now, toISO, fromISO, addNanoseconds, duration } from '../src/time.mjs';

describe('KGC Time Module - HDIT Theory Applied', () => {
  describe('now() - Nanosecond Clock', () => {
    it('should return BigInt nanosecond timestamp', () => {
      const timestamp = now();
      expect(typeof timestamp).toBe('bigint');
      expect(timestamp > 0n).toBe(true);
    });

    it('should enforce monotonic ordering (never go backwards)', () => {
      const t1 = now();
      const t2 = now();
      const t3 = now();
      expect(t1 < t2).toBe(true);
      expect(t2 < t3).toBe(true);
    });

    it('should increment by at least 1ns on rapid calls', () => {
      const t1 = now();
      const t2 = now();
      expect(t2 - t1).toBeGreaterThanOrEqual(1n);
    });

    it('should handle 100 rapid sequential calls with strict monotonicity', () => {
      const timestamps = [];
      for (let i = 0; i < 100; i++) {
        timestamps.push(now());
      }
      for (let i = 1; i < timestamps.length; i++) {
        expect(timestamps[i] > timestamps[i - 1]).toBe(true);
      }
    });
  });

  describe('toISO() - Nanosecond to ISO Conversion', () => {
    it('should convert BigInt nanoseconds to ISO 8601 string', () => {
      const t_ns = BigInt(1_700_000_000_000_000_000); // 2023-11-15 00:00:00Z
      const iso = toISO(t_ns);
      expect(iso).toMatch(/^\d{4}-\d{2}-\d{2}T\d{2}:\d{2}:\d{2}\.\d{3}Z$/);
    });

    it('should preserve millisecond precision (nanoseconds truncated)', () => {
      const t_ns = BigInt(1_700_000_000_000_000_000);
      const iso = toISO(t_ns);
      const reconstructed = new Date(iso).getTime();
      expect(reconstructed).toBe(1700000000000);
    });

    it('should throw TypeError for non-BigInt input', () => {
      expect(() => toISO('not a bigint')).toThrow(TypeError);
      expect(() => toISO(123)).toThrow(TypeError);
      expect(() => toISO(null)).toThrow(TypeError);
    });

    it('should handle epoch time correctly', () => {
      const epoch_ns = 0n;
      const iso = toISO(epoch_ns);
      expect(iso).toBe('1970-01-01T00:00:00.000Z');
    });
  });

  describe('fromISO() - ISO to Nanosecond Conversion', () => {
    it('should convert ISO 8601 string to BigInt nanoseconds', () => {
      const iso = '2023-11-15T10:30:45.123Z';
      const t_ns = fromISO(iso);
      expect(typeof t_ns).toBe('bigint');
      expect(t_ns > 0n).toBe(true);
    });

    it('should roundtrip correctly: ISO -> ns -> ISO', () => {
      const originalISO = '2023-11-15T10:30:45.123Z';
      const t_ns = fromISO(originalISO);
      const reconstructedISO = toISO(t_ns);
      expect(reconstructedISO).toBe(originalISO);
    });

    it('should throw error for invalid ISO dates', () => {
      expect(() => fromISO('not-a-date')).toThrow();
      expect(() => fromISO('2023-13-45T00:00:00.000Z')).toThrow();
    });

    it('should throw TypeError for non-string input', () => {
      expect(() => fromISO(123)).toThrow(TypeError);
      expect(() => fromISO(null)).toThrow(TypeError);
    });

    it('should handle epoch correctly', () => {
      const epoch = '1970-01-01T00:00:00.000Z';
      const t_ns = fromISO(epoch);
      expect(t_ns).toBe(0n);
    });
  });

  describe('addNanoseconds() - Time Arithmetic', () => {
    it('should add nanoseconds to timestamp', () => {
      const t_ns = BigInt(1_000_000_000_000);
      const delta = BigInt(500_000);
      const result = addNanoseconds(t_ns, delta);
      expect(result).toBe(BigInt(1_000_000_500_000));
    });

    it('should throw TypeError for non-BigInt delta (GAP-T5 strict type enforcement)', () => {
      const t_ns = BigInt(1_000_000_000_000);
      // GAP-T5 fix: Enforce strict BigInt type for delta to prevent unit confusion
      expect(() => addNanoseconds(t_ns, 1000)).toThrow(TypeError);
      expect(() => addNanoseconds(t_ns, '1000')).toThrow(TypeError);
    });

    it('should throw TypeError for non-BigInt timestamp', () => {
      expect(() => addNanoseconds(123, 1000n)).toThrow(TypeError);
      expect(() => addNanoseconds('time', 1000n)).toThrow(TypeError);
    });

    it('should handle large additions without overflow', () => {
      const t_ns = BigInt(1_000_000_000_000);
      const delta = BigInt(Number.MAX_SAFE_INTEGER) * 1000n;
      const result = addNanoseconds(t_ns, delta);
      expect(result).toBe(t_ns + delta);
    });
  });

  describe('duration() - Time Difference Calculation', () => {
    it('should calculate duration between two timestamps', () => {
      const t1 = BigInt(1_000_000_000_000);
      const t2 = BigInt(1_000_000_005_000);
      const dur = duration(t1, t2);
      expect(dur).toBe(BigInt(5_000));
    });

    it('should return negative duration for backwards times', () => {
      const t1 = BigInt(1_000_000_000_000);
      const t2 = BigInt(999_999_000_000);
      const dur = duration(t1, t2);
      expect(dur).toBe(BigInt(-1_000_000));
    });

    it('should return 0 for identical timestamps', () => {
      const t = BigInt(1_000_000_000_000);
      const dur = duration(t, t);
      expect(dur).toBe(0n);
    });

    it('should throw TypeError for non-BigInt inputs', () => {
      const t1 = BigInt(1_000_000_000_000);
      expect(() => duration(t1, 1000)).toThrow(TypeError);
      expect(() => duration(123, t1)).toThrow(TypeError);
      expect(() => duration('time1', 'time2')).toThrow(TypeError);
    });

    it('should handle large durations without overflow', () => {
      const t1 = 0n;
      const t2 = BigInt(Number.MAX_SAFE_INTEGER) * 1000n;
      const dur = duration(t1, t2);
      expect(dur).toBe(t2);
    });
  });

  describe('Time Ordering Invariants (HDIT Concentration)', () => {
    /**
     * From thesis: Concentration of Measure
     * "The probability that the time ordering is violated approaches 0 exponentially"
     * P(ordering_violation) ≤ 2^(-D) for dimension D
     */
    it('should maintain strict total ordering across 1000 calls', () => {
      const times = Array.from({ length: 1000 }, () => now());
      for (let i = 1; i < times.length; i++) {
        expect(times[i]).toBeGreaterThan(times[i - 1]);
      }
    });

    it('should never produce duplicate timestamps', () => {
      const times = Array.from({ length: 100 }, () => now());
      const unique = new Set(times.map(t => t.toString()));
      expect(unique.size).toBe(100);
    });

    /**
     * Test: Pareto Frontier of Time Operations
     * 80/20 insight: conversion operations (toISO/fromISO) are 20% of time
     * but critical for 80% of use cases
     */
    it('should complete 1000 time operations in reasonable bounds', () => {
      const start = performance.now();
      for (let i = 0; i < 1000; i++) {
        const t = now();
        const iso = toISO(t);
        fromISO(iso);
      }
      const elapsed = performance.now() - start;
      // Should complete in <100ms (20+ ops/ms)
      expect(elapsed).toBeLessThan(100);
    });
  });

  describe('Edge Cases and Boundaries', () => {
    it('should handle minimum safe integer', () => {
      const minSafe = BigInt(Number.MIN_SAFE_INTEGER);
      const iso = toISO(minSafe);
      expect(iso).toBeDefined();
    });

    it('should preserve nanosecond granularity through conversions', () => {
      // Nanoseconds are truncated to milliseconds in JS Date
      const t_ns_1 = BigInt(1_700_000_000_123_456_789);
      const t_ns_2 = BigInt(1_700_000_000_123_456_790); // 1ns difference
      const iso_1 = toISO(t_ns_1);
      const iso_2 = toISO(t_ns_2);
      // Both should round to same millisecond
      expect(iso_1).toBe(iso_2);
    });

    it('should handle leap seconds gracefully', () => {
      // ISO 8601 doesn't officially support leap seconds, but handle attempt
      const iso = '2023-06-30T23:59:60Z';
      expect(() => fromISO(iso)).toThrow();
    });
  });
});
