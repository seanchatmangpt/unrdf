/**
 * Type Validation Tests
 *
 * Tests Zod schema validation for all substrate types.
 */

import { describe, it, expect } from 'vitest';
import {
  validateStorageSnapshot,
  validateQueryPattern,
  validateTripleEntry,
  validateStateCommitment,
} from '../src/types.mjs';

describe('Type Validation', () => {
  describe('StorageSnapshot', () => {
    it('should validate valid snapshot', () => {
      const snapshot = {
        epoch: 0,
        timestamp_ns: 123456789n,
        quads_hash: 'abc123',
        commit_hash: 'def456',
        snapshot_id: '550e8400-e29b-41d4-a716-446655440000',
      };

      const result = validateStorageSnapshot(snapshot);
      expect(result).toEqual(snapshot);
    });

    it('should validate snapshot with optional quad_count', () => {
      const snapshot = {
        epoch: 0,
        timestamp_ns: 123456789n,
        quads_hash: 'abc123',
        commit_hash: 'def456',
        snapshot_id: '550e8400-e29b-41d4-a716-446655440000',
        quad_count: 42,
      };

      const result = validateStorageSnapshot(snapshot);
      expect(result.quad_count).toBe(42);
    });

    it('should reject invalid epoch (negative)', () => {
      const snapshot = {
        epoch: -1,
        timestamp_ns: 123456789n,
        quads_hash: 'abc123',
        commit_hash: 'def456',
        snapshot_id: '550e8400-e29b-41d4-a716-446655440000',
      };

      expect(() => validateStorageSnapshot(snapshot)).toThrow();
    });

    it('should reject invalid snapshot_id (not UUID)', () => {
      const snapshot = {
        epoch: 0,
        timestamp_ns: 123456789n,
        quads_hash: 'abc123',
        commit_hash: 'def456',
        snapshot_id: 'not-a-uuid',
      };

      expect(() => validateStorageSnapshot(snapshot)).toThrow();
    });

    it('should reject missing required fields', () => {
      const snapshot = {
        epoch: 0,
        timestamp_ns: 123456789n,
      };

      expect(() => validateStorageSnapshot(snapshot)).toThrow();
    });
  });

  describe('QueryPattern', () => {
    it('should validate pattern with all null', () => {
      const pattern = {
        subject: null,
        predicate: null,
        object: null,
        graph: null,
      };

      const result = validateQueryPattern(pattern);
      expect(result).toEqual(pattern);
    });

    it('should validate pattern with mixed null and values', () => {
      const pattern = {
        subject: { value: 'http://example.org/s' },
        predicate: null,
        object: null,
      };

      const result = validateQueryPattern(pattern);
      expect(result.subject).toBeDefined();
      expect(result.predicate).toBeNull();
    });

    it('should validate pattern without graph field', () => {
      const pattern = {
        subject: null,
        predicate: null,
        object: null,
      };

      const result = validateQueryPattern(pattern);
      expect(result).toBeDefined();
    });
  });

  describe('TripleEntry', () => {
    it('should validate valid add entry', () => {
      const entry = {
        index: 0n,
        timestamp_ns: 123456789n,
        operation: 'add',
        subject: { value: 'http://example.org/s' },
        predicate: { value: 'http://example.org/p' },
        object: { value: 'literal' },
      };

      const result = validateTripleEntry(entry);
      expect(result).toEqual(entry);
    });

    it('should validate valid delete entry', () => {
      const entry = {
        index: 1n,
        timestamp_ns: 123456789n,
        operation: 'delete',
        subject: { value: 'http://example.org/s' },
        predicate: { value: 'http://example.org/p' },
        object: { value: 'literal' },
      };

      const result = validateTripleEntry(entry);
      expect(result.operation).toBe('delete');
    });

    it('should validate entry with optional graph', () => {
      const entry = {
        index: 0n,
        timestamp_ns: 123456789n,
        operation: 'add',
        subject: { value: 'http://example.org/s' },
        predicate: { value: 'http://example.org/p' },
        object: { value: 'literal' },
        graph: { value: 'http://example.org/g' },
      };

      const result = validateTripleEntry(entry);
      expect(result.graph).toBeDefined();
    });

    it('should reject invalid operation', () => {
      const entry = {
        index: 0n,
        timestamp_ns: 123456789n,
        operation: 'invalid',
        subject: { value: 'http://example.org/s' },
        predicate: { value: 'http://example.org/p' },
        object: { value: 'literal' },
      };

      expect(() => validateTripleEntry(entry)).toThrow();
    });

    it('should reject negative index', () => {
      const entry = {
        index: -1n,
        timestamp_ns: 123456789n,
        operation: 'add',
        subject: { value: 'http://example.org/s' },
        predicate: { value: 'http://example.org/p' },
        object: { value: 'literal' },
      };

      expect(() => validateTripleEntry(entry)).toThrow();
    });
  });

  describe('StateCommitment', () => {
    it('should validate valid commitment', () => {
      const commitment = {
        state_hash: 'abc123',
        log_index: 42n,
        timestamp_ns: 123456789n,
        quad_count: 100,
      };

      const result = validateStateCommitment(commitment);
      expect(result).toEqual(commitment);
    });

    it('should reject empty state_hash', () => {
      const commitment = {
        state_hash: '',
        log_index: 42n,
        timestamp_ns: 123456789n,
        quad_count: 100,
      };

      expect(() => validateStateCommitment(commitment)).toThrow();
    });

    it('should reject negative quad_count', () => {
      const commitment = {
        state_hash: 'abc123',
        log_index: 42n,
        timestamp_ns: 123456789n,
        quad_count: -1,
      };

      expect(() => validateStateCommitment(commitment)).toThrow();
    });

    it('should reject missing fields', () => {
      const commitment = {
        state_hash: 'abc123',
        log_index: 42n,
      };

      expect(() => validateStateCommitment(commitment)).toThrow();
    });
  });
});
