/**
 * @file Streaming Sync Protocol Tests (FAST)
 * @description Fast tests for sync protocol operations with mocked data
 * Target: <50ms execution time
 */

import { describe, it, expect, beforeEach } from 'vitest';
import { randomUUID } from 'crypto';
import {
  createSyncMessage,
  parseSyncMessage,
  calculateChecksum,
  mergeSyncMessages,
  applySyncMessage,
} from '../src/index.mjs';

describe('Streaming: Sync Protocol (Fast)', () => {
  describe('sync message creation', () => {
    it('should create sync message with quads', () => {
      const quads = [
        {
          subject: 'http://example.com/s1',
          predicate: 'http://example.com/p1',
          object: 'http://example.com/o1',
        },
      ];

      const message = createSyncMessage({
        version: 1,
        timestamp: new Date(),
        quads,
      });

      expect(message).toBeDefined();
      expect(message.quads).toHaveLength(1);
      expect(message.timestamp).toBeDefined();
    });

    it('should calculate checksum for sync message', () => {
      const quads = [
        {
          subject: 'http://example.com/s1',
          predicate: 'http://example.com/p1',
          object: 'http://example.com/o1',
        },
      ];

      const message = createSyncMessage({
        version: 1,
        timestamp: new Date(),
        quads,
      });

      const checksum = calculateChecksum(message);

      expect(checksum).toBeDefined();
      expect(typeof checksum).toBe('string');
      expect(checksum.length).toBeGreaterThan(0);
    });
  });

  describe('sync message operations', () => {
    it('should parse and serialize sync message', () => {
      const original = createSyncMessage({
        version: 1,
        timestamp: new Date(),
        quads: [
          {
            subject: 'http://example.com/s1',
            predicate: 'http://example.com/p1',
            object: 'http://example.com/o1',
          },
        ],
      });

      const serialized = JSON.stringify(original);
      const parsed = parseSyncMessage(serialized);

      expect(parsed.version).toBe(original.version);
      expect(parsed.quads).toHaveLength(original.quads.length);
    });

    it('should merge multiple sync messages', () => {
      const msg1 = createSyncMessage({
        version: 1,
        timestamp: new Date(),
        quads: [
          {
            subject: 'http://example.com/s1',
            predicate: 'http://example.com/p1',
            object: 'http://example.com/o1',
          },
        ],
      });

      const msg2 = createSyncMessage({
        version: 1,
        timestamp: new Date(),
        quads: [
          {
            subject: 'http://example.com/s2',
            predicate: 'http://example.com/p2',
            object: 'http://example.com/o2',
          },
        ],
      });

      const merged = mergeSyncMessages([msg1, msg2]);

      expect(merged.quads).toHaveLength(2);
    });
  });
});
