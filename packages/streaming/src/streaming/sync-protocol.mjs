/**
 * @file Sync Protocol - Message format for replicating changes
 * @module streaming/sync-protocol
 *
 * @description
 * Provides simple protocol for encoding and decoding change messages
 * for transmission between systems.
 */

import { z } from 'zod';
import { createHash } from 'crypto';

/**
 * Schema for sync messages
 */
const SyncMessageSchema = z.object({
  version: z.literal('1.0'),
  changes: z.array(
    z.object({
      type: z.enum(['add', 'remove', 'update']),
      quad: z.object({
        subject: z.any(),
        predicate: z.any(),
        object: z.any(),
        graph: z.any().optional(),
      }),
      timestamp: z.number(),
      metadata: z.record(z.any()).optional(),
    })
  ),
  checksum: z.string(),
  timestamp: z.number(),
});

/**
 * Create a sync message from changes
 *
 * @param {Array} changes - Array of change events
 * @returns {Object} Sync message
 *
 * @example
 * const message = createSyncMessage([
 *   { type: 'add', quad: {...}, timestamp: Date.now() }
 * ]);
 */
export function createSyncMessage(changes) {
  const message = {
    version: '1.0',
    changes,
    checksum: calculateChecksum(changes),
    timestamp: Date.now(),
  };

  return SyncMessageSchema.parse(message);
}

/**
 * Parse a sync message
 *
 * @param {Object} message - Sync message to parse
 * @returns {Object} Validated sync message
 * @throws {Error} If message is invalid or checksum doesn't match
 *
 * @example
 * const parsed = parseSyncMessage(receivedMessage);
 * for (const change of parsed.changes) {
 *   console.log('Change:', change);
 * }
 */
export function parseSyncMessage(message) {
  const validated = SyncMessageSchema.parse(message);

  const expectedChecksum = calculateChecksum(validated.changes);
  if (validated.checksum !== expectedChecksum) {
    throw new Error(`Checksum mismatch: expected ${expectedChecksum}, got ${validated.checksum}`);
  }

  return validated;
}

/**
 * Calculate checksum for changes
 *
 * @param {Array} changes - Array of changes
 * @returns {string} SHA-256 checksum
 *
 * @example
 * const checksum = calculateChecksum(changes);
 */
export function calculateChecksum(changes) {
  const normalized = changes.map(change => ({
    type: change.type,
    quad: {
      subject: change.quad.subject?.value ?? null,
      predicate: change.quad.predicate?.value ?? null,
      object: change.quad.object?.value ?? null,
      graph: change.quad.graph?.value ?? null,
    },
    timestamp: change.timestamp,
  }));

  const json = JSON.stringify(normalized, Object.keys(normalized).sort());
  return createHash('sha256').update(json).digest('hex');
}

/**
 * Merge sync messages
 *
 * @param {Array} messages - Array of sync messages
 * @returns {Object} Merged sync message
 *
 * @example
 * const merged = mergeSyncMessages([message1, message2]);
 */
export function mergeSyncMessages(messages) {
  const allChanges = [];

  for (const message of messages) {
    const parsed = parseSyncMessage(message);
    allChanges.push(...parsed.changes);
  }

  // Sort by timestamp
  allChanges.sort((a, b) => a.timestamp - b.timestamp);

  return createSyncMessage(allChanges);
}
