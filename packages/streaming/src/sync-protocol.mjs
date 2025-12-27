/**
 * @file Sync Protocol - Synchronization message protocol
 * @module streaming/sync-protocol
 *
 * @description
 * Implements a synchronization protocol for distributing RDF changes
 * across nodes with checksum verification and delta merging.
 */

import { createHash } from 'crypto';
import { z } from 'zod';

/**
 * Schema for sync messages
 */
const SyncMessageSchema = z.object({
  version: z.string().default('1.0'),
  changes: z.array(z.object({
    type: z.enum(['add', 'remove', 'update']),
    quad: z.any(),
    timestamp: z.number(),
    metadata: z.record(z.any()).optional(),
  })),
  checksum: z.string(),
  timestamp: z.number(),
  source: z.string().optional(),
  sequence: z.number().optional(),
});

/**
 * Create a sync message from changes
 *
 * @param {Array<Object>} changes - Array of change events
 * @param {Object} [options] - Additional options
 * @param {string} [options.source] - Source identifier
 * @param {number} [options.sequence] - Sequence number
 * @returns {Object} Sync message
 *
 * @example
 * const message = createSyncMessage([
 *   { type: 'add', quad: {...}, timestamp: Date.now() }
 * ]);
 */
export function createSyncMessage(changes, options = {}) {
  const timestamp = Date.now();
  const checksum = calculateChecksum(changes);

  return SyncMessageSchema.parse({
    version: '1.0',
    changes,
    checksum,
    timestamp,
    source: options.source,
    sequence: options.sequence,
  });
}

/**
 * Parse and validate a sync message
 *
 * @param {Object} message - Sync message to parse
 * @returns {Object} Validated sync message
 * @throws {Error} If checksum mismatch or invalid format
 *
 * @example
 * const parsed = parseSyncMessage(receivedMessage);
 */
export function parseSyncMessage(message) {
  const validated = SyncMessageSchema.parse(message);

  // Verify checksum
  const expectedChecksum = calculateChecksum(validated.changes);
  if (validated.checksum !== expectedChecksum) {
    throw new Error('Checksum mismatch: message may be corrupted');
  }

  return validated;
}

/**
 * Calculate SHA-256 checksum of changes
 *
 * @param {Array<Object>} changes - Array of changes
 * @returns {string} Hex-encoded SHA-256 checksum
 *
 * @example
 * const checksum = calculateChecksum(changes);
 */
export function calculateChecksum(changes) {
  // Sort changes by timestamp for consistent hashing
  const sortedChanges = [...changes].sort((a, b) => a.timestamp - b.timestamp);

  // Create canonical representation
  const canonical = sortedChanges.map(change => {
    const quad = change.quad;
    return `${change.type}:${quad.subject?.value || ''}:${quad.predicate?.value || ''}:${quad.object?.value || ''}:${change.timestamp}`;
  }).join('|');

  // Calculate SHA-256 hash
  return createHash('sha256').update(canonical).digest('hex');
}

/**
 * Merge multiple sync messages into one
 *
 * @param {Array<Object>} messages - Array of sync messages
 * @returns {Object} Merged sync message
 *
 * @example
 * const merged = mergeSyncMessages([message1, message2, message3]);
 */
export function mergeSyncMessages(messages) {
  // Collect all changes from all messages
  const allChanges = [];

  for (const message of messages) {
    // Parse and validate each message
    const validated = parseSyncMessage(message);
    allChanges.push(...validated.changes);
  }

  // Sort by timestamp
  allChanges.sort((a, b) => a.timestamp - b.timestamp);

  // Deduplicate based on quad identity and type
  const seen = new Set();
  const deduplicated = [];

  for (const change of allChanges) {
    const key = `${change.type}:${change.quad.subject?.value}:${change.quad.predicate?.value}:${change.quad.object?.value}:${change.timestamp}`;
    if (!seen.has(key)) {
      seen.add(key);
      deduplicated.push(change);
    }
  }

  // Create new merged message
  return createSyncMessage(deduplicated, {
    source: 'merged',
    sequence: 0,
  });
}

/**
 * Apply sync message changes to a change feed
 *
 * @param {Object} feed - Change feed instance
 * @param {Object} message - Sync message
 * @returns {number} Number of changes applied
 *
 * @example
 * const applied = applySyncMessage(feed, message);
 * console.log(`Applied ${applied} changes`);
 */
export function applySyncMessage(feed, message) {
  const validated = parseSyncMessage(message);
  let applied = 0;

  for (const change of validated.changes) {
    feed.emitChange(change);
    applied++;
  }

  return applied;
}

/**
 * Create a sync message from change feed history
 *
 * @param {Object} feed - Change feed instance
 * @param {Object} [options] - Query options
 * @param {number} [options.since] - Only include changes after this timestamp
 * @param {number} [options.limit] - Maximum number of changes
 * @returns {Object} Sync message
 *
 * @example
 * const syncMsg = createSyncMessageFromFeed(feed, { since: lastSync });
 */
export function createSyncMessageFromFeed(feed, options = {}) {
  const history = feed.getHistory(options);
  return createSyncMessage(history, {
    source: 'feed',
  });
}
