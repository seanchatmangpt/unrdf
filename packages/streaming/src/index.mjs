/**
 * @unrdf/streaming
 *
 * Streaming - Real-time Change Feeds and Synchronization
 *
 * @module @unrdf/streaming
 */

// Export change feed
export { createChangeFeed } from './streaming/change-feed.mjs';

// Export subscription manager
export { createSubscriptionManager } from './streaming/subscription-manager.mjs';

// Export stream processor
export { createStreamProcessor } from './streaming/stream-processor.mjs';

// Export sync protocol
export {
  createSyncMessage,
  parseSyncMessage,
  calculateChecksum,
  mergeSyncMessages,
} from './streaming/sync-protocol.mjs';
