/**
 * @unrdf/streaming - Real-time Change Feeds and Synchronization
 *
 * Provides streaming capabilities for RDF stores including:
 * - Real-time change feeds with subscription support
 * - Streaming protocol for data synchronization
 * - Stream processing pipelines
 * - Change event management and validation
 *
 * @module @unrdf/streaming
 * @version 2.0.0
 * @license MIT
 */

/**
 * Create a change feed for tracking RDF store modifications
 *
 * @typedef {Object} ChangeFeed
 * @property {Function} subscribe - Subscribe to change events
 * @property {Function} unsubscribe - Unsubscribe from changes
 * @property {Function} getChanges - Retrieve buffered changes
 *
 * @returns {ChangeFeed} Change feed instance
 * @throws {Error} If store is invalid
 *
 * @example
 * const feed = createChangeFeed(store);
 * feed.subscribe((changes) => console.log('Changed quads:', changes));
 */
export { createChangeFeed } from './streaming/change-feed.mjs';

/**
 * Create a subscription manager for handling multiple listeners
 *
 * @typedef {Object} SubscriptionManager
 * @property {Function} subscribe - Subscribe with handler
 * @property {Function} unsubscribe - Remove subscription
 * @property {Function} notify - Broadcast to all subscribers
 *
 * @returns {SubscriptionManager} Subscription manager instance
 * @throws {Error} If handler is not a function
 *
 * @example
 * const manager = createSubscriptionManager();
 * manager.subscribe(changeHandler);
 */
export { createSubscriptionManager } from './streaming/subscription-manager.mjs';

/**
 * Create a stream processor for building data pipelines
 *
 * @typedef {Object} StreamProcessor
 * @property {Function} map - Transform stream data
 * @property {Function} filter - Filter stream items
 * @property {Function} batch - Batch items by size/time
 * @property {Function} subscribe - Attach listener
 *
 * @returns {StreamProcessor} Processor with chainable methods
 * @throws {Error} If source stream is invalid
 *
 * @example
 * const processor = createStreamProcessor(source)
 *   .filter(item => item.predicate === 'rdf:type')
 *   .batch({ size: 100 });
 */
export { createStreamProcessor } from './streaming/stream-processor.mjs';

/**
 * Synchronization protocol operations for distributed RDF stores
 *
 * Creates typed sync messages with checksum validation:
 * - `createSyncMessage(data, metadata)` - Construct sync message
 * - `parseSyncMessage(raw)` - Parse received message
 * - `calculateChecksum(data)` - Validate data integrity
 * - `mergeSyncMessages(messages)` - Combine multiple sync batches
 *
 * @typedef {Object} SyncMessage
 * @property {string} id - Unique message identifier
 * @property {Array} quads - RDF quads
 * @property {string} checksum - Data integrity hash
 * @property {number} timestamp - Creation timestamp
 * @property {Object} metadata - Optional message metadata
 *
 * @throws {Error} If message validation fails
 *
 * @example
 * const msg = createSyncMessage(quads, { version: 2 });
 * const checksum = calculateChecksum(msg.quads);
 * const merged = mergeSyncMessages([msg1, msg2]);
 */
export {
  createSyncMessage,
  parseSyncMessage,
  calculateChecksum,
  mergeSyncMessages,
} from './streaming/sync-protocol.mjs';
