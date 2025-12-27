/**
 * @file UNRDF Streaming - Main exports
 * @module @unrdf/streaming
 *
 * @description
 * Real-time RDF streaming, change feeds, and synchronization.
 */

// Change Feed
export { createChangeFeed } from './streaming/change-feed.mjs';

// Subscription Manager
export { createSubscriptionManager } from './streaming/subscription-manager.mjs';

// Stream Processor
export { createStreamProcessor } from './streaming/stream-processor.mjs';

// Real-time Validator
export {
  RealTimeValidator,
  createRealTimeValidator,
  ValidationMode,
} from './streaming/real-time-validator.mjs';

// Sync Protocol
export {
  createSyncMessage,
  parseSyncMessage,
  calculateChecksum,
  mergeSyncMessages,
  applySyncMessage,
  createSyncMessageFromFeed,
} from './sync-protocol.mjs';

// Validation
export {
  validateShacl,
  validateQuad,
} from './validate.mjs';

// Observability
export {
  createObservabilityManager,
} from './observability.mjs';

// RDF Stream Parser (V6 Features)
export {
  RDFStreamParser,
  createRDFStreamParser,
  parseRDFStream,
} from './rdf-stream-parser.mjs';

// Performance Monitor (V6 Features)
export {
  PerformanceMonitor,
  createPerformanceMonitor,
} from './performance-monitor.mjs';

// Benchmarks
export {
  generateSyntheticRDF,
  createReadableStreamFromString,
  benchmarkParsingThroughput,
  benchmarkChangeFeedLatency,
  benchmarkBackpressure,
  benchmarkMemoryEfficiency,
  runComprehensiveBenchmarks,
  saveBenchmarkResults,
} from './benchmarks.mjs';
