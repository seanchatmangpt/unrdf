/**
 * @file Stream Processor Utilities - Advanced stream processing exports
 * @module streaming/processor
 *
 * @description
 * Advanced stream processing utilities including RDF parsing, performance
 * monitoring, and transform pipelines.
 */

// RDF Stream Parser
export {
  RDFStreamParser,
  createRDFStreamParser,
  parseRDFStream,
} from './rdf-stream-parser.mjs';

// Performance Monitor
export {
  PerformanceMonitor,
  createPerformanceMonitor,
} from './performance-monitor.mjs';

// Stream Processor (basic)
export { createStreamProcessor } from './streaming/stream-processor.mjs';

// Re-export sync protocol for convenience
export {
  createSyncMessage,
  parseSyncMessage,
  calculateChecksum,
  mergeSyncMessages,
} from './sync-protocol.mjs';
