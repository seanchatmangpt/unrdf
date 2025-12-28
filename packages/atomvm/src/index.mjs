/**
 * AtomVM Library Entry Point
 *
 * Exports all public APIs for library usage.
 * This is the main entry point when importing @unrdf/atomvm as a library.
 *
 * For the browser application, see src/app.mjs
 *
 * @module @unrdf/atomvm
 */

// Re-export all public APIs for library usage
export { AtomVMRuntime } from './atomvm-runtime.mjs';
export { AtomVMNodeRuntime } from './node-runtime.mjs';
export { TerminalUI } from './terminal-ui.mjs';
export {
  registerServiceWorker,
  checkCrossOriginIsolation,
  getCOIStatus,
  waitForCOI
} from './service-worker-manager.mjs';

// Hot code loading and supervision
export { HotCodeLoader } from './hot-code-loader.mjs';
export { SupervisorTree } from './supervisor-tree.mjs';

// Query caching for SPARQL results
export { QueryCache, createQueryCache } from './query-cache.mjs';

// Oxigraph RDF store integration
export { OxigraphBridge, BRIDGE_OPERATIONS } from './oxigraph-bridge.mjs';

// RDF Validation Framework
export {
  RDFValidator,
  NAMESPACES,
  createPreInsertionValidator
} from './rdf-validator.mjs';

// Export App class for browser application usage
export { App } from './app.mjs';

// Triple Stream Batcher for bulk RDF operations
export {
  TripleStreamBatcher,
  createTripleStreamBatcher,
} from './triple-stream-batcher.mjs';

// SPARQL Pattern Matching
export {
  SPARQLPatternMatcher,
  createSPARQLPatternMatcher,
} from './sparql-pattern-matcher.mjs';

// Distributed Message Validation
export {
  messageSchemas,
  validateTriplePattern,
  validateRPCCall,
  validateRPCResult,
  validateSPARQLQuery,
  validateBatchOperation,
  validateHealthCheck,
  validateMessage,
  createValidationMiddleware,
  withValidation,
} from './message-validator.mjs';

// OTEL Instrumentation
export {
  tracer,
  getTracer,
  createSpan,
  withSpan,
  recordAttribute,
  recordAttributes,
  recordError,
  recordMetric,
  traceTriplePattern,
  traceSPARQLQuery,
  traceRPCCall,
  traceMessageValidation,
  traceCacheOperation,
  traceBatchOperation,
  traceWithTiming,
} from './otel-instrumentation.mjs';

// SLA Monitoring
export {
  SLAMonitor,
  createSLAMonitor,
  defaultSLAMonitor,
  OPERATION_TYPES,
} from './sla-monitor.mjs';

// Additional exports from pre-existing modules
export { CircuitBreaker } from './circuit-breaker.mjs';
export {
  startRoundtrip,
  endRoundtrip,
  getSLAStats,
  canStartRoundtrip,
  validateRoundtripLatency,
} from './roundtrip-sla.mjs';
