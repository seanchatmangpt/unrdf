/** Main public entry point for @unrdf/atomvm. */
export { AtomVMRuntime } from './atomvm-runtime.mjs';
export { AtomVMNodeRuntime } from './node-runtime.mjs';
export { TerminalUI } from './terminal-ui.mjs';
export {
  registerServiceWorker,
  checkCrossOriginIsolation,
  getCOIStatus,
  waitForCOI,
} from './service-worker-manager.mjs';

export { HotCodeLoader } from './hot-code-loader.mjs';
export { SupervisorTree } from './supervisor-tree.mjs';
export { QueryCache, createQueryCache } from './query-cache.mjs';
export { OxigraphBridge, BRIDGE_OPERATIONS } from './oxigraph-bridge.mjs';
export {
  createOxigraphStore,
  createOxigraphBridge,
  createIntegratedStore,
  dataFactory,
} from './oxigraph-integration.mjs';
export { RDFValidator, NAMESPACES, createPreInsertionValidator } from './rdf-validator.mjs';
export { App } from './app.mjs';
export { TripleStreamBatcher, createTripleStreamBatcher } from './triple-stream-batcher.mjs';
export { SPARQLPatternMatcher, createSPARQLPatternMatcher } from './sparql-pattern-matcher.mjs';
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
export { SLAMonitor, createSLAMonitor, defaultSLAMonitor, OPERATION_TYPES } from './sla-monitor.mjs';
export { HooksBridge, HOOKS_BRIDGE_OPERATIONS } from './hooks-bridge.mjs';
export { CircuitBreaker } from './circuit-breaker.mjs';
export {
  startRoundtrip,
  endRoundtrip,
  getSLAStats,
  canStartRoundtrip,
  validateRoundtripLatency,
} from './roundtrip-sla.mjs';
export { HardenedAtomVM } from './vm/facade.mjs';
export { Powl8Builder } from './vm/builder.mjs';
export { ConstitutionalLoader } from './vm/loader.mjs';
export { SecuritySandbox } from './vm/sandbox.mjs';
export { POWL8_OPCODES, registerPowl8Opcodes } from './vm/opcodes.mjs';
export { Powl8Scheduler } from './vm/scheduler.mjs';
export { VmTelemetry } from './vm/telemetry.mjs';
export { ReceiptGenerator } from './vm/receipt-generator.mjs';
export {
  AtomVMSwarmCluster,
  SwarmClusterRefusal,
  createAtomVMSwarmCluster,
} from './swarm-cluster.mjs';
export {
  AtomVMProcessBroker,
  AtomVMProcessRefusal,
  createAtomVMProcessBroker,
} from './process-broker.mjs';
export {
  INNOVATION_CHECKPOINTS,
  evaluateInnovationCheckpoints,
  receiptToOcel,
} from './innovation-checkpoints.mjs';

// JOTP pattern-language port: real processes, lifecycle, workers, and applications.
export * from './otp/index.mjs';
