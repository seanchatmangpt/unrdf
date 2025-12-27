/**
 * @file OTEL Validation Package Entry Point
 * @module @unrdf/validation
 *
 * @description
 * Main entry point for the OTEL validation framework.
 * Exports validation runner and OTEL validator for span-based testing.
 */

// Export validation runner
export {
  ValidationRunner,
  createValidationRunner,
  defaultValidationRunner,
} from './validation-runner.mjs';

// Export OTEL validator
export {
  OTELValidator,
  createOTELValidator,
  defaultOTELValidator,
  ValidationResultSchema,
  FeatureValidationConfigSchema,
} from './otel-validator.mjs';

// Export span builder utilities
export {
  createSpanData,
  executeKnowledgeEngine,
  executeCLIParse,
  executeCLIQuery,
  executeCLIValidate,
  executeCLIHook,
  executeTransactionManager,
  executeKnowledgeEngineCore,
  executeKnowledgeHooksAPI,
  executePolicyPacks,
  executeLockchainIntegrity,
  executeBrowserCompatibility,
  executeAtomVMBridge,
  executeAtomVMRuntime,
  executeAtomVMErlang,
} from './otel-span-builder.mjs';

// Export metrics collector
export {
  MetricsCollector,
  validateMetrics,
  defaultMetricsCollector,
  createMetricsCollector,
} from './otel-metrics-collector.mjs';
