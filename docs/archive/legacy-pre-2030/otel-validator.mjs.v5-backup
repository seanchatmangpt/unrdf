/**
 * @file OpenTelemetry Span-Based Validation Framework
 * @module validation/otel-validator
 *
 * @description
 * Replaces traditional unit tests with OpenTelemetry span validation.
 * Features are validated by analyzing OTEL spans, metrics, and traces
 * instead of isolated test assertions.
 *
 * This is a facade module that re-exports from the split modules.
 */

// Core validator
export {
  OTELValidator,
  createOTELValidator,
  ValidationResultSchema,
  FeatureValidationConfigSchema,
} from './otel-validator-core.mjs';

// Span builder utilities
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
} from './otel-span-builder.mjs';

// Metrics collector
export {
  MetricsCollector,
  validateMetrics,
  defaultMetricsCollector,
  createMetricsCollector,
} from './otel-metrics-collector.mjs';

// Reporter utilities
export {
  formatValidationResult,
  formatValidationSummary,
  formatAsJSON,
  formatAsMarkdown,
  createReport,
  printResult,
  printSummary,
  ValidationReporter,
  defaultReporter,
  createReporter,
} from './otel-reporter.mjs';

// Create and export default instance
import { createOTELValidator } from './otel-validator-core.mjs';

/**
 * Default OTEL validator instance
 */
export const defaultOTELValidator = createOTELValidator();
