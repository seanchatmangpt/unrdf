/**
 * @file CLI Utilities Index
 * @module cli/utils
 *
 * @description
 * Central export for all CLI utility modules.
 */

export { loadConfig, createDefaultConfig, ConfigSchema } from './config-loader.mjs';

export {
  handleError,
  withErrorHandling,
  ErrorCodes,
  ParseError,
  QueryError,
  ValidationError,
  ConversionError,
  HookError,
  ConfigError,
} from './error-handler.mjs';

export {
  withContext,
  createExecutionContext,
  validateRequiredArgs,
  getArg,
} from './context-wrapper.mjs';

// Hook and Policy utilities
export { evaluateHook } from './hook-evaluator.mjs';
export { validatePolicy, formatValidationReport } from './policy-validator.mjs';

// OTEL utilities
export { initializeTracer, shutdownTracer, getTracer, printTraceInfo } from './otel-tracer.mjs';

// Sidecar utilities
export {
  withSidecar,
  getSidecarClient,
  closeSidecarClient,
  isSidecarAvailable,
  formatSidecarError,
} from './sidecar-helper.mjs';
