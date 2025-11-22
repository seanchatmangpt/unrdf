/**
 * @file Validation Framework Index
 * @module validation
 *
 * @description
 * Main entry point for OTEL span-based validation framework.
 * Replaces traditional unit testing with OpenTelemetry span analysis.
 */

// Export main validation components
export { OTELValidator, createOTELValidator, defaultOTELValidator } from './otel-validator.mjs';
export {
  ValidationHelpers,
  createValidationHelpers,
  defaultValidationHelpers,
} from './validation-helpers.mjs';
export {
  ValidationRunner,
  createValidationRunner,
  defaultValidationRunner,
} from './validation-runner.mjs';

// Re-export for convenience
export { defaultValidationRunner as validator } from './validation-runner.mjs';
export { defaultOTELValidator as otelValidator } from './otel-validator.mjs';
export { defaultValidationHelpers as helpers } from './validation-helpers.mjs';
