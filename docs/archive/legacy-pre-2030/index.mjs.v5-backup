/**
 * @fileoverview Production Validation System - Main exports
 *
 * Comprehensive production readiness validation for the UNRDF monorepo.
 * Every package must pass the production gate before deployment.
 *
 * @module validation
 */

export {
  ProductionValidator,
  createProductionValidator,
  validatePackage,
  validatePackages,
  isProductionReady,
  CheckResultSchema,
  ProductionReceiptSchema,
  DEFAULT_WEIGHTS,
  DEFAULT_THRESHOLDS,
  VALIDATOR_VERSION
} from './production-validator.mjs';

export {
  BenchmarkManager,
  createBenchmarkManager,
  checkSLA,
  DEFAULT_SLAS,
  BenchmarkStatus
} from './benchmarks.mjs';

export * from './checks/index.mjs';
