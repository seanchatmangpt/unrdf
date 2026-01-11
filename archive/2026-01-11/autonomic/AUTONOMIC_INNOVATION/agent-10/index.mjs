/**
 * @fileoverview Agent 10 - Quality Assurance & E2E Validation
 * @module agent-10
 *
 * Public exports:
 * - e2eValidation: Complete integration test
 * - determinismValidation: Hash verification across runs
 * - runQualityGates: Aggregate test results & scoring
 * - validateIntegration: System coherence checks
 * - statisticalDeterminism: 10-run confidence test
 * - generateQualityReport: Comprehensive quality report
 */

export {
  e2eValidation,
  validateE2ESLA
} from './e2e-test.mjs';

export {
  determinismValidation,
  statisticalDeterminism,
  quickDeterminismCheck
} from './determinism-test.mjs';

export {
  runQualityGates,
  validateIntegration,
  validatePrimitives,
  generateQualityReport
} from './quality-report.mjs';
