/**
 * Agent 10 - E2E Scenarios and Proof System
 * @module agent-10
 */

// Scenario Suite
export {
  defineScenarios,
  getScenario,
  validateScenario,
} from './scenario-suite.mjs';

// Scenario Runner
export {
  runScenario,
  runAllScenarios,
  runScenarioByName,
  filterResultsByStatus,
  getSummaryStats,
} from './scenario-runner.mjs';

// Shadow Comparator
export {
  compareLegacyVsMigrated,
  generateDiffReport,
  isEquivalent,
  calculateSimilarity,
  createMismatchReport,
} from './shadow-comparator.mjs';

// Receipt Verifier
export {
  computeReceiptHash,
  verifyReceipt,
  verifyChain,
  verifyAllReceipts,
  generateVerificationReport,
  createReceipt,
  createReceiptChain,
  exportReceiptChain,
  importReceiptChain,
} from './receipt-verifier.mjs';

// Proof Report
export {
  generateProofReport,
  formatProofReport,
  generateFailureReport,
  exportReportJSON,
  generateSummaryStats,
  isReportPassing,
  getExitCode,
} from './proof-report.mjs';
