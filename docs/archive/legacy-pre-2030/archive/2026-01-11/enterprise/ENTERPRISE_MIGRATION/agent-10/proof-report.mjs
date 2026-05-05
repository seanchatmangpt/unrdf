/**
 * Proof Report - Generate comprehensive proof reports
 * @module agent-10/proof-report
 */

/**
 * @typedef {Object} ProofReport
 * @property {number} timestamp - Report timestamp
 * @property {string} status - 'PASS' | 'FAIL'
 * @property {Object} contracts - Contract verification status
 * @property {Object} scenarios - Scenario execution results
 * @property {Object} receipts - Receipt verification results
 * @property {Object} mismatches - Mismatch analysis
 * @property {number} duration - Total execution duration in ms
 */

/**
 * Generate comprehensive proof report
 * @param {Object} results - Aggregated results
 * @param {Object} results.contracts - Contract verification results
 * @param {Object} results.scenarios - Scenario execution results
 * @param {Object} results.receipts - Receipt verification results
 * @param {Object} results.mismatches - Mismatch report
 * @param {number} results.duration - Total duration
 * @returns {ProofReport} Proof report
 */
export function generateProofReport(results) {
  const { contracts, scenarios, receipts, mismatches, duration } = results;

  // Determine overall status
  const contractsPass = contracts?.verified === true;
  const scenariosPass = scenarios?.failed === 0;
  const receiptsPass = receipts?.tampered === 0 && receipts?.valid === receipts?.total;
  const mismatchesPass = mismatches?.count === 0;

  const status = contractsPass && scenariosPass && receiptsPass && mismatchesPass ? 'PASS' : 'FAIL';

  return {
    timestamp: Date.now(),
    status,
    contracts: {
      verified: contracts?.verified || false,
      count: contracts?.count || 0,
      driftDetected: contracts?.driftDetected || false,
    },
    scenarios: {
      passed: scenarios?.passed || 0,
      failed: scenarios?.failed || 0,
      total: scenarios?.total || 0,
      passRate: scenarios?.passRate || 0,
    },
    receipts: {
      valid: receipts?.valid || 0,
      tampered: receipts?.tampered || 0,
      total: receipts?.total || 0,
      chainIntact: receipts?.chainIntact || false,
    },
    mismatches: {
      count: mismatches?.count || 0,
      acceptableThreshold: mismatches?.acceptableThreshold || 0,
      rate: mismatches?.rate || 0,
    },
    duration: duration || 0,
  };
}

/**
 * Format proof report as human-readable text
 * @param {ProofReport} report - Proof report
 * @returns {string} Formatted report
 */
export function formatProofReport(report) {
  const lines = [];

  lines.push('='.repeat(60));
  lines.push('ENTERPRISE MIGRATION PROOF REPORT');
  lines.push('='.repeat(60));
  lines.push('');

  // Overall status
  const statusIcon = report.status === 'PASS' ? '✓' : '✗';
  lines.push(`Overall Status: ${statusIcon} ${report.status}`);
  lines.push(`Timestamp: ${new Date(report.timestamp).toISOString()}`);
  lines.push(`Duration: ${report.duration}ms`);
  lines.push('');

  // Contracts
  lines.push('-'.repeat(60));
  lines.push('CONTRACTS');
  lines.push('-'.repeat(60));
  lines.push(`Verified: ${report.contracts.verified ? '✓ Yes' : '✗ No'}`);
  lines.push(`Count: ${report.contracts.count}`);
  lines.push(`Drift Detected: ${report.contracts.driftDetected ? '✗ Yes' : '✓ No'}`);
  lines.push('');

  // Scenarios
  lines.push('-'.repeat(60));
  lines.push('SCENARIOS');
  lines.push('-'.repeat(60));
  lines.push(`Total: ${report.scenarios.total}`);
  lines.push(`Passed: ${report.scenarios.passed}`);
  lines.push(`Failed: ${report.scenarios.failed}`);
  lines.push(`Pass Rate: ${report.scenarios.passRate.toFixed(2)}%`);
  lines.push('');

  // Receipts
  lines.push('-'.repeat(60));
  lines.push('RECEIPTS');
  lines.push('-'.repeat(60));
  lines.push(`Total: ${report.receipts.total}`);
  lines.push(`Valid: ${report.receipts.valid}`);
  lines.push(`Tampered: ${report.receipts.tampered}`);
  lines.push(`Chain Intact: ${report.receipts.chainIntact ? '✓ Yes' : '✗ No'}`);
  lines.push('');

  // Mismatches
  lines.push('-'.repeat(60));
  lines.push('MISMATCHES');
  lines.push('-'.repeat(60));
  lines.push(`Count: ${report.mismatches.count}`);
  lines.push(`Rate: ${report.mismatches.rate.toFixed(2)}%`);
  lines.push(`Acceptable Threshold: ${report.mismatches.acceptableThreshold}`);
  lines.push('');

  lines.push('='.repeat(60));

  return lines.join('\n');
}

/**
 * Generate detailed failure report
 * @param {Object} results - Full results object with details
 * @returns {Object} Detailed failure report
 */
export function generateFailureReport(results) {
  const failures = {
    contracts: [],
    scenarios: [],
    receipts: [],
    mismatches: [],
  };

  // Contract failures
  if (results.contracts?.driftDetected) {
    failures.contracts.push({
      type: 'CONTRACT_DRIFT',
      message: 'Contract drift detected',
      details: results.contracts.driftDetails || {},
    });
  }

  // Scenario failures
  if (results.scenarioResults?.results) {
    const failedScenarios = results.scenarioResults.results.filter(r => !r.passed);
    failures.scenarios = failedScenarios.map(s => ({
      name: s.name,
      differences: s.differences,
      legacyError: s.legacyError?.message,
      migratedError: s.migratedError?.message,
    }));
  }

  // Receipt failures
  if (results.receiptVerification?.tamperedDetails) {
    failures.receipts = results.receiptVerification.tamperedDetails.map(t => ({
      id: t.id,
      index: t.index,
      error: t.error,
    }));
  }

  // Chain errors
  if (results.receiptVerification?.chainErrors) {
    failures.receipts.push(...results.receiptVerification.chainErrors.map(e => ({
      type: 'CHAIN_ERROR',
      error: e,
    })));
  }

  // Mismatches
  if (results.mismatchReport?.mismatches) {
    failures.mismatches = results.mismatchReport.mismatches.map(m => ({
      scenario: m.scenario,
      differences: m.differences,
    }));
  }

  return {
    timestamp: Date.now(),
    totalFailures:
      failures.contracts.length +
      failures.scenarios.length +
      failures.receipts.length +
      failures.mismatches.length,
    failures,
  };
}

/**
 * Export report as JSON
 * @param {ProofReport} report - Proof report
 * @returns {string} JSON string
 */
export function exportReportJSON(report) {
  return JSON.stringify(report, null, 2);
}

/**
 * Generate summary statistics
 * @param {ProofReport} report - Proof report
 * @returns {Object} Summary statistics
 */
export function generateSummaryStats(report) {
  return {
    overallStatus: report.status,
    totalChecks:
      1 + // contracts
      report.scenarios.total +
      report.receipts.total +
      1, // mismatches
    passedChecks:
      (report.contracts.verified ? 1 : 0) +
      report.scenarios.passed +
      report.receipts.valid +
      (report.mismatches.count === 0 ? 1 : 0),
    failedChecks:
      (!report.contracts.verified ? 1 : 0) +
      report.scenarios.failed +
      report.receipts.tampered +
      (report.mismatches.count > 0 ? 1 : 0),
    duration: report.duration,
    timestamp: report.timestamp,
  };
}

/**
 * Check if report passes all criteria
 * @param {ProofReport} report - Proof report
 * @returns {boolean} True if all checks pass
 */
export function isReportPassing(report) {
  return report.status === 'PASS';
}

/**
 * Get exit code based on report
 * @param {ProofReport} report - Proof report
 * @param {Object} results - Full results for detailed error codes
 * @returns {number} Exit code (0=success, 1=contract drift, 2=scenario fail, 3=receipt tamper)
 */
export function getExitCode(report, results) {
  if (report.status === 'PASS') {
    return 0;
  }

  // Priority: contracts > receipts > scenarios
  if (report.contracts.driftDetected) {
    return 1; // Contract drift
  }

  if (report.receipts.tampered > 0 || !report.receipts.chainIntact) {
    return 3; // Receipt tampering
  }

  if (report.scenarios.failed > 0) {
    return 2; // Scenario failure
  }

  return 1; // Default failure
}
