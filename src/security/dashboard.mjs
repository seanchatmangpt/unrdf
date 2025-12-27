/**
 * @fileoverview Security Dashboard - Aggregate security metrics and emit reports
 *
 * Provides unified security view across all 42 packages:
 * - Vulnerabilities per package
 * - Secrets risk score
 * - Injection risk areas
 * - License compatibility matrix
 * - Trend analysis (improving or degrading)
 *
 * @module security/dashboard
 */

import { z } from 'zod';
import { writeFileSync, readFileSync, existsSync, mkdirSync } from 'node:fs';
import { join, dirname } from 'node:path';

import secretDetector from './secret-detector.mjs';
import injectionChecker from './injection-checker.mjs';
import dependencyAuditor from './dependency-auditor.mjs';
import licenseChecker from './license-checker.mjs';
import auditTrailValidator from './audit-trail-validator.mjs';

/**
 * Security metrics schema
 */
export const SecurityMetricsSchema = z.object({
  timestamp: z.string(),
  overallScore: z.number().min(0).max(100),
  passed: z.boolean(),
  components: z.object({
    secrets: z.object({
      score: z.number(),
      findings: z.number(),
      critical: z.number(),
      high: z.number()
    }),
    injection: z.object({
      score: z.number(),
      findings: z.number(),
      critical: z.number(),
      high: z.number()
    }),
    dependencies: z.object({
      score: z.number(),
      vulnerabilities: z.number(),
      critical: z.number(),
      high: z.number()
    }),
    licenses: z.object({
      score: z.number(),
      incompatible: z.number(),
      compliance: z.number()
    }),
    auditTrail: z.object({
      score: z.number(),
      coverage: z.number(),
      integrity: z.number()
    })
  }),
  trend: z.enum(['improving', 'stable', 'degrading']).optional()
});

/**
 * @typedef {z.infer<typeof SecurityMetricsSchema>} SecurityMetrics
 */

/**
 * Run all security scans and generate dashboard
 *
 * @param {string} directory - Root directory to scan
 * @param {Object} [options] - Scan options
 * @returns {Promise<Object>} Dashboard results
 */
export async function generateDashboard(directory, options = {}) {
  const startTime = Date.now();

  console.log('Starting security scan...');

  // Run all scans in parallel
  const [
    secretsResult,
    injectionResult,
    dependencyResult,
    licenseResult
  ] = await Promise.all([
    secretDetector.scanDirectory(directory, options.secrets),
    injectionChecker.scanDirectory(directory, options.injection),
    dependencyAuditor.auditDirectory(directory, options.dependencies),
    licenseChecker.checkDirectory(directory, options.licenses)
  ]);

  // Validate audit trail if directory exists
  const receiptsDir = join(directory, 'receipts');
  let auditResult = {
    passed: true,
    summary: { integrityScore: 100 },
    metadata: { directory: receiptsDir }
  };

  if (existsSync(receiptsDir)) {
    auditResult = auditTrailValidator.validateDirectory(receiptsDir);
  }

  // Calculate component scores (0-100, higher is better)
  const secretsScore = Math.max(0, 100 - secretsResult.summary.riskScore);
  const injectionScore = Math.max(0, 100 - injectionResult.summary.riskScore);
  const dependencyScore = Math.max(0, 100 - dependencyResult.summary.riskScore);
  const licenseScore = licenseResult.summary.complianceScore;
  const auditScore = auditResult.summary.integrityScore;

  // Calculate overall score (weighted average)
  const weights = {
    secrets: 0.25,
    injection: 0.25,
    dependencies: 0.25,
    licenses: 0.15,
    audit: 0.10
  };

  const overallScore = Math.round(
    secretsScore * weights.secrets +
    injectionScore * weights.injection +
    dependencyScore * weights.dependencies +
    licenseScore * weights.licenses +
    auditScore * weights.audit
  );

  // Determine pass/fail
  const passed = (
    secretsResult.passed &&
    injectionResult.passed &&
    dependencyResult.passed &&
    licenseResult.passed
  );

  // Build metrics
  const metrics = {
    timestamp: new Date().toISOString(),
    overallScore,
    passed,
    components: {
      secrets: {
        score: secretsScore,
        findings: secretsResult.summary.total,
        critical: secretsResult.summary.bySeverity.critical,
        high: secretsResult.summary.bySeverity.high
      },
      injection: {
        score: injectionScore,
        findings: injectionResult.summary.total,
        critical: injectionResult.summary.bySeverity.critical,
        high: injectionResult.summary.bySeverity.high
      },
      dependencies: {
        score: dependencyScore,
        vulnerabilities: dependencyResult.summary.total,
        critical: dependencyResult.summary.bySeverity.critical,
        high: dependencyResult.summary.bySeverity.high
      },
      licenses: {
        score: licenseScore,
        incompatible: licenseResult.summary.incompatible,
        compliance: licenseResult.summary.complianceScore
      },
      auditTrail: {
        score: auditScore,
        coverage: 100,
        integrity: auditResult.summary.integrityScore
      }
    }
  };

  // Load historical data for trend analysis
  const historyPath = options.historyPath || join(directory, '.security-history.json');
  const history = loadHistory(historyPath);

  if (history.length > 0) {
    const previousScore = history[history.length - 1].overallScore;
    const scoreDiff = overallScore - previousScore;

    if (scoreDiff > 5) {
      metrics.trend = 'improving';
    } else if (scoreDiff < -5) {
      metrics.trend = 'degrading';
    } else {
      metrics.trend = 'stable';
    }
  }

  // Save to history
  history.push({
    timestamp: metrics.timestamp,
    overallScore: metrics.overallScore,
    passed: metrics.passed
  });

  // Keep last 30 entries
  const trimmedHistory = history.slice(-30);
  saveHistory(historyPath, trimmedHistory);

  const scanDuration = Date.now() - startTime;

  return {
    metrics,
    results: {
      secrets: secretsResult,
      injection: injectionResult,
      dependencies: dependencyResult,
      licenses: licenseResult,
      auditTrail: auditResult
    },
    history: trimmedHistory,
    scanDuration
  };
}

/**
 * Load historical metrics
 *
 * @param {string} historyPath - Path to history file
 * @returns {Array} Historical metrics
 */
function loadHistory(historyPath) {
  try {
    if (existsSync(historyPath)) {
      const content = readFileSync(historyPath, 'utf-8');
      return JSON.parse(content);
    }
  } catch {
    // Return empty if file doesn't exist or is invalid
  }
  return [];
}

/**
 * Save historical metrics
 *
 * @param {string} historyPath - Path to history file
 * @param {Array} history - Historical data
 */
function saveHistory(historyPath, history) {
  try {
    const dir = dirname(historyPath);
    if (!existsSync(dir)) {
      mkdirSync(dir, { recursive: true });
    }
    writeFileSync(historyPath, JSON.stringify(history, null, 2));
  } catch {
    // Ignore write errors
  }
}

/**
 * Format dashboard as text report
 *
 * @param {Object} dashboard - Dashboard results
 * @returns {string} Formatted report
 */
export function formatTextReport(dashboard) {
  const { metrics, scanDuration } = dashboard;
  const lines = [];

  lines.push('');
  lines.push('='.repeat(70));
  lines.push('                    SECURITY DASHBOARD');
  lines.push('='.repeat(70));
  lines.push('');
  lines.push(`Scan Time: ${metrics.timestamp}`);
  lines.push(`Duration: ${scanDuration}ms`);
  lines.push('');
  lines.push('-'.repeat(70));
  lines.push('                         OVERALL STATUS');
  lines.push('-'.repeat(70));
  lines.push('');
  lines.push(`    Score: ${metrics.overallScore}/100 ${getScoreBar(metrics.overallScore)}`);
  lines.push(`   Status: ${metrics.passed ? 'PASSED' : 'FAILED'}`);
  if (metrics.trend) {
    lines.push(`    Trend: ${getTrendIndicator(metrics.trend)}`);
  }
  lines.push('');
  lines.push('-'.repeat(70));
  lines.push('                       COMPONENT SCORES');
  lines.push('-'.repeat(70));
  lines.push('');

  const components = [
    { name: 'Secrets Detection', score: metrics.components.secrets.score,
      detail: `${metrics.components.secrets.findings} findings` },
    { name: 'Injection Analysis', score: metrics.components.injection.score,
      detail: `${metrics.components.injection.findings} findings` },
    { name: 'Dependency Audit', score: metrics.components.dependencies.score,
      detail: `${metrics.components.dependencies.vulnerabilities} vulnerabilities` },
    { name: 'License Compliance', score: metrics.components.licenses.score,
      detail: `${metrics.components.licenses.incompatible} incompatible` },
    { name: 'Audit Trail', score: metrics.components.auditTrail.score,
      detail: `${metrics.components.auditTrail.integrity}% integrity` }
  ];

  for (const comp of components) {
    const bar = getScoreBar(comp.score);
    lines.push(`  ${comp.name.padEnd(20)} ${String(comp.score).padStart(3)}/100 ${bar}  ${comp.detail}`);
  }

  // Critical findings summary
  const criticalFindings = (
    metrics.components.secrets.critical +
    metrics.components.injection.critical +
    metrics.components.dependencies.critical
  );

  const highFindings = (
    metrics.components.secrets.high +
    metrics.components.injection.high +
    metrics.components.dependencies.high
  );

  if (criticalFindings > 0 || highFindings > 0) {
    lines.push('');
    lines.push('-'.repeat(70));
    lines.push('                      ATTENTION REQUIRED');
    lines.push('-'.repeat(70));
    lines.push('');

    if (criticalFindings > 0) {
      lines.push(`  [CRITICAL] ${criticalFindings} critical issues require immediate attention`);
    }
    if (highFindings > 0) {
      lines.push(`  [HIGH] ${highFindings} high severity issues should be addressed`);
    }
  }

  lines.push('');
  lines.push('='.repeat(70));
  lines.push('');

  return lines.join('\n');
}

/**
 * Generate score visualization bar
 *
 * @param {number} score - Score 0-100
 * @returns {string} Visual bar
 */
function getScoreBar(score) {
  const filled = Math.round(score / 5);
  const empty = 20 - filled;

  let color = '';
  if (score >= 80) color = 'green';
  else if (score >= 60) color = 'yellow';
  else color = 'red';

  const filledChar = '#';
  const emptyChar = '-';

  return `[${filledChar.repeat(filled)}${emptyChar.repeat(empty)}]`;
}

/**
 * Get trend indicator
 *
 * @param {string} trend - Trend direction
 * @returns {string} Trend indicator
 */
function getTrendIndicator(trend) {
  switch (trend) {
    case 'improving': return 'Improving (+)';
    case 'degrading': return 'Degrading (!)';
    case 'stable': return 'Stable (=)';
    default: return 'Unknown';
  }
}

/**
 * Format dashboard as JSON
 *
 * @param {Object} dashboard - Dashboard results
 * @returns {string} JSON string
 */
export function formatJsonReport(dashboard) {
  return JSON.stringify(dashboard.metrics, null, 2);
}

/**
 * Format dashboard as HTML
 *
 * @param {Object} dashboard - Dashboard results
 * @returns {string} HTML string
 */
export function formatHtmlReport(dashboard) {
  const { metrics } = dashboard;

  const getStatusClass = (passed) => passed ? 'status-pass' : 'status-fail';
  const getScoreClass = (score) => {
    if (score >= 80) return 'score-good';
    if (score >= 60) return 'score-warn';
    return 'score-bad';
  };

  return `<!DOCTYPE html>
<html lang="en">
<head>
  <meta charset="UTF-8">
  <meta name="viewport" content="width=device-width, initial-scale=1.0">
  <title>Security Dashboard</title>
  <style>
    :root {
      --bg-dark: #1a1a2e;
      --bg-card: #16213e;
      --text-primary: #eee;
      --text-secondary: #aaa;
      --green: #00d26a;
      --yellow: #ffc107;
      --red: #ff4757;
    }
    * { box-sizing: border-box; margin: 0; padding: 0; }
    body {
      font-family: 'Segoe UI', system-ui, sans-serif;
      background: var(--bg-dark);
      color: var(--text-primary);
      line-height: 1.6;
      padding: 2rem;
    }
    .dashboard { max-width: 1200px; margin: 0 auto; }
    .header { text-align: center; margin-bottom: 2rem; }
    .header h1 { font-size: 2rem; margin-bottom: 0.5rem; }
    .header .timestamp { color: var(--text-secondary); }
    .overall-status {
      background: var(--bg-card);
      padding: 2rem;
      border-radius: 12px;
      margin-bottom: 2rem;
      display: flex;
      align-items: center;
      justify-content: space-around;
    }
    .score-circle {
      width: 150px;
      height: 150px;
      border-radius: 50%;
      display: flex;
      flex-direction: column;
      align-items: center;
      justify-content: center;
      font-size: 2.5rem;
      font-weight: bold;
    }
    .score-good { background: linear-gradient(135deg, #00d26a33, #00d26a11); border: 3px solid var(--green); color: var(--green); }
    .score-warn { background: linear-gradient(135deg, #ffc10733, #ffc10711); border: 3px solid var(--yellow); color: var(--yellow); }
    .score-bad { background: linear-gradient(135deg, #ff475733, #ff475711); border: 3px solid var(--red); color: var(--red); }
    .status-pass { color: var(--green); }
    .status-fail { color: var(--red); }
    .components { display: grid; grid-template-columns: repeat(auto-fit, minmax(300px, 1fr)); gap: 1rem; }
    .component-card {
      background: var(--bg-card);
      padding: 1.5rem;
      border-radius: 12px;
    }
    .component-card h3 { margin-bottom: 1rem; }
    .component-score { font-size: 1.5rem; font-weight: bold; }
    .progress-bar {
      height: 8px;
      background: #333;
      border-radius: 4px;
      margin: 0.5rem 0;
      overflow: hidden;
    }
    .progress-fill { height: 100%; border-radius: 4px; }
    .stats { display: flex; gap: 1rem; margin-top: 1rem; color: var(--text-secondary); }
    .stat { flex: 1; }
    .stat-label { font-size: 0.8rem; }
    .stat-value { font-size: 1.2rem; color: var(--text-primary); }
  </style>
</head>
<body>
  <div class="dashboard">
    <div class="header">
      <h1>Security Dashboard</h1>
      <div class="timestamp">${metrics.timestamp}</div>
    </div>

    <div class="overall-status">
      <div class="score-circle ${getScoreClass(metrics.overallScore)}">
        ${metrics.overallScore}
        <span style="font-size: 1rem; font-weight: normal;">/100</span>
      </div>
      <div style="text-align: center;">
        <div style="font-size: 1.2rem; margin-bottom: 0.5rem;">Status</div>
        <div class="${getStatusClass(metrics.passed)}" style="font-size: 2rem; font-weight: bold;">
          ${metrics.passed ? 'PASSED' : 'FAILED'}
        </div>
        ${metrics.trend ? `<div style="margin-top: 0.5rem;">Trend: ${metrics.trend}</div>` : ''}
      </div>
    </div>

    <div class="components">
      <div class="component-card">
        <h3>Secrets Detection</h3>
        <div class="component-score ${getScoreClass(metrics.components.secrets.score)}">${metrics.components.secrets.score}/100</div>
        <div class="progress-bar"><div class="progress-fill ${getScoreClass(metrics.components.secrets.score)}" style="width: ${metrics.components.secrets.score}%"></div></div>
        <div class="stats">
          <div class="stat"><div class="stat-label">Findings</div><div class="stat-value">${metrics.components.secrets.findings}</div></div>
          <div class="stat"><div class="stat-label">Critical</div><div class="stat-value">${metrics.components.secrets.critical}</div></div>
          <div class="stat"><div class="stat-label">High</div><div class="stat-value">${metrics.components.secrets.high}</div></div>
        </div>
      </div>

      <div class="component-card">
        <h3>Injection Analysis</h3>
        <div class="component-score ${getScoreClass(metrics.components.injection.score)}">${metrics.components.injection.score}/100</div>
        <div class="progress-bar"><div class="progress-fill ${getScoreClass(metrics.components.injection.score)}" style="width: ${metrics.components.injection.score}%"></div></div>
        <div class="stats">
          <div class="stat"><div class="stat-label">Findings</div><div class="stat-value">${metrics.components.injection.findings}</div></div>
          <div class="stat"><div class="stat-label">Critical</div><div class="stat-value">${metrics.components.injection.critical}</div></div>
          <div class="stat"><div class="stat-label">High</div><div class="stat-value">${metrics.components.injection.high}</div></div>
        </div>
      </div>

      <div class="component-card">
        <h3>Dependency Audit</h3>
        <div class="component-score ${getScoreClass(metrics.components.dependencies.score)}">${metrics.components.dependencies.score}/100</div>
        <div class="progress-bar"><div class="progress-fill ${getScoreClass(metrics.components.dependencies.score)}" style="width: ${metrics.components.dependencies.score}%"></div></div>
        <div class="stats">
          <div class="stat"><div class="stat-label">Vulnerabilities</div><div class="stat-value">${metrics.components.dependencies.vulnerabilities}</div></div>
          <div class="stat"><div class="stat-label">Critical</div><div class="stat-value">${metrics.components.dependencies.critical}</div></div>
          <div class="stat"><div class="stat-label">High</div><div class="stat-value">${metrics.components.dependencies.high}</div></div>
        </div>
      </div>

      <div class="component-card">
        <h3>License Compliance</h3>
        <div class="component-score ${getScoreClass(metrics.components.licenses.score)}">${metrics.components.licenses.score}/100</div>
        <div class="progress-bar"><div class="progress-fill ${getScoreClass(metrics.components.licenses.score)}" style="width: ${metrics.components.licenses.score}%"></div></div>
        <div class="stats">
          <div class="stat"><div class="stat-label">Incompatible</div><div class="stat-value">${metrics.components.licenses.incompatible}</div></div>
          <div class="stat"><div class="stat-label">Compliance</div><div class="stat-value">${metrics.components.licenses.compliance}%</div></div>
        </div>
      </div>

      <div class="component-card">
        <h3>Audit Trail</h3>
        <div class="component-score ${getScoreClass(metrics.components.auditTrail.score)}">${metrics.components.auditTrail.score}/100</div>
        <div class="progress-bar"><div class="progress-fill ${getScoreClass(metrics.components.auditTrail.score)}" style="width: ${metrics.components.auditTrail.score}%"></div></div>
        <div class="stats">
          <div class="stat"><div class="stat-label">Coverage</div><div class="stat-value">${metrics.components.auditTrail.coverage}%</div></div>
          <div class="stat"><div class="stat-label">Integrity</div><div class="stat-value">${metrics.components.auditTrail.integrity}%</div></div>
        </div>
      </div>
    </div>
  </div>
</body>
</html>`;
}

/**
 * Export dashboard to file
 *
 * @param {Object} dashboard - Dashboard results
 * @param {string} outputPath - Output file path
 * @param {string} [format] - Output format (json, html, text)
 */
export function exportDashboard(dashboard, outputPath, format = 'json') {
  let content;

  switch (format) {
    case 'html':
      content = formatHtmlReport(dashboard);
      break;
    case 'text':
      content = formatTextReport(dashboard);
      break;
    case 'json':
    default:
      content = JSON.stringify(dashboard, null, 2);
  }

  const dir = dirname(outputPath);
  if (!existsSync(dir)) {
    mkdirSync(dir, { recursive: true });
  }

  writeFileSync(outputPath, content);
  return outputPath;
}

/**
 * Generate security receipt
 *
 * @param {Object} dashboard - Dashboard results
 * @returns {Object} Security receipt
 */
export function generateSecurityReceipt(dashboard) {
  return {
    id: `security-scan-${Date.now()}`,
    timestamp: dashboard.metrics.timestamp,
    operation: 'security-scan',
    status: dashboard.metrics.passed ? 'success' : 'failure',
    result: {
      overallScore: dashboard.metrics.overallScore,
      passed: dashboard.metrics.passed,
      components: dashboard.metrics.components,
      trend: dashboard.metrics.trend
    },
    metadata: {
      scanDuration: dashboard.scanDuration,
      componentsChecked: 5
    }
  };
}

export default {
  generateDashboard,
  formatTextReport,
  formatJsonReport,
  formatHtmlReport,
  exportDashboard,
  generateSecurityReceipt,
  SecurityMetricsSchema
};
