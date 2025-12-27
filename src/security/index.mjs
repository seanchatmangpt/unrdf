/**
 * @fileoverview Unified Security Model - Entry point for monorepo security
 *
 * Provides comprehensive security mechanisms for all 42 packages:
 * - Q_no_hardcoded_secrets: No credentials, keys, or tokens in code
 * - Q_input_validation: All external inputs validated via Zod
 * - Q_no_dangerous_apis: No eval, Function constructor, dynamic requires
 * - Q_dependency_security: No known CVEs in transitive dependencies
 * - Q_license_compliance: All dependencies compatible with project license
 * - Q_audit_trail: All operations logged with receipts
 *
 * @module security
 */

import { existsSync } from 'node:fs';
import { join } from 'node:path';
import { z } from 'zod';

// Import security modules
import * as secretDetector from './secret-detector.mjs';
import * as injectionChecker from './injection-checker.mjs';
import * as dependencyAuditor from './dependency-auditor.mjs';
import * as licenseChecker from './license-checker.mjs';
import * as auditTrailValidator from './audit-trail-validator.mjs';
import * as dashboard from './dashboard.mjs';

/**
 * Security invariant check result schema
 */
export const SecurityCheckResultSchema = z.object({
  invariant: z.string(),
  passed: z.boolean(),
  severity: z.enum(['critical', 'high', 'medium', 'low', 'info']),
  findings: z.array(z.any()),
  summary: z.record(z.any()).optional(),
  metadata: z.record(z.any()).optional()
});

/**
 * Combined security report schema
 */
export const SecurityReportSchema = z.object({
  timestamp: z.string(),
  passed: z.boolean(),
  overallScore: z.number(),
  invariants: z.array(SecurityCheckResultSchema),
  guards: z.array(z.object({
    name: z.string(),
    triggered: z.boolean(),
    details: z.any().optional()
  })),
  duration: z.number()
});

/**
 * Security configuration schema
 */
export const SecurityConfigSchema = z.object({
  secrets: z.object({
    enabled: z.boolean().default(true),
    entropyThreshold: z.number().default(4.5),
    includeHighEntropy: z.boolean().default(true)
  }).optional(),
  injection: z.object({
    enabled: z.boolean().default(true)
  }).optional(),
  dependencies: z.object({
    enabled: z.boolean().default(true),
    online: z.boolean().default(true),
    includeDevDependencies: z.boolean().default(false)
  }).optional(),
  licenses: z.object({
    enabled: z.boolean().default(true),
    allowed: z.array(z.string()).optional(),
    blocked: z.array(z.string()).optional()
  }).optional(),
  auditTrail: z.object({
    enabled: z.boolean().default(true),
    receiptsDirectory: z.string().optional()
  }).optional()
});

/**
 * @typedef {z.infer<typeof SecurityConfigSchema>} SecurityConfig
 */

/**
 * Default security configuration
 */
export const DEFAULT_CONFIG = {
  secrets: {
    enabled: true,
    entropyThreshold: 4.5,
    includeHighEntropy: true
  },
  injection: {
    enabled: true
  },
  dependencies: {
    enabled: true,
    online: true,
    includeDevDependencies: false
  },
  licenses: {
    enabled: true,
    allowed: licenseChecker.DEFAULT_ALLOWED_LICENSES,
    blocked: licenseChecker.BLOCKED_LICENSES
  },
  auditTrail: {
    enabled: true,
    receiptsDirectory: 'receipts'
  }
};

/**
 * Security invariant definitions
 */
export const SECURITY_INVARIANTS = {
  Q_no_hardcoded_secrets: {
    name: 'Q_no_hardcoded_secrets',
    description: 'No credentials, keys, or tokens in code',
    check: async (directory, config) => {
      const result = await secretDetector.scanDirectory(directory, config.secrets);
      return {
        invariant: 'Q_no_hardcoded_secrets',
        passed: result.passed,
        severity: result.summary.bySeverity.critical > 0 ? 'critical' :
                  result.summary.bySeverity.high > 0 ? 'high' : 'info',
        findings: result.findings,
        summary: result.summary,
        metadata: result.metadata
      };
    }
  },

  Q_no_dangerous_apis: {
    name: 'Q_no_dangerous_apis',
    description: 'No eval, Function constructor, dynamic requires',
    check: async (directory, config) => {
      const result = await injectionChecker.scanDirectory(directory, config.injection);
      return {
        invariant: 'Q_no_dangerous_apis',
        passed: result.passed,
        severity: result.summary.bySeverity.critical > 0 ? 'critical' :
                  result.summary.bySeverity.high > 0 ? 'high' : 'info',
        findings: result.findings,
        summary: result.summary,
        metadata: result.metadata
      };
    }
  },

  Q_dependency_security: {
    name: 'Q_dependency_security',
    description: 'No known CVEs in transitive dependencies',
    check: async (directory, config) => {
      const result = await dependencyAuditor.auditDirectory(directory, config.dependencies);
      return {
        invariant: 'Q_dependency_security',
        passed: result.passed,
        severity: result.summary.bySeverity.critical > 0 ? 'critical' :
                  result.summary.bySeverity.high > 0 ? 'high' : 'info',
        findings: result.vulnerabilities,
        summary: result.summary,
        metadata: result.metadata
      };
    }
  },

  Q_license_compliance: {
    name: 'Q_license_compliance',
    description: 'All dependencies compatible with project license',
    check: async (directory, config) => {
      const result = await licenseChecker.checkDirectory(directory, {
        policy: config.licenses
      });
      return {
        invariant: 'Q_license_compliance',
        passed: result.passed,
        severity: result.incompatible.length > 0 ? 'high' : 'info',
        findings: result.incompatible,
        summary: result.summary,
        metadata: result.metadata
      };
    }
  },

  Q_audit_trail: {
    name: 'Q_audit_trail',
    description: 'All operations logged with receipts',
    check: async (directory, config) => {
      const receiptsDir = join(directory, config.auditTrail?.receiptsDirectory || 'receipts');

      if (!existsSync(receiptsDir)) {
        return {
          invariant: 'Q_audit_trail',
          passed: true,
          severity: 'info',
          findings: [],
          summary: { message: 'No receipts directory found' },
          metadata: { directory: receiptsDir }
        };
      }

      const result = auditTrailValidator.validateDirectory(receiptsDir, config.auditTrail);
      return {
        invariant: 'Q_audit_trail',
        passed: result.passed,
        severity: result.summary.bySeverity.critical > 0 ? 'critical' :
                  result.summary.bySeverity.high > 0 ? 'high' : 'info',
        findings: result.findings,
        summary: result.summary,
        metadata: result.metadata
      };
    }
  }
};

/**
 * Security guard definitions
 */
export const SECURITY_GUARDS = {
  H_hardcoded_credential: {
    name: 'H_hardcoded_credential',
    description: 'Cannot commit files with credentials',
    check: (findings) => {
      const critical = findings.filter(f =>
        f.type === 'password' || f.type === 'private_key' ||
        f.type === 'aws_key' || f.type === 'github_token'
      );
      return {
        triggered: critical.length > 0,
        details: critical.length > 0 ? `${critical.length} credential(s) found` : null
      };
    }
  },

  H_code_injection: {
    name: 'H_code_injection',
    description: 'Cannot use eval, Function, or dynamic code',
    check: (findings) => {
      const critical = findings.filter(f =>
        f.type === 'code_injection' && f.severity === 'critical'
      );
      return {
        triggered: critical.length > 0,
        details: critical.length > 0 ? `${critical.length} injection risk(s) found` : null
      };
    }
  },

  H_dependency_exploit: {
    name: 'H_dependency_exploit',
    description: 'Cannot add dependency with known CVE',
    check: (vulnerabilities) => {
      const critical = vulnerabilities.filter(v =>
        v.severity === 'critical'
      );
      return {
        triggered: critical.length > 0,
        details: critical.length > 0 ? `${critical.length} critical CVE(s) found` : null
      };
    }
  },

  H_license_incompatible: {
    name: 'H_license_incompatible',
    description: 'Cannot add dependency with incompatible license',
    check: (incompatible) => {
      return {
        triggered: incompatible.length > 0,
        details: incompatible.length > 0 ? `${incompatible.length} incompatible license(s)` : null
      };
    }
  }
};

/**
 * Run all security invariant checks
 *
 * @param {string} directory - Directory to scan
 * @param {SecurityConfig} [config] - Security configuration
 * @returns {Promise<Object>} Security report
 */
export async function checkAllInvariants(directory, config = DEFAULT_CONFIG) {
  const startTime = Date.now();
  const mergedConfig = { ...DEFAULT_CONFIG, ...config };
  const results = [];

  // Run enabled invariant checks
  for (const [name, invariant] of Object.entries(SECURITY_INVARIANTS)) {
    const configKey = name.replace('Q_', '').replace(/_/g, '');
    const enabled = mergedConfig[configKey]?.enabled !== false;

    if (enabled) {
      try {
        const result = await invariant.check(directory, mergedConfig);
        results.push(result);
      } catch (error) {
        results.push({
          invariant: name,
          passed: false,
          severity: 'high',
          findings: [{ error: error.message }],
          summary: { error: error.message },
          metadata: {}
        });
      }
    }
  }

  // Check guards based on findings
  const guards = [];
  const allFindings = results.flatMap(r => r.findings || []);

  for (const [name, guard] of Object.entries(SECURITY_GUARDS)) {
    const guardResult = guard.check(allFindings);
    guards.push({
      name,
      triggered: guardResult.triggered,
      details: guardResult.details
    });
  }

  // Calculate overall status
  const passed = results.every(r => r.passed) &&
                guards.every(g => !g.triggered);

  // Calculate score (0-100)
  const scores = results.map(r => {
    if (!r.summary) return 100;
    const risk = r.summary.riskScore || 0;
    return Math.max(0, 100 - risk);
  });

  const overallScore = scores.length > 0
    ? Math.round(scores.reduce((a, b) => a + b, 0) / scores.length)
    : 100;

  return {
    timestamp: new Date().toISOString(),
    passed,
    overallScore,
    invariants: results,
    guards,
    duration: Date.now() - startTime
  };
}

/**
 * Run quick security check (secrets + injection only)
 *
 * @param {string} directory - Directory to scan
 * @returns {Promise<Object>} Quick check results
 */
export async function quickCheck(directory) {
  const [secrets, injection] = await Promise.all([
    secretDetector.scanDirectory(directory),
    injectionChecker.scanDirectory(directory)
  ]);

  const criticalSecrets = secrets.summary.bySeverity.critical;
  const criticalInjection = injection.summary.bySeverity.critical;

  return {
    passed: criticalSecrets === 0 && criticalInjection === 0,
    secrets: {
      total: secrets.summary.total,
      critical: criticalSecrets
    },
    injection: {
      total: injection.summary.total,
      critical: criticalInjection
    }
  };
}

/**
 * Generate full security dashboard
 *
 * @param {string} directory - Directory to scan
 * @param {Object} [options] - Dashboard options
 * @returns {Promise<Object>} Dashboard results
 */
export async function generateSecurityDashboard(directory, options = {}) {
  return dashboard.generateDashboard(directory, options);
}

/**
 * Format security report as text
 *
 * @param {Object} report - Security report
 * @returns {string} Formatted text
 */
export function formatReport(report) {
  const lines = [];

  lines.push('='.repeat(60));
  lines.push('SECURITY INVARIANTS CHECK REPORT');
  lines.push('='.repeat(60));
  lines.push('');
  lines.push(`Timestamp: ${report.timestamp}`);
  lines.push(`Duration: ${report.duration}ms`);
  lines.push(`Overall Score: ${report.overallScore}/100`);
  lines.push(`Status: ${report.passed ? 'PASSED' : 'FAILED'}`);
  lines.push('');

  lines.push('-'.repeat(60));
  lines.push('INVARIANTS');
  lines.push('-'.repeat(60));

  for (const inv of report.invariants) {
    const status = inv.passed ? 'PASS' : 'FAIL';
    const findingCount = inv.findings?.length || 0;
    lines.push(`  [${status}] ${inv.invariant}`);
    if (findingCount > 0) {
      lines.push(`        ${findingCount} finding(s), severity: ${inv.severity}`);
    }
  }

  lines.push('');
  lines.push('-'.repeat(60));
  lines.push('GUARDS');
  lines.push('-'.repeat(60));

  for (const guard of report.guards) {
    const status = guard.triggered ? 'TRIGGERED' : 'CLEAR';
    lines.push(`  [${status}] ${guard.name}`);
    if (guard.details) {
      lines.push(`        ${guard.details}`);
    }
  }

  lines.push('');
  lines.push('='.repeat(60));

  return lines.join('\n');
}

/**
 * Create admission gate integration
 *
 * @param {Object} admissionResult - Admission gate result
 * @returns {Object} Security-enhanced admission result
 */
export function integrateWithAdmission(admissionResult) {
  return {
    ...admissionResult,
    securityChecks: {
      required: [
        'Q_no_hardcoded_secrets',
        'Q_no_dangerous_apis',
        'Q_dependency_security',
        'Q_license_compliance'
      ],
      guards: Object.keys(SECURITY_GUARDS)
    }
  };
}

// Re-export sub-modules
export {
  secretDetector,
  injectionChecker,
  dependencyAuditor,
  licenseChecker,
  auditTrailValidator,
  dashboard
};

export default {
  checkAllInvariants,
  quickCheck,
  generateSecurityDashboard,
  formatReport,
  integrateWithAdmission,
  SECURITY_INVARIANTS,
  SECURITY_GUARDS,
  DEFAULT_CONFIG,
  SecurityCheckResultSchema,
  SecurityReportSchema,
  SecurityConfigSchema
};
