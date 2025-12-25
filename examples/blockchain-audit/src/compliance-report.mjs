/**
 * @file Compliance Reporter
 * @description Generate compliance reports from audit trail
 * @module blockchain-audit/compliance-report
 */

import { trace } from '@opentelemetry/api';

const tracer = trace.getTracer('compliance-reporter');

/**
 * Compliance Reporter
 *
 * Features:
 * - SOX compliance
 * - GDPR audit trails
 * - Custom compliance frameworks
 * - PDF report generation
 *
 * @class
 */
export class ComplianceReporter {
  /**
   * @param {object} config - Configuration
   */
  constructor(config = {}) {
    this.config = {
      framework: 'SOX',
      ...config,
    };
  }

  /**
   * Generate compliance report
   *
   * @param {object} auditTrail - Audit trail instance
   * @param {object} criteria - Report criteria
   * @returns {Promise<object>} Report
   */
  async generate(auditTrail, criteria = {}) {
    return tracer.startActiveSpan('compliance.generate', async (span) => {
      try {
        const report = await auditTrail.generateComplianceReport(criteria);

        // Add framework-specific sections
        report.framework = this.config.framework;
        report.sections = this._generateSections(report);

        span.setStatus({ code: 1 });
        return report;
      } catch (error) {
        span.setStatus({ code: 2, message: error.message });
        throw error;
      } finally {
        span.end();
      }
    });
  }

  /**
   * Generate framework-specific sections
   *
   * @private
   * @param {object} report - Base report
   * @returns {object[]} Sections
   */
  _generateSections(report) {
    const sections = [
      {
        title: 'Executive Summary',
        content: `Audit trail contains ${report.summary.totalWorkflows} workflows with ${report.summary.verifiedReceipts} verified receipts.`,
      },
      {
        title: 'Compliance Status',
        content: JSON.stringify(report.compliance, null, 2),
      },
    ];

    return sections;
  }
}

export default ComplianceReporter;
