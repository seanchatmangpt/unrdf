/**
 * @file SLA Compliance Reporter
 * @module @unrdf/daemon/uptime/sla-reporter
 * @description Defines SLA targets, calculates compliance, generates reports,
 * and tracks SLA credits/penalties for service level agreement management.
 */

import { z } from 'zod';
import { UptimeTracker, RollingWindowType, AvailabilityResultSchema } from './uptime-tracker.mjs';

/**
 * SLA tier configuration schema
 */
export const SLATierSchema = z.object({
  name: z.string().min(1),
  targetAvailability: z.number().min(0).max(100),
  maxDowntimePerMonth: z.number().int().min(0),
  creditPercentage: z.number().min(0).max(100).default(0),
  penaltyPercentage: z.number().min(0).max(100).default(0),
  description: z.string().optional(),
});

/**
 * SLA configuration schema
 */
export const SLAConfigSchema = z.object({
  serviceName: z.string().default('unrdf-daemon'),
  defaultTier: z.string().default('standard'),
  tiers: z.array(SLATierSchema).default([]),
  billingPeriodMs: z.number().int().positive().default(2592000000), // 30 days
  gracePeriodMs: z.number().int().min(0).default(300000), // 5 minutes
  excludeScheduledMaintenance: z.boolean().default(true),
});

/**
 * SLA compliance result schema
 */
export const SLAComplianceResultSchema = z.object({
  tier: z.string(),
  targetAvailability: z.number(),
  actualAvailability: z.number(),
  isCompliant: z.boolean(),
  uptimeMs: z.number().int().min(0),
  downtimeMs: z.number().int().min(0),
  allowedDowntimeMs: z.number().int().min(0),
  remainingDowntimeMs: z.number().int(),
  creditEarned: z.number().min(0),
  penaltyIncurred: z.number().min(0),
  periodStart: z.number().int().positive(),
  periodEnd: z.number().int().positive(),
  violations: z.array(z.object({
    start: z.number().int().positive(),
    end: z.number().int().positive(),
    durationMs: z.number().int().positive(),
    reason: z.string().optional(),
  })).default([]),
});

/**
 * SLA report format enum
 */
export const SLAReportFormat = {
  JSON: 'json',
  TEXT: 'text',
  MARKDOWN: 'markdown',
};

/**
 * Standard SLA tier presets
 */
export const StandardSLATiers = [
  {
    name: 'basic',
    targetAvailability: 99,
    maxDowntimePerMonth: 432000000, // ~5 days
    creditPercentage: 0,
    penaltyPercentage: 0,
    description: '99% availability - Basic tier',
  },
  {
    name: 'standard',
    targetAvailability: 99.9,
    maxDowntimePerMonth: 43200000, // ~12 hours
    creditPercentage: 10,
    penaltyPercentage: 0,
    description: '99.9% availability - Standard tier (3 nines)',
  },
  {
    name: 'premium',
    targetAvailability: 99.99,
    maxDowntimePerMonth: 4320000, // ~1.2 hours
    creditPercentage: 25,
    penaltyPercentage: 5,
    description: '99.99% availability - Premium tier (4 nines)',
  },
  {
    name: 'enterprise',
    targetAvailability: 99.999,
    maxDowntimePerMonth: 432000, // ~7.2 minutes
    creditPercentage: 50,
    penaltyPercentage: 10,
    description: '99.999% availability - Enterprise tier (5 nines)',
  },
];

/**
 * SLA Reporter for compliance tracking and reporting
 */
export class SLAReporter {
  /**
   * Create SLA reporter instance
   * @param {UptimeTracker} uptimeTracker - Uptime tracker instance
   * @param {Object} [config] - SLA configuration
   * @example
   * const reporter = new SLAReporter(uptimeTracker, {
   *   defaultTier: 'premium',
   *   tiers: StandardSLATiers
   * });
   */
  constructor(uptimeTracker, config = {}) {
    if (!(uptimeTracker instanceof UptimeTracker)) {
      throw new Error('Valid UptimeTracker instance required');
    }

    const parsedConfig = SLAConfigSchema.parse(config);

    // Add standard tiers if none provided
    if (parsedConfig.tiers.length === 0) {
      parsedConfig.tiers = StandardSLATiers;
    }

    this.uptimeTracker = uptimeTracker;
    this.config = parsedConfig;

    /** @type {Map<string, z.infer<typeof SLATierSchema>>} */
    this.tiers = new Map();

    for (const tier of this.config.tiers) {
      this.tiers.set(tier.name, SLATierSchema.parse(tier));
    }

    /** @type {Array<z.infer<typeof SLAComplianceResultSchema>>} */
    this.complianceHistory = [];
  }

  /**
   * Add or update an SLA tier
   * @param {Object} tier - Tier configuration
   * @returns {z.infer<typeof SLATierSchema>}
   * @example
   * reporter.addTier({
   *   name: 'custom',
   *   targetAvailability: 99.95,
   *   maxDowntimePerMonth: 21600000,
   *   creditPercentage: 15
   * });
   */
  addTier(tier) {
    const validatedTier = SLATierSchema.parse(tier);
    this.tiers.set(validatedTier.name, validatedTier);
    return validatedTier;
  }

  /**
   * Remove an SLA tier
   * @param {string} tierName - Tier name to remove
   * @returns {boolean} True if tier was removed
   */
  removeTier(tierName) {
    return this.tiers.delete(tierName);
  }

  /**
   * Get an SLA tier by name
   * @param {string} tierName - Tier name
   * @returns {z.infer<typeof SLATierSchema>|null}
   */
  getTier(tierName) {
    return this.tiers.get(tierName) || null;
  }

  /**
   * Get all SLA tiers
   * @returns {Array<z.infer<typeof SLATierSchema>>}
   */
  getAllTiers() {
    return Array.from(this.tiers.values());
  }

  /**
   * Calculate SLA compliance for a billing period
   * @param {string} [tierName] - Tier name (defaults to config.defaultTier)
   * @param {number} [periodStartMs] - Period start (defaults to current billing period)
   * @param {number} [periodEndMs] - Period end (defaults to now)
   * @returns {z.infer<typeof SLAComplianceResultSchema>}
   * @throws {Error} If tier not found
   * @example
   * const compliance = reporter.calculateCompliance('premium');
   */
  calculateCompliance(tierName, periodStartMs, periodEndMs) {
    const tier = this.tiers.get(tierName || this.config.defaultTier);
    if (!tier) {
      throw new Error(`SLA tier '${tierName || this.config.defaultTier}' not found`);
    }

    const now = Date.now();
    const endMs = periodEndMs || now;
    const startMs = periodStartMs || (endMs - this.config.billingPeriodMs);

    const availability = this.uptimeTracker.calculateAvailability(
      startMs,
      endMs,
      this.config.excludeScheduledMaintenance
    );

    const periodMs = endMs - startMs;
    const allowedDowntimeMs = Math.floor(periodMs * (1 - tier.targetAvailability / 100));
    const isCompliant = availability.availability >= tier.targetAvailability;
    const remainingDowntimeMs = Math.max(0, allowedDowntimeMs - availability.downtimeMs);

    // Calculate credits/penalties
    let creditEarned = 0;
    let penaltyIncurred = 0;

    if (!isCompliant && tier.creditPercentage > 0) {
      const shortfall = tier.targetAvailability - availability.availability;
      creditEarned = Math.min(100, shortfall * tier.creditPercentage);
    }

    if (!isCompliant && tier.penaltyPercentage > 0) {
      const shortfall = tier.targetAvailability - availability.availability;
      penaltyIncurred = Math.min(100, shortfall * tier.penaltyPercentage);
    }

    // Get violation periods
    const downtimePeriods = this.uptimeTracker.getDowntimePeriods(startMs, endMs);
    const violations = downtimePeriods
      .filter(p => {
        const duration = (p.end || now) - p.start;
        return duration > this.config.gracePeriodMs;
      })
      .map(p => ({
        start: p.start,
        end: p.end || now,
        durationMs: (p.end || now) - p.start,
        reason: p.reason,
      }));

    const result = SLAComplianceResultSchema.parse({
      tier: tier.name,
      targetAvailability: tier.targetAvailability,
      actualAvailability: availability.availability,
      isCompliant,
      uptimeMs: availability.uptimeMs,
      downtimeMs: availability.downtimeMs,
      allowedDowntimeMs,
      remainingDowntimeMs,
      creditEarned,
      penaltyIncurred,
      periodStart: startMs,
      periodEnd: endMs,
      violations,
    });

    this.complianceHistory.push(result);
    return result;
  }

  /**
   * Generate SLA compliance report
   * @param {string} format - Report format ('json', 'text', 'markdown')
   * @param {string} [tierName] - Tier name for report
   * @param {number} [periodStartMs] - Period start
   * @param {number} [periodEndMs] - Period end
   * @returns {string|Object} Formatted report
   * @example
   * const report = reporter.generateReport('markdown', 'premium');
   */
  generateReport(format, tierName, periodStartMs, periodEndMs) {
    const compliance = this.calculateCompliance(tierName, periodStartMs, periodEndMs);
    const tier = this.tiers.get(compliance.tier);

    switch (format) {
      case SLAReportFormat.JSON:
        return this.generateJSONReport(compliance, tier);
      case SLAReportFormat.TEXT:
        return this.generateTextReport(compliance, tier);
      case SLAReportFormat.MARKDOWN:
        return this.generateMarkdownReport(compliance, tier);
      default:
        return this.generateJSONReport(compliance, tier);
    }
  }

  /**
   * Generate JSON report
   * @private
   */
  generateJSONReport(compliance, tier) {
    return {
      reportGenerated: new Date().toISOString(),
      serviceName: this.config.serviceName,
      tier: {
        name: tier.name,
        description: tier.description,
        target: `${tier.targetAvailability}%`,
      },
      compliance: {
        status: compliance.isCompliant ? 'COMPLIANT' : 'VIOLATION',
        actualAvailability: `${compliance.actualAvailability.toFixed(4)}%`,
        targetAvailability: `${compliance.targetAvailability}%`,
      },
      uptime: {
        uptimeMs: compliance.uptimeMs,
        downtimeMs: compliance.downtimeMs,
        allowedDowntimeMs: compliance.allowedDowntimeMs,
        remainingDowntimeMs: compliance.remainingDowntimeMs,
      },
      billing: {
        creditEarned: `${compliance.creditEarned.toFixed(2)}%`,
        penaltyIncurred: `${compliance.penaltyIncurred.toFixed(2)}%`,
      },
      period: {
        start: new Date(compliance.periodStart).toISOString(),
        end: new Date(compliance.periodEnd).toISOString(),
        durationDays: Math.round((compliance.periodEnd - compliance.periodStart) / 86400000),
      },
      violations: compliance.violations.map(v => ({
        start: new Date(v.start).toISOString(),
        end: new Date(v.end).toISOString(),
        durationMinutes: Math.round(v.durationMs / 60000),
        reason: v.reason || 'Unknown',
      })),
    };
  }

  /**
   * Generate plain text report
   * @private
   */
  generateTextReport(compliance, tier) {
    const lines = [
      `SLA Compliance Report - ${this.config.serviceName}`,
      `Generated: ${new Date().toISOString()}`,
      ``,
      `Tier: ${tier.name} (${tier.description || 'No description'})`,
      `Target: ${tier.targetAvailability}%`,
      ``,
      `STATUS: ${compliance.isCompliant ? 'COMPLIANT' : 'VIOLATION'}`,
      ``,
      `Actual Availability: ${compliance.actualAvailability.toFixed(4)}%`,
      `Target Availability: ${compliance.targetAvailability}%`,
      ``,
      `Uptime: ${this.formatDuration(compliance.uptimeMs)}`,
      `Downtime: ${this.formatDuration(compliance.downtimeMs)}`,
      `Allowed Downtime: ${this.formatDuration(compliance.allowedDowntimeMs)}`,
      `Remaining Budget: ${this.formatDuration(compliance.remainingDowntimeMs)}`,
      ``,
      `Period: ${new Date(compliance.periodStart).toISOString()} - ${new Date(compliance.periodEnd).toISOString()}`,
      ``,
    ];

    if (compliance.creditEarned > 0 || compliance.penaltyIncurred > 0) {
      lines.push(`Credit Earned: ${compliance.creditEarned.toFixed(2)}%`);
      lines.push(`Penalty Incurred: ${compliance.penaltyIncurred.toFixed(2)}%`);
      lines.push(``);
    }

    if (compliance.violations.length > 0) {
      lines.push(`Violations (${compliance.violations.length}):`);
      for (const v of compliance.violations) {
        lines.push(`  - ${new Date(v.start).toISOString()}: ${this.formatDuration(v.durationMs)} (${v.reason || 'Unknown'})`);
      }
    }

    return lines.join('\n');
  }

  /**
   * Generate Markdown report
   * @private
   */
  generateMarkdownReport(compliance, tier) {
    const statusBadge = compliance.isCompliant
      ? '![Status](https://img.shields.io/badge/SLA-Compliant-green)'
      : '![Status](https://img.shields.io/badge/SLA-Violation-red)';

    const lines = [
      `# SLA Compliance Report`,
      ``,
      `**Service:** ${this.config.serviceName}`,
      `**Generated:** ${new Date().toISOString()}`,
      ``,
      `## Status`,
      ``,
      statusBadge,
      ``,
      `| Metric | Value |`,
      `|--------|-------|`,
      `| Tier | ${tier.name} |`,
      `| Target Availability | ${tier.targetAvailability}% |`,
      `| Actual Availability | ${compliance.actualAvailability.toFixed(4)}% |`,
      `| Status | ${compliance.isCompliant ? 'COMPLIANT' : 'VIOLATION'} |`,
      ``,
      `## Uptime Statistics`,
      ``,
      `| Metric | Duration |`,
      `|--------|----------|`,
      `| Total Uptime | ${this.formatDuration(compliance.uptimeMs)} |`,
      `| Total Downtime | ${this.formatDuration(compliance.downtimeMs)} |`,
      `| Allowed Downtime | ${this.formatDuration(compliance.allowedDowntimeMs)} |`,
      `| Remaining Budget | ${this.formatDuration(compliance.remainingDowntimeMs)} |`,
      ``,
      `## Billing Period`,
      ``,
      `- **Start:** ${new Date(compliance.periodStart).toISOString()}`,
      `- **End:** ${new Date(compliance.periodEnd).toISOString()}`,
      `- **Duration:** ${Math.round((compliance.periodEnd - compliance.periodStart) / 86400000)} days`,
      ``,
    ];

    if (compliance.creditEarned > 0 || compliance.penaltyIncurred > 0) {
      lines.push(`## Credits & Penalties`);
      lines.push(``);
      lines.push(`- Credit Earned: **${compliance.creditEarned.toFixed(2)}%**`);
      lines.push(`- Penalty Incurred: **${compliance.penaltyIncurred.toFixed(2)}%**`);
      lines.push(``);
    }

    if (compliance.violations.length > 0) {
      lines.push(`## Violations`);
      lines.push(``);
      lines.push(`| Start | Duration | Reason |`);
      lines.push(`|-------|----------|--------|`);
      for (const v of compliance.violations) {
        lines.push(`| ${new Date(v.start).toISOString()} | ${this.formatDuration(v.durationMs)} | ${v.reason || 'Unknown'} |`);
      }
    }

    return lines.join('\n');
  }

  /**
   * Format duration in human-readable form
   * @private
   * @param {number} ms - Duration in milliseconds
   * @returns {string}
   */
  formatDuration(ms) {
    if (ms < 1000) return `${ms}ms`;
    if (ms < 60000) return `${(ms / 1000).toFixed(1)}s`;
    if (ms < 3600000) return `${(ms / 60000).toFixed(1)}m`;
    if (ms < 86400000) return `${(ms / 3600000).toFixed(2)}h`;
    return `${(ms / 86400000).toFixed(2)}d`;
  }

  /**
   * Get compliance history
   * @param {number} [limit=10] - Max results
   * @returns {Array<z.infer<typeof SLAComplianceResultSchema>>}
   */
  getComplianceHistory(limit = 10) {
    return this.complianceHistory.slice(-limit);
  }

  /**
   * Check if currently compliant
   * @param {string} [tierName] - Tier name to check
   * @returns {boolean}
   */
  isCurrentlyCompliant(tierName) {
    const compliance = this.calculateCompliance(tierName);
    return compliance.isCompliant;
  }

  /**
   * Get remaining downtime budget
   * @param {string} [tierName] - Tier name
   * @returns {number} Remaining downtime in ms
   */
  getRemainingDowntimeBudget(tierName) {
    const compliance = this.calculateCompliance(tierName);
    return compliance.remainingDowntimeMs;
  }

  /**
   * Export reporter state
   * @returns {Object}
   */
  toJSON() {
    return {
      config: this.config,
      tiers: Array.from(this.tiers.entries()),
      complianceHistory: this.complianceHistory,
    };
  }
}

/**
 * Create an SLA reporter instance
 * @param {UptimeTracker} uptimeTracker - Uptime tracker instance
 * @param {Object} [config] - SLA configuration
 * @returns {SLAReporter}
 * @example
 * const reporter = createSLAReporter(tracker, { defaultTier: 'premium' });
 */
export function createSLAReporter(uptimeTracker, config = {}) {
  return new SLAReporter(uptimeTracker, config);
}
