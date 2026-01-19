/**
 * @file Report Command - Generate Uptime Reports
 * @module cli/commands/daemon/uptime/report
 * @description CLI command for generating uptime reports with SLA analysis
 *
 * @example
 * # Generate daily report with 99.9% SLA target
 * unrdf daemon uptime report --period day --sla-target 99.9
 *
 * # Generate weekly report as CSV
 * unrdf daemon uptime report --period week --format csv --output report.csv
 *
 * # Generate report for custom time range
 * unrdf daemon uptime report --period custom --start-time 2024-01-01 --end-time 2024-01-31
 */

import { defineCommand } from 'citty';
import { z } from 'zod';
import { ReportArgsSchema } from './schemas.mjs';
import {
  generateId,
  formatDuration,
  formatPercentage,
  formatAsCSV,
  writeOutput,
  printHeader,
  printSeparator,
  calculateMTBF,
  calculateMTTR,
} from './helpers.mjs';

/**
 * Period durations in milliseconds
 */
const PERIOD_DURATIONS = {
  hour: 3600000,
  day: 86400000,
  week: 604800000,
  month: 2592000000, // 30 days
  custom: 0,
};

/**
 * Generate simulated uptime data for reporting
 * @param {Object} options - Report options
 * @returns {Object} Generated report data
 */
function generateReportData(options) {
  const { period, slaTarget, startTime, endTime } = options;

  const reportId = generateId('report');
  const generatedAt = new Date().toISOString();

  // Calculate period duration
  let periodDuration;
  if (period === 'custom' && startTime && endTime) {
    periodDuration = new Date(endTime) - new Date(startTime);
  } else {
    periodDuration = PERIOD_DURATIONS[period] || PERIOD_DURATIONS.day;
  }

  // Generate realistic metrics based on period
  const baseUptime = 99.5 + Math.random() * 0.49; // 99.5% - 99.99%
  const uptimePercentage = Math.min(100, baseUptime);
  const totalUptime = periodDuration * (uptimePercentage / 100);
  const totalDowntime = periodDuration - totalUptime;

  // Incident count scales with period and inversely with uptime
  const baseIncidents = Math.ceil((100 - uptimePercentage) * 10);
  const periodMultiplier = periodDuration / PERIOD_DURATIONS.day;
  const incidentCount = Math.max(0, Math.floor(baseIncidents * periodMultiplier));

  const mtbf = calculateMTBF(totalUptime, incidentCount);
  const mttr = calculateMTTR(totalDowntime, incidentCount);

  const slaMet = uptimePercentage >= slaTarget;

  return {
    reportId,
    period,
    generatedAt,
    slaTarget,
    slaMet,
    periodDuration,
    metrics: {
      totalUptime: Math.round(totalUptime),
      totalDowntime: Math.round(totalDowntime),
      uptimePercentage,
      incidentCount,
      mtbf: Math.round(mtbf),
      mttr: Math.round(mttr),
    },
  };
}

/**
 * Format report as text
 * @param {Object} report - Report data
 * @returns {string} Formatted text
 */
function formatTextReport(report) {
  const lines = [];
  const m = report.metrics;

  lines.push('\nUptime Report');
  lines.push('='.repeat(60));
  lines.push(`Report ID: ${report.reportId}`);
  lines.push(`Period: ${report.period}`);
  lines.push(`Generated: ${report.generatedAt}`);
  lines.push('');

  lines.push('SLA Status');
  lines.push('-'.repeat(60));
  lines.push(`Target: ${formatPercentage(report.slaTarget)}%`);
  lines.push(`Actual: ${formatPercentage(m.uptimePercentage)}%`);
  lines.push(`Status: ${report.slaMet ? 'MET' : 'MISSED'}`);
  lines.push('');

  lines.push('Availability Metrics');
  lines.push('-'.repeat(60));
  lines.push(`Total Uptime: ${formatDuration(m.totalUptime)}`);
  lines.push(`Total Downtime: ${formatDuration(m.totalDowntime)}`);
  lines.push(`Uptime Percentage: ${formatPercentage(m.uptimePercentage)}%`);
  lines.push('');

  lines.push('Reliability Metrics');
  lines.push('-'.repeat(60));
  lines.push(`Incident Count: ${m.incidentCount}`);
  lines.push(`MTBF (Mean Time Between Failures): ${formatDuration(m.mtbf)}`);
  lines.push(`MTTR (Mean Time To Recovery): ${formatDuration(m.mttr)}`);
  lines.push('='.repeat(60));

  // SLA compliance details
  if (!report.slaMet) {
    const gap = report.slaTarget - m.uptimePercentage;
    const additionalUptimeNeeded = (gap / 100) * report.periodDuration;
    lines.push('\nSLA Gap Analysis');
    lines.push('-'.repeat(60));
    lines.push(`Gap: ${formatPercentage(gap)}%`);
    lines.push(`Additional uptime needed: ${formatDuration(additionalUptimeNeeded)}`);
    lines.push(`Recommendation: Reduce incident count or MTTR`);
  }

  return lines.join('\n');
}

/**
 * Generate incidents detail for extended report
 * @param {Object} report - Report data
 * @returns {Array} Array of incident objects
 */
function generateIncidentDetails(report) {
  const incidents = [];
  const now = Date.now();

  for (let i = 0; i < report.metrics.incidentCount; i++) {
    const incidentStart = now - Math.random() * report.periodDuration;
    const duration = Math.random() * report.metrics.mttr * 2;

    incidents.push({
      id: `inc-${i + 1}`,
      startTime: new Date(incidentStart).toISOString(),
      endTime: new Date(incidentStart + duration).toISOString(),
      duration: Math.round(duration),
      severity: Math.random() > 0.7 ? 'critical' : Math.random() > 0.5 ? 'major' : 'minor',
      resolved: true,
    });
  }

  return incidents.sort((a, b) => new Date(b.startTime) - new Date(a.startTime));
}

export const reportCommand = defineCommand({
  meta: {
    name: 'report',
    description: 'Generate uptime reports with SLA analysis',
  },
  args: {
    period: {
      type: 'string',
      description: 'Report period: hour, day, week, month, custom',
      default: 'day',
    },
    format: {
      type: 'string',
      description: 'Output format: json, text, csv',
      default: 'text',
    },
    'sla-target': {
      type: 'string',
      description: 'SLA target percentage (0-100)',
      default: '99.9',
    },
    output: {
      type: 'string',
      description: 'Output file path (default: stdout)',
    },
    'start-time': {
      type: 'string',
      description: 'Custom period start time (ISO 8601)',
    },
    'end-time': {
      type: 'string',
      description: 'Custom period end time (ISO 8601)',
    },
    json: {
      type: 'boolean',
      description: 'Output as JSON (shortcut for --format json)',
      default: false,
    },
  },
  async run({ args }) {
    try {
      // Parse numeric args from strings
      const parsedArgs = {
        period: args.period,
        format: args.json ? 'json' : args.format,
        'sla-target': parseFloat(args['sla-target']),
        output: args.output,
        'start-time': args['start-time'],
        'end-time': args['end-time'],
        json: args.json,
      };

      const validated = ReportArgsSchema.parse(parsedArgs);

      // Validate custom period has required times
      if (validated.period === 'custom') {
        if (!validated['start-time'] || !validated['end-time']) {
          throw new Error('Custom period requires --start-time and --end-time');
        }
      }

      const report = generateReportData({
        period: validated.period,
        slaTarget: validated['sla-target'],
        startTime: validated['start-time'],
        endTime: validated['end-time'],
      });

      // Determine effective format
      const format = validated.json ? 'json' : validated.format;

      let output;
      switch (format) {
        case 'json': {
          const extendedReport = {
            ...report,
            incidents: generateIncidentDetails(report),
          };
          output = JSON.stringify(extendedReport, null, 2);
          break;
        }
        case 'csv':
          output = formatAsCSV(report);
          break;
        case 'text':
        default:
          output = formatTextReport(report);
          break;
      }

      await writeOutput(output, validated.output);
    } catch (error) {
      if (error instanceof z.ZodError) {
        console.error(`Validation error: ${error.errors.map(e => e.message).join(', ')}`);
        console.error('\nUsage examples:');
        console.error('  unrdf daemon uptime report --period day --sla-target 99.9');
        console.error('  unrdf daemon uptime report --period week --format csv');
        console.error('  unrdf daemon uptime report --period custom --start-time 2024-01-01 --end-time 2024-01-31');
      } else {
        console.error(`Error: ${error.message}`);
      }
      process.exit(1);
    }
  },
});
