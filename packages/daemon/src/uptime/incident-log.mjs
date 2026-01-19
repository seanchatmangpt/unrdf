/**
 * @file Incident Logging and Post-Mortem Data
 * @module @unrdf/daemon/uptime/incident-log
 * @description Log incidents with severity, track MTTR (Mean Time To Recovery),
 * track MTBF (Mean Time Between Failures), and generate incident summaries.
 */

import { z } from 'zod';

/**
 * Incident severity levels
 */
export const IncidentSeverity = {
  CRITICAL: 'critical',
  HIGH: 'high',
  MEDIUM: 'medium',
  LOW: 'low',
  INFO: 'info',
};

/**
 * Incident status values
 */
export const IncidentStatus = {
  OPEN: 'open',
  INVESTIGATING: 'investigating',
  IDENTIFIED: 'identified',
  MONITORING: 'monitoring',
  RESOLVED: 'resolved',
  POSTMORTEM: 'postmortem',
};

/**
 * Incident schema for logging service incidents
 */
export const IncidentSchema = z.object({
  id: z.string().min(1),
  title: z.string().min(1).max(500),
  description: z.string().optional(),
  severity: z.enum(['critical', 'high', 'medium', 'low', 'info']),
  status: z.enum(['open', 'investigating', 'identified', 'monitoring', 'resolved', 'postmortem']),
  createdAt: z.number().int().positive(),
  detectedAt: z.number().int().positive(),
  acknowledgedAt: z.number().int().positive().optional(),
  resolvedAt: z.number().int().positive().optional(),
  affectedServices: z.array(z.string()).default([]),
  assignee: z.string().optional(),
  tags: z.array(z.string()).default([]),
  impactDescription: z.string().optional(),
  rootCause: z.string().optional(),
  resolution: z.string().optional(),
  preventiveMeasures: z.array(z.string()).default([]),
  timeline: z.array(z.object({
    timestamp: z.number().int().positive(),
    event: z.string(),
    actor: z.string().optional(),
  })).default([]),
  metadata: z.record(z.string(), z.any()).default({}),
});

/**
 * Incident log configuration schema
 */
export const IncidentLogConfigSchema = z.object({
  serviceName: z.string().default('unrdf-daemon'),
  retentionPeriodMs: z.number().int().positive().default(31536000000), // 1 year
  autoGenerateId: z.boolean().default(true),
  requireAcknowledgement: z.boolean().default(true),
  severityThresholds: z.object({
    critical: z.number().int().min(0).default(0),
    high: z.number().int().min(0).default(5),
    medium: z.number().int().min(0).default(30),
    low: z.number().int().min(0).default(60),
  }).default({}),
});

/**
 * MTTR/MTBF metrics schema
 */
export const ReliabilityMetricsSchema = z.object({
  mttr: z.number().min(0),
  mtbf: z.number().min(0),
  mttrBySeverity: z.record(z.string(), z.number()),
  incidentCount: z.number().int().min(0),
  resolvedCount: z.number().int().min(0),
  averageResolutionTime: z.number().min(0),
  periodStart: z.number().int().positive(),
  periodEnd: z.number().int().positive(),
});

/**
 * Incident summary schema
 */
export const IncidentSummarySchema = z.object({
  totalIncidents: z.number().int().min(0),
  openIncidents: z.number().int().min(0),
  resolvedIncidents: z.number().int().min(0),
  bySeverity: z.record(z.string(), z.number()),
  byStatus: z.record(z.string(), z.number()),
  topAffectedServices: z.array(z.object({
    service: z.string(),
    count: z.number().int().min(0),
  })),
  recentIncidents: z.array(IncidentSchema),
  metrics: ReliabilityMetricsSchema,
});

/**
 * Generate unique incident ID
 * @returns {string}
 */
function generateIncidentId() {
  const timestamp = Date.now().toString(36);
  const random = Math.random().toString(36).substring(2, 8);
  return `INC-${timestamp}-${random}`.toUpperCase();
}

/**
 * Incident Log for tracking and managing service incidents
 */
export class IncidentLog {
  /**
   * Create incident log instance
   * @param {Object} [config] - Log configuration
   * @example
   * const log = new IncidentLog({ serviceName: 'api-server' });
   */
  constructor(config = {}) {
    this.config = IncidentLogConfigSchema.parse(config);

    /** @type {Map<string, z.infer<typeof IncidentSchema>>} */
    this.incidents = new Map();

    /** @type {Array<z.infer<typeof IncidentSchema>>} */
    this.resolvedIncidents = [];
  }

  /**
   * Create a new incident
   * @param {Object} incident - Incident data
   * @param {string} incident.title - Incident title
   * @param {string} [incident.description] - Incident description
   * @param {string} incident.severity - Severity level
   * @param {Array<string>} [incident.affectedServices] - Affected services
   * @param {string} [incident.assignee] - Assigned person
   * @param {Array<string>} [incident.tags] - Tags
   * @returns {z.infer<typeof IncidentSchema>}
   * @example
   * const incident = log.createIncident({
   *   title: 'Database connection timeout',
   *   severity: 'high',
   *   affectedServices: ['api', 'worker']
   * });
   */
  createIncident(incident) {
    const now = Date.now();
    const id = this.config.autoGenerateId
      ? generateIncidentId()
      : incident.id;

    if (!id) {
      throw new Error('Incident ID required when autoGenerateId is disabled');
    }

    const newIncident = IncidentSchema.parse({
      id,
      title: incident.title,
      description: incident.description,
      severity: incident.severity || IncidentSeverity.MEDIUM,
      status: IncidentStatus.OPEN,
      createdAt: now,
      detectedAt: incident.detectedAt || now,
      affectedServices: incident.affectedServices || [],
      assignee: incident.assignee,
      tags: incident.tags || [],
      impactDescription: incident.impactDescription,
      metadata: incident.metadata || {},
      timeline: [{
        timestamp: now,
        event: 'Incident created',
      }],
    });

    this.incidents.set(id, newIncident);
    return newIncident;
  }

  /**
   * Get incident by ID
   * @param {string} id - Incident ID
   * @returns {z.infer<typeof IncidentSchema>|null}
   */
  getIncident(id) {
    return this.incidents.get(id) ||
      this.resolvedIncidents.find(i => i.id === id) ||
      null;
  }

  /**
   * Update incident status
   * @param {string} id - Incident ID
   * @param {string} status - New status
   * @param {string} [note] - Optional note
   * @param {string} [actor] - Person making the change
   * @returns {z.infer<typeof IncidentSchema>}
   * @throws {Error} If incident not found
   * @example
   * log.updateStatus('INC-123', 'investigating', 'Starting investigation');
   */
  updateStatus(id, status, note, actor) {
    const incident = this.incidents.get(id);
    if (!incident) {
      throw new Error(`Incident ${id} not found`);
    }

    const now = Date.now();
    incident.status = status;

    // Set timestamps based on status
    if (status === IncidentStatus.INVESTIGATING && !incident.acknowledgedAt) {
      incident.acknowledgedAt = now;
    }

    if (status === IncidentStatus.RESOLVED && !incident.resolvedAt) {
      incident.resolvedAt = now;
      this.incidents.delete(id);
      this.resolvedIncidents.push(incident);
    }

    incident.timeline.push({
      timestamp: now,
      event: note || `Status changed to ${status}`,
      actor,
    });

    return incident;
  }

  /**
   * Acknowledge incident
   * @param {string} id - Incident ID
   * @param {string} [assignee] - Person acknowledging
   * @returns {z.infer<typeof IncidentSchema>}
   */
  acknowledgeIncident(id, assignee) {
    const incident = this.incidents.get(id);
    if (!incident) {
      throw new Error(`Incident ${id} not found`);
    }

    incident.acknowledgedAt = Date.now();
    if (assignee) {
      incident.assignee = assignee;
    }
    incident.status = IncidentStatus.INVESTIGATING;

    incident.timeline.push({
      timestamp: Date.now(),
      event: 'Incident acknowledged',
      actor: assignee,
    });

    return incident;
  }

  /**
   * Resolve incident
   * @param {string} id - Incident ID
   * @param {Object} resolution - Resolution details
   * @param {string} resolution.rootCause - Root cause description
   * @param {string} resolution.resolution - Resolution description
   * @param {Array<string>} [resolution.preventiveMeasures] - Preventive measures
   * @param {string} [actor] - Person resolving
   * @returns {z.infer<typeof IncidentSchema>}
   * @example
   * log.resolveIncident('INC-123', {
   *   rootCause: 'Connection pool exhaustion',
   *   resolution: 'Increased pool size and added connection timeout',
   *   preventiveMeasures: ['Add monitoring for pool usage']
   * });
   */
  resolveIncident(id, resolution, actor) {
    const incident = this.incidents.get(id);
    if (!incident) {
      throw new Error(`Incident ${id} not found`);
    }

    const now = Date.now();
    incident.status = IncidentStatus.RESOLVED;
    incident.resolvedAt = now;
    incident.rootCause = resolution.rootCause;
    incident.resolution = resolution.resolution;
    incident.preventiveMeasures = resolution.preventiveMeasures || [];

    incident.timeline.push({
      timestamp: now,
      event: 'Incident resolved',
      actor,
    });

    this.incidents.delete(id);
    this.resolvedIncidents.push(incident);

    this.pruneHistory();
    return incident;
  }

  /**
   * Add timeline event to incident
   * @param {string} id - Incident ID
   * @param {string} event - Event description
   * @param {string} [actor] - Person adding event
   * @returns {z.infer<typeof IncidentSchema>}
   */
  addTimelineEvent(id, event, actor) {
    const incident = this.incidents.get(id);
    if (!incident) {
      throw new Error(`Incident ${id} not found`);
    }

    incident.timeline.push({
      timestamp: Date.now(),
      event,
      actor,
    });

    return incident;
  }

  /**
   * Get all open incidents
   * @returns {Array<z.infer<typeof IncidentSchema>>}
   */
  getOpenIncidents() {
    return Array.from(this.incidents.values());
  }

  /**
   * Get incidents by severity
   * @param {string} severity - Severity level
   * @returns {Array<z.infer<typeof IncidentSchema>>}
   */
  getIncidentsBySeverity(severity) {
    const all = [...this.incidents.values(), ...this.resolvedIncidents];
    return all.filter(i => i.severity === severity);
  }

  /**
   * Get incidents within a time range
   * @param {number} startMs - Range start
   * @param {number} endMs - Range end
   * @returns {Array<z.infer<typeof IncidentSchema>>}
   */
  getIncidentsInRange(startMs, endMs) {
    const all = [...this.incidents.values(), ...this.resolvedIncidents];
    return all.filter(i => i.createdAt >= startMs && i.createdAt <= endMs);
  }

  /**
   * Calculate MTTR (Mean Time To Recovery)
   * @param {number} [periodStartMs] - Period start (optional)
   * @param {number} [periodEndMs] - Period end (optional)
   * @returns {number} MTTR in milliseconds
   * @example
   * const mttr = log.calculateMTTR();
   */
  calculateMTTR(periodStartMs, periodEndMs) {
    const now = Date.now();
    const startMs = periodStartMs || 0;
    const endMs = periodEndMs || now;

    const resolved = this.resolvedIncidents.filter(
      i => i.resolvedAt && i.createdAt >= startMs && i.createdAt <= endMs
    );

    if (resolved.length === 0) {
      return 0;
    }

    const totalRecoveryTime = resolved.reduce((sum, incident) => {
      const detectionTime = incident.detectedAt;
      const resolutionTime = incident.resolvedAt;
      return sum + (resolutionTime - detectionTime);
    }, 0);

    return totalRecoveryTime / resolved.length;
  }

  /**
   * Calculate MTTR by severity
   * @returns {Record<string, number>}
   */
  calculateMTTRBySeverity() {
    const result = {};

    for (const severity of Object.values(IncidentSeverity)) {
      const resolved = this.resolvedIncidents.filter(
        i => i.severity === severity && i.resolvedAt
      );

      if (resolved.length === 0) {
        result[severity] = 0;
        continue;
      }

      const totalRecoveryTime = resolved.reduce((sum, incident) => {
        return sum + (incident.resolvedAt - incident.detectedAt);
      }, 0);

      result[severity] = totalRecoveryTime / resolved.length;
    }

    return result;
  }

  /**
   * Calculate MTBF (Mean Time Between Failures)
   * @param {number} [periodStartMs] - Period start (optional)
   * @param {number} [periodEndMs] - Period end (optional)
   * @returns {number} MTBF in milliseconds
   * @example
   * const mtbf = log.calculateMTBF();
   */
  calculateMTBF(periodStartMs, periodEndMs) {
    const now = Date.now();
    const startMs = periodStartMs || (now - 2592000000); // 30 days default
    const endMs = periodEndMs || now;

    const incidents = this.getIncidentsInRange(startMs, endMs)
      .filter(i => i.severity === IncidentSeverity.CRITICAL ||
                   i.severity === IncidentSeverity.HIGH)
      .sort((a, b) => a.createdAt - b.createdAt);

    if (incidents.length < 2) {
      return endMs - startMs; // Return total period if less than 2 incidents
    }

    let totalTimeBetween = 0;
    for (let i = 1; i < incidents.length; i++) {
      totalTimeBetween += incidents[i].createdAt - incidents[i - 1].createdAt;
    }

    return totalTimeBetween / (incidents.length - 1);
  }

  /**
   * Calculate comprehensive reliability metrics
   * @param {number} [periodStartMs] - Period start
   * @param {number} [periodEndMs] - Period end
   * @returns {z.infer<typeof ReliabilityMetricsSchema>}
   * @example
   * const metrics = log.calculateReliabilityMetrics();
   */
  calculateReliabilityMetrics(periodStartMs, periodEndMs) {
    const now = Date.now();
    const startMs = periodStartMs || (now - 2592000000);
    const endMs = periodEndMs || now;

    const incidents = this.getIncidentsInRange(startMs, endMs);
    const resolved = incidents.filter(i => i.resolvedAt);

    return ReliabilityMetricsSchema.parse({
      mttr: this.calculateMTTR(startMs, endMs),
      mtbf: this.calculateMTBF(startMs, endMs),
      mttrBySeverity: this.calculateMTTRBySeverity(),
      incidentCount: incidents.length,
      resolvedCount: resolved.length,
      averageResolutionTime: resolved.length > 0
        ? resolved.reduce((sum, i) => sum + (i.resolvedAt - i.detectedAt), 0) / resolved.length
        : 0,
      periodStart: startMs,
      periodEnd: endMs,
    });
  }

  /**
   * Generate incident summary
   * @param {number} [recentLimit=10] - Number of recent incidents to include
   * @returns {z.infer<typeof IncidentSummarySchema>}
   * @example
   * const summary = log.generateSummary();
   */
  generateSummary(recentLimit = 10) {
    const allIncidents = [...this.incidents.values(), ...this.resolvedIncidents];
    const openIncidents = Array.from(this.incidents.values());

    // Count by severity
    const bySeverity = {};
    for (const severity of Object.values(IncidentSeverity)) {
      bySeverity[severity] = allIncidents.filter(i => i.severity === severity).length;
    }

    // Count by status
    const byStatus = {};
    for (const status of Object.values(IncidentStatus)) {
      byStatus[status] = allIncidents.filter(i => i.status === status).length;
    }

    // Top affected services
    const serviceCounts = new Map();
    for (const incident of allIncidents) {
      for (const service of incident.affectedServices) {
        serviceCounts.set(service, (serviceCounts.get(service) || 0) + 1);
      }
    }

    const topAffectedServices = Array.from(serviceCounts.entries())
      .map(([service, count]) => ({ service, count }))
      .sort((a, b) => b.count - a.count)
      .slice(0, 10);

    // Recent incidents
    const recentIncidents = allIncidents
      .sort((a, b) => b.createdAt - a.createdAt)
      .slice(0, recentLimit);

    return IncidentSummarySchema.parse({
      totalIncidents: allIncidents.length,
      openIncidents: openIncidents.length,
      resolvedIncidents: this.resolvedIncidents.length,
      bySeverity,
      byStatus,
      topAffectedServices,
      recentIncidents,
      metrics: this.calculateReliabilityMetrics(),
    });
  }

  /**
   * Format duration for display
   * @param {number} ms - Duration in milliseconds
   * @returns {string}
   */
  formatDuration(ms) {
    if (ms < 60000) return `${Math.round(ms / 1000)}s`;
    if (ms < 3600000) return `${Math.round(ms / 60000)}m`;
    if (ms < 86400000) return `${(ms / 3600000).toFixed(1)}h`;
    return `${(ms / 86400000).toFixed(1)}d`;
  }

  /**
   * Generate post-mortem report for an incident
   * @param {string} id - Incident ID
   * @returns {string} Markdown formatted post-mortem
   * @throws {Error} If incident not found or not resolved
   */
  generatePostMortem(id) {
    const incident = this.getIncident(id);
    if (!incident) {
      throw new Error(`Incident ${id} not found`);
    }

    if (!incident.resolvedAt) {
      throw new Error(`Incident ${id} is not yet resolved`);
    }

    const duration = incident.resolvedAt - incident.detectedAt;
    const timeToAck = incident.acknowledgedAt
      ? incident.acknowledgedAt - incident.detectedAt
      : null;

    const lines = [
      `# Post-Mortem Report: ${incident.id}`,
      ``,
      `## Summary`,
      ``,
      `**Title:** ${incident.title}`,
      `**Severity:** ${incident.severity.toUpperCase()}`,
      `**Duration:** ${this.formatDuration(duration)}`,
      `**Affected Services:** ${incident.affectedServices.join(', ') || 'None specified'}`,
      ``,
      `## Timeline`,
      ``,
      `| Time | Event |`,
      `|------|-------|`,
      `| ${new Date(incident.detectedAt).toISOString()} | Incident detected |`,
    ];

    if (incident.acknowledgedAt) {
      lines.push(`| ${new Date(incident.acknowledgedAt).toISOString()} | Incident acknowledged (TTAck: ${this.formatDuration(timeToAck)}) |`);
    }

    for (const event of incident.timeline) {
      if (event.event !== 'Incident created' && event.event !== 'Incident resolved') {
        lines.push(`| ${new Date(event.timestamp).toISOString()} | ${event.event}${event.actor ? ` (${event.actor})` : ''} |`);
      }
    }

    lines.push(`| ${new Date(incident.resolvedAt).toISOString()} | Incident resolved |`);
    lines.push(``);

    if (incident.impactDescription) {
      lines.push(`## Impact`);
      lines.push(``);
      lines.push(incident.impactDescription);
      lines.push(``);
    }

    lines.push(`## Root Cause`);
    lines.push(``);
    lines.push(incident.rootCause || 'Not documented');
    lines.push(``);

    lines.push(`## Resolution`);
    lines.push(``);
    lines.push(incident.resolution || 'Not documented');
    lines.push(``);

    if (incident.preventiveMeasures.length > 0) {
      lines.push(`## Preventive Measures`);
      lines.push(``);
      for (const measure of incident.preventiveMeasures) {
        lines.push(`- ${measure}`);
      }
      lines.push(``);
    }

    lines.push(`## Metrics`);
    lines.push(``);
    lines.push(`- **Time to Detect:** N/A (manual detection)`);
    lines.push(`- **Time to Acknowledge:** ${timeToAck ? this.formatDuration(timeToAck) : 'N/A'}`);
    lines.push(`- **Time to Resolve:** ${this.formatDuration(duration)}`);

    return lines.join('\n');
  }

  /**
   * Prune old resolved incidents
   * @private
   */
  pruneHistory() {
    const cutoff = Date.now() - this.config.retentionPeriodMs;
    this.resolvedIncidents = this.resolvedIncidents.filter(
      i => i.resolvedAt && i.resolvedAt > cutoff
    );
  }

  /**
   * Export incident log state
   * @returns {Object}
   */
  toJSON() {
    return {
      config: this.config,
      incidents: Array.from(this.incidents.entries()),
      resolvedIncidents: this.resolvedIncidents,
    };
  }

  /**
   * Restore incident log from JSON
   * @param {Object} data - Serialized state
   * @returns {IncidentLog}
   */
  static fromJSON(data) {
    const log = new IncidentLog(data.config);
    log.incidents = new Map(data.incidents || []);
    log.resolvedIncidents = data.resolvedIncidents || [];
    return log;
  }
}

/**
 * Create an incident log instance
 * @param {Object} [config] - Log configuration
 * @returns {IncidentLog}
 * @example
 * const log = createIncidentLog({ serviceName: 'api-server' });
 */
export function createIncidentLog(config = {}) {
  return new IncidentLog(config);
}
