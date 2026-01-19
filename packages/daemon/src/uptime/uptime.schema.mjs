/**
 * @file Uptime Module Schemas
 * @module @unrdf/daemon/uptime/schemas
 * @description Zod schema definitions for uptime tracking, SLA reporting, and incident logging.
 */

import { z } from 'zod';

// ============================================================================
// Uptime Tracker Schemas
// ============================================================================

/**
 * Time period schema for uptime/downtime intervals
 */
export const TimePeriodSchema = z.object({
  start: z.number().int().positive(),
  end: z.number().int().positive().optional(),
  type: z.enum(['up', 'down', 'maintenance']),
  reason: z.string().optional(),
});

/**
 * Maintenance window schema
 */
export const MaintenanceWindowSchema = z.object({
  id: z.string().min(1),
  start: z.number().int().positive(),
  end: z.number().int().positive(),
  description: z.string().optional(),
  excludeFromSLA: z.boolean().default(true),
});

/**
 * Uptime tracker configuration schema
 */
export const UptimeTrackerConfigSchema = z.object({
  serviceName: z.string().default('unrdf-daemon'),
  rollingWindowMs: z.number().int().positive().default(86400000),
  historyRetentionMs: z.number().int().positive().default(2592000000),
  precisionMs: z.number().int().positive().default(1000),
  checkIntervalMs: z.number().int().positive().default(10000),
});

/**
 * Availability result schema
 */
export const AvailabilityResultSchema = z.object({
  availability: z.number().min(0).max(100),
  uptimeMs: z.number().int().min(0),
  downtimeMs: z.number().int().min(0),
  maintenanceMs: z.number().int().min(0),
  totalMs: z.number().int().min(0),
  periodStart: z.number().int().positive(),
  periodEnd: z.number().int().positive(),
  nines: z.string(),
});

// ============================================================================
// SLA Reporter Schemas
// ============================================================================

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
  billingPeriodMs: z.number().int().positive().default(2592000000),
  gracePeriodMs: z.number().int().min(0).default(300000),
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

// ============================================================================
// Incident Log Schemas
// ============================================================================

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
  retentionPeriodMs: z.number().int().positive().default(31536000000),
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
